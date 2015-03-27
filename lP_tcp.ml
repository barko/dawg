let close_ignore sock =
  try_lwt
    Lwt_unix.close sock
  with _ ->
    Lwt.return ()

let read_int32 s =
  let b0 = Char.code s.[3] in
  let b1 = Char.code s.[2] in
  let b2 = Char.code s.[1] in
  let b3 = Char.code s.[0] in
  (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3

let write_int32 s i =
  Bytes.set s 3 (Char.chr (0xff land (i lsr 24)));
  Bytes.set s 2 (Char.chr (0xff land (i lsr 16)));
  Bytes.set s 1 (Char.chr (0xff land (i lsr  8)));
  Bytes.set s 0 (Char.chr (0xff land (i       )))

let really_read sock buf offset n_to_read =
  let rec loop offset n =
    lwt n' = Lwt_unix.read sock buf offset n in
    if n' = 0 then
      (* closed connection *)
      Lwt.return None
    else if n' = n then
      (* reading complete *)
      Lwt.return (Some buf)
    else
      (* reading incomplete; read remaining bytes *)
      loop (offset + n') (n - n')
  in
  loop offset n_to_read

let really_read sock n =
  let buf = Bytes.create n in
  really_read sock buf 0 n

let read sock =
  (* read four-byte prefix, encoding the message body's length *)
  lwt message_length_s_opt = really_read sock 4 in
  match message_length_s_opt with
    | None ->
      (* disconnected while reading message-length prefix *)
      Lwt.return None

    | Some message_length_s ->
      let message_length = read_int32 message_length_s in
      if message_length = 0 then
        (* empty messages are ok *)
        Lwt.return (Some "")
      else
        (* with the body length now known, read the body *)
        really_read sock message_length


let really_write sock buf offset n_to_write =
  let rec loop offset n =
    lwt n' = Lwt_unix.write sock buf offset n in
    if n' = n then
      (* reading complete *)
      Lwt.return ()
    else
      (* reading incomplete *)
      loop (offset + n') (n - n')
  in
  loop offset n_to_write

let really_write sock buf =
  try
    lwt () = really_write sock buf 0 (Bytes.length buf) in
    Lwt.return true
  with Unix.Unix_error _ ->
    Lwt.return false

let write sock message =
  let message_length = Bytes.length message in
  let message_length_s = Bytes.create 4 in
  write_int32 message_length_s message_length;
  lwt write_ok = really_write sock message_length_s in
  if write_ok then
    (* message length prefix written without error *)
    really_write sock message
  else
    Lwt.return false

module Server = struct

  type peer = {
    fd : Lwt_unix.file_descr;
    sock : Unix.sockaddr;
    write_queue : string Lwt_queue.t;
    read_queue : string option Lwt_queue.t;
  }

  type event = [
  | `Connect
  | `Disconnect
  | `Message of string
  ]

  type t = {
    output : (peer * string option) Lwt_queue.t;
    input : (peer * event) Lwt_queue.t
  }

  let sockaddr_of_peer peer =
    peer.sock

  let recv t =
    Lwt_queue.take t.input

  let send t peer body_opt =
    match body_opt with
      | Some body ->
        Lwt_queue.put peer.write_queue body
      | None ->
        lwt () = close_ignore peer.fd in
        Lwt_queue.put t.output (peer, None)

  let run port listening_socket t  =

    let rec write_loop peer =
      lwt body = Lwt_queue.take peer.write_queue in
      lwt sent = write peer.fd body in
      if sent then
        write_loop peer
      else
        (* terminate, by doing the same thing a user of [t] would do
           to terminate: place [None] in the output queue *)
        lwt () = close_ignore peer.fd in
        Lwt_queue.put t.output (peer, None)
    in

    let rec read_loop peer =
      lwt body_opt = read peer.fd in
      lwt () = Lwt_queue.put peer.read_queue body_opt in
      match body_opt with
        | Some body ->
          lwt () = Lwt_queue.put t.input (peer, `Message body) in
          read_loop peer

        | None ->
          lwt () = close_ignore peer.fd in
          lwt () = Lwt_queue.put t.input (peer, `Disconnect) in
          Lwt.return ()
    in

    let rec loop () =
      (* accept a connection *)
      lwt fd, sock = Lwt_unix.accept listening_socket in
      let peer = {
        fd;
        sock;
        write_queue = Lwt_queue.create () ;
        read_queue = Lwt_queue.create ()
      } in

      (* create new thread to write serially read and write to the
         file descriptor *)
      Lwt.ignore_result ( write_loop peer );
      Lwt.ignore_result ( read_loop peer );

      lwt () = Lwt_queue.put t.input (peer, `Connect) in
      loop ()
    in
    loop ()

  let create port =
    let listening_socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Lwt_unix.setsockopt listening_socket Unix.SO_REUSEADDR true;
    let local_addr = Unix.ADDR_INET (Unix.inet_addr_any, port) in
    Lwt_unix.bind listening_socket local_addr;
    Lwt_unix.listen listening_socket 1024;
    let t = {
      input = Lwt_queue.create ();
      output = Lwt_queue.create ();
    } in
    Lwt.ignore_result (run port listening_socket t);
    t

end

module Client = struct

  type t = {
    fd : Lwt_unix.file_descr;
    input : string option Lwt_queue.t;
    output : string option Lwt_queue.t
  }

  let rec write_loop t =
    lwt body_opt = Lwt_queue.take t.output in
    match body_opt with
      | None -> (
        lwt () = close_ignore t.fd in
        (* signal that the connection is terminated *)
        Lwt_queue.put t.input None
      )

      | Some body -> (
        lwt sent = write t.fd body in
        if sent then
          write_loop t
        else
          lwt () = close_ignore t.fd in
          (* signal that the connection is terminated *)
          Lwt_queue.put t.input None
      )

  let rec read_loop t =
    lwt body_opt = read t.fd in
    match body_opt with
      | None ->
        lwt () = close_ignore t.fd in
        lwt () = Lwt_queue.put t.input None in
        Lwt.return ()

      | Some _ ->
        lwt () = Lwt_queue.put t.input body_opt in
        read_loop t

  let create host port =
    let server_addr = (Unix.gethostbyname host).Unix.h_addr_list.(0) in
    let addr = Unix.ADDR_INET (server_addr, port) in
    let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    lwt () = Lwt_unix.connect fd addr in
    let t = {
      fd ;
      input = Lwt_queue.create ();
      output = Lwt_queue.create ();
    } in
    Lwt.ignore_result (write_loop t);
    Lwt.ignore_result (read_loop t);
    Lwt.return t

  let recv t =
    Lwt_queue.take t.input

  let send t v =
    Lwt_queue.put t.output v

end
