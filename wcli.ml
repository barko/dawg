open LP_tcp

(* the client connects to the server, reads text from stdin, and sends
   it to the server; it also writes to stdout any messages received
   from the server; it quits when the server disconnects, or when it
   reads end-of-file from stdin. *)
let client host port =
  let open Lwt in
  lwt client = Client.create host port in

  let rec send_loop () =
    lwt () = Lwt_io.write Lwt_io.stdout "> " in

    try_lwt
      lwt js = Lwt_io.read_line Lwt_io.stdin in
      (* json string -> t *)
      let t = Proto_j.to_worker_of_string js in
      (* t -> biniou string *)
      let bs = Proto_b.string_of_to_worker t in

      lwt () = Client.send client (Some bs) in
      send_loop ()
    with
      | Ag_oj_run.Error msg (* not part of protocol type definition *)
      | Yojson.Json_error msg (* malformed json *)
      | Failure msg -> (* malformed json *)
        lwt () = Lwt_io.write Lwt_io.stdout (msg ^ "\n") in
        send_loop ()
  in

  let send_loop () =
    try_lwt
      send_loop ()
    with End_of_file ->
      Lwt_io.write Lwt_io.stdout "bye!\n"
  in

  let rec recv_loop () =
    lwt body_opt = Client.recv client in
    match body_opt with
      | None ->
        Lwt_io.printf "server disconnected\n%!"

      | Some bs ->
        (* biniou string -> t *)
        let t = Proto_b.from_worker_of_string bs in
        (* t -> json string *)
        let js = Proto_j.string_of_from_worker t in

        let js_pretty = Yojson.Safe.prettify js in
        lwt () = Lwt_io.printf "%s\n> %!" js_pretty in
        recv_loop ()
  in

  Lwt.pick [ recv_loop (); send_loop () ]

let _ =
  let usage () =
    Printf.printf "usage: %s <host> <port>\n%!"
      Sys.argv.(0);
    exit 1
  in

  let int_of_string_exit p =
    try
      int_of_string p
    with Failure msg ->
      print_endline msg;
      exit 1
  in

  match Sys.argv with
    | [| _; host; port_s |] ->
      let port = int_of_string_exit port_s in
      Lwt_unix.run (client host port)

    | other -> usage ()
