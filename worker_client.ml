open LP_tcp

type t = LP_tcp.Client.t

let string_of_peer peer =
  let open Unix in
  match Server.sockaddr_of_peer peer with
    | ADDR_UNIX s -> "unix:" ^ s
    | ADDR_INET (inet, port) -> Printf.sprintf "inet:%s:%d"
      (string_of_inet_addr inet) port

let create host =
  LP_tcp.Client.create host Worker.port

let send t m =
  let s = Proto_b.string_of_to_worker m in
  LP_tcp.Client.send t (Some s)

(* avoid serializing the message multiple times *)
let broadcast ts m =
  let s = Proto_b.string_of_to_worker m in
  Lwt_list.iter_p (fun t -> LP_tcp.Client.send t (Some s)) ts

let recv t =
  lwt s_opt = LP_tcp.Client.recv t in
  let m_opt =
    match s_opt with
      | Some s -> Some (Proto_b.from_worker_of_string s)
      | None -> None
  in
  Lwt.return m_opt

let timed_incast ts =
  let tick = Unix.gettimeofday () in
  Lwt_list.map_p (
    fun t ->
      lwt result = recv t in
      let tock = Unix.gettimeofday () in
      Lwt.return (tock -. tick, (t, result))
  ) ts

let w_recv t tick =
  lwt ro = recv t in
  let tock = Unix.gettimeofday () in
  Lwt.return (`R (tock -. tick, ro))

let w_sleep timeout =
  lwt () = Lwt_unix.sleep timeout in
  Lwt.return `T


exception DisconnectedPeer of (LP_tcp.Client.t * Proto_t.to_worker)
exception TimedOutPeer of (LP_tcp.Client.t * Proto_t.to_worker)
exception ProtocolError of
    (LP_tcp.Client.t * Proto_t.to_worker * Proto_t.from_worker )

let broad_send_recv ts timeout request is_response_valid =
  lwt () = broadcast ts request in
  let tick = Unix.gettimeofday () in
  lwt ok = Lwt_list.map_p (
      fun t ->
        lwt rs = Lwt.pick [w_recv t tick; w_sleep timeout] in
        match rs with
          | `R (_, None) -> Lwt.fail (DisconnectedPeer (t, request))
          | `T -> Lwt.fail (TimedOutPeer (t, request))
          | `R (tock, Some response) ->
            if is_response_valid response then
              Lwt.return (tock, t, response)
            else
              Lwt.fail (ProtocolError (t, request, response))
    ) ts
  in
  Lwt.return ok

let broad_send_recv_nx ts timeout request is_response_valid =
  lwt () = broadcast ts request in
  let tick = Unix.gettimeofday () in
  lwt ok = Lwt_list.map_p (
      fun t ->
        lwt rs = Lwt.pick [w_recv t tick; w_sleep timeout] in
        lwt rs =
          match rs with
            | `R (_, None) -> Lwt.return `E
            | `T -> Lwt.return `T
            | `R (tock, Some response) ->
              if is_response_valid response then
                Lwt.return (`R (tock, response))
              else
                Lwt.fail (ProtocolError (t, request, response))
        in
        Lwt.return (t, rs)
    ) ts
  in
  Lwt.return ok

let incast ts =
  lwt results = timed_incast ts in
  let results_sans_time = List.map snd results in
  Lwt.return results_sans_time
