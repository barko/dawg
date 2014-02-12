open LP_tcp

type t = LP_tcp.Client.t

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


type timed_incast_tmo = {
  ok : (float * LP_tcp.Client.t * Proto_b.from_worker) list;
  failed : LP_tcp.Client.t list;
  timed_out : LP_tcp.Client.t list;
}

let all_ok { failed; timed_out } =
  match failed, timed_out with
    | [], [] -> true
    | _ -> false


exception DisconnectedPeer of (LP_tcp.Client.t * Proto_t.to_worker)
exception TimedOutPeer of (LP_tcp.Client.t * Proto_t.to_worker)
exception ProtocolError of
    (LP_tcp.Client.t * Proto_t.to_worker * Proto_t.from_worker )

let timed_incast_tmo ts timeout =
  let ok = ref [] in
  let failed = ref [] in
  let timed_out = ref [] in

  let tick = Unix.gettimeofday () in
  lwt () = Lwt_list.iter_p (
    fun t ->
      lwt rs = Lwt.pick [w_recv t tick; w_sleep timeout] in
      (match rs with
        | `R (_, None) -> failed := t :: !failed
        | `R (tock, Some r) -> ok := (tock, t, r) :: !ok
        | `T -> timed_out := t :: !timed_out
      );

      Lwt.return ()

  ) ts in
  Lwt.return { ok = !ok; failed = !failed; timed_out = !timed_out }

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

let incast ts =
  lwt results = timed_incast ts in
  let results_sans_time = List.map snd results in
  Lwt.return results_sans_time
