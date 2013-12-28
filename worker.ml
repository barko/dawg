(* server which computes best splits on behalf of clients *)

let port = 60_287
(* if we can't bind to this port, we concluded that another instance
   of this server is running on the same host; only one such instance may
   run on each host, so we refuse to start another on another port *)

let create () =
  LP_tcp.Server.create port

let send t peer m =
  let s = Proto_b.string_of_worker_to_master m in
  LP_tcp.Server.send t peer (Some s)

let recv t peer =
  lwt peer, event = LP_tcp.Server.recv t in
  let r =
    match event with
      | `Connect -> `Connect
      | `Disconnect -> `Disconnect
      | `Message s -> `Message (Proto_b.worker_to_master_of_string s)
  in
  Lwt.return r

let is_sleeping thr =
  match Lwt.state thr with
    | Lwt.Sleep -> true
    | Lwt.Fail _ -> assert false
    | Lwt.Return _ -> false

let nchoose_fold f threads x0 =
  lwt results = Lwt.nchoose threads in
  let sleeping_threads = List.filter is_sleeping threads in
  let rec loop x threads_accu = function
    | [] -> Lwt.return (x, List.flatten threads_accu)
    | h :: t ->
      lwt x, threads = f x h in
      loop x (threads :: threads_accu) t
  in
  loop x0 [sleeping_threads] results

let rec run t threads =
  lwt t, threads = nchoose_fold react threads t in
  run t threads

and react t = function
  | _ -> Lwt.return (t, [])

let worker detach =
  let _srv = create () in
  Lwt_main.run (Lwt.join [])


open Cmdliner

let commands =
  let worker_cmd =
    let doc = "start the EigenDog worker server" in
    let detach =
      let doc = "detach from the terminal" in
      Arg.(value & opt (some bool) (Some true) &
           info ["d";"detach"] ~docv:"BOOL" ~doc)
    in

    Term.( pure worker $ detach ), Term.info "worker" ~doc
  in
  [worker_cmd]
