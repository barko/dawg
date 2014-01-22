(* server which computes best splits on behalf of clients *)

let port = 60_287
(* if we can't bind to this port, we concluded that another instance
   of this server is running on the same host; only one such instance
   may run on each host, so we refuse to start another on another
   port *)

let sp = Printf.sprintf

let create () =
  LP_tcp.Server.create port

(* serialize and send outgoing message *)
let send t peer m =
  let s = Proto_b.string_of_from_worker m in
  LP_tcp.Server.send t peer (Some s)

(* deserialize (parse) incoming message *)
let recv srv =
  lwt peer, event = LP_tcp.Server.recv srv in
  let event =
    match event with
      | `Connect -> `Connect
      | `Disconnect -> `Disconnect
      | `Message s -> `Message (Proto_b.to_worker_of_string s)
  in
  Lwt.return (peer, event)

let is_sleeping thr =
  match Lwt.state thr with
    | Lwt.Sleep
    | Lwt.Return _ -> false
    | Lwt.Fail _ -> assert false

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

type ba = bool array
type subset_list = [
  | `S of (ba * subset_list)
  | `LR of (ba * ba * subset_list)
  | `N
]

module Working = struct
  type t = {
    task_id : Proto_t.task_id;
    y_feature_id : Proto_t.feature_id;
    fold_feature_id : Proto_t.feature_id option;
    splitter : < best_split : Feat.afeature -> (float * Proto_t.split) option >;
    feature_map : D_feat_map.t;
    sampler : Sampler.t;
    fold_set : bool array;
    subsets : subset_list;
  }
end

type state = [
  | `Available
  (* worker is free to do work for any master that cares for its services *)

  | `Working of Working.t
  (* worker has successfully setup the task; that means
     it has at least the target (y) feature, and the fold
     feature (if one is required) *)
]

type t = {
  srv : LP_tcp.Server.t;
  worker_id : string;
  user : string;
  state : state;
}


let rec service t threads =
  lwt t, threads = nchoose_fold react threads t in
  service t threads

and react t = function
  | peer, `Connect -> Lwt.return (t, [recv t.srv])
  | peer, `Disconnect -> Lwt.return (t, [recv t.srv])
  | peer, `Message msg -> react_msg t peer msg

and react_msg t peer = function
  | `Id ->
    let ack_id = `AckId { Proto_b.worker_id = t.worker_id; user = t.user } in
    lwt () = send t.srv peer ack_id in
    Lwt.return (t, [recv t.srv])

  | `Heel task_id -> assert false
  | `InformPeerHosts _ -> assert false

  | `Working (task_id, working_msg) ->
    lwt t, result =
      match t.state with
        | `Working working -> (
            let open Working in
            if task_id = working.task_id then
              react_working_msg t working working_msg
            else
              Lwt.return (t, (`Error "busy/working on another task"))
          )

        | `Available -> Lwt.return (t, `Error "available")
    in
    lwt () = send t.srv peer result in
    Lwt.return (t, [recv t.srv])

and react_working_msg t working = function
  | `BestSplit ->
    let result = best_split working in
    Lwt.return (t, result)

  | `Sample    -> Lwt.return (sample t working)
  | `Ascend    -> Lwt.return (ascend t working)
  | `Push p    -> Lwt.return (push t working p)
  | `Descend d -> Lwt.return (descend t working d)
  | `CopyFeatures cf -> Lwt.return (copy_features t working cf)

  | _ -> assert false

and best_split working =
  let open Working in
  match working.subsets with
    | `LR _ | `N -> `Error "best_split: not in S state"

    | `S (subset, _) ->
      let result =
        D_feat_map.best_split_of_features working.feature_map
          working.splitter
      in
      let split_opt =
        match result with
          | Some (_,_, split) -> Some split
          | None -> None
      in
      `AckBestSplit split_opt

and sample t working =
  let open Working in
  match working.subsets with
    | `N ->
      let subset = Sampler.array (
          fun ~index ~value ->
            (* sample half the data that is also in the current fold *)
            working.fold_set.(index) && value mod 2 = 0
        ) working.sampler in
      let working = { working with subsets = `S ( subset, `N ) } in
      let t = { t with state = `Working working } in
      t, `AckSample

    | `LR _ | `S _ ->
      t, `Error "sample: not in N state"

and ascend t working =
  let open Working in
  match working.subsets with
    | `LR (_, _, subsets ) -> (
        let working = { working with subsets } in
        let t = { t with state = `Working working } in
        t, `AckAscend
      )

    | `S _ | `N  ->
      t, `Error "ascend: not in LR state"


and push t working {Proto_t.split; feature_id} =
  let open Working in
  match working.subsets with
    | `S (subset, _) -> (
        try
          let splitting_feature = D_feat_map.find_i working.feature_map
              feature_id in
          let left, right =
            Tree.partition_observations subset splitting_feature split in
          let subsets = `LR (left, right, working.subsets) in
          let working = { working with subsets } in
          let t = { t with state = `Working working } in
          t, `AckPush

        with D_feat_map.FeatureIdNotFound _ ->
          t, `Error (sp "push: feature %d not found" feature_id)
      )

    | `LR _ | `N  ->
      t, `Error "push: not in S state"

and descend t working direction =
  let open Working in
  match working.subsets with
    | `S _ | `N ->
      t, `Error "descend: not in LR state"

    | `LR ( left, right, _) ->
      let subsets =
        let subset =
          match direction with
            | `Left -> left
            | `Right -> right
        in
        `S (subset , working.subsets)
      in
      let working = { working with subsets } in
      let t = { t with state = `Working working } in
      t, `AckDescend

and copy_features t working list =
  let open Working in
  let feature_map = List.fold_left (
    fun t (feature_id, vector) ->
      D_feat_map.add working.feature_map feature_id vector `Active
    ) working.feature_map list in
  let working = { working with feature_map } in
  let t = { t with state = `Working working } in
  t, `AckCopyFeatures


let worker detach : unit =
  (* igore SIGPIPE's *)
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;

  (* create the working directory [$HOME/.dawg] if one does not
     already exist *)
  let home = Unix.getenv "HOME" in
  let dot_dawg = Filename.concat home ".dawg" in
  Utils.mkdir_else_exit dot_dawg;

  (* read the existing worker id (stored in [$HOME/.dawg/worker-id])
     or create a random worker id, and write it to that path *)
  let worker_id =
    let worker_id_path = Filename.concat dot_dawg "worker-id" in
    if Sys.file_exists worker_id_path then
      Utils.bi_read_from_file Proto_b.read_worker_id worker_id_path
    else
      (* create the worker id *)
      let worker_id = "asdfasdfasdf" in
      Utils.bi_write_to_file Proto_b.write_worker_id worker_id_path worker_id;
      worker_id
  in

  let srv =
    try
      create ()
    with Unix.Unix_error( _, "bind", _) ->
      (* TODO: connect to the process, to get its id and user *)
      Printf.printf "another process already has port %d bound\n%!" port;
      exit 1
  in

  let threads = [recv srv]  in
  let t = {
    srv;
    worker_id;
    user = Unix.getlogin ();
    state = `Available

  } in
  Lwt_main.run (service t threads)

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
