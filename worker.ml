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

module Configured = struct
  type t = {
    task_id : Proto_t.task_id;
    y_feature_id : Proto_t.feature_id;
    fold_feature_id_opt : Proto_t.feature_id option;
    splitter : Logistic.splitter;
    (* < best_split : Feat.afeature -> (float * Proto_t.split) option >; *)
    feature_map : D_feat_map.t;
    sampler : Sampler.t;
    fold : int array;
  }
end

module Learning = struct
  type t = {
    task_id : Proto_t.task_id;
    y_feature_id : Proto_t.feature_id;
    fold_feature_id_opt : Proto_t.feature_id option;
    splitter : Logistic.splitter;
    feature_map : D_feat_map.t;
    sampler : Sampler.t;
    fold : int array;

    fold_set : bool array;
    subsets : subset_list;
  }
end

type state = [
  | `Available
  (* worker is free to do work for any master that cares for its services *)

  | `Acquired of Proto_t.task_id
  (* configured is setting up the task *)

  | `Configured of Configured.t
  (* worker has successfully setup the task; that means
     it has at least the target (y) feature, and the fold
     feature (if one is required) *)

  | `Learning of Learning.t
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

  | `InformPeerHosts _ -> assert false

  | `Acquire task_id -> (
      match t.state with
        | `Available ->
          let t, response = react_available t task_id in
          lwt () = send t.srv peer response in
          Lwt.return (t, [recv t.srv])

        | `Acquired _
        | `Configured _
        | `Learning _ ->
          let response = `AckAcquire false in (* not available *)
          lwt () = send t.srv peer response in
          Lwt.return (t, [recv t.srv])

    )

  | `Acquired (s_task_id, acquired_msg) -> (
      match t.state with
        | `Acquired task_id ->
          if s_task_id = task_id then
            let t, response = react_acquired t task_id acquired_msg in
            lwt () = send t.srv peer response in
            Lwt.return (t, [recv t.srv])
          else
            let response = `Error "busy with another task" in
            lwt () = send t.srv peer response in
            Lwt.return (t, [recv t.srv])

        | _ ->
          let response = `Error "not acquired" in
          lwt () = send t.srv peer response in
          Lwt.return (t, [recv t.srv])

    )

  | `Configured (task_id, configured_msg) ->
    assert false

  | `Learning (task_id, learning_msg) -> (
    match t.state with
        | `Learning learning -> (
            let open Learning in
            if task_id = learning.task_id then
              lwt t, result = react_learning_msg t learning learning_msg in
              lwt () = send t.srv peer result in
              Lwt.return (t, [recv t.srv])

            else
              let result = `Error "busy on another task" in
              lwt () = send t.srv peer result in
              Lwt.return (t, [recv t.srv])
          )

        | _ ->
          let response = `Error "not learning" in
          lwt () = send t.srv peer response in
          Lwt.return (t, [recv t.srv])
    )

and react_available t task_id =
  let t = { t with state = `Acquired task_id } in
  t, `AckAcquire true

and react_acquired t task_id = function
  | `Configure setup ->
    let open Proto_t in
    let path = task_id in (* TODO *)
    let dog_ra = Dog_io.RW.create path setup.dog_file_size setup.dog_t in
    let feature_map = D_feat_map.create dog_ra in

    (* add the target feature *)
    let y_feature_id, y_feature_vector = setup.y_feature in
    let feature_map = D_feat_map.add feature_map y_feature_id
        y_feature_vector `Inactive in

    (* now extract it *)
    let y_feature =
      try
        D_feat_map.find_i feature_map y_feature_id
      with Dog_io.RW.FeatureIdNotFound _ ->
        assert false
    in

    let num_observations = Dog_io.RW.num_observations dog_ra in
    let splitter = new Logistic.splitter y_feature num_observations in
    let sampler = Sampler.create num_observations in

    let random_state = Random.State.make setup.random_seed in
    Sampler.shuffle sampler random_state;

    match setup.fold_feature_opt with
      | None ->
        (* we don't have a fold feature; randomly assign folds *)
        let fold = Sampler.array (
            fun ~index ~value ->
              value mod setup.num_folds
          ) sampler in

        let configured = Configured.({
            task_id;
            y_feature_id;
            fold_feature_id_opt = None;
            feature_map;
            sampler;
            splitter;
            fold;
          }) in
        { t with state = `Configured configured }, `AckConfigure

      | Some (fold_feature_id, fold_feature_vector) ->
        let feature_map = D_feat_map.add feature_map fold_feature_id
            fold_feature_vector `Inactive in

        try
          let fold_feature = D_feat_map.find_i feature_map fold_feature_id in
          match Feat_utils.folds_of_feature ~n:num_observations
                  ~num_folds:setup.num_folds fold_feature with
            | `Folds fold ->

              let configured =
              let open Configured in
              { task_id;
                y_feature_id;
                fold_feature_id_opt = Some fold_feature_id;
                feature_map;
                sampler;
                splitter;
                fold
              }
            in
            { t with state = `Configured configured }, `AckConfigure

          | `TooManyOrdinalFolds cardinality ->
            let err = sp "the cardinality of ordinal fold feature (%d) is \
                          too large relative to the number of folds (%d)"
                cardinality setup.num_folds in
            t, `Error err

          | `CategoricalCardinalityMismatch cardinality ->
            let err = sp "the cardinality of the categorical fold feature (%d) \
                          must equal the number of folds (%d)"
                cardinality setup.num_folds in
            t, `Error err

      with Dog_io.RW.FeatureIdNotFound _ ->
        t, `Error (sp "fold feature %d not found" fold_feature_id)


and react_learning_msg t learning = function
  | `BestSplit ->
    let result = best_split learning in
    Lwt.return (t, result)

  | `Sample    -> Lwt.return (sample t learning)
  | `Ascend    -> Lwt.return (ascend t learning)
  | `Push p    -> Lwt.return (push t learning p)
  | `Descend d -> Lwt.return (descend t learning d)
  | `CopyFeatures cf ->
    Lwt.return (copy_features t learning cf)

and best_split learning =
  let open Learning in
  match learning.subsets with
    | `LR _ | `N -> `Error "best_split: not in S state"

    | `S (subset, _) ->
      let result =
        D_feat_map.best_split_of_features learning.feature_map
          learning.splitter
      in
      let split_opt =
        match result with
          | Some (_,_, split) -> Some split
          | None -> None
      in
      `AckBestSplit split_opt

and sample t learning =
  let open Learning in
  match learning.subsets with
    | `N ->
      let subset = Sampler.array (
          fun ~index ~value ->
            (* sample half the data that is also in the current fold *)
            learning.fold_set.(index) && value mod 2 = 0
        ) learning.sampler in
      let learning = { learning with subsets = `S ( subset, `N ) } in
      let t = { t with state = `Learning learning } in
      t, `AckSample

    | `LR _ | `S _ ->
      t, `Error "sample: not in N state"

and ascend t learning =
  let open Learning in
  match learning.subsets with
    | `LR (_, _, subsets ) -> (
        let learning = { learning with subsets } in
        let t = { t with state = `Learning learning } in
        t, `AckAscend
      )

    | `S _ | `N  ->
      t, `Error "ascend: not in LR state"


and push t learning {Proto_t.split; feature_id} =
  let open Learning in
  match learning.subsets with
    | `S (subset, _) -> (
        try
          let splitting_feature = D_feat_map.find_i learning.feature_map
              feature_id in
          let left, right =
            Tree.partition_observations subset splitting_feature split in
          let subsets = `LR (left, right, learning.subsets) in
          let learning = { learning with subsets } in
          let t = { t with state = `Learning learning } in
          t, `AckPush

        with D_feat_map.FeatureIdNotFound _ ->
          t, `Error (sp "push: feature %d not found" feature_id)
      )

    | `LR _ | `N  ->
      t, `Error "push: not in S state"

and descend t learning direction =
  let open Learning in
  match learning.subsets with
    | `S _ | `N ->
      t, `Error "descend: not in LR state"

    | `LR ( left, right, _) ->
      let subsets =
        let subset =
          match direction with
            | `Left -> left
            | `Right -> right
        in
        `S (subset , learning.subsets)
      in
      let learning = { learning with subsets } in
      let t = { t with state = `Learning learning } in
      t, `AckDescend

and copy_features t learning list =
  let open Learning in
  let feature_map = List.fold_left (
    fun t (feature_id, vector) ->
      D_feat_map.add learning.feature_map feature_id vector `Active
    ) learning.feature_map list in
  let learning = { learning with feature_map } in
  let t = { t with state = `Learning learning } in
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
