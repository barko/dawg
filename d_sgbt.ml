(** Friedman's Stochastic Gradient Boosting Trees *)

let pr = Printf.printf
let sp = Printf.sprintf

let random_seed = [| 9271 ; 12074; 3; 12921; 92; 763 |]

type loss_type = [ `Logistic | `Square ]

open Sgbt

type t = {
  (* how many observations are there in the training set? *)
  n : int;

  (* how do we read features from a file? *)
  dog_rw : Dog_io.RW.t;

  (* how do we find best splits? *)
  splitter : Loss.splitter;

  (* how do we evaluate a tree over all the observations in the
     training set? *)
  eval : (Dog_t.feature_id -> Feat.afeature) -> Model_t.l_tree -> float array;

  (* how do we create random paritions of the training set (and
     subsets thereof) ? *)
  sampler : Sampler.t;

  (* what fold does each observation in the training set belong
     to? *)
  folds : int array;

  (* what is the list of active workers, used to compute best splits
     over subsets of the features ? *)
  active_workers : LP_tcp.Client.t list ;

  (* what is the learning tasks unique id ? *)
  task_id : string;
}

let exceed_max_trees num_iters max_trees_opt =
  match max_trees_opt with
    | None -> false
    | Some max_trees ->
      num_iters >= max_trees


let reset t first_tree =
  let gamma = t.eval (
      fun feature_id ->
        let open Dog_io.RW in
        let q_feature = Utils.IntMap.find feature_id
            t.dog_rw.feature_id_to_feature in
        q_to_a_feature t.dog_rw q_feature
    ) first_tree in
  t.splitter#clear;
  (match t.splitter#boost gamma with
    | `NaN -> assert false
    | `Ok -> ()
  )

type learning_iteration = {
  (* iteration number; also, number of trees *)
  i : int ;

  (* what is the fold currently being held out for the purpose of
     identifying the optimal termination point? This fold is the
     validation fold. *)
  fold : int;

  (* is the observation in the 'working folds' or the 'validation
     fold' ?  *)
  fold_set : bool array;

  learning_rate : float;
  first_loss : float;
  prev_loss : float;
  trees : Model_t.l_tree list;
  convergence_rate_smoother : Rls1.t;
  random_state : Random.State.t;

  (* what is the first tree? *)
  first_tree : Model_t.l_tree;

  (* should learning stop, returning the best model produced so
     far? *)
  timeout : unit -> bool
}

let rec learn_with_fold_rate conf t iteration =
  (* TODO : rpc Sample *)

  match_lwt D_tree.make t.task_id conf.max_depth t.active_workers 0 with
    | None ->
      print_endline "converged: no more trees";
      let r = `Converged (iteration.learning_rate, iteration.trees) in
      Lwt.return r

    | Some tree ->
      let shrunken_tree = Tree.shrink iteration.learning_rate tree in
      let gamma = t.eval (Dog_io.RW.a_find t.dog_rw) shrunken_tree in

      match t.splitter#boost gamma with
        | `NaN -> (
            pr "diverged: nan\n%!";
            cut_learning_rate conf t iteration
          )

        | `Ok ->
          let { Loss.s_wrk; s_val; has_converged; val_loss } =
            t.splitter#metrics (Array.get iteration.fold_set) in

          (* compute convergence rate and smooth it *)
          let convergence_rate =
            (iteration.prev_loss -. val_loss) /. val_loss in
          let convergence_rate_smoother = Rls1.add
              iteration.convergence_rate_smoother convergence_rate in
          let convergence_rate_hat = Rls1.theta convergence_rate_smoother in

          pr "iter % 3d % 7d  %s %s    %+.4e %+.4e\n%!"
            iteration.fold
            iteration.i
            s_wrk
            s_val
            convergence_rate
            convergence_rate_hat;

          if has_converged then (
            pr "converged: metrics inidicate continuing is pointless\n";
            let r = `Converged (iteration.learning_rate, iteration.trees) in
            Lwt.return r
          )
          else if val_loss >= 2.0 *. iteration.prev_loss then (
            pr "diverged: loss rose dramatically!\n";
            cut_learning_rate conf t iteration
          )
          else if iteration.timeout () then (
            pr "timeout!\n";
            let r = `Timeout iteration.trees in
            Lwt.return r
          )
          else if exceed_max_trees iteration.i conf.max_trees_opt then (
            (* convergence, kinda *)
            pr "tree limit constraint met\n";
            let r = `Converged (iteration.learning_rate, iteration.trees) in
            Lwt.return r
          )
          else if convergence_rate_hat < conf.min_convergence_rate then (
            (* convergence! *)
            pr "converged: rate exceeded\n";
            let r = `Converged (iteration.learning_rate, iteration.trees) in
            Lwt.return r
          )
          else
            (* continue learning *)

            let iteration = {
              iteration with
                prev_loss = val_loss;
                i = iteration.i + 1;
                trees = shrunken_tree :: iteration.trees;
                convergence_rate_smoother;
            } in

            learn_with_fold_rate conf t iteration


and cut_learning_rate conf t iteration =
  (* cut the learning rate in half and try again *)
  let learning_rate = 0.5 *. iteration.learning_rate in
  pr "reducing learning rate from %f to %f\n"
    iteration.learning_rate learning_rate;
  let shrunken_first_tree = Tree.shrink learning_rate iteration.first_tree in
  reset t shrunken_first_tree;
  let new_random_seed = [| Random.int 10_000 |] in
  let iteration = {
    iteration with
      learning_rate;
      random_state = Random.State.make new_random_seed;
      prev_loss = iteration.first_loss;
      i = 1;
      trees = [shrunken_first_tree]
  } in
  learn_with_fold_rate conf t iteration


let timeout = 1000. (* TODO *)

let learn_with_fold conf t task_id active_workers fold
    initial_learning_rate deadline =
  lwt _ =
    let request =
    let open Proto_t in
    `Configured (task_id, `Learn { fold; learning_rate = initial_learning_rate })
    in
    let is_response_valid = function
      | `AckLearn -> true
      | _ -> false
    in
    Worker_client.broad_send_recv active_workers timeout
      request is_response_valid
  in

  let fold_set = Array.init t.n (fun i -> t.folds.(i) <> fold) in
  let leaf0 = t.splitter#first_tree fold_set in
  let shrunken_leaf0 = Tree.shrink initial_learning_rate leaf0 in
  reset t shrunken_leaf0;

  let { Loss.s_wrk; s_val; val_loss = first_val_loss } =
    t.splitter#metrics (Array.get fold_set) in

  pr "fold % 3d          %s %s\n%!" fold s_wrk s_val;

  let new_random_seed = [| Random.int 10_000 |] in

  let timeout =
    match conf.deadline with
      | None -> fun () -> false (* don't timeout *)
      | Some deadline ->
        fun () ->
          Unix.gettimeofday () >= deadline (* obey deadline *)
  in

  let convergence_rate_smoother = Rls1.create
      conf.convergence_rate_smoother_forgetful_factor in

  let iteration = {
    i = 1;
    fold;
    fold_set;
    first_loss = first_val_loss;
    prev_loss = first_val_loss;
    first_tree = leaf0;
    trees = [shrunken_leaf0];
    learning_rate = initial_learning_rate;
    convergence_rate_smoother;
    random_state = Random.State.make new_random_seed;
    timeout
  } in

  learn_with_fold_rate conf t iteration

let find_all dog_rw =
  let open Dog_io.RW in
  function
    | `Id feature_id -> (
        try
          let feature = find dog_rw feature_id in
          [feature]
        with FeatureIdNotFound _ ->
          []
      )

    | `Name feature_name ->
      Utils.IntMap.fold (
        fun _ feature features ->
          match Feat_utils.name_of_feature feature with
            | Some fn ->
              if fn = feature_name then
                feature :: features
              else
                features
            | None -> features
      ) dog_rw.feature_id_to_feature []


let folds_of_feature feature_descr dog_rw ~num_folds =
  let fold_feature =
    match find_all dog_rw feature_descr with
      | [ feature ] -> feature
      | [] ->
        pr "no feature %s\n%!"
          (Feat_utils.string_of_feature_descr feature_descr);
        exit 1
      | _ :: _ :: _ ->
        pr "more than one feature satisfying %s\n%!"
          (Feat_utils.string_of_feature_descr feature_descr);
        exit 1
  in

  let fold_feature = Dog_io.RW.q_to_a_feature dog_rw fold_feature in
  let num_observations = dog_rw.Dog_io.RW.num_observations in
  match Feat_utils.folds_of_feature ~n:num_observations
          ~num_folds fold_feature with
    | `Folds fold ->
      fold, fold_feature

    | `TooManyOrdinalFolds cardinality ->
      pr "the cardinality of ordinal fold feature (%d) is \
          too large relative to the number of folds (%d)\n%!"
        cardinality num_folds;
      exit 1

    | `CategoricalCardinalityMismatch cardinality ->
      pr "the cardinality of the categorical fold feature (%d) \
          must equal the number of folds (%d)\n%!"
        cardinality num_folds;
      exit 1


let learn conf =
  let dog_rw, dog_file_size, dog_t = Dog_io.RW.create conf.dog_file_path None in

  let num_observations = dog_rw.Dog_io.RW.num_observations in

  assert ( conf.num_folds > 0 );
  if conf.num_folds >= num_observations then (
    pr "number of folds %d must be smaller than the number of observations \
        %d\n%!" conf.num_folds num_observations;
    exit 1
  );

  let y_feature =
    let y_features = find_all dog_rw conf.y in
    match y_features with
      | [] ->
        pr "target %s not found\n%!"
          (Feat_utils.string_of_feature_descr conf.y);
        exit 1

      | one :: two :: _ ->
        pr "more than one target feature %s\n%!"
          (Feat_utils.string_of_feature_descr conf.y);
        exit 1

      | [ y_feature ]-> y_feature
  in

  let y_feature = Dog_io.RW.q_to_a_feature dog_rw y_feature in
  let y_feature_id = Feat_utils.id_of_feature y_feature in
  let y_feature_blob = Dog_io.RW.read dog_rw y_feature_id in

  (* remove excluded features, if any *)
  (*
  let feature_map =
    match conf.excluded_feature_name_regexp_opt with
      | None -> feature_map

      | Some rex ->
          let is_excluded feature =
            match Feat_utils.name_of_feature feature with
            | None -> false (* anonymous features cannot be excluded *)
            | Some name ->
                Pcre.pmatch ~rex name
        in
        let feature_map = D_feat_map.deactivate_if feature_map is_excluded in
        feature_map
  in

  pr "features: active=%d inactive=%d\n%!"
    (D_feat_map.num_active feature_map) (D_feat_map.num_inactive feature_map);

  *)

  let random_state = Random.State.make random_seed in
  let sampler = Sampler.create num_observations in
  Sampler.shuffle sampler random_state;

  let folds, fold_feature_opt =
    match conf.fold_feature_opt with
      | None ->
        (* we don't have a fold feature; randomly assign folds *)
        let fold = Sampler.array (
            fun ~index ~value ->
              value mod conf.num_folds
          ) sampler in
        fold, None

      | Some feature_descr ->
        let folds, fold_feature =
          folds_of_feature feature_descr dog_rw num_observations in

        let fold_feature_id = Feat_utils.id_of_feature fold_feature in
        if fold_feature_id = y_feature_id then (
          pr "fold feature and target feature must be different\n%!";
          exit 1
        );

        let fold_feature_blob = Dog_io.RW.read dog_rw fold_feature_id in
        folds, Some (fold_feature_id, fold_feature_blob)
  in

  let splitter : Loss.splitter =
    match conf.loss_type with
      | `Logistic ->
        new Logistic.splitter conf.binarization_threshold_opt y_feature
          num_observations

      | `Square ->
        new Square.splitter y_feature num_observations
  in

  let eval = Tree.mk_eval num_observations in
  let task_id = "UUID-TODO" in

  lwt active_workers =
    let open Worker_client in
    lwt worker = create "localhost" in
    let workers = [ worker ] in

    let request = `Acquire task_id in
    let timeout = 10. in
    let is_response_valid = function
      | `AckAcquire _ -> true
      | _ -> false
    in
    lwt responses =
      broad_send_recv_nx workers timeout request is_response_valid in

    (*
       e := network error
       t := timeout
       a := worker available
       na : worker not available
    *)

    let e, t, a, na = List.fold_left (
        fun (e, t, a, na) (worker, response) ->
          match response with
            | `E -> worker :: e, t, a, na
            | `T -> e, worker :: t, a, na
            | `R (_, `AckAcquire true) -> e, t, worker :: a, na
            | `R (_, `AckAcquire false) -> e, t, a, worker :: na
            | `R (_, _) -> assert false
      ) ([], [], [], []) responses
    in

    (* TODO: close *)
    (* TODO: log *)

    (match a with
      | [] ->
        print_endline "no workers available!"; exit 1
      | _ -> ();
    );

    Lwt.return a
  in

  let d_conf =
    let open Proto_t in
    {
      task_id;
      loss_type = conf.Sgbt.loss_type;
      num_folds = conf.Sgbt.num_folds;
      y_feature = (y_feature_id, y_feature_blob);
      fold_feature_opt;
      random_seed;
      dog_t;
      dog_file_size;
    }
  in

  let request = `Acquired (task_id, `Configure d_conf) in
  let is_response_valid = function
    | `AckConfigure -> true
    | _ -> false
  in
  let open Worker_client in
  lwt _ = broad_send_recv active_workers timeout request is_response_valid in

  let t = {
    n = num_observations;
    dog_rw;
    splitter;
    eval;
    sampler;
    folds;
    task_id;
    active_workers;
  } in

  let rec loop fold trees_list initial_learning_rate =
    if fold < conf.num_folds then

      match_lwt learn_with_fold conf t task_id active_workers
                  fold initial_learning_rate 0.0 with

        | `Converged (effective_learning_rate, trees) ->
          let trees_list = List.rev_append trees trees_list in
          (* set the initial learning rate of the next fold model to the
             effective learning rate of the previous one; this means that
             the learning rate can gradually decrease from one fold to the
             next, as a learning attempt on folds fail (diverge) *)
          loop (fold + 1) trees_list effective_learning_rate

        | `Timeout trees ->
          (* time's up!  only include [trees] if no other trees were
             previously learned. *)
          match trees_list with
            | [] -> Lwt.return trees
            | _ -> Lwt.return trees_list

    else
      Lwt.return trees_list
  in

  (* combine the model learned for each fold into a mega-model,
     where these sequence of trees are simply averaged (bagged!) *)
  lwt trees = loop 0 [] conf.initial_learning_rate in
  let trees =
    let fold_weight = 1.0 /. (float conf.num_folds) in
    List.rev_map (
      fun tree ->
        Tree.shrink fold_weight tree
    ) trees
  in

  let i_find_by_id = Dog_io.RW.a_find dog_rw in
  let trees, features = Model_utils.l_to_c i_find_by_id trees in

  (* write model file *)
  let () =
    (* open channel *)
    let ouch = open_out conf.output_file_path in

    (* open output buffer *)
    let out_buf = Bi_outbuf.create_channel_writer ouch in

    (* write model to buffer *)
    splitter#write_model trees features out_buf;

    (* flush buffer *)
    Bi_outbuf.flush_channel_writer out_buf;

    (* close channel *)
    close_out ouch
  in
  Lwt.return ()

let learn conf =
  try_lwt
    learn conf
  with Worker_client.ProtocolError (_, _, from_worker) ->
    (match from_worker with
      | `Error msg ->
        pr "ProtocolError: %s\n%!" msg;
      | _ ->
        print_endline "ProtocolError"
    );
    exit 1
