(** Friedman's Stochastic Gradient Boosting Trees *)

let pr = Printf.printf

let random_seed = [| 9271 ; 12074; 3; 12921; 92; 763 |]

type loss_type = [ `Logistic | `Square ]

type conf = {
  loss_type : loss_type;
  dog_file_path : string;
  num_folds : int;
  min_convergence_rate : float;
  initial_learning_rate : float;
  y : Feat_map.feature_descr;
  max_depth : int;
  convergence_rate_smoother_forgetful_factor : float;
  deadline : float option;
  output_file_path : string;
  excluded_feature_name_regexp_opt : Pcre.regexp option;
  fold_feature_opt : Feat_map.feature_descr option;
  max_trees_opt : int option;
}

module type SGBT = sig
  val learn : conf -> unit
end

type t = {
  (* how many observations are there in the training set? *)
  n : int;

  (* what is the map of feature id's to features? *)
  feature_map : Feat_map.t;

  (* how do we find best splits? *)
  splitter : Loss.splitter;

  (* how do we evaluate a tree over all the observations in the
     training set? *)
  eval : Model_t.l_tree -> float array;

  (* how do we create random paritions of the training set (and
     subsets thereof) ? *)
  sampler : Sampler.t;

  (* what fold does each observation in the training set belong
     to? *)
  folds : int array;
}

let exceed_max_trees num_iters max_trees_opt =
  match max_trees_opt with
    | None -> false
    | Some max_trees ->
      num_iters >= max_trees


let reset t first_tree =
  let gamma = t.eval first_tree in
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
  let m = {
    Tree.max_depth = conf.max_depth;
    feature_map = t.feature_map;
    splitter = t.splitter
  } in

  (* draw a random subset of this fold *)
  Sampler.shuffle t.sampler iteration.random_state;
  let sub_set = Sampler.array (
      fun ~index ~value ->
        (* sample half the data that is also in the current fold *)
        iteration.fold_set.(index) && value mod 2 = 0
    ) t.sampler in

  match Tree.make m 0 sub_set with
    | None ->
      print_endline "converged: no more trees";
      `Converged (iteration.learning_rate, iteration.trees)

    | Some tree ->
      let shrunken_tree = Tree.shrink iteration.learning_rate tree in
      let gamma = t.eval shrunken_tree in

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
            `Converged (iteration.learning_rate, iteration.trees)
          )
          else if val_loss >= 2.0 *. iteration.prev_loss then (
            pr "diverged: loss rose dramatically!\n";
            cut_learning_rate conf t iteration
          )
          else if iteration.timeout () then (
            pr "timeout!\n";
            `Timeout iteration.trees
          )
          else if exceed_max_trees iteration.i conf.max_trees_opt then (
            (* convergence, kinda *)
            pr "tree limit constraint met\n";
            `Converged (iteration.learning_rate, iteration.trees)
          )
          else if convergence_rate_hat < conf.min_convergence_rate then (
            (* convergence! *)
            pr "converged: rate exceeded\n";
            `Converged (iteration.learning_rate, iteration.trees)
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

let learn_with_fold conf t fold initial_learning_rate deadline =
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

let folds_of_feature_name conf sampler feature_map n y_feature_id =
  match conf.fold_feature_opt with
    | None ->
      (* randomly choose fold assignments *)
      let folds = Sampler.array (
          fun ~index ~value ->
            value mod conf.num_folds
        ) sampler in
      folds, feature_map

    | Some fold_feature ->

      match Feat_map.find feature_map fold_feature with
        | None ->
          pr "feature %S to be used for fold assignment not found\n%!"
            (Feat_map.string_of_feature_descr fold_feature);
          exit 1
        | Some i_fold_feature ->

          let fold_feature_id = Feat_utils.id_of_feature i_fold_feature in
          if fold_feature_id = y_feature_id then (
            pr "fold feature and target feature must be different\n%!";
            exit 1
          );

          (* fold feature found; use it to construct folds *)
          let a_fold_feature = Feat_map.i_to_a feature_map i_fold_feature in
          match Feat_utils.folds_of_feature ~n ~num_folds:conf.num_folds
                  a_fold_feature with
            | `TooManyOrdinalFolds cardinality ->
              pr "the cardinality of ordinal feature %s is %d, which is \
                  too large relative to the number of folds %d\n%!"
                (Feat_map.string_of_feature_descr fold_feature)
                cardinality conf.num_folds;
              exit 1

            | `CategoricalCardinalityMismatch cardinality ->
              pr "the cardinality of the categorical feature %s (%d) must \
                  equal the number of folds (%d)\n%!"
                (Feat_map.string_of_feature_descr fold_feature)
                cardinality conf.num_folds;
              exit 1

            | `Folds folds ->
              (* remove the fold_feature from the [feature_map] *)
              let fold_feature_id = Feat_utils.id_of_feature
                  a_fold_feature in
              let feature_map = Feat_map.remove feature_map
                  fold_feature_id in
              folds, feature_map


let learn conf =
  let dog_reader = Dog_io.RO.create conf.dog_file_path in
  let feature_map = Feat_map.create dog_reader in

  let num_observations =
    let dog = Dog_io.RO.dog dog_reader in
    dog.Dog_t.num_observations
  in
  let n = num_observations in

  assert ( conf.num_folds > 0 );
  if conf.num_folds >= n then (
    pr "number of folds %d must be smaller than the number of observations \
        %d\n%!" conf.num_folds n;
    exit 1
  );

  let y_feature =
    let y_feature_opt = Feat_map.find feature_map conf.y in
    match y_feature_opt with
      | None ->
        pr "target %s not found\n%!"
          (Feat_map.string_of_feature_descr conf.y);
        exit 1

      | Some y_feature ->
        Feat_map.i_to_a feature_map y_feature
  in

  (* remove target from the feature set *)
  let feature_map = Feat_map.remove feature_map
      (Feat_utils.id_of_feature y_feature) in

  (* remove excluded features, if any *)
  let feature_map, num_excluded_features =
    match conf.excluded_feature_name_regexp_opt with
      | None -> feature_map, 0

      | Some rex ->
        let num_excluded = ref 0 in
        let is_excluded feature =
          match Feat_utils.name_of_feature feature with
            | None -> false (* anonymous features cannot be excluded *)
            | Some name ->
              let is_ex = Pcre.pmatch ~rex name in
              if is_ex then
                incr num_excluded;
              is_ex
        in
        let is_included _ feature =
          not (is_excluded feature)
        in
        let feature_map = Feat_map.filter feature_map is_included in
        feature_map, !num_excluded
  in

  let random_state = Random.State.make random_seed in
  let sampler = Sampler.create n in
  Sampler.shuffle sampler random_state;

  let folds, feature_map =
    let y_feature_id = Feat_utils.id_of_feature y_feature in
    folds_of_feature_name conf sampler feature_map n y_feature_id
  in

  pr "features: included=%d excluded=%d\n%!"
    (Feat_map.length feature_map) num_excluded_features;

  let splitter : Loss.splitter =
    match conf.loss_type with
      | `Logistic ->
        new Logistic.splitter y_feature num_observations
      | `Square ->
        new Square.splitter y_feature num_observations
  in

  let eval = Tree.mk_eval num_observations feature_map in

  let t = {
    n;
    feature_map;
    splitter;
    eval;
    sampler;
    folds
  } in

  let rec loop fold trees_list initial_learning_rate =
    if fold < conf.num_folds then
      match learn_with_fold conf t fold initial_learning_rate 0.0 with

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
            | [] -> trees
            | _ -> trees_list

    else
      trees_list
  in

  (* combine the model learned for each fold into a mega-model,
     where these sequence of trees are simply averaged (bagged!) *)
  let trees = loop 0 [] conf.initial_learning_rate in
  let trees =
    let fold_weight = 1.0 /. (float conf.num_folds) in
    List.rev_map (
      fun tree ->
        Tree.shrink fold_weight tree
    ) trees
  in

  let trees, features = Model_utils.l_to_c feature_map trees in

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
  ()
