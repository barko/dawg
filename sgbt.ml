(** Friedman's Stochastic Gradient Boosting Trees *)

let random_seed = [| 9271 ; 12074; 3; 12921; 92; 763 |]

type loss_type = [ `Logistic | `Square ]

type feature_monotonicity = (Feat_utils.feature_descr * Dog_t.monotonicity) list
type conf = {
  loss_type : loss_type;
  dog_file_path : string;
  num_folds : int;
  min_convergence_rate : float;
  initial_learning_rate : float;
  y : Feat_utils.feature_descr;
  max_depth : int;
  convergence_rate_smoother_forgetful_factor : float;
  deadline : float option;
  output_file_path : string;
  selected_feature_name_regexp_opt : Pcre.regexp option;
  excluded_feature_name_regexp_opt : Pcre.regexp option;
  fold_feature_opt : Feat_utils.feature_descr option;
  weight_feature_opt : Feat_utils.feature_descr option;
  max_trees_opt : int option;
  max_gamma_opt : float option;
  binarization_threshold_opt : Logistic.binarization_threshold option;
  feature_monotonicity : feature_monotonicity;
  exclude_nan_target : bool;
  exclude_inf_target : bool;
}

type t = {
  (* how many observations are there in the training set? *)
  n_rows : int;

  (* what is the map of feature id's to features? *)
  feature_map : Feat_map.t;

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
}

let exceed_max_trees num_iters max_trees_opt =
  match max_trees_opt with
    | None -> false
    | Some max_trees ->
      num_iters >= max_trees


let reset t first_tree =
  let gamma = t.eval (Feat_map.a_find_by_id t.feature_map) first_tree in
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
  in_set : bool array;
  out_set : bool array;

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
  let feature_monotonicity_map =
    Feat_map.assoc t.feature_map conf.feature_monotonicity
  in
  let m = {
    Tree.max_depth = conf.max_depth;
    feature_map = t.feature_map;
    feature_monotonicity_map;
    splitter = t.splitter;
  } in

  (* draw a random subset of this fold *)
  Sampler.shuffle t.sampler iteration.random_state;
  let { in_set; out_set } = iteration in
  let in_subset = Sampler.array (
      fun ~index ~value ->
        (* sample half the data that is also in the current fold *)
        in_set.(index) && value mod 2 = 0
    ) t.sampler in

  match Tree.make m 0 in_subset with
    | None ->
      print_endline "converged: no more trees";
      `Converged (iteration.learning_rate, iteration.trees)

    | Some tree ->
      let shrunken_tree = Tree.shrink iteration.learning_rate tree in
      let gamma = t.eval (Feat_map.a_find_by_id t.feature_map) shrunken_tree in

      match t.splitter#boost gamma with
        | `NaN -> (
            Utils.pr "diverged: nan\n%!";
            cut_learning_rate conf t iteration
          )

        | `Ok ->
          let { Loss.s_wrk; s_val; has_converged; val_loss } =
            t.splitter#metrics ~in_set ~out_set in

          (* compute convergence rate and smooth it *)
          let convergence_rate =
            (iteration.prev_loss -. val_loss) /. val_loss in
          let convergence_rate_smoother = Rls1.add
              iteration.convergence_rate_smoother convergence_rate in
          let convergence_rate_hat = Rls1.theta convergence_rate_smoother in

          Utils.pr "iter % 3d % 7d  %s %s    %+.4e %+.4e\n%!"
            iteration.fold
            iteration.i
            s_wrk
            s_val
            convergence_rate
            convergence_rate_hat;

          if has_converged then (
            Utils.pr "converged: metrics indicate continuing is pointless\n";
            `Converged (iteration.learning_rate, iteration.trees)
          )
          else if val_loss >= 2.0 *. iteration.prev_loss then (
            Utils.pr "diverged: loss rose dramatically!\n";
            cut_learning_rate conf t iteration
          )
          else if iteration.timeout () then (
            Utils.pr "timeout!\n";
            `Timeout iteration.trees
          )
          else if exceed_max_trees iteration.i conf.max_trees_opt then (
            (* convergence, kinda *)
            Utils.pr "tree limit constraint met\n";
            `Converged (iteration.learning_rate, iteration.trees)
          )
          else if convergence_rate_hat < conf.min_convergence_rate then (
            (* convergence! *)
            Utils.pr "converged: rate exceeded\n";
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
  Utils.pr "reducing learning rate from %f to %f\n"
    iteration.learning_rate learning_rate;
  let first_tree = iteration.first_tree in
  reset t first_tree;
  let new_random_seed = [| Random.int 10_000 |] in
  let iteration = {
    iteration with
      learning_rate;
      random_state = Random.State.make new_random_seed;
      prev_loss = iteration.first_loss;
      i = 1;
      trees = [first_tree]
  } in
  learn_with_fold_rate conf t iteration

let learn_with_fold conf t fold initial_learning_rate deadline =
  let in_set = Array.init t.n_rows (fun i -> let n = t.folds.(i) in n >= 0 && n <> fold) in
  let out_set = Array.init t.n_rows (fun i -> t.folds.(i) = fold) in

  let first_tree = t.splitter#first_tree in_set in
  reset t first_tree;

  let { Loss.s_wrk; s_val; val_loss = first_val_loss } =
    t.splitter#metrics ~in_set ~out_set
  in

  Utils.pr "fold % 3d          %s %s\n%!" fold s_wrk s_val;

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
    in_set;
    out_set;
    first_loss = first_val_loss;
    prev_loss = first_val_loss;
    first_tree;
    trees = [first_tree];
    learning_rate = initial_learning_rate;
    convergence_rate_smoother;
    random_state = Random.State.make new_random_seed;
    timeout
  } in

  learn_with_fold_rate conf t iteration

let folds_and_weights conf sampler feature_map n a_y_feature =
  let y_feature_id = Feat_utils.id_of_feature a_y_feature in
  let folds, feature_map = match conf.fold_feature_opt with
    | None ->
      (* randomly choose fold assignments *)
      let folds = Sampler.array (
          fun ~index ~value ->
            value mod conf.num_folds
        ) sampler in
      folds, feature_map

    | Some fold_feature ->

      match Feat_map.find feature_map fold_feature with
        | [] ->
          Utils.epr "[ERROR] feature %S to be used for fold assignment not found\n%!"
            (Feat_utils.string_of_feature_descr fold_feature);
          exit 1

        | fold_features ->
          let num_fold_features = List.length fold_features in
          if num_fold_features > 1 then
            Utils.pr "[WARNING] There are %d fold features satisfying %s\n%!"
              num_fold_features
              (Feat_utils.string_of_feature_descr fold_feature);

          (* arbitrarily pick the first fold feature (should there be
             more than one) *)
          let i_fold_feature = List.hd fold_features in
          let fold_feature_id = Feat_utils.id_of_feature i_fold_feature in
          if fold_feature_id = y_feature_id then (
            Utils.epr "[ERROR] Fold feature and target feature must be different\n%!";
            exit 1
          );

          (* fold feature found; use it to construct folds *)
          let a_fold_feature = Feat_map.i_to_a feature_map i_fold_feature in
          match Feat_utils.folds_of_feature ~n ~num_folds:conf.num_folds
                  a_fold_feature with
            | `TooManyCategoricalFolds cardinality ->
              Utils.epr "[ERROR] The cardinality of categorical feature %s is %d, which is \
                  too small relative to the number of folds %d\n%!"
                (Feat_utils.string_of_feature_descr fold_feature)
                cardinality conf.num_folds;
              exit 1

            | `TooManyOrdinalFolds cardinality ->
              Utils.epr "[ERROR] The cardinality of ordinal feature %s is %d, which is \
                  too small relative to the number of folds %d\n%!"
                (Feat_utils.string_of_feature_descr fold_feature)
                cardinality conf.num_folds;
              exit 1

            | `Folds folds ->
              (* remove all the fold_features from the [feature_map] *)
              let feature_map =
                List.fold_left (
                  fun feature_map_0 a_fold_feature ->
                    let fold_feature_id = Feat_utils.id_of_feature
                      a_fold_feature in
                    let fold_feature_name = Feat_utils.name_of_feature a_fold_feature in
                    let () = match fold_feature_name with
                      | Some name ->
                        Utils.epr "[INFO] excluding fold feature %s (id: %d)\n%!"
                          name fold_feature_id;
                      | None ->
                        Utils.epr "[INFO] excluding nameless fold feature (id: %d)\n%!"
                          fold_feature_id;
                    in
                    Feat_map.remove feature_map_0 fold_feature_id
                ) feature_map fold_features  in
              folds, feature_map
  in

  let weights = match conf.weight_feature_opt with
    | None ->
      (* all weights are set to 1.0 *)
      let weights = Sampler.array (
          fun ~index ~value ->
            1.0
        ) sampler in
      weights

    | Some weight_feature ->
      match Feat_map.find feature_map weight_feature with
        | [] ->
          Utils.epr "[ERROR] feature %S to be used for observation weights not found\n%!"
            (Feat_utils.string_of_feature_descr weight_feature);
          exit 1

        | weight_features ->
          let num_weight_features = List.length weight_features in
          if num_weight_features > 1 then (
            Utils.epr "[ERROR] There are %d weight features satisfying %s\n%!"
              num_weight_features
              (Feat_utils.string_of_feature_descr weight_feature);
            exit 1
          );

          (* there is exactly one weight feature) *)
          let i_weight_feature = List.hd weight_features in
          let weight_feature_id = Feat_utils.id_of_feature i_weight_feature in
          if weight_feature_id = y_feature_id then (
            Utils.epr "[ERROR] weights feature and target feature must be different\n%!";
            exit 1
          );

          (* weight feature found; use it *)
          let a_weight_feature = Feat_map.i_to_a feature_map i_weight_feature in
          let weights = Feat_utils.weights_of_afeature a_weight_feature in
          Array.iteri (fun i w ->
            match classify_float w with
              | FP_infinite ->
                Utils.epr "[ERROR] Infinite weight for observation %d: %f" i w;
                exit 2
              | FP_nan ->
                Utils.epr "[ERROR] NaN weight for observation %d" i;
                exit 2
              | FP_zero -> ()
              | _ when w < 0.0 ->
                Utils.epr "[ERROR] Negative weight for observation %d: %f" i w;
                exit 2
              | _ -> ()
          ) weights;

          weights
  in

  let y_array = Feat_utils.array_of_afeature a_y_feature in

  let () =
    if conf.exclude_inf_target || conf.exclude_nan_target then (
      Utils.epr "[INFO] exclude_inf_target=%b exclude_nan_target=%b\n%!"
        conf.exclude_inf_target conf.exclude_nan_target;
      match y_array with
        | `Float y_values ->
          let count_inf = ref 0 in
          let count_nan = ref 0 in
          Array.iteri (fun i (_, y_value) ->
        (* Let's exclude any bad data by setting the fold to -1. *)
            match classify_float y_value with
            | FP_infinite when conf.exclude_inf_target ->
              folds.(i) <- -1;
              incr count_inf
            | FP_nan when conf.exclude_nan_target ->
              folds.(i) <- -1;
              incr count_nan
            | _ -> ()
          ) y_values;
          let n_rows = Array.length y_values in
          if !count_inf <> 0 then
            Utils.epr "[WARNING] excluding %d inf rows out of %d\n%!" !count_inf n_rows;
          if !count_nan <> 0 then
            Utils.epr "[WARNING] excluding %d nan rows out of %d\n%!" !count_nan n_rows
        | _ -> ()
    )
  in
  folds, weights, feature_map

let learn conf =
  let dog_reader = Dog_io.RO.create conf.dog_file_path in
  let feature_map = Feat_map.create dog_reader in

  let n_rows =
    let dog = Dog_io.RO.dog dog_reader in
    dog.Dog_t.num_observations
  in
  (* let n = num_observations in *)

  assert ( conf.num_folds > 0 );
  if conf.num_folds >= n_rows then (
    Utils.epr "[ERROR] number of folds %d must be smaller than the number of observations \
        %d\n%!" conf.num_folds n_rows;
    exit 1
  );

  let i_y_feature, a_y_feature =
    match Feat_map.find feature_map conf.y with
      | [] ->
        Utils.epr "[ERROR] target %s not found\n%!"
          (Feat_utils.string_of_feature_descr conf.y);
        exit 1

      | (_ :: _ :: _) as y_features ->
        Utils.epr "[ERROR] %d target features satisfying %s found; only one expected\n%!"
          (List.length y_features)
          (Feat_utils.string_of_feature_descr conf.y);
        exit 1

      | [i_y_feature] ->
        i_y_feature, Feat_map.i_to_a feature_map i_y_feature
  in

  (* remove target from the feature set *)
  let feature_map = Feat_map.remove feature_map
      (Feat_utils.id_of_feature i_y_feature) in

  let random_state = Random.State.make random_seed in
  let sampler = Sampler.create n_rows in
  Sampler.shuffle sampler random_state;

  let folds, weights, feature_map =
    folds_and_weights conf sampler feature_map n_rows a_y_feature
  in

  let feature_map, num_excluded_features =
    let num_excluded = ref 0 in

    (* remove not-selected features, if any *)
    let feature_map =
      match conf.selected_feature_name_regexp_opt with
        | None -> feature_map
        | Some rex ->
          let is_selected feature =
            match Feat_utils.name_of_feature feature with
            | None -> false (* anonymous features cannot be excluded *)
            | Some name ->
              let is_ex = Pcre.pmatch ~rex name in
              if is_ex then
                incr num_excluded;
              is_ex
          in
          let is_included _ feature =
            is_selected feature
          in
          Feat_map.filter feature_map is_included
    in

    (* remove excluded features, if any *)
    let feature_map =
      match conf.excluded_feature_name_regexp_opt with
        | None -> feature_map
        | Some rex ->
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
          Feat_map.filter feature_map is_included
    in
    feature_map, !num_excluded
  in

  let exclude_set = Array.init n_rows (fun i -> folds.(i) < 0) in
  let excluded_observations =
    Array.fold_left (fun n b -> if b then succ n else n) 0 exclude_set
  in
  Array.iteri (fun i ex -> if ex then weights.(i) <- 0.0) exclude_set;
  let num_observations = n_rows - excluded_observations in

  assert (
    (* make sure fold features (if any) are gone from the
       [feature_map] *)
    match conf.fold_feature_opt with
      | Some fold_feature -> (
          let ff = Feat_map.find feature_map fold_feature in
          match ff with
            | [] -> true
            | _ -> false
        )
      | None -> true
  );

  Utils.pr "features: included=%d excluded=%d\n%!"
    (Feat_map.length feature_map) num_excluded_features;

  let splitter : Loss.splitter =
    match conf.loss_type with
      | `Logistic ->
        new Logistic.splitter
          conf.max_gamma_opt conf.binarization_threshold_opt
          weights a_y_feature n_rows num_observations
      | `Square ->
        new Square.splitter
          conf.max_gamma_opt weights a_y_feature n_rows num_observations
  in

  let eval = Tree.mk_eval n_rows in

  let t = {
    n_rows;
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
