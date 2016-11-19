module IntMap = Utils.IntMap
module ISet = Utils.XSet(Utils.Int)
module SSet = Utils.XSet(String)

let pr fmt = Printf.printf (fmt ^^ "\n%!")

let seconds_of_string = function
  | RE (float as number : float ) ( 's' | 'S' ) -> (* seconds *)
    Some number

  | RE (float as number : float ) ( 'm' | 'M' ) -> (* minutes *)
    Some (number *. 60.)

  | RE (float as number : float ) ( 'h' | 'H' ) -> (* hours *)
    Some (number *. 60. *. 60.)

  | RE (float as number : float ) ( 'd' | 'D' ) -> (* days *)
    Some (number *. 60. *. 60. *. 24.)

  | _ -> None

let deadline_of_string str =
  match seconds_of_string str with
    | None ->
      pr "%S is not a valid time-delta sepcifier" str;
      exit 1

    | Some delta_seconds ->
      let now = Unix.gettimeofday () in
      now +. delta_seconds

let feature_descr_of_args name_opt id_opt =
  match name_opt, id_opt with
    | None, None -> None
    | Some _, Some _ ->
      pr "can only specify the a feature by its name or its id, not both";
      exit 1

    | Some name, None -> Some (`Name name)
    | None, Some id -> Some (`Id id)

let learn
    dog_file_path
    y_name_opt
    y_id_opt
    max_depth
    initial_learning_rate
    min_convergence_rate
    num_folds
    fold_feature_name_opt
    fold_feature_id_opt
    convergence_rate_smoother_forgetful_factor
    deadline
    output_file_path
    excluded_feature_name_regexp
    loss_type_s
    max_trees_opt
    lte_binarization_threshold
    gte_binarization_threshold
    feature_name_positive
    feature_name_negative
    feature_id_positive
    feature_id_negative
    exclude_nan_target
    exclude_inf_target
  =

  if max_depth < 1 then (
    pr "max-depth must be greater than 0";
    exit 1
  );

  if 0.0 >= initial_learning_rate || initial_learning_rate > 1.0 then (
    pr "initial-learning-rate must be in (0,1]";
    exit 1
  );

  if min_convergence_rate < 0.0 then (
    pr "min-convergence-rate must be non-negative";
    exit 1
  );

  if num_folds < 2 then (
    pr "num-folds must be greater than one";
    exit 1
  );

  if convergence_rate_smoother_forgetful_factor <= 0.0 ||     (* forget everything *)
     convergence_rate_smoother_forgetful_factor >= 1.0 then ( (* forget nothing *)
    pr "forgetful-factor must be between 0 and 1, exclusive";
    exit 1
  );

  (match max_trees_opt with
    | None -> ()
    | Some max_trees ->
      if max_trees < 1 then (
        pr "the maximum number of trees must be positive";
        exit 1
      )
  );

  let deadline =
    match deadline with
      | Some str -> Some (deadline_of_string str)
      | None -> None (* no deadline *)
  in

  if not (Sys.file_exists dog_file_path) then (
    pr "file %S does not exist!" dog_file_path;
    exit 1
  );

  let output_dir_name = Filename.dirname output_file_path in
  if not (Sys.file_exists output_dir_name) then (
    pr "output directory %S does not exist!" output_dir_name;
    exit 1
  );

  let regexp_opt =
    match excluded_feature_name_regexp with
      | Some re -> (
          try
            pr "Exclude feature regexp: %S\n%!" re;
            Some (Pcre.regexp re)
          with Pcre.Error _ ->
            pr "bad regulalar expression %S" re;
            exit 1
        )
      | None -> None
  in

  let loss_type =
    match loss_type_s with
      | "logistic"-> `Logistic
      | "square" -> `Square
      | _ ->
        pr "bad loss type %S" loss_type_s;
        exit 1
  in

  let y =
    match feature_descr_of_args y_name_opt y_id_opt with
      | None ->
        pr "no target feature specified";
        exit 1
      | Some y -> y
  in

  let fold_feature_opt =
    feature_descr_of_args fold_feature_name_opt fold_feature_id_opt
  in

  let binarization_threshold_opt =
    match lte_binarization_threshold, gte_binarization_threshold with
      | Some _, Some _ ->
        pr "cannot specify both -gte and -lte binarization";
        exit 1
      | Some lte, None -> Some (`LTE lte)
      | None, Some gte -> Some (`GTE gte)
      | None, None -> None
  in

  let positive_feature_ids =
    List.fold_left (fun accu id -> ISet.add id accu) ISet.empty feature_id_positive
  in
  let negative_feature_ids =
    List.fold_left (fun accu id -> ISet.add id accu) ISet.empty feature_id_negative
  in
  let invalid_ids = ISet.inter positive_feature_ids negative_feature_ids in
  let positive_feature_names =
    List.fold_left (fun accu name -> SSet.add name accu) SSet.empty feature_name_positive
  in
  let negative_feature_names =
    List.fold_left (fun accu name -> SSet.add name accu) SSet.empty feature_name_negative
  in
  let invalid_names = SSet.inter positive_feature_names negative_feature_names in

  let () =
    if not(ISet.is_empty invalid_ids) || not(SSet.is_empty invalid_names) then
      begin
        prerr_endline "[ERROR] Some are features declared to be both positive and negative:";
        prerr_endline
          (String.concat ", " (List.map string_of_int (ISet.to_list invalid_ids)));
        prerr_endline
          (String.concat ", " (SSet.to_list invalid_names));
      end
  in

  let feature_monotonicity = [] in
  let feature_monotonicity =
    List.rev_append
      (List.map (fun id -> ((`Id id), `Positive)) (ISet.to_list positive_feature_ids))
      feature_monotonicity in
  let feature_monotonicity =
    List.rev_append
      (List.map (fun id -> ((`Id id), `Negative)) (ISet.to_list negative_feature_ids))
      feature_monotonicity in
  let feature_monotonicity =
    List.rev_append
      (List.map (fun name -> ((`Name name), `Positive)) (SSet.to_list positive_feature_names))
      feature_monotonicity in
  let feature_monotonicity =
    List.rev_append
      (List.map (fun name -> ((`Name name), `Negative)) (SSet.to_list negative_feature_names))
      feature_monotonicity in

  let conf =
    let open Sgbt in
    {
      loss_type;
      dog_file_path;
      y;
      num_folds;
      min_convergence_rate;
      convergence_rate_smoother_forgetful_factor;
      initial_learning_rate;
      max_depth;
      deadline;
      output_file_path;
      excluded_feature_name_regexp_opt = regexp_opt;
      fold_feature_opt;
      max_trees_opt;
      binarization_threshold_opt;
      feature_monotonicity;
      exclude_nan_target;
      exclude_inf_target;
    }
  in

  try
    Sgbt.learn conf
  with
    | Loss.WrongTargetType ->
      pr "target %s is not binary\n%!"
        (Feat_utils.string_of_feature_descr y);
      exit 1
    | Loss.BadTargetDistribution ->
      pr "target %s has a bad distribution\n%!"
        (Feat_utils.string_of_feature_descr y);


open Cmdliner
let commands =
  let learn_cmd =
    let doc = "Learn a stochastic gradient boosting tree model. \
               The algorithm performs k-fold cross validation, training k \
               seperate sub-models which are averaged in a final model." in

    let input_file_path =
      let doc = "path of the input dog file" in
      Arg.(required & opt (some string) None &
           info ["i";"input"] ~docv:"PATH" ~doc)
    in
    let y_name =
      let doc = "the name of the response feature" in
      Arg.(value & opt (some string) None &
           info ["y";"response-name"] ~docv:"NAME" ~doc)
    in

    let y_id =
      let doc = "the id of the response feature" in
      Arg.(value & opt (some int) None &
           info ["y-id";"response-id"] ~docv:"INT" ~doc)
    in

    let max_depth =
      let doc = "the maximum depth of any tree in the learned model. \
                (Tree stumps have depth=1)." in
      Arg.(value & opt int 2 & info ["d";"max-depth"] ~docv:"INT" ~doc)
    in

    let learning_rate =
      let doc = "the learning rate (aka shrinkage factor)" in
      Arg.(value & opt float 0.05 &
           info ["r";"learning-rate"] ~docv:"FLOAT" ~doc)
    in

    let min_convergence_rate =
      let doc = "learn until the convergence rate of the validation loss is \
                 below this value" in
      Arg.(value & opt float 0.001 &
           info ["c";"min-convergence-rate"] ~docv:"FLOAT" ~doc)
    in

    let num_folds =
      let doc = "the number of folds, k." in
      Arg.(value & opt int 2 & info ["k";"num-folds"] ~docv:"INT" ~doc)
    in

    let fold_feature_name =
      let doc = "rather than assigning observations to folds randomly, \
                 use the values of the feature with this name to infer \
                 the assignment. The feature will be excluded from \
                 learning." in
      Arg.(value & opt (some string) None &
           info ["f";"fold-feature-name"] ~docv:"STRING" ~doc )
    in

    let fold_feature_id =
      let doc = "rather than assigning observations to folds randomly, \
                 use the values of the feature with this id to infer \
                 the assignment. The feature will be excluded from \
                 learning." in
      Arg.(value & opt (some int) None &
           info ["f-id";"fold-feature-id"] ~docv:"INT" ~doc )
    in

    let feature_name_positive =
      let doc = "Constrain feature to have a monotonic non-decreasing marginal \
                 relationship with the response variable" in
      Arg.(value & opt_all string [] &
           info ["p";"positive-feature-name"] ~docv:"STRING" ~doc )
    in
    let feature_name_negative =
      let doc = "Constrain feature to have a monotonic non-increasing marginal \
                 relationship with the response variable" in
      Arg.(value & opt_all string [] &
           info ["n";"negative-feature-name"] ~docv:"STRING" ~doc )
    in
    let feature_id_positive =
      let doc = "Constrain feature to have a monotonic non-decreasing marginal \
                 relationship with the response variable" in
      Arg.(value & opt_all int [] &
           info ["p-id";"positive-feature-id"] ~docv:"STRING" ~doc )
    in
    let feature_id_negative =
      let doc = "Constrain feature to have a monotonic non-increasing marginal \
                 relationship with the response variable" in
      Arg.(value & opt_all int [] &
           info ["n-id";"negative-feature-id"] ~docv:"STRING" ~doc )
    in

    let convergence_rate_smoother_forgetful_factor =
      let doc = "The loss convergence rate measured over the test fold \
                 does not decrease monotonically.  A forgetful recursive \
                 least-squares model is used to smooth it, and the convergence \
                 rate test is applied to this smoothed value.  The forgetful \
                 factor, a value in (0,1) modulates the degree to which the \
                 loss of older iterations are forgotten in the smoothed convergence \
                 rate.  Values that are closer to zero are more forgetful, and \
                 therefore less smooth." in
      Arg.(value & opt float 0.9 &
           info ["m";"forgetful-factor"] ~docv:"FLOAT" ~doc)
    in

    let deadline =
      let doc = "stop learning after this much time has expired, resulting in \
          the best model learned to that point.  The deadline is expressed in \
          terms of a time offset, with \"s\" denoting seconds, \"m\" minutes, \
          \"h\" hours, and \"d\" days.  For example, \"1.2h\" represents a deadline \
          of 1.2 hours (equivalently \"72m\") from now." in
      Arg.(value & opt (some string) None &
           info ["t";"deadline"] ~docv:"DELTA-TIME" ~doc)
    in

    let output_file_path =
      let doc = "path of the output model file" in
      Arg.(required & opt (some string) None &
           info ["o";"output"] ~docv:"PATH" ~doc)
    in

    let excluded_feature_name_regexp =
      let doc = "regular expression of names of features to exclude from \
                 learning" in
      Arg.(value & opt (some string) None &
           info ["x";"exclude"] ~docv:"STRING" ~doc)
    in

    let loss_type =
      let doc = "the kind of model to learn: either \"logistic\" for binary \
                 classifier, or \"square\" for regression" in
      Arg.(value & opt string "logistic" &
           info ["l";"loss"] ~docv:"STRING" ~doc)
    in

    let max_trees =
      let doc = "the maximum number of trees per fold sub-model" in
      Arg.( value & opt (some int) None &
            info ["t";"max-trees"] ~docv:"INT" ~doc)
    in

    let binarize_lte =
      let doc = "provide a binariziation threshold to an ordinal prediction \
                 target.  The generated labels are \"LTE\" and \"GT\"" in
      Arg.( value & opt (some float) None &
            info ["L"; "lte"; "binarization-threshold-lte"] ~docv:"FLOAT" ~doc)
    in

    let binarize_gte =
      let doc = "provide a binariziation threshold to an ordinal prediction \
                 target.  The generated labels are \"GTE\" and \"LT\"" in
      Arg.( value & opt (some float) None &
            info ["G"; "gte"; "binarization-threshold-gte"] ~docv:"FLOAT" ~doc)
    in

    let exclude_nan_target =
      let doc = "Exclude row where the target is a floating point nan" in
      Arg.(value & opt bool false & info ["exclude-nan-target"] ~docv:"BOOL" ~doc)
    in
    let exclude_inf_target =
      let doc = "Exclude row where the target is a floating point infinity" in
      Arg.(value & opt bool false & info ["exclude-inf-target"] ~docv:"BOOL" ~doc)
    in

    Term.(pure learn $
            input_file_path $
            y_name $
            y_id $
            max_depth $
            learning_rate $
            min_convergence_rate $
            num_folds $
            fold_feature_name $
            fold_feature_id $
            convergence_rate_smoother_forgetful_factor $
            deadline $
            output_file_path $
            excluded_feature_name_regexp $
            loss_type $
            max_trees $
            binarize_lte $
            binarize_gte $
            feature_name_positive $
            feature_name_negative $
            feature_id_positive $
            feature_id_negative $
            exclude_nan_target $
            exclude_inf_target
         ),
    Term.info "learn" ~doc
  in
  [learn_cmd]
