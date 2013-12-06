let pr = Printf.printf

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
      pr "%S is not a valid time-delta sepcifier\n%!" str;
      exit 1

    | Some delta_seconds ->
      let now = Unix.gettimeofday () in
      now +. delta_seconds


module LogisticSGBT = Sgbt.Make(Logistic)
module SquareSGBT   = Sgbt.Make(Square)

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
    binarization_threshold_opt
  =

  if max_depth < 1 then (
    pr "max-depth must be greater than 0\n%!";
    exit 1
  );

  if 0.0 >= initial_learning_rate || initial_learning_rate > 1.0 then (
    pr "initial-learning-rate must be in (0,1]\n%!";
    exit 1
  );

  if min_convergence_rate < 0.0 then (
    pr "min-convergence-rate must be non-negative\n%!";
    exit 1
  );

  if num_folds < 2 then (
    pr "num-folds must be greater than one\n%!";
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
    pr "file %S does not exist!\n%!" dog_file_path;
    exit 1
  );

  let output_dir_name = Filename.dirname output_file_path in
  if not (Sys.file_exists output_dir_name) then (
    pr "output directory %S does not exist!\n%!" output_dir_name;
    exit 1
  );

  let regexp_opt =
    match excluded_feature_name_regexp with
      | Some re -> (
          try
            Some (Pcre.regexp re)
          with Pcre.Error _ ->
            pr "bad regulalar expression %S\n%!" re;
            exit 1
        )
      | None -> None
  in

  let loss_type =
    match loss_type_s with
      | "logistic"-> `Logistic
      | "square" -> `Square
      | _ ->
        pr "bad loss type %S\n%!" loss_type_s;
        exit 1
  in

  let module Learner = (
    val
      match loss_type with
        | `Logistic -> (module LogisticSGBT : Sgbt.SGBT)
        | `Square -> (module SquareSGBT)
  ) in

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

  let open Learner in
  let conf =
    let open Sgbt in
    {
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
    }
  in

  (* we only create [LS] so as to generically catch exceptions below *)
  let module LS = (
    val
      match loss_type with
        | `Logistic -> (module Logistic : Loss.LOSS)
        | `Square -> (module Square : Loss.LOSS)
  ) in

  try
    Learner.learn conf
  with
    | LS.WrongTargetType ->
      pr "target %s is not binary\n%!"
        (Feat_map.string_of_feature_descr y);
      exit 1
    | LS.BadTargetDistribution ->
      pr "target %s has a bad distribution\n%!"
        (Feat_map.string_of_feature_descr y);


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

    let binarization_threshold =
      let doc = "binarize real targets with this threshold \
                 (implies loss=\"logistic\")" in
      Arg.( value & opt (some float) None &
            info ["b";"binarization-threshold"] ~docv:"FLOAT" ~doc)
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
            binarization_threshold
         ),
    Term.info "learn" ~doc
  in
  [learn_cmd]
