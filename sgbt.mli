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
  excluded_feature_name_regexp_opt : Pcre.regexp option;
  fold_feature_opt : Feat_utils.feature_descr option;
  boost_feature_opt : Feat_utils.feature_descr option;
  max_trees_opt : int option;
  binarization_threshold_opt : Logistic.binarization_threshold option;
  feature_monotonicity : feature_monotonicity;
}

val learn : conf -> unit
