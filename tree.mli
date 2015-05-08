type feature_monotonicity_map = Dog_t.monotonicity Utils.IntMap.t

val partition_observations :
  bool array ->
  Feat.afeature ->
  Proto_t.split -> bool array * bool array

type m = {
  max_depth : int;
  feature_map : Feat_map.t;
  feature_monotonicity_map : feature_monotonicity_map;
  splitter : Loss.splitter;
}

val make : m -> int -> bool array -> Model_t.l_tree option
val shrink : float -> Model_t.l_tree -> Model_t.l_tree
val mk_eval : int ->
  ( (Dog_t.feature_id -> Feat.afeature) ->  Model_t.l_tree -> float array )
