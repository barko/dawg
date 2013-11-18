module Make : functor (L : Loss.LOSS) ->
sig
  type m = {
    max_depth : int;
    feature_map : Feat_map.t;
    splitter : L.splitter;
  }
  val make : m -> int -> bool array -> Model_t.l_tree option
  val shrink : float -> Model_t.l_tree -> Model_t.l_tree
  val mk_eval : int -> (Feat_map.t -> Model_t.l_tree -> float array)

end
