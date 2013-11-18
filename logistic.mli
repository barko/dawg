type metrics
val string_of_metrics : metrics -> string
val loss : metrics -> float
val probability : float -> float

type model

exception WrongTargetType
exception BadTargetDistribution

class splitter : Feat.afeature -> int -> Calc.map option ->
  object
    method clear : unit
    method best_split : Feat.afeature -> (float * Split.t) option
    method boost : float array -> [ `NaN | `Ok ]
    method update_with_subset : bool array -> unit
    method first_tree : bool array -> Model_t.l_tree
    method metrics : (int -> bool) -> metrics option
    method num_observations : int
    method write_model : Model_t.c_trees -> Model_t.feature list ->
      Bi_outbuf.t -> unit
  end

