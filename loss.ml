exception WrongTargetType
exception BadTargetDistribution

type metrics = {
  (* string rendering of working-fold loss metrics *)
  s_wrk : string;

  (* string rendering of validation-fold loss metrics *)
  s_val : string;

  (* on the basis of validation-fold metrics, should we stop
     learning? *)
  has_converged : bool;

  (* what is the validation loss? *)
  val_loss : float
}

class type splitter = object
  method clear : unit
  method best_split : Feat.afeature -> (float * Proto_t.split) option
  method boost : float array -> [ `NaN | `Ok ]
  method update_with_subset : bool array -> unit
  method first_tree : bool array -> Model_t.l_tree
  method metrics : (int -> bool) -> metrics
  method num_observations : int
  method write_model : Model_t.c_trees -> Model_t.feature list -> Bi_outbuf.t ->
    unit
end
