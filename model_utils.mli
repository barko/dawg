val l_to_c : (int -> ('a, 'a) Dog_t.feature) -> Model_t.l_trees ->
  Model_t.c_trees * Model_t.feature list

val rle_to_array : ('a, Model_t.direction_rle) Model_t.trees ->
  ('a, Model_t.category_direction array) Model_t.trees
(* tranform trees, decoding the run-length-encoded representation of
   the category directions, if any *)

val category_array_of_rle : Model_t.direction_rle ->
  Model_t.category_direction array
