type afeature = (Vec.t, Vec.t) Dog_t.feature
type lfeature = ((int * int) list, int list) Dog_t.feature
type ifeature = Dog_t.ifeature

let feature_id : ('a, 'b) Dog_t.feature -> Dog_t.feature_id =
  let open Dog_t in
  function
    | `Cat cat_feature -> cat_feature.c_feature_id
    | `Ord ord_feature -> ord_feature.o_feature_id
