(* Support binarization of ordinal features onto two labels *)
type binarization_threshold = [
  | `LTE of float (* positive label is LTE, negative case is GT *)
  | `GTE of float (* positive label is GTE, negative case is LT *)
]

val probability : float -> float
class splitter :
  float option -> binarization_threshold option
    -> bool array -> Feat.afeature -> int -> int
    -> Loss.splitter
