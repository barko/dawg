(* Support binarization of ordinal features onto two labels *)
type binarization_threshold = [
  | `LTE of float (* positive label is LTE, negative case is GT *)
  | `GTE of float (* positive label is GTE, negative case is LT *)
  | `LT of float (* positive label is LT, negative case is GTE *)
  | `GT of float (* positive label is GT, negative case is LTE *)
]

val probability : float -> float
class splitter :
  float option -> binarization_threshold option
    -> float array -> Feat.afeature -> int -> int
    -> Loss.splitter
