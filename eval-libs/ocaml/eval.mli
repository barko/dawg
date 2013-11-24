type sq
val square_evaluator : string -> sq

type feature_key = [ `Id of int | `Name of string ]
type feature_value = [ `Float of float | `String of string ]

exception FeatureNotFound of feature_key
exception TypeMismatch of (feature_key * feature_value)
exception CategoryNotFound of (feature_key * string)

type feature_vector = (feature_key * feature_value) list
val square_eval : sq -> feature_vector -> float

val logistic_evaluator : 'a -> unit
