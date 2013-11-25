type square
type logistic

type evaluator = [ `Square of square | `Logistic of logistic ]

val create : string -> evaluator
(* [create path] create a model evaluator from a model file at
   [path]. *)

type feature_key = [ `Id of int | `Name of string ]
type feature_value = [ `Float of float | `String of string ]
type feature_vector = (feature_key * feature_value) list
(* A feature vector consists of key-value pairs. A key identifies a
   feature by name or id.  A value of ordinal features is of type
   [float].  The value of categorical features is of type
   [string]. [`Float 0.0] is the default value of ordinal features.
   For categorical features with an anonymous category, the anonymous
   category is the default. A category must be specified for a
   categorical feature that do not have an anonymous category
   (otherwise, a [CategoryMissing _] exception is raised. *)

exception FeatureNotFound of feature_key
(* raised when a feature was provided in a feature vector which is not
   required by the model to be evaluated. *)

exception TypeMismatch of (feature_key * feature_value)
(* raised when the model expects a feature of one type, but a value of
   another type is provided in the feature vector.  The two types are
   ordinal and categorical, represented by values [`Float _] and
   [`String _], respectively. *)

exception CategoryNotFound of (feature_key * string)
(* raised when a category provided in a feature vector is unknown to
   the model.  In other words, the categorical feature for which the
   category was provided as value, does not have that category in its
   set of categories. *)

exception CategoryMissing of feature_key
(* raised when the feature vector lacks a value for a categorical
   feature.  Note that [`Float 0.0] is the default value of ordinal
   features. *)

val eval : evaluator -> feature_vector -> float
(* evaluate the model using over input [feature_vector] *)

val positive_category : logistic -> string
(* return the positive category; without inversion (through the
   [invert] function), this is the category associated with
   probability equal to one. *)

val invert : logistic -> logistic
(* invert the polarity of the logistic model *)
