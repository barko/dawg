exception TypeMismatch
type t = [ `Float of float | `String of string | `Bool of bool | `Int of int ]
type map = t -> t

