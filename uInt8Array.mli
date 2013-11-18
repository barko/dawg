open Bigarray
type t =  (int, int8_unsigned_elt, c_layout) Array1.t
val map_to_list : (int -> 'a) -> t -> 'a list
val length : t -> int
val create : int -> t
val sub : t ->  int -> int -> t
val char_iter_i : (int -> char -> unit) -> t -> unit
