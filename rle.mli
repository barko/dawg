type 'a run = int * 'a
(* run length and value *)

type 'a runs = 'a run list
(* a sequence of runs *)

val encode_dense_rev : 'a list -> int * int * 'a runs
val encode_dense : 'a list -> int * int * 'a runs
(* [encode_dense values] returns the number of runs, the number of
   elements (vector length) and runs *)

val encode_sparse_rev : int -> (int * 'a) list -> 'a -> 'a runs
val encode_sparse : int -> (int * 'a) list -> 'a -> 'a runs
(* encode_sparse vec_length pairs zero] enocdes the list of
   element-index and value pairs [pairs] as a run length encoded
   vector whose length is [vec_length].  Gaps among pairs are assumed
   to have value [zero]. *)

val decode : 'a runs -> int * 'a list
(* [decode runs] returns the materialized sequence which the runs
   represent, along with the length of that sequence *)

val decode_rev : 'a runs -> int * 'a list
(* [decode_rev runs] returns the reverse of [decode runs], but is more
   efficient *)
