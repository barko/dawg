val max_length : int
val max_value : int

type t = UInt8Array.t

val write : (int * int) list -> t * int * int
(* write a list of runs (each a pair of length and value), and return
   a buffer into which those runs were written, in addition to the
   length of the rle vector (the number of elements it represents),
   and the number of runs written (the length of the input list) *)

val read_run : t -> int -> int * int * int
(* [read_run vec i] reads a run from an rlevec at offset [off], along
   with the number of bytes that were read *)

val write_run : t -> i:int -> length:int -> value:int -> int
(* [write_run vec ~i ~length ~value] writes a run [length, value]
   to rlevec [vec] at offset [i] *)

val iter : Vec.t -> (index:int -> length:int -> value:int -> 'a) -> unit
val fold : Vec.t -> (index:int -> length:int -> value:int -> 'a -> 'a) ->
  'a -> 'a
