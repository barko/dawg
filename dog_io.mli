type w
val create_writer : string -> int -> w

val add_feature : w -> Feat.lfeature -> unit

val close_writer : w -> unit

type r
val create_reader : string -> r
val dog : r -> Dog_t.t
val array : r -> UInt8Array.t

(* where does the vector sequence end?  This the offset of the last
   byte of the last vector in the sequence, plus one *)
val end_array_offset : r -> int

val close_reader : r -> unit
