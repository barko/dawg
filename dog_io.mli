(* write-only *)
type w
val create_writer : string -> int -> w

val add_feature : w -> Feat.lfeature -> unit

val close_writer : w -> unit

(* read-only *)
type r
val create_reader : string -> r
val dog : r -> Dog_t.t
val array : r -> UInt8Array.t

(* where does the vector sequence end?  This the offset of the last
   byte of the last vector in the sequence, plus one *)
val end_array_offset : r -> int

val close_reader : r -> unit

(* read and append *)
module RA : sig
  type t
  val create : string -> int -> t
  (* [create path size] creates a [t], backed by a memory-mapped file
     whose path is [path], and whose size is [size] in bytes. *)

  exception TooFull
  val append : t -> string -> t
  (* [append t vec] appends string [vec] to [t], raising [TooFull] if
     there's not enough space to do so. *)

  val array : t -> UInt8Array.t
end
