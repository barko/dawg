(* write-only *)
module WO : sig
  type t

  val create : string -> int -> t

  val add_feature : t -> Feat.lfeature -> unit

  val close_writer : t -> unit
end

(* read-only *)
module RO : sig
  type t
  val create : string -> t
  val dog : t -> Dog_t.t
  val array : t -> UInt8Array.t

  (* where does the vector sequence end?  This the offset of the last
     byte of the last vector in the sequence, plus one *)
  val end_array_offset : t -> int

end

(* read and append *)
module RA : sig
  type t
  val create : string -> int -> t
  (* [create path size] creates a [t], backed by a memory-mapped file
     whose path is [path], and whose size is [size] in bytes. *)

  exception TooFull
  val append : t -> string -> int
  (* [append t vec] appends string [vec] to [t], raising [TooFull] if
     there's not enough space to do so. Return the position at which
     the encoded vector was written. *)

  val array : t -> UInt8Array.t
end
