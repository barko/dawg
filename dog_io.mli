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
module RW : sig
  type t
  val create : string -> int -> string -> t
  (* [create path size dog_t_blob] creates a [t], backed by a
     memory-mapped file whose path is [path], and whose size is [size]
     in bytes. [dog_t_blob] is the serialization of [Dog_t.t]. *)

  val write: t -> int -> string -> unit
  (* [write t pos vec] writes string [vec] to [t] at position
     [pos]. *)

  val read: t -> offset:int -> length:int -> string

  val array : t -> UInt8Array.t
end
