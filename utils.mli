val divide_up : int -> int -> int

val rand_bools : float -> int -> bool list

val string_of_bools : bool list -> string

val repeat : int -> (unit -> unit) -> unit

val time : (unit -> 'a) -> 'a * float

val f_xor : bool -> bool -> bool

val f_and_not : bool -> bool -> bool

module XMap : functor ( M : Map.OrderedType ) -> sig
  include Map.S with type key = M.t
  val find_opt : key -> 'a t -> 'a option
  val find_assert : key -> 'a t -> 'a

end

module Int : sig
  type t = int
  val compare : int -> int -> int
end

val width : int -> int
  (* cardinality -> num bits required to represent it *)

val num_bytes : int -> int
  (* cardinality -> num_bytes required to represent it *)

val fold_range : (int -> 'a -> 'a) -> start:int -> finix:int -> 'a -> 'a
val iter_range : (int -> unit) -> start:int -> finix:int -> unit

val mkdir_else_exit : string -> unit

val bi_read_from_file : (Bi_inbuf.t -> 'a) -> string -> 'a
(* read a biniou-serialized value from a file *)

val bi_write_to_file : (Bi_outbuf.t -> 'a -> unit) -> string -> 'a -> unit
(* write a biniou-serialized value to a file *)
