val pr : ('a, out_channel, unit) format -> 'a
val epr : ('a, out_channel, unit) format -> 'a

val add_to : float ref -> float -> unit

val divide_up : int -> int -> int

val rand_bools : float -> int -> bool list

val string_of_bools : bool list -> string

val repeat : int -> (unit -> unit) -> unit

val time : (unit -> 'a) -> 'a * float

val f_xor : bool -> bool -> bool

val f_and_not : bool -> bool -> bool

module type XMapS = sig
  include Map.S
  val find_opt : key -> 'a t -> 'a option
  val find_assert : key -> 'a t -> 'a
end
module type XSetS = sig
  include Set.S
  val to_list : t -> elt list
end

module XMap : functor ( M : Map.OrderedType ) -> XMapS with type key = M.t
module XSet : functor ( M : Set.OrderedType ) -> XSetS with type elt = M.t

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

val abspath : string -> string
  (* returns a non-normalized absolute path *)

module List : sig
  include module type of List
  val iteri : (int -> 'a -> unit) -> 'a list -> unit
  val first : int -> 'a list -> 'a list
end

module IntMap : XMapS with type key = int
