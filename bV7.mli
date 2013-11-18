open Bigarray
type uint8_array = (int, int8_unsigned_elt, c_layout) Array1.t

(* the type of bitvectors encoded into [int8_unsigned] bigarray's *)
type e = {
  offset : int;
  (* where in {a} does the bitvector encoding begin? *)

  num_bytes : int;
  (* how many bytes are used to encode the bitvector in UInt8Array [a] ? *)

  e_length : int;
  (* how many bits are in the bitvector? *)

  a : uint8_array
}

type d
(* the type of decoded bitvectors; useful for temporary values, that
   need not ever be encoded. *)

val encode : d -> e
(* encode a decoded bitvector *)

val decode : e -> d
(* decode an encoded bitvector *)

val string_of_d : d -> string
(* pretty-print a decoded bitvector *)

val decoded_of_bools : bool list -> d
(* convert a list of bools to the equivalent bitvector *)

val bools_of_decoded : d -> bool list
(* convert a decoded bitvector into a list of bools *)

val decoded_of_indexes : bool -> int list -> int -> d
(* [decoded_of_indexes bit indexes length] creates a bit vector whose
   length is [length], and has value [bit] at indexes [indexes], and
   value [not bit] everywhere else. *)

val work_factor : d -> int
(* an indication of the work involved in processing this t *)

val density : d -> float
(* what is the density of the bitvector, defined as the ratio of its
   length and its work factor? *)

val d_length : d -> int
(* what is the length of the decoded bitvector? *)

val e_length : e -> int
(* what is the length of the encoded bitvector? *)

val char_iter_i : (int -> char -> unit) -> e -> unit
(* [char_iter_i f e] applies function [f] to the bytes underlying
   encoded bitvector [e] *)

val uniform : bool -> int -> d
(* [uniform value length] creates a a bitvector of length [length] and
   whose value is uniformly [value]. *)

val negate : d -> d
(* negate the bits in a bitvector *)

val iter_d_true :
  (bit_index:int -> length:int -> unit) ->
  (bit_index:int -> unit) -> d -> unit

val fold_d_true :
  (bit_index:int -> length:int -> 'a -> 'a) ->
  (bit_index:int -> 'a -> 'a) -> d -> 'a -> 'a

val iter_e :
  (bit_index:int -> value:bool -> length:int -> unit) ->
  (bit_index:int -> value:bool -> unit) -> e -> unit


type binary_op = XOR | OR | AND | AND_NOT
val binary : binary_op -> d -> d -> d

val binary_de : binary_op -> d -> e -> d

(* [binary_de_iter_true_rev fu fl op d e] iterates over the elements
   of a bitvector.  This vector is the result of applying the operator
   [op] to input bitvectors [d] and [e].  It calls function [fu] for
   regions of that bitvector which are uniformly true (bit = '1') and
   calls [fl] for bits that are true but which are in mixed regions of
   the bitvector. The length of the uniform region will always be a
   multiple of 7. The functions [fu] and [fl] are presented with bit
   indexes in descending order. Bits of the bitvector which are false
   (bit = '0') are ignored. *)
val binary_de_iter_true_rev :
  (bit_index:int -> length:int -> unit) ->
  (bit_index:int -> unit) ->
  binary_op -> d -> e -> unit

(* fold version of [binary_de_iter_true_rev] *)
val binary_de_fold_true :
  (bit_index:int -> length:int -> 'a -> 'a) ->
  (bit_index:int -> 'a -> 'a) ->
  binary_op -> d -> e -> 'a -> 'a
