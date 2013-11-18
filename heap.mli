(* A totally ordered type and its comparison functions *)
module type ORDERED = sig
  type t
  val leq : t -> t -> bool
end

module Make : functor ( Ord : ORDERED ) ->
sig
  type heap

  exception Empty

  val empty : heap
  val is_empty : heap -> bool

  val insert : Ord.t -> heap -> heap
  val merge : heap -> heap -> heap

  val find_min : heap -> Ord.t  (* raises Empty if heap is empty *)
  val delete_min : heap -> heap  (* raises Empty if heap is empty *)

  val fold : ('a -> Ord.t -> 'a) -> 'a -> heap -> 'a
  val elements : heap -> Ord.t list
end
