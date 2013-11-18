module Make : functor (Elem : Heap.ORDERED) ->
sig
  val create : Elem.t Stream.t list -> Elem.t Stream.t
end
