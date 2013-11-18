type t
val create : int -> t
val shuffle : t -> Random.State.t -> unit
val iter : (int -> unit) -> t -> unit
val fold_left : ('a -> int -> 'a) -> 'a -> t -> 'a
val array : (index:int -> value:int -> 'b) -> t -> 'b array
