val iter : width:int -> Vec.t -> (index:int -> value:int -> unit) -> unit
val fold : width:int -> Vec.t ->
  (index:int -> value:int -> 'b -> 'b) -> 'b -> 'b

val write_to_channel : out_channel -> width:int -> int list -> int
