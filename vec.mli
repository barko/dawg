type t = UInt8Array.t

type v = {
  offset : int;
  length : int;
  array : t;
}

val iter : width:int -> v -> (index:int -> value:int -> unit) -> unit
val fold : width:int -> v -> (index:int -> value:int -> 'b -> 'b) -> 'b -> 'b

val write_to_channel : out_channel -> width:int -> int list -> int
