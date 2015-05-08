open Dog_t

type t

val create : Dog_io.RO.t -> t

val filter : t -> (int -> Feat.ifeature -> bool) -> t

val i_find_by_id : t -> int -> Feat.ifeature
val a_find_by_id : t -> int -> Feat.afeature

val i_find_by_id : t -> int -> Feat.ifeature
(* can raise [Not_found] *)

val i_find_by_name : t -> string -> Feat.ifeature list
(* several features can have the same name *)

val remove : t -> int -> t
val length : t -> int

val iter : t -> (Feat.afeature -> unit) -> unit
val fold : t -> (Feat.afeature -> 'a -> 'a) -> 'a -> 'a
val i_to_a : t -> Feat.ifeature -> Feat.afeature

val find : t -> Feat_utils.feature_descr -> Feat.ifeature list
val assoc : t -> (Feat_utils.feature_descr * 'a) list -> 'a Utils.IntMap.t
