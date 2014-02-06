open Dog_t

type t

val create : Dog_io.RO.t -> t

val filter : t -> (int -> Feat.ifeature -> bool) -> t

val i_find_by_id : t -> int -> Feat.ifeature
val a_find_by_id : t -> int -> Feat.afeature

val i_find_by_id_opt : t -> int -> Feat.ifeature option
val i_find_by_name_opt : t -> string -> Feat.ifeature option

val remove : t -> int -> t
val length : t -> int

val iter : t -> (Feat.afeature -> unit) -> unit
val fold : t -> (Feat.afeature -> 'a -> 'a) -> 'a -> 'a
val i_to_a : t -> Feat.ifeature -> Feat.afeature

val find : t -> Feat_utils.feature_descr -> Feat.ifeature option
