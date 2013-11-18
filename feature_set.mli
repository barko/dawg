type predicate =
  | Any
  | Name of string
  | ID of Dog_t.feature_id
  | NameRegExp of Pcre.regexp

type instruction = Include of predicate | Exclude of predicate

type t

val create : Dog_io.r -> t
val eval_instructions : t -> instruction list -> t
val num_included : t -> int
val num_excluded : t -> int


val find_feature_by_id : t -> Dog_t.feature_id ->
  Dog_t.pointer Dog_t.feature option

val find_feature_by_name : t -> Dog_t.feature_name ->
  Dog_t.pointer Dog_t.feature option

type bv_set

val bitvector_set_of_t : t -> bv_set

val iter : (Dog_t.pointer_feature -> BV7.e -> unit) -> bv_set -> unit
val fold_left : ('a -> Dog_t.pointer_feature -> BV7.e -> 'a) ->
  'a -> bv_set -> 'a

val elements : bv_set -> (Dog_t.pointer_feature * BV7.e) list
val count : bv_set -> int

val uniform_split : int -> bv_set -> bv_set list
