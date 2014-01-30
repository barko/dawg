(* a feature map backed by the read only Dog_io.RO *)
open Dog_t

module IntMap = Utils.XMap( Utils.Int )

type t = {
  id_to_feature : Feat.ifeature IntMap.t;
  num_observations : int;
  array : UInt8Array.t;
  appendable : bool;
}

let add_to_map features =
  let map = List.fold_left (
      fun map cat ->
        let feature = `Cat cat in
        IntMap.add cat.c_feature_id feature map
    ) IntMap.empty features.cat_a in

  let map = List.fold_left (
      fun map ord ->
        let feature = `Ord ord in
        IntMap.add ord.o_feature_id feature map
    ) map features.ord_a in
  map

let create dog_reader =
  let dog = Dog_io.RO.dog dog_reader in
  let id_to_feature = add_to_map dog.Dog_t.features in
  { id_to_feature;
    num_observations = dog.Dog_t.num_observations;
    array = Dog_io.RO.array dog_reader;
    appendable = false;
  }

let offset_to_vec t offset =
  {
    Vec.length = t.num_observations;
    array = t.array;
    offset = offset;
  }

let i_to_a t =
  Feat_utils.i_to_a (offset_to_vec t)

let iter t f =
  IntMap.iter (
    fun _ ifeature ->
      f (i_to_a t ifeature)
  ) t.id_to_feature

let fold t f x0 =
  let x = ref x0 in
  iter t (
    fun feature ->
      x := f feature !x
  );
  !x

let filter t f =
  let id_to_feature = IntMap.filter f t.id_to_feature in
  { t with id_to_feature }

let i_find_by_id t feature_id =
  IntMap.find feature_id t.id_to_feature

let a_find_by_id t feature_id =
  i_to_a t (i_find_by_id t feature_id)

let i_find_by_id_opt t feature_id =
  try
    Some (i_find_by_id t feature_id)
  with Not_found ->
    None

let remove t feature_id =
  let id_to_feature = IntMap.remove feature_id t.id_to_feature in
  { t with id_to_feature }

let length t =
  IntMap.fold (fun _ _ c -> c + 1) t.id_to_feature 0

let i_find_by_name_opt t feature_name =
  let features = IntMap.fold (
    fun _ feature accu ->
      match feature with
        | `Ord { o_feature_name_opt = opt }
        | `Cat { c_feature_name_opt = opt } ->
          match opt with
            | Some name ->
              if name = feature_name then
                feature :: accu
              else
                accu
            | None -> accu
  ) t.id_to_feature [] in
  match features with
    | [] -> None
    | [ feature ] -> Some feature
    | _ -> assert false

type feature_descr = [ `Name of string | `Id of int ]

let find t = function
  | `Name name -> i_find_by_name_opt t name
  | `Id id -> i_find_by_id_opt t id

let string_of_feature_descr = function
  | `Name name -> Printf.sprintf "name:%s" name
  | `Id id -> Printf.sprintf "id:%d" id
