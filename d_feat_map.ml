(* a feature map backed by the read-append Dog_io.RW *)
module IntMap = Utils.XMap( Utils.Int )

type t = {
  num_observations : int;
  array : UInt8Array.t;
  active_id_to_feature : Dog_io.RW.qfeature IntMap.t;
  inactive_id_to_feature : Dog_io.RW.qfeature IntMap.t;
}

let create dog_rw = {
  active_id_to_feature = IntMap.empty;
  inactive_id_to_feature = IntMap.empty;
  num_observations = dog_rw.Dog_io.RW.num_observations;
  array = dog_rw.Dog_io.RW.array;
}

exception FeatureIdNotFound of Dog_t.feature_id

let add t feature_id feature status =
  if IntMap.mem feature_id t.active_id_to_feature then
    t (* silently drop *)
  else if IntMap.mem feature_id t.inactive_id_to_feature then
    t (* silently drop *)
  else
    match status with
      | `Active ->
        let active_id_to_feature = IntMap.add feature_id
            feature t.active_id_to_feature in
        { t with active_id_to_feature }

      | `Inactive ->
        let inactive_id_to_feature = IntMap.add feature_id
            feature t.inactive_id_to_feature in
        { t with inactive_id_to_feature }

let activate t feature_id =
  try
    let feature = IntMap.find feature_id t.inactive_id_to_feature in
    let active_id_to_feature = IntMap.add feature_id feature
        t.active_id_to_feature in
    { t with active_id_to_feature }
  with Not_found ->
    if IntMap.mem feature_id t.active_id_to_feature then
      t (* already active: nothing to do *)
    else
      raise (FeatureIdNotFound feature_id)


let deactivate t feature_id =
  try
    let feature = IntMap.find feature_id t.active_id_to_feature in
    let inactive_id_to_feature = IntMap.add feature_id feature
        t.inactive_id_to_feature in
    { t with inactive_id_to_feature }
  with Not_found ->
    if IntMap.mem feature_id t.inactive_id_to_feature then
      t (* already inactive: nothing to do *)
    else
      raise (FeatureIdNotFound feature_id)

let deactivate_if t f =
  let active_id_to_feature, inactive_id_to_feature = IntMap.fold (
    fun feature_id feature (active, inactive) ->
      if f feature then
        let inactive = IntMap.add feature_id feature inactive in
        active, inactive
      else
        let active = IntMap.add feature_id feature active in
        active, inactive
    ) t.active_id_to_feature (IntMap.empty, IntMap.empty)
  in
  { t with active_id_to_feature; inactive_id_to_feature }


(*
let q_find t feature_id =
  try
    IntMap.find feature_id t.active_id_to_feature
  with Not_found ->
    try
      IntMap.find feature_id t.inactive_id_to_feature
    with Not_found ->
      raise (FeatureIdNotFound feature_id)

let q_to_a_vector t = function
  | `Dense { Dog_io.RW.vector_id } ->
    `Dense {
      Vec.length = t.num_observations;
      array = t.array;
      offset = vector_id;
    }

  | `RLE { Dog_io.RW.vector_id } ->
    `RLE {
      Vec.length = t.num_observations;
      array = t.array;
      offset = vector_id;
    }

let q_to_a_feature t = function
  | `Cat {
      Dog_t.c_feature_id;
      c_feature_name_opt;
      c_anonymous_category;
      c_categories;
      c_cardinality;
      c_vector;
    } ->
    `Cat {
      Dog_t.c_feature_id;
      c_feature_name_opt;
      c_anonymous_category;
      c_categories;
      c_cardinality;
      c_vector = q_to_a_vector t c_vector;
    }

  | `Ord {
      Dog_t.o_feature_id;
      o_feature_name_opt;
      o_cardinality;
      o_breakpoints;
      o_vector;
    } ->
    `Ord {
      Dog_t.o_feature_id;
      o_feature_name_opt;
      o_cardinality;
      o_breakpoints;
      o_vector = q_to_a_vector t o_vector;
    }

let a_find_by_id t feature_id =
  let q_feature = q_find t feature_id in
  q_to_a_feature t q_feature

let fold_active f features x0 =
  IntMap.fold (
    fun feature_id feature x ->
      f (Dog_io.RW.q_to_a_feature dog_rw feature) x
  ) t.active_id_to_feature x0
  *)

let best_split_of_features splitter features =
  List.fold_left (
    fun best_opt feature ->
      let s_opt = splitter#best_split feature in
      match best_opt, s_opt with
        | Some (_, best_loss, best_split), Some (loss, split) ->

          if best_loss < loss then
            (* still superior *)
            best_opt
          else
            (* new champ *)
            Some (feature, loss, split)

        | None, Some (loss, split) ->
          (* first guy's always champ *)
          Some (feature, loss, split)

        | Some _, None -> best_opt
        | None, None -> None

  ) None features

(*
let q_find_all_by_name feature_name map =
  (* since feature names are not unique, we may have multiple features
     satisfying the query *)
  IntMap.fold (
    fun _ feature features ->
      match Feat_utils.name_of_feature feature with
        | Some fn ->
          if fn = feature_name then
            feature :: features
          else
            features
        | None -> features
  ) map []

let q_find_all_by_name t feature_name =
  (q_find_all_by_name feature_name t.active_id_to_feature) @
    (q_find_all_by_name feature_name t.inactive_id_to_feature)

let a_find_all_by_name t feature_name =
  List.map (q_to_a_feature t) (q_find_all_by_name t feature_name)

let a_find_all t = function
  | `Id feature_id -> [a_find_by_id t feature_id]
  | `Name feature_name -> a_find_all_by_name t feature_name

let num_observations { num_observations } =
  num_observations
*)

let num_active { active_id_to_feature } =
  IntMap.cardinal active_id_to_feature

let num_inactive { inactive_id_to_feature } =
  IntMap.cardinal inactive_id_to_feature
