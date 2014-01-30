(* a feature map backed by the read-append Dog_io.RA *)
module IntMap = Utils.XMap( Utils.Int )

type t = {
  num_observations : int;
  dog_ra : Dog_io.RW.t;
  active_id_to_feature : Dog_io.RW.qfeature IntMap.t;
  inactive_id_to_feature : Dog_io.RW.qfeature IntMap.t;
}

let create dog_ra =
{
  dog_ra;
  active_id_to_feature = IntMap.empty;
  inactive_id_to_feature = IntMap.empty;
  num_observations = Dog_io.RW.num_observations dog_ra;
}

let add t feature_id vector status =
  if IntMap.mem feature_id t.active_id_to_feature then
    t (* silently drop *)
  else if IntMap.mem feature_id t.inactive_id_to_feature then
    t (* silently drop *)
  else
    let feature = Dog_io.RW.find t.dog_ra feature_id in
    let () = Dog_io.RW.write t.dog_ra feature_id vector in
    match status with
      | `Active ->
        let active_id_to_feature = IntMap.add feature_id
            feature t.active_id_to_feature in
        { t with active_id_to_feature }

      | `Inactive ->
        let inactive_id_to_feature = IntMap.add feature_id
            feature t.inactive_id_to_feature in
        { t with inactive_id_to_feature }

exception FeatureIdNotFound of Dog_t.feature_id

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


let inactivate t feature_id =
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

let q_find t feature_id =
  try
    IntMap.find feature_id t.active_id_to_feature
  with Not_found ->
    try
      IntMap.find feature_id t.inactive_id_to_feature
    with Not_found ->
      raise (FeatureIdNotFound feature_id)

let map_vector t = function
  | `Dense { Dog_io.RW.vector_id } ->
    `Dense {
      Vec.length = t.num_observations;
      array = Dog_io.RW.array t.dog_ra;
      offset = vector_id;
    }

  | `RLE { Dog_io.RW.vector_id } ->
    `RLE {
      Vec.length = t.num_observations;
      array = Dog_io.RW.array t.dog_ra;
      offset = vector_id;
    }

let q_to_a t = function
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
      c_vector = map_vector t c_vector;
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
      o_vector = map_vector t o_vector;
    }

let a_find_by_id t feature_id =
  let qfeature = q_find t feature_id in
  q_to_a t qfeature

let fold_active t f x0 =
  IntMap.fold (
    fun feature_id feature x ->
      f (q_to_a t feature) x
  ) t.active_id_to_feature x0

let best_split_of_features t splitter  =
  fold_active t (
    fun feature best_opt ->
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

  ) None
