(* a feature map backed by the read-append Dog_io.RA *)
module IntMap = Utils.XMap( Utils.Int )

type t = {
  num_observations : int;
  dog_ra : Dog_io.RW.t;

  active_id_to_feature : Feat.ifeature IntMap.t;
  (* features on which we call best_split *)

  inactive_id_to_feature : Feat.ifeature IntMap.t;
  (* features upon which we do not call best_split *)
}

let create dog_ra num_observations =
{
  dog_ra;
  active_id_to_feature = IntMap.empty;
  inactive_id_to_feature = IntMap.empty;
  num_observations;
}

let add t feature pos feature_blob status =
  let feature_id = Feat_utils.id_of_feature feature in
  if IntMap.mem feature_id t.active_id_to_feature then
    t (* silently drop *)
  else if IntMap.mem feature_id t.inactive_id_to_feature then
    t (* silently drop *)
  else
    let () = Dog_io.RW.write t.dog_ra pos feature_blob in
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
  assert false

let inactivate t feature_id =
  assert false

let find t feature_id =
  try
    Some (IntMap.find feature_id t.active_id_to_feature)
  with Not_found ->
    try
      Some (IntMap.find feature_id t.inactive_id_to_feature)
    with Not_found ->
      None

let fold_active t f x0 =
  IntMap.fold (
    fun feature_id feature x ->
      f feature x
  ) t.active_id_to_feature x0

let offset_to_vec t offset = {
  Vec.length = t.num_observations;
  array = Dog_io.RW.array t.dog_ra;
  offset = offset;
}

let i_to_a t ifeature =
  Feat_utils.i_to_a (offset_to_vec t) ifeature
