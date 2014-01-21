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

let add t feature vector status =
  let feature_id = Feat_utils.id_of_feature feature in
  if IntMap.mem feature_id t.active_id_to_feature then
    t (* silently drop *)
  else if IntMap.mem feature_id t.inactive_id_to_feature then
    t (* silently drop *)
  else
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

let find_a t feature_id =
  try
    IntMap.find feature_id t.active_id_to_feature
  with Not_found ->
    try
      IntMap.find feature_id t.inactive_id_to_feature
    with Not_found ->
      raise (FeatureIdNotFound feature_id)

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

let find_i t feature_id =
  let ifeature = find_a t feature_id in
  i_to_a t ifeature
