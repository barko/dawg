module IntMap = Utils.XMap( Utils.Int )

type t = {
  num_observations : int;
  dog_read_append : Dog_io.RA.t;

  active_id_to_feature : Feat.ifeature IntMap.t;
  (* features on which we call best_split *)

  inactive_id_to_feature : Feat.ifeature IntMap.t;
  (* features upon which we do not call best_split *)
}

let create dog_read_append num_observations =
{
  dog_read_append;
  active_id_to_feature = IntMap.empty;
  inactive_id_to_feature = IntMap.empty;
  num_observations;
}

let mem t feature_id =
  IntMap.mem feature_id t.active_id_to_feature ||
  IntMap.mem feature_id t.inactive_id_to_feature

let set_vector vector = function
  | `Dense _ -> `Dense vector
  | `RLE _ -> `RLE vector

let add t feature feature_blob status =
  let feature_id = Feat_utils.id_of_feature feature in
  if IntMap.mem feature_id t.active_id_to_feature then
    t (* silently drop *)
  else if IntMap.mem feature_id t.inactive_id_to_feature then
    t (* silently drop *)
  else
    let vector = Dog_io.RA.append t.dog_read_append feature_blob in
    let feature_with_vector =
      let open Dog_t in
      match feature with
        | `Cat cat ->
          let c_vector = set_vector vector cat.c_vector in
          `Cat { cat with c_vector }

        | `Ord ord ->
          let o_vector = set_vector vector ord.o_vector in
          `Ord { ord with o_vector }
    in
    match status with
      | `Active ->
        let active_id_to_feature = IntMap.add feature_id
            feature_with_vector t.active_id_to_feature in
        { t with active_id_to_feature }

      | `Inactive ->
        let inactive_id_to_feature = IntMap.add feature_id
            feature_with_vector t.inactive_id_to_feature in
        { t with inactive_id_to_feature }
