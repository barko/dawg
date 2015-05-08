open Model_t
open Dog_t


(* translate from the feature type defined in [Dog_t] to [Model_t] *)
let feature_d_to_m = function
  | `Cat {
      c_feature_id;
      c_feature_name_opt;
      c_categories;
      c_anonymous_category } ->

    `CategoricalFeature {
      cf_feature_id = c_feature_id;
      cf_feature_name_opt = c_feature_name_opt;
      cf_categories = c_categories;
      cf_anonymous_category_index_opt = c_anonymous_category;
    }

  | `Ord { o_feature_id; o_feature_name_opt } ->

    `OrdinalFeature {
      of_feature_id = o_feature_id;
      of_feature_name_opt = o_feature_name_opt
    }

(* convert int's to float's (model doesn't make a distinction, unlike
   the dog file, in which this has implications on compression).  We
   also convert to arrays, so we can easily get the breakpoint value
   corresponding to a split (index). *)
let float_array_of_breakpoints = function
  | `Int int_list ->
    let float_list = List.rev_map float_of_int (List.rev int_list) in
    Array.of_list float_list

  | `Float float_list ->
    Array.of_list float_list

(* create a map from the feature id of ordinal features to their
   breakpoints *)
let id_to_breakpoints id_to_feature =
  Utils.IntMap.fold (
    fun feature_id feature map ->
      match feature with
        | `Ord { o_breakpoints } ->
          let float_array = float_array_of_breakpoints o_breakpoints in
          Utils.IntMap.add feature_id float_array map
        | `Cat _ -> map
  ) id_to_feature Utils.IntMap.empty

let rle_of_category_array directions =
  let _, _, rle = Rle.encode_dense (Array.to_list directions) in
  let dr_first_direction =
    match rle with
      | (_, `Left ) :: _ -> `Left
      | (_, `Right) :: _ -> `Right
      | _ -> assert false (* must have at least two direction! *)
  in
  let dr_run_lengths = List.rev_map fst (List.rev rle) in
  { dr_first_direction; dr_run_lengths }

let opposite_direction = function
  | `Right -> `Left
  | `Left  -> `Right

let category_array_of_rle { dr_first_direction; dr_run_lengths } =
  (* first, add a direction to each run length, so we can use [Rle.decode_runs_rev] *)
  let _, runs_rev = List.fold_left (
      fun (direction, runs_rev) run_length ->
        let runs_rev = (run_length, direction) :: runs_rev in
        let direction = opposite_direction direction in
        direction, runs_rev
    ) (dr_first_direction, []) dr_run_lengths in
  let _, directions = Rle.decode_rev runs_rev in
  Array.of_list directions

let rec tree_l_to_c id_to_breakpoints = function
  | `OrdinalNode { on_feature_id; on_split; on_left_tree; on_right_tree } ->
    let breakpoints = Utils.IntMap.find on_feature_id id_to_breakpoints in
    let on_split = breakpoints.( on_split ) in
    let on_left_tree = tree_l_to_c id_to_breakpoints on_left_tree in
    let on_right_tree = tree_l_to_c id_to_breakpoints on_right_tree in
    `OrdinalNode { on_feature_id; on_split; on_left_tree; on_right_tree }

  | `CategoricalNode {
      cn_feature_id;
      cn_category_directions;
      cn_left_tree;
      cn_right_tree
    } ->
    let cn_left_tree = tree_l_to_c id_to_breakpoints cn_left_tree in
    let cn_right_tree = tree_l_to_c id_to_breakpoints cn_right_tree in
    let cn_category_directions = rle_of_category_array cn_category_directions in
    `CategoricalNode {
      cn_feature_id;
      cn_category_directions;
      cn_left_tree;
      cn_right_tree
    }

  | (`Leaf _) as leaf -> leaf


let rec add_features_of_tree feature_set map = function
  | `CategoricalNode { cn_feature_id; cn_left_tree; cn_right_tree } ->
    let feature = Feat_map.i_find_by_id feature_set cn_feature_id in
    let map = Utils.IntMap.add cn_feature_id feature map in
    let map = add_features_of_tree feature_set map cn_left_tree in
    let map = add_features_of_tree feature_set map cn_right_tree in
    map

  | `OrdinalNode { on_feature_id; on_left_tree; on_right_tree } ->
    let feature = Feat_map.i_find_by_id feature_set on_feature_id in
    let map = Utils.IntMap.add on_feature_id feature map in
    let map = add_features_of_tree feature_set map on_left_tree in
    let map = add_features_of_tree feature_set map on_right_tree in
    map

  | `Leaf _ -> map

(* as a performance optimization, create a map containing only the
   features referenced by the trees; this is presumeably a (much)
   smaller map that the (misnamed) [feature_set]. *)
let id_to_feature feature_set trees =
  List.fold_left (
    fun map tree ->
      add_features_of_tree feature_set map tree
  ) Utils.IntMap.empty trees

let l_to_c feature_set trees =
  let id_to_feature = id_to_feature feature_set trees in
  let features = Utils.IntMap.fold (
      fun feature_id feature features ->
        (feature_d_to_m feature) :: features
    ) id_to_feature [] in
  let id_to_breakpoints = id_to_breakpoints id_to_feature in

  let trees = List.map (tree_l_to_c id_to_breakpoints) trees in
  trees, features

let rec tree_rle_to_array = function
  | (`Leaf _) as leaf -> leaf

  | `CategoricalNode {
      cn_feature_id;
      cn_category_directions;
      cn_left_tree;
      cn_right_tree;
    } ->
    let cn_category_directions = category_array_of_rle cn_category_directions in
    let cn_left_tree = tree_rle_to_array cn_left_tree in
    let cn_right_tree = tree_rle_to_array cn_right_tree in
    `CategoricalNode {
      cn_feature_id;
      cn_category_directions;
      cn_left_tree;
      cn_right_tree
    }

  | `OrdinalNode { on_feature_id; on_split; on_left_tree; on_right_tree } ->
    let on_left_tree = tree_rle_to_array on_left_tree in
    let on_right_tree = tree_rle_to_array on_right_tree in
    `OrdinalNode { on_feature_id; on_split; on_left_tree; on_right_tree }

let rle_to_array trees =
  List.rev_map tree_rle_to_array trees
