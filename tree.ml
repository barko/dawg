open Proto_t
open Model_t

type feature_monotonicity_map = Dog_t.monotonicity Utils.IntMap.t

let partition_observations in_subset splitting_feature best_split =
  let in_subset_left  = Array.copy in_subset in
  let in_subset_right = Array.copy in_subset in

  (match splitting_feature, best_split with
    | `Ord { Dog_t.o_vector; o_cardinality },
      `OrdinalSplit { os_split } -> (
        match o_vector with
          | `Dense v ->
            let width_num_bytes = Utils.num_bytes o_cardinality in
            Dense.iter ~width:width_num_bytes v (
              fun ~index ~value ->
                if in_subset.(index) then
                  let in_left = value <= os_split in
                  in_subset_left.(index) <- in_left;
                  in_subset_right.(index) <- not in_left
            );

          | `RLE v ->
            Rlevec.iter v (
              fun ~index ~length ~value ->
                for i = index to index + length - 1 do
                  if in_subset.(i) then
                    let in_left = value <= os_split in
                    in_subset_left.(i) <- in_left;
                    in_subset_right.(i) <- not in_left
                done
            );
      )

    | `Cat { Dog_t.c_vector; c_cardinality },
      `CategoricalSplit ({ os_split }, s_to_k) -> (
        assert (Array.length s_to_k = c_cardinality);

        (* create reverse mapping *)
        let s_by_k = Array.make c_cardinality (-1) in
        for s = 0 to c_cardinality-1 do
          let k = s_to_k.(s) in
          s_by_k.(k) <- s;
        done;

        match c_vector with
          | `Dense v ->
            let width_num_bytes = Utils.num_bytes c_cardinality in
            Dense.iter ~width:width_num_bytes v (
              fun ~index ~value ->
                if in_subset.(index) then
                  let s = s_by_k.(value) in
                  let in_left = s <= os_split in
                  in_subset_left.(index) <- in_left;
                  in_subset_right.(index) <- not in_left
            );

          | `RLE v ->
            Rlevec.iter v (
              fun ~index ~length ~value ->
                for i = index to index + length - 1 do
                  if in_subset.(i) then
                    let s = s_by_k.(value) in
                    let in_left = s <= os_split in
                    in_subset_left.(i) <- in_left;
                    in_subset_right.(i) <- not in_left
                done
            );
      )

    | _ -> assert false
  );
  in_subset_left, in_subset_right

type m = {
  max_depth : int;
  feature_map : Feat_map.t;
  feature_monotonicity_map : feature_monotonicity_map;
  splitter : Loss.splitter;
}

let string_of_split { s_gamma ; s_n ; s_loss } =
  Printf.sprintf "gamma=%f n=%d loss=%f" s_gamma s_n s_loss

let best_split_of_features m =
  Feat_map.fold m.feature_map (
    fun feature best_opt ->
      let feature_id = Feat.feature_id feature in
      let monotonicity =
        try Utils.IntMap.find feature_id m.feature_monotonicity_map
        with Not_found -> `Arbitrary
      in
      let s_opt = m.splitter#best_split monotonicity feature in
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

let directions_of_split s_to_k s_pivot =
  let num_categories = Array.length s_to_k in
  let directions = Array.make num_categories `Right in
  assert (0 <= s_pivot && s_pivot < num_categories );
  for s = 0 to s_pivot do
    let k = s_to_k.(s) in
    directions.(k) <- `Left
  done;
    (*
      for s = s_pivot to num_directions-1 do
       let k = s_to_k.(s) in
        directions.(k) <- `Right
      done;
    *)
  directions

let rec terminate (best_split : Proto_t.split) =
  let difference_in_gamma =
    let os =
      match best_split with
        | `OrdinalSplit os -> os
        | `CategoricalSplit (os, _) -> os
    in
    os.os_left.s_gamma -. os.os_right.s_gamma
  in
  if abs_float difference_in_gamma < -1e8 then
    None (* split not found *)
  else
    let node =
      match best_split with
        | `CategoricalSplit (os, s_to_k) ->
          let directions = directions_of_split s_to_k os.os_split in
          `CategoricalNode {
            cn_feature_id = os.os_feature_id;
            cn_category_directions = directions;
            cn_left_tree = `Leaf os.os_left.s_gamma;
            cn_right_tree = `Leaf os.os_right.s_gamma;
          }

        | `OrdinalSplit os -> `OrdinalNode {
            on_feature_id = os.os_feature_id;
            on_split = os.os_split;
            on_left_tree = `Leaf os.os_left.s_gamma;
            on_right_tree = `Leaf os.os_right.s_gamma
          }
    in
    Some node

and make m depth in_subset =
  m.splitter#update_with_subset in_subset;
  match best_split_of_features m with
    | None -> None

    | Some split ->
      let splitting_feature, loss, split = split in

      if depth + 1 >= m.max_depth then
        terminate split

      else
        let in_subset_left, in_subset_right =
          partition_observations in_subset splitting_feature split in

        let os =
          match split with
            | `CategoricalSplit (os,_) -> os
            | `OrdinalSplit os -> os
        in

        let side_left =
          match make m (depth+1) in_subset_left with
            | None -> `Leaf os.os_left.s_gamma
            | Some tree -> tree
        in

        let side_right =
          match make m (depth+1) in_subset_right with
            | None -> `Leaf os.os_right.s_gamma
            | Some tree -> tree
        in

        let node =
          match split with
            | `CategoricalSplit (os, s_to_k) ->
              let directions = directions_of_split s_to_k os.os_split in
              `CategoricalNode {
                cn_feature_id = os.os_feature_id;
                cn_category_directions = directions;
                cn_left_tree = side_left;
                cn_right_tree = side_right
              }

            | `OrdinalSplit os -> `OrdinalNode {
                on_feature_id = os.os_feature_id;
                on_split = os.os_split;
                on_left_tree = side_left;
                on_right_tree = side_right;
              }
        in
        Some node


(* partially evaluate a tree when the value of feature with id
   [feature_id] is [value] *)
let rec eval_partially_1 feature_id value = function
  | `OrdinalNode {
      on_split;
      on_left_tree;
      on_right_tree;
      on_feature_id
    } ->
    if on_feature_id = feature_id then
      let sub_tree =
        if value <= on_split then
          on_left_tree
        else
          on_right_tree
      in
      eval_partially_1 feature_id value sub_tree
    else
      (* create a new tree, the result of simplifying each subtree *)
      let on_left_tree  = eval_partially_1 feature_id value on_left_tree in
      let on_right_tree = eval_partially_1 feature_id value on_right_tree in
      `OrdinalNode { on_split; on_left_tree; on_right_tree; on_feature_id }

  | `CategoricalNode {
      cn_category_directions;
      cn_left_tree;
      cn_right_tree;
      cn_feature_id
    } ->
    if cn_feature_id = feature_id then
      let sub_tree =
        match cn_category_directions.(value) with
          | `Left -> cn_left_tree
          | `Right -> cn_right_tree
      in
      eval_partially_1 feature_id value sub_tree
    else
      let cn_left_tree  = eval_partially_1 feature_id value cn_left_tree in
      let cn_right_tree = eval_partially_1 feature_id value cn_right_tree in
      `CategoricalNode {
        cn_category_directions;
        cn_left_tree;
        cn_right_tree;
        cn_feature_id
      }

  | (`Leaf _) as leaf -> leaf


let is_leaf = function
  | `Leaf _ -> true
  | _ -> false

let eval_partially trees cardinality feature_id = function
  | `Dense v ->
    let width_num_bytes = Utils.num_bytes cardinality in
    let num_leaves = ref 0 in
    Dense.iter ~width:width_num_bytes v (
      fun ~index ~value ->
        let tree = eval_partially_1 feature_id value trees.(index) in
        trees.(index) <- tree;

        match tree with
          | `Leaf _ -> incr num_leaves
          | _ -> ()
    );
    !num_leaves

  | `RLE v ->
    let num_leaves = ref 0 in
    Rlevec.iter v (
      fun ~index ~length ~value ->
        for i = index to index + length - 1 do
          trees.(i) <- eval_partially_1 feature_id value trees.(i);
          match trees.(i) with
            | `Leaf _ -> incr num_leaves
            | _ -> ()
        done
    );
    !num_leaves


module IntSet = Set.Make( Utils.Int )

(* accumulate all the feature_id's referenced by a tree *)
let rec add_feature_id_to_set set = function
  | `Leaf _ -> set

  | `OrdinalNode { on_feature_id; on_left_tree; on_right_tree } ->
    let set = add_feature_id_to_set set on_left_tree in
    let set = add_feature_id_to_set set on_right_tree in
    IntSet.add on_feature_id set

  | `CategoricalNode { cn_feature_id; cn_left_tree; cn_right_tree } ->
    let set = add_feature_id_to_set set cn_left_tree in
    let set = add_feature_id_to_set set cn_right_tree in
    IntSet.add cn_feature_id set

let feature_id_set_of_tree tree =
  add_feature_id_to_set IntSet.empty tree

(*
  let nl tree_array =
    Array.fold_left (
      fun count tree ->
        match tree with
          | `Leaf _ -> count + 1
          | _ -> count
    ) 0 tree_array
*)

let mk_eval num_observations =
  let gamma = Array.make num_observations nan in
  let gamma_leaf = Array.make num_observations (`Leaf nan) in
  fun find_by_id tree ->
    let feature_id_set = feature_id_set_of_tree tree in
    Array.fill gamma_leaf 0 num_observations tree;

    let num_leaves = ref 0 in
    IntSet.iter (
      fun feature_id ->
        let a_feature = find_by_id feature_id in
        (*
          let i_feature =
          Feat_map.i_to_a feature_map i_feature
        in *)
        let open Dog_t in
        let cardinality = Feat_utils.cardinality_of_feature a_feature in
        let vector = Feat_utils.vector_of_feature a_feature in
        let num_leaves_new = eval_partially gamma_leaf cardinality
            feature_id vector in
        assert ( num_leaves_new >= !num_leaves );
        num_leaves := num_leaves_new

    ) feature_id_set;
    assert ( if !num_leaves <> 0 then !num_leaves = num_observations else true );
    for i = 0 to num_observations-1 do
      match gamma_leaf.(i) with
        | `Leaf g ->  gamma.(i) <- g
        | `OrdinalNode _
        | `CategoricalNode _ -> assert false
    done;
    gamma


let rec shrink alpha = function
  | `Leaf gamma ->  `Leaf (alpha *. gamma)

  | `CategoricalNode cn ->
    let cn_left_tree = shrink alpha cn.cn_left_tree in
    let cn_right_tree = shrink alpha cn.cn_right_tree in
    `CategoricalNode { cn with cn_left_tree; cn_right_tree }

  | `OrdinalNode on ->
    let on_left_tree = shrink alpha on.on_left_tree in
    let on_right_tree = shrink alpha on.on_right_tree in
    `OrdinalNode { on with on_left_tree; on_right_tree }
