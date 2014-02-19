open Proto_t
open Model_t

type m = {
  max_depth : int;
  feature_map : Feat_map.t;
  splitter : Loss.splitter
}

let string_of_split { s_gamma ; s_n ; s_loss } =
  Printf.sprintf "gamma=%f n=%d loss=%f" s_gamma s_n s_loss

let directions_of_split s_to_k s_pivot =
  let num_categories = Array.length s_to_k in
  let directions = Array.create num_categories `Right in
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

let timeout = 1000.

let best_split_of_splits best_splits =
  List.fold_left (
    fun best_opt (_,_, response) ->
      let s_opt =
        match response with
          | `AckBestSplit s_opt -> s_opt
          | _ -> assert false
      in
      match best_opt, s_opt with
        | Some (best_loss, best_split), Some (loss, split) ->

          if best_loss < loss then
            (* still superior *)
            best_opt
          else
            (* new champ *)
            Some (loss, split)

        | None, Some (loss, split) ->
          (* first guy's always champ *)
          Some (loss, split)

        | Some _, None -> best_opt
        | None, None -> None

  ) None best_splits

let terminate (best_split : Proto_t.split) =
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

let rec make task_id max_depth workers depth =
  let open Worker_client in
  let request = `Learning (task_id, `Sample) in
  let is_response_valid = function
    | `AckSample -> true
    | _ -> false
  in
  lwt result = broad_send_recv workers timeout request is_response_valid in

  let request = `Learning (task_id, `BestSplit) in
  let is_response_valid = function
    | `AckBestSplit _ -> true
    | _ -> false
  in
  lwt result = broad_send_recv workers timeout request is_response_valid in

  match best_split_of_splits result with
    | None -> Lwt.return None

    | Some (loss, split) ->

      if depth + 1 >= max_depth then
        Lwt.return (terminate split)

      else
        (* push *)
        let request = `Learning (task_id, `Push split) in
        let is_response_valid = function
          | `AckPush -> true
          | _ -> false
        in
        lwt _ = broad_send_recv workers timeout request is_response_valid in

        (* descend left *)
        let request = `Learning (task_id, `Descend `Left) in
        let is_response_valid = function
          | `AckDescend -> true
          | _ -> false
        in
        lwt _ = broad_send_recv workers timeout request is_response_valid in

        let os =
          match split with
            | `CategoricalSplit (os,_) -> os
            | `OrdinalSplit os -> os
        in

        lwt side_left =
          match_lwt make task_id max_depth workers (depth+1) with
            | None -> Lwt.return (`Leaf os.os_left.s_gamma)
            | Some tree -> Lwt.return tree
        in

        (* ascend *)
        let request = `Learning (task_id, `Ascend) in
        let is_response_valid = function
          | `AckAscend -> true
          | _ -> false
        in
        lwt _ = broad_send_recv workers timeout request is_response_valid in

        (* descend right *)
        let request = `Learning (task_id, `Descend `Right) in
        let is_response_valid = function
          | `AckDescend -> true
          | _ -> false
        in
        lwt _ = broad_send_recv workers timeout request is_response_valid in

        lwt side_right =
          match_lwt make task_id max_depth workers (depth+1) with
            | None -> Lwt.return (`Leaf os.os_right.s_gamma)
            | Some tree -> Lwt.return tree
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
        Lwt.return (Some node)


