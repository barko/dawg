open Cmdliner
open Printf
open Model_j

type feature_importance = {
  unsigned : float option;
  positive : float option;
  negative : float option;
}

let incr_opt delta = function
  | None -> Some delta
  | Some x -> Some (x +. delta)

let sum_opt x_opt y_opt =
  match x_opt, y_opt with
    | None, None -> None
    | Some _, None -> x_opt
    | None, Some _ -> y_opt
    | Some x, Some y -> Some ( x +. y )

let div_opt x_opt d =
  match x_opt with
    | None -> None
    | Some n -> Some (n /. d)

let string_of_float_opt = function
  | None -> "        "
  | Some f -> sprintf "%.6f" f

module IntMap = Utils.XMap( Utils.Int )

let update_importance branch branch_size leaf feature_id_to_importance =
  (* the leaf value get's equally divided among all the features along
     the branch *)
  if branch_size = 0 then
    feature_id_to_importance
  else
    let score = abs_float (leaf /. (float branch_size)) in
    let sign =
      if leaf < 0.0 then
        `Neg
      else if leaf > 0.0 then
        `Pos
      else
        (* error in tree construction: leafs that have value zero are
           trivial; we can assign them to either the positive or
           negative side *)
        `Zero
    in

    (* iterate through the feature_id's in a branch, updating their
       feature importance scores *)
    List.fold_left (
      fun map feature_id ->
        let feature_importance =
          try
            IntMap.find feature_id feature_id_to_importance
          with Not_found ->
            { unsigned = None; positive = None; negative = None }
        in
        let unsigned = feature_importance.unsigned in
        let unsigned = incr_opt score unsigned in
        let positive, negative =
          match sign with
            | `Neg ->
              (* update negative *)
              let negative = feature_importance.negative in
              let negative = incr_opt score negative in
              feature_importance.positive, negative

            | `Pos ->
              (* update negative *)
              let positive = feature_importance.positive in
              let positive = incr_opt score positive in
              positive, feature_importance.negative

            | `Zero ->
              (* update neither *)
              feature_importance.positive, feature_importance.negative
        in
        let feature_importance = { unsigned; positive; negative } in
        IntMap.add feature_id feature_importance map

    ) feature_id_to_importance branch

let rec fold_tree f branch branch_size x = function
  | `Leaf value ->
    let x = f branch branch_size value x in
    x

  | `OrdinalNode {
      on_feature_id = feature_id;
      on_left_tree = left_tree;
      on_right_tree = right_tree
    }

  | `CategoricalNode {
      cn_feature_id = feature_id;
      cn_left_tree = left_tree;
      cn_right_tree = right_tree;
    } ->

    let branch = feature_id :: branch in
    let branch_size = branch_size + 1 in
    let x = fold_tree f branch branch_size x left_tree in
    let x = fold_tree f branch branch_size x right_tree in
    x

let fold_tree f x0 tree =
  fold_tree f [] 0 x0 tree

let importance_of_features trees =
  let feature_id_to_importance = List.fold_left (
    fun feature_id_to_importance tree ->
      fold_tree update_importance feature_id_to_importance tree
  ) IntMap.empty trees in

  (* sum, in order to normalize *)
  let sum_importance = IntMap.fold (
      fun feature_id importance sum_importance ->
        { unsigned = sum_opt importance.unsigned sum_importance.unsigned;
          positive = sum_opt importance.positive sum_importance.positive;
          negative = sum_opt importance.negative sum_importance.negative
        }
    ) feature_id_to_importance
      { unsigned = None; positive = None; negative = None }

  in

  let sum_unsigned, sum_positive, sum_negative =
    match sum_importance with
      | { unsigned = Some u; positive = Some p; negative = Some n } -> u, p, n
      | _ ->
        (* in a valid model, we must have at least one positive and
           one negative leaf *)
        assert false
  in

  (* normalize *)
  let feature_id_to_importance = IntMap.map (
      fun importance ->
        { unsigned = div_opt importance.unsigned sum_unsigned;
          positive = div_opt importance.positive sum_positive;
          negative = div_opt importance.negative sum_negative
        }
    ) feature_id_to_importance
  in

  (* convert to list *)
  let feature_id_to_importance = IntMap.fold (
    fun feature_id importance accu ->
      (feature_id, importance) :: accu
  ) feature_id_to_importance [] in

  (* sort by unsigned score *)
  List.sort (
    fun (_, { unsigned = u1o }) (_, { unsigned = u2o }) ->
      match u1o, u2o with
        | Some u1, Some u2 ->
          Pervasives.compare u2 u1 (* descending *)
        | _ ->
          assert false (* in a valid model, every feature must have at least one leaf *)
  ) feature_id_to_importance


let main model_file_path =
  let model_s = Mikmatch.Text.file_contents model_file_path in
  let model = Model_j.c_model_of_string model_s in
  let trees, features =
    match model with
      | `Logistic logistic ->
        printf "kind: logistic positive=%s negative=%s\n%!"
          logistic.bi_positive_category
          (match logistic.bi_negative_category_opt with
            | None -> "<none>"
            | Some nc -> nc
          );
        logistic.bi_trees, logistic.bi_features

      | `Square square ->
        printf "kind: square\n%!";
        square.re_trees, square.re_features
  in
  printf "num trees: %d\nnum features: %d\n%!"
    (List.length trees) (List.length features);

  (* create mapping between feature id to feature name (optional) and
     feature kind *)
  let feature_id_to_name_opt = Hashtbl.create (List.length features) in
  List.iter (
      fun feature ->
        let feature_id, feature_name_opt, kind =
          match feature with
            | `CategoricalFeature { cf_feature_id; cf_feature_name_opt } ->
              cf_feature_id, cf_feature_name_opt, "Cat"
            | `OrdinalFeature { of_feature_id; of_feature_name_opt } ->
              of_feature_id, of_feature_name_opt, "Ord"
        in
        Hashtbl.replace feature_id_to_name_opt
          feature_id (feature_name_opt, kind)

    ) features;

  let max_feature_name_len = Hashtbl.fold (
      fun _ (feature_name_opt, _) mx ->
        match feature_name_opt with
          | Some feature_name -> max (String.length feature_name) mx
          | None -> mx
    ) feature_id_to_name_opt 0 in

  let feature_id_to_importance = importance_of_features trees in

  List.iter (
    fun (feature_id, { unsigned; positive; negative }) ->
      let feature_name_opt, kind = Hashtbl.find feature_id_to_name_opt
          feature_id in
      printf "%10d %-*s %s %s %s %s\n"
        feature_id
        max_feature_name_len
        (match feature_name_opt with Some n -> n | None -> "")
        kind
        (string_of_float_opt unsigned)
        (string_of_float_opt positive)
        (string_of_float_opt negative)
  ) feature_id_to_importance


let commands =
  let mod_cmd =
    let doc = "describe properties of an EigenDog model" in

    let dog_path =
      let doc = "the path of the dog file" in
      Arg.(required & pos 0 (some string) None & info [] ~doc)
    in

    Term.(pure main $ dog_path ),
    Term.info "mod" ~doc
  in

  [mod_cmd]
