type t

open Model_t

(* evaluate a single tree, using a function [get] to get feature
   values by their id *)
let rec eval_tree get = function
  | `Leaf value -> value
  | `OrdinalNode { on_feature_id; on_split; on_left_tree; on_right_tree } ->
    assert ( on_feature_id >= 0 );
    let value =
      match get on_feature_id with
        | `Float value -> value
        | `String _ -> assert false (* type mismatch would have been raised *)
    in
    let sub_tree =
      if value <= on_split then
        on_left_tree
      else
        on_right_tree
    in
    eval_tree get sub_tree

  | `CategoricalNode {
      cn_feature_id;
      cn_category_directions;
      cn_left_tree;
      cn_right_tree;
    } ->
    assert ( cn_feature_id >= 0 );
    let value =
      match get cn_feature_id with
        | `String index -> cn_category_directions.(index)
        | `Float _ -> assert false (* type mismatch would have been raised *)
    in
    let sub_tree =
      match value with
        | `Left -> cn_left_tree
        | `Right -> cn_right_tree
    in
    eval_tree get sub_tree

(* evaluate multiple trees, summing their results *)
let eval_trees get trees =
  List.fold_left (
    fun sum tree ->
      let f = eval_tree get tree in
      sum +. f
  ) 0.0 trees


let opposite_direction = function
  | `Right -> `Left
  | `Left  -> `Right

let rec add_repeat length x accu =
  if length = 0 then
    accu
  else
    add_repeat (length-1) x (x :: accu)

let decode_rev =
  let rec loop vec_length accu = function
    | (run_length, value) :: t ->
      let accu = add_repeat run_length value accu in
      let vec_length = vec_length + run_length in
      loop vec_length accu t
    | [] ->
      vec_length, accu
  in
  fun runs ->
    loop 0 [] runs

let category_array_of_rle { dr_first_direction; dr_run_lengths } =
  (* first, add a direction to each run length, so we can use
     [decode_rev] *)
  let _, runs_rev = List.fold_left (
      fun (direction, runs_rev) run_length ->
        let runs_rev = (run_length, direction) :: runs_rev in
        let direction = opposite_direction direction in
        direction, runs_rev
    ) (dr_first_direction, []) dr_run_lengths in
  let _, directions = decode_rev runs_rev in
  Array.of_list directions

(* transform the [cn_category_directions] field of any categorical
   node in a tree, from a run-length-encoding representaition to array
   representation, to optimize evaluate speed (rather than memory or
   disk consumption) *)
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

(* add to the standard definition of categorical feature a map from
   category name to category id (the latter being the value stored in
   a tree's categorical node *)
type categorical_feature = {
  cf : Model_t.categorical_feature;
  category_to_id : (string, int) Hashtbl.t;
}

type feature = [
  | `OrdinalFeature of Model_t.ordinal_feature
  | `CategoricalFeature of categorical_feature
]

type sq = {
  trees : (float, direction_array) Model_t.tree list;
  num_features : int;
  id_to_feature : (int, feature) Hashtbl.t;
  name_to_feature : (string, feature) Hashtbl.t;
}

let add_category_to_id categorical_feature =
  let category_to_id = Hashtbl.create 10 in
  (match categorical_feature.cf_anonymous_category_index_opt with
    | None ->
      List.iteri (
        fun index category ->
          Hashtbl.replace category_to_id category index
      ) categorical_feature.cf_categories;

    | Some anon_index ->
      List.iteri (
        fun index category ->
          if index < anon_index then
            Hashtbl.replace category_to_id category index
          else
            Hashtbl.replace category_to_id category (index + 1)
      ) categorical_feature.cf_categories
  );

  { cf = categorical_feature; category_to_id }

let add_category_to_id features =
  List.rev_map (
    function
      | (`OrdinalFeature _) as f-> f
      | `CategoricalFeature cat ->
        `CategoricalFeature (add_category_to_id cat)
  ) features

let square_evaluator model_file_path =
  (* the the contents of the file into a string *)
  let model_s = Mikmatch.Text.file_contents model_file_path in
  (* parse the string into a model *)
  let model = Model_j.c_model_of_string model_s in
  let square =
    match model with
      | `Square square -> square
      | `Logistic _ -> failwith "expecting regression model"
  in
  let trees = rle_to_array square.re_trees in
  let features = add_category_to_id square.Model_t.re_features in

  (* used in correctly sizing the [Hashtbl] in the [eval_*] fuctions *)
  let num_features = List.length features in

  let id_to_feature = Hashtbl.create num_features in
  let name_to_feature = Hashtbl.create num_features in

  { trees ; num_features; id_to_feature; name_to_feature }

type feature_key = [ `Id of int | `Name of string ]
type feature_value = [ `Float of float | `String of string ]

exception FeatureNotFound of feature_key
exception TypeMismatch of (feature_key * feature_value)
exception CategoryNotFound of (feature_key * string)

type feature_vector = (feature_key * feature_value) list

let square_eval sq feature_vector =
  let id_to_feature = Hashtbl.create sq.num_features in
  List.iter (
    fun (key, value) ->
      let feature =
        match key with
          | `Id feature_id -> (
            try
              Hashtbl.find sq.id_to_feature feature_id
            with Not_found ->
              raise (FeatureNotFound key)
            )
          | `Name feature_name ->
            try
              Hashtbl.find sq.name_to_feature feature_name
            with Not_found ->
              raise (FeatureNotFound key)
      in
      match feature, value with
        | `OrdinalFeature ord, `Float f ->
          Hashtbl.replace id_to_feature ord.of_feature_id (`Float f)

        | `CategoricalFeature cat, `String category ->
          let cat_id =
            try
              Hashtbl.find cat.category_to_id category
            with Not_found ->
              raise (CategoryNotFound (key, category))
          in
          Hashtbl.replace id_to_feature cat.cf.cf_feature_id (`String cat_id)

        | _ -> raise (TypeMismatch (key, value))

  ) feature_vector;

  let get = Hashtbl.find id_to_feature in
  eval_trees get sq.trees

let logistic_evaluator model_file_path =
  ()
