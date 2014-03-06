type t

open Model_t

(* Support OCaml 3.12 *)
module List = struct
  include List

  let iteri f l =
    let rec iteri i = function
      | hd :: tl -> f i hd; iteri (succ i) tl
      | [] -> ()
    in iteri 0 l
end

(* evaluate a single tree, using a function [get] to get feature
   values by their id *)
let rec eval_tree get = function
  | `Leaf value -> value
  | `OrdinalNode { on_feature_id; on_split; on_left_tree; on_right_tree } ->
    assert ( on_feature_id >= 0 );
    let value =
      match get `ORD on_feature_id with
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
      match get `CAT cn_feature_id with
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

type square = {
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

type logistic = {
  sq : square;
  positive_category : string;
  invert : bool;
}

type evaluator = [ `Logistic of logistic | `Square of square ]

let index_features features num_features =
  let id_to_feature = Hashtbl.create num_features in
  let name_to_feature = Hashtbl.create num_features in
  List.iter (
    fun feature ->
      match feature with
        | `OrdinalFeature { of_feature_id; of_feature_name_opt } -> (
            Hashtbl.replace id_to_feature of_feature_id feature;
            match of_feature_name_opt with
              | Some feature_name ->
                Hashtbl.replace name_to_feature feature_name feature

              | None -> ()
          )

        | `CategoricalFeature {cf = { cf_feature_id; cf_feature_name_opt }} -> (
            Hashtbl.replace id_to_feature cf_feature_id feature;
            match cf_feature_name_opt with
              | Some feature_name ->
                Hashtbl.replace name_to_feature feature_name feature

              | None -> ()
          )

  ) features;
  id_to_feature, name_to_feature

let create model_file_path =
  (* the the contents of the file into a string *)
  let model_s = Mikmatch.Text.file_contents model_file_path in
  (* parse the string into a model *)
  let model = Model_j.c_model_of_string model_s in
  match model with
    | `Square square ->
      let trees = rle_to_array square.re_trees in
      let features = add_category_to_id square.Model_t.re_features in

      (* used in correctly sizing the [Hashtbl] in the [eval_*] fuctions *)
      let num_features = List.length features in

      let id_to_feature, name_to_feature =
        index_features features num_features in

      `Square { trees ; num_features; id_to_feature; name_to_feature }

    | `Logistic logistic ->
      let trees = rle_to_array logistic.bi_trees in
      let features = add_category_to_id logistic.Model_t.bi_features in

      let num_features = List.length features in

      let id_to_feature, name_to_feature =
        index_features features num_features in

      let sq = { trees ; num_features; id_to_feature; name_to_feature } in
      let positive_category = logistic.bi_positive_category in
      `Logistic { sq; positive_category; invert = false }

(* invert the polarity of the classifier *)
let invert logistic =
  { logistic with invert = not logistic.invert }

type feature_key = [ `Id of int | `Name of string ]
type feature_value = [ `Float of float | `String of string ]

exception TypeMismatch of (feature_key * feature_value)
exception CategoryNotFound of (feature_key * string)
exception CategoryMissing of feature_key
exception ValueMissing of feature_key

type feature_vector = (feature_key * feature_value) list

let eval_square sq feature_vector missing_ok =
  (* build a map from integer feature id to feature value for all
     features values provided in [feature_vector].  Ignore feature
     values provided in [feature_vector] that are not required by the
     model. Check that categories (values of a categorical feature)
     are ones known in the model. *)
  let id_to_feature = Hashtbl.create sq.num_features in

  List.iter (
    fun (key, value) ->
      try
        let feature =
          match key with
            | `Id feature_id ->
              Hashtbl.find sq.id_to_feature feature_id

            | `Name feature_name ->
              Hashtbl.find sq.name_to_feature feature_name
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
      with Not_found ->
        (* ignore feature id's and names that are not referenced by the model *)
        ()
  ) feature_vector;

  let get kind feature_id =
    try
      Hashtbl.find id_to_feature feature_id
    with Not_found ->
      if missing_ok then
        match kind with
          | `ORD ->
            (* implicit value is zero *)
            `Float 0.0

          | `CAT ->
            match Hashtbl.find sq.id_to_feature feature_id with
              | `OrdinalFeature _ -> assert false
              | `CategoricalFeature cat ->
                (* is this categorical feature have an anonymous category? *)
                match cat.cf.cf_anonymous_category_index_opt with
                  | Some cat_id -> (`String cat_id)
                  | None -> raise (CategoryMissing (`Id feature_id))
      else
        raise (ValueMissing (`Id feature_id))
  in
  eval_trees get sq.trees

let logistic_probability f =
  let f2 = -2.0 *. f in
  let ef2 = exp f2 in
  1. /. ( 1. +. ef2 )

let eval evaluator ?(missing_ok=true) feature_vector =
  match evaluator with
    | `Square sq -> eval_square sq feature_vector missing_ok
    | `Logistic { sq; invert } ->
      let f = eval_square sq feature_vector missing_ok in
      let p = logistic_probability f in
      if invert then
        1. -. p
      else
        p

let positive_category { positive_category } =
  positive_category

let square_feature_id_and_name_list { id_to_feature } =
  Hashtbl.fold (
    fun feature_id feature accu ->
      let feature_id_name =
        match feature with
          | `CategoricalFeature cat ->
            cat.cf.cf_feature_id, cat.cf.cf_feature_name_opt

          | `OrdinalFeature ord ->
            ord.of_feature_id, ord.of_feature_name_opt
      in
      feature_id_name :: accu
  ) id_to_feature []

let feature_id_and_name_list = function
  | `Logistic logistic -> square_feature_id_and_name_list logistic.sq
  | `Square sq -> square_feature_id_and_name_list sq
