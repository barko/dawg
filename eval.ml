(* evaluate binary classification models over a csv file *)

exception TypeMismatch of (int * Csv_types.value)

(* model calls or a categorical (ordinal) type, but value
   presented is ordinal (categorical) *)

exception ColumnOutOfBounds of int
exception MissingValue of int
(* data presented does not have a value for a particular feature id *)

exception UnknownCategory of (int * string)
(* a category presented as a value for a categorical feature is
   unknown, in that it was not present at training *)

open Model_t

open Printf
let epr = eprintf
let pr = printf
let fpf = fprintf

let in_0_1 v =
  0.0 <= v && v <= 1.0

let in_0_1_opt = function
  | None -> true
  | Some v -> in_0_1 v

let rec eval_tree get branch branch_size = function
  | `Leaf gamma -> gamma, branch, branch_size
  | `OrdinalNode { on_feature_id; on_split; on_left_tree; on_right_tree } ->
    assert ( on_feature_id >= 0 );
    let value =
      match get `Ord on_feature_id with
        | `Float value -> value
        | `String _ -> assert false (* type mismatch would have been raised *)
    in
    let sub_tree =
      if value <= on_split then
        on_left_tree
      else
        on_right_tree
    in
    eval_tree get (on_feature_id :: branch) (branch_size + 1) sub_tree

  | `CategoricalNode {
      cn_feature_id;
      cn_category_directions;
      cn_left_tree;
      cn_right_tree;
    } ->
    assert ( cn_feature_id >= 0 );
    let value =
      match get `Cat cn_feature_id with
        | `String index -> cn_category_directions.(index)
        | `Float _ -> assert false (* type mismatch would have been raised *)
    in
    let sub_tree =
      match value with
        | `Left -> cn_left_tree
        | `Right -> cn_right_tree
    in
    eval_tree get (cn_feature_id :: branch) (branch_size + 1) sub_tree

  | `LinearNode { ln_feature_id; ln_coef } ->
    assert ( ln_feature_id >= 0 );
    let value =
      match get `Ord ln_feature_id with
        | `Float value -> value
        | `String _ -> assert false (* type mismatch would have been raised *)
    in
    let gamma = ln_coef *. value in
    let branch = ln_feature_id :: branch in
    let branch_size = branch_size + 1 in
    gamma, branch, branch_size

let eval_tree get tree =
  eval_tree get [] 0 tree

let eval_trees get trees =
  List.fold_left (
    fun sum tree ->
      let f, branch, branch_size = eval_tree get tree in
      sum +. f
  ) 0.0 trees


type categorical_entry = {
  category_to_index : (string, int) Hashtbl.t;
  anonymous_category_index_opt : int option;
}

(* given a feature id and an observation get the corresponding value
   from the observation *)
let mk_get features header =
  let feature_id_to_categorical_entry = Hashtbl.create 10 in
  let feature_id_to_feature_name      = Hashtbl.create 10 in

  let column_id_to_feature_id  = Hashtbl.create 10 in
  let column_name_to_column_id = Hashtbl.create 10 in

  let feature_id_to_column_id = Hashtbl.create 10 in

  let add_to_id_map ~column_id ~feature_id =
    Hashtbl.add column_id_to_feature_id column_id feature_id;
    Hashtbl.add feature_id_to_column_id feature_id column_id
  in

  List.iteri (
    fun column_id column_name ->
      Hashtbl.replace column_name_to_column_id column_name column_id
  ) header;

  List.iter (
    function
      | `CategoricalFeature {
          cf_feature_id = feature_id;
          cf_categories;
          cf_anonymous_category_index_opt;
          cf_feature_name_opt = feature_name_opt;
        } -> (
          match feature_name_opt with
            | Some feature_name ->
              Hashtbl.replace feature_id_to_feature_name feature_id
                feature_name;
              (try
                  let column_id =
                    Hashtbl.find column_name_to_column_id feature_name in
                  add_to_id_map ~column_id ~feature_id;

                with Not_found ->
                  pr "categorical feature %S is not present in input file\n%!"
                     feature_name;
                  exit 1
              )
            | None ->
               add_to_id_map ~column_id:feature_id ~feature_id
        );

        let category_to_index = Hashtbl.create 10 in

        (match cf_anonymous_category_index_opt with
          | None ->
            List.iteri (
              fun index category ->
                Hashtbl.replace category_to_index category index
            ) cf_categories;

          | Some anon_index ->
            List.iteri (
              fun index category ->
                if index < anon_index then
                  Hashtbl.replace category_to_index category index
                else
                  Hashtbl.replace category_to_index category (index + 1)
            ) cf_categories
        );

        let entry = {
          anonymous_category_index_opt = cf_anonymous_category_index_opt;
          category_to_index;
        } in
        Hashtbl.replace feature_id_to_categorical_entry feature_id entry

      | `OrdinalFeature {
          of_feature_id = feature_id;
          of_feature_name_opt = feature_name_opt
        } -> (
          match feature_name_opt with
            | Some feature_name -> (
                Hashtbl.replace feature_id_to_feature_name feature_id
                  feature_name;
                try
                  let column_id =
                    Hashtbl.find column_name_to_column_id feature_name in
                  add_to_id_map ~column_id ~feature_id;
                with Not_found ->
                  pr "ordinal feature %S not present in input file\n%!"
                    feature_name;
                  exit 1
              )
            | None ->
               add_to_id_map ~column_id:feature_id ~feature_id
        )
  ) features;

  let translate_value feature_id = function
    | `Int i -> `Float (float_of_int i) (* cast *)
    | `Float f -> `Float f
    | `String category ->
      try
        let entry = Hashtbl.find feature_id_to_categorical_entry feature_id in
        try
          let index = Hashtbl.find entry.category_to_index category in
          `String index

        with Not_found ->
          (* perhaps this is filler for an anonymous category? *)
          match entry.anonymous_category_index_opt with
            | Some index -> (* assume that it is filler *) `String index
            | None ->
              raise (UnknownCategory (feature_id, category))

      with Not_found ->
        raise (TypeMismatch (feature_id, `String category))
  in

  let get = function
    | `Dense dense ->
      (* convert to array, for fast random access *)
      let dense = Array.of_list dense in
      let len_dense = Array.length dense in
      fun kind feature_id ->
        assert ( feature_id >= 0 );
        let column_id =
          try
            Hashtbl.find feature_id_to_column_id feature_id
          with Not_found ->
            pr "feature with id %d is anonymous" feature_id;
            exit 1
        in
        assert( column_id >= 0 );
        if column_id < len_dense then
          translate_value feature_id dense.(column_id)
        else
          raise (ColumnOutOfBounds feature_id)

    | `Sparse (sparse : (int * Csv_types.value) list) ->
      (* convert to hashtable, for faster random access *)
      let feature_id_to_value = Hashtbl.create 10 in
      List.iter (
        fun (column_id, value) ->
          try
            let feature_id = Hashtbl.find column_id_to_feature_id column_id in
            Hashtbl.replace feature_id_to_value feature_id value
          with Not_found ->
            (* don't care about this column *)
            ()
      ) sparse;

      fun kind feature_id ->
        assert ( feature_id >= 0 );
        try
          let value = Hashtbl.find feature_id_to_value feature_id in
          let tr_value = translate_value feature_id value in
          match tr_value, kind with
            | `Float _, `Ord
            | `String _, `Cat -> tr_value
            | _ ->
              raise (TypeMismatch (feature_id, value))

        with Not_found ->
          (* perhaps this is an anonymous value? *)
          try
            let entry = Hashtbl.find feature_id_to_categorical_entry
                feature_id in
            match entry.anonymous_category_index_opt with
              | None -> raise (MissingValue feature_id)
              | Some index -> `String index

          with Not_found ->
            (* this is an ordinal feature, value is [0] *)
            `Float 0.0
  in
  get, feature_id_to_feature_name

let normal f =
  let probability = Logistic.probability f in
  probability

let invert f =
  let probability = Logistic.probability f in
  1. -. probability

let noop f = f

let model_eval
    model_file_path
    csv_file_path_opt
    prediction_file_path
    positive_category_opt
    no_header
  =

  let pch =
    match prediction_file_path with
      | None -> stdout
      | Some path -> open_out path
  in

  let model_s = Mikmatch.Text.file_contents model_file_path in
  let model = Model_j.c_model_of_string model_s in

  let transform, trees, features =
    match positive_category_opt, model with
      | Some positive_category, `Logistic logistic -> (

          let transform =
            if positive_category = logistic.bi_positive_category then
              (* user requests the model's notion of positive; nothing to
                 do *)
              normal
            else
              match logistic.bi_negative_category_opt with
                | Some neg_category ->
                  if neg_category = positive_category then
                    (* invert polarity *)
                    invert
                  else (
                    pr "unknown target category %S\n%!" positive_category;
                    exit 1
                  )

                | None ->
                  (* negative category is anonymous; so any string will do *)
                  invert
          in
          transform, logistic.bi_trees, logistic.bi_features
        )

      | None, `Square square ->
        noop, square.re_trees, square.re_features

      | Some _, `Square _ ->
        pr "file %S contains a regression model, not a logistic model as \
            implied by the positive category argument\n%!" model_file_path;
        exit 1

      | None, `Logistic _ ->
        pr "file %S contains a logistic model, but no positive category was \
            provided\n%!" model_file_path;
        exit 1
  in

  (* decode category directions from rle to array *)
  let trees = Model_utils.rle_to_array trees in

  let csv_ch =
    match csv_file_path_opt with
      | None -> stdin
      | Some path -> open_in path
  in
  let header, next_row =
    match Csv_io.of_channel ~no_header csv_ch with
      | `Ok (header, next_row) -> header, next_row

      | `SyntaxError loc ->
        print_endline (Csv_io.string_of_error_location loc);
        exit 1

      | `UnterminatedString line_num ->
        epr "unterminated string on line %d\n%!" line_num;
        exit 1

      | `IntOverflow (line, offending_string) ->
        epr "value %S on line %d cannot be represented as an integer\n%!"
          offending_string line;
        exit 1
  in

  let get, feature_id_to_feature_name = mk_get features header in

  let rec loop row_num pch =
    match next_row () with
      | `Ok `EOF -> ()
      | `Ok ((`Dense _ | `Sparse _ ) as row) ->

        let is_ok =
          try
            let f = eval_trees (get row) trees in
            fprintf pch "%f\n" (transform f);
            true
          with
            | TypeMismatch (feature_id, value) ->
              epr "row %d: %s for feature %d is incompatible with the type \
                      of that feature\n%!"
                row_num
                (match value with
                  | `Int i -> sprintf "integer value %d" i
                  | `Float f -> sprintf "float value %f" f
                  | `String s -> sprintf "string value %S" s
                )
                feature_id;
              false

            | MissingValue feature_id ->
              epr "sparse row %d: value for feature %d missing\n%!"
                row_num feature_id;
              false

            | ColumnOutOfBounds feature_id ->
              epr "dense row %d: column out of bounds: %d\n%!"
                row_num feature_id;
              false

            | UnknownCategory (feature_id, cat) ->
              epr "row %d: value %S for categorical feature %d \
                      is not recognized\n%!"
                row_num cat feature_id;
              false

        in
        if is_ok then
          loop (row_num + 1) pch
        else
          exit 1

      | `SyntaxError loc ->
        print_endline (Csv_io.string_of_error_location loc);
        exit 1

      | `UnterminatedString line_num ->
        epr "unterminated string on line %d\n%!" line_num;
        exit 1

      | `IntOverflow (line, offending_string) ->
        epr "value %S on line %d cannot be represented as an integer\n%!"
          offending_string line;
        exit 1

  in
  (* report row number with 1-index *)
  loop 1 pch;

  if pch <> stdout then
    close_out pch

open Cmdliner

let commands =
  let model_file_path =
    let doc = "model file path" in
    Arg.(required & opt (some string) None &
         info ["m";"model"] ~docv:"PATH" ~doc)
  in

  let csv_file_path =
    let doc = "csv file path (absent=stdin)" in
    Arg.(value & opt (some string) None & info ["i";"input"] ~docv:"PATH" ~doc)
  in

  let positive_category =
    let doc = "the positive target class (implies logistic model)" in
    Arg.(value & opt (some string) None &
         info ["P";"positive"] ~docv:"STRING" ~doc)
  in

  let prediction_file_path =
    let doc = "path of file to containing predictions, one per observation \
               (absent=stdout)" in
    Arg.(value & opt (some string) None &
         info ["p";"prediction"] ~docv:"PATH" ~doc)
  in

  let no_header =
    let doc = "interpret the first line of the csv file as data, rather
               than a header providing names for the fields in file" in
    Arg.(value & flag & info ["h";"no-header"] ~doc)
  in

  let eval_cmd =
    let doc = "evaluate a binary classification model on each \
               row of a csv file" in
    Term.( pure model_eval $
             model_file_path $
             csv_file_path $
             prediction_file_path $
             positive_category $
             no_header
         ),
    Term.info "eval" ~doc
  in
  [eval_cmd]
