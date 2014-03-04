(* Support OCaml 3.12 *)
module List = struct
  include List

  let iteri f l =
    let rec iteri i = function
      | hd :: tl -> f i hd; iteri (succ i) tl
      | [] -> ()
    in iteri 0 l
end

(* evaluate binary classification models over a csv file *)

(* the reason we do not use module [Eval] eval-libs/ocaml is that we
   want to do more work here; namely, compute feature importance as we
   evaluate the model *)

exception TypeMismatch of (int * Csv_types.value)

(* model calls or a categorical (ordinal) type, but value
   presented is ordinal (categorical) *)

exception MissingValue of int
(* data presented does not have a value for a particular feature id *)

exception UnknownCategory of (int * string)
(* a category presented as a value for a categorical feature is
   unknown, in that it was not present at training *)

open Model_t

type feature_importance_score = float option

type feature_importance = {
  unsigned : feature_importance_score;
  positive : feature_importance_score;
  negative : feature_importance_score
}

module IntMap = Utils.XMap( Utils.Int )

let max_opt y = function
  | None -> Some y
  | Some x -> Some (max x y)

let f_incr_opt delta = function
  | None -> Some delta
  | Some x -> Some (x +. delta)

let i_incr_opt delta = function
  | None -> Some delta
  | Some x -> Some (x + delta)

let i_to_f_opt = function
  | None -> None
  | Some x -> Some (float x)

let div_opt x y =
  match x, y with
    | None, _ -> None
    | _, None -> None
    | Some x, Some y -> Some (x /. y)

let update_importance leaf branch branch_size feature_id_to_importance =
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
               trivial, therefore not allowed *)
        assert false
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
        let unsigned = f_incr_opt score unsigned in
        let positive, negative =
          match sign with
            | `Neg ->
              (* update negative *)
              let negative = feature_importance.negative in
              let negative = f_incr_opt score negative in
              feature_importance.positive, negative

            | `Pos ->
              (* update negative *)
              let positive = feature_importance.positive in
              let positive = f_incr_opt score positive in
              positive, feature_importance.negative

        in
        let feature_importance = { unsigned; positive; negative } in
        IntMap.add feature_id feature_importance map

    ) feature_id_to_importance branch

(* for each importance metric, normalize the scores so that the
   highest scoring feature has value 1 *)
let normalize_feature_importance feature_id_to_importance =
  let sup0 = { unsigned = None; positive = None; negative = None } in
  let sup = IntMap.fold (
      fun feature_id fi sup ->

        let unsigned = max sup.unsigned fi.unsigned in
        let positive = max sup.positive fi.positive in
        let negative = max sup.negative fi.negative in
        { unsigned; positive; negative }

    ) feature_id_to_importance sup0 in

  (* now divide each by [sup] *)
  let norm = IntMap.fold (
      fun feature_id fi accu ->
        let unsigned = div_opt fi.unsigned sup.unsigned in
        let positive = div_opt fi.positive sup.positive in
        let negative = div_opt fi.negative sup.negative in

        let sum_importance = ref 0.0 in
        let count = ref 0 in
        (match unsigned with
          | Some x -> sum_importance := !sum_importance +. x; incr count
          | None -> ()
        );
        (match positive with
          | Some x -> sum_importance := !sum_importance +. x; incr count
          | None -> ()
        );
        (match negative with
          | Some x -> sum_importance := !sum_importance +. x; incr count
          | None -> ()
        );
        assert ( !count > 0 );
        let average_importance = !sum_importance /. (float !count) in
        let fi= { unsigned; positive; negative } in
        (feature_id, average_importance, fi) :: accu

    ) feature_id_to_importance [] in

  (* sort by average importance, descending *)
  List.sort (fun (_,a1,_) (_,a2,_) -> Pervasives.compare a2 a1) norm


open Printf
let pr = printf
let fpf = fprintf

let in_0_1 v =
  0.0 <= v && v <= 1.0

let in_0_1_opt = function
  | None -> true
  | Some v -> in_0_1 v

let string_of_float_opt = function
  | None -> ""
  | Some f -> sprintf "%.4e" f

let print_feature_importance ch feature_id_to_importance feature_id_to_name =
  fprintf ch "id,name,average,unsigned,positive,negative\n";

  List.iter (
    fun (feature_id, average_importance, fi) ->
      assert( in_0_1 average_importance );
      assert( in_0_1_opt fi.unsigned );
      assert( in_0_1_opt fi.positive );
      assert( in_0_1_opt fi.negative );

      let feature_name = IntMap.find feature_id feature_id_to_name in
      fprintf ch "%d,%s,%.4e,%s,%s,%s\n"
        feature_id
        feature_name
        average_importance
        (string_of_float_opt fi.unsigned)
        (string_of_float_opt fi.positive)
        (string_of_float_opt fi.negative)

  ) feature_id_to_importance


let rec eval_tree get branch branch_size = function
  | `Leaf value -> value, branch, branch_size
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

let eval_tree get tree =
  eval_tree get [] 0 tree

let eval_trees get feature_id_to_importance trees =
  List.fold_left (
    fun (sum, map) tree ->
      let f, branch, branch_size = eval_tree get tree in
      let map = update_importance f branch branch_size map in
      sum +. f, map
  ) (0.0, feature_id_to_importance) trees


type categorical_entry = {
  category_to_index : (string, int) Hashtbl.t;
  anonymous_category_index_opt : int option;
}

(* given a feature id and an observation get the corresponding value
   from the observation *)
let mk_get features header =
  let id_to_categorical_entry = Hashtbl.create 10 in
  let feature_id_to_column_id = Hashtbl.create 10 in
  let column_id_to_feature_id = Hashtbl.create 10 in
  let column_name_to_column_id = Hashtbl.create 10 in

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
          cf_feature_name_opt;
        } ->
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
        Hashtbl.replace id_to_categorical_entry feature_id entry;

        (match cf_feature_name_opt with
          | Some feature_name -> (
              try
                let column_id =
                  Hashtbl.find column_name_to_column_id feature_name in
                add_to_id_map ~column_id ~feature_id;

              with Not_found ->
                pr "categorical feature %S is not present in input file\n%!"
                  feature_name;
                exit 1
            )
          | None -> ()

        );

      | `OrdinalFeature { of_feature_id = feature_id; of_feature_name_opt } -> (
          match of_feature_name_opt with
            | Some feature_name -> (
                try
                  let column_id =
                    Hashtbl.find column_name_to_column_id feature_name in
                  add_to_id_map ~column_id ~feature_id;
                with Not_found ->
                  pr "ordinal feature %S not present in input file\n%!"
                    feature_name;
                  exit 1
              )
            | None -> ()
        )

  ) features;

  let translate_value feature_id = function
    | `Int i -> `Float (float_of_int i) (* cast *)
    | `Float f -> `Float f
    | `String category ->
      try
        let entry = Hashtbl.find id_to_categorical_entry feature_id in
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
          raise (MissingValue feature_id)

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
            let entry = Hashtbl.find id_to_categorical_entry feature_id in
            match entry.anonymous_category_index_opt with
              | None -> raise (MissingValue feature_id)
              | Some index -> `String index

          with Not_found ->
            (* this is an ordinal feature, value is [0] *)
            `Float 0.0
  in
  get

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
    importance_file_path
  =

  let pch =
    match prediction_file_path with
      | None -> stdout
      | Some path -> open_out path
  in

  let ich =
    match importance_file_path with
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
    match Csv_io.of_channel csv_ch with
      | `Ok (header, next_row) -> header, next_row

      | `SyntaxError loc ->
        print_endline (Csv_io.string_of_error_location loc);
        exit 1

      | `UnterminatedString line_num ->
        printf "unterminated string on line %d\n%!" line_num;
        exit 1

      | `IntOverflow (line, offending_string) ->
        Printf.printf "value %S on line %d cannot be represented as an integer\n%!"
          offending_string line;
        exit 1
  in

  let get = mk_get features header in

  let rec loop row_num feature_id_to_importance pch =
    match next_row () with
      | `Ok `EOF -> feature_id_to_importance
      | `Ok ((`Dense _ | `Sparse _ ) as row) ->

        let is_ok, feature_id_to_importance =
          try
            let f, feature_id_to_importance = eval_trees (get row)
                feature_id_to_importance trees in
            fprintf pch "%f\n" (transform f);
            true, feature_id_to_importance
          with
            | TypeMismatch (feature_id, value) ->
              printf "row %d: %s for feature %d is incompatible with the type \
                      of that feature\n%!"
                row_num
                (match value with
                  | `Int i -> sprintf "integer value %d" i
                  | `Float f -> sprintf "float value %f" f
                  | `String s -> sprintf "string value %S" s
                )
                feature_id;
              false, IntMap.empty

            | MissingValue feature_id ->
              printf "row %d: value for feature %d missing\n%!"
                row_num feature_id;
              false, IntMap.empty

            | UnknownCategory (feature_id, cat) ->
              printf "row %d: value %S for categorical feature %d \
                      is not recognized\n%!"
                row_num cat feature_id;
              false, IntMap.empty

        in
        if is_ok then
          loop (row_num + 1) feature_id_to_importance pch
        else
          exit 1

      | `SyntaxError loc ->
        print_endline (Csv_io.string_of_error_location loc);
        exit 1

      | `UnterminatedString line_num ->
        printf "unterminated string on line %d\n%!" line_num;
        exit 1

      | `IntOverflow (line, offending_string) ->
        Printf.printf "value %S on line %d cannot be represented as an integer\n%!"
          offending_string line;
        exit 1

  in
  (* report row number with 1-index *)
  let feature_id_to_importance = loop 1 IntMap.empty pch in

  (* compute and print feature importance report *)
  let norm_feature_id_to_importance = normalize_feature_importance
      feature_id_to_importance in

  let num_columns, feature_id_to_name = List.fold_left (
      fun (feature_id, feature_id_to_name) name ->
        let feature_id_to_name = IntMap.add feature_id name
            feature_id_to_name in
        feature_id + 1, feature_id_to_name
    ) (0, IntMap.empty) header in

  print_feature_importance ich norm_feature_id_to_importance feature_id_to_name;

  if pch <> stdout then
    close_out pch;

  if ich <> stdout then
    close_out ich

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
         info ["p";"positive"] ~docv:"STRING" ~doc)
  in

  let prediction_file_path =
    let doc = "path of file to containing predictions, one per observation \
               (absent=stdout)" in
    Arg.(value & opt (some string) None &
         info ["p";"prediction"] ~docv:"PATH" ~doc)
  in

  let importance_file_path =
    let doc = "path of file to contain feature importance report \
               (absent=stdout)" in
    Arg.(value & opt (some string) None & info ["importance"] ~docv:"PATH" ~doc)
  in

  let eval_cmd =
    let doc = "evaluate a binary classification model on each \
               row of a csv file" in
    Term.( pure model_eval $
             model_file_path $
             csv_file_path $
             prediction_file_path $
             positive_category $
             importance_file_path
         ),
    Term.info "eval" ~doc
  in
  [eval_cmd]

