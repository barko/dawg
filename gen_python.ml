(* generate python code to evaluate a model *)

open Gen_code
open Model_t

(* translate a single tree into a code block *)
let rec py_code_of_tree = function
  | `OrdinalNode {
      on_feature_id;
      on_split;
      on_left_tree;
      on_right_tree } ->
    `Inline [
      `Line (sp "if ord_%d <= %.17g:" on_feature_id on_split);
      `Block [py_code_of_tree on_left_tree];
      `Line "else:";
      `Block [py_code_of_tree on_right_tree];
    ]

  | `CategoricalNode {
      cn_feature_id;
      cn_category_directions;
      cn_left_tree;
      cn_right_tree } ->
    `Inline [
      `Line (sp "if go_left_%d[ cat_id_%d ]:"
               cn_category_directions cn_feature_id);
      `Block [py_code_of_tree cn_left_tree];
      `Line "else:";
      `Block [py_code_of_tree cn_right_tree];
    ]

  | `Leaf leaf ->
    `Line (sp "r += %.17g" leaf)

let string_of_direction = function
  | `Left  -> "T"
  | `Right -> "F"

(* create a string consisting of comman seperated booleans ('T' or
   'F') , where each value answers 'go left?' *)
let list_of_category_directions_rle rle =
  let category_directions_rev =
    List.rev (Array.to_list (Model_utils.category_array_of_rle rle)) in

  let elements = String.concat ","
      (List.rev_map string_of_direction category_directions_rev) in
  elements


exception AnonymousFeature of int (* feature id *)

let empty_line = `Line ""

(* code to bind entries from the feature vector, assumed a python
   dictionary which maps feature names (string) to values, which are
   either floating point numbers (for ordinal features) or strings
   (categorical features). *)
let py_extract_feature_values features =
  List.fold_left (
    fun code feature ->
      let lines =
        match feature with
          | `OrdinalFeature { of_feature_id; of_feature_name_opt } ->
            let feature_name =
              match of_feature_name_opt with
                | Some feature_name -> feature_name
                | None -> raise (AnonymousFeature of_feature_id)
            in
            `Inline [
              (* absence of a value in the python dictionary
                 ["fv"] implicitly defines the value as [0.0] *)
              `Line "try:";
              `Block [
                (* convert the input to a float, or die trying *)
                `Line (sp "ord_%d = float( fv[ '%s' ] )"
                         of_feature_id feature_name);
              ];
              `Line "except KeyError:";
              `Block [
                `Line (sp "ord_%d = 0." of_feature_id)
              ];
              empty_line
            ]

          | `CategoricalFeature {
              cf_feature_id;
              cf_feature_name_opt;
              cf_categories;
              cf_anonymous_category_index_opt
            } ->

            let feature_name =
              match cf_feature_name_opt with
                | Some feature_name -> feature_name
                | None -> raise (AnonymousFeature cf_feature_id)
            in

            match cf_anonymous_category_index_opt with
              | Some index ->
                `Inline [
                  (* absence of a value in the python dictionary
                     ["fv"] implicitly defines the value as [index] *)
                  `Line "try:";
                  `Block [
                    (* find the category string *)
                    `Line (sp "cat_%d = fv[ '%s' ]"
                             cf_feature_id feature_name);
                    `Line "try:";
                    `Block [
                      `Line (sp "cat_id_%d = cat_to_id_%d[ cat_%d ]"
                               cf_feature_id cf_feature_id cf_feature_id
                            );
                    ];
                    `Line "except KeyError:";
                    `Block [
                      `Line (sp "raise UnknownCategory( '%s', cat_%d )"
                               feature_name cf_feature_id )
                    ]
                  ];
                  `Line "except KeyError:";
                  `Block [
                    (* category string not found; use the anonymous
                       category index as the default *)
                    `Line (sp "cat_id_%d = %d" cf_feature_id index)
                  ];
                  empty_line
                ]

              | None ->
                (* here, anonymous categories are not allowed *)
                `Inline [
                  `Line "try:";
                  `Block [
                    `Line (sp "cat_%d = fv[ '%s' ]"
                             cf_feature_id feature_name);
                    `Line "try:";
                    `Block [
                      `Line (sp "cat_id_%d = cat_to_id_%d[ cat_%d ]"
                               cf_feature_id cf_feature_id cf_feature_id
                            );
                    ];
                    `Line "except KeyError:";
                    `Block [
                      `Line (sp "raise UnknownCategory( '%s', cat_%d )"
                               feature_name cf_feature_id);
                    ]
                  ];
                  `Line "except KeyError:";
                  `Block [
                    `Line (sp "raise MissingCategoricalFeatureValue( '%s' )"
                             feature_name);
                  ];

                  empty_line
                ]

      in
      lines :: code
  ) [] features

(* for each categorical feature, define the mapping from category
   (string) to category id (integer), the latter being an index into the
   category directions array of booleans *)
let py_category_to_index {
    cf_feature_id;
    cf_categories;
    cf_anonymous_category_index_opt
  } =
  match cf_anonymous_category_index_opt with
    | None ->
      let bindings = foldi_left (
          fun category_id bindings category ->
            let binding = sp "'%s':%d" category category_id in
            binding :: bindings
        ) 0 [] cf_categories in
      (* python dictionary bound to a variable *)
      sp "cat_to_id_%d = {%s}" cf_feature_id (String.concat "," bindings)

    | Some anon_index ->
      let bindings = foldi_left (
          fun category_id bindings category ->
            let bindings =
              if category_id = anon_index then
                bindings
              else
                let binding = sp "'%s':%d" category category_id in
                binding :: bindings
            in
            bindings
        ) 0 [] cf_categories in
      sp "cat_to_id_%d = {%s}" cf_feature_id (String.concat "," bindings)

let py_category_to_index_stmts features =
  List.fold_left (
    fun code feature ->
      match feature with
        | `CategoricalFeature cf ->
          let line = `Line (py_category_to_index cf) in
          line :: code

        | `OrdinalFeature _ ->
          code

  ) [] features

let py_exceptions = [
  `Line "class MissingCategoricalFeatureValue( Exception ):";
  `Block [
    `Line "def __init__( self, feature_name ):";
    `Block [
      `Line "self.feature_name = feature_name"
    ]
  ];
  empty_line;
  `Line "class UnknownCategory( Exception ):";
  `Block [
    `Line "def __init__( self, feature_name, category ):";
    `Block [
      `Line "self.feature_name = feature_name";
      `Line "self.category = category"
    ]
  ];
]

let now_datetime_string () =
  let open Unix in
  let now = localtime (gettimeofday ()) in
  let { tm_sec; tm_min;  tm_hour; tm_mday; tm_mon; tm_year } = now in
  sp "%d/%02d/%02d %02d:%02d:%02d" (tm_year + 1900) (tm_mon + 1) (tm_mday)
    tm_hour tm_min tm_sec


(* generate the if/then statements making up the core of the decsion
   tree ensemble implementaiton *)
let py_code_of_trees trees =
  let code = List.rev_map py_code_of_tree trees in
  `Block [
    `Line "r = 0.";
    `Inline code;
  ]

let py_eval_function features trees model kind
    ~input_file_path ~model_md5 ~function_name ~output_logodds
    :
    Atd_indent.t list
    =
  let trees, category_directions_to_id =
    category_direction_ids_of_trees trees in

  (* code to define 'T' and 'F', aliases we use to save a few bytes in
     the output code file. *)
  let define_T_F = [
    `Line "T = True";
    `Line "F = False";
  ] in

  (* code to define the category directions lists for the tree nodes
     that require them *)
  let category_directions_lists = RLEMap.fold (
      fun category_directions_rle category_directions_id code ->
        let elements = list_of_category_directions_rle
            category_directions_rle in
        let line =
          `Line (sp "go_left_%d = [%s]" category_directions_id elements) in
        line :: code
    ) category_directions_to_id [] in

  let import_stmt =
    match kind with
      | `Logistic _ ->
        (* need math.exp *)
        `Line "from math import exp"
      | `Square ->
        `Inline []
  in

  let transform =
    let return_stmt = `Line "return r" in
    match kind with
      | _ when output_logodds ->
        `Inline [return_stmt] (* noop *)
      | `Logistic invert ->
        let lines =
          if invert then
            (* only difference is sign on the [2] *)
            [`Line "r = 1. / (1. + exp(  2. * r ))"; return_stmt ]
          else
            [`Line "r = 1. / (1. + exp( -2. * r ))"; return_stmt ]
        in
        `Inline lines

      | `Square ->
        `Inline [return_stmt] (* noop *)
  in

  let user_name =
    try
      Unix.getenv "USER"
    with Not_found ->
      sp "id=%d" (Unix.getuid ())
  in

  let comments = [
      `Line (sp "# auto-generated by user %S on %s"
               user_name (now_datetime_string ()));
      `Line "# command-line invocation:";
      `Line ("#    " ^ (String.concat " " (Array.to_list Sys.argv)));
      `Line (sp "# input file: %s" (Utils.abspath input_file_path));
      `Line (sp "# input file md5: %s" model_md5)
  ] in

  [
    `Inline comments;
    empty_line;
    import_stmt;
    empty_line;
    `Inline py_exceptions;
    empty_line;
    `Inline define_T_F;
    empty_line;
    `Inline category_directions_lists;
    empty_line;
    `Inline (py_category_to_index_stmts features);
    empty_line;
    `Line (Printf.sprintf "def %s( fv ):" function_name);
    `Block (py_extract_feature_values features);
    py_code_of_trees trees;
    `Block [transform];
  ]

let gen input_file_path output_file_path_opt function_name
    positive_category_opt output_logodds =
  let model, model_md5 =
    let model_s = Mikmatch.Text.file_contents input_file_path in
    let model_md5 = Digest.(to_hex (string model_s)) in
    Model_j.c_model_of_string model_s, model_md5
  in

  let trees, features =
    match model with
      | `Logistic { bi_trees; bi_features } ->
        bi_trees, bi_features
      | `Square { re_trees; re_features } ->
        re_trees, re_features
  in

  let kind =
    match model with
      | `Logistic { bi_positive_category; bi_negative_category_opt } -> (
          match positive_category_opt, bi_negative_category_opt with
            | Some positive_category, None ->
              `Logistic (positive_category <> bi_positive_category)

            | Some positive_category, Some bi_negative_category ->
              if positive_category <> bi_positive_category &&
                 positive_category <> bi_negative_category then (
                Printf.printf "unknown target category %S\n%!"
                  positive_category;
                exit 1
              );

              `Logistic (
                positive_category <> bi_positive_category &&
                positive_category = bi_negative_category
              )
            | None, _ ->
              `Logistic false
        )
      | `Square _ ->
        match positive_category_opt with
          | Some _ ->
            Printf.eprintf
              "[WARNING] file %S contains a regression model, not a \
                logistic model as implied by the positive \
                category argument\n%!"
              input_file_path;
            `Square
          | None -> `Square
  in
  let code = py_eval_function features trees model kind
      ~input_file_path ~model_md5 ~function_name  ~output_logodds in

  let ouch =
    match output_file_path_opt with
      | Some path -> open_out path
      | None -> stdout
  in
  Atd_indent.to_channel ouch code;
  close_out ouch

open Cmdliner

let commands =
  let gen_cmd =
    let doc = "translate a model into Python code" in

    let input_file_path =
      let doc = "path of input model file" in
      Arg.(required & opt (some string) None &
           info ["i";"input"] ~docv:"PATH" ~doc)
    in
    let output_file_path =
      let doc = "path of output file (absent=stdout)" in
      Arg.(value & opt (some string) None &
           info ["o";"output"] ~docv:"PATH" ~doc)
    in
    let function_name =
      let doc = "name of C/C++ function to be generated" in
      Arg.(value & opt string "eval_by_name" &
           info ["f";"function"] ~docv:"STRING" ~doc)
    in
    let positive_category =
      let doc = "the positive target class (implies logistic model)" in
      Arg.(value & opt (some string) None &
           info ["p";"positive"] ~docv:"STRING" ~doc)
    in
    let output_logodds =
      let doc = "output is a log-odds value instead of a probability value" in
      Arg.(value & flag & info ["l";"log-odds"] ~doc)
    in

    Term.(pure gen $
            input_file_path $
            output_file_path $
            function_name $
            positive_category $
            output_logodds
    ),
    Term.info "python" ~doc
  in
  [gen_cmd]
