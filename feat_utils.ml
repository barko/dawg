open Dog_t

let pr = Printf.printf
let epr = Printf.eprintf

let apply_max_gamma ~max_gamma gamma =
  if gamma < 0.0 then
    max gamma (~-. max_gamma)
  else
    min gamma max_gamma

let apply_max_gamma_opt ~max_gamma_opt left right =
  match max_gamma_opt with
    | None -> left, right
    | Some max_gamma ->
      apply_max_gamma ~max_gamma left, apply_max_gamma ~max_gamma right

let array_of_afeature = function
  | `Cat cat -> (
      let categories = Array.of_list cat.c_categories in
      let num_categories = Array.length categories in
      match cat.c_anonymous_category with
        | Some anon_value -> (

            let num_categories = num_categories + 1 in

            let string_opt_of_int value =
              if 0 <= value && value < anon_value then
                Some categories.( value )
              else if value = anon_value then (* anonymous *)
                None
              else if anon_value < value && value < num_categories then
                Some categories.( value - 1 )
              else
                assert false
            in
            match cat.c_vector with
              | `RLE (rle:Vec.t) ->
                let result = Array.make rle.Vec.length (-1, None) in
                Rlevec.iter rle (
                  fun ~index ~length ~value ->
                    let string_opt = value, string_opt_of_int value in
                    for i = index to index + length - 1 do
                      result.(i) <- string_opt
                    done
                );
                `StringAnon result

              | `Dense (vec:Vec.t) ->
                let result = Array.make vec.Vec.length (-1, None) in
                let width = Utils.num_bytes cat.c_cardinality in
                Dense.iter ~width vec (
                  fun ~index ~value ->
                    let string_opt = string_opt_of_int value in
                    result.(index) <- value, string_opt
                );
                `StringAnon result
          )
        | None -> (
            let category_0 = List.hd cat.c_categories in
            match cat.c_vector with
              | `RLE rle ->
                let result = Array.make rle.Vec.length (-1, category_0) in
                Rlevec.iter rle (
                  fun ~index ~length ~value ->
                    let res = value, categories.( value ) in
                    for i = index to index + length - 1 do
                      result.(i) <- res
                    done

                );
                `String result

              | `Dense vec ->
                let result = Array.make vec.Vec.length (-1, category_0) in
                let width = Utils.num_bytes cat.c_cardinality in
                Dense.iter ~width vec (
                  fun ~index ~value ->
                    result.(index) <- value, categories.( value )
                );
                `String result
          )
    )

  | `Ord { o_vector; o_breakpoints; o_cardinality } -> (
      match o_vector with
        | `RLE rle -> (
            match o_breakpoints with
              | `Float breakpoints ->
                let result = Array.make rle.Vec.length (-1, 0.0) in
                let breakpoints = Array.of_list breakpoints in

                Rlevec.iter rle (
                  fun ~index ~length ~value ->
                    let res = value, breakpoints.( value ) in
                    for i = index to index + length - 1 do
                      result.(i) <- res
                    done
                );
                `Float result

              | `Int breakpoints ->
                let result = Array.make rle.Vec.length (-1, 0) in
                let breakpoints = Array.of_list breakpoints in

                Rlevec.iter rle (
                  fun ~index ~length ~value ->
                    let res = value, breakpoints.( value ) in
                    for i = index to index + length - 1 do
                      result.(i) <- res
                    done
                );
                `Int result
          )
        | `Dense vec -> (
            let width = Utils.num_bytes o_cardinality in
            match o_breakpoints with
              | `Float breakpoints ->

                let result = Array.make vec.Vec.length (-1, 0.0) in
                let breakpoints = Array.of_list breakpoints in
                assert (o_cardinality = Array.length breakpoints);

                Dense.iter ~width vec (
                  fun ~index ~value ->
                    result.( index ) <- value, breakpoints.( value )
                );
                `Float result

              | `Int breakpoints ->

                let result = Array.make vec.Vec.length (-1, 0) in
                let breakpoints = Array.of_list breakpoints in
                assert (o_cardinality = Array.length breakpoints);

                Dense.iter ~width vec (
                  fun ~index ~value ->
                    result.( index ) <- value, breakpoints.( value )
                );
                `Int result
          )
    )


let id_of_feature = function
  | `Cat { c_feature_id } -> c_feature_id
  | `Ord { o_feature_id } -> o_feature_id

let name_of_feature = function
  | `Cat { c_feature_name_opt } -> c_feature_name_opt
  | `Ord { o_feature_name_opt } -> o_feature_name_opt

let cardinality_of_feature = function
  | `Cat { c_cardinality } -> c_cardinality
  | `Ord { o_cardinality } -> o_cardinality

let vector_of_feature = function
  | `Cat { c_vector } -> c_vector
  | `Ord { o_vector } -> o_vector

let folds_of_feature ~n ~num_folds = function
  | `Ord { o_cardinality; o_vector } ->
    assert ( o_cardinality <= n );
    let cardinality_per_fold = o_cardinality / num_folds in
    if cardinality_per_fold = 0 then
      `TooManyOrdinalFolds o_cardinality
    else
      let folds = Array.make n (-1) in
      (match o_vector with
        | `RLE rle ->
          Rlevec.iter rle (
            fun ~index ~length ~value ->
              let fold = value / cardinality_per_fold in
              for i = index to index + length - 1 do
                folds.(i) <- fold
              done
          );

        | `Dense vec ->
          let width_num_bytes = Utils.num_bytes o_cardinality in
          Dense.iter ~width:width_num_bytes vec (
            fun ~index ~value ->
              let fold = value / cardinality_per_fold in
              folds.(index) <- fold
          );
      );
      `Folds folds

  | `Cat { c_cardinality; c_vector } ->
    if c_cardinality < num_folds then
      `TooManyCategoricalFolds c_cardinality
    else
      let folds = Array.make n (-1) in
      (match c_vector with
        | `RLE rle ->
          Rlevec.iter rle (
            fun ~index ~length ~value ->
              for i = index to index + length - 1 do
                folds.(i) <- value mod num_folds
              done
          );

        | `Dense vec ->
          let width_num_bytes = Utils.num_bytes c_cardinality in
          Dense.iter ~width:width_num_bytes vec (
            fun ~index ~value ->
              folds.(index) <- value mod num_folds
          )
      );
      `Folds folds

let weights_of_afeature = function
  | `Cat { c_feature_name_opt = Some feature_name } ->
    epr "[ERROR] feature %s is categorical but a float feature is needed" feature_name;
    exit 1
  | `Cat { c_feature_id = feature_id } ->
    epr "[ERROR] nameless feature id %d is categorical but a float feature is needed" feature_id;
    exit 1
  | afeature ->
    match array_of_afeature afeature with
      | `String _ -> assert false
      | `StringAnon _ -> assert false
      | `Int id_weight_array ->
        let total = Array.fold_left (fun accu (id, w_int) -> accu + w_int) 0 id_weight_array in
        let total = float total in
        Array.map (fun (id, w_int) -> float w_int /. total) id_weight_array
      | `Float id_weight_array ->
        let total = Array.fold_left (fun accu (id, w) -> accu +. w) 0.0 id_weight_array in
        Array.map (fun (id, w) -> w /. total) id_weight_array

let vector f = function
  | `RLE rle -> `RLE (f rle)
  | `Dense dense -> `Dense (f dense)


let i_to_a f = function
  | `Cat {
      c_feature_id;
      c_feature_name_opt;
      c_anonymous_category;
      c_categories;
      c_cardinality;
      c_vector;
    } ->
    `Cat {
      c_feature_id;
      c_feature_name_opt;
      c_anonymous_category;
      c_categories;
      c_cardinality;
      c_vector = vector f c_vector;
    }

  | `Ord {
      o_feature_id;
      o_feature_name_opt;
      o_cardinality;
      o_breakpoints;
      o_vector;
    } ->
    `Ord {
      o_feature_id;
      o_feature_name_opt;
      o_cardinality;
      o_breakpoints;
      o_vector = vector f o_vector;
    }

type feature_descr = [ `Name of string | `Id of int ]

let string_of_feature_descr = function
  | `Name name -> Printf.sprintf "name:%s" name
  | `Id id -> Printf.sprintf "id:%d" id

let feature_descr_of_string = function
  | RE "name:" (_+ as name) -> Some (`Name name)
  | RE "id:" (int as id) -> Some (`Id (int_of_string id))
  | _ -> None
