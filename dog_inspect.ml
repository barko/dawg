let pr = Printf.printf

let meta path key_opt value_opt =
  let open Feat_map in
  let reader = Dog_io.create_reader path in
  let dog = Dog_io.dog reader in
  let feature_set = Feat_map.create reader in

  match key_opt, value_opt with
    | Some "id", Some value -> (
        let target_feature_id = int_of_string value in
        try
          let feature = Feat_map.find_by_id feature_set target_feature_id in
          let feature_s = Dog_j.string_of_ifeature feature in
          print_endline (Yojson.Safe.prettify feature_s)
        with Not_found ->
          pr "feature with id %d not found\n%!" target_feature_id;
          exit 1
      )

    | Some "name", Some value -> (
        let feature_opt = Feat_map.find_by_name_opt feature_set value in
        match feature_opt with
          | Some feature ->
            let feature_s = Dog_j.string_of_ifeature feature in
            print_endline (Yojson.Safe.prettify feature_s)

          | None ->
            pr "feature with name %s not found\n%!" value;
            exit 1
      )

    | Some unknown, Some _ ->
      pr "unknown search key %S (either \"id\", \
          \"name\") with a corresponding value)\n%!" unknown;
      exit 1

    | None, Some value ->
      pr "must specify search key (either \"id\", \
          \"name\") along with the value %S\n%!" value;
      exit 1

    | Some key, None ->
      pr "must specify search value along with key %S\n%!" key;
      exit 1

    | None, None ->
      let dog_s = Yojson.Safe.prettify (Dog_j.string_of_t dog) in
      print_endline dog_s

let string_or_empty = function
  | Some s -> s
  | None -> ""

let meta_short path =
  let open Feat_map in
  let reader = Dog_io.create_reader path in
  let _dog = Dog_io.dog reader in
  let feature_map = Feat_map.create reader in
  let open Dog_t in
  Feat_map.iter feature_map (
    fun feature ->
      match feature with
        | `Ord { o_feature_id; o_feature_name_opt; o_cardinality } ->
          pr "ord,%d,%s,%d\n"
            o_feature_id (string_or_empty o_feature_name_opt) o_cardinality
        | `Cat { c_feature_id; c_feature_name_opt; c_cardinality } ->
          pr "cat,%d,%s,%d\n" c_feature_id
            (string_or_empty c_feature_name_opt) c_cardinality
  )

let select path target_feature_id =
  let reader = Dog_io.create_reader path in
  let dog = Dog_io.dog reader in

  let map = Feat_map.create reader in
  let feature_opt = Feat_map.find_by_id_opt map target_feature_id in
  let ifeature =
    match feature_opt with
      | None ->
        pr "feature with id %d not found\n%!" target_feature_id;
        exit 1

      | Some feature -> feature
  in

  let afeature = Feat_map.i_to_a map ifeature in

  let n = dog.Dog_t.num_observations in
  let array = Feat_utils.array_of_afeature afeature in

  match array with
    | `Float a ->
      for i = 0 to n-1 do
        pr "%d %f\n" i a.(i)
      done

    | `Int a ->
      for i = 0 to n-1 do
        pr "%d %d\n" i a.(i)
      done

    | `String a ->
      for i = 0 to n-1 do
        pr "%d %s\n" i a.(i)
      done

    | `StringAnon a ->
      for i = 0 to n-1 do
        match a.(i) with
          | None   -> pr "%d {}\n" i
          | Some s -> pr "%d %s\n" i s

      done


open Cmdliner
let commands =
  let dog_path =
    let doc = "the path of the dog file" in
    Arg.(required & pos 0 (some string) None & info [] ~doc)
  in

  let meta_cmd =
    let doc = "show a dog file's metadata either for a single feature \
               or all of them" in
    let key =
      let doc = "limit result to a single feature; \
                 search by either \"id\" or \"name\"" in
      Arg.(value & opt (some string) None & info ["k";"key"] ~doc)
    in

    let value =
      let doc = "limit result to a single feature; \
                 value associated with key" in
      Arg.(value & opt (some string) None & info ["v";"value"] ~doc)
    in

    Term.(pure meta $ dog_path $ key $ value),
    Term.info "dog-meta" ~doc
  in

  let meta_short_cmd =
    let doc = "show a short version of the dog feature metadata" in
    Term.( pure meta_short $ dog_path ), Term.info "dog-meta-short" ~doc
  in

  let select_cmd =
    let doc = "show the data for a single feature" in
    let feature_id =
      let doc = "limit display of metadata to this feature id" in
      Arg.(required & pos 1 (some int) None & info [] ~doc)
    in
    Term.(pure select $ dog_path $ feature_id),
    Term.info "dog-select" ~doc
  in

  [meta_cmd; select_cmd; meta_short_cmd]
