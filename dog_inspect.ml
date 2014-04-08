let pr = Printf.printf

let meta path feature_opt =
  let open Feat_map in
  let reader = Dog_io.RO.create path in
  let dog = Dog_io.RO.dog reader in

  match feature_opt with
    | Some feature -> (
        match Feat_utils.feature_descr_of_string feature with
          | Some descr -> (

              let map = Feat_map.create reader in
              let features = Feat_map.find map descr in
              match features with
                | [] ->
                  pr "feature(s) %s found\n%!" feature;
                  exit 1

                | _ ->
                  List.iter (
                    fun feature ->
                      let feature_s = Dog_j.string_of_ifeature feature in
                      print_endline (Yojson.Safe.prettify feature_s)
                  ) features
            )
          | None ->
            pr "malformed feature descriptor %S\n%!" feature;
            exit 1
      )

    | None ->
      let dog_s = Yojson.Safe.prettify (Dog_j.string_of_t dog) in
      print_endline dog_s

let string_or_empty = function
  | Some s -> s
  | None -> ""

let meta_short path =
  let open Feat_map in
  let reader = Dog_io.RO.create path in
  let _dog = Dog_io.RO.dog reader in
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

let select path feature =
  match Feat_utils.feature_descr_of_string feature with
    | None ->
      pr "malformed feature descriptor %S\n%!" feature;
      exit 1

    | Some descr ->

      let reader = Dog_io.RO.create path in
      let dog = Dog_io.RO.dog reader in
      let map = Feat_map.create reader in

      let ifeature =
        match Feat_map.find map descr with
          | [ ifeature ] -> ifeature

          | [] ->
            pr "feature %s not found\n%!" feature;
            exit 1

          | _ ->
            pr "feature %s not unique\n%!" feature;
            exit 1
      in
      let afeature = Feat_map.i_to_a map ifeature in

      let n = dog.Dog_t.num_observations in
      let array = Feat_utils.array_of_afeature afeature in

      match array with
        | `Float a ->
          for i = 0 to n-1 do
            let idx, v = a.(i) in
            pr "%d %d %f\n" i idx v
          done

        | `Int a ->
          for i = 0 to n-1 do
            let idx, v = a.(i) in
            pr "%d %d %d\n" i idx v
          done

        | `String a ->
          for i = 0 to n-1 do
            let idx, v = a.(i) in
            pr "%d %d %s\n" i idx v
          done

        | `StringAnon a ->
          for i = 0 to n-1 do
            match a.(i) with
              | idx, None   -> pr "%d %d {}\n" i idx
              | idx, Some v -> pr "%d %d %s\n" i idx v
          done


open Cmdliner
let commands =
  let dog_path =
    let doc = "the path of the dog file" in
    Arg.(required & pos 0 (some string) None & info [] ~doc)
  in

  let descr_doc = "limit result to a single feature, specified as either \
                   'name:<feature-name>' or 'id:<feature-id>'" in

  let meta_cmd =
    let doc = "show a dog file's metadata either for a single feature \
               or all of them" in

    let descr =
      let doc = descr_doc in
      Arg.(value & opt (some string) None & info ["f";"feature"] ~doc)
    in

    Term.(pure meta $ dog_path $ descr ),
    Term.info "dog-meta" ~doc
  in

  let meta_short_cmd =
    let doc = "show a short version of the dog feature metadata" in
    Term.( pure meta_short $ dog_path ), Term.info "dog-meta-short" ~doc
  in

  let select_cmd =
    let doc = "show the data for a single feature" in

    let descr =
      let doc = descr_doc in
      Arg.(required & opt (some string) None & info ["f";"feature"] ~doc)
    in

    Term.(pure select $ dog_path $ descr ),
    Term.info "dog-select" ~doc
  in

  [meta_cmd; select_cmd; meta_short_cmd]
