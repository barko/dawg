type predicate =
  | Any
  | Name of string
  | ID of Dog_t.feature_id
  | NameRegExp of Pcre.regexp

type instruction = Include of predicate | Exclude of predicate

module StringMap = Utils.XMap(String)
module PointerMap = Utils.XMap(
  struct
    type t = Dog_t.pointer
    let compare = Pervasives.compare
  end
)

module FeatureSet = Set.Make(
  struct
    type t = Dog_t.pointer Dog_t.feature
    let compare = Pervasives.compare
  end
  )

let name_of_feature =
  let open Dog_t in
  function
    | `Binary b -> b.b_feature_name_opt
    | `BinaryAnon a -> a.a_feature_name_opt
    | `NAry n -> n.n_feature_name_opt
    | `NAryAnon m -> m.m_feature_name_opt
    | `Int o -> o.o_feature_name_opt
    | `Float o -> o.o_feature_name_opt

let id_of_feature =
  let open Dog_t in
  function
    | `Binary b -> b.b_feature_id
    | `BinaryAnon a -> a.a_feature_id
    | `NAry n -> n.n_feature_id
    | `NAryAnon m -> m.m_feature_id
    | `Int o -> o.o_feature_id
    | `Float o -> o.o_feature_id

let satisfies feature = function
  | Any -> true

  | Name feature_name -> (
    match name_of_feature feature with
      | Some fn -> fn = feature_name
      | None -> false
    )
  | ID feature_id ->
    feature_id = id_of_feature feature

  | NameRegExp rex ->
    match name_of_feature feature with
      | Some fn -> Pcre.pmatch ~rex fn
      | None -> false

type feature_partition = {
  included : FeatureSet.t;
  excluded : FeatureSet.t;
}

let eval_instruction partition = function
  | Include Any ->
    let included = FeatureSet.union partition.included partition.excluded in
    let excluded = FeatureSet.empty in
    { included; excluded }

  | Include ((Name _ | NameRegExp _ | ID _ ) as predicate) ->
    FeatureSet.fold (
      fun feature ({included; excluded} as p) ->
        if satisfies feature predicate then
          let excluded = FeatureSet.remove feature excluded in
          let included = FeatureSet.add feature included in
          {included; excluded}
        else
          p
    ) partition.excluded partition

  | Exclude Any ->
    let excluded = FeatureSet.union partition.included partition.excluded in
    let included = FeatureSet.empty in
    { included; excluded }

  | Exclude ((Name _ | NameRegExp _ | ID _ ) as predicate) ->
    FeatureSet.fold (
      fun feature ({included; excluded} as p) ->
        if satisfies feature predicate then
          let included = FeatureSet.remove feature included in
          let excluded = FeatureSet.add feature excluded in
          {included; excluded}
        else
          p
    ) partition.included partition

let eval_instructions partition instructions =
  List.fold_left (
    fun partition0 filtering_rule ->
      eval_instruction partition0 filtering_rule
  ) partition instructions

type bv_entry = {
  (* the feature of which [pointer] is a constituent *)
  feature : Dog_t.pointer Dog_t.feature;

  (* what are the pointers (integer offsets) into [bitvector_sequence]
     at which each of the bitvectors start? there are [p] pointers.
     bitvector [i] starts at [pointers.(i)], and its length is
     [pointers.(i+1) - pointers.(i)].  The last element of the
     pointers array is NOT the offset of a bitvector; rather it is the
     size in bytes of the block of bitvectors, and is there only to
     facillitate the computation of the length of each bitvector.  *)
  pointer : Dog_t.pointer;

  (* the length of the bitvector in bytes *)
  num_bytes : int ;
}

type bv_set = {
  s_num_observations : int;
  s_entries : bv_entry list;
  s_bitvector_sequence : BV7.uint8_array;
}

type t = {
  bitvector_sequence : BV7.uint8_array;
  num_observations : int;
  partition : feature_partition;
  num_pointers : int;
  entries : bv_entry list;
  features : Dog_t.features;
}

let eval_instructions t instructions =
  let partition = eval_instructions t.partition instructions in
  { t with partition }

let feature_set_of_features features =
  let open Dog_t in
  let snp =
    let set = FeatureSet.empty in
    let n = 0 in (* number of pointers *)
    let pfl = [] in (* list of pointer and features that refer to them *)
    set, n, pfl
  in

  let snp = List.fold_left (
      fun (set0, n0, pointers0) cat_binary ->
        let feature = `Binary cat_binary in
        let set0 = FeatureSet.add feature set0 in
        let n = n0 + 1 in
        let pointers = (cat_binary.b_bitvector, feature) :: pointers0 in
        set0, n, pointers
    ) snp features.cat_binary_a in

  let snp = List.fold_left (
      fun (set0, n0, pfl0) cat_binary_anon ->
        let feature = `BinaryAnon cat_binary_anon in
        let set = FeatureSet.add feature set0 in
        let n = n0 + 1 in
        let pfl = (cat_binary_anon.a_bitvector, feature) :: pfl0 in
        set, n, pfl
    ) snp features.cat_binary_anon_a in

  let snp = List.fold_left (
      fun (set0, n0, pfl0) cat_nary ->
        let feature = `NAry cat_nary in
        let set = FeatureSet.add feature set0 in
        let n = n0 + (List.length cat_nary.n_cpa) in
        let pflx = List.map (
            fun cp ->
              cp.bitvector, feature
          ) cat_nary.n_cpa in
        let pfl = List.rev_append pflx pfl0 in
        set, n, pfl
    ) snp features.cat_nary_a in

  let snp = List.fold_left (
      fun (set0, n0, pfl0) cat_nary_anon ->
        let feature = `NAryAnon cat_nary_anon in
        let set = FeatureSet.add feature set0 in
        let n = n0 + (List.length cat_nary_anon.m_cpa) in
        let pflx = List.map (
            fun cp ->
              cp.bitvector, feature
          ) cat_nary_anon.m_cpa in
        let pfl = List.rev_append pflx pfl0 in
        set, n, pfl
    ) snp features.cat_nary_anon_a in

  let snp = List.fold_left (
      fun (set0, n0, pfl0) ord_int ->
        let feature = `Int ord_int in
        let set = FeatureSet.add feature set0 in
        let n = n0 + (Array.length ord_int.o_bitvectors) in
        let pflx = List.map (
            fun bv ->
              bv, feature
          ) (Array.to_list ord_int.o_bitvectors) in
        let pfl = List.rev_append pflx pfl0 in
        set, n, pfl
    ) snp features.ord_int_a in

  let snp = List.fold_left (
      fun (set0, n0, pfl0) ord_float ->
        let feature = `Float ord_float in
        let set = FeatureSet.add feature set0 in
        let n = n0 + (Array.length ord_float.o_bitvectors) in
        let pflx = List.map (
            fun bv ->
              bv, feature
          ) (Array.to_list ord_float.o_bitvectors) in
        let pfl = List.rev_append pflx pfl0 in
        set, n, pfl
    ) snp features.ord_float_a in

  snp

let create dog_reader =
  let dog = Dog_io.dog dog_reader in
  let all_features_set, num_pointers, pointer_feature_assoc =
    feature_set_of_features dog.Dog_t.features in
  let partition = {
    included = all_features_set ;
    excluded = FeatureSet.empty
  } in

  (* sort the pointer, feature pairs in descending order of pointer;
     the difference between successive pointer i and pointer i+1 is
     the length (num_bytes) of bitvector i *)
  let pointer_feature_assoc = List.sort (
      fun (pointer1,_) (pointer2,_) -> Pervasives.compare pointer2 pointer1
    ) pointer_feature_assoc in

  let entries =
    let rec loop prev_pointer entries = function
      | (pointer, feature) :: rest ->
        let num_bytes = prev_pointer - pointer in
        assert (num_bytes > 0);
        let entries = { pointer; feature; num_bytes } :: entries in
        loop pointer entries rest
      | [] -> entries
    in
    let prev_pointer = Dog_io.end_bitvector_sequence_offset dog_reader in
    loop prev_pointer [] pointer_feature_assoc
  in
  { partition;
    bitvector_sequence = Dog_io.bitvector_sequence dog_reader;
    num_observations = dog.Dog_t.num_observations;
    num_pointers;
    entries;
    features = dog.Dog_t.features
  }

(* since we expect use of the function to be rare, a linear scan is
   ok *)
let find_feature_by_id t feature_id =
  let features = t.features in
  let open Dog_t in
  try
    let f = List.find (
        fun binary ->
          binary.b_feature_id = feature_id
      ) features.cat_binary_a in
    Some (`Binary f)
  with Not_found ->
    try
      let f = List.find (
          fun binary_anon ->
            binary_anon.a_feature_id = feature_id
        ) features.cat_binary_anon_a in
      Some (`BinaryAnon f)
    with Not_found ->
      try
        let f = List.find (
            fun nary ->
              nary.n_feature_id = feature_id
          ) features.cat_nary_a in
        Some (`NAry f)
      with Not_found ->
        try
          let f = List.find (
              fun nary_anon ->
                nary_anon.m_feature_id = feature_id
            ) features.cat_nary_anon_a in
          Some (`NAryAnon f)
        with Not_found ->
          try
            let f = List.find (
                fun ord_int ->
                  ord_int.o_feature_id = feature_id
              ) features.ord_int_a in
            Some (`Int f)
          with Not_found ->
            try
              let f = List.find (
                  fun ord_float ->
                    ord_float.o_feature_id = feature_id
                ) features.ord_float_a in
              Some (`Float f)
            with Not_found ->
              None

(* since we expect use of the function to be rare, a linear scan is
   ok *)
let find_feature_by_name t feature_name =
  let features = t.features in
  let open Dog_t in
  try
    let f = List.find (
        fun binary ->
          match binary.b_feature_name_opt with
            | Some name -> name = feature_name
            | None -> false
      ) features.cat_binary_a in
    Some (`Binary f)
  with Not_found ->
    try
      let f = List.find (
          fun binary_anon ->
            match binary_anon.a_feature_name_opt with
              | Some name -> name = feature_name
              | None -> false
        ) features.cat_binary_anon_a in
      Some (`BinaryAnon f)
    with Not_found ->
      try
        let f = List.find (
            fun nary ->
              match nary.n_feature_name_opt with
                | Some name -> name = feature_name
                | None -> false
          ) features.cat_nary_a in
        Some (`NAry f)
      with Not_found ->
        try
          let f = List.find (
              fun nary_anon ->
                match nary_anon.m_feature_name_opt with
                  | Some name -> name = feature_name
                  | None -> false
            ) features.cat_nary_anon_a in
          Some (`NAryAnon f)
        with Not_found ->
          try
            let f = List.find (
                fun ord_int ->
                  match ord_int.o_feature_name_opt with
                    | Some name -> name = feature_name
                    | None -> false
              ) features.ord_int_a in
            Some (`Int f)
          with Not_found ->
            try
              let f = List.find (
                  fun ord_float ->
                    match ord_float.o_feature_name_opt with
                      | Some name -> name = feature_name
                      | None -> false
                ) features.ord_float_a in
              Some (`Float f)
            with Not_found ->
              None


let num_included t =
  FeatureSet.cardinal t.partition.included

let num_excluded t =
  FeatureSet.cardinal t.partition.excluded

let bitvector_set_of_t t =
  let is_pointer_included =
    let n_included = FeatureSet.cardinal t.partition.included in
    let n_excluded = FeatureSet.cardinal t.partition.excluded in
    if n_included < n_excluded then
      fun feature ->
        FeatureSet.mem feature t.partition.included
    else
      fun feature ->
        not (FeatureSet.mem feature t.partition.excluded)
  in
  let s_entries = List.filter (
      fun entry ->
        is_pointer_included entry.feature
    ) t.entries in

  { s_bitvector_sequence = t.bitvector_sequence;
    s_num_observations = t.num_observations;
    s_entries;
  }

let iter f bv_set =
  List.iter (
    fun { feature; pointer; num_bytes } ->
    let bv = {
      offset = pointer;
      BV7.num_bytes;
      a = bv_set.s_bitvector_sequence;
      e_length = bv_set.s_num_observations
    } in
    f feature bv
  ) bv_set.s_entries

let fold_left f x0 bv_set =
  let x = ref x0 in
  iter (
    fun feature bv ->
      x := f !x feature bv;
  ) bv_set;
  !x

let elements bv_set =
  fold_left (fun list feature bv -> (feature, bv) :: list) [] bv_set

let count bv_set =
  fold_left (fun sum _ _ -> sum + 1) 0 bv_set

let uniform_split n t =
  assert false


