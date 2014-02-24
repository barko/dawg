(* read and write a Dog-formatted file.  A dog file contains three
   distinct blocks.  The first block contains a sequence of vectors.
   The second block encodes meta-data, and the third and final block is
   just 8 bytes, and encodes the offset into the file of the second
   block. *)

open Dog_t

module WO = struct

  type t = {
    ouch : out_channel;
    mutable dog : Dog_t.t;
    mutable num_bytes : int;
    mutable is_open : bool;
  }


  let create path num_observations =
    let ouch = open_out path in
    let features = {
      cat_a = [];
      ord_a = [];
    } in

    let dog = { features; num_observations } in
    { ouch; dog; num_bytes = 0; is_open = true }

  (* can translate [`RLE _] to [`Dense _], but [`Dense _] always remains
     [`Dense _] *)
  let rec write_vector t width_bytes = function
    | `Dense list ->
      let n_bytes = Dense.write_to_channel t.ouch ~width:width_bytes list in
      let vector_id = t.num_bytes in
      t.num_bytes <- n_bytes + t.num_bytes;
      `Dense vector_id

    | `RLE runs ->
      let a, num_observations, _ = Rlevec.write runs in
      let size_rle = UInt8Array.length a in
      let size_dense = num_observations * width_bytes in
      if size_dense < size_rle then
        (* dense representation is cheaper *)
        let num_observations_, list = Rle.decode runs in
        assert (num_observations_ = num_observations );
        write_vector t width_bytes (`Dense list)

      else (
        UInt8Array.char_iter_i (
          fun _ c ->
            output_char t.ouch c
        ) a;
        let vector_id = t.num_bytes in
        t.num_bytes <- size_rle + t.num_bytes;
        `RLE vector_id
      )

  let add_feature_o t feature =
    let features =
      match feature with
        | `Cat cat ->
          let width_bytes = Utils.num_bytes cat.c_cardinality in
          let c_vector = write_vector t width_bytes cat.c_vector in
          let cat = {
            c_vector;
            c_categories = cat.c_categories;
            c_feature_name_opt = cat.c_feature_name_opt;
            c_feature_id = cat.c_feature_id;
            c_cardinality = cat.c_cardinality;
            c_anonymous_category = cat.c_anonymous_category;
          } in
          let cat_a = cat :: t.dog.features.cat_a in
          { t.dog.features with cat_a }

        | `Ord ord ->
          let width_bytes = Utils.num_bytes ord.o_cardinality in
          let o_vector = write_vector t width_bytes ord.o_vector in
          let ord = {
            o_vector;
            o_cardinality = ord.o_cardinality;
            o_breakpoints = ord.o_breakpoints;
            o_feature_name_opt = ord.o_feature_name_opt;
            o_feature_id = ord.o_feature_id;
          } in
          let ord_a = ord :: t.dog.features.ord_a in
          { t.dog.features with ord_a }
    in
    t.dog <- { t.dog with features }

  let add_feature t (feature : Feat.lfeature) =
    if t.is_open then
      add_feature_o t feature
    else
      raise (Invalid_argument "add_feature: stream is closed for writing")

  let close_writer t =
    if t.is_open then
      (* note the position where we will serialize [Dog.t] *)
      let pos = t.num_bytes in

      (* serialize [Dog_t.t] *)
      let dog_s = Dog_b.string_of_t t.dog in
      output_string t.ouch dog_s;
      t.num_bytes <- t.num_bytes + (String.length dog_s);

      (* write the position of [Dog.t] as the last 64 bits of the
         file *)
      let pos_s = Bi_util.string8_of_int pos in
      output_string t.ouch pos_s;
      t.num_bytes <- t.num_bytes + (String.length pos_s);

      (* flush and close *)
      close_out t.ouch;
      t.is_open <- false

end

module RO = struct

  type t = {
    (* Feature metadata: what is the metadata, relating raw features to
       their decomposed binary ones? *)
    dog : Dog_t.t;

    (* what is the array encoding the sequence of vectors? *)
    array : UInt8Array.t;

    end_array_offset : int;
  }

  let dog { dog } =
    dog

  let array { array } =
    array

  let end_array_offset { end_array_offset } =
    end_array_offset

  let create path =
    let open Unix in

    (* open the file for reading *)
    let fd = openfile path [O_RDONLY] 0o640 in

    (* read the last 8 bytes of the file; it encodes an integer which
       represents the position of the dog metadata (type [Dog_t.t]). It
       also represents size of the preceding (first) block of the dog
       file, which encodes all the bitvectors. *)
    let end_array_offset =

      let pos = lseek fd (-8) SEEK_END in
      ignore pos;
      (* Printf.printf "pos=%d\n%!" pos; *)

      let s8 = String.make 8 '\000' in
      if read fd s8 0 8 <> 8 then
        failwith (Printf.sprintf "failed to read last 8 bytes of %s" path);

      (* create a Biniou input buffer from the 8-byte string, so we can
         read a bloody int from it *)
      let binb = Bi_inbuf.from_string s8 in
      let dog_t_pos = Bi_io.read_untagged_int64 binb in

      (* Printf.printf "dog pos=%Ld\n%!" dog_t_pos; *)
      Int64.to_int dog_t_pos
    in

    let dog =
      let _ = lseek fd end_array_offset SEEK_SET in
      let inch = in_channel_of_descr fd in
      let binb = Bi_inbuf.from_channel inch in
      Dog_b.read_t binb
    in

    let open Bigarray in
    let shared = false in
    let dim = end_array_offset in
    (* memory-map file into a char byte bigarray *)
    let array =
      Array1.map_file fd int8_unsigned c_layout shared dim in
    close fd;
    { dog; array; end_array_offset }

end

module RW = struct
  (* RW represents a feature set, like WO, but whose vectors are
     written incrementally.  RW is initialized with the metadata
     [Dog_t.t] for all the features it may contain, along with a
     fixed-size but empty file buffer.  Feature blobs are subsequently
     written to that buffer.  The file can eventually evolve to be a
     complete replica of the file produced by the [WO] module.  It is
     up to the user of RA to keep track of which features have been
     written to it.  RA does not check whether the user is reading
     features that have not yet been written to it, and doing so is
     unsafe and will lead to errors. *)

  let vector_id_of_vector = function
    | `Dense vector_id
    | `RLE vector_id -> vector_id

  (* a silly name for the pair of vector_id and its corresponding length *)

  type veq = {
    vector_id : vector_id;
    vector_length : int
  }

  module IntMap = Utils.XMap( Utils.Int )

  let anotatate_length vector_length = function
    | `Dense vector_id -> `Dense { vector_id; vector_length }
    | `RLE vector_id -> `RLE { vector_id; vector_length }

  let veq_of_feature = function
    | `Ord { o_vector } -> vector_id_of_vector o_vector
    | `Cat { c_vector } -> vector_id_of_vector c_vector

  (* create a map between a feature id to the vector (offset into
     array). [size] is the sum of all bytes used for encoding vectors,
     that is, excluding the space used for encoding [Dog_t.t]. *)
  let feature_id_to_vector_of_features { cat_a; ord_a } size =

    let feature_id_to_vector = List.fold_left (
        fun feature_id_to_vector { c_feature_id; c_vector } ->
          (c_feature_id, vector_id_of_vector c_vector) :: feature_id_to_vector
      ) [] cat_a in

    let feature_id_to_vector = List.fold_left (
        fun feature_id_to_vector { o_feature_id; o_vector } ->
          (o_feature_id, vector_id_of_vector o_vector) :: feature_id_to_vector
      ) feature_id_to_vector ord_a in

    (* sort by vector offset -- second element of pair; the bytes for
       each vector are sequential, without intervenining gap bytes.
       Therefore, the length of a vector with offset v is the distance
       in bytes to the subsequent vector.  If the subsequent vector has
       offset u, the length of the v-vector is u-v. *)

    let feature_id_to_vector = List.sort (
        fun (feature_id_1, vector_1) (feature_id_2, vector_2) ->
          Pervasives.compare vector_1 vector_2
      ) feature_id_to_vector in

    let feature_id_to_veq = Hashtbl.create 100 in

    let rec loop prev_feature_id prev_vector = function
      | (feature_id, vector) :: rest ->
        let prev_vector_length = vector - prev_vector in
        Hashtbl.replace feature_id_to_veq prev_feature_id prev_vector_length;
        loop feature_id vector rest
      | [] ->
        let prev_vector_length = size - prev_vector in
        Hashtbl.replace feature_id_to_veq prev_feature_id prev_vector_length;
    in

    (match feature_id_to_vector with
      | [] -> () (* empty feature set! *)
      | (feature_id, vector) :: rest ->
        loop feature_id vector rest
    );

    let map = List.fold_left (
        fun map cat ->
          let vector_length = Hashtbl.find feature_id_to_veq
              cat.c_feature_id in
          let c_vector = anotatate_length vector_length cat.c_vector in
          let feature = `Cat { cat with c_vector } in
          IntMap.add cat.c_feature_id feature map
      ) IntMap.empty cat_a in

    let map = List.fold_left (
        fun map ord ->
          let vector_length = Hashtbl.find feature_id_to_veq
              ord.o_feature_id in
          let o_vector = anotatate_length vector_length ord.o_vector in
          let feature = `Ord { ord with o_vector } in
          IntMap.add ord.o_feature_id feature map
      ) map ord_a in

    map

  type qfeature = (veq, veq) Dog_t.feature

  type t = {
    (* what is the array encoding the sequence of vectors? *)
    array : UInt8Array.t;

    (* map feature id to features *)
    feature_id_to_feature : qfeature IntMap.t;

    (* what are the number of observations in the feature set *)
    num_observations : int;
  }

  let create_r path =
    let open Unix in

    (* open the file for reading *)
    let fd = openfile path [O_RDONLY] 0o640 in

    let dog_t_offset =

      (* read the last 8 bytes of the file; it encodes an integer
         which represents the position of the dog metadata (type
         [Dog_t.t]). It also represents size of the preceding (first)
         block of the dog file, which encodes all the bitvectors. *)
      let pos = lseek fd (-8) SEEK_END in
      ignore pos;
      (* Printf.printf "pos=%d\n%!" pos; *)

      let s8 = String.make 8 '\000' in
      if read fd s8 0 8 <> 8 then
        failwith (Printf.sprintf "failed to read last 8 bytes of %s" path);

      (* create a Biniou input buffer from the 8-byte string, so we can
         read a bloody int from it *)
      let binb = Bi_inbuf.from_string s8 in
      let dog_t_pos = Bi_io.read_untagged_int64 binb in

      (* Printf.printf "dog pos=%Ld\n%!" dog_t_pos; *)
      Int64.to_int dog_t_pos
    in

    let dog_t =
      let _ = lseek fd dog_t_offset SEEK_SET in
      let inch = in_channel_of_descr fd in
      let binb = Bi_inbuf.from_channel inch in
      Dog_b.read_t binb
    in

    let open Bigarray in
    let shared = false in
    let dim = dog_t_offset in
    (* memory-map file into a char byte bigarray *)
    let array =
      Array1.map_file fd int8_unsigned c_layout shared dim in
    close fd;

    let num_observations = dog_t.Dog_t.num_observations in

    let feature_id_to_feature =
      feature_id_to_vector_of_features dog_t.features dog_t_offset in

    { array; num_observations; feature_id_to_feature }

  let create_w path size dog_t =
    assert (size > 0);
    let open Unix in
    (* open a file *)
    let fd = openfile path [O_CREAT; O_RDWR] 0o640 in

    (* seek to its dimension -- creating a sparse file in that its data
       is entirely unwritten yet *)
    let pos = lseek fd size SEEK_CUR in
    ignore pos;

    let open Bigarray in
    let shared = false in
    let array = Array1.map_file fd int8_unsigned c_layout shared size in
    close fd;

    let dog_t_blob = Dog_b.string_of_t dog_t in

    let dog_t_size = String.length dog_t_blob in
    let dog_t_offset = size - dog_t_size - 8 in
    assert (dog_t_offset >= 0);

    (* copy [dog_t_blob], so that its last byte is 8 bytes from the
       end of the (mmap'd) file *)
    for i = 0 to dog_t_size - 1 do
      array.{dog_t_offset + i} <- Char.code dog_t_blob.[i]
    done;

    let feature_id_to_feature =
      feature_id_to_vector_of_features dog_t.features dog_t_offset in

    let dog_t_offset_s = Bi_util.string8_of_int dog_t_offset in
    assert( String.length dog_t_offset_s = 8 );
    for i = 0 to 8-1 do
      array.{size - 8 + i} <- Char.code dog_t_offset_s.[i]
    done;

    let num_observations = dog_t.Dog_t.num_observations in

    { array; feature_id_to_feature; num_observations }

  let create path write_opt =
    match write_opt with
      | Some (size, dog_t) -> (* read-write mode *)
        create_w path size dog_t
      | None -> (* read-only *)
        create_r path

  type size_mismatch = {
    expected : int;
    actual : int
  }

  exception SizeMismatch of size_mismatch
  exception FeatureIdNotFound of feature_id

  let write ra feature_id encoded_vec =
    let { array } = ra in
    try
      let feature = IntMap.find feature_id ra.feature_id_to_feature in
      let { vector_id; vector_length } = veq_of_feature feature in
      let encoded_vector_length = String.length encoded_vec in
      if vector_length <> encoded_vector_length then
        let size_mismatch = {
          expected = vector_length;
          actual = encoded_vector_length
        } in
        raise (SizeMismatch size_mismatch)
      else
        for i = 0 to vector_length - 1 do
          array.{ vector_id + i } <- Char.code encoded_vec.[i]
        done
    with Not_found ->
      raise (FeatureIdNotFound feature_id)

  let read ra feature_id =
    try
      let feature = IntMap.find feature_id ra.feature_id_to_feature in
      let { vector_id; vector_length } = veq_of_feature feature in
      let buf = String.create vector_length in
      let array = ra.array in
      for i = 0 to vector_length - 1 do
        buf.[i] <- Char.chr array.{ vector_id + i }
      done;
      buf
    with Not_found ->
      raise (FeatureIdNotFound feature_id)

  let find ra feature_id =
    try
      IntMap.find feature_id ra.feature_id_to_feature
    with Not_found ->
      raise (FeatureIdNotFound feature_id)

  let q_to_a_vector t = function
    | `Dense { vector_id } ->
      `Dense {
        Vec.length = t.num_observations;
        array = t.array;
        offset = vector_id;
      }

    | `RLE { vector_id } ->
      `RLE {
        Vec.length = t.num_observations;
        array = t.array;
        offset = vector_id;
      }

  let q_to_a_feature t = function
    | `Cat {
        Dog_t.c_feature_id;
        c_feature_name_opt;
        c_anonymous_category;
        c_categories;
        c_cardinality;
        c_vector;
      } ->
      `Cat {
        Dog_t.c_feature_id;
        c_feature_name_opt;
        c_anonymous_category;
        c_categories;
        c_cardinality;
        c_vector = q_to_a_vector t c_vector;
      }

    | `Ord {
        Dog_t.o_feature_id;
        o_feature_name_opt;
        o_cardinality;
        o_breakpoints;
        o_vector;
      } ->
      `Ord {
        Dog_t.o_feature_id;
        o_feature_name_opt;
        o_cardinality;
        o_breakpoints;
        o_vector = q_to_a_vector t o_vector;
      }

  let a_find ra feature_id =
    try
      let q = IntMap.find feature_id ra.feature_id_to_feature in
      q_to_a_feature ra q
    with Not_found ->
      raise (FeatureIdNotFound feature_id)


end
