(* read and write a Dog-formatted file.  A dog file contains three
   distinct blocks.  The first block contains a sequence of vectors.
   The second block encodes meta-data, and the third and final block is
   just 8 bytes, and encodes the offset into the file of the second
   block. *)

open Dog_t

type w = {
  ouch : out_channel;
  mutable dog : Dog_t.t;
  mutable num_bytes : int;
  mutable is_open : bool;
}


let create_writer path num_observations =
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
    let n_bytes = Vec.write_to_channel t.ouch ~width:width_bytes list in
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

    (* serialize [Dog.t] *)
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

type r = {
  (* Feature metadata: what is the metadata, relating raw features to
     their decomposed binary ones? *)
  r_dog : Dog_t.t;

  (* what is the array encoding the sequence of vectors? *)
  array : UInt8Array.t;

  (* what is the file descriptor corresponding to the memory-mapped
     bigarray ? *)
  fd: Unix.file_descr;

  end_array_offset : int;
}

let dog r =
  r.r_dog

let array r =
  r.array

let end_array_offset r =
  r.end_array_offset


let create_reader path =
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
  { r_dog = dog; array; fd; end_array_offset }

let close_reader { fd } =
  Unix.close fd


module RA = struct
  type t = { (* aka read and append *)
    (* what is the array encoding the sequence of vectors? *)
    array : UInt8Array.t;

    (* at which offset into [array] should the next vector be written? *)
    append_pos : int;

    (* what is the dimension of [array] in bytes? *)
    size : int

  }

  let create path size =
    assert (size > 0);
    let open Unix in
    (* open a file *)
    let fd = openfile path [O_CREAT; O_RDWR] 0o640 in

    (* seek to its dimension -- creating a sparse file in that its data
       is entirely unwritten yet *)
    let _ = lseek fd size SEEK_CUR in

    let open Bigarray in
    let shared = false in
    let array = Array1.map_file fd int8_unsigned c_layout shared size in
    close fd;
    { array; size; append_pos = 0 }

  exception TooFull

  let append ra encoded_vec =
    let { array; size; append_pos } = ra in
    let remaining_bytes = size - append_pos in
    let encoded_vec_len = String.length encoded_vec in
    if remaining_bytes < encoded_vec_len then
      raise TooFull
    else
      for i = 0 to encoded_vec_len - 1 do
        array.{ append_pos + i } <- Char.code encoded_vec.[i]
      done;
    { ra with append_pos = append_pos + encoded_vec_len }

  let array { array } =
    array

end
