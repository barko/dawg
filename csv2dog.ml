(** create a training set from a csv file *)
open Csv_io
open Printf
open Dog_t

module IntMap = Utils.XMap(Utils.Int)

let index =
  let rec loop i map = function
    | [] -> map
    | h :: t ->
      let map = IntMap.add i h map in
      loop (i+1) map t
  in
  fun list ->
    loop 0 IntMap.empty list

(* column index * row index * value *)
type cell = int * int * Csv_types.value

(* order by feature_id [j_*] in ascending order, then by observation
   in ascending order *)
let compare_cells (j_1, value_1, _) (j_2, value_2, _) =
  match Pervasives.compare j_1 j_2 with
    | -1 -> -1
    |  1 ->  1
    |  0 -> Pervasives.compare value_1 value_2
    | _ -> assert false

(* the type of a value is encoded in two bits *)
let tag_of_cell = function
  | _, _, `Int _    -> 0b01
  | _, _, `Float _  -> 0b10
  | _, _, `String _ -> 0b11

(* write the tags of (up to) four cells into the lower byte of an
   integer, with the tag of the first cell in bits 8 and 7 of that
   byte. *)
let tag4_of_cells offset cells =
  let b = ref 0 in
  let num_cells_1 = (Array.length cells) - 1 in
  let offset_3 = offset + 3 in
  for i = offset to min offset_3 num_cells_1 do
    let tag = tag_of_cell cells.(i) in
    b := (!b lsl 2) lor tag
  done;
  for i = num_cells_1+1 to offset_3 do
    b := !b lsl 2
  done;
  !b

let write_cells_to_file work_dir file_num cells =
  let path = Filename.concat work_dir (string_of_int file_num) in
  let ouch = open_out path in
  let bobuf = Bi_outbuf.create_channel_writer ouch in
  for i = 0 to (Array.length cells)-1 do
    (* every set of four consecutive cells, we write a byte that
       encodes the type of value in each of those cells *)
    if i mod 4 = 0 then (
      let tag4 = tag4_of_cells i cells in
      Bi_outbuf.add_char bobuf (Char.chr tag4)
    );

    match cells.(i) with
      | (j, i, `Int value) ->
        TS_b.write_int_cell bobuf (j, i, value)
      | (j, i, `Float value) ->
        TS_b.write_float_cell bobuf (j, i, value)
      | (j, i, `String value) ->
        TS_b.write_string_cell bobuf (j,  i, value)
  done;
  Bi_outbuf.flush_output_writer bobuf;
  close_out ouch

let cell_stream path =
  let inch = open_in path in
  let bibuf = Bi_inbuf.from_channel inch in
  let tag4 = ref 0 in

  let next i =
    let rem = i mod 4 in
    try
      if rem = 0 then (
        let c = Bi_inbuf.read_char bibuf in
        tag4 := Char.code c
      );
      let tag =
        match rem with
          | 3 ->  !tag4 land 0b00_00_00_11
          | 2 -> (!tag4 land 0b00_00_11_00) lsr 2
          | 1 -> (!tag4 land 0b00_11_00_00) lsr 4
          | 0 -> (!tag4 land 0b11_00_00_00) lsr 6
          | _ -> assert false
      in
      match tag with
        | 0b01 -> (* [`Int] *)
          let j, i, value =
            TS_b.read_int_cell bibuf in
          Some (j, i, `Int value)
        | 0b10 -> (* [`Float] *)
          let j, i, value =
            TS_b.read_float_cell bibuf in
          Some (j, i, `Float value)
        | 0b11 -> (* [`String] *)
          let j, i, value =
            TS_b.read_string_cell bibuf in
          Some (j, i, `String value)
        | 0b00 ->
          (* the trailing, unused bits of a tag4, indicating that we
             are at the end of the input buffer *)
          close_in inch;
          None
        | _ -> assert false

    with Bi_inbuf.End_of_input ->
      close_in inch;
      None
  in
  Stream.from next

let string_of_value = function
  | `Float f -> string_of_float f
  | `Int i -> string_of_int i
  | `String s -> s

type create = {
  input_path_opt : string option;
  output_path : string;
  max_density : float;
  max_cells_in_mem : int;
  no_header : bool;
  work_dir : string;
  max_width : int;
}


let dummy_cell = (-1, -1, `Float nan)

(* i := row index
   j := column index
   c := cell index
   f := file index
*)

let write_cells_to_work_dir work_dir header next_row max_cells =
  let num_features = List.length header in

  let cells = Array.create max_cells dummy_cell in

  let append_cell ~c ~f cell =
    if c < max_cells then (
      cells.( c ) <- cell;
      c + 1, f
    )
    else (
      Printf.printf "sortting ... %!";
      Array.sort compare_cells cells;
      Printf.printf "done\n%!";
      write_cells_to_file work_dir f cells;
      Array.fill cells 0 max_cells dummy_cell;
      cells.(0) <- cell;
      1, f + 1
    )
  in

  let rec loop ~i ~f ~c =
    if c mod 1000 = 0 then
      Printf.printf "files=%d rows=%d\n%!" f i;

    match next_row () with
      | `Ok `EOF ->
        if c > 0 then (
          (* write trailing cells, if there are any *)
          let trailing_cells = Array.create c dummy_cell in
          (* copy *)
          Array.blit cells 0 trailing_cells 0 c;
          Array.sort compare_cells trailing_cells;
          write_cells_to_file work_dir f trailing_cells;
          i, f+1
        )
        else
          i, f

      | `Ok (`Dense dense) ->
        let row_length, c, f = List.fold_left (
            fun (j, c, f) value ->
              let c, f =
                append_cell ~c ~f (j, i, value) in
              j + 1, c, f
          ) (0, c, f) dense in
        loop ~i:(i+1) ~f ~c

      | `Ok (`Sparse sparse) ->
        let c, f = List.fold_left (
            fun (c, f) (j, value) ->
              append_cell ~c ~f (j, i, value)
          ) (c, f) sparse in
        loop ~i:(i+1) ~f ~c

      | `SyntaxError err ->
        print_endline (Csv_io.string_of_error_location err);
        exit 1

      | `UnterminatedString line ->
        Printf.printf "unterminated string on line %d\n%!" line;
        exit 1

      | `IntOverflow (line, offending_string) ->
        Printf.printf "value %S on line %d cannot be represented as an integer\n%!"
          offending_string line;
        exit 1

  in
  Printf.printf "num features: %d\n%!" num_features;
  loop ~i:0 ~f:0 ~c:0

let csv_to_cells work_dir config =
  let ch =
    match config.input_path_opt with
      | None ->
        print_endline "reading csv data from standard input";
        stdin
      | Some path ->
        open_in path
  in
  match Csv_io.of_channel ch with
    | `Ok (header, next_row) ->
      let num_rows, num_cell_files =
        write_cells_to_work_dir work_dir header next_row
          config.max_cells_in_mem in
      close_in ch;
      header, num_rows, num_cell_files

    | _ ->
      print_endline "syntax error"; exit 1

module CellMerge = Stream_merge.Make(
    struct
      type t = cell
      let leq h1 h2 =
        compare_cells h1 h2 < 1
    end
  )

type kind_count = {
  n_float : int ;
  n_int : int;
  n_string : int
}

let kind_count_of_histogram hist =
  Hashtbl.fold (
    fun value count kc ->
      match value with
        | `Float _ -> { kc with n_float = kc.n_float + count }
        | `Int _ -> { kc with n_int = kc.n_int + count }
        | `String _ -> { kc with n_string = kc.n_string + count }

  ) hist {n_float=0; n_int=0; n_string=0}

module StringSet = Set.Make(
  struct
    type t = string
    let compare = Pervasives.compare
  end
  )

module StringOMap = Map.Make(
  struct
    type t = string option
    let compare = Pervasives.compare
  end
  )

let indexes_of_cat values cat_x =
  let _, indexes = List.fold_left (
      fun (i, indexes) (ii, value) ->
        assert (i = ii);
        match value with
          | `String cat ->
            let indexes =
              if cat = cat_x then
                i :: indexes
              else
                indexes
            in
            i+1, indexes
          | `Int _ | `Float _ -> assert false
    ) (0, []) values in
  indexes

let rec exclude_none accu = function
  | [] -> List.rev accu
  | (Some h) :: t ->  exclude_none (h :: accu) t
  | None :: t -> exclude_none accu t

let exclude_none list =
  exclude_none [] list

let categorical_feature j kc hist n i_values feature_id_to_name config =
  let n_anonymous = n - kc.n_string in
  assert (n_anonymous >= 0);
  let category_to_count, num_categories =
    Hashtbl.fold (
      fun value count (category_to_count, num_categories) ->
        assert (count > 0);
        let s_value =
          match value with
            | `String s -> s
            | `Float _
            | `Int _ -> assert false
        in
        let category_to_count = (s_value, count) :: category_to_count in
        category_to_count, num_categories + 1
    ) hist ([], 0) in

  if n_anonymous = 0 then
    if num_categories = 1 then
      `Uniform
    else (
      (* sort so that categories with lower counts first *)
      let category_to_count = List.sort (
          fun (_, count1) (_, count2) ->
            Pervasives.compare count1 count2
        ) category_to_count in

      (* categories with higher counts are first *)
      let categories = List.rev_map fst category_to_count in

      let cat_to_cat_id = Hashtbl.create num_categories in
      List.iteri (
        fun cat_id cat ->
          Hashtbl.replace cat_to_cat_id cat cat_id
      ) categories;

      let i_cats = List.rev_map (
          fun (i, value) ->
            match value with
              | `Int _ | `Float _ -> assert false
              | `String cat ->
                i, Hashtbl.find cat_to_cat_id cat
        ) i_values (* i_values are in i-reverse order *) in

      let cat_runs = Rle.encode_sparse n i_cats (-1) in
      let c_vector = `RLE cat_runs in

      let cat = `Cat {
          c_feature_id = j;
          c_feature_name_opt = feature_id_to_name j;
          c_categories = categories;
          c_cardinality = num_categories;
          c_anonymous_category = None;
          c_vector
        } in
      `NonUniform cat
    )
  else
    (* some categories are anonymous; assume that this signals
       sparsity *)
    let category_to_count = List.rev_map (
        fun (category, count) -> Some category, count
      ) category_to_count in

    (* add anonymous category count *)
    let category_to_count = (None, n_anonymous) :: category_to_count in

    (* sort so that categories with lower counts first *)
    let category_to_count = List.sort (
        fun (_, count1) (_, count2) ->
          Pervasives.compare count1 count2
      ) category_to_count in

    (* now categories are sorted in decreasing frequency *)
    let categories = List.rev_map fst category_to_count in

    let cat_to_cat_id = Hashtbl.create num_categories in
    List.iteri (
      fun cat_id cat ->
        Hashtbl.replace cat_to_cat_id cat cat_id
    ) categories;

    let anon_cat_id = Hashtbl.find cat_to_cat_id None in

    let i_cats = List.rev_map (
        fun (i, value) ->
          match value with
            | `Int _ | `Float _ -> assert false
            | `String cat ->
              let cat_id = Hashtbl.find cat_to_cat_id (Some cat) in
              i, cat_id
      ) i_values in (* [i_values] are in i-reversed order *)

    (* exclude the anonymous category from the list of features;
       otherwise preseving the order of the (Some _) payloads *)
    let categories = exclude_none categories in
    let c_cardinality = num_categories + 1 in
    let cat_runs = Rle.encode_sparse n i_cats anon_cat_id in

    let c_vector = `RLE cat_runs in

    let cat = `Cat {
        c_feature_id = j;
        c_feature_name_opt = feature_id_to_name j;
        c_categories = categories;
        c_cardinality;
        c_anonymous_category = Some anon_cat_id;
        c_vector
      } in
    `NonUniform cat

module DEBUG = struct
  let rec rand_hist max_total_count max_value_incr max_count accu total_count
      value =
    if total_count >= max_total_count then
      total_count, List.rev accu
    else
      let v_incr = Random.int max_value_incr in
      let value = value + 1 + v_incr in
      let count = 1 + Random.int max_count in
      let total_count = count + total_count in
      let accu = (value, count) :: accu in
      rand_hist max_total_count max_value_incr max_count accu total_count value

  let rand_hist ~max_total_count ~max_value_incr ~min_value ~max_count =
    rand_hist max_total_count max_value_incr max_count [] 0 min_value
end

let sort_fst_ascending list =
  List.sort (fun (v1,_) (v2,_) -> Pervasives.compare v1 v2) list

let sort_fst_descending list =
  List.sort (fun (v1,_) (v2,_) -> Pervasives.compare v2 v1) list


let incr hist ?(by=1) k =
  let count =
    try
      Hashtbl.find hist k
    with Not_found ->
      0
  in
  Hashtbl.replace hist k (count + by)


let downsample_hist =
  let rec loop n num_bins cum_count breakpoint accu hist =
    match hist, num_bins with
      | _ , 0
      | [], _ -> (
          (* Printf.printf "last: n=%d cum_count=%d num_bins=%d breakpoint=%f\n"
            n cum_count num_bins breakpoint; *)
          List.rev accu
        )
      | (value, count) :: rest, _ ->
        assert (count > 0);
        (* Printf.printf "n=%d value=%d count=%d cum_count=%d num_bins=%d breakpoint=%f\n"
           n value count cum_count num_bins breakpoint; *)
        let cum_count = count + cum_count in
        if (float cum_count) >= breakpoint then
          (* the bin size varies, depending on how many bins we've
             already consumed *)
          let num_bins = num_bins - 1 in
          let bin_size = (float (n - cum_count)) /. (float num_bins) in
          let breakpoint = breakpoint +. bin_size in
          let accu = value :: accu in
          loop n num_bins cum_count breakpoint accu rest
        else
          loop n num_bins cum_count breakpoint accu rest
  in
  fun n num_bins hist ->
    loop n num_bins 0 0. [] hist


(* cast int's to float's; strings are errors *)
let float_of_value = function
  | `Int v -> float v
  | `Float v -> v
  | `String _ -> assert false

(* strings and floats are errors *)
let int_of_value = function
  | `Int v -> v
  | `Float _
  | `String _ -> assert false

let ordinal_feature
    zero
    of_value
    to_breakpoints
    j
    kc
    hist
    n
    i_values
    feature_id_to_name
    config =
  let n_anonymous = n - kc.n_int - kc.n_float in
  assert (n_anonymous >= 0);
  (* augment the histogram with zero's, assuming this is the anonymous
     value *)
  if n_anonymous > 0 then
    (* we don't want any values with value=0, count=0 in [hist_list] *)
    incr hist ~by:n_anonymous zero;

  (* if this is a float feature, merge the number of (`Int 0) and
     (`Float 0.0) *)
  if kc.n_float > 0 then (
    (* this is a float feature *)
    try
      let int_zero_count = Hashtbl.find hist (`Int 0) in
      Hashtbl.remove hist (`Int 0);
      incr hist ~by:int_zero_count zero
    with Not_found ->
      ()
  );

  (* unbox, and convert to list, so we can sort *)
  let hist_list, num_distinct_values = Hashtbl.fold (
      fun value count (accu, num_distinct_values) ->
        let v = of_value value in
        ((v, count) :: accu, num_distinct_values + 1)
    ) hist ([], 0) in

  if num_distinct_values = 1 then
    `Uniform
  else
    let uncapped_width = Utils.width num_distinct_values in
    let breakpoints =
      if uncapped_width <= config.max_width then
        List.map fst (sort_fst_ascending hist_list)
      else
        (* cap the width through down-sampling *)
        let num_bins = 1 lsl config.max_width in
        let hist = sort_fst_ascending hist_list in
        downsample_hist n num_bins hist
    in
    let breakpoints_a = Array.of_list breakpoints in
    let o_cardinality = Array.length breakpoints_a in
    let find = Binary_search.find_x breakpoints_a in
    let i_rank = List.rev_map (
        fun (i, value) ->
          let rank = find (of_value value) in
          i, rank
      ) i_values in (* [i_values] are in i-reversed order *)
    let zero_rank = find (of_value zero) in
    let rank_runs = Rle.encode_sparse n i_rank zero_rank in
    let o_vector = `RLE rank_runs in
    let ord = `Ord {
        o_feature_id = j;
        o_feature_name_opt = feature_id_to_name j;
        o_cardinality;
        o_vector;
        o_breakpoints = to_breakpoints breakpoints;
      } in
    `NonUniform ord

let int_feature j kc hist n i_values =
  ordinal_feature (`Int 0) int_of_value (fun b -> `Int b)
    j kc hist n i_values

let float_feature j kc hist n i_values =
  ordinal_feature (`Float 0.0) float_of_value (fun b -> `Float b)
    j kc hist n i_values

exception MixedTypeFeature of int (* feature id *)

let write_feature j i_values n dog feature_id_to_name config =
  let hist = Hashtbl.create (n / 100) in
  let kc = List.fold_left (
      fun kc (i, value) ->
        incr hist value;
        match value with
          | `Float _  -> { kc with n_float  = kc.n_float  + 1 }
          | `Int _    -> { kc with n_int    = kc.n_int    + 1 }
          | `String _ -> { kc with n_string = kc.n_string + 1 }

    ) {n_float=0; n_int=0; n_string=0} i_values in
  if kc.n_string > 0 then
    if kc.n_float = 0 && kc.n_int = 0 then
      let cf = categorical_feature j kc hist n i_values
          feature_id_to_name config in
      match cf with
        | `Uniform ->
          Printf.printf "%d: cat uniform\n%!" j

        | `NonUniform cat ->
          Printf.printf "%d: cat\n%!" j;
          Dog_io.add_feature dog cat
    else
      raise (MixedTypeFeature j)
  else if kc.n_float > 0 then
    let float_feat = float_feature j kc hist n i_values
        feature_id_to_name config in
    match float_feat with
      | `Uniform ->
        Printf.printf "%d: float uniform\n%!" j

      | `NonUniform ord ->
        Printf.printf "%d: float\n%!" j;
        Dog_io.add_feature dog ord

  else if kc.n_int > 0 then
    let int_feat = int_feature j kc hist n i_values
        feature_id_to_name config in
    match int_feat with
      | `Uniform ->
        Printf.printf "%d: int uniform\n%!" j

      | `NonUniform ord ->
        Printf.printf "%d: int\n%!" j;
        Dog_io.add_feature dog ord

  else (
    Printf.printf "%d: implicit uniform\n%!" j
  )


let pr_hist j i_values =
  let hist = Hashtbl.create 10_000 in (* TODO *)
  let kc = List.fold_left (
    fun kc (i, value) ->
      incr hist value;
      match value with
        | `Float _ -> { kc with n_float = kc.n_float + 1 }
        | `Int _ -> { kc with n_int = kc.n_int + 1 }
        | `String _ -> { kc with n_string = kc.n_string + 1 }

  ) {n_float=0; n_int=0; n_string=0} i_values in
  printf "%d: f=%d i=%d s=%d\n" j kc.n_float kc.n_int kc.n_string

let read_cells_write_features work_dir ~num_cell_files ~num_rows header config =
  let feature_id_to_name =
    let idx = index header in
    fun feature_id ->
      IntMap.find_opt feature_id idx
  in

  let rec loop prev_j i_values dog =
    parser
  | [< '(j, i, value); tail >] ->
    if j = prev_j then (
      let i_values = (i, value) :: i_values in
      loop j i_values dog tail
    )
    else (
      write_feature prev_j i_values num_rows dog feature_id_to_name config;
      loop j [i, value] dog tail
    )

  | [< >] ->
    write_feature prev_j i_values num_rows dog feature_id_to_name
      config;
    Dog_io.close_writer dog
  in

  let dog = Dog_io.create_writer config.output_path num_rows in

  (* merge all the files to which cells were written in feature id,
     then (reverse) row id order *)
  let cell_streams = Utils.fold_range (
      fun file_num accu ->
        let cell_path = Filename.concat work_dir (string_of_int file_num) in
        (cell_stream cell_path) :: accu
    ) ~start:0 ~finix:num_cell_files [] in
  let merged_stream = CellMerge.create cell_streams in
  let j, i, value = Stream.next merged_stream in
  let i_values = [i, value] in
  loop j i_values dog merged_stream;

  (* remove all the cell files *)
  Utils.iter_range (
    fun file_num ->
      let cell_path = Filename.concat work_dir (string_of_int file_num) in
      Unix.unlink cell_path
  ) 0 num_cell_files

let mkdir_else_exit path =
  try
    Unix.mkdir path 0o750;
  with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    | exn ->
      print_endline (Printexc.to_string exn);
      exit 1

let create_work_dir () =
  let home = Unix.getenv "HOME" in
  let dot_dawg = Filename.concat home ".dawg" in
  mkdir_else_exit dot_dawg;
  let pid_s = string_of_int (Unix.getpid ()) in
  let work_dir = Filename.concat dot_dawg pid_s in
  mkdir_else_exit work_dir;
  work_dir

let pr = Printf.printf
let sp = Printf.sprintf

let create config =
  let work_dir = create_work_dir () in
  let header, num_rows, num_cell_files = csv_to_cells work_dir config in
  pr "num rows: %d, num_cell_files: %d\n%!" num_rows num_cell_files;
  let exit_status =
    if num_rows > 0 then (
      try
        read_cells_write_features work_dir ~num_rows ~num_cell_files header
          config;
        0
      with (MixedTypeFeature feature_id) ->
        pr "feature %d (column %d) has feature values that are both numeric \
          and categorical\n%!" feature_id (feature_id + 1);
        1
    )
    else (
      pr "input %scontains no data\n%!" (
        match config.input_path_opt with
          | Some path -> sp "%S " path
          | None -> ""
      );
      1
    )
  in
  (* remove the working directory *)
  Unix.rmdir work_dir;
  exit_status


module Defaults = struct
  let max_width = 8
  let max_density = 0.1
  let max_cells_in_mem = 10_000_000
  let work_dir =
    let home = Unix.getenv "HOME" in
    let dot_dawg = Filename.concat home ".dawg" in
    dot_dawg
end

let create output_path input_path_opt max_density no_header max_cells_in_mem
    max_width =
  if max_density > 1.0 || max_density < 0.0 then (
    printf "max-density must be between 0 and 1 (inclusive)";
    exit 1
  );

  if max_cells_in_mem < 1 then (
    printf "max-cells-in-mem must be positive";
    exit 1
  );

  if max_width < 1 || max_width > 63 then (
    printf "max-width must be between 1 and 63 (inclusive)";
    exit 1
  );

  let config = {
    input_path_opt;
    output_path;
    max_density;
    no_header;
    max_cells_in_mem;
    work_dir = Defaults.work_dir;
    max_width;
  } in
  let exit_status = create config in

  (* hmm unnecessary verbage to make compiler happy *)
  let () = exit exit_status in
  ()

open Cmdliner

let commands =
  let create_cmd =
    let doc = "create dog files from csv inputs" in

    let output_path =
      let doc = "path of the output dog file" in
      Arg.(required & opt (some string) None &
           info ["o";"output"] ~docv:"PATH" ~doc)
    in

    let input_path =
      let doc = "path of input csv file; if absent, stdin is read instead" in
      Arg.(value & opt (some string) None &
           info ["i";"input"] ~docv:"PATH" ~doc)
    in

    let max_density =
      let doc = "bitvectors should be considered dense when the minority bit \
                 represents less than this fraction of the length of the vector; \
                 this does not affect the output file, but may affect the speed \
                 at which it is generated" in
      Arg.(required & opt (some float) (Some Defaults.max_density) &
           info ["d";"bitvector-density"] ~docv:"FLOAT" ~doc)
    in

    let no_header =
      let doc = "interpret the first line of the csv file as data, rather
                 than a header providing names for the fields in file" in
      Arg.(value & flag & info ["h";"no-header"] ~doc)
    in

    let max_cells_in_mem =
      let doc = "the maximum number of csv data cells that will be held in memory; \
                 a smaller value will lead to more intermediate files, and overall \
                 slower processing" in
      Arg.(required & opt (some int) (Some Defaults.max_cells_in_mem) &
           info ["m";"max-cells-in-mem"] ~docv:"INT" ~doc)
    in

    let max_width =
      let doc = "the maximum number of bitvectors that can represent a feature; \
                 if 2^max_width is smaller than the number of distinct values of \
                 a feature, the feature will be down-sampled" in
      Arg.(required & opt (some int) (Some Defaults.max_width) &
           info ["w";"max-width"] ~docv:"INT" ~doc)
    in

    Term.(
      pure create $
        output_path $
        input_path $
        max_density $
        no_header $
        max_cells_in_mem $
        max_width
    ),
    Term.info "csv" ~doc
  in

  [create_cmd]
