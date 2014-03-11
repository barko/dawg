open Printf

(* apply [f] to each element of the input list, calling [sep]
   inbetween such elements *)
let rec iter_sep f sep = function
  | a :: b :: rest ->
      f a;
      sep ();
      iter_sep f sep (b :: rest)

  | [ a ] ->
      f a

  | [] ->
      ()

let rec iteri f i has_prev = function
  | a :: b :: rest ->
      let has_prev = f i has_prev a in
      iteri f (i+1) has_prev (b :: rest)

  | [ a ] ->
      ignore (f i has_prev a)

  | [] ->
      ()

let iteri f list =
  iteri f 0 false list


let string_of_value = function
  | `Int i -> string_of_int i
  | `Float f -> string_of_float f
  | `String s -> "\"" ^ s ^ "\""

let pr_strings ?(sep=",") out strings =
  out (String.concat sep strings);
  out "\n"

let pr_dense_row out row =
  iter_sep (fun v -> print_string (string_of_value v))
    (fun () -> out ",") row;
  out "\n"

let pr_sparse_row out pairs =
  out "{";
  iter_sep (
    fun (feature_id, v) ->
      out (string_of_int feature_id);
      out " ";
      out (string_of_value v)
  ) (fun () -> out ",") pairs;
  out "}\n"


let pr_dense_subset_row out row column_included =
  iteri (
    fun index has_prev value ->
      if column_included.(index) then (
        if has_prev then
          out ",";

        out (string_of_value value);
        true
      )
      else
        has_prev
  ) row;
  out "\n"


let pr_sparse_subset_row out row column_included =
  out "{";

  (* TODO: optinally print out default values, if they are included *)
  iteri (
    fun _ has_prev (index, value) ->
      if column_included.(index) then (
        if has_prev then
          out ",";

        out (string_of_int index);
        out " ";
        out (string_of_value value);
        true
      )
      else
        has_prev

  ) row;

  out "}\n"


type incl_excl_spec = {
  start_with : [ `All | `None ] ;
  commands : ([`Include | `Exclude ] * Pcre.regexp) list
}

type column = int * string (* a column's name with its 0-based index *)

module ColumnSet = Set.Make(
  struct
    type t = column
    let compare = Pervasives.compare
  end
  )

let rec apply_commands incl_excl_sets = function
  | (`Include, rex) :: rest ->
    (* move elements from [excluded_set] to [included_set] *)
    let _, excluded_set = incl_excl_sets in

    let incl_excl_sets = ColumnSet.fold (
      fun element incl_excl_sets ->
        let included_set, excluded_set = incl_excl_sets in
        let _, column_name = element in
        if Pcre.pmatch ~rex column_name then
          ColumnSet.add element included_set,
          ColumnSet.remove element excluded_set
        else
          incl_excl_sets
    ) excluded_set incl_excl_sets in
    apply_commands incl_excl_sets rest

  | (`Exclude, rex) :: rest ->
    (* move elements from [included_set] to [excluded_set] *)
    let included_set, _ = incl_excl_sets in

    let incl_excl_sets = ColumnSet.fold (
      fun element incl_excl_sets ->
        let included_set, excluded_set = incl_excl_sets in
        let _, column_name = element in
        if Pcre.pmatch ~rex column_name then
          ColumnSet.remove element included_set,
          ColumnSet.add element excluded_set
        else
          incl_excl_sets
    ) included_set incl_excl_sets in
    apply_commands incl_excl_sets rest

  | [] -> incl_excl_sets


let rec parse_commands accu = function
  | "i" :: regexp :: rest ->
    let accu = (`Include, Pcre.regexp regexp) :: accu in
    parse_commands accu rest

  | "x" :: regexp :: rest ->
    let accu = (`Exclude, Pcre.regexp regexp) :: accu in
    parse_commands accu rest

  | [] ->
    List.rev accu

  | other :: _ ->
    Printf.printf "unknown command %S; must be either \"i\" (for include) \
      or \"x\" (for exclude)\n%!" other;
    exit 1


let parse_incl_excl_spec = function
  | "all" :: rest ->
    let commands = parse_commands [] rest in
    { start_with = `All; commands }

  | "none" :: rest ->
    let commands = parse_commands [] rest in
    { start_with = `None; commands }

  | (("i" | "x") :: _ as commands)  ->
    (* assume [`All] when ["i"] and ["x"] appear without a starting
       set specifier *)
    let commands = parse_commands [] commands in
    { start_with = `All; commands }

  | _ ->
    print_endline "column subset specifier must begin with \"all\" \
      (to start with all columns) or \"none\" to start with no columns.";
    exit 1

let included_columns_of_spec header incl_excl_spec_as_list =
  let incl_excl_spec = parse_incl_excl_spec incl_excl_spec_as_list in
  let num_columns, column_set = List.fold_left (
      fun (column_index, column_set) column_name ->
        column_index + 1, ColumnSet.add (column_index, column_name) column_set
    ) (0, ColumnSet.empty) header in
  let incl_excl_set =
    match incl_excl_spec.start_with with
      | `All -> column_set, ColumnSet.empty
      | `None -> ColumnSet.empty, column_set
  in
  let included_set, _excluded_set =
    apply_commands incl_excl_set incl_excl_spec.commands in
  let is_included = Array.create num_columns false in
  ColumnSet.iter (
    fun (index, _) ->
      is_included.(index) <- true
  ) included_set;
  is_included

let rec loop_subset_of_columns is_included next_row out =
  match next_row () with
    | `SyntaxError err ->
      print_endline (Csv_io.string_of_error_location err);
      exit 1

    | `UnterminatedString line ->
      Printf.printf "unterminated quote on line %d\n%!" line;
      exit 1

    | `IntOverflow (line, offending_string) ->
      Printf.printf "value %S on line %d cannot be represented as an integer\n%!"
        offending_string line;
      exit 1

    | `Ok `EOF -> ()

    | `Ok `Dense dense ->
      pr_dense_subset_row out dense is_included;
      loop_subset_of_columns is_included next_row out

    | `Ok `Sparse sparse ->
      pr_sparse_subset_row out sparse is_included;
      loop_subset_of_columns is_included next_row out


let rec loop_all_columns next_row out =
  match next_row () with
    | `SyntaxError err ->
      print_endline (Csv_io.string_of_error_location err);
      exit 1

    | `UnterminatedString line ->
      Printf.printf "unterminated quote on line %d\n%!" line;
      exit 1

    | `IntOverflow (line, offending_string) ->
      Printf.printf "value %S on line %d cannot be represented as an integer\n%!"
        offending_string line;
      exit 1

    | `Ok `EOF -> ()

    | `Ok `Dense dense ->
      pr_dense_row out dense;
      loop_all_columns next_row out

    | `Ok `Sparse sparse ->
      pr_sparse_row out sparse;
      loop_all_columns next_row out


let main input_path output_path incl_excl_spec_as_list header_only =
  let inch =
    match input_path with
      | None -> stdin
      | Some path -> open_in path
  in

  let ouch =
    match output_path with
      | None -> stdout
      | Some path -> open_out path
  in

  let out = output_string ouch in

  match Csv_io.of_channel inch with
    | `SyntaxError err ->
      print_endline (Csv_io.string_of_error_location err);
      exit 1

    | `UnterminatedString line ->
      Printf.printf "unterminated quote on line %d\n%!" line;
      exit 1

    | `IntOverflow (line, offending_string) ->
      Printf.printf "value %S on line %d cannot be represented as an integer\n%!"
        offending_string line;
      exit 1

    | `Ok (header, next_row) ->

      match incl_excl_spec_as_list with
        | [] ->
          (* no spec: echo every column *)
          if header_only then
            pr_strings ~sep:"\n" out header
          else (
            pr_strings out header;
            loop_all_columns next_row out
          )

        | _ ->
          (* have spec: analyze it, and render only the desired
             columns *)
          let is_column_included =
            included_columns_of_spec header incl_excl_spec_as_list in

          let header_sep =
            if header_only then
              "\n"
            else
              ","
          in

          (* print subset of header *)
          iteri (
            fun index has_prev column_name ->
              if is_column_included.(index) then (
                if has_prev then
                  out header_sep;
                out column_name;
                true
              )
              else
                has_prev
          ) header;
          out "\n";

          if not header_only then
            loop_subset_of_columns is_column_included next_row out


open Cmdliner

let _ =
  let command =
    let doc = "echo a csv file, or a subset of its columns.  To select \
               a subset, provide an inclusion/exclusion specifier.  A \
               specifier starts with 'all' or 'none', and proceeds \
               with pairs: either 'i <regexp>' to include columns matching \
               a regexp, or 'x <regexp>' to exclude columns matching a \
               regexp." in

    let input_file_path =
      let doc = "path of the input csv file (if absent: stdin)" in
      Arg.(value & opt (some string) None &
           info ["i"; "input"] ~docv:"PATH" ~doc)
    in

    let output_file_path =
      let doc = "path of output csv file (if absent: stdout)" in
      Arg.(value & opt (some string) None &
           info ["o"; "output"] ~docv:"PATH" ~doc)
    in

    let header_only =
      let doc = "only echo the elements of the header, one per line" in
      Arg.(value & flag & info ["h";"header-only"] ~doc)
    in

    let incl_excl_cmds = Arg.(value & pos_all string [] & info []) in

    Term.(pure main
          $ input_file_path
          $ output_file_path
          $ incl_excl_cmds
          $ header_only
         ), Term.info "csvcat" ~doc
  in
  match Term.eval ~catch:false command with
    | `Error _ -> exit 1
    | _ -> exit 0
