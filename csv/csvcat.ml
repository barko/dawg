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

let string_of_value = function
  | `Int i -> string_of_int i
  | `Float f -> string_of_float f
  | `String s -> "\"" ^ s ^ "\""

let pr_strings ch strings =
  output_string ch (String.concat "," strings);
  output_string ch "\n"

let pr_dense_row ch row =
  iter_sep (fun v -> print_string (string_of_value v))
    (fun () -> fprintf ch ",") row;
  fprintf ch "\n"

let pr_sparse_row ch pairs =
  fprintf ch "{";
  iter_sep
    (fun (feature_id, v) -> fprintf ch "%d %s" feature_id (string_of_value v))
    (fun () -> fprintf ch ",") pairs;
  fprintf ch "}\n"

let _ =
  let usage = Printf.sprintf "usage: %s [OPTIONS]" Sys.argv.(0) in
  let input_path = ref None in
  let output_path = ref None in

  Arg.parse [
    "-i", Arg.String (fun s -> input_path := Some s),
    "input file (default: stdin)";

    "-o", Arg.String (fun s -> output_path := Some s),
    "output file (default: stdout)";
  ] (fun _ -> ()) usage;

  let inch =
    match !input_path with
      | None -> stdin
      | Some path ->
        (* Csv_lexer.tokenize_file path; *)
        open_in path
  in

  let ouch =
    match !output_path with
      | None -> stdout
      | Some path -> open_out path
  in

  match Csv_io.of_channel inch with
    | `SyntaxError err ->
      print_endline (Csv_io.string_of_error_location err);
      exit 1

    | `UnterminatedString line ->
      Printf.printf "unterminated quote on line %d\n%!" line;
      exit 1

    | `Ok (header, next_row) ->
      let rec loop () =
        match next_row () with
          | `SyntaxError err ->
            print_endline (Csv_io.string_of_error_location err);
            exit 1

          | `UnterminatedString line ->
            Printf.printf "unterminated quote on line %d\n%!" line;
            exit 1

          | `Ok `EOF -> ()

          | `Ok `Dense dense -> pr_dense_row ouch dense; loop ()
          | `Ok `Sparse sparse -> pr_sparse_row ouch sparse; loop ()
      in
      pr_strings ouch header;
      loop ()
