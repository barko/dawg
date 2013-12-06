type error_location = {
  e_line_number : int;
  e_start : int;
  e_end : int;
}

open Csv_types
val string_of_error_location : error_location -> string

type error = [
  | `SyntaxError of error_location
  | `UnterminatedString of int (* line number *)
  | `IntOverflow of (int * string) (* line number and offending string *)
]

type next_row = unit -> [ `Ok of Csv_types.row | error ]
val of_channel : in_channel ->  [ `Ok of string list * next_row | error ]
