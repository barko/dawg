type error_location = {
  e_line_number : int;
  e_start : int;
  e_end : int;
}

open Csv_types
val string_of_error_location : error_location -> string

type next_row = unit ->
  [ `Ok of Csv_types.row
  | `SyntaxError of error_location
  | `UnterminatedString of int (* line number *) ]

val of_channel :
  in_channel ->
  [> `Ok of string list * next_row
  | `SyntaxError of error_location
  | `UnterminatedString of int (* line number *) ]
