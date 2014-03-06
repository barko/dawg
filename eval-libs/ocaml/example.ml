(* example usage of Eval module *)

open Eval
let pr = Printf.printf

let string_of_key = function
  | `Id id -> string_of_int id
  | `Name name -> Printf.sprintf "%S" name

let _ =
  let path = Sys.argv.(1) in
  let e = create path in
  let logistic =
    match e with
      | `Square _ -> failwith "expecting a logistic model"
      | `Logistic logistic -> logistic
  in
  let logistic =
    if (positive_category logistic) = "N" then
      (* we want "Y" to be associated with probability = 1 *)
      invert logistic
    else
      logistic
  in
  let e = `Logistic logistic in

  (* populate a feature vector with values *)
  let feature_vector = [
    `Name "x", `Float 0.1;  (* ordinal feature *)
    `Id 2    , `Float 0.3;  (* ordinal feature *)
    `Name "c", `String "A"  (* categorical feature *)
  ] in

  try
    let p = eval e feature_vector in
    pr "probability = %f\n" p
  with
    | TypeMismatch (key, value) -> (
        match value with
          | `String _ ->
            pr "expecting ordinal value for feature %s\n"
              (string_of_key key)

          | `Float _ ->
            pr "expecting categorical value for feature %s\n"
              (string_of_key key)

      )

    | CategoryNotFound (key, category) -> (
        pr "category %S is not part of categorical feature %s\n" category
          (string_of_key key)
      )

    | CategoryMissing key ->
      pr "categorical feature %s lacks a value\n" (string_of_key key)
