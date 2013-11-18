open Printf

let yn_of_bool = function
  | true -> "Y"
  | false -> "N"

let random_cat () =
  match Random.int 3 with
    | 0 -> "A"
    | 1 -> "B"
    | 2 -> "C"
    | _ -> assert false



let logistic n error_rate with_cat =
  print_endline "z,x,y,p";

  for i = 0 to n-1 do
    let x = (Random.float 4.0) -. 2.0 in
    let y = (Random.float 4.0) -. 2.0 in
    let r2 = x *. x +. y *. y in
    let r = sqrt r2 in
    let in_circle = r <= 1.0 in
    let cat = random_cat () in
    let z =
      in_circle &&
      (match cat with
        | "A" | "C" -> true
        | "B" -> false
        | _ -> assert false
      )
    in
    let is_error = (Random.float 1.0) <= error_rate in
    let z_hat =
      if is_error then
        not z
      else
        z
    in
    printf "%s,%.5f,%.5f,%s\n"
      (yn_of_bool z_hat) x y cat
  done

let square n error with_cat =
  let m_half_error = -0.5 *. error in
  let header = "z,x,y" in
  let header =
    if with_cat then
      header ^ ",p"
    else
      header
  in
  print_endline header;
  for i = 0 to n-1 do
    let x = (Random.float 4.0) -. 2.0 in
    let y = (Random.float 4.0) -. 2.0 in
    let r2 = x *. x +. y *. y in
    let exp2 = exp (-.r2) in
    if with_cat then
      let cat = random_cat () in
      let z =
        match cat with
          | "A" | "C" -> exp2
          | "B" -> 3.0
          | _ -> assert false
      in
      let err = m_half_error +. (Random.float error) in
      let z_hat = z +. err in
      printf "%.5f,%.5f,%.5f,%s\n" z_hat x y cat
    else
      let z = exp2 in
      let err = m_half_error +. (Random.float error) in
      let z_hat = z +. err in
      printf "%.5f,%.5f,%.5f\n" z_hat x y
  done

let square_sparse n error with_cat =
  let m_half_error = -0.5 *. error in

  print_endline "z,x,y,p";
  for i = 0 to n-1 do
    let x = (Random.float 4.0) -. 2.0 in
    let y = (Random.float 4.0) -. 2.0 in
    let r2 = x *. x +. y *. y in
    let exp2 = exp (-.r2) in
    let cat = random_cat () in
    let z =
      match cat with
        | "A" | "C" -> exp2
        | "B" -> 3.0
        | _ -> assert false
    in
    let err = m_half_error +. (Random.float error) in
    let z_hat = z +. err in
    printf "{0 %.5f,1 %.5f,2 %.5f%s}\n" z_hat x y
      (match cat with
        | "B" -> "" (* implicit *)
        | "A" -> ",3 A"
        | "C" -> ",3 C"
        | _ -> assert false
      )
  done


let _ =
  let usage = sprintf "usage: %s (logistic|square|square-sparse) <n> <error>"
      Sys.argv.(0) in

  let n, error_rate =
    match Sys.argv with
      [| _; _; n_s; error_rate |] ->
        let n = int_of_string n_s in
        let error_rate = float_of_string error_rate in
        n, error_rate
      | _ ->
        print_endline usage;
        exit 1
  in
  let f =
    match Sys.argv.(1) with
      | "logistic" -> logistic
      | "square" -> square
      | "square-sparse" -> square_sparse
      | _ -> print_endline usage; exit 1
  in
  f n error_rate false
