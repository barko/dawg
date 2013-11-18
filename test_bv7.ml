open BV7

let test_encoding () =
  let length = Random.int 10 in
  let density_true = Random.float 1.0 in
  let b = Utils.rand_bools density_true length in
  let d = decoded_of_bools b in
  let b' = bools_of_decoded d in

  if b <> b' then (
    print_endline "fail bool/decoded";
    print_endline (Utils.string_of_bools b );
    print_endline (Utils.string_of_bools b');

    print_endline (string_of_d d);
    exit 1
  );

  let e = encode d in
  let d' = decode e in

  if d <> d' then (
    print_endline "fail encoded/decoded";
    print_endline (Utils.string_of_bools b );
    print_endline (Utils.string_of_bools b');

    print_endline (string_of_d d );
    print_endline (string_of_d d');
    exit 1;
  )

let find_indexes =
  let rec loop f i accu = function
    | h :: t ->
      let accu =
        if f h then
          i :: accu
        else
          accu
      in
      loop f (i+1) accu t
    | [] -> List.rev accu
  in
  fun f indexes ->
    loop f 0 [] indexes

let pr = Printf.printf
let string_of_bools = Utils.string_of_bools

let test_indexes () =
  let length = Random.int 10_000 in
  let density_true = Random.float 1.0 in
  let b = Utils.rand_bools density_true length in

  let test d kind =
    let b' = BV7.bools_of_decoded d in
    if b' <> b then (
      pr "b =%s\nb'=%s\n" (string_of_bools b)
      (string_of_bools b');

    print_endline ("fail indexes " ^ kind);
    exit 1
    )
  in

  let b_true  = find_indexes (fun bit -> bit) b in
  let d_true = BV7.decoded_of_indexes true b_true length in
  test d_true "true";

  let b_false = find_indexes (fun bit -> not bit) b in
  let d_false = BV7.decoded_of_indexes false b_false length in
  test d_false "false"


let bools_binary_op op b1 b2 =
  let f =
    match op with
      | `AND -> ( && )
      | `OR  -> ( || )
      | `XOR -> Utils.f_xor
      | `AND_NOT -> Utils.f_and_not
  in
  List.rev (List.rev_map2 f b1 b2)

let time = Utils.time

let test_binary_op density_true n =
  let b1 = Utils.rand_bools density_true n in
  let b2 = Utils.rand_bools density_true n in

  let v1 = BV7.decoded_of_bools b1 in
  let v2 = BV7.decoded_of_bools b2 in

  let b_and = bools_binary_op `AND b1 b2 in
  let v_and, t_and = time (fun () -> BV7.binary BV7.AND v1 v2) in
  let b_and' = BV7.bools_of_decoded v_and in
  if b_and <> b_and' then (
    pr "b1=%s\nb2=%s\n" (string_of_bools b1)
      (string_of_bools b2);
    pr "v1=%s\nv2=%s\n" (BV7.string_of_d v1)
      (BV7.string_of_d v2);
    pr "b_and =%s\n" (string_of_bools b_and);
    pr "b_and'=%s\n" (string_of_bools b_and');
    pr "v_and=%s\n" (BV7.string_of_d v_and);

    print_endline "fail and";
    exit 1
  );

  let b_or  = bools_binary_op `OR  b1 b2 in
  let v_or, t_or  = time (fun () -> BV7.binary BV7.OR v1 v2) in
  let b_or' = BV7.bools_of_decoded v_or in
  if b_or <> b_or' then (
    pr "b1=%s\nb2=%s\n" (string_of_bools b1)
      (string_of_bools b2);
    pr "v1=%s\nv2=%s\n" (BV7.string_of_d v1)
      (BV7.string_of_d v2);

    pr "b_or =%s\n" (string_of_bools b_or);
    pr "b_or'=%s\n" (string_of_bools b_or');
    pr "v_or=%s\n" (BV7.string_of_d v_or);

    print_endline "fail or";
    exit 1
  );

  let b_xor = bools_binary_op `XOR b1 b2 in
  let v_xor, t_xor = time (fun () -> BV7.binary BV7.XOR v1 v2) in
  let b_xor' = BV7.bools_of_decoded v_xor in
  if b_xor <> b_xor' then (
    pr "b1=%s\nb2=%s\n" (string_of_bools b1)
      (string_of_bools b2);
    pr "v1=%s\nv2=%s\n" (BV7.string_of_d v1)
      (BV7.string_of_d v2);

    pr "b_xor =%s\n" (string_of_bools b_xor);
    pr "b_xor'=%s\n" (string_of_bools b_xor');
    pr "v_xor=%s\n" (BV7.string_of_d v_xor);

    print_endline "fail xor";
    exit 1
  );

  let b_and_not = bools_binary_op `AND_NOT b1 b2 in
  let v_and_not, t_and_not = time (fun () -> BV7.binary BV7.AND_NOT v1 v2) in
  let b_and_not' = BV7.bools_of_decoded v_and_not in
  if b_and_not <> b_and_not' then (
    pr "b1=%s\nb2=%s\n" (string_of_bools b1)
      (string_of_bools b2);
    pr "v1=%s\nv2=%s\n" (BV7.string_of_d v1)
      (BV7.string_of_d v2);

    pr "b_and_not =%s\n" (string_of_bools b_and_not);
    pr "b_and_not'=%s\n" (string_of_bools b_and_not');
    pr "v_and_not=%s\n" (BV7.string_of_d v_and_not);

    print_endline "fail and_not";
    exit 1
  );

  Printf.printf "% 8d %0.5f %f %f %f %f\n%!" n density_true
    t_and t_or t_xor t_and_not

let test_binary_op_de density_true n =
  let b1 = Utils.rand_bools density_true n in
  let b2 = Utils.rand_bools density_true n in

  let d1 = BV7.decoded_of_bools b1 in
  let d2 = BV7.decoded_of_bools b2 in
  let e2 = BV7.encode d2 in

  let b_and = bools_binary_op `AND b1 b2 in
  let v_and, t_and = time (fun () -> BV7.binary_de BV7.AND d1 e2) in
  let b_and' = BV7.bools_of_decoded v_and in
  if b_and <> b_and' then (
    pr "b1=%s\nb2=%s\n" (string_of_bools b1)
      (string_of_bools b2);
    pr "d1=%s\nv2=%s\n" (BV7.string_of_d d1)
      (BV7.string_of_d d2);
    pr "b_and =%s\n" (string_of_bools b_and);
    pr "b_and'=%s\n" (string_of_bools b_and');
    pr "v_and=%s\n" (BV7.string_of_d v_and);

    print_endline "fail and";
    exit 1
  );

  let b_or  = bools_binary_op `OR  b1 b2 in
  let v_or, t_or  = time (fun () -> BV7.binary_de BV7.OR d1 e2) in
  let b_or' = BV7.bools_of_decoded v_or in
  if b_or <> b_or' then (
    pr "b1=%s\nb2=%s\n" (string_of_bools b1)
      (string_of_bools b2);
    pr "d1=%s\nd2=%s\n" (BV7.string_of_d d1)
      (BV7.string_of_d d2);

    pr "b_or =%s\n" (string_of_bools b_or);
    pr "b_or'=%s\n" (string_of_bools b_or');
    pr "v_or=%s\n" (BV7.string_of_d v_or);

    print_endline "fail or";
    exit 1
  );

  let b_xor = bools_binary_op `XOR b1 b2 in
  let v_xor, t_xor = time (fun () -> BV7.binary_de BV7.XOR d1 e2) in
  let b_xor' = BV7.bools_of_decoded v_xor in
  if b_xor <> b_xor' then (
    pr "b1=%s\nb2=%s\n" (string_of_bools b1)
      (string_of_bools b2);
    pr "d1=%s\nd2=%s\n" (BV7.string_of_d d1)
      (BV7.string_of_d d2);

    pr "b_xor =%s\n" (string_of_bools b_xor);
    pr "b_xor'=%s\n" (string_of_bools b_xor');
    pr "v_xor=%s\n" (BV7.string_of_d v_xor);

    print_endline "fail xor";
    exit 1
  );

  let b_and_not = bools_binary_op `AND_NOT b1 b2 in
  let v_and_not, t_and_not = time (fun () -> BV7.binary_de BV7.AND_NOT d1 e2) in
  let b_and_not' = BV7.bools_of_decoded v_and_not in
  if b_and_not <> b_and_not' then (
    pr "b1=%s\nb2=%s\n" (string_of_bools b1)
      (string_of_bools b2);
    pr "d1=%s\nd2=%s\n" (BV7.string_of_d d1)
      (BV7.string_of_d d2);

    pr "b_and_not =%s\n" (string_of_bools b_and_not);
    pr "b_and_not'=%s\n" (string_of_bools b_and_not');
    pr "v_and_not=%s\n" (BV7.string_of_d v_and_not);

    print_endline "fail and_not";
    exit 1
  );

  Printf.printf "% 8d %0.5f %f %f %f %f\n%!" n density_true
    t_and t_or t_xor t_and_not

let test_rand_binary_op () =
  let density_true = Random.float 0.1 in
  let n = Random.int 1_000_000 in
  test_binary_op density_true n

let test_rand_binary_op_de () =
  let density_true = Random.float 0.5 in
  let n = Random.int 1_000_000 in
  test_binary_op_de density_true n


let rec add_n x n accu =
  if n = 0 then
    accu
  else
    add_n x (n-1) (x :: accu)


let add_index_rev ~start ~length list =
  let fini = start + length in
  (* forward *)
  let rec loop i accu =
    if i < fini then
      loop (i+1) (i :: accu)
    else
      accu
  in
  loop start list

let index_true bools =
  let rec loop i accu = function
    | [] -> List.rev accu
    | h :: t ->
      let accu =
        if h then
          i :: accu
        else
          accu
      in
      loop (i+1) accu t
  in
  loop 0 [] bools

let string_of_int_list list =
  "[" ^ (String.concat ", " (List.map string_of_int list)) ^ "]"

(* because ... [list1 = list2] or [list1 <> list2] overflows the stack
   for long lists! *)
let rec lists_equal = function
  | h1 :: t1, h2 :: t2 ->
    if h1 = h2 then
      lists_equal (t1, t2)
    else
      false
  | [], _ :: _ -> false
  | _ :: _, [] -> false
  | [], [] -> true

let test_rand_binary_fold () =
  let n = Random.int 200_000 in

  let x1 = Random.float 1.0 in
  let x2 = Random.float 1.0 in

  let b1 = Utils.rand_bools x1 n in
  let b2 = Utils.rand_bools x2 n in

  let b_and = bools_binary_op `AND_NOT b1 b2 in

  let d1 = BV7.decoded_of_bools b1 in
  let d2 = BV7.decoded_of_bools b2 in
  let e2 = BV7.encode d2 in

  let rec f_uniform ~bit_index ~length accu =
    add_index_rev ~start:bit_index ~length accu
  in

  let f_literal ~bit_index accu =
    bit_index :: accu
  in

  let true_bit_indexes_rev, t_fold = time (
      fun () ->
        BV7.binary_de_fold_true f_uniform f_literal AND_NOT d1 e2 []
    ) in

  let true_bit_indexes = List.rev true_bit_indexes_rev in
  let true_bit_indexes' = index_true b_and in

  if lists_equal (true_bit_indexes, true_bit_indexes') then
    Printf.printf "%0.4f\t%0.4f\t%0.6f\n%!"
      (BV7.density d1) (BV7.density d2) t_fold
  else (
    Printf.printf "b1=%s\nb2=%s\nb_and=%s\n"
      (string_of_bools b1) (string_of_bools b2) (string_of_bools b_and);

    Printf.printf "fold=%s\nsimple=%s\n%!"
      (string_of_int_list true_bit_indexes)
      (string_of_int_list true_bit_indexes');

    exit 1
  )

let test_iter_d () =
  let n = Random.int 1_000_00 in
  let b = Utils.rand_bools 0.1 n in
  let d = BV7.decoded_of_bools b in
  let a = Array.create n false in

  let rec f_uniform ~bit_index ~length =
    Array.fill a bit_index length true;
  in

  let f_literal ~bit_index =
    a.(bit_index) <- true
  in

  BV7.iter_d_true f_uniform f_literal d;

  let b' = Array.to_list a in
  if b' <> b then (
    print_endline "iter failed";
    exit 1
  )

let test_iter_e () =
  let n = Random.int 1_000_000 in
  let b = Utils.rand_bools 0.001 n in
  let d = BV7.decoded_of_bools b in
  let e = BV7.encode d in

  let a = Array.create n false in

  let rec f_uniform ~bit_index ~value ~length =
    Array.fill a bit_index length value;
  in

  let f_literal ~bit_index ~value =
    a.(bit_index) <- value
  in

  BV7.iter_e f_uniform f_literal e;

  let b' = Array.to_list a in
  if b' <> b then (
    print_endline "iter failed";
    exit 1
  )

let test_negate () =
  let n = Random.int 1_000_00 in
  let b = Utils.rand_bools 0.1 n in
  let d = BV7.decoded_of_bools b in
  let not_d = BV7.negate d in
  let not_b = BV7.bools_of_decoded not_d in
  let b' = List.map ( not ) not_b in
  if b' <> b then (
    print_endline "negate failed";
    exit 1
  )


let _ =
  Utils.repeat 100 test_encoding;
  Utils.repeat 100 test_rand_binary_op;
  Utils.repeat 100 test_rand_binary_op_de;
  Utils.repeat 100 test_iter_d;
  Utils.repeat 100 test_negate;
  Utils.repeat 1000 test_indexes;
  Utils.repeat 100 test_rand_binary_fold;
  Utils.repeat 100 test_iter_e

