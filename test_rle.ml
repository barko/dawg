let rec random_pairs ~max_gap ~max_value ~vec_length index accu =
  let index = index + 1 + Random.int max_gap in
  if index >= vec_length then
    List.rev accu
  else
    let value = Random.int max_value in
    let pair = index, value in
    random_pairs ~max_gap ~max_value ~vec_length index (pair :: accu)

let random_pairs ~max_gap ~max_value ~vec_length =
  random_pairs ~max_gap ~max_value ~vec_length (-1) []

let test_rle () =
  let max_gap = 3 in
  let max_value = 20 in
  let vec_length = 50 in
  let zero = 0 in
  let pairs = random_pairs ~max_gap ~max_value ~vec_length in
  let e = Rle.encode_sparse vec_length pairs zero in
  let _, d = Rle.decode e in
  let _, _, ee = Rle.encode_dense d in
  if not ( e = ee ) then (
    print_endline "fail";
    exit 1
  )
  else
    print_endline "ok"

let _ =
  Utils.repeat 100 test_rle

