open Rlevec

let random_length_value_hi =
  let max_length = Int64.of_int Rlevec.max_length in
  let max_value = Int64.of_int Rlevec.max_value in
  let max_length_1 = Int64.sub max_length 1L in
  fun () ->
    let length = Int64.to_int (Int64.add 1L (Random.int64 max_length_1)) in
    let value = Int64.to_int (Random.int64 max_value) in
    length, value

let random_length_value_lo =
  let max_length = 10 in
  let max_value = 10 in
  fun () ->
    let length = 1 + (Random.int max_length) in
    let value = Random.int max_value in
    length, value

let test_random_run () =
  let a = UInt8Array.create 9 in
  let length, value = random_length_value_lo () in
  let num_written = write_run a ~i:0 ~length ~value in
  let l, v, _ = read_run a 0 in
  if l = length && v = value then
    ()
  else (
    Printf.printf "fail: num_written=%d length=%d,%d value=%d,%d\n%!" num_written
      length l value v;
    exit 1
  )

let _ =
  Utils.repeat 1_000_000 test_random_run
