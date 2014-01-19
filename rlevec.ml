open UInt8Array

type t = UInt8Array.t

let read_int (a:t) ~i ~num_bytes ~ms_bits =
  assert ( 0 < num_bytes && num_bytes <= 4 );
  match num_bytes with
    | 1 ->
      let b0 = a.{i} in
      (ms_bits lsl 8) lor b0

    | 2 ->
      let b0 = a.{i} in
      let b1 = a.{i+1} in
      (ms_bits lsl 16) lor (b1 lsl 8) lor b0

    | 3 ->
      let b0 = a.{i} in
      let b1 = a.{i+1} in
      let b2 = a.{i+2} in
      (* Printf.printf "R:3 --> %d,%d,%d\n" a.{i} a.{i+1} a.{i+2}; *)
      (ms_bits lsl 24) lor (b2 lsl 16) lor (b1 lsl 8) lor b0

    | 4 ->
      let b0 = a.{i} in
      let b1 = a.{i+1} in
      let b2 = a.{i+2} in
      let b3 = a.{i+3} in
      (* Printf.printf "R:4 --> %d,%d,%d,%d\n" a.{i} a.{i+1} a.{i+2} a.{i+3}; *)
      (ms_bits lsl 32) lor (b3 lsl 24) lor (b2 lsl 16) lor (b1 lsl 8) lor b0

    | _ -> assert false


let max_length = 7 + (1 lsl 32)
let max_value = 6 + (1 lsl 32)

let read_run (a:t) i =
  let ai = a.{i} in
  match ai land 0b11_00_00_00 with
    | 0b00_00_00_00 ->
      (* length inline, value inline *)
      let length = 1 + ((ai land 0b00_111_0_00) lsr 3) in
      let value = ai land 0b00_000_111 in
      length, value, 1

    | 0b01_00_00_00 ->
      (* length inline, value not inline *)
      let length = 1 + ((ai land 0b00_111_0_00) lsr 3) in
      let value_msb = (ai land 0b00_000_1_00) lsr 2 in
      let value_num_bytes = 1 + (ai land 0b00_000_0_11) in
      let value_m7 = read_int a ~i:(i+1) ~num_bytes:value_num_bytes
          ~ms_bits:value_msb in
      let value = value_m7 + 7 in
      length, value, 1 + value_num_bytes

    | 0b10_00_00_00 ->
      (* length not inline, value inline *)
      let length_msb = (ai land 0b00_1_00_000) lsr 5 in
      let length_num_bytes = 1 + ((ai land 0b00_0_11_000) lsr 3) in
      let value = ai land 0b00_0_00_111 in
      let length_m8 = read_int a ~i:(i+1) ~num_bytes:length_num_bytes
          ~ms_bits:length_msb in
      let length = length_m8 + 8 in
      length, value, 1 + length_num_bytes

    | 0b11_00_00_00 ->
      (* length not inline, value not inline *)
      let length_msb = (ai land 0b00_1_00_000) lsr 5 in
      let length_num_bytes = 1 + ((ai land 0b00_0_11_000) lsr 3) in
      let value_msb = (ai land 0b00_000_1_00) lsr 2 in
      let value_num_bytes = 1 + (ai land 0b00_000_0_11) in
      (* Printf.printf "length_msb=%d length_num_bytes=%d value_msb=%d value_num_bytes=%d\n%!"
        length_msb length_num_bytes value_msb value_num_bytes; *)
      let i1 = i + 1 in
      (* first length *)
      let length_m8 = read_int a ~i:i1
          ~num_bytes:length_num_bytes ~ms_bits:length_msb in
      let length = length_m8 + 8 in

      (* then value *)
      let value_m7 = read_int a ~i:(i1 + length_num_bytes)
          ~num_bytes:value_num_bytes ~ms_bits:value_msb in
      let value = value_m7 + 7 in
      length, value, 1 + length_num_bytes + value_num_bytes

    | _ -> assert false


let write_msb_and_num_bytes a ~i ~x =
  if x < 256 then (
    a.{i} <- x;
    0, 1
  )
  else if x < 512 then (
    a.{i} <- (x land 0xff);
    1, 1
  )
  else if x < 65536 then (
    a.{i  } <- (x land 0x00_ff);
    a.{i+1} <- (x land 0xff_00) lsr 8;
    0, 2
  )
  else if x < 131_072 then (
    a.{i  } <- (x land 0x00_ff);
    a.{i+1} <- (x land 0xff_00) lsr 8;
    1, 2
  )
  else if x < 16_777_216 then (
    a.{i  } <- (x land 0x00_00_ff);
    a.{i+1} <- (x land 0x00_ff_00) lsr 8;
    a.{i+2} <- (x land 0xff_00_00) lsr 16;
    0, 3
  )
  else if x < 33_554_432 then (
    a.{i  } <- (x land 0x00_00_ff);
    a.{i+1} <- (x land 0x00_ff_00) lsr 8;
    a.{i+2} <- (x land 0xff_00_00) lsr 16;
    (* Printf.printf "1,3 --> %d,%d,%d\n" a.{i} a.{i+1} a.{i+2}; *)
    1, 3
  )
  else if x < 4_294_967_296 then (
    a.{i  } <- (x land 0x00_00_00_ff);
    a.{i+1} <- (x land 0x00_00_ff_00) lsr 8;
    a.{i+2} <- (x land 0x00_ff_00_00) lsr 16;
    a.{i+3} <- (x land 0xff_00_00_00) lsr 24;
    (* Printf.printf "0,4 --> %d,%d,%d,%d\n" a.{i} a.{i+1} a.{i+2} a.{i+3}; *)
    0, 4
  )
  else if x < 8_589_934_592 then (
    a.{i  } <- (x land 0x00_00_00_ff);
    a.{i+1} <- (x land 0x00_00_ff_00) lsr 8;
    a.{i+2} <- (x land 0x00_ff_00_00) lsr 16;
    a.{i+3} <- (x land 0xff_00_00_00) lsr 24;
    1, 4
  )
  else
    assert false


let write_run a ~i ~length ~value =
  assert (0 <= value  && value  <= max_value &&
          1 <= length && length <= max_length );

  let ai, num_bytes_written =
    if length < 9 then
      if value < 8 then
        (* length inline; value inline *)
        ((length - 1) lsl 3) lor value, 1

      else
        (* length inline; value not inline *)
        let value_m7 = value - 7 in
        let value_msb, value_num_bytes = write_msb_and_num_bytes a ~i:(i+1)
            ~x:value_m7 in
        0b01_000_0_00 lor
          ((length - 1) lsl 3) lor
          (value_msb lsl 2) lor
          (value_num_bytes - 1), 1 + value_num_bytes

    else
      let length_m8 = length - 8 in
      let i1 = i + 1 in
      let length_msb, length_num_bytes = write_msb_and_num_bytes a ~i:i1
          ~x:length_m8 in
      if value < 8 then
        (* length not inline; value inline *)
        0b10_0_00_000 lor
          (length_msb lsl 5) lor
          ((length_num_bytes - 1) lsl 3) lor
          value, 1 + length_num_bytes

      else
        let value_m7 = value - 7 in
        let value_msb, value_num_bytes = write_msb_and_num_bytes a
            ~i:(i1+length_num_bytes) ~x:value_m7 in
        0b11_0_00_0_00 lor
          (length_msb lsl 5) lor
          ((length_num_bytes - 1) lsl 3) lor
          (value_msb lsl 2) lor
          (value_num_bytes - 1), 1 + length_num_bytes + value_num_bytes
  in
  a.{i} <- ai;
  num_bytes_written


let write runs =
  (* the maximum possible size for a single run *)
  let a = UInt8Array.create 9 in
  (* first, determine the length that would be consumed by each run,
     and their sum: *)
  let vec_num_bytes, total_length, num_runs = List.fold_left (
    fun (vec_num_bytes, total_length, num_runs) (length, value) ->
      let num_bytes = write_run a ~i:0 ~length ~value in
      vec_num_bytes + num_bytes, total_length + length, num_runs + 1
    ) (0,0,0) runs in

  let a = UInt8Array.create vec_num_bytes in
  let vec_num_bytes_0 = List.fold_left (
    fun offset (length, value) ->
      let num_bytes = write_run a ~i:offset ~length ~value in
      offset + num_bytes
  ) 0 runs in
  assert ( vec_num_bytes = vec_num_bytes_0 );
  a, total_length, num_runs


let read_rev =
  let rec loop t vec_length offset sum_length accu =
    if sum_length < vec_length then
      let length, value, num_bytes_read = read_run t offset in
      let run = length, value in
      let accu = run :: accu in
      let sum_length = sum_length + length in
      let offset = offset + num_bytes_read in
      loop t vec_length offset sum_length accu
    else if sum_length = vec_length then
      accu
    else
      assert false

  in
  fun t ~vec_length ~offset ->
    assert (vec_length > 0);
    assert (offset > 0 && offset < UInt8Array.length t );
    loop t vec_length offset 0 []

let iter =
  let rec loop v f sum_length offset =
    if sum_length < v.Vec.length then
      let length, value, num_bytes_read = read_run v.Vec.array offset in
      f ~index:sum_length ~length ~value;
      let sum_length = sum_length + length in
      let offset = offset + num_bytes_read in
      loop v f sum_length offset
    else if sum_length = v.Vec.length then
      ()
    else
      assert false
  in
  fun v f ->
    assert (v.Vec.length > 0);
    assert (v.Vec.offset >= 0 && v.Vec.offset < UInt8Array.length v.Vec.array );
    loop v f 0 v.Vec.offset


let fold v f x0 =
  let x = ref x0 in
  iter v (
    fun ~index ~length ~value ->
      x := f ~index ~length ~value !x
  );
  !x
