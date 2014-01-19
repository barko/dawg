type t = UInt8Array.t

let read_uint_1 (t:t) offset =
  t.{offset}

let write_uint_1 (t:t) ~offset ~value =
  assert( 0 <= value && value < 256 );
  t.{offset} <- value

let write_uint_1_to_channel ch value =
  assert( 0 <= value && value < 256 );
  output_char ch (Char.chr value)

let read_uint_2 (t:t) offset =
  let b0 = t.{offset  } in
  let b1 = t.{offset+1} in
  (b1 lsl 8) lor b0

let write_uint_2 (t:t) ~offset ~value =
  assert( 0 <= value && value < 65_536 );
  let b0 = (value      ) land 0xff in
  let b1 = (value lsr 8) land 0xff in
  t.{offset  } <- b0;
  t.{offset+1} <- b1

let write_uint_2_to_channel ch value =
  assert( 0 <= value && value < 65_536 );
  let b0 = (value      ) land 0xff in
  let b1 = (value lsr 8) land 0xff in
  output_char ch (Char.chr b0);
  output_char ch (Char.chr b1)

let read_uint_3 (t:t) offset =
  let b0 = t.{offset  } in
  let b1 = t.{offset+1} in
  let b2 = t.{offset+2} in
  (b2 lsl 16) lor (b1 lsl 8) lor b0

let write_uint_3 (t:t) ~offset ~value =
  assert( 0 <= value && value < 16_777_216 );
  let b0 = (value       ) land 0xff in
  let b1 = (value lsr  8) land 0xff in
  let b2 = (value lsr 16) land 0xff in
  t.{offset  } <- b0;
  t.{offset+1} <- b1;
  t.{offset+2} <- b2

let write_uint_3_to_channel ch value =
  assert( 0 <= value && value < 16_777_216 );
  let b0 = (value       ) land 0xff in
  let b1 = (value lsr  8) land 0xff in
  let b2 = (value lsr 16) land 0xff in
  output_char ch (Char.chr b0);
  output_char ch (Char.chr b1);
  output_char ch (Char.chr b2)

let read_uint_4 (t:t) offset =
  let b0 = t.{offset  } in
  let b1 = t.{offset+1} in
  let b2 = t.{offset+2} in
  let b3 = t.{offset+3} in
  (b3 lsl 24) lor (b2 lsl 16) lor (b1 lsl 8) lor b0

let write_uint_4 (t:t) ~offset ~value =
  assert( 0 <= value && value < 4_294_967_296 );
  let b0 = (value       ) land 0xff in
  let b1 = (value lsr  8) land 0xff in
  let b2 = (value lsr 16) land 0xff in
  let b3 = (value lsr 24) land 0xff in
  t.{offset  } <- b0;
  t.{offset+1} <- b1;
  t.{offset+2} <- b2;
  t.{offset+3} <- b3

let write_uint_4_to_channel ch value =
  assert( 0 <= value && value < 4_294_967_296 );
  let b0 = (value       ) land 0xff in
  let b1 = (value lsr  8) land 0xff in
  let b2 = (value lsr 16) land 0xff in
  let b3 = (value lsr 24) land 0xff in
  output_char ch (Char.chr b0);
  output_char ch (Char.chr b1);
  output_char ch (Char.chr b2);
  output_char ch (Char.chr b3)

let read_uint_5 (t:t) offset =
  let b0 = t.{offset  } in
  let b1 = t.{offset+1} in
  let b2 = t.{offset+2} in
  let b3 = t.{offset+3} in
  let b4 = t.{offset+4} in
  (b4 lsl 32) lor (b3 lsl 24) lor (b2 lsl 16) lor (b1 lsl 8) lor b0

let write_uint_5 (t:t) ~offset ~value =
  assert( 0 <= value && value < 1_099_511_627_776 );
  let b0 = (value       ) land 0xff in
  let b1 = (value lsr  8) land 0xff in
  let b2 = (value lsr 16) land 0xff in
  let b3 = (value lsr 24) land 0xff in
  let b4 = (value lsr 32) land 0xff in
  t.{offset  } <- b0;
  t.{offset+1} <- b1;
  t.{offset+2} <- b2;
  t.{offset+3} <- b3;
  t.{offset+4} <- b4

let write_uint_5_to_channel ch value =
  assert( 0 <= value && value < 1_099_511_627_776 );
  let b0 = (value       ) land 0xff in
  let b1 = (value lsr  8) land 0xff in
  let b2 = (value lsr 16) land 0xff in
  let b3 = (value lsr 24) land 0xff in
  let b4 = (value lsr 32) land 0xff in
  output_char ch (Char.chr b0);
  output_char ch (Char.chr b1);
  output_char ch (Char.chr b2);
  output_char ch (Char.chr b3);
  output_char ch (Char.chr b4)

let read_fns = [|
  read_uint_1;
  read_uint_2;
  read_uint_3;
  read_uint_4;
  read_uint_5;
|]

let write_fns = [|
  write_uint_1_to_channel;
  write_uint_2_to_channel;
  write_uint_3_to_channel;
  write_uint_4_to_channel;
  write_uint_5_to_channel
|]

let write_to_channel ch ~width list =
  let write = write_fns.( width - 1 ) ch in
  List.fold_left (
    fun num_written value ->
      write value;
      num_written + width
  ) 0 list


let iter =
  let rec loop e read width f i offset =
    if i < e.Vec.length then
      let value = read offset in
      f ~index:i ~value;
      loop e read width f (i+1) (offset+width)
  in
  fun ~width e f ->
    assert( 0 < width && width <= 5 );
    let read = read_fns.(width - 1) e.Vec.array in
    loop e read width f 0 e.Vec.offset

let fold ~width =
  let iter_w = iter ~width in
  fun e f x0 ->
    let x = ref x0 in
    iter_w e (
      fun ~index ~value ->
        x := f ~index ~value !x
    );
    !x
