module ISM = Stream_merge.Make(
  struct
    type t = string * int
    let leq (_, i1) (_, i2) = i1 <= i2
  end)

let () =
  let key k i = k, i in

  let a = List.map (key "A") [1;2;6;9;10;12] in
  let b = List.map (key "B") [0;3;5;6;6;8;11] in
  let c = List.map (key "C") [10;10;10;30;30;30;30] in

  let sa = Stream.of_list a in
  let sb = Stream.of_list b in
  let sc = Stream.of_list c in

  let merger = ISM.create [sa;sb;sc] in

  let rec loop () =
    let value = Stream.next merger in
    let k, v = value in
    Printf.printf "k=%s v=%d\n" k v;
    loop ()
  in
  try
    loop ()
  with Stream.Failure ->
    print_endline "done"

