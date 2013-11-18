open Bigarray
type uint8_array = (int, int8_unsigned_elt, c_layout) Array1.t

type t = uint8_array

let rec map_to_list f t i accu =
  if i < 0 then
    accu
  else
    let accu = (f t.{i}) :: accu in
    map_to_list f t (i-1) accu

let map_to_list f (t:t) =
  let n = Array1.dim t in
  map_to_list f t (n - 1) []

let length t =
  Array1.dim t

let create n : uint8_array =
  Array1.create int8_unsigned c_layout n

let sub = Array1.sub

let char_iter_i f t =
  for i = 0 to (Array1.dim t)-1 do
    f i (Char.chr t.{i})
  done

