let divide_up n d =
  ((n - 1) / d) + 1

let rand_bools density_true n =
  assert (density_true >= 0.0 && density_true <= 1.0);
  let rec loop i accu =
    if i = n then
      accu
    else
      let b = Random.float 1.0 < density_true in
      loop (i+1) (b :: accu)
  in
  loop 0 []

let string_of_bools bools =
  "[" ^ (String.concat ";" (List.map string_of_bool bools)) ^ "]"

let string_of_bools bools =
  let buf = Buffer.create 100 in
  let rec loop i = function
    | h :: t ->
      if i > 0 && i mod 7 = 0 then
        Buffer.add_string buf "\n ";
      let c =
        if h then
          '1'
        else
          '0'
      in
      Buffer.add_char buf c;
      loop (i+1) t
    | [] ->
      Buffer.add_string buf " ]\n";
  in
  Buffer.add_string buf "[\n ";
  loop 0 bools;
  Buffer.contents buf


let repeat n f =
  for i = 0 to n-1 do
    f ()
  done

let time f =
  let tick = Unix.gettimeofday () in
  let y = f () in
  let tock = Unix.gettimeofday () in
  y, tock -. tick


let f_xor b1 b2 =
  match b1, b2 with
    | false, false -> false
    | true , true  -> false
    | true , false -> true
    | false, true  -> true

let f_and_not b1 b2 =
  match b1, b2 with
    | false, false -> false
    | true , true  -> false
    | true , false -> true
    | false, true  -> false

module XMap ( M : Map.OrderedType ) = struct
  include Map.Make( M )
  let find_opt k t =
    try
      Some (find k t)
    with Not_found ->
      None

  let find_assert k t =
    try
      find k t
    with Not_found ->
      assert false
end

module Int = struct
  type t = int
  let compare = Pervasives.compare
end


(* [log2 x] returns pair [y, s], where [y + 1] is the highest bit index
   whose of [x] value is 1; and [s], the sum of bits whose
   value is one, up to but excluding the highest bit index.  *)
let log2 =
  let rec loop x r one_count =
    if x = 0 then
      r - 1, one_count - 1
    else
      let z =
        if x land 1 = 1 then
          1
        else
          0
      in
      let one_count = one_count + z in
      let r = r + 1 in
      loop (x lsr 1) r one_count
  in
  fun x ->
    if x <= 0 then
      raise (Invalid_argument "log2")
    else
      loop x 0 0

let width x =
  let y, s = log2 x in
  let has_remainder = s > 0 in
  if has_remainder || y = 0 then
    (* ceil *)
    y + 1
  else
    y

let num_bytes card =
  divide_up (width card) 8


let rec fold_range f ~start ~finix x =
  if start < finix then
    let x = f start x in
    fold_range f ~start:(start+1) ~finix x
  else
    x

let rec iter_range f ~start ~finix =
  if start < finix then (
    f start;
    iter_range f ~start:(start+1) ~finix
  )

(* returns a non-normalized absolute path *)
let abspath file_path =
  if Filename.is_relative file_path then
    Filename.concat (Unix.getcwd ()) file_path
  else
    file_path

