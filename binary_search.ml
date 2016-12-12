let rec find_r a v low high =
  if low = high then
    low
  else
    let mid = (low + high) / 2 in
    if v <= a.(mid) then
      find_r a v low mid
    else
      find_r a v (mid+1) high

let find a v =
  let n = Array.length a in
  if n = 0 then
    raise Not_found
  else if v < a.(0) then
    0
  else
    let n1 = n - 1 in
    if a.(n1) < v then
      n
    else
      find_r a v 0 n1

let find_x a v =
  let n = Array.length a in
  if n = 0 then
    raise Not_found
  else if n = 1 then
    0
  else if v < a.(0) then
    0
  else
    let n2 = n - 2 in
    if a.(n2) < v then
      n - 1
    else
      find_r a v 0 n2
