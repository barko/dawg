type t = int array

let create n =
  Array.init n (fun i -> i)

let shuffle a state =
  let n = Array.length a in
  for i = 0 to n-1 do
    let j = (Random.State.bits state) mod (i+1) in
    a.(i) <- a.(j);
    a.(j) <- i;
  done

let iter f t =
  Array.iter f t

let fold_left f x0 t =
  Array.fold_left f x0 t

let array f t =
  let n = Array.length t in
  Array.init n (
    fun i ->
      f ~index:i ~value:t.(i)
  )
