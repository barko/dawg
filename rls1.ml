(* recursive least squares for a mean-only model *)

type t = {
  theta : float;
  rinv : float;
  lambda : float;
  num_updates : int;
}

let create lambda = {
  theta = 0.0;
  rinv = 1e+8;
  lambda;
  num_updates = 0;
}

let add t y =
  let rinv = t.rinv *. (1.0 -. t.rinv /. (t.lambda +. t.rinv)) /. t.lambda in
  let theta = t.theta +. rinv *. ( y -. t.theta ) in
  let num_updates = t.num_updates + 1 in
  { t with rinv; theta; num_updates }

let theta t =
  t.theta

let num_updates t =
  t.num_updates
