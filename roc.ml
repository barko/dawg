let trapezoid ~x1 ~x2 ~y1 ~y2 =
  let base = abs_float (x1 -. x2) in
  let height = 0.5 *. (y1 +. y2) in
  base *. height

(* area under an curve *)
let auc p y =
  let len = Array.length p in
  assert (len = Array.length y);

  let py = Array.init len (
    fun i ->
      p.(i), y.(i)
  ) in

  Array.sort (
    fun (p1, _) (p2, _) ->
      Pervasives.compare p2 p1
  ) py;

  let fp = ref 0. in
  let tp = ref 0. in
  let fp_prev = ref 0. in
  let tp_prev = ref 0. in
  let a = ref 0. in

  let p_prev = ref neg_infinity in

  for i = 0 to (Array.length py) - 1 do
    let p_i, y_i = py.(i) in
    if p_i <> !p_prev then (
      a := !a +. trapezoid ~x1:!fp ~x2:!fp_prev ~y1:!tp ~y2:!tp_prev;
      p_prev := p_i;
      fp_prev := !fp;
      tp_prev := !tp;
    );

    (* number of positive and negative's *)
    if y_i then (
      tp := !tp +. 1.;
    )
    else (
      fp := !fp +. 1.;
    )

  done;

  a := !a +. trapezoid ~x1:!fp ~x2:!fp_prev ~y1:!tp ~y2:!tp_prev;
  a := !a /. (!fp *. !tp);
  !a

(*
let _ = 
  let p = ref [] in
  let y = ref [] in

  let rec loop () =
    let line = read_line () in
    (match Pcre.split line with
       | [p_i_s; y_i_s] ->
           let p_i = float_of_string p_i_s in
           let y_i = 
             match y_i_s with
               | "1" -> true
               | "0" -> false
               | _ -> failwith "expecting 1 or 0"
           in
           p := p_i :: !p;
           y := y_i :: !y;
       | _ ->
           failwith "parse error"
    );
    loop ()
  in

  (try
     loop ()
   with End_of_file ->
     ()
  );

  let p = Array.of_list !p in
  let y = Array.of_list !y in
  let n = Array.length y in
  Printf.printf "n=%d\n%!" n;

  let a = auc p y in
  Printf.printf "auc=%f\n%!" a
*)
