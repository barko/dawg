(** binary classification metrics *)
open Printf

(*
   for y in {-1, 1}, F,
   loss = log ( 1 + exp ( -2 y F ) ),
   F = 0.5 * log ( p / ( 1 - p ) )
*)
let one_minus_eps = 1.0 -. 1e-8

let loss ~p ~y =
  if p < one_minus_eps then
    let y = if y then 1. else -1. in
    let odds = p /. (1. -. p) in
    let f = 0.5 *. (log odds) in
    log ( 1. +. ( exp (-2.0 *. y *. f) ) )
  else
    (* F -> infinity, exp(-2yF) -> 0, loss -> log(1) = 0 *)
    if y then
      (* prediction is perfect; no loss *)
      0.
    else
      infinity

let err msg line_number =
  printf "line %d: %s\n%!" line_number msg;
  exit 1

let iter_file positive_label inch =
  let positive_label = match positive_label with Some x -> x | None -> "1" in
  let negative_label_ref = ref None in
  let p = ref [] in
  let y = ref [] in

  let tt = ref 0 in
  let tf = ref 0 in
  let ft = ref 0 in
  let ff = ref 0 in

  let total_loss = ref 0. in

  let rec loop line_number =
    let line = input_line inch in
    (match Pcre.split line with
       | [ y_i_s; p_i_s ] -> (
           let p_i = float_of_string p_i_s in

           let y_i =
             if y_i_s = positive_label then
               true
             else
               match !negative_label_ref with
                 | None -> negative_label_ref := Some y_i_s; false
                 | Some x when y_i_s <> x -> err (sprintf "expecting %s" x) line_number
                 | _ -> false
           in

           let loss_i = loss ~y:y_i ~p:p_i in
           (* printf "%b %f %f\n%!" y_i p_i loss_i; *)

           let call = p_i >= 0.5 in

           p := p_i :: !p;
           y := y_i :: !y;
           total_loss := !total_loss +. loss_i;

           (match y_i, call with
             | true, true -> incr tt
             | true, false -> incr tf
             | false, true -> incr ft
             | false, false -> incr ff
           );

         )

       | _ ->
           err (sprintf "line %S doesn't have two columns" line) line_number
    );
    loop (line_number+1)

  in

  (try
     loop 1
   with End_of_file ->
     ()
  );

  let p = Array.of_list !p in
  let y = Array.of_list !y in
  let n = Array.length y in

  let nn = float n in

  let z_tt = (float !tt) /. nn in
  let z_tf = (float !tf) /. nn in
  let z_ft = (float !ft) /. nn in
  let z_ff = (float !ff) /. nn in

  let accuracy = (float (!tt + !ff)) /. nn in

  let tx = !tf + !tt in
  let fx = !ff + !ft in

  let xt = !tt + !ft in
  let xf = !tf + !ff in

  let z_tx = (float tx) /. nn in
  let z_fx = (float fx) /. nn in
  let z_xt = (float xt) /. nn in
  let z_xf = (float xf) /. nn in

  let loss = !total_loss /. (float n) in
  let auc = Roc.auc p y in

  printf "\
    n %d\n\
    tt %d %0.4e\n\
    tf %d %0.4e\n\
    ft %d %0.4e\n\
    ff %d %0.4e\n\
    t* %d %0.4e\n\
    f* %d %0.4e\n\
    *t %d %0.4e\n\
    *f %d %0.4e\n\
    accuracy %0.4e\n\
    auc %0.4e\n\
    loss %0.4e\n%!"
    n
    !tt z_tt
    !tf z_tf
    !ft z_ft
    !ff z_ff
    tx  z_tx
    fx  z_fx
    xt  z_xt
    xf  z_xf
    accuracy
    auc
    loss


let bi_metrics input_file_path_opt positive_label_opt =
  let inch =
    match input_file_path_opt with
      | None -> stdin
      | Some path ->
        open_in path
  in
  iter_file positive_label_opt inch;
  close_in inch

open Cmdliner

let commands =
  let bi_metrics_cmd =
    let doc = "compute binary classification metrics from an input file \
               containing a label {0,1} and probability in [0.0,1.0]." in

    let input_file_path =
      let doc = "csv file path (absent=stdin)" in
      Arg.(value & opt (some string) None &
           info ["i";"input"] ~docv:"PATH" ~doc)
    in

    let positive_category =
      let doc = "the positive class label" in
      Arg.(value & opt (some string) None &
             info ["p";"positive"] ~docv:"STRING" ~doc)
    in

    Term.( pure bi_metrics $ input_file_path $ positive_category ), Term.info "bi-metrics" ~doc
  in

  [bi_metrics_cmd]
