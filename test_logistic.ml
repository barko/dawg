open Logistic

let bench () =
  let n = 1_000_000 in
  let y = Array.init n (fun _ -> if Random.bool () then 1.0 else -1.0) in

  let p = create y in
  let set = [| (* TODO *) |] in
  let best_split = update p set in
  let gamma = Array.init n (fun _ -> (Random.float 2.0) -. 1.0) in
  let result = boost p gamma in
  if result = `NaN then
    failwith "result is NaN";

  let rec run ~set_density ~feature_density =
    let feature_d =
      let b = Utils.rand_bools feature_density n in
      BV7.decoded_of_bools b in
    let feature = BV7.encode feature_d in

    let _, t_bs = Utils.time (fun () -> best_split feature) in
    Printf.printf "   % .5f % .5f % .5f %1.6f\n%!"
      set_density
      feature_density
      (BV7.density feature_d)
      t_bs;
    t_bs
  in

  let num_trials = 10 in

  let rec n_run ~set_density ~feature_density i sum_time =
    if i = num_trials then
      sum_time /. (float num_trials)
    else
      let t = run ~set_density ~feature_density in
      let sum_time = t +. sum_time in
      n_run ~set_density ~feature_density (i+1) sum_time
  in

  let n_run ~set_density ~feature_density =
    n_run ~set_density ~feature_density 0 0.0
  in

  let densities = [0.5; 0.1; 0.02; 0.004; 0.0008] in

  List.iter (
    fun set_density ->
      List.iter (
        fun feature_density ->
          let t = n_run ~set_density ~feature_density in
          Printf.printf ">> % .5f % .5f %1.6f \n" set_density feature_density t
      ) densities
  ) densities



let _ =
  bench ()
