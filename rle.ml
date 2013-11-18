type 'a run = int * 'a
type 'a runs = 'a run list

let encode_dense_rev =
  let rec loop ((prev_length, prev_value) as prev_run)
      num_runs num_elements accu = function
    | h :: t ->
      if h = prev_value then
        (* extend [prev_run] *)
        loop (prev_length+1, prev_value) num_runs (num_elements+1) accu t
      else
        (* start a new run *)
        let accu = prev_run :: accu in
        loop (1, h) (num_runs+1) (num_elements+1) accu t
    | [] ->
      num_runs, num_elements, prev_run :: accu
  in
  function
    | h :: t ->
      loop (1, h) 1 1 [] t
    | [] -> 0, 0, []

let encode_dense list =
  let num_runs, num_elements, runs_rev = encode_dense_rev list in
  num_runs, num_elements, List.rev runs_rev


exception NonIncreasing of int
exception OutOfBounds of int

let check_oob vec_length index =
  if index < 0 || index >= vec_length then
    raise (OutOfBounds index)

let pr_pair (length, value) =
  Printf.sprintf "L=%d V=%d" length value

let str_list list =
  String.concat "," (List.map pr_pair list)

let str_list_rev list =
  str_list (List.rev list)

let pr_list_rev list =
  print_endline (str_list_rev list)

let rec encode_sparse_rev ~zero ~vec_length ~vec_length_so_far list runs =
  match list, runs with
    | (index, value) :: t, [] ->
      check_oob vec_length index;
      (* no runs so far *)
      if value = zero then
        (* strip leading zeros *)
        encode_sparse_rev ~zero ~vec_length ~vec_length_so_far t runs
      else if index = 0 then
        let runs = [1, value] in
        let vec_length_so_far = 1 in
        encode_sparse_rev ~zero ~vec_length ~vec_length_so_far t runs
      else if index > 0 then
        (* insert a zero run *)
        let runs = [index, zero] in
        let vec_length_so_far = index in
        encode_sparse_rev ~zero ~vec_length ~vec_length_so_far list runs
      else
        assert false


    | (index, value) :: t, (prev_length, prev_value) :: runs_t ->
      check_oob vec_length index;
      let diff = index - vec_length_so_far in
      assert (prev_length > 0);
      if diff < 0 then
        raise (NonIncreasing index)
      else if diff = 0 then
        (* no gap *)
        let vec_length_so_far = vec_length_so_far + 1 in
        if value = prev_value then
          (* extend prev run by 1 *)
          let run = prev_length + 1, value in
          let runs  = run :: runs_t in
          encode_sparse_rev ~zero ~vec_length ~vec_length_so_far t runs
        else
          (* new run *)
          let run = 1, value in
          let runs = run :: runs in
          encode_sparse_rev ~zero ~vec_length ~vec_length_so_far t runs
      else
        (* add or extend a zero run; [index, value] remain unconsumed *)
        let vec_length_so_far = index in

        let runs =
          if zero = prev_value then
            let run = prev_length + diff, prev_value in
            run :: runs_t
          else
            let zero_run = diff, zero in
            zero_run :: runs
        in
        encode_sparse_rev ~zero ~vec_length ~vec_length_so_far list runs

    | [], (prev_length, prev_value) :: runs_t ->
      (* no more in [list]; terminate *)
      if vec_length = vec_length_so_far then
        runs
      else if prev_value = zero then
        (* extend the last run, whose value is zero *)
        let diff = vec_length - vec_length_so_far in
        assert (diff > 0);
        let last_run = prev_length + diff, zero in
        last_run :: runs_t
      else
        (* add the last run, a zero run, different from the previous
           run *)
        let diff = vec_length - vec_length_so_far in
        assert (diff > 0);
        let last_run = diff, zero in
        last_run :: runs

    | [], [] ->
      assert (vec_length_so_far = 0);
      let single_run = vec_length, zero in
      [single_run]

let encode_sparse_rev vec_length pairs zero =
  encode_sparse_rev ~zero ~vec_length ~vec_length_so_far:0 pairs []

let encode_sparse vec_length pairs zero =
  let runs_rev = encode_sparse_rev vec_length pairs zero in
  List.rev runs_rev


let rec add_repeat length x accu =
  if length = 0 then
    accu
  else
    add_repeat (length-1) x (x :: accu)

let decode_rev =
  let rec loop vec_length accu = function
    | (run_length, value) :: t ->
      let accu = add_repeat run_length value accu in
      let vec_length = vec_length + run_length in
      loop vec_length accu t
    | [] ->
      vec_length, accu
  in
  fun runs ->
    loop 0 [] runs

let decode runs =
  let vec_length, runs_rev = decode_rev runs in
  vec_length, List.rev runs_rev
