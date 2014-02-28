(* unit test for Huf_hist module *)

let sort_fst_ascending list =
  List.sort (fun (v1,_) (v2,_) -> Pervasives.compare v1 v2) list

let sort_fst_descending list =
  List.sort (fun (v1,_) (v2,_) -> Pervasives.compare v2 v1) list


let _ =
  let inch = open_in Sys.argv.(1) in
  let num_bins = int_of_string Sys.argv.(2) in

  (* collect the set of distinct values in a hash table; the count is
     the value of each entry in the hash table *)

  let value_to_count = Hashtbl.create 10 in
  (try
     while true do
       let line = input_line inch in
       let value = float_of_string line in
       let count =
         try
           Hashtbl.find value_to_count value
         with Not_found ->
           0
       in
       Hashtbl.replace value_to_count value (count+1)
     done
   with End_of_file ->
     ()
  );
  close_in inch;

  (* convert to a list *)
  let distinct_value_list, num_distinct_values = Hashtbl.fold (
      fun value count (accu, num_distinct_values) ->
        ((value, count) :: accu, num_distinct_values + 1)
    ) value_to_count ([], 0) in


  let sorted_distinct_value_list = sort_fst_descending distinct_value_list in

  let open Huf_hist in
  let hist = create sorted_distinct_value_list num_bins in


  List.iter (
    fun ({ left; mean; right }, bin_count) ->
      Printf.printf "l=%f m=%f r=%f c=%d\n%!" left mean right bin_count
  ) hist

