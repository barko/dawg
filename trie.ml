module H = Hashtbl

type 'a trie = {
  i: int;
  node_value: 'a option;
  branches: (char * 'a trie) list;
}

let rec make_trie (strings_to_value: (string * 'a) list) (i: int) : 'a trie =
  let ht : (char, (string * 'a) list) Hashtbl.t = Hashtbl.create 32 in
  let long_to_value, short_to_value = List.partition (fun (s, _) -> String.length s > i) strings_to_value in
  List.iter (fun string_value ->
    let c = (fst string_value).[i] in
    if not (Hashtbl.mem ht c) then (
      Hashtbl.replace ht c [string_value];
    ) else (
      let prev_string_values = Hashtbl.find ht c in
      Hashtbl.replace ht c (string_value :: prev_string_values);
    )
  ) long_to_value;
  let i' = succ i in
  match short_to_value with
    | [s, v] ->
      let branches = H.fold (fun c sv accu -> (c, make_trie sv i') :: accu) ht [] in
      { i; node_value = Some v; branches }
    | [] ->
      let branches = H.fold (fun c sv accu -> (c, make_trie sv i') :: accu) ht [] in
      { i; node_value = None; branches }
    | _ -> assert false

let rec trie_lookup trie string =
  match trie with
    | {i; node_value} when i >= String.length string ->
      (match node_value with Some x -> x | None -> raise Not_found)
    | {i; branches } ->
      trie_lookup (List.assoc string.[i] branches) string
