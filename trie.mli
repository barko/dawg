type 'a trie = {
  i: int;
  node_value: 'a option;
  branches: (char * 'a trie) list;
}

val make_trie : (string * 'a) list -> int -> 'a trie
val trie_lookup : 'a trie -> string -> 'a
