type point = {
  (* what is the value of the piecewise function at the split? *)
  s_gamma : float ;

  (* how many observations does the split cover? *)
  s_n : int ;

  (* what is the loss? *)
  s_loss  : float ;
}

type ordinal_split = {
  os_id : int ;
  os_split : int ;
  os_left : point ;
  os_right : point ;
}

type categorical_split = ordinal_split * int array

type t = [
  | `OrdinalSplit of ordinal_split
  | `CategoricalSplit of categorical_split
]
