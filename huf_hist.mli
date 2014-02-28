type bin = {
  left: float;
  mean: float;
  right : float
}

val create : (float * int) list -> int -> (bin * int) list
(* [create distinct_values_and_counts num_bins] returns a histogram,
   represented by a list of bins, and their corresponding count.  Each
   bin is bounded on by [bin.left] [bin.right], with mean
   [bin.mean]. The count associated with a bin represent the total sum
   of the counts of values between the left and right bounds.  The
   interpretation of [bin.left] and [bin.right] depends on the order
   of [distinct_values_and_counts]: When [distinct_values_and_counts]
   is ascending order, [bin.left] represents the upper bound of the
   bin, and [bin.right] represents the lower bound.  In descending
   order, the reverse is true.
*)
