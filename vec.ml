type t = {
  offset : int; (* the offset into the array where the data is stored *)
  length : int; (* the number of elements in the vector *)
  array : UInt8Array.t; (* the array in which the vector's data is stored *)
}
