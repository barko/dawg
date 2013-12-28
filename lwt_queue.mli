(** concurrent queue, based on Lwt *)

type 'a t 

val num_readers : 'a t -> int
  (* return the number of threads waiting to take elements from the queue *)

val num_writers : 'a t -> int
  (* return the number of threads waiting to add elements to the queue *)

val length : 'a t -> int
  (* the number of elements in the queue *)

val set_max_length : 'a t -> int -> unit
  (* set the maximum length to a new value *)

val get_max_length : 'a t -> int
  (* inspect the current maximum length *)

val create : ?max_length:int -> unit -> 'a t
  (* create a new queue, optionally specifying a bound on its length *)

val put : 'a t -> 'a -> unit Lwt.t
val add : 'a t -> 'a -> unit Lwt.t
  (* add an element to the queue *)

val take : 'a t -> 'a Lwt.t
  (* take elements from the queue *)

val iter : ('a -> unit) -> 'a t -> unit
  (* iterate over the queue's elements *)

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (* fold over the queue's elements *)

(* Copyright (c) 2011, barko 00336ea19fcb53de187740c490f764f4 All
   rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are
   met:
   
   1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the
   distribution.

   3. Neither the name of barko nor the names of contributors may be used
   to endorse or promote products derived from this software without
   specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

