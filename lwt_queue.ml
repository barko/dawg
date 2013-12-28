(** concurrent queue, based on Lwt *)

open Lwt

type 'a t = {
  mutable q : 'a Queue.t;
  (* underlying queue *)

  mutable writers : unit Lwt.u Lwt_sequence.t;
  (* threads waiting to write a value *)

  mutable readers : 'a Lwt.u Lwt_sequence.t;
  (* threads waiting to read a value *)

  mutable max_length : int ;
}

let sequence_length seq =
  Lwt_sequence.fold_l (fun _ c -> c + 1) seq 0

let num_readers t =
  sequence_length t.readers

let num_writers t =
  sequence_length t.writers

let length t = 
  Queue.length t.q

let get_max_length t =
  t.max_length

let iter f t =
  Queue.iter f t.q

let fold f x0 t =
  Queue.fold f x0 t.q

let create ?(max_length=max_int) () = { 
  (* by default, create a queue that is effectively unbounded. *)
  q = Queue.create ();
  writers = Lwt_sequence.create ();
  readers = Lwt_sequence.create ();
  max_length = max_length
}

let put t v =
  (* using [>=] because [t.max_length] is mutable *)
  if Queue.length t.q >= t.max_length then
    (* queue is full *)
    let (res, w) = Lwt.task () in
    let node = Lwt_sequence.add_r w t.writers in
    Lwt.on_cancel res (fun _ -> Lwt_sequence.remove node);
    lwt () = res in
    Queue.add v t.q;
    return ()
  else (
    (match Lwt_sequence.take_opt_l t.readers with
      | None -> Queue.add v t.q;
      | Some w -> Lwt.wakeup w v
    );
    return ()
  )
    
let add = put

let take t =
  if Queue.is_empty t.q then
    (* queue is empty *)
    let (res, w) = Lwt.task () in
    let node = Lwt_sequence.add_r w t.readers in
    Lwt.on_cancel res (fun _ -> Lwt_sequence.remove node);
    res
  else
    let v = Queue.take t.q in
    (match Lwt_sequence.take_opt_l t.writers with
      | Some w -> Lwt.wakeup w ()
      | None -> ()
    );
    return v


let set_max_length t new_max_length =
  if new_max_length < 1 then
    raise (Invalid_argument "Lwt_queue.set_max_length")

  else if new_max_length > t.max_length then (
    (* the new max size is bigger than the current one, wake up
       writers (if any) until queue is full or all writers have
       written *)
    let rec loop () =
      if Queue.length t.q < t.max_length then
        match Lwt_sequence.take_opt_l t.writers with
          | Some w -> Lwt.wakeup w (); loop ()
          | None -> ()
      else
        ()
    in
    (* commit to the new length before waking up writers, because
       writers might again increase the max length *)
    t.max_length <- new_max_length;
    loop ()
  )
  else
    t.max_length <- new_max_length



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

