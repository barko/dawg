(** TCP server and client *)

(* These modules implement a simple protocol, whereby the a message
   payload is preceded by its length, encoded as a binary 4-byte
   integer.  Empty (zero-length) messages are permitted. *)

module Server : sig
  type t

  type peer
  val sockaddr_of_peer : peer -> Unix.sockaddr

  type event = [
  | `Connect (* a client connected *)
  | `Disconnect  (* a client disconnected *)
  | `Message of string  (* client sent a message (which could be the
                           empty string *)
  ]

  val recv : t -> (peer * event) Lwt.t
  (* [recv t] gets a message, along with the peer which sent it *)

  val send : t -> peer -> string option -> unit Lwt.t
  (* [send t peer (Some "foo")] sends client [peer] message ["foo"].
     [send t peer None] terminates the connection to client [peer].  *)

  val create : int -> t
  (* [create port] creates a server at port [port] *)

end

module Client : sig
  type t

  val recv : t -> string option Lwt.t
  (* [recv t] gets a message from the server; [None] is received when
     the server disconnects *)

  val send : t -> string option -> unit Lwt.t
  (* [send t (Some msg)] sends message [msg] to the server; [send t
     None] disconnects from the server *)

  val create : string -> int -> t Lwt.t
  (* [create host port] creates a client to the server listening on
     [host] and [port] *)

end
