module ErrorBody : sig
  type t
  (** An error message *)

  type kind =
    | Timeout
    | NodeNotFound
    | NotSupported
    | TemporarilyUnavailable
    | MalformedRequest
    | Crash
    | Abort
    | KeyDoesNotExists
    | KeyAlreadyExists
    | PreconditionFailed
    | TxnConflict
    | User of int (* [n] should be higher than 1000 *)

  val make : kind -> string -> t
  val kind : t -> kind
  val text : t -> string
end

module MessageBody : sig
  type t
  (** An application message *)

  type payload = (string * Yojson.Safe.t) list
  (** The payload is an associative list included in the final JSON object. 
      It cannot include reserved values: `type`, `msg_id`, `in_reply_to`. *)

  val make : type':string -> payload -> t
  val type' : t -> string
  val payload : t -> payload
end

type t
(** The type for a Maelstrom node that has performed the initialization step *)

type node
(** An accessible node *)

type 'a res = ('a, ErrorBody.t) Result.t
(** The type for standard messages. *)

(** Setup *)

val with_init :
  stdin:#Eio.Flow.source -> stdout:#Eio.Flow.sink -> (t -> 'a) -> 'a
(** [with_init ~stdint ~stdout fn] handshakes with Maelstrom and set up the callback infrastructure, then calls [fn] *)

val node : t -> node
(** [node ms] returns the node of the current Maelstrom node *)

val nodes : t -> node list
(** [nodes ms] returns the list of all Maelstrom nodes *)

val id_of_node : node -> string
(** [id_of_node n] returns the string representation of [n]*)

(** Communication*)

val send : t -> node -> MessageBody.t -> unit
(** [send ms node message] sends a one-off message to [node], and doesn't 
    expect an answer. *)

val rpc : t -> node -> MessageBody.t -> (MessageBody.t res -> unit) -> unit
(** [rpc node message handler] sends [message] to [node] and expects an 
    answer at some point. When the answer is received, [handler] is called with 
    the response. *)

val with_handler :
  t -> string -> (MessageBody.t -> MessageBody.t res) -> (unit -> 'a) -> 'a
(** [with_handler ms type' handler fn] sets up [handler] as the function to 
    call when a message of type [type'] is received. The handler is expected 
    to send a response, be it an error or a message. Then, [fn] is called, when it returns, the handler is removed. *)

val wait_eof : t -> unit
(** [wait_eof ms] waits for the input to go end of file before terminating. *)
