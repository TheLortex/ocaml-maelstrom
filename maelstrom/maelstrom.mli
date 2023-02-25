module ErrorBody : sig
  type t

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
    | User of int

  val make : kind -> string -> t
  val kind : t -> kind
  val text : t -> string
end

module MessageBody : sig
  type t

  val make : type':string -> [< `Assoc of (string * Yojson.Safe.t) list ] -> t
  val type' : t -> string
  val payload : t -> [> `Assoc of (string * Yojson.Safe.t) list ]
end

type t
type node
type error = ErrorBody.t
type 'a res = ('a, error) Result.t

val with_init :
  stdin:#Eio.Flow.source -> stdout:#Eio.Flow.sink -> (t -> 'a) -> 'a

val send : t -> node -> MessageBody.t -> unit

val with_handler :
  t -> string -> (MessageBody.t -> MessageBody.t res) -> (unit -> 'a) -> 'a

val rpc : t -> node -> MessageBody.t -> (MessageBody.t res -> unit) -> unit
val node : t -> node
val nodes : t -> node list
val id_of_node : node -> string
val wait_eof : t -> unit
