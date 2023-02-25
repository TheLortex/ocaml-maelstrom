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

  val make : in_reply_to:int -> kind -> string -> t
  val in_reply_to : t -> int
  val kind : t -> kind
  val text : t -> string
end

module MessageBody : sig
  type t

  val make :
    ?msg_id:int ->
    ?in_reply_to:int ->
    type':string ->
    [< `Assoc of (string * Yojson.Safe.t) list ] ->
    t

  val type' : t -> string
  val msg_id : t -> int option
  val in_reply_to : t -> int option
  val payload : t -> [> `Assoc of (string * Yojson.Safe.t) list ]

  val reply :
    t -> type':string -> [< `Assoc of (string * Yojson.Safe.t) list ] -> t
end

type t

module Message : sig
  type init = t
  type t

  val make : ms:init -> string -> Yojson.Safe.t -> t
  val body : t -> Yojson.Safe.t
  val src : t -> string
  val dest : t -> string
end

type error = ErrorBody.t
type 'a res = ('a, error) Result.t

val ms_to_json : MessageBody.t res -> Yojson.Safe.t
val ms_of_json : Yojson.Safe.t -> MessageBody.t res

val with_init :
  stdin:#Eio.Flow.source -> stdout:#Eio.Flow.sink -> (t -> 'a) -> 'a

val read_raw : t -> Message.t
val read : t -> string * MessageBody.t res
val write_raw : t -> Message.t -> unit
val write : t -> string -> MessageBody.t res -> unit

val respond_with :
  t -> (string -> MessageBody.t -> MessageBody.t res * 'a) -> 'a

val node_id : t -> string
val node_ids : t -> string list
