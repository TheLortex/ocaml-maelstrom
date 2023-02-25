let or_fail = function Ok v -> v | Error s -> invalid_arg s

module ErrorBody = struct
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

  let kind_of_yojson = function
    | `Int 0 -> Ok Timeout
    | `Int 1 -> Ok NodeNotFound
    | `Int 10 -> Ok NotSupported
    | `Int 11 -> Ok TemporarilyUnavailable
    | `Int 12 -> Ok MalformedRequest
    | `Int 13 -> Ok Crash
    | `Int 14 -> Ok Abort
    | `Int 20 -> Ok KeyDoesNotExists
    | `Int 21 -> Ok KeyAlreadyExists
    | `Int 22 -> Ok PreconditionFailed
    | `Int 30 -> Ok TxnConflict
    | `Int n -> Ok (User n)
    | _ -> Error "expected an integer value"

  let kind_to_yojson = function _ -> `Int 10

  type t = { in_reply_to : int; kind : kind; text : string } [@@deriving yojson]

  let of_json t = of_yojson t |> or_fail
  let in_reply_to t = t.in_reply_to
  let kind t = t.kind
  let text t = t.text
  let make ~in_reply_to kind text = { in_reply_to; kind; text }
end

module MessageBody = struct
  type assoc = [ `Assoc of (string * Yojson.Safe.t) list ]

  type t = {
    type' : string;
    msg_id : int option;
    in_reply_to : int option;
    payload : assoc;
  }

  let to_int_option ~label = Option.map (fun v -> (label, `Int v))
  let ( **? ) a b = match a with None -> b | Some a -> a :: b

  let to_json t =
    let (`Assoc assoc) = t.payload in
    `Assoc
      (("type", `String t.type')
      :: to_int_option ~label:"msg_id" t.msg_id
         **? to_int_option ~label:"in_reply_to" t.in_reply_to
         **? assoc)

  let is_field_allowed (k, _) =
    match k with "type" | "msg_id" | "in_reply_to" -> false | _ -> true

  let of_json json =
    let open Yojson.Safe.Util in
    let type' = member "type" json |> to_string in
    let msg_id = member "msg_id" json |> to_int_option in
    let in_reply_to = member "in_reply_to" json |> to_int_option in
    let payload = to_assoc json |> List.filter is_field_allowed in
    { type'; msg_id; in_reply_to; payload = `Assoc payload }

  let check_assoc (`Assoc payload) =
    List.iter
      (fun (k, v) ->
        if not (is_field_allowed (k, v)) then
          invalid_arg (Fmt.str "Use of reserved payload member %s" k))
      payload

  let counter = ref 0

  let make ?msg_id ?in_reply_to ~type' payload =
    check_assoc payload;
    incr counter;
    let msg_id = match msg_id with None -> Some !counter | v -> v in
    { msg_id; in_reply_to; type'; payload = (payload :> assoc) }

  let reply msg ~type' payload = make ?in_reply_to:msg.msg_id ~type' payload
  let type' t = t.type'
  let msg_id t = t.msg_id
  let in_reply_to t = t.in_reply_to

  let payload t =
    let (`Assoc p) = t.payload in
    `Assoc p
end

module Init = struct
  type t = { node_id : string; node_ids : string list } [@@deriving yojson]

  let of_json t = of_yojson t |> or_fail
end

type t = {
  stdin : unit -> Yojson.Safe.t option;
  stdout : Eio.Flow.sink;
  init : Init.t;
}

module Message = struct
  type init = t

  type t = { src : string; dest : string; body : Yojson.Safe.t }
  [@@deriving yojson { strict = false }]

  let of_json t = of_yojson t |> or_fail
  let to_json = to_yojson
  let make_raw ~init dest body = { src = init.Init.node_id; dest; body }
  let make ~ms dest body = { src = ms.init.node_id; dest; body }
  let body t = t.body
  let dest t = t.dest
  let src t = t.src
end

type error = ErrorBody.t
type 'a res = ('a, error) Result.t

let ms_to_json = function
  | Ok v -> MessageBody.to_json v
  | Error e -> (
      match ErrorBody.to_yojson e with
      | `Assoc a -> `Assoc (("type", `String "error") :: a)
      | _ -> failwith "errorbody is an object")

let ms_of_json j =
  let open Yojson.Safe.Util in
  let type' = member "type" j |> to_string in
  if type' = "error" then Error (ErrorBody.of_json j)
  else Ok (MessageBody.of_json j)

let read_raw ~stdin =
  let packet = stdin () |> Option.get in
  Eio.traceln "%s"
    (Yojson.Safe.pretty_to_string packet
    |> String.split_on_char '\n'
    |> List.map (fun s -> "<< " ^ s)
    |> String.concat "\n");
  Message.of_json packet

let read ~stdin =
  let msg = read_raw ~stdin in
  (Message.src msg, ms_of_json (Message.body msg))

let write_raw ~stdout msg =
  let json = Message.to_json msg in
  let msg = Yojson.Safe.to_string json in
  Eio.traceln "%s"
    (Yojson.Safe.pretty_to_string json
    |> String.split_on_char '\n'
    |> List.map (fun s -> ">> " ^ s)
    |> String.concat "\n");
  Eio.Flow.copy_string (msg ^ "\n") stdout

let write ~stdout ~init dest body =
  let json = ms_to_json body in
  let message = Message.make_raw ~init dest json in
  write_raw ~stdout message

let respond_with ~stdin ~stdout fn =
  let raw = read_raw ~stdin in
  let src, message = (Message.src raw, ms_of_json (Message.body raw)) in
  let response, ret = fn src (Result.get_ok message) in
  let json = ms_to_json response in
  let message = Message.{ src = raw.dest; dest = raw.src; body = json } in
  write_raw ~stdout message;
  ret

let with_init ~stdin ~stdout fn =
  let lexbuf =
    Lexing.from_function ~with_positions:false (fun bytes n ->
        Eio.traceln "read %d" n;
        let c = Cstruct.create_unsafe n in
        let l = Eio.Flow.single_read stdin c in
        Cstruct.blit_to_bytes c 0 bytes 0 l;
        l)
  in
  let lexer_state = Yojson.Safe.init_lexer () in
  let stdin =
    Yojson.Safe.seq_from_lexbuf lexer_state lexbuf |> Seq.to_dispenser
  in
  let init =
    respond_with ~stdin ~stdout @@ fun _ message ->
    ( Ok (MessageBody.reply message ~type':"init_ok" (`Assoc [])),
      message |> MessageBody.payload |> Init.of_json )
  in
  fn { stdin; stdout = (stdout :> Eio.Flow.sink); init }

let read_raw v = read_raw ~stdin:v.stdin

let read v =
  let msg = read_raw v in
  (Message.src msg, ms_of_json (Message.body msg))

let write_raw v = write_raw ~stdout:v.stdout
let write v = write ~stdout:v.stdout ~init:v.init
let respond_with v = respond_with ~stdout:v.stdout ~stdin:v.stdin

let node_id t = t.init.node_id

let node_ids t = t.init.node_ids