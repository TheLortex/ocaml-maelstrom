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

  type t = { kind : kind; text : string }

  let make kind text = { kind; text }
  let kind t = t.kind
  let text t = t.text
end

module ErrorBodyWire = struct
  include ErrorBody

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
  let make ~in_reply_to { ErrorBody.kind; text } = { in_reply_to; kind; text }
  let to_body t = ErrorBody.make t.kind t.text
end

module MessageBody = struct
  type payload = (string * Yojson.Safe.t) list
  type t = { type' : string; payload : payload }

  let is_field_allowed (k, _) =
    match k with "type" | "msg_id" | "in_reply_to" -> false | _ -> true

  let check_assoc payload =
    List.iter
      (fun (k, v) ->
        if not (is_field_allowed (k, v)) then
          invalid_arg (Fmt.str "Use of reserved payload member %s" k))
      payload

  let make ~type' payload =
    check_assoc payload;
    { type'; payload }

  let type' t = t.type'
  let payload t = t.payload
end

module MessageBodyWire = struct
  include MessageBody

  type t = {
    type' : string;
    msg_id : int option;
    in_reply_to : int option;
    payload : payload;
  }

  let to_int_option ~label = Option.map (fun v -> (label, `Int v))
  let ( **? ) a b = match a with None -> b | Some a -> a :: b

  let to_json t =
    let assoc = t.payload in
    `Assoc
      (("type", `String t.type')
      :: to_int_option ~label:"msg_id" t.msg_id
         **? to_int_option ~label:"in_reply_to" t.in_reply_to
         **? assoc)

  let of_json json =
    let open Yojson.Safe.Util in
    let type' = member "type" json |> to_string in
    let msg_id = member "msg_id" json |> to_int_option in
    let in_reply_to = member "in_reply_to" json |> to_int_option in
    let payload = to_assoc json |> List.filter is_field_allowed in
    { type'; msg_id; in_reply_to; payload }

  let counter = ref 0

  let make ?in_reply_to v =
    incr counter;
    let msg_id = Some !counter in
    { msg_id; in_reply_to; type' = v.MessageBody.type'; payload = v.payload }

  let type' t = t.type'
  let msg_id t = t.msg_id
  let in_reply_to t = t.in_reply_to
  let payload t = t.payload
  let to_body t = { MessageBody.payload = t.payload; type' = t.type' }
end

module Init = struct
  type t = { node_id : string; node_ids : string list } [@@deriving yojson]

  let of_payload t = of_yojson (`Assoc t) |> or_fail
end

type error = ErrorBody.t
type 'a res = ('a, error) Result.t
type handler = MessageBody.t -> MessageBody.t res
type callback = MessageBody.t res -> unit

type t = {
  stdout : Eio.Flow.sink;
  init : Init.t;
  callbacks : (int, callback) Hashtbl.t;
  handlers : (string, handler) Hashtbl.t;
  stop_cond : Eio.Condition.t;
}

module Message = struct
  type t = { src : string; dest : string; body : Yojson.Safe.t }
  [@@deriving yojson { strict = false }]

  let of_json t = of_yojson t |> or_fail
  let to_json = to_yojson
  let make_raw ~init dest body = { src = init.Init.node_id; dest; body }
  let body t = t.body
  let src t = t.src
end

let ms_to_json = function
  | Ok v -> MessageBodyWire.to_json v
  | Error e -> (
      match ErrorBodyWire.to_yojson e with
      | `Assoc a -> `Assoc (("type", `String "error") :: a)
      | _ -> failwith "errorbody is an object")

let ms_of_json j =
  let open Yojson.Safe.Util in
  let type' = member "type" j |> to_string in
  if type' = "error" then Error (ErrorBodyWire.of_json j)
  else Ok (MessageBodyWire.of_json j)

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

let handle_incoming_message ~state (src, message) =
  match message with
  (* Errors occur as a response to a request, so we look for the callback *)
  | Error v -> (
      match Hashtbl.find_opt state.callbacks (ErrorBodyWire.in_reply_to v) with
      | None -> Eio.traceln "WARNING: uncaught error response"
      | Some cb -> cb (Error (ErrorBodyWire.to_body v)))
  (* Ok can happen either as a request or as a response *)
  | Ok res -> (
      let in_reply_to = MessageBodyWire.msg_id res in
      match Hashtbl.find_opt state.handlers (MessageBodyWire.type' res) with
      | Some cb ->
          (* There's a request handler for the message *)
          let response = cb (MessageBodyWire.to_body res) in
          let response =
            match response with
            | Ok v -> Ok (MessageBodyWire.make ?in_reply_to v)
            | Error v ->
                Error
                  (ErrorBodyWire.make ~in_reply_to:(Option.get in_reply_to) v)
          in
          let json = ms_to_json response in
          let message =
            { Message.src = state.init.node_id; dest = src; body = json }
          in
          write_raw ~stdout:state.stdout message
      | None -> (
          (* No request handler, we check if a callback was setup *)
          match
            Hashtbl.find_opt state.callbacks
              (MessageBodyWire.in_reply_to res |> Option.value ~default:(-1))
          with
          | None -> Eio.traceln "WARNING: unhandled message"
          | Some cb -> cb (Ok (MessageBodyWire.to_body res))))

let flow_to_json_seq ~stdin =
  let lexbuf =
    Lexing.from_function ~with_positions:false (fun bytes n ->
        let c = Cstruct.create_unsafe n in
        let l = Eio.Flow.single_read stdin c in
        Cstruct.blit_to_bytes c 0 bytes 0 l;
        l)
  in
  let lexer_state = Yojson.Safe.init_lexer () in
  Yojson.Safe.seq_from_lexbuf lexer_state lexbuf |> Seq.to_dispenser

let with_init ~stdin ~stdout fn =
  (* setup stdin *)
  let stdin = flow_to_json_seq ~stdin in
  (* initial handshake *)
  let init =
    respond_with ~stdin ~stdout @@ fun _ message ->
    ( Ok
        (MessageBodyWire.make
           ?in_reply_to:(MessageBodyWire.msg_id message)
           (MessageBody.make ~type':"init_ok" [])),
      message |> MessageBodyWire.payload |> Init.of_payload )
  in
  (* setup state *)
  let state =
    {
      stdout = (stdout :> Eio.Flow.sink);
      init;
      callbacks = Hashtbl.create 10;
      handlers = Hashtbl.create 10;
      stop_cond = Eio.Condition.create ();
    }
  in
  (* background job to dispatch messages *)
  Eio.Fiber.first
    (fun () -> fn state)
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      while true do
        let message =read ~stdin in
        Eio.Fiber.fork ~sw (fun () -> handle_incoming_message ~state message)
      done;
      failwith "unreachable")

let send v node msg =
  let msg = Ok (MessageBodyWire.make msg) in
  write ~stdout:v.stdout ~init:v.init node msg

let with_handler v type' handler_fn fn =
  Hashtbl.add v.handlers type' handler_fn;
  Fun.protect ~finally:(fun () -> Hashtbl.remove v.handlers type') fn

let rpc v node message handler =
  let message = MessageBodyWire.make message in
  let id = MessageBodyWire.msg_id message |> Option.get in
  Hashtbl.add v.callbacks id (fun msg ->
      Hashtbl.remove v.callbacks id;
      handler msg);
  write ~stdout:v.stdout ~init:v.init node (Ok message)

let node t = t.init.node_id
let nodes t = t.init.node_ids

type node = string

let id_of_node = Fun.id
let wait_eof t = Eio.Condition.await_no_mutex t.stop_cond
