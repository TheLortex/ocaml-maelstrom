open Maelstrom

let can_send_to = ref []

module IntSet = Set.Make (Int)

let messages = ref IntSet.empty
let ack str = Ok (MessageBody.make ~type':str [])

let broadcast_handler ~clock ~ms msg =
  let open Yojson.Safe.Util in
  let message =
    `Assoc (MessageBody.payload msg) |> member "message" |> to_int
  in
  if not (IntSet.mem message !messages) then (
    messages := IntSet.add message !messages;
    let gossip =
      MessageBody.make ~type':"broadcast" [ ("message", `Int message) ]
    in
    let handler ~acked = function
      | Ok _ -> acked := true
      | Error s -> Eio.traceln "error: %s" (ErrorBody.text s)
    in
    Eio.Fiber.List.iter
      (fun node ->
        let acked = ref false in
        let rec retry () =
          rpc ms node gossip (handler ~acked);
          Eio.Time.sleep clock 1.0;
          if not !acked then (
            Eio.traceln "node %s didn't ack" (id_of_node node);
            retry ())
        in
        retry ())
      !can_send_to);
  ack "broadcast_ok"

let read_handler _ =
  Ok
    (MessageBody.make ~type':"read_ok"
       [
         ( "messages",
           `List
             (!messages |> IntSet.to_seq |> List.of_seq
             |> List.map (fun v -> `Int v)) );
       ])

let topology_handler ~ms msg =
  let open Yojson.Safe.Util in
  let node_id = node ms |> id_of_node in
  let allow_list =
    `Assoc (MessageBody.payload msg)
    |> member "topology" |> member node_id |> convert_each to_string
    |> List.map (fun node_id ->
           List.find (fun v -> id_of_node v = node_id) (nodes ms))
  in
  Eio.traceln "updated allow list";
  can_send_to := allow_list;
  ack "topology_ok"

let () =
  Eio_main.run @@ fun env ->
  let clock = env#clock in
  with_init ~stdout:env#stdout ~stdin:env#stdin @@ fun ms ->
  Eio.traceln "init!";
  with_handler ms "broadcast" (broadcast_handler ~clock ~ms) @@ fun () ->
  with_handler ms "read" read_handler @@ fun () ->
  with_handler ms "topology" (topology_handler ~ms) @@ fun () -> wait_eof ms
