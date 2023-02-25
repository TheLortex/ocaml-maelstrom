open Maelstrom

let can_send_to = ref []

module IntSet = Set.Make (Int)

let messages = ref IntSet.empty
let ack str = Ok (MessageBody.make ~type':str (`Assoc []))

let broadcast_handler ~ms msg =
  let open Yojson.Safe.Util in
  let message = MessageBody.payload msg |> member "message" |> to_int in
  if not (IntSet.mem message !messages) then (
    messages := IntSet.add message !messages;
    let gossip =
      MessageBody.make ~type':"broadcast" (`Assoc [ ("message", `Int message) ])
    in
    let handler = function
      | Ok _ -> ()
      | Error s -> failwith (ErrorBody.text s)
    in
    List.iter (fun node -> rpc ms node gossip handler) !can_send_to);
  ack "broadcast_ok"

let read_handler msg =
  Ok
    (MessageBody.make ~type':"read_ok"
       (`Assoc
         [
           ( "messages",
             `List
               (!messages |> IntSet.to_seq |> List.of_seq
               |> List.map (fun v -> `Int v)) );
         ]))

let topology_handler ~ms msg =
  let open Yojson.Safe.Util in
  let node_id = node ms |> id_of_node in
  let allow_list =
    MessageBody.payload msg |> member "topology" |> member node_id
    |> convert_each to_string
    |> List.map (fun node_id ->
           List.find (fun v -> id_of_node v = node_id) (nodes ms))
  in
  Eio.traceln "updated allow list";
  can_send_to := allow_list;
  ack "topology_ok"

let () =
  Eio_main.run @@ fun env ->
  with_init ~stdout:env#stdout ~stdin:env#stdin @@ fun ms ->
  Eio.traceln "init!";
  with_handler ms "broadcast" (broadcast_handler ~ms) @@ fun () ->
  with_handler ms "read" read_handler @@ fun () ->
  with_handler ms "topology" (topology_handler ~ms) @@ fun () -> wait_eof ms
