open Maelstrom

let counter = ref 0

let node_id_to_int s =
  (* format: "n1" *)
  int_of_string (String.sub s 1 (String.length s - 1))

let rec find_ofs = function 0 -> 0 | n -> 1 + find_ofs (n / 2)

(* the goal is to generate globally unique IDs with full availability
   even when the network is partioned. By encoding the worker id in
   the ID it's possible to do that easily.*)

let () =
  Eio_main.run @@ fun env ->
  with_init ~stdout:env#stdout ~stdin:env#stdin @@ fun ms ->
  Eio.traceln "init!";
  let worker_id = node_id_to_int (node ms |> id_of_node) in
  let max_worker_id =
    nodes ms
    |> List.map (fun v -> node_id_to_int (id_of_node v))
    |> List.fold_left max 0
  in
  let offset = find_ofs max_worker_id + 1 in
  let generate () =
    incr counter;
    (!counter lsl offset) + worker_id
  in
  with_handler ms "generate"
    (fun _ ->
      Ok (MessageBody.make ~type':"generate_ok" [ ("id", `Int (generate ())) ]))
    (fun () -> wait_eof ms)
