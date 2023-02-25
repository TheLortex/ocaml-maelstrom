open Maelstrom

(* Create a new Node *)
let () =
  Eio_main.run @@ fun env ->
  with_init ~stdout:env#stdout ~stdin:env#stdin @@ fun ms ->
  Eio.traceln "init!";
  while true do
    let src, msg = read ms in
    let msg = Result.get_ok msg in
    let response =
      MessageBody.make ?in_reply_to:(MessageBody.msg_id msg) ~type':"echo_ok"
        (MessageBody.payload msg)
    in
    write ms src (Ok response)
  done
