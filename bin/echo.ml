open Maelstrom

let () =
  Eio_main.run @@ fun env ->
  with_init ~stdout:env#stdout ~stdin:env#stdin @@ fun ms ->
  Eio.traceln "init!";
  with_handler ms "echo"
    (fun msg ->
      Ok (MessageBody.make ~type':"echo_ok" (MessageBody.payload msg)))
    (fun () -> wait_eof ms)
