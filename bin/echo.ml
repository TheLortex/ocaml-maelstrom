open Maelstrom

let () =
  Eio_main.run @@ fun env ->
  with_init ~stdout:env#stdout ~stdin:env#stdin @@ fun ms ->
  Eio.traceln "init!";
  while true do
    respond_with ms @@ fun _ msg ->
    assert (MessageBody.type' msg = "echo");
    let response =
      MessageBody.reply msg ~type':"echo_ok" (MessageBody.payload msg)
    in
    (Ok response, ())
  done
