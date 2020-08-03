Js_of_ocaml.Worker.set_onmessage((data: Synthesiscomm.MessageTypes.to_worker) =>
  Js_of_ocaml.Worker.post_message(
    Synthesiscore.Main.main(data): Synthesiscomm.MessageTypes.from_worker,
  )
);
