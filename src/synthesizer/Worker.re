Js_of_ocaml.Worker.set_onmessage((data: MessageTypes.to_worker) =>
  Js_of_ocaml.Worker.post_message(Main.main(data): MessageTypes.from_worker)
);
