Js_of_ocaml.Worker.set_onmessage((data: Synthesiscore.Types.input) =>
  Js_of_ocaml.Worker.post_message(
    Synthesiscore.Main.main(data): Synthesiscore.Types.output,
  )
);
