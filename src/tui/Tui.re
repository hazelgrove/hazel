/* Entry point for the Hazel TUI. Run via ./hazel-tui (see repo root),
   which builds this executable and runs it under node with the browser
   polyfill preloaded. */

open Haz3ltui;

/* Echo parsed key events; for developing/debugging the input layer.
   Run as: ./hazel-tui --keys-debug */
let keys_debug = () => {
  NodeTerm.install_exit_guards();
  NodeTerm.set_raw_mode(true);
  NodeTerm.resume_stdin();
  print_endline("keys-debug: press keys; Ctrl+C to exit\r");
  let state = ref(AnsiInput.init);
  NodeTerm.on_data(chunk => {
    let (st, events) = AnsiInput.parse(state^, chunk);
    state := st;
    List.iter(
      ev => {
        NodeTerm.write(AnsiInput.show_event(ev) ++ "\r\n");
        switch (Keymap.handle(ev)) {
        | Some(Quit) =>
          NodeTerm.set_raw_mode(false);
          NodeTerm.exit(0);
        | Some(a) => NodeTerm.write("  -> " ++ Keymap.show(a) ++ "\r\n")
        | None => NodeTerm.write("  -> (unmapped)\r\n")
        };
      },
      events,
    );
  });
};

let eval_debounce_ms = 200.0;
let esc_flush_ms = 30.0;

let run = (file: option(string)) => {
  NodeTerm.install_exit_guards();
  NodeTerm.enter();
  let model = ref(App.init(file));
  let input = ref(AnsiInput.init);
  let eval_timer: ref(option(Js_of_ocaml.Js.Unsafe.any)) = ref(None);
  let esc_timer: ref(option(Js_of_ocaml.Js.Unsafe.any)) = ref(None);

  let render = () => {
    let size = NodeTerm.size();
    let (frame, m) = App.render(~size, model^);
    model := m;
    NodeTerm.write(Frame.render(~size, frame));
  };

  let quit = () => {
    NodeTerm.leave();
    NodeTerm.exit(0);
  };

  let schedule_eval = () => {
    Option.iter(NodeTerm.clear_timeout, eval_timer^);
    eval_timer :=
      Some(
        NodeTerm.set_timeout(
          () => {
            eval_timer := None;
            if (model^.result == ResultView.Pending) {
              model := App.run_eval(model^);
              render();
            };
          },
          eval_debounce_ms,
        ),
      );
  };

  let handle_events = (events: list(AnsiInput.event)): unit => {
    List.iter(
      ev =>
        switch (Keymap.handle(ev)) {
        | None => ()
        | Some(action) =>
          let page = App.editor_height(~size=NodeTerm.size(), model^);
          let now = Unix.gettimeofday();
          let (m, should_quit) = App.apply(~now, ~page, model^, action);
          model := App.disarm(m, action);
          if (should_quit) {
            quit();
          };
        },
      events,
    );
    if (model^.result == ResultView.Pending) {
      schedule_eval();
    };
    render();
  };

  NodeTerm.on_data(chunk => {
    Option.iter(NodeTerm.clear_timeout, esc_timer^);
    esc_timer := None;
    let (st, events) = AnsiInput.parse(input^, chunk);
    input := st;
    handle_events(events);
    /* A pending lone ESC is a bare Escape press unless more bytes of a
       sequence arrive immediately; resolve it on a short timer. */
    if (st.pending != "") {
      esc_timer :=
        Some(
          NodeTerm.set_timeout(
            () => {
              esc_timer := None;
              let (st, events) = AnsiInput.flush(input^);
              input := st;
              handle_events(events);
            },
            esc_flush_ms,
          ),
        );
    };
  });
  NodeTerm.on_resize(render);
  schedule_eval();
  render();
};

let usage = () => {
  print_endline("usage: hazel-tui [file.haz]");
  print_endline("       hazel-tui --keys-debug");
  print_endline("       hazel-tui --replay '<keys>' [file.haz]");
  NodeTerm.exit(1);
};

let () = {
  Util.Os.is_mac := false; /* terminals deliver Ctrl, not Cmd: PC keymap */
  let args = Array.to_list(Sys.argv) |> List.tl;
  switch (args) {
  | ["--keys-debug", ..._] => keys_debug()
  | ["--replay", keys] => print_endline(Replay.run(keys))
  | ["--replay", keys, path] =>
    print_endline(Replay.run(~file=Some(path), keys))
  | [] when NodeTerm.is_tty() => run(None)
  | [path] when NodeTerm.is_tty() => run(Some(path))
  | []
  | [_] =>
    print_endline("hazel-tui: stdout is not a terminal");
    NodeTerm.exit(1);
  | _ => usage()
  };
};
