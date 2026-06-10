/* Entry point for the Hazel TUI: a native executable (no node/JS
   runtime). Run via ./hazel-tui (see repo root) or
   _build/default/src/tui/tui.exe directly. */

open Haz3ltui;

let eval_debounce = 0.2; /* seconds */
let esc_flush = 0.03;

/* Echo parsed key events; for developing/debugging the input layer.
   Run as: ./hazel-tui --keys-debug */
let keys_debug = () => {
  TermIO.install_exit_guards();
  TermIO.enter();
  TermIO.write("keys-debug: press keys; Ctrl+C to exit\r\n");
  let state = ref(AnsiInput.init);
  let quit = ref(false);
  while (! quit^) {
    switch (TermIO.read_input(~timeout=None)) {
    | None => quit := true
    | Some("") => ()
    | Some(chunk) =>
      let (st, events) = AnsiInput.parse(state^, chunk);
      state := st;
      List.iter(
        ev => {
          TermIO.write(AnsiInput.show_event(ev) ++ "\r\n");
          switch (Keymap.handle(ev)) {
          | Some(Quit) => quit := true
          | Some(a) => TermIO.write("  -> " ++ Keymap.show(a) ++ "\r\n")
          | None => TermIO.write("  -> (unmapped)\r\n")
          };
        },
        events,
      );
    };
  };
  TermIO.leave();
};

let run = (file: option(string)) => {
  TermIO.install_exit_guards();
  TermIO.install_winch_handler();
  TermIO.enter();
  let model = ref(App.init(file));
  let input = ref(AnsiInput.init);
  /* absolute-time deadlines for the debounced eval and lone-ESC flush */
  let eval_at: ref(option(float)) = ref(None);
  let esc_at: ref(option(float)) = ref(None);
  /* the forked evaluation worker, if one is running */
  let worker: ref(option(EvalWorker.t)) = ref(None);

  let kill_worker = () =>
    switch (worker^) {
    | Some(w) =>
      EvalWorker.kill(w);
      worker := None;
    | None => ()
    };

  let render = () => {
    let size = TermIO.size();
    let (frame, m) = App.render(~size, model^);
    model := m;
    TermIO.write(Frame.render(~size, frame));
  };

  let handle_events = (events: list(AnsiInput.event)): bool => {
    let quit = ref(false);
    List.iter(
      ev =>
        switch (Keymap.handle(ev)) {
        | None => ()
        | Some(action) =>
          let page = App.editor_height(~size=TermIO.size(), model^);
          let now = Unix.gettimeofday();
          let (m, should_quit) = App.apply(~now, ~page, model^, action);
          model := App.disarm(m, action);
          if (should_quit) {
            quit := true;
          };
        },
      events,
    );
    if (model^.result == ResultView.Pending && eval_at^ == None) {
      /* the program changed: any in-flight result is stale */
      kill_worker();
      eval_at := Some(Unix.gettimeofday() +. eval_debounce);
    };
    render();
    quit^;
  };

  /* main loop: wait for input until the nearest deadline; fire timers
     that have come due; re-render on SIGWINCH */
  let quit = ref(false);
  while (! quit^) {
    let now = Unix.gettimeofday();
    /* fire due timers */
    switch (esc_at^) {
    | Some(t) when now >= t =>
      esc_at := None;
      let (st, events) = AnsiInput.flush(input^);
      input := st;
      if (handle_events(events)) {
        quit := true;
      };
    | _ => ()
    };
    switch (eval_at^) {
    | Some(t) when now >= t =>
      eval_at := None;
      if (model^.result == ResultView.Pending) {
        kill_worker();
        worker := Some(EvalWorker.start(model^.statics));
      };
    | _ => ()
    };
    if (TermIO.resized^) {
      TermIO.resized := false;
      TermIO.refresh_size();
      render();
    };
    /* sleep until input or the nearest pending deadline */
    let timeout =
      [esc_at^, eval_at^]
      |> List.filter_map(Fun.id)
      |> List.fold_left(
           (acc, t) =>
             switch (acc) {
             | None => Some(t)
             | Some(a) => Some(min(a, t))
             },
           None,
         )
      |> Option.map(t => max(0.0, t -. Unix.gettimeofday()));
    let extra =
      switch (worker^) {
      | Some(w) => [w.fd]
      | None => []
      };
    switch (TermIO.wait(~extra, ~timeout, ())) {
    | Eof => quit := true
    | Tick => () /* timeout or signal; loop fires timers */
    | Ready(_) =>
      /* evaluation finished: install the result + probe samples */
      switch (worker^) {
      | Some(w) =>
        worker := None;
        switch (EvalWorker.collect(w)) {
        | Some(payload) =>
          model := App.apply_eval_result(payload, model^);
          render();
        | None => () /* worker died mid-write; an edit will reschedule */
        };
      | None => ()
      }
    | Input(chunk) =>
      let (st, events) = AnsiInput.parse(input^, chunk);
      input := st;
      /* a pending lone ESC is a bare Escape press unless more bytes of
         a sequence arrive immediately; resolve it on a short deadline */
      esc_at :=
        st.pending != "" ? Some(Unix.gettimeofday() +. esc_flush) : None;
      if (handle_events(events)) {
        quit := true;
      };
    };
  };
  kill_worker();
  TermIO.leave();
};

let usage = () => {
  print_endline("usage: hazel-tui [file.haz]");
  print_endline("       hazel-tui --keys-debug");
  print_endline("       hazel-tui --replay '<keys>' [file.haz]");
  exit(1);
};

let () = {
  Util.Os.is_mac := false; /* terminals deliver Ctrl, not Cmd: PC keymap */
  Util.TimeUtil.now_ms := (() => Unix.gettimeofday() *. 1000.0);
  let args = Array.to_list(Sys.argv) |> List.tl;
  switch (args) {
  | ["--keys-debug", ..._] => keys_debug()
  | ["--replay", keys] => print_endline(Replay.run(keys))
  | ["--replay", keys, path] =>
    print_endline(Replay.run(~file=Some(path), keys))
  | [] when TermIO.is_tty() => run(None)
  | [path] when TermIO.is_tty() => run(Some(path))
  | []
  | [_] =>
    print_endline("hazel-tui: stdin/stdout is not a terminal");
    exit(1);
  | _ => usage()
  };
};
