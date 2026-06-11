/* Entry point for the Hazel TUI: a native executable built on notty
   (terminal lifecycle, input parsing, image rendering). Run via
   ./hazel-tui (see repo root). */

open Haz3ltui;
module Term = Notty_unix.Term;

let eval_debounce = 0.2; /* seconds */

/* IMPORTANT: the term is created with ~dispose=false and we never
   register at_exit handlers that touch the terminal — the eval worker
   forks, and an inherited exit-time restore in the child would
   tcsetattr the shared pty back to cooked mode under our feet (the
   bug fixed in 9f05fc5734). Restoration is explicit on every parent
   exit path instead. */
let mk_term = () =>
  Term.create(~dispose=false, ~nosig=true, ~mouse=true, ~bpaste=true, ());

let install_sigterm = (release: unit => unit) =>
  try(
    Sys.set_signal(
      Sys.sigterm,
      Signal_handle(
        _ => {
          release();
          exit(143);
        },
      ),
    )
  ) {
  | _ => ()
  };

/* Echo translated input events; for developing/debugging the input
   layer. Run as: ./hazel-tui --keys-debug */
let keys_debug = () => {
  let term = mk_term();
  install_sigterm(() => Term.release(term));
  let lines = ref(["keys-debug: press keys; Ctrl+C to exit"]);
  let show = () =>
    Term.image(
      term,
      lines^
      |> List.rev
      /* I.string rejects control chars; show_event output is multi-line */
      |> List.map(l =>
           Notty.I.string(
             Notty.A.empty,
             String.map(c => c == '\n' ? ' ' : c, l),
           )
         )
      |> Notty.I.vcat,
    );
  let st = ref(NottyEvents.init);
  show();
  let quit = ref(false);
  while (! quit^) {
    switch (Term.event(term)) {
    | `End => quit := true
    | `Resize(_) => show()
    | #Notty.Unescape.event as ev =>
      let (st', events) = NottyEvents.translate(st^, ev);
      st := st';
      List.iter(
        ev => {
          let mapped =
            switch (Keymap.handle(ev)) {
            | Some(Quit) =>
              quit := true;
              "Quit";
            | Some(a) => Keymap.show(a)
            | None => "(unmapped)"
            };
          lines :=
            [AnsiInput.show_event(ev) ++ "  ->  " ++ mapped, ...lines^];
        },
        events,
      );
      show();
    };
  };
  Term.release(term);
};

let run = (file: option(string)) => {
  let term = mk_term();
  let release = () => Term.release(term);
  install_sigterm(release);
  let (input_fd, _) = Term.fds(term);
  let model = ref(App.init(file));
  let events_st = ref(NottyEvents.init);
  /* absolute-time deadline for the debounced eval */
  let eval_at: ref(option(float)) = ref(None);
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
    let (cols, rows) = Term.size(term);
    let (frame, m) = App.render(~size=(rows, cols), model^);
    model := m;
    NottyIO.render(term, frame);
  };

  let handle_events = (events: list(AnsiInput.event)): bool => {
    let quit = ref(false);
    List.iter(
      ev =>
        switch (Keymap.handle(ev)) {
        | None => ()
        | Some(action) =>
          let (cols, rows) = Term.size(term);
          let page = App.editor_height(~size=(rows, cols), model^);
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

  /* one notty event (+ any buffered behind it) -> app actions */
  let pump = (): bool => {
    let quit = ref(false);
    let step = () =>
      switch (Term.event(term)) {
      | `End => quit := true
      | `Resize(_) => render()
      | #Notty.Unescape.event as ev =>
        let (st', events) = NottyEvents.translate(events_st^, ev);
        events_st := st';
        if (handle_events(events)) {
          quit := true;
        };
      };
    step();
    while (! quit^ && Term.pending(term)) {
      step();
    };
    quit^;
  };

  let quit = ref(false);
  render();
  while (! quit^) {
    let now = Unix.gettimeofday();
    /* fire the eval debounce if due */
    switch (eval_at^) {
    | Some(t) when now >= t =>
      eval_at := None;
      if (model^.result == ResultView.Pending) {
        kill_worker();
        worker := Some(EvalWorker.start(model^.statics));
      };
    | _ => ()
    };
    let extra =
      switch (worker^) {
      | Some(w) => [w.fd]
      | None => []
      };
    let t =
      switch (eval_at^) {
      | Some(t) => max(0.0, t -. Unix.gettimeofday())
      | None => (-1.0)
      };
    switch (Unix.select([input_fd, ...extra], [], [], t)) {
    | exception (Unix.Unix_error(EINTR, _, _)) => () /* e.g. SIGWINCH */
    | ([], _, _) => () /* timeout; loop fires timers */
    | (ready, _, _) =>
      if (List.mem(input_fd, ready)) {
        if (pump()) {
          quit := true;
        };
      } else {
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
        };
      }
    };
  };
  kill_worker();
  release();
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
  let tty = () => Unix.isatty(Unix.stdin) && Unix.isatty(Unix.stdout);
  switch (args) {
  | ["--keys-debug", ..._] => keys_debug()
  | ["--replay", keys] => print_endline(Replay.run(keys))
  | ["--replay", keys, path] =>
    print_endline(Replay.run(~file=Some(path), keys))
  | [] when tty() => run(None)
  | [path] when tty() => run(Some(path))
  | []
  | [_] =>
    print_endline("hazel-tui: stdin/stdout is not a terminal");
    exit(1);
  | _ => usage()
  };
};
