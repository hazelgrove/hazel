open Js_of_ocaml;

/* Bindings to the node.js terminal APIs (process.stdin/stdout). The TUI
   runs as a js_of_ocaml executable under node (like src/CLI), so terminal
   control goes through node rather than Unix termios. */

let process = Js.Unsafe.global##.process;

let is_tty = (): bool =>
  Js.to_bool(Js.Unsafe.coerce(process)##.stdout##.isTTY);

/* (rows, cols); falls back to 24x80 when stdout is not a terminal */
let size = (): (int, int) => {
  let stdout = Js.Unsafe.coerce(process)##.stdout;
  let rows = Js.Optdef.to_option(Js.Unsafe.coerce(stdout)##.rows);
  let cols = Js.Optdef.to_option(Js.Unsafe.coerce(stdout)##.columns);
  (Option.value(rows, ~default=24), Option.value(cols, ~default=80));
};

let write = (s: string): unit => {
  let _: bool =
    Js.Unsafe.meth_call(
      Js.Unsafe.coerce(process)##.stdout,
      "write",
      [|Js.Unsafe.inject(Js.string(s))|],
    );
  ();
};

let on_data = (f: string => unit): unit => {
  let stdin = Js.Unsafe.coerce(process)##.stdin;
  let _ =
    Js.Unsafe.meth_call(
      stdin,
      "setEncoding",
      [|Js.Unsafe.inject(Js.string("utf8"))|],
    );
  let _ =
    Js.Unsafe.meth_call(
      stdin,
      "on",
      [|
        Js.Unsafe.inject(Js.string("data")),
        Js.Unsafe.inject(
          Js.wrap_callback((chunk: Js.t(Js.js_string)) =>
            f(Js.to_string(chunk))
          ),
        ),
      |],
    );
  ();
};

let on_resize = (f: unit => unit): unit => {
  let _ =
    Js.Unsafe.meth_call(
      Js.Unsafe.coerce(process)##.stdout,
      "on",
      [|
        Js.Unsafe.inject(Js.string("resize")),
        Js.Unsafe.inject(Js.wrap_callback(() => f())),
      |],
    );
  ();
};

/* Node's global setTimeout. Do NOT use Dom_html.window##setTimeout: the
   CLI polyfill's fake `window` (preloaded so the web library initializes
   under node) has no setTimeout. Returns the timer handle for clearing. */
let set_timeout = (f: unit => unit, ms: float): Js.Unsafe.any =>
  Js.Unsafe.fun_call(
    Js.Unsafe.global##.setTimeout,
    [|Js.Unsafe.inject(Js.wrap_callback(() => f())), Js.Unsafe.inject(ms)|],
  );

let clear_timeout = (handle: Js.Unsafe.any): unit =>
  Js.Unsafe.fun_call(Js.Unsafe.global##.clearTimeout, [|handle|]);

let set_raw_mode = (b: bool): unit => {
  let stdin = Js.Unsafe.coerce(process)##.stdin;
  if (Js.to_bool(stdin##.isTTY)) {
    let _ =
      Js.Unsafe.meth_call(
        stdin,
        "setRawMode",
        [|Js.Unsafe.inject(Js.bool(b))|],
      );
    ();
  };
};

let resume_stdin = (): unit => {
  let _ =
    Js.Unsafe.meth_call(Js.Unsafe.coerce(process)##.stdin, "resume", [||]);
  ();
};

let pause_stdin = (): unit => {
  let _ =
    Js.Unsafe.meth_call(Js.Unsafe.coerce(process)##.stdin, "pause", [||]);
  ();
};

let exit = (code: int): unit => {
  let _ = Js.Unsafe.meth_call(process, "exit", [|Js.Unsafe.inject(code)|]);
  ();
};

/* Terminal control sequences */
let alt_screen_on = "\x1b[?1049h";
let alt_screen_off = "\x1b[?1049l";
let cursor_show = "\x1b[?25h";
let bracketed_paste_on = "\x1b[?2004h";
let bracketed_paste_off = "\x1b[?2004l";
/* autowrap off so an over-wide span can't push the grid out of shape */
let autowrap_off = "\x1b[?7l";
let autowrap_on = "\x1b[?7h";
/* button-event mouse tracking (press/release/drag) + SGR encoding */
let mouse_on = "\x1b[?1002h\x1b[?1006h";
let mouse_off = "\x1b[?1006l\x1b[?1002l";
let sgr_reset = "\x1b[0m";

let entered = ref(false);

/* Enter raw mode + alternate screen. */
let enter = (): unit =>
  if (! entered^) {
    entered := true;
    set_raw_mode(true);
    resume_stdin();
    write(alt_screen_on ++ bracketed_paste_on ++ autowrap_off ++ mouse_on);
  };

/* Restore the user's terminal. Idempotent; must run before any exit path
   (including crashes) or the shell is left in raw/alt-screen mode. */
let leave = (): unit =>
  if (entered^) {
    entered := false;
    write(
      mouse_off
      ++ autowrap_on
      ++ bracketed_paste_off
      ++ sgr_reset
      ++ cursor_show
      ++ alt_screen_off,
    );
    set_raw_mode(false);
    pause_stdin();
  };

let on_process_event = (name: string, f: unit => unit): unit => {
  let _ =
    Js.Unsafe.meth_call(
      process,
      "on",
      [|
        Js.Unsafe.inject(Js.string(name)),
        Js.Unsafe.inject(Js.wrap_callback((_: Js.Unsafe.any) => f())),
      |],
    );
  ();
};

let install_exit_guards = (): unit => {
  on_process_event("exit", leave);
  on_process_event("SIGTERM", () => {
    leave();
    exit(143);
  });
  /* Restore the terminal before node prints the stack trace, then die. */
  let _ =
    Js.Unsafe.meth_call(
      process,
      "on",
      [|
        Js.Unsafe.inject(Js.string("uncaughtException")),
        Js.Unsafe.inject(
          Js.wrap_callback((err: Js.Unsafe.any) => {
            leave();
            let _ =
              Js.Unsafe.meth_call(
                Js.Unsafe.global##.console,
                "error",
                [|err|],
              );
            exit(1);
          }),
        ),
      |],
    );
  ();
};
