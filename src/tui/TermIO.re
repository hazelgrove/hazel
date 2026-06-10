/* Native terminal control (raw mode via Unix termios, ANSI escapes on
   stdout) — the native replacement for the node-hosted NodeTerm layer.
   The TUI runs as a plain native executable; no JS runtime involved. */

let is_tty = (): bool => Unix.isatty(Unix.stdin) && Unix.isatty(Unix.stdout);

let write = (s: string): unit => {
  print_string(s);
  flush(stdout);
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

/* === window size === */

/* There is no ioctl(TIOCGWINSZ) binding in the stdlib Unix module, so
   query via stty (against the controlling terminal, which works in raw
   mode) and cache; SIGWINCH invalidates the cache. */
let query_size = (): (int, int) =>
  switch (Unix.open_process_in("stty size 2>/dev/null </dev/tty")) {
  | ic =>
    let line =
      switch (input_line(ic)) {
      | line => line
      | exception _ => ""
      };
    let _ = Unix.close_process_in(ic);
    switch (String.split_on_char(' ', String.trim(line))) {
    | [r, c] =>
      switch (int_of_string_opt(r), int_of_string_opt(c)) {
      | (Some(rows), Some(cols)) when rows > 0 && cols > 0 => (rows, cols)
      | _ => (24, 80)
      }
    | _ => (24, 80)
    };
  | exception _ => (24, 80)
  };

let cached_size: ref((int, int)) = ref((24, 80));

let size = (): (int, int) => cached_size^;

let refresh_size = (): unit => cached_size := query_size();

/* SIGWINCH (28 on both macOS and Linux): set a flag; the main loop's
   select gets interrupted (EINTR) and re-renders. */
let resized: ref(bool) = ref(false);

let sigwinch = 28;

let install_winch_handler = (): unit =>
  try(Sys.set_signal(sigwinch, Signal_handle(_ => resized := true))) {
  | _ => () /* unsupported platform: live without resize events */
  };

/* === raw mode + alternate screen === */

let saved_termios: ref(option(Unix.terminal_io)) = ref(None);
let entered = ref(false);

let enter = (): unit =>
  if (! entered^) {
    entered := true;
    let tio = Unix.tcgetattr(Unix.stdin);
    saved_termios := Some(tio);
    /* raw-enough: no line buffering, no echo, no signal keys (Ctrl+C
       arrives as byte 0x03 and is a TUI binding), no XON/XOFF (frees
       Ctrl+S for Save) */
    Unix.tcsetattr(
      Unix.stdin,
      TCSANOW,
      {
        ...tio,
        c_icanon: false,
        c_echo: false,
        c_isig: false,
        c_ixon: false,
        c_vmin: 1,
        c_vtime: 0,
      },
    );
    write(alt_screen_on ++ bracketed_paste_on ++ autowrap_off ++ mouse_on);
    refresh_size();
  };

/* Restore the user's terminal. Idempotent; must run on every exit path
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
    switch (saved_termios^) {
    | Some(tio) =>
      try(Unix.tcsetattr(Unix.stdin, TCSANOW, tio)) {
      | _ => ()
      }
    | None => ()
    };
  };

let install_exit_guards = (): unit => {
  at_exit(leave);
  /* SIGTERM: restore terminal, then die */
  try(
    Sys.set_signal(
      Sys.sigterm,
      Signal_handle(
        _ => {
          leave();
          exit(143);
        },
      ),
    )
  ) {
  | _ => ()
  };
};

/* Blocking wait for input with an optional timeout (seconds).
   Returns the bytes read ("" on timeout), or None on EOF. */
let read_input = (~timeout: option(float)): option(string) => {
  let t =
    switch (timeout) {
    | Some(t) => max(0.0, t)
    | None => (-1.0)
    };
  switch (Unix.select([Unix.stdin], [], [], t)) {
  | exception (Unix.Unix_error(EINTR, _, _)) => Some("") /* signal (e.g. WINCH) */
  | ([], _, _) => Some("") /* timeout */
  | (_, _, _) =>
    let buf = Bytes.create(4096);
    switch (Unix.read(Unix.stdin, buf, 0, 4096)) {
    | 0 => None /* EOF */
    | n => Some(Bytes.sub_string(buf, 0, n))
    | exception (Unix.Unix_error(EINTR, _, _)) => Some("")
    };
  };
};
