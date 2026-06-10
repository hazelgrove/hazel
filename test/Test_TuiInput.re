open Alcotest;
open Haz3ltui;
open AnsiInput;

/* Tests for the TUI's ANSI input parser (bytes -> key events). */

let show_events = (evs: list(event)): string =>
  evs |> List.map(show_event) |> String.concat("; ");

let parse_all = (s: string): list(event) => {
  let (_, events) = parse(init, s);
  events;
};

let check_events = (msg, expected: list(event), input: string) =>
  check(string, msg, show_events(expected), show_events(parse_all(input)));

let tests = (
  "TuiInput",
  [
    test_case("printable chars become editor keys", `Quick, () =>
      check_events(
        "abc",
        [Editor(mk("a")), Editor(mk("b")), Editor(mk("c"))],
        "abc",
      )
    ),
    test_case("multi-byte utf8 char is one key", `Quick, () =>
      check_events("lambda", [Editor(mk("\xce\xbb"))], "\xce\xbb")
    ),
    test_case("csi arrows", `Quick, () =>
      check_events(
        "up down right left",
        [
          Editor(mk("ArrowUp")),
          Editor(mk("ArrowDown")),
          Editor(mk("ArrowRight")),
          Editor(mk("ArrowLeft")),
        ],
        "\x1b[A\x1b[B\x1b[C\x1b[D",
      )
    ),
    test_case("ss3 application-mode arrows", `Quick, () =>
      check_events(
        "up home",
        [Editor(mk("ArrowUp")), Editor(mk("Home"))],
        "\x1bOA\x1bOH",
      )
    ),
    test_case("modified arrows: shift, ctrl, ctrl+shift", `Quick, () =>
      check_events(
        "mods",
        [
          Editor(mk(~shift=true, "ArrowRight")),
          Editor(mk(~ctrl=true, "ArrowLeft")),
          Editor(mk(~shift=true, ~ctrl=true, "ArrowUp")),
        ],
        "\x1b[1;2C\x1b[1;5D\x1b[1;6A",
      )
    ),
    test_case("home and end variants", `Quick, () =>
      check_events(
        "home/end",
        [
          Editor(mk("Home")),
          Editor(mk("End")),
          Editor(mk("Home")),
          Editor(mk("End")),
        ],
        "\x1b[H\x1b[F\x1b[1~\x1b[4~",
      )
    ),
    test_case("delete and shift+delete", `Quick, () =>
      check_events(
        "delete",
        [Editor(mk("Delete")), Editor(mk(~shift=true, "Delete"))],
        "\x1b[3~\x1b[3;2~",
      )
    ),
    test_case("page up / page down", `Quick, () =>
      check_events(
        "paging",
        [Tui(PageUp), Tui(PageDown)],
        "\x1b[5~\x1b[6~",
      )
    ),
    test_case("reserved ctrl chords", `Quick, () =>
      check_events(
        "ctrl chords",
        [
          Tui(Quit),
          Tui(Save),
          Tui(Undo),
          Tui(Redo),
          Tui(ToggleResultPane),
          Tui(Quit),
        ],
        "\x03\x13\x1a\x19\x12\x11",
      )
    ),
    test_case(
      "other ctrl letters pass through to the editor keymap", `Quick, () =>
      check_events(
        "ctrl+a ctrl+d",
        [Editor(mk(~ctrl=true, "a")), Editor(mk(~ctrl=true, "d"))],
        "\x01\x04",
      )
    ),
    test_case("backspace enter tab shift-tab", `Quick, () =>
      check_events(
        "edit keys",
        [
          Editor(mk("Backspace")),
          Editor(mk("Enter")),
          Tui(TabKey),
          Tui(ShiftTab),
        ],
        "\x7f\r\t\x1b[Z",
      )
    ),
    test_case("alt+char", `Quick, () =>
      check_events("alt+p", [Editor(mk(~alt=true, "p"))], "\x1bp")
    ),
    test_case(
      "escape sequence split across chunks",
      `Quick,
      () => {
        let (st, evs1) = parse(init, "\x1b[1;");
        let (st, evs2) = parse(st, "2C");
        check(
          string,
          "first chunk yields nothing",
          show_events([]),
          show_events(evs1),
        );
        check(
          string,
          "second chunk completes shift+right",
          show_events([Editor(mk(~shift=true, "ArrowRight"))]),
          show_events(evs2),
        );
        check(string, "no pending input remains", "", st.pending);
      },
    ),
    test_case(
      "lone escape resolves to Escape key on flush",
      `Quick,
      () => {
        let (st, evs) = parse(init, "\x1b");
        check(string, "no event yet", show_events([]), show_events(evs));
        let (st, evs) = flush(st);
        check(
          string,
          "flush yields Escape",
          show_events([Editor(mk("Escape"))]),
          show_events(evs),
        );
        check(string, "pending cleared", "", st.pending);
      },
    ),
    test_case(
      "flush keeps a split sequence pending",
      `Quick,
      () => {
        let (st, _) = parse(init, "\x1b[1;");
        let (st, evs) = flush(st);
        check(string, "no event", show_events([]), show_events(evs));
        check(string, "pending kept", "\x1b[1;", st.pending);
      },
    ),
    test_case("bracketed paste", `Quick, () =>
      check_events(
        "paste",
        [PasteText("let x = 5 in x")],
        "\x1b[200~let x = 5 in x\x1b[201~",
      )
    ),
    test_case(
      "bracketed paste split across chunks",
      `Quick,
      () => {
        let (st, evs1) = parse(init, "\x1b[200~let x");
        let (st, evs2) = parse(st, " = 5\x1b[201~b");
        check(
          string,
          "paste waits for terminator",
          show_events([]),
          show_events(evs1),
        );
        check(
          string,
          "paste completes plus trailing key",
          show_events([PasteText("let x = 5"), Editor(mk("b"))]),
          show_events(evs2),
        );
        check(string, "no pending input remains", "", st.pending);
      },
    ),
    test_case(
      "keymap: tui-level bindings and web keymap delegation",
      `Quick,
      () => {
        Util.Os.is_mac := false;
        let key = (ev: event): option(Keymap.t) => Keymap.handle(ev);
        check(
          string,
          "ctrl+a is select all",
          "(Perform (Select All))",
          switch (key(Editor(mk(~ctrl=true, "a")))) {
          | Some(a) => Keymap.show(a)
          | None => "(none)"
          },
        );
        check(
          string,
          "alt+p is pretty print",
          "(Perform PrettyPrint)",
          switch (key(Editor(mk(~alt=true, "p")))) {
          | Some(a) => Keymap.show(a)
          | None => "(none)"
          },
        );
      },
    ),
  ],
);
