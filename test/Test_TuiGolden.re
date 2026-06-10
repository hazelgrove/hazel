open Alcotest;
open Haz3ltui;

/* Golden tests for the TUI: drive scripted key sequences through the
   same input-parse/update/render path as the interactive loop (via
   Replay) and check the resulting plain-text frame or buffer text. */

let small = (10, 40); /* rows, cols */

let frame = (keys: string): string => Replay.run(~size=small, keys);

let buffer = (keys: string): string => Replay.buffer_text(~size=small, keys);

let check_frame = (msg, expected, keys) =>
  check(string, msg, expected, frame(keys));

let check_buffer = (msg, expected, keys) =>
  check(string, msg, expected, buffer(keys));

let tests = (
  "TuiGolden",
  [
    test_case("simple arithmetic with result", `Quick, () =>
      check_frame(
        "frame",
        "1 1 + 2\n"
        ++ "\n"
        ++ "\n"
        ++ "\n"
        ++ "\n"
        ++ "\n"
        ++ "\n"
        ++ "── result ──────────────────────────────\n"
        ++ "3\n"
        ++ " [scratch] *  1:6  Number literal : Int",
        "1 + 2",
      )
    ),
    test_case("incomplete expression renders a hole", `Quick, () =>
      check_buffer("buffer", "1 + \xc2\xbf", "1 + ")
    ),
    test_case("let with linebreak", `Quick, () =>
      check_buffer("buffer", "let x = 5 in\nx + 3", "let x = 5 in\rx + 3")
    ),
    test_case("backspace deletes", `Quick, () =>
      check_buffer("buffer", "12", "123\x7f")
    ),
    test_case("undo restores deleted text", `Quick, () =>
      check_buffer("buffer", "123", "123\x7f\x1a")
    ),
    test_case("redo after undo", `Quick, () =>
      check_buffer("buffer", "12", "123\x7f\x1a\x19")
    ),
    test_case("shift-select then destruct removes selection", `Quick, () =>
      check_buffer("buffer", "1 ", "1 + 2\x1b[1;2D\x1b[1;2D\x1b[1;2D\x7f")
    ),
    test_case("select-all then type replaces program", `Quick, () =>
      check_buffer("buffer", "9", "1 + 2\x01\x7f9")
    ),
    test_case("paste inserts as a unit", `Quick, () =>
      check_buffer(
        "buffer",
        "let y = 7 in y",
        "\x1b[200~let y = 7 in y\x1b[201~",
      )
    ),
    test_case(
      "evaluation error is reported, not fatal",
      `Quick,
      () => {
        /* 2 + true is ill-typed; indet result still renders a frame */
        let f = frame("2 + true");
        check(
          bool,
          "frame mentions result pane",
          true,
          Util.StringUtil.match(Util.StringUtil.regexp("result"), f),
        );
      },
    ),
  ],
);
