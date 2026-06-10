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

let show_ranges = (ranges: list(EditorView.col_range)): string =>
  ranges
  |> List.map((EditorView.{range_row, first, last}) =>
       Printf.sprintf("%d:%d-%d", range_row, first, last)
     )
  |> List.sort(compare)
  |> String.concat(" ");

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
      "type error underlines the inconsistent term's shards",
      `Quick,
      () => {
        let model = Replay.final_model(~size=small, "2 + true");
        check(
          string,
          "error ranges",
          "0:4-8", /* the `true` token */
          show_ranges(EditorView.error_ranges(model.statics, model.editor)),
        );
      },
    ),
    test_case(
      "well-typed program has no error ranges",
      `Quick,
      () => {
        let model = Replay.final_model(~size=small, "1 + 2");
        check(
          string,
          "error ranges",
          "",
          show_ranges(EditorView.error_ranges(model.statics, model.editor)),
        );
      },
    ),
    test_case("undercurl SGR emission", `Quick, () =>
      check(
        string,
        "sgr",
        "\x1b[0;38;5;167;4:3;58:5:167m",
        Style.sgr(Style.undercurl(167, Style.fg(167))),
      )
    ),
    test_case(
      "mouse click places the caret",
      `Quick,
      () => {
        /* "1 + 2" on row 0; gutter is 2 cols, so screen col 4 = buffer
           col 2 (just left of `+`) */
        let model = Replay.final_model(~size=small, "1 + 2\x1b[<0;5;1M");
        let caret = EditorView.caret_point(model.editor);
        check(int, "caret row", 0, caret.row);
        check(int, "caret col", 2, caret.col);
      },
    ),
    test_case(
      "double-click selects the clicked token",
      `Quick,
      () => {
        open Haz3lcore;
        let m1 = Replay.final_model(~size=small, "1 + 234");
        /* two rapid clicks on `234` (screen col 6 = buffer col 4) */
        let click =
          Keymap.Mouse(
            AnsiInput.Press(
              {
                row: 0,
                col: 6,
              },
              false,
            ),
          );
        let (m2, _) = App.apply(~now=1.0, ~page=8, m1, click);
        let (m3, _) = App.apply(~now=1.2, ~page=8, m2, click);
        check(
          bool,
          "selection non-empty after double click",
          false,
          Selection.is_empty(m3.editor.state.zipper.selection),
        );
        check(
          bool,
          "single click leaves selection empty",
          true,
          Selection.is_empty(m2.editor.state.zipper.selection),
        );
      },
    ),
    test_case(
      "shift-click selects from caret to point",
      `Quick,
      () => {
        open Haz3lcore;
        /* caret ends after `2`; shift-click back at buffer col 2 */
        let model = Replay.final_model(~size=small, "1 + 2\x1b[<4;3;1M");
        check(
          bool,
          "selection non-empty",
          false,
          Selection.is_empty(model.editor.state.zipper.selection),
        );
      },
    ),
    test_case(
      "wheel scroll detaches viewport from caret",
      `Quick,
      () => {
        let m1 = Replay.final_model(~size=small, "1 + 2");
        let (m2, _) =
          App.apply(~page=8, m1, Keymap.Mouse(AnsiInput.Wheel(3)));
        check(int, "row_off scrolled", 3, m2.row_off);
        check(bool, "free_scroll set", true, m2.free_scroll);
        /* a keyboard action re-attaches */
        let (m3, _) =
          App.apply(
            ~page=8,
            m2,
            Keymap.Perform(Haz3lcore.Action.Move(Local(Left, ByChar))),
          );
        check(bool, "free_scroll cleared", false, m3.free_scroll);
      },
    ),
    test_case(
      "inspector pane explains the error under the cursor",
      `Quick,
      () => {
        /* caret ends after `true`; Ctrl+T opens the inspector */
        let f = Replay.run(~size=(24, 60), "2 + true\x14");
        let has = re => Util.StringUtil.match(Util.StringUtil.regexp(re), f);
        check(bool, "inspector separator", true, has("inspector"));
        check(
          bool,
          "explains the inconsistency",
          true,
          has("Expecting type Int but got inconsistent type Bool"),
        );
        check(bool, "shows fixed type", true, has("type: Int"));
        /* Ctrl+T again closes it */
        let f2 = Replay.run(~size=(12, 60), "2 + true\x14\x14");
        check(
          bool,
          "toggles off",
          false,
          Util.StringUtil.match(Util.StringUtil.regexp("inspector"), f2),
        );
      },
    ),
    test_case(
      "backpack chip shows held shards near the caret",
      `Quick,
      () => {
        /* typing `(` leaves the `)` shard in the backpack */
        let f = frame("(1");
        let has = re => Util.StringUtil.match(Util.StringUtil.regexp(re), f);
        check(bool, "chip with held paren", true, has("\xe2\x87\xa7 \\)"));
        /* Tab puts it down: chip gone, buffer closed */
        let f2 = frame("(1\t");
        check(
          bool,
          "chip gone after put-down",
          false,
          Util.StringUtil.match(Util.StringUtil.regexp("\xe2\x87\xa7"), f2),
        );
        check_buffer("paren closed by tab", "(1)", "(1\t");
      },
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
