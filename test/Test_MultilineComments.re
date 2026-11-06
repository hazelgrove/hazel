open Alcotest;
open Util;
open Haz3lcore;
open Action;

let parse = text => Parser.to_zipper(~root=Exp, text) |> Option.get;
let measure = z =>
  Measured.of_segment(
    Zipper.unselect_and_zip(z),
    Id.Map.empty,
    Id.Map.empty,
  );
let point = z => Zipper.Caret.point(measure(z), z);
let at = (text, row, col) => {
  let z = parse(text);
  Move.to_point(
    ~measured=measure(z),
    ~goal=
      Point.{
        row,
        col,
      },
    z,
  )
  |> Option.value(~default=z);
};
let perform = Test_Editing.perform;
let printed = z => Printer.of_zipper(~holes="", z);
let check_point = (row, col, z) => {
  let p = point(z);
  check(pair(int, int), "caret row/column", (row, col), (p.row, p.col));
};
let run = (name, f) => test_case(name, `Quick, f);
let tests = (
  "MultilineComments",
  [
    run("parse, print and measure blank and indented lines", () => {
      let text = "let x = 1 in\n  # first\n\n    last # x";
      let z = parse(text);
      check(string, "verbatim", text, printed(z));
      check(int, "rows", 4, Measured.total_rows(measure(z)));
      check_point(3, 12, z);
    }),
    run("all comment positions support clicking and horizontal movement", () => {
      let text = "  # ab\n\n    日本😀 z # 1";
      let z = at(text, 0, 2);
      let expected = [
        (0, 3),
        (0, 4),
        (0, 5),
        (0, 6),
        (1, 0),
        (2, 0),
        (2, 1),
        (2, 2),
        (2, 3),
        (2, 4),
        (2, 6),
        (2, 8),
        (2, 10),
        (2, 11),
        (2, 12),
        (2, 13),
        (2, 14),
      ];
      let _ =
        List.fold_left(
          (z, (row, col)) => {
            let z = Move.by_char(Right, z) |> Option.get;
            check_point(row, col, z);
            check_point(row, col, at(text, row, col));
            z;
          },
          z,
          expected,
        );
      ();
    }),
    run("Enter retains indentation and Backspace joins lines", () => {
      let z = at("  # abc# 1", 0, 7) |> perform(_, [Insert("\n")]);
      check(string, "new line", "  # abc\n  # 1", printed(z));
      check_point(1, 2, z);
      let z = perform(z, [Insert("d"), Destruct(Local(Left, ByChar))]);
      check_point(1, 2, z);
    }),
    run("paste preserves whitespace and linebreaks inside a comment", () => {
      let z = at("# ab# 1", 0, 2) |> perform(_, [Paste("x\n  y\n\nz")]);
      check(string, "paste", "# x\n  y\n\nzab# 1", printed(z));
      check_point(3, 1, z);
    }),
    run("clipboard preprocessing preserves nested comment indentation", () => {
      let text = "let f = fun x ->\n  # first\n    中🙂\n  \n  last #\n  x\nin f(2)";
      let prepared = Parser.prepare_clipboard(parse(""), text);
      check(
        string,
        "code indentation only",
        "let f = fun x ->\n# first\n    中🙂\n  \n  last #\nx\nin f(2)",
        prepared,
      );
      let z = perform(parse(""), [Paste(prepared)]);
      check(
        bool,
        "body whitespace retained",
        true,
        StringUtil.plain_search(
          "# first\n    中🙂\n  \n  last #",
          printed(z),
          0,
        )
        >= 0,
      );
    }),
    run("clipboard fragments inside comments keep leading spaces", () => {
      let z = at("# a# 1", 0, 2);
      let text = "  x\n    y\n  ";
      check(
        string,
        "literal fragment",
        text,
        Parser.prepare_clipboard(z, text),
      );
    }),
    run("source indentation ignores hashes in strings and labels", () => {
      check(
        string,
        "quoted hashes and Windows newlines",
        "let x = \"#\" in\n`#`\n# a\n  b #\nx",
        Token.trim_leading_code_whitespace(
          "  let x = \"#\" in\r\n  `#`\r\n  # a\r\n  b #\r\n  x",
        ),
      )
    }),
    run("segment cache distinguishes comment indentation", () => {
      let original = "# a\n  b # 1";
      let changed = "# a\n    b # 1";
      Parser.set_segment_cache(
        Some(Zipper.unselect_and_zip(parse(original))),
        original,
      );
      let cached = Parser.try_segment_paste(changed, parse(""), ~root=Exp);
      check(
        bool,
        "different body must reparse",
        true,
        Option.is_none(cached),
      );
    }),
    run("clicking after a nested comment selection restores its token", () => {
      let text = "let f = fun x ->\n  # first comment\n    a long continuation\n\n    日本語 🙂 café\n  #\n  let y =\n    # short\n    #\n    x + 1\n  in y\nin f(4)";
      let z = at(text, 2, 10);
      let z =
        Select.to_point(
          ~measured=measure(z),
          ~goal=
            Point.{
              row: 4,
              col: 13,
            },
          ~chunkiness=ByChar,
          z,
        )
        |> Option.get;
      let z =
        perform(
          z,
          [AdjustIndent(Right, Always), AdjustIndent(Left, Always)],
        );
      let z =
        perform(
          z,
          [
            Move(
              Point(
                Point.{
                  row: 2,
                  col: 13,
                },
                None,
              ),
            ),
          ],
        );
      check_point(2, 13, z);
      check(string, "comment unchanged", text, printed(z));
    }),
    run("clicking beyond blank and short comment lines clamps to the row", () => {
      let text = "1 # first\n  longer continuation\n\n  短🙂\n#\n2 # another\n  comment #";
      List.iter(
        extra_rows => {
          let initial = parse(text);
          let segment = Zipper.unselect_and_zip(initial);
          let measured =
            Measured.of_segment(
              segment,
              Id.Map.empty,
              Id.Map.singleton(Piece.id(List.hd(segment)), extra_rows),
            );
          List.iter(
            ((row, col)) => {
              let z =
                Move.to_point(
                  ~measured,
                  ~goal=
                    Point.{
                      row: row + extra_rows,
                      col: 80,
                    },
                  initial,
                )
                |> Option.get;
              let p = Zipper.Caret.point(measured, z);
              check(
                pair(int, int),
                "nearest position on clicked row",
                (row + extra_rows, col),
                (p.row, p.col),
              );
              check(string, "text unchanged", printed(initial), printed(z));
            },
            [(1, 21), (2, 0), (3, 6), (4, 1)],
          );
        },
        [0, 1, 2],
      );
    }),
    run("Home/End stop at internal comment linebreaks", () => {
      let z = at("# first\n  second\nthird # 1", 1, 4);
      check_point(1, 0, perform(z, [Move(Line(Left))]));
      check_point(1, 8, perform(z, [Move(Line(Right))]));
    }),
    run("vertical movement stays within comment", () => {
      let z = at("# first\n second\n third # 1", 0, 3);
      let z =
        Move.vertical(
          ~measured=measure(z),
          ~col_target=3,
          ~refractor_rows=Id.Map.empty,
          Down,
          z,
        )
        |> Option.get;
      check_point(1, 3, z);
    }),
    run("multiline selection highlights and deletes exact characters", () => {
      let z = at("# first\n  second\nthird # 1", 0, 3);
      let z =
        Select.to_point(
          ~measured=measure(z),
          ~goal=
            Point.{
              row: 1,
              col: 5,
            },
          ~chunkiness=ByChar,
          z,
        )
        |> Option.get;
      check(string, "copied text", "irst\n  sec", Insert.selected_text(z));
      let rows =
        Web.Highlight.rows_of_segment(
          ~measured=measure(z),
          ~shape_map=Id.Map.empty,
          ~shape_init=None,
          z.selection.content,
        )
        |> List.map(((m, tips)) => Web.Highlight.row_data_of(m, tips));
      let rows =
        Web.Highlight.clip_char_selection(~measured=measure(z), z, rows);
      check(
        list(triple(int, int, int)),
        "highlight extents",
        [(0, 3, 7), (1, 0, 5)],
        List.map(
          (r: Web.Highlight.row_data) =>
            (r.row_num, r.left_col, r.right_col),
          rows,
        ),
      );
      let z = perform(z, [Destruct(Local(Left, ByChar))]);
      check(string, "deleted", "# fond\nthird # 1", printed(z));
    }),
    run("wrap multiline selection as one block", () => {
      let z = parse("1 +\n2") |> Select.all |> perform(_, [Insert("#")]);
      check(string, "block", "#1 +\n2#", printed(z));
    }),
    run("indent and dedent the current comment row only", () => {
      let text = "  # first\n  second\n    third # 1";
      let z = at(text, 1, 5) |> perform(_, [AdjustIndent(Right, Always)]);
      check(
        string,
        "indent",
        "  # first\n    second\n    third # 1",
        printed(z),
      );
      check_point(1, 7, z);
      let z = perform(z, [AdjustIndent(Left, Always)]);
      check(string, "dedent", text, printed(z));
      check_point(1, 5, z);
    }),
    run("indent a selection across comment and code rows", () => {
      let text = "# a\n  b #\n1";
      let z =
        parse(text)
        |> Select.all
        |> perform(_, [AdjustIndent(Right, Always)]);
      check(string, "indent all", "  # a\n    b #\n  1", printed(z));
      let z = perform(z, [AdjustIndent(Left, Always)]);
      check(string, "dedent all", text, printed(z));
    }),
    run("whole-block indentation preserves list layout and blank lines", () => {
      let text = "# Notes\n  - first\n    detail\n\n  - second\n#";
      let z =
        parse(text)
        |> Select.all
        |> perform(_, [AdjustIndent(Right, Always)]);
      check(
        string,
        "relative indentation",
        "  # Notes\n    - first\n      detail\n  \n    - second\n  #",
        printed(z),
      );
      let z = perform(z, [AdjustIndent(Left, Always)]);
      check(string, "exact reversal including blank line", text, printed(z));
    }),
    run("indent keeps a partial selection inside a comment", () => {
      let text = "# a\n  second\nthird # 1";
      let z = at(text, 1, 4);
      let z =
        Select.to_point(
          ~measured=measure(z),
          ~goal=
            Point.{
              row: 2,
              col: 3,
            },
          z,
        )
        |> Option.get;
      let z = perform(z, [AdjustIndent(Right, Always)]);
      check(
        string,
        "indent rows",
        "# a\n    second\n  third # 1",
        printed(z),
      );
      check_point(2, 5, z);
      check(string, "selection", "cond\n  thi", Insert.selected_text(z));
    }),
    run("toggle from an interior row unwraps the entire comment", () => {
      let z = at("#1 +\n2#", 1, 1) |> perform(_, [ToggleLineComment]);
      check(string, "uncommented", "1 +\n2", printed(z));
    }),
    run("joining an internal newline preserves the closing delimiter", () => {
      let z =
        at("#first\nsecond# 1", 1, 0)
        |> perform(_, [Destruct(Local(Left, ByChar))]);
      check(string, "joined", "#firstsecond# 1", printed(z));
      check_point(0, 6, z);
    }),
    run("selection entirely on a continuation line clips earlier rows", () => {
      let z = at("# first\n  second\nthird # 1", 1, 3);
      let z =
        Select.to_point(
          ~measured=measure(z),
          ~goal=
            Point.{
              row: 1,
              col: 6,
            },
          z,
        )
        |> Option.get;
      let rows =
        Web.Highlight.rows_of_segment(
          ~measured=measure(z),
          ~shape_map=Id.Map.empty,
          ~shape_init=None,
          z.selection.content,
        )
        |> List.map(((m, tips)) => Web.Highlight.row_data_of(m, tips))
        |> Web.Highlight.clip_char_selection(~measured=measure(z), z);
      check(
        list(triple(int, int, int)),
        "single row",
        [(1, 3, 6)],
        List.map(
          (r: Web.Highlight.row_data) =>
            (r.row_num, r.left_col, r.right_col),
          rows,
        ),
      );
    }),
    run("backspace reverses Enter's comment indent in one action", () => {
      let settings = {
        ...Test_Editing.default_settings,
        indentation_ux: true,
      };
      let text = "  # first# 1";
      let z = at(text, 0, 9) |> perform(~settings, _, [Insert("\n")]);
      check_point(1, 2, z);
      let z = perform(~settings, z, [Destruct(Local(Left, ByChar))]);
      check(string, "undo newline", text, printed(z));
      check_point(0, 9, z);
    }),
    run("shift backspace dedents a comment continuation boundary", () => {
      let z =
        at("# first\n  second# 1", 1, 2)
        |> perform(_, [AdjustIndent(Left, AtBoundary)]);
      check(string, "dedented", "# first\nsecond# 1", printed(z));
      check_point(1, 0, z);
    }),
    run("CRLF paste uses the same normalization inside a comment", () => {
      let z = parse("") |> perform(_, [Paste("# first\r\nsecond # 1")]);
      check(string, "LF", "# first\nsecond # 1", printed(z));
    }),
    run("lexer tracks line and column after multiline comments", () => {
      let lexbuf = Lexing.from_string("# first\r\nsecond\nlast # 1");
      MenhirParser.Lexer.reset_delims();
      let _ = MenhirParser.Lexer.token(lexbuf);
      let p = Lexing.lexeme_start_p(lexbuf);
      check(
        pair(int, int),
        "token position",
        (3, 7),
        (p.pos_lnum, p.pos_cnum - p.pos_bol),
      );
    }),
    run(
      "a newline after the closing delimiter remains outside the comment", () => {
      check(
        bool,
        "no trailing newline token",
        false,
        Token.is_comment("# closed #\n"),
      );
      let z = parse("# closed #\n1");
      let has_linebreak =
        List.exists(Piece.is_linebreak, Zipper.unselect_and_zip(z));
      check(bool, "separate newline", true, has_linebreak);
      check_point(1, 1, z);
    }),
    run("reserved projector rows precede a comment continuation", () => {
      let z = parse("1 # first\nsecond # + 2");
      let segment = Zipper.unselect_and_zip(z);
      let id = Piece.id(List.hd(segment));
      let measured =
        Measured.of_segment(segment, Id.Map.empty, Id.Map.singleton(id, 2));
      let z =
        Move.to_point(
          ~measured,
          ~goal=
            Point.{
              row: 3,
              col: 4,
            },
          z,
        )
        |> Option.get;
      let p = Zipper.Caret.point(measured, z);
      check(
        pair(int, int),
        "caret after reserved rows",
        (3, 4),
        (p.row, p.col),
      );
      check(int, "rendered rows", 4, Measured.total_rows(measured));
    }),
    Test_FastParse.verbatim(
      "fast parse keeps multiline comment",
      "let x = # a\n\n  b # 1 in x",
    ),
    Test_FastParse.semantic(
      "fast and typing parsers agree on multiline comment",
      "let x = # a\n\n  b # 1 in x",
    ),
  ],
);
