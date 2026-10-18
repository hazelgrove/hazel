open Alcotest;
open Haz3lcore;

let parse = Test_Editing.parse_with_caret;
let convert = z => {
  let elaborated = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  ProjectorPerform.newline_in_string(z, ~elaborated) |> Option.get;
};
let content = z =>
  switch (Zipper.generalized_neighbor(Right, z)) {
  | Some(Projector(pr)) =>
    check(bool, "text projector", true, pr.kind == TextArea);
    Piece.unparenthesize(pr.syntax)
    |> MakeTerm.for_projection
    |> Option.bind(_, TextAreaProj.string_of)
    |> Option.get;
  | _ => fail("Expected a text projector at the caret")
  };
let conversion = (name, source, expected, offset) =>
  test_case(
    name,
    `Quick,
    () => {
      let (z, _, actual_offset) = convert(parse(source));
      check(string, "text value", expected, content(z));
      check(int, "UTF-16 caret after newline", offset, actual_offset);
      /* A projected edit must survive the normal clipboard/persistence syntax. */
      let printed = Printer.of_zipper(~holes="", z);
      let reparsed = Parser.to_zipper(~root=Exp, printed) |> Option.get;
      check(
        string,
        "serialized round trip",
        printed,
        Printer.of_zipper(~holes="", reparsed),
      );
    },
  );

let join_roundtrip = (name, source) =>
  test_case(
    name,
    `Quick,
    () => {
      let original = parse(source);
      let (projected, id, offset) = convert(original);
      List.iter(
        ((direction, position)) => {
          let restored =
            ProjectorPerform.join_text_lines(
              projected,
              id,
              position,
              position,
              direction,
            )
            |> Option.get;
          check(
            string,
            "source and caret restored",
            Test_Editing.printer_indented(original),
            Test_Editing.printer_indented(restored),
          );
        },
        [(Util.Direction.Left, offset), (Right, offset - 1)],
      );
    },
  );

let paste = (z, text) =>
  Test_Editing.perform(
    z,
    [Action.Paste(Parser.prepare_clipboard(z, text))],
  );

let paste_case = (name, source, text, expected) =>
  test_case(
    name,
    `Quick,
    () => {
      let z = paste(parse(source), text);
      check(string, "pasted value", expected, content(z));
      let serialized = Printer.of_zipper(~holes="", z);
      check(
        string,
        "copy/paste round trip",
        serialized,
        Parser.to_zipper(~root=Exp, serialized)
        |> Option.get
        |> Printer.of_zipper(~holes=""),
      );
    },
  );

let tests = (
  "StringEnterProjector",
  [
    paste_case(
      "multiline paste preserves leading spaces",
      "\"left¦right\"",
      "  a\n    b",
      "left  a\n    bright",
    ),
    paste_case(
      "paste normalizes Windows newlines",
      "\"¦\"",
      "a\r\nb",
      "a\nb",
    ),
    paste_case(
      "paste preserves combining characters",
      "\"é¦中\"",
      "é\n👩‍💻",
      "éé\n👩‍💻中",
    ),
    test_case(
      "multiline paste replaces either selection direction", `Quick, () => {
      List.iter(
        ((source, direction)) => {
          let z =
            Test_Editing.perform(
              parse(source),
              [
                Select(Resize(Local(direction, ByChar))),
                Select(Resize(Local(direction, ByChar))),
              ],
            );
          let z = paste(z, " X\n  Y ");
          check(
            string,
            "only selected characters replaced",
            "ab X\n  Y ef",
            content(z),
          );
        },
        [("\"ab¦cdef\"", Util.Direction.Right), ("\"abcd¦ef\"", Left)],
      )
    }),
    test_case(
      "ordinary source paste remains syntax",
      `Quick,
      () => {
        let z = paste(parse("¦"), "\"a\"\n\"b\"");
        check(
          string,
          "source",
          "\"a\"\n \"b\"",
          Printer.of_zipper(~holes="", z),
        );
      },
    ),
    test_case(
      "unrepresentable pasted text is not silently changed", `Quick, () => {
      List.iter(
        ((source, text)) => {
          let z = parse(source);
          let elaborated = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
          check(
            bool,
            "conversion refused",
            true,
            Option.is_none(
              ProjectorPerform.insert_text_in_string(z, text, ~elaborated),
            ),
          );
        },
        [
          ("\"¦\"", "quote \"\ntext"),
          ("\"¦\"", "literal \\n\ntext"),
          ("\"¦n\"", "text\n\\"),
        ],
      )
    }),
    test_case(
      "native Unicode boundaries and selections",
      `Quick,
      () => {
        open Js_of_ocaml;
        let area: Util.WebUtil.TextArea.t =
          Js.Unsafe.coerce(
            Js.Unsafe.obj([|
              ("value", Js.Unsafe.inject(Js.string("é\n👩‍💻"))),
              ("selectionStart", Js.Unsafe.inject(7)),
              ("selectionEnd", Js.Unsafe.inject(7)),
            |]),
          );
        check(
          bool,
          "UTF-16 end",
          true,
          Util.WebUtil.TextArea.caret_at_end(area),
        );
        let pos = Util.WebUtil.TextArea.caret_pos(area);
        check(
          pair(int, int),
          "Unicode row/column",
          (1, 5),
          (pos.row, pos.col),
        );
        Js.Unsafe.set(area, "selectionStart", 0);
        check(
          bool,
          "selected range stays inside at start",
          false,
          Util.WebUtil.TextArea.caret_at_start(area),
        );
        check(
          bool,
          "selected range stays inside at end",
          false,
          Util.WebUtil.TextArea.caret_at_end(area),
        );
        Js.Unsafe.set(area, "selectionEnd", 0);
        check(
          bool,
          "collapsed start",
          true,
          Util.WebUtil.TextArea.caret_at_start(area),
        );
      },
    ),
    join_roundtrip("Backspace/Delete round trip", "\"hello¦ world\""),
    join_roundtrip("round trip at start", "\"¦hello\""),
    join_roundtrip("round trip at end", "\"hello¦\""),
    join_roundtrip("round trip empty string", "\"¦\""),
    join_roundtrip("round trip Unicode", "\"é👩‍💻é¦中\""),
    join_roundtrip("round trip indented", "let x =\n  \"left¦right\"\nin x"),
    test_case("only removing the final linebreak converts", `Quick, () => {
      List.iter(
        ((text, start, finish, direction)) => {
          check(
            bool,
            "stays projected",
            true,
            Option.is_none(
              TextAreaProj.joined_string(text, start, finish, direction),
            ),
          )
        },
        [
          ("single line", 4, 4, Util.Direction.Left),
          ("a\nb\nc", 2, 2, Left),
          ("a\nb", 3, 3, Left),
          ("a\nb", 0, 0, Left),
          ("a\nb", 3, 3, Right),
          ("a\n\"b", 2, 2, Left),
          ("a\\\nn", 3, 3, Left),
        ],
      )
    }),
    test_case("selection deletion can remove the final linebreaks", `Quick, () => {
      check(
        option(pair(string, int)),
        "selection replacement",
        Some(("ad", 1)),
        TextAreaProj.joined_string("ab\nc\nd", 1, 5, Left),
      )
    }),
    test_case("joining can form a new grapheme", `Quick, () => {
      check(
        option(pair(string, int)),
        "caret after the combined grapheme",
        Some(("áb", 1)),
        TextAreaProj.joined_string("a\ńb", 2, 2, Left),
      )
    }),
    conversion("middle", "\"hello¦ world\"", "hello\n world", 6),
    conversion("start", "\"¦hello\"", "\nhello", 1),
    conversion("end", "\"hello¦\"", "hello\n", 6),
    conversion("empty", "\"¦\"", "\n", 1),
    conversion(
      "Unicode graphemes and UTF-16",
      "\"é👩‍💻é¦中\"",
      "é👩‍💻é\n中",
      9,
    ),
    conversion("existing linebreak escape", "\"a\\nb¦c\"", "a\nb\nc", 4),
    conversion("caret inside linebreak escape", "\"a\\¦nb\"", "a\n\nb", 3),
    conversion(
      "indented expression",
      "let x =\n  \"left¦right\"\nin x",
      "left\nright",
      5,
    ),
    test_case("comments and boundaries are not converted", `Quick, () => {
      List.iter(
        source =>
          check(
            bool,
            source,
            false,
            ProjectorPerform.can_newline_in_string(parse(source)),
          ),
        ["# a¦b #", "¦\"ab\"", "\"ab\"¦", "12¦3"],
      )
    }),
    test_case(
      "undo retains native selection metadata",
      `Quick,
      () => {
        let initial = Test_Undo.mk_model();
        let focus =
          FocusEffect.TextAreaFocus("test-textarea", 2, 7, "backward");
        FocusEffect.input_focus := Some(focus);
        let edited = Test_Undo.apply(initial, Test_Undo.insert("x"));
        FocusEffect.scheduled := None;
        let undone = Test_Undo.apply(edited, Test_Undo.undo);
        check(
          bool,
          "same program model",
          true,
          undone.current === initial.current,
        );
        check(
          bool,
          "native range and direction scheduled",
          true,
          FocusEffect.scheduled^
          == Some(TextArea("test-textarea", 2, 7, "backward")),
        );
        FocusEffect.scheduled := None;
      },
    ),
    test_case(
      "conversion is one undoable edit with caret restoration",
      `Quick,
      () => {
        let m = Test_Undo.mk_model();
        let m = Test_Undo.apply(m, Globals(ActiveEditor(Select(All))));
        let m =
          Test_Undo.apply(m, Globals(ActiveEditor(Paste("\"abcd\""))));
        let m =
          List.fold_left(
            (m, _) =>
              Test_Undo.apply(
                m,
                Globals(ActiveEditor(Move(Local(Left, ByChar)))),
              ),
            m,
            [(), (), ()],
          );
        let edited =
          Test_Undo.apply(
            m,
            Globals(ActiveEditor(Project(NewlineInString))),
          );
        check(
          int,
          "one history entry",
          Test_Undo.undo_len(m) + 1,
          Test_Undo.undo_len(edited),
        );
        /* The browser recalculates syntax between edits; projector indices
         * are resolved from that refreshed cache. */
        let edited =
          Web.History.Update.calculate(
            ~schedule_action=_ => (),
            ~is_edited=true,
            ~dynamics=false,
            edited,
          );
        let joined =
          Test_Undo.apply(
            edited,
            Globals(ActiveEditor(Project(JoinTextLines(0, 3, 3, Left)))),
          );
        check(
          int,
          "joining adds one history entry",
          Test_Undo.undo_len(edited) + 1,
          Test_Undo.undo_len(joined),
        );
        check(
          string,
          "joining restores original text",
          Test_Undo.text_of(m),
          Test_Undo.text_of(joined),
        );
        let undo_join = Test_Undo.apply(joined, Test_Undo.undo);
        check(
          bool,
          "undo restores the multiline projector",
          true,
          undo_join.current === edited.current,
        );
        let redo_join = Test_Undo.apply(undo_join, Test_Undo.redo);
        check(
          bool,
          "redo restores the joined string",
          true,
          redo_join.current === joined.current,
        );
        let undone = Test_Undo.apply(edited, Test_Undo.undo);
        check(
          bool,
          "original model and caret",
          true,
          undone.current === m.current,
        );
        let redone = Test_Undo.apply(undone, Test_Undo.redo);
        check(
          bool,
          "projector restored",
          true,
          redone.current === edited.current,
        );
      },
    ),
  ],
);
