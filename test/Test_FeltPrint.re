open Alcotest;
open Haz3lcore;

/* FELT pins: how placed segments will LOOK on screen under the
   zero-width-grout model (FeltPrint renders placed pieces — sigil in
   an existing space cell, free cell at a line end, zero-width pinch
   between tokens). Expectations are virtual-grout's Test_HolePlacement
   pins verbatim wherever the case carries over: same policy, same
   look, derived from material placed pieces instead of re-derived at
   render time. ? ~ occupy a cell; ‽ ∻ are zero-width.

   LAYOUT INVISIBILITY (the shut property behind "grout must not push
   the user around"): mapping cell-occupying sigils back to a space
   and zero-width sigils to nothing must reproduce the grout-stripped
   render EXACTLY — grout contributes nothing to layout, so its
   appearance/disappearance cannot displace text or the caret.

   SCENARIOS: felt trajectories of real editing (via Perform) — one
   render per applied step, caret marked. These are the look-and-feel
   review surface; jank observations get annotated at the pin. */

let string_testable = testable(Fmt.string, String.equal);

let parse = (s: string): Segment.t =>
  switch (Parser.to_zipper(~root=Sort.Exp, s)) {
  | None => Alcotest.fail("parse failed: " ++ String.escaped(s))
  | Some(z) => Zipper.unselect_and_zip(z)
  };

let felt = (s: string): string =>
  s |> parse |> GroutPlace.place |> FeltPrint.render;

let t = (name: string, input: string, expected: string) =>
  test_case(name, `Quick, () =>
    check(
      string_testable,
      Printf.sprintf("%s (input: %s)", name, String.escaped(input)),
      expected,
      felt(input),
    )
  );

/* ---- the virtual-grout pin table, reproduced from placed pieces ---- */

let space_runs = [
  t("operand hole, one space", "let x = in x", "let x =?in x"),
  t("operand hole, two spaces", "let x =  in x", "let x = ?in x"),
  t("operand hole, three spaces", "let x =   in x", "let x = ? in x"),
  t(
    "operand hole, long run anchors left",
    "let x =     in x",
    "let x = ?   in x",
  ),
  t("operator hole, one space", "1 2", "1~2"),
  t("operator hole, two spaces", "1  2", "1 ~2"),
  t("operator hole, long run anchors left", "1     2", "1 ~   2"),
  t("adjacent operators", "1 + + 2", "1 +?+ 2"),
  t("leading operand hole in child", "let x = + 2 in x", "let x =?+ 2 in x"),
];

let empty_runs = [
  t("no whitespace, child trailing", "(1 +)", "(1 +‽)"),
  t("no whitespace, child leading", "(* 2)", "(‽* 2)"),
  t("leading at top level, no whitespace", "* 2", "‽* 2"),
  t("leading anchors right: one space before token", "  * 2", "? * 2"),
  t("leading long run anchors right", "    * 2", "  ? * 2"),
];

let trailing_edge = [
  t("trailing at top level, no whitespace", "1 +", "1 +?"),
  t("trailing space survives next to the hole", "1 + ", "1 + ?"),
  t("trailing two spaces: hole in the second", "1 +  ", "1 + ?"),
  t(
    "trailing long run: one space after, rest continues",
    "1 +    ",
    "1 + ?  ",
  ),
  t("trailing at top level, linebreak", "1 +\n", "1 +\n?"),
  t(
    "let chain trailing hole gets next line",
    "let x = 1 in\n",
    "let x = 1 in\n?",
  ),
  t(
    "space before trailing linebreak: hole still next line",
    "let a = 1 in \n",
    "let a = 1 in \n?",
  ),
  t("trailing indent after linebreak: hole at caret", "1 +\n  ", "1 +\n  ?"),
  t(
    "trailing extra linebreaks don't drag the hole down",
    "1 +\n\n",
    "1 +\n?\n",
  ),
  t(
    "trailing blank then blank: first blank line wins",
    "1 +\n  \n",
    "1 +\n  ?\n",
  ),
  t("blank program: hole stays on the first line", "\n\n\n", "?\n\n\n"),
  t("blank program with typed indent: hole at its end", "  \n\n", "  ?\n\n"),
  t(
    "leading blank lines before content: hole on the first",
    "\n\n* 2",
    "?\n\n* 2",
  ),
];

let linebreaks = [
  t(
    "mid-segment linebreak: end of previous line",
    "let x =\nin x",
    "let x =?\nin x",
  ),
  t(
    "mid-segment space then linebreak: space survives",
    "let x = \nin x",
    "let x = ?\nin x",
  ),
  t(
    "case scrutinee: stays on case's line",
    "case\n| 1 => 1 end",
    "case?\n| 1 => 1 end",
  ),
  t(
    "case scrutinee with indented rule",
    "case\n  | 1 => 1 end",
    "case?\n  | 1 => 1 end",
  ),
  t(
    "blank line inside gap is the prepared slot",
    "let x =\n\nin x",
    "let x =\n?\nin x",
  ),
  t(
    "indented blank line: hole at end of indent",
    "let a =\n  \nin a",
    "let a =\n  ?\nin a",
  ),
  t(
    "multiple blank lines: first one, never further down",
    "let x =\n\n\nin x",
    "let x =\n?\n\nin x",
  ),
  t(
    "blank line beats trailing spaces on the owner's line",
    "let a =  \n  \nin a",
    "let a =  \n  ?\nin a",
  ),
  t(
    "comment occupies the blank line: hole falls back to owner's line",
    "let a =\n  #c#\nin a",
    "let a =?\n  #c#\nin a",
  ),
  t(
    "spaces before linebreak: hole on previous line",
    "let x =  \nin x",
    "let x = ?\nin x",
  ),
];

let gallery = [
  t(
    "missing if condition, one space",
    "if then 2 else 3",
    "if?then 2 else 3",
  ),
  t(
    "missing if condition, two spaces",
    "if  then 2 else 3",
    "if ?then 2 else 3",
  ),
  t("missing list element", "[1, , 3]", "[1,?, 3]"),
  t("missing fun pattern", "fun -> 2", "fun?-> 2"),
  t("missing ap argument", "f( )", "f(?)"),
  t("adjacent operands", {|"a" "b"|}, {|"a"~"b"|}),
  t("missing operand before in", "let x = 1 + in x", "let x = 1 +?in x"),
  t("trailing hole after multiline program", "1 +\n2 +\n", "1 +\n2 +\n?"),
];

let comments = [
  t("space then comment: hole in the space", "1 + #c#", "1 +?#c#"),
  t("comment directly adjacent: thin before it", "1 +#c#", "1 +‽#c#"),
  t("linebreak then comment: end of previous line", "1 +\n#c#", "1 +?\n#c#"),
  t("no conflict across comment line", "1 +\n#c#\n2", "1 +\n#c#\n2"),
];

/* ---- layout invisibility ---- */

let corpus: list(string) = [
  "let x = in x",
  "let x =  in x",
  "1 2",
  "1 + + 2",
  "(1 +)",
  "(* 2)",
  "* 2",
  "  * 2",
  "1 +",
  "1 + ",
  "1 +    ",
  "1 +\n",
  "let x = 1 in\n",
  "1 +\n  ",
  "1 +\n\n",
  "\n\n\n",
  "let x =\nin x",
  "case\n| 1 => 1 end",
  "let x =\n\nin x",
  "let a =\n  #c#\nin a",
  "if then 2 else 3",
  "[1, , 3]",
  "fun -> 2",
  "f( )",
  {|"a" "b"|},
  "1 + #c#",
  "1 +#c#",
  "1 +\n#c#\n2",
];

let invisibility = [
  test_case(
    "layout invisibility over the corpus",
    `Quick,
    () => {
      let out =
        corpus
        |> List.map(s => {
             let seg = parse(s);
             let placed = GroutPlace.place(seg);
             let ghostless = FeltPrint.render_ghostless(placed);
             let stripped = FeltPrint.render(GroutPlace.strip(seg));
             ghostless == stripped
               ? ""
               : Printf.sprintf(
                   "input=%s ghostless=%s stripped=%s\n",
                   String.escaped(s),
                   String.escaped(ghostless),
                   String.escaped(stripped),
                 );
           })
        |> String.concat("");
      check(string_testable, "grout contributes nothing to layout", "", out);
    },
  ),
];

/* ---- editing scenarios: felt trajectories with the caret ---- */

let scenario = (name: string, script: list(Action.t), expected: string) =>
  test_case(
    name,
    `Quick,
    () => {
      let (_, states_rev) =
        List.fold_left(
          ((z, acc), a) => {
            let z' = Test_Editing.perform(z, [a]);
            (z', [FeltPrint.of_zipper(z'), ...acc]);
          },
          (Zipper.init(), []),
          script,
        );
      let got =
        states_rev |> List.rev_map(s => "  " ++ s) |> String.concat("\n");
      check(string_testable, name, expected, got);
    },
  );

let type_string = (s: string): list(Action.t) =>
  Token.to_list(s) |> List.map(c => Action.Insert(c));

let backspaces = (n: int): list(Action.t) =>
  List.init(n, _ => Action.Destruct(Local(Left, ByChar)));

let lefts = (n: int): list(Action.t) =>
  List.init(n, _ => Action.Move(Local(Left, ByChar)));

/* FELT ASSESSMENT (2026-07-20): both entry trajectories hold the
   invisibility promise end to end — typed text never moves a column,
   holes only ever materialize in the line-end free cell AFTER the
   caret, and the caret is never displaced by grout appearing or
   disappearing. `1 i` stays hole-free mid-word because the engine
   molds `i` as a partial infix (`in` prefix), so the felt view shows
   `1 i¦?` not `1~i¦`. Deletion mirror-images entry exactly. These
   single-line runs exercise only free-cell holes; interior
   (cell-consuming) and multi-line trajectories are the next tier. */
let entry_let =
  "  l¦\n"
  ++ "  le¦\n"
  ++ "  let¦?\n"
  ++ "  let ¦?\n"
  ++ "  let x¦\n"
  ++ "  let x ¦\n"
  ++ "  let x =¦?\n"
  ++ "  let x = ¦?\n"
  ++ "  let x = 1¦\n"
  ++ "  let x = 1 ¦\n"
  ++ "  let x = 1 i¦?\n"
  ++ "  let x = 1 in¦?\n"
  ++ "  let x = 1 in ¦?\n"
  ++ "  let x = 1 in x¦";

let idel =
  entry_let
  ++ "\n"
  ++ "  let x = 1 in ¦x\n"
  ++ "  let x = 1 in¦ x\n"
  ++ "  let x = 1 i¦n x\n"
  ++ "  let x = 1 ¦in x\n"
  ++ "  let x = 1¦ in x\n"
  ++ "  let x = ¦?in x";

let mline =
  "  l¦\n"
  ++ "  le¦\n"
  ++ "  let¦?\n"
  ++ "  let ¦?\n"
  ++ "  let x¦\n"
  ++ "  let x ¦\n"
  ++ "  let x =¦?\n"
  ++ "  let x =\n  ¦?\n"
  ++ "  let x =\n  1¦\n"
  ++ "  let x =\n  1 ¦\n"
  ++ "  let x =\n  1 i¦?\n"
  ++ "  let x =\n  1 in¦?\n"
  ++ "  let x =\n  1 in ¦?\n"
  ++ "  let x =\n  1 in x¦";

/* ANDREW'S LIVE REPROS (2026-07-22): the caret model at borrowed
   cells. A1: deleting the pattern `a` must move the caret ONE
   perceived position (landing RIGHT of the appearing hole, where
   `a` was); every subsequent arrow-left/right changes the drawn
   column (no dead presses); deleting the last pattern space must
   not throw the caret to another row. Pinned with the
   editor-faithful of_zipper (caret computed against the placed
   display measured, exactly as the live editor does). */
let a1_backspaces =
  type_string("let a = 1 in a")
  @ lefts(9)
  @ backspaces(1)
  @ backspaces(1)
  @ lefts(0);

/* SELECTION-ANCHOR pin (PosMap): the anchor marker ‹ must obey the
   same consumed-space edge rule as the caret — shift-selecting left
   across the borrowed-cell hole of `let ␣ = ...` anchors at the
   hole's RIGHT edge (where the deleted `a` was), not the collapsed
   column. Verified failing before selection_anchor_point routed
   through PosMap. */
let sel_render = (z: Zipper.t): string => {
  let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
  let placed = GroutPlace.place(seg);
  let measured = Measured.of_segment(placed, Id.Map.empty, Id.Map.empty);
  let caret =
    FeltPrint.measured_caret(
      ~measured,
      placed,
      Zipper.Caret.point(measured, z),
    );
  let rows =
    FeltPrint.measured_print(~measured, placed) |> String.split_on_char('\n');
  let rows =
    switch (Zipper.selection_anchor_point(measured, z)) {
    | Some(a) =>
      let a = FeltPrint.measured_caret(~measured, placed, a);
      if (Util.Point.compare(a, caret) >= 0) {
        rows
        |> Printer.insert_string("‹", a)
        |> Printer.insert_string("¦", caret);
      } else {
        rows
        |> Printer.insert_string("¦", caret)
        |> Printer.insert_string("‹", a);
      };
    | None => rows |> Printer.insert_string("¦", caret)
    };
  String.concat("\n", rows);
};

let selection_pins = [
  /* two-space config (delete only `a`): anchor piece is the REAL
     space — unconsumed, no redirect involved; pins that the plain
     path stays plain */
  test_case(
    "anchor between spaces, hole in the old cell",
    `Quick,
    () => {
      let z =
        Test_Editing.perform(
          Zipper.init(),
          Test_Editing.mk("let a¦ = 1 in a")
          @ [
            Action.Destruct(Local(Left, ByChar)),
            Action.Select(Resize(Local(Left, ByChar))),
          ],
        );
      check(
        string_testable,
        "real-space anchor needs no redirect",
        "let¦ ‹?= 1 in a",
        sel_render(z),
      );
    },
  ),
  /* andrew's A1 prep (delete the space, then `a`): the anchor piece
     IS the consumed space — its right boundary is the hole's right
     edge, same rule as the caret. Verified failing (anchor collapsed
     onto the caret's column) before selection_anchor_point routed
     through PosMap. */
  test_case(
    "anchor at consumed-space edge (A1 prep)",
    `Quick,
    () => {
      let z =
        Test_Editing.perform(
          Zipper.init(),
          Test_Editing.mk("let a ¦= 1 in a")
          @ [
            Action.Destruct(Local(Left, ByChar)),
            Action.Destruct(Local(Left, ByChar)),
            Action.Select(Resize(Local(Left, ByChar))),
          ],
        );
      check(
        string_testable,
        "consumed-space anchor redirects to the hole edge",
        "let¦?‹= 1 in a",
        sel_render(z),
      );
    },
  ),
];

let scenarios = [
  /* ANDREW'S C REPRO: extra typed spaces beyond the auto-indent are
     REAL material — backspace deletes them one per press; only a run
     at (or under) the line's indent level joins the linebreak in one
     keystroke. Script: let a = SP, Enter (auto-indents body line),
     type 4 extra spaces, then two backspaces: each removes ONE
     space; the caret stays on its line. */
  scenario(
    "C: backspace beyond the indent deletes one space per press",
    type_string("let a = ")
    @ [Action.Insert("\n")]
    @ type_string("    ")
    @ backspaces(2),
    "  l¦\n  le¦\n  let¦?\n  let ¦?\n  let a¦\n  let a ¦\n"
    ++ "  let a =¦?\n  let a = ¦?\n  let a = \n  ¦?\n  let a = \n   ¦?\n"
    ++ "  let a = \n    ¦?\n  let a = \n     ¦?\n  let a = \n      ¦?\n"
    ++ "  let a = \n     ¦?\n  let a = \n    ¦?",
  ),
  /* ANDREW'S A1 REPRO PINNED FIXED: deleting `a` moves the caret
     exactly ONE perceived position, landing RIGHT of the appearing
     hole (where `a` was); arrow left/right through the borrowed
     cell each change the drawn column (the consumed-space redirect
     in Zipper.base_point — no dead presses, no skipping `=`);
     deleting the last pattern space leaves an empty child whose
     caret homes to the parent's shard (ancestor fallback), NOT
     (0,0) — no jump to another row. */
  /* ANDREW'S B REPRO: an unclosed `let` above absorbs later lines
     as its presumed body; its owed hole trails the caret. Enter on
     the later line must NOT indent for that presumption — a derived
     hole is not user content (Indentation: Grout does not fire the
     continuation rule). The new line lands at the CURRENT line's
     level. */
  scenario(
    "B: Enter under an unclosed let above stays at the line's level",
    type_string("let b = \nlet a = 1 in x") @ [Action.Insert("\n")],
    "  l¦\n  le¦\n  let¦?\n  let ¦?\n  let b¦\n  let b ¦\n"
    ++ "  let b =¦?\n  let b = ¦?\n  let b = \n  ¦?\n  let b = \n  l¦\n"
    ++ "  let b = \n  le¦\n  let b = \n  let¦?\n  let b = \n  let ¦?\n"
    ++ "  let b = \n  let a¦\n  let b = \n  let a ¦\n  let b = \n"
    ++ "  let a =¦?\n  let b = \n  let a = ¦?\n  let b = \n"
    ++ "  let a = 1¦\n  let b = \n  let a = 1 ¦\n  let b = \n"
    ++ "  let a = 1 i¦?\n  let b = \n  let a = 1 in¦?\n  let b = \n"
    ++ "  let a = 1 in ¦?\n  let b = \n  let a = 1 in x¦\n  let b = \n"
    ++ "  let a = 1 in x\n¦",
  ),
  scenario(
    "A1: delete the space right of pattern a, then a itself",
    type_string("let a = 1 in a") @ lefts(8) @ backspaces(2),
    "  l¦\n"
    ++ "  le¦\n"
    ++ "  let¦?\n"
    ++ "  let ¦?\n"
    ++ "  let a¦\n"
    ++ "  let a ¦\n"
    ++ "  let a =¦?\n"
    ++ "  let a = ¦?\n"
    ++ "  let a = 1¦\n"
    ++ "  let a = 1 ¦\n"
    ++ "  let a = 1 i¦?\n"
    ++ "  let a = 1 in¦?\n"
    ++ "  let a = 1 in ¦?\n"
    ++ "  let a = 1 in a¦"
    ++ "\n"
    ++ "  let a = 1 in ¦a\n"
    ++ "  let a = 1 in¦ a\n"
    ++ "  let a = 1 i¦n a\n"
    ++ "  let a = 1 ¦in a\n"
    ++ "  let a = 1¦ in a\n"
    ++ "  let a = ¦1 in a\n"
    ++ "  let a =¦ 1 in a\n"
    ++ "  let a ¦= 1 in a"
    ++ "\n  let a¦= 1 in a"
    ++ "\n  let?¦= 1 in a",
  ),
  scenario(
    "A1: arrows through the borrowed cell are never dead",
    type_string("let a = 1 in a")
    @ lefts(8)
    @ backspaces(2)
    @ lefts(1)
    @ [Action.Move(Local(Right, ByChar))],
    "  l¦\n"
    ++ "  le¦\n"
    ++ "  let¦?\n"
    ++ "  let ¦?\n"
    ++ "  let a¦\n"
    ++ "  let a ¦\n"
    ++ "  let a =¦?\n"
    ++ "  let a = ¦?\n"
    ++ "  let a = 1¦\n"
    ++ "  let a = 1 ¦\n"
    ++ "  let a = 1 i¦?\n"
    ++ "  let a = 1 in¦?\n"
    ++ "  let a = 1 in ¦?\n"
    ++ "  let a = 1 in a¦"
    ++ "\n"
    ++ "  let a = 1 in ¦a\n"
    ++ "  let a = 1 in¦ a\n"
    ++ "  let a = 1 i¦n a\n"
    ++ "  let a = 1 ¦in a\n"
    ++ "  let a = 1¦ in a\n"
    ++ "  let a = ¦1 in a\n"
    ++ "  let a =¦ 1 in a\n"
    ++ "  let a ¦= 1 in a"
    ++ "\n  let a¦= 1 in a"
    ++ "\n  let?¦= 1 in a"
    ++ "\n  let¦?= 1 in a"
    ++ "\n  let?¦= 1 in a",
  ),
  scenario(
    "A1: deleting the last pattern space keeps the caret on its row",
    type_string("let a = 1 in a") @ lefts(8) @ backspaces(3),
    "  l¦\n"
    ++ "  le¦\n"
    ++ "  let¦?\n"
    ++ "  let ¦?\n"
    ++ "  let a¦\n"
    ++ "  let a ¦\n"
    ++ "  let a =¦?\n"
    ++ "  let a = ¦?\n"
    ++ "  let a = 1¦\n"
    ++ "  let a = 1 ¦\n"
    ++ "  let a = 1 i¦?\n"
    ++ "  let a = 1 in¦?\n"
    ++ "  let a = 1 in ¦?\n"
    ++ "  let a = 1 in a¦"
    ++ "\n"
    ++ "  let a = 1 in ¦a\n"
    ++ "  let a = 1 in¦ a\n"
    ++ "  let a = 1 i¦n a\n"
    ++ "  let a = 1 ¦in a\n"
    ++ "  let a = 1¦ in a\n"
    ++ "  let a = ¦1 in a\n"
    ++ "  let a =¦ 1 in a\n"
    ++ "  let a ¦= 1 in a"
    ++ "\n  let a¦= 1 in a"
    ++ "\n  let?¦= 1 in a"
    ++ "\n  let¦?= 1 in a",
  ),
  scenario(
    "left-to-right entry: let x = 1 in x",
    type_string("let x = 1 in x"),
    "  l¦\n"
    ++ "  le¦\n"
    ++ "  let¦?\n"
    ++ "  let ¦?\n"
    ++ "  let x¦\n"
    ++ "  let x ¦\n"
    ++ "  let x =¦?\n"
    ++ "  let x = ¦?\n"
    ++ "  let x = 1¦\n"
    ++ "  let x = 1 ¦\n"
    ++ "  let x = 1 i¦?\n"
    ++ "  let x = 1 in¦?\n"
    ++ "  let x = 1 in ¦?\n"
    ++ "  let x = 1 in x¦",
  ),
  scenario(
    "left-to-right entry: 1 + 2",
    type_string("1 + 2"),
    "  1¦\n" ++ "  1 ¦\n" ++ "  1 +¦?\n" ++ "  1 + ¦?\n" ++ "  1 + 2¦",
  ),
  /* FELT ASSESSMENT: pure caret movement produces zero hole churn;
     deleting `1` materializes the hole IN the vacated cell with the
     caret at its left and `in x` never moving a column; retyping is
     the exact inverse. Killing an operator gives the concave hole the
     same way (`1 ¦~2`). Enter carries the trailing obligation onto
     the caret's fresh line (top-level blank-line slot), landing
     after the stored auto-indent spaces — obligation follows the
     caret to where typing will continue. */
  scenario(
    "interior deletion: kill the 1 of let x = 1 in x",
    type_string("let x = 1 in x") @ lefts(5) @ backspaces(1),
    idel,
  ),
  scenario(
    "interior repair: retype into the gap",
    type_string("let x = 1 in x")
    @ lefts(5)
    @ backspaces(1)
    @ type_string("2"),
    idel ++ "\n  let x = 2¦ in x",
  ),
  scenario(
    "multiline entry: let x = then Enter",
    type_string("let x =") @ [Action.Insert("\n")] @ type_string("1 in x"),
    mline,
  ),
  scenario(
    "kill an operator mid-program: 1 + 2 minus the +",
    type_string("1 + 2") @ lefts(2) @ backspaces(1),
    "  1¦\n"
    ++ "  1 ¦\n"
    ++ "  1 +¦?\n"
    ++ "  1 + ¦?\n"
    ++ "  1 + 2¦\n"
    ++ "  1 + ¦2\n"
    ++ "  1 +¦ 2\n"
    ++ "  1 ¦~2",
  ),
  /* CARET-AT-BORROWED-CELLS (andrew's live repro, pinned before any
     caret fix): shrink the gap between delimiters to zero and watch
     where the caret lands; then walk back left through the borrowed
     cells. The judged property: the caret column in the felt render
     always sits between two user glyphs or at a line edge — never
     inside a sigil's cell, and never at a column the user cannot
     reach back. */
  /* FELT ASSESSMENT (judged): through shrink-to-zero the caret is
     column-stable (no jump when the pinch forms), and walking left
     crosses exactly one user glyph per press — the caret never
     enters a sigil's cell and every column is reachable back. The
     live jank andrew reported traced to the view's old local cell
     guess, not to placement; these pins hold the felt truth the
     one-home view now renders. Known residual (pinned in the
     fuzzer): INNER carets beside a consumed cell resolve one col
     short. */
  scenario(
    "shrink f( ) to zero: pinch forms under the caret",
    type_string("f( ") @ backspaces(1),
    "  f¦\n" ++ "  f(¦?\n" ++ "  f( ¦?\n" ++ "  f(¦?",
  ),
  scenario(
    "shrink then walk left through the pinch",
    type_string("f( ") @ backspaces(1) @ lefts(2),
    "  f¦\n"
    ++ "  f(¦?\n"
    ++ "  f( ¦?\n"
    ++ "  f(¦?\n"
    ++ "  f¦(?\n"
    ++ "  ¦f(?",
  ),
  scenario(
    "shrink an operator gap: 1  2 minus the middle space",
    type_string("1  2") @ lefts(1) @ backspaces(1),
    "  1¦\n"
    ++ "  1 ¦\n"
    ++ "  1  ¦\n"
    ++ "  1 ~2¦\n"
    ++ "  1 ~¦2\n"
    ++ "  1~¦2",
  ),
  scenario(
    "delete back through: 1 + 2",
    type_string("1 + 2") @ backspaces(5),
    "  1¦\n"
    ++ "  1 ¦\n"
    ++ "  1 +¦?\n"
    ++ "  1 + ¦?\n"
    ++ "  1 + 2¦\n"
    ++ "  1 + ¦?\n"
    ++ "  1 +¦?\n"
    ++ "  1 ¦\n"
    ++ "  1¦\n"
    ++ "  ¦?",
  ),
];

/* A2: the caret FACES the derived hole beside it (ported
   direction_at_hole): shape probes over Zipper.Caret.direction. */
let dir_show = (d: option(Util.Direction.t)): string =>
  switch (d) {
  | None => "flat"
  | Some(Left) => "left"
  | Some(Right) => "right"
  };

let facing = (name: string, script: list(Action.t), expected: string) =>
  Alcotest.test_case(
    name,
    `Quick,
    () => {
      let z =
        List.fold_left(
          (z, a) => Test_Editing.perform(z, [a]),
          Zipper.init(),
          script,
        );
      Alcotest.check(
        string_testable,
        name,
        expected,
        dir_show(Zipper.Caret.direction(z)),
      );
    },
  );

/* values verified against virtual-grout's absolute()/decide()
   computation by hand — the port is exact; `right` at a concave
   hole's right side and `left` leaning on a convex hole both match
   vg's andrew-approved live behavior */
let facings = [
  facing(
    "A2: let b = 1 typed on the line above an existing let",
    type_string("let a = 1 in a")
    @ lefts(14)
    @ [Action.Insert("\n")]
    @ lefts(1)
    @ type_string("let b = 1"),
    /* the complaint case: was flat-left (edge rule); the derived
       junction hole between `1` and the next line now drives it */
    "right",
  ),
  facing(
    "after 1 in let b = 1 (in owed)",
    type_string("let b = 1"),
    "right",
  ),
  facing("trailing op: 1 +", type_string("1 +"), "left"),
  facing("trailing op with space: 1 + ", type_string("1 + "), "left"),
  facing(
    "left of an operand hole: let x = | in x",
    type_string("let x = w in x") @ lefts(5) @ backspaces(1) @ lefts(1),
    "left",
  ),
  facing(
    "right of an operand hole: let x = |SP in x",
    type_string("let x = w in x") @ lefts(5) @ backspaces(1),
    "left",
  ),
  facing(
    "between operands: 1 SP 2 mid",
    type_string("1  2") @ lefts(2),
    "right",
  ),
];

let tests = [
  ("FeltPrint: caret facing", facings),
  ("FeltPrint: space runs", space_runs),
  ("FeltPrint: empty runs", empty_runs),
  ("FeltPrint: trailing edge", trailing_edge),
  ("FeltPrint: linebreaks", linebreaks),
  ("FeltPrint: gallery", gallery),
  ("FeltPrint: comments", comments),
  ("FeltPrint: layout invisibility", invisibility),
  ("FeltPrint: selection", selection_pins),
  ("FeltPrint: scenarios", scenarios),
];
