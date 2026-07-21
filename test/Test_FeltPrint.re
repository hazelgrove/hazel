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

let scenarios = [
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

let tests = [
  ("FeltPrint: space runs", space_runs),
  ("FeltPrint: empty runs", empty_runs),
  ("FeltPrint: trailing edge", trailing_edge),
  ("FeltPrint: linebreaks", linebreaks),
  ("FeltPrint: gallery", gallery),
  ("FeltPrint: comments", comments),
  ("FeltPrint: layout invisibility", invisibility),
  ("FeltPrint: scenarios", scenarios),
];
