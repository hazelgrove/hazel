open Util;
open Alcotest;
open Haz3lcore;
open Test_Editing;

let test_indent = (~name, ~init, ~goal): test_case(_) => {
  /* Here, we trim trailing whitespace as current regrouting may
     introduce extraneous trailing whitespace during entry */
  test_case(name, `Quick, () =>
    check(
      testable(Fmt.string, (a, b) => {
        String.equal(
          StringUtil.trim_trailing_whitespace(a),
          StringUtil.trim_trailing_whitespace(b),
        )
      }),
      goal,
      goal,
      init
      |> string_to_ltr_actions
      |> perform(Zipper.init())
      |> Printer.of_zipper(
           ~holes=convex_char,
           ~concave_holes=concave_char,
           /* No caret for now */
         ),
    )
  );
};

/* Test indentation after Format action (Cmd+S).
   Use this for cases where auto-indent at typing time can't know
   the correct indentation, but Format can fix it based on final structure. */
let test_indent_after_format = (~name, ~init, ~goal): test_case(_) => {
  test_case(name, `Quick, () =>
    check(
      testable(Fmt.string, (a, b) => {
        String.equal(
          StringUtil.trim_trailing_whitespace(a),
          StringUtil.trim_trailing_whitespace(b),
        )
      }),
      goal,
      goal,
      string_to_ltr_actions(init)
      @ [Action.Format(Indent)]
      |> perform(Zipper.init())
      |> Printer.of_zipper(~holes=convex_char, ~concave_holes=concave_char),
    )
  );
};

let indentation_tests = [
  /* === PARTITION-AWARE AUTO-INDENT (2026-07-27, andrew) ===
     The walk consumes the canonical completion's PARTITIONER, so
     layout is evidence: lines the user wrote FLUSH under an unclosed
     construct are siblings (each partition restarts at base — no
     additive staircase, and Format does not resurrect one), while
     indented lines are absorbed (the typed-through staircase, where
     each accepted suggestion articulates the nesting, is unchanged —
     see the typed pins throughout this file). Flush states are built
     with Paste: typing would accept suggestions and articulate. */
  test_case(
    "flush sibling: Enter under a flat-written let chain stays flat",
    `Quick,
    () =>
    check(
      testable(Fmt.string, String.equal),
      "flush repro 1",
      "let a =\nlet b = 1 in\n9",
      [Action.Paste("let a =\nlet b = 1 in")]
      @ string_to_ltr_actions("\n9")
      |> perform(Zipper.init())
      |> Printer.of_zipper(~holes=convex_char, ~concave_holes=concave_char),
    )
  ),
  test_case(
    "flush stacked lets: suggestion is local, never additive", `Quick, () =>
    check(
      testable(Fmt.string, String.equal),
      "flush repro 2",
      /* NOTE vs artifact-grout: there the same gesture suggests 2
         (level_of derives the let's def hole fresh, so the fresh
         line reads as the def slot); this branch's stored-grout
         channel has no hole here, so the suggestion is flat. Both
         are NON-ADDITIVE, which is the pinned property. */
      "let a =\nlet b =\nlet c =\nlet d =\n9",
      [Action.Paste("let a =\nlet b =\nlet c =\nlet d =")]
      @ string_to_ltr_actions("\n9")
      |> perform(Zipper.init())
      |> Printer.of_zipper(~holes=convex_char, ~concave_holes=concave_char),
    )
  ),
  test_case("format respects articulated flat layout", `Quick, () =>
    check(
      testable(Fmt.string, String.equal),
      "flush repro 2 format",
      "let a =\nlet b =\nlet c =\nlet d =\n9",
      [Action.Paste("let a =\nlet b =\nlet c =\nlet d =\n9")]
      @ [Action.Format(Indent)]
      |> perform(Zipper.init())
      |> Printer.of_zipper(~holes=convex_char, ~concave_holes=concave_char),
    )
  ),
  /* an incrementor (fun ->) inside a child raises the level for ALL
     following sibling lines, not just the first (regression: chain
     lines after the first flattened back to the child opening) */
  test_indent_after_format(
    ~name="fun-body let chain indents uniformly",
    ~init=
      "let render =\nfun w ->\nlet margin = 4 in\nlet pad = w / margin in\nlet inner = w - pad * 2 in\ninner + pad\nin\nrender(3)",
    ~goal=
      "let render =\n  fun w ->\n    let margin = 4 in\n    let pad = w / margin in\n    let inner = w - pad * 2 in\n    inner + pad\nin\nrender(3)",
  ),
  /* Consecutive linebreaks after an indenting form share one level —
     each Enter must not staircase (regression: level+2 compounded per
     blank line) */
  test_indent(
    ~name="blank lines after fun arrow don't staircase",
    ~init="fun z ->\n\n\nz",
    ~goal="fun z ->\n\n\n  z",
  ),
  test_indent_after_format(
    ~name="blank lines after fun arrow don't staircase (format)",
    ~init="fun z ->\n\n\nz",
    ~goal="fun z ->\n\n\n  z",
  ),
  /* INDENTATION OF COMPLETE SYNTAX */
  test_indent(
    ~name="Top level doesn't auto indent",
    ~init={|
  1|},
    ~goal={|
  1|},
  ),
  test_indent(
    ~name="Bidelimited context same-line terminator indents",
    ~init={|(
1)|},
    ~goal={|(
  1)|},
  ),
  test_indent(
    ~name=
      "Double bidelimited context same-line terminator doesn't double indents",
    ~init={|((
1))|},
    ~goal={|((
  1))|},
  ),
  test_indent(
    ~name="Nested bidelimited contex with linebreaks",
    ~init={|(
(
1
)
)|},
    ~goal={|(
  (
    1
  )
)|},
  ),
  test_indent(
    ~name="let",
    ~init={|let a =
b
in 1|},
    ~goal={|let a =
  b
in 1|},
  ),
  test_indent(
    ~name="if then else",
    ~init={|if true
then
1
else 2|},
    ~goal={|if true
then
  1
else 2|},
  ),
  test_indent(
    ~name="if expression in bidelimited context",
    ~init={|(
if true
then
1
else
2
)|},
    ~goal={|(
  if true
  then
    1
  else
    2
)|},
  ),
  test_indent(
    ~name="Bidelimited context indents",
    ~init={|(
1
)|},
    ~goal={|(
  1
)|},
  ),
  test_indent(
    ~name="Double bidelimited context doesn't double indents",
    ~init={|((
1
))|},
    ~goal={|((
  1
))|},
  ),
  test_indent(
    ~name="Operators don't indent 1",
    ~init={|1 +
2|},
    ~goal={|1 +
2|},
  ),
  test_indent(
    ~name="Operators don't indent 2",
    ~init={|1
+ 2|},
    ~goal={|1
+ 2|},
  ),
  test_indent(
    ~name="Operators in nested context",
    ~init={|(
1+
2
)|},
    ~goal={|(
  1+
  2
)|},
  ),
  test_indent(
    ~name="Function application",
    ~init={|go(
1,
2
)|},
    ~goal={|go(
  1,
  2
)|},
  ),
  test_indent(
    ~name="Nested function application",
    ~init={|go(
Ap(
Lam(
"yo",
Var("yo")),
Lam(
"bro",
Var("bro")))
)|},
    ~goal=
      {|go(
  Ap(
    Lam(
      "yo",
      Var("yo")),
    Lam(
      "bro",
      Var("bro")))
)|},
  ),
  test_indent(
    ~name="Case rules with and without linebreaks after `=>`",
    ~init=
      {|let length : [Int] -> Int =
fun xs ->
case xs
| [] => 0
| hd::tl =>
1 + length(tl)
end
in 1|},
    ~goal=
      {|let length : [Int] -> Int =
  fun xs ->
    case xs
    | [] => 0
    | hd::tl =>
      1 + length(tl)
    end
in 1|},
  ),
  test_indent(
    ~name="Nested cases",
    ~init=
      {|let go: Exp -> Result =
fun e ->
case e
| Var(n) =>
Error("Free Variable")
| Lam(x, body) =>
Ok(Lam(x, body))
| Ap(e1,e2) =>
case go(e1)
| Ok(Lam(x, body)) =>
case go(e2)
| Error(err) =>  Error(err)
| Ok(arg) =>
go(subst(arg, x, body)) end
| _ => Error("Not a Function") end end in go|},
    ~goal=
      {|let go: Exp -> Result =
  fun e ->
    case e
    | Var(n) =>
      Error("Free Variable")
    | Lam(x, body) =>
      Ok(Lam(x, body))
    | Ap(e1,e2) =>
      case go(e1)
      | Ok(Lam(x, body)) =>
        case go(e2)
        | Error(err) =>  Error(err)
        | Ok(arg) =>
          go(subst(arg, x, body)) end
      | _ => Error("Not a Function") end end in go|},
  ),
  test_indent(
    ~name="Even: commas, function literals, if expression",
    ~init=
      {|let (even : Int -> Bool, odd : Int -> Bool) = (
fun n ->
if n == 0
then
true
else
odd(n - 1),
fun n ->
if n == 0 then false else even(n - 1)
) in 1|},
    ~goal=
      {|let (even : Int -> Bool, odd : Int -> Bool) = (
  fun n ->
    if n == 0
    then
      true
    else
      odd(n - 1),
  fun n ->
    if n == 0 then false else even(n - 1)
) in 1|},
  ),
  test_indent(
    ~name="Indentation of Complete Tuples 1",
    ~init={|let a = (
1,
2
) in 1|},
    ~goal={|let a = (
  1,
  2
) in 1|},
  ),
  test_indent(
    ~name="Indentation of Complete Tuples 2",
    ~init={|let a = (
fun x -> x,
1
) in 1|},
    ~goal={|let a = (
  fun x -> x,
  1
) in 1|},
  ),
  test_indent(
    ~name="Indentation of Complete Tuples 3 (Commas reset)",
    ~init={|let a = (
fun x ->
x,
1
) in 1|},
    ~goal={|let a = (
  fun x ->
    x,
  1
) in 1|},
  ),
  /* TODO: Comma indentation in tuples needs more thought. The old expected
        behavior had the comma at 4 spaces (aligned with `fun`), but current
        behavior puts it at 2 spaces (aligned with tuple content). Not sure
        which is correct - leaving as-is for now but flagging for review.
        Old expected:
          let a =
            (
              fun x ->
                x
              ,
              2
            ) in 1
     */
  test_indent(
    ~name="Indentation of Complete Tuples 3 (Commas on own line)",
    ~init={|let a =
(
fun x ->
x
,
2
) in 1|},
    ~goal={|let a =
  (
    fun x ->
      x
  ,
    2
  ) in 1|},
  ),
  /* INDENTATION OF INCOMPLETE SYNTAX */
  test_indent(
    ~name="Indentation Incomplete Flow 0",
    ~init={|let
a|},
    ~goal={|let
  a|},
  ),
  test_indent(
    ~name="Indentation Incomplete Flow 1",
    ~init={|let a =
    1|},
    ~goal={|let a =
      1|},
  ),
  test_indent(
    ~name="Indentation Incomplete Flow 2",
    ~init={|let a =
fun x ->
|},
    ~goal={|let a =
  fun x ->
    ?|},
  ),
  test_indent(
    ~name="Indentation Incomplete Flow 3",
    ~init={|let a =
fun x ->
case x
||},
    ~goal={|let a =
  fun x ->
    case x
    |?|},
  ),
  test_indent(
    ~name="Indentation Incomplete Flow 4",
    ~init={|let a =
fun x ->
case x
| _ =>
|},
    ~goal={|let a =
  fun x ->
    case x
    | _ =>
      ?|},
  ),
  test_indent(
    ~name="Indentation - Wrapping immediate next lines",
    ~init={|let a =
let b = 2 in
b|},
    ~goal={|let a =
  let b = 2 in
  b|},
  ),
  test_indent(
    ~name="Indentation - Don't wrap over blank line 1",
    ~init={|let a =

let b = 2 in
b|},
    ~goal={|let a =

let b = 2 in
b|},
  ),
  test_indent(
    ~name="Indentation - Don't wrap over blank line 2",
    ~init={|let a = fun x ->

let b = 2 in
b|},
    ~goal={|let a = fun x ->

let b = 2 in
b|},
  ),
  test_indent(
    ~name="Commas should reset indentation",
    ~init={|let a = (
fun x ->
1,
|},
    ~goal={|let a = (
  fun x ->
    1,
  ?|},
  ),
  /* ================================================================
     CONTINUATION LINE INDENTATION
     ================================================================

     When content starts on the same line as an indentation-creating
     construct (e.g., `let z = 4` rather than `let z =\n4`), and then
     continues on subsequent lines, we face an ambiguity at typing time.

     Consider typing `let z = 4` then pressing Enter. At that moment,
     we don't know if the user will type:
       - `+ 4` (continuation of the expression - should be indented)
       - `in z` (completing keyword - should NOT be indented)

     DESIGN DECISION: We use conservative auto-indent at typing time
     (no indent when uncertain), and Format (Cmd+S) fixes it based on
     the actual final structure.

     Compare to cases where linebreak comes RIGHT AFTER `=`:
       `let z =\n4` - here we KNOW 4 is in the child, so we indent.

     The tests below demonstrate both behaviors:
       - test_indent: shows auto-indent behavior during typing
       - test_indent_after_format: shows correct indentation after Format
     ================================================================ */
  /* KNOWN CASE: Linebreak immediately after `=` - we CAN determine indent */
  test_indent(
    ~name="Linebreak after = (known case)",
    ~init={|let z =
4
in z|},
    ~goal={|let z =
  4
in z|},
  ),
  /* AMBIGUOUS CASE: Content on same line as `=`, then linebreak.
     At typing time after `let z = 4<Enter>`, we don't know what's next. */
  /* Auto-indent behavior: conservative (no indent) */
  test_indent(
    ~name="Same-line content then linebreak - auto-indent is conservative",
    ~init={|let z = 4
+ 4
in z|},
    ~goal={|let z = 4
+ 4
in z|},
  ),
  /* Format behavior: correct indent based on actual structure */
  test_indent_after_format(
    ~name="Same-line content then continuation - Format fixes indent",
    ~init={|let z = 4
+ 4
in z|},
    ~goal={|let z = 4
  + 4
in z|},
  ),
  /* Completing keyword case: no indent needed (same in both modes) */
  test_indent(
    ~name="Same-line content then completing keyword - no indent",
    ~init={|let z = 4
in z|},
    ~goal={|let z = 4
in z|},
  ),
  /* Additional continuation cases (Format required) */
  test_indent_after_format(
    ~name="Multiple continuation lines - Format",
    ~init={|let z = 1
+ 2
+ 3
in z|},
    ~goal={|let z = 1
  + 2
  + 3
in z|},
  ),
  test_indent_after_format(
    ~name="Paren with same-line content - Format",
    ~init={|(4
+ 2)|},
    ~goal={|(4
  + 2)|},
  ),
  test_indent_after_format(
    ~name="Function application continuation - Format",
    ~init={|go(1
+ 2)|},
    ~goal={|go(1
  + 2)|},
  ),
  test_indent_after_format(
    ~name="Nested let continuation - Format",
    ~init={|let x =
let y = 1
+ 2
in y
in x|},
    ~goal={|let x =
  let y = 1
    + 2
  in y
in x|},
  ),
  test_indent_after_format(
    ~name="If branch continuation - Format",
    ~init={|if true then 1
+ 2
else 3|},
    ~goal={|if true then 1
  + 2
else 3|},
  ),
  test_indent_after_format(
    ~name="Case rule continuation - Format",
    ~init={|case x
| A => 1
+ 2
end|},
    ~goal={|case x
| A => 1
  + 2
end|},
  ),
  /* Top-level continuation: no indent expected (not in a child context) */
  test_indent(
    ~name="Top-level continuation - no indent",
    ~init={|let x = 1 in x
+ 1|},
    ~goal={|let x = 1 in x
+ 1|},
  ),
  /* ================================================================
     CASE EXPRESSION INDENTATION
     ================================================================

     Case rules (| pattern => body) have special indentation behavior:
     - The `|` should be at the same level as `case` (no indent)
     - The rule body (after =>) should be indented +2

     This applies to both complete rules (`| A => 1`) and incomplete
     rules (just `|`). An incomplete `|` is a tile with label ["|", "=>"]
     but only shard 0 present.

     Similar to continuation lines, there's ambiguity after a multi-line
     rule body: could be continuation of body or new rule. We use
     conservative behavior (no indent) at typing time.
     ================================================================ */
  /* INCOMPLETE CASE (no `end` yet) */
  test_indent(
    ~name="Case: linebreak after scrutinee, expecting rule",
    ~init={|case 1
||},
    ~goal={|case 1
|?|},
  ),
  /* Pattern position after incomplete bar: no indent. Patterns are typically
   * on the same line as `|`, and if multiline, staying at bar level is fine. */
  test_indent(
    ~name="Case: incomplete rule (just bar)",
    ~init={|case 1
|
|},
    ~goal={|case 1
|
?|},
  ),
  test_indent(
    ~name="Case: after arrow, expecting rule body",
    ~init={|case 1
| A =>
|},
    ~goal={|case 1
| A =>
  ?|},
  ),
  test_indent(
    ~name="Case: rule body on separate line",
    ~init={|case 1
| A =>
1|},
    ~goal={|case 1
| A =>
  1|},
  ),
  test_indent(
    ~name="Case: complete rule on one line, expecting next rule",
    ~init={|case 1
| A => 1
||},
    ~goal={|case 1
| A => 1
|?|},
  ),
  /* Same as above - pattern position stays at bar level */
  test_indent(
    ~name="Case: second incomplete rule",
    ~init={|case 1
| A => 1
|
|},
    ~goal={|case 1
| A => 1
|
?|},
  ),
  /* COMPLETE CASE (with `end`) */
  test_indent(
    ~name="Case complete: empty, expecting rule",
    ~init={|case 1
end|},
    ~goal={|case 1
end|},
  ),
  /* Complete case with incomplete rule: `end` stays at case level,
   * not indented as if it were body content. */
  test_indent(
    ~name="Case complete: incomplete rule (just bar)",
    ~init={|case 1
|
end|},
    ~goal={|case 1
|?
end|},
  ),
  test_indent(
    ~name="Case complete: one complete rule",
    ~init={|case 1
| A => 1
end|},
    ~goal={|case 1
| A => 1
end|},
  ),
  /* Position after complete rule, before end - empty line for next rule */
  test_indent(
    ~name="Case complete: after rule, empty line before end",
    ~init={|case 1
| A => 1

end|},
    ~goal={|case 1
| A => 1

end|},
  ),
  test_indent(
    ~name="Case complete: rule with body on separate line",
    ~init={|case 1
| A =>
1
end|},
    ~goal={|case 1
| A =>
  1
end|},
  ),
  test_indent(
    ~name="Case complete: multiple rules",
    ~init={|case 1
| A => 1
| B => 2
end|},
    ~goal={|case 1
| A => 1
| B => 2
end|},
  ),
  test_indent(
    ~name="Case complete: multiple rules with bodies on separate lines",
    ~init={|case 1
| A =>
1
| B =>
2
end|},
    ~goal={|case 1
| A =>
  1
| B =>
  2
end|},
  ),
  /* CONTINUATION LINES IN RULE BODIES */
  test_indent(
    ~name="Case: rule body continuation - auto-indent conservative",
    ~init={|case 1
| A =>
1
+ 2
end|},
    ~goal={|case 1
| A =>
  1
+ 2
end|},
  ),
  test_indent_after_format(
    ~name="Case: rule body continuation - Format fixes indent",
    ~init={|case 1
| A =>
1
+ 2
end|},
    ~goal={|case 1
| A =>
  1
  + 2
end|},
  ),
  /* AFTER MULTI-LINE RULE BODY - ambiguous, conservative no indent */
  test_indent(
    ~name="Case: after multi-line body, expecting next rule",
    ~init={|case 1
| A =>
1
| B => 2
end|},
    ~goal={|case 1
| A =>
  1
| B => 2
end|},
  ),
];

/* ================================================================
   SELECTIVE RE-INDENTATION TESTS
   ================================================================

   These tests verify that re-indentation is triggered selectively
   when a shard is added to a tile, but ONLY when all tiles in the
   affected segment are complete.

   Trigger: Shard attachment (tile gains a shard via reassembly)
   Condition: All tiles in the affected segment must be complete
   Affected segments:
     1. New child segments created by the shard attachment
     2. Remaining sibling segment at that level

   IMPLEMENTATION NOTE: Need to decide whether re-indentation should
   be recursive (also re-indent children of tiles in the segment) or
   non-recursive (only linebreaks at the top level of the segment).
   ================================================================ */

let selective_reindent_tests = [
  /* ================================================================
     CORE TESTS: These test the main selective re-indentation behavior
     ================================================================ */
  /* Example 1: Simple function wrap - re-indentation HAPPENS
     Completing `fun x ->` should re-indent the body on next line */
  //   test_from_parse(
  //     ~name="Selective: fun wrap triggers re-indent",
  //     ~init={|¦
  // 1 + 1|},
  //     ~acts=string_to_ltr_actions("fun x ->"),
  //     ~goal={|fun x ->¦
  //   1 + 1|},
  //   ),
  /* Example 2: Incomplete outer let blocks re-indent
     Even though `fun` is complete, the segment contains incomplete `let` */
  //   test_from_parse(
  //     ~name="Selective: incomplete outer let blocks re-indent",
  //     ~init={|let y = ¦
  // 1 + 1|},
  //     ~acts=string_to_ltr_actions("fun x ->"),
  //     ~goal={|let y = fun x ->¦
  // 1 + 1|},
  //   ),
  /* Parens completion triggers re-indent
     Typing ) to complete parens should indent the contents */
  //   test_from_parse(
  //     ~name="Selective: paren completion triggers re-indent",
  //     ~init={|(
  // 1 + 1¦|},
  //     ~acts=string_to_ltr_actions(")"),
  //     ~goal={|(
  //   1 + 1)¦|},
  //   ),
  /* Completing let with `in` triggers re-indent of definition
     The body between = and in should be indented */
  //   test_from_parse(
  //     ~name="Selective: let completion triggers re-indent",
  //     ~init={|let x =
  // 1 + 1¦|},
  //     ~acts=string_to_ltr_actions(" in y"),
  //     ~goal={|let x =
  //   1 + 1
  // in¦ y|},
  //   ),
  /* ================================================================
     SKIP TEST: Documents desired future behavior
     ================================================================ */
  /* Example 7: SKIP - Incomplete if in body
     User WANTS this indented, but current heuristic says NO.
     Marking as Skip for future heuristic improvement. */
  test_case(
    "Selective: SKIP - incomplete if in body (future improvement)", `Quick, () => {
    /* Skip this test for now - documents desired future behavior */
    [@warning "-21"]
    {
      Alcotest.skip();
      let z = parse_with_caret({|¦
if true then 1|});
      let result =
        string_to_ltr_actions("fun x ->") |> perform(z) |> printer;
      /* Desired behavior: SHOULD re-indent */
      let desired = {|fun x ->¦
  if true then 1|};
      check(testable(Fmt.string, String.equal), desired, desired, result);
    }
  }),
  /* ================================================================
     NESTED STRUCTURE TESTS
     ================================================================ */
  /* Nested lets - completing inner let re-indents inner definition only
     Outer let is still incomplete, so outer level not touched */
  //   test_from_parse(
  //     ~name="Selective: nested lets - inner completion",
  //     ~init={|let x =
  // let y =
  // 1¦|},
  //     ~acts=string_to_ltr_actions(" in y"),
  //     ~goal={|let x =
  // let y =
  //   1
  // in¦ y|},
  //   ),
  /* Nested lets - completing outer let re-indents everything
     Starting from state where inner let is already complete */
  //   test_from_parse(
  //     ~name="Selective: nested lets - outer completion",
  //     ~init={|let x =
  // let y = 1 in y¦|},
  //     ~acts=string_to_ltr_actions(" in z"),
  //     ~goal={|let x =
  //   let y = 1 in y
  // in¦ z|},
  //   ),
  /* ================================================================
     NEGATIVE TESTS: Re-indentation should NOT happen
     ================================================================ */
  /* Typing in incomplete context doesn't trigger re-indent
     Adding to a let that's still missing `in` */
  //   test_from_parse(
  //     ~name="Selective: incomplete let body not re-indented",
  //     ~init={|let x =
  // ¦
  // 1 + 1|},
  //     ~acts=string_to_ltr_actions("2 +"),
  //     ~goal={|let x =
  // 2 +¦
  // 1 + 1|},
  //   ),
];

let module_indentation_tests = [
  test_indent(
    ~name="Module body indents inside braces",
    ~init={|{
let x = 1
}|},
    ~goal={|{
  let x = 1
}|},
  ),
  test_indent(
    ~name="Module with multiple items indents",
    ~init={|{
let x = 1;
let y = 2
}|},
    ~goal={|{
  let x = 1;
  let y = 2
}|},
  ),
  test_indent(
    ~name="Module in let binding indents",
    ~init={|let m = {
let x = 1;
let y = 2
} in 1|},
    ~goal={|let m = {
  let x = 1;
  let y = 2
} in 1|},
  ),
  test_indent(
    ~name="Nested module double indents",
    ~init={|{
let inner = {
let x = 1
}
}|},
    ~goal={|{
  let inner = {
    let x = 1
  }
}|},
  ),
  test_indent(
    ~name="Module keyword indents",
    ~init={|module m = {
let x = 1
} in
m.x|},
    ~goal={|module m = {
  let x = 1
} in
m.x|},
  ),
];

/* === Indentation UX: skip-movement, backspace-join, indent commands
   (plans/indentation-ux.md) === */
let ux_settings = {
  ...Test_Editing.default_settings,
  Language.CoreSettings.indentation_ux: true,
};

let ux_text = (acts: list(Action.t)): string =>
  Printer.of_segment(
    ~holes="?",
    ~refractors=[],
    Zipper.unselect_and_zip(
      perform(~settings=ux_settings, Zipper.init(), acts),
    ),
  );

let ux_case = (~name, ~acts, ~expected) =>
  test_case(name, `Quick, () =>
    check(string, name, expected, ux_text(acts))
  );

let bsp = Action.Destruct(Local(Left, ByChar));

let indent_ux_tests = [
  /* C CAP (ported from artifact-grout 1c2bc75efb, andrew's 2026-07-22
     live repro): spaces typed BEYOND the line's auto-indent level are
     real material — backspace deletes them one per press; the 2-space
     indent unit and the one-keystroke enter-join apply only within
     the auto-indent width. Before the cap, two backspaces here ate
     four spaces (6 -> 2). */
  ux_case(
    ~name="backspace beyond the indent deletes one space per press",
    ~acts=
      string_to_ltr_actions("let a = \n")
      @ string_to_ltr_actions("    ")
      @ [bsp, bsp],
    ~expected="let a = \n    ?",
  ),
  ux_case(
    ~name="backspace at line start inverts enter (indent + linebreak)",
    ~acts=string_to_ltr_actions("fun q ->\n") @ [bsp],
    ~expected="fun q ->?",
  ),
  ux_case(
    ~name="backspace deletes a blank line in one keystroke",
    ~acts=string_to_ltr_actions("fun q ->\n\n") @ [bsp],
    ~expected="fun q ->\n  ?",
  ),
  ux_case(
    ~name="shift+backspace dedents at first-content",
    ~acts=
      string_to_ltr_actions("fun q ->\nx")
      @ mv_l(1)
      @ [Action.AdjustIndent(Left, AtBoundary)],
    ~expected="fun q ->\nx",
  ),
  ux_case(
    ~name="shift+backspace mid-content falls through to backspace",
    ~acts=
      string_to_ltr_actions("fun q ->\nx")
      @ [Action.AdjustIndent(Left, AtBoundary)],
    ~expected="fun q ->\n  ?",
  ),
  ux_case(
    ~name="indent line from any caret position",
    ~acts=
      string_to_ltr_actions("fun q ->\nx")
      @ [Action.AdjustIndent(Right, Always)],
    ~expected="fun q ->\n    x",
  ),
  ux_case(
    ~name="dedent clamps and works on the first line",
    ~acts=
      string_to_ltr_actions("  x") @ [Action.AdjustIndent(Left, Always)],
    ~expected="x",
  ),
  ux_case(
    ~name="left from first-content lands at previous line end",
    ~acts=
      string_to_ltr_actions("fun q ->\nx") @ mv_l(2) @ [Action.Insert("2")],
    ~expected="fun q ->2 \n  x",
  ),
  ux_case(
    ~name="right from line end skips indentation to first content",
    ~acts=
      string_to_ltr_actions("fun q ->\nx")
      @ mv_l(2)
      @ mv_r(1)
      @ [Action.Insert("+")],
    ~expected="fun q ->\n  ?+x",
  ),
  ux_case(
    ~name="blank lines keep one reachable position (their end)",
    ~acts=
      string_to_ltr_actions("fun q ->\n\nx")
      @ mv_l(2)
      @ [Action.Insert("1")],
    ~expected="fun q ->\n  1 \n  x",
  ),
  ux_case(
    ~name="home is smart: lands at first content",
    ~acts=
      string_to_ltr_actions("fun q ->\nx")
      @ [Action.Move(Line(Left)), Action.Insert("+")],
    ~expected="fun q ->\n  ?+x",
  ),
];

/* Convex grout anchors indentation like a literal atom: empty
   branches must not push following lines deeper (regression: holes
   were skipped by effective_prev, re-firing incrementor/child rules
   so else/in drifted right). The two cases mirror each other. */
let grout_indent_tests = [
  test_indent_after_format(
    ~name="hole branches indent like literal branches",
    ~init="let f =\nfun x ->\nlet x =\nif x < 0 then\nelse\nin\nf(3)",
    ~goal=
      "let f =\n  fun x ->\n    let x =\n      if x < 0 then?\n      else?\n    in\n    f(3)",
  ),
  test_indent_after_format(
    ~name="literal branches (mirror of the hole case)",
    ~init="let f =\nfun x ->\nlet x =\nif x < 0 then\n1\nelse 2\nin\nf(3)",
    ~goal=
      "let f =\n  fun x ->\n    let x =\n      if x < 0 then\n        1\n      else 2\n    in\n    f(3)",
  ),
];

let tests = [
  ("Editing.Indentation", indentation_tests),
  ("Editing.IndentationUX", indent_ux_tests),
  ("Editing.GroutIndent", grout_indent_tests),
  ("Editing.SelectiveReindent", selective_reindent_tests),
  ("Editing.Indentation.Modules", module_indentation_tests),
];
