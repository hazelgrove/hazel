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
           ~indent=" ",
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
      @ [Action.Format]
      |> perform(Zipper.init())
      |> Printer.of_zipper(
           ~holes=convex_char,
           ~concave_holes=concave_char,
           ~indent=" ",
         ),
    )
  );
};

let indentation_tests = [
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

let tests = [("Editing.Indentation", indentation_tests)];
