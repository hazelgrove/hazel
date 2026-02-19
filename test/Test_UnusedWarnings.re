open Alcotest;
open Language;
open Test_Statics_Prelude;

/* Extract the set of variable names that have UnusedVar warnings */
let unused_warnings = (s: Statics.Map.t): list(string) =>
  Id.Map.fold(
    (_id, info: Info.t, acc) =>
      switch (Info.warning_of(info)) {
      | WarningPat(UnusedVar(name)) => [name, ...acc]
      | _ => acc
      },
    s,
    [],
  )
  |> List.sort(String.compare);

let check_unused = (name, input, expected_unused) =>
  test_case(
    name,
    `Quick,
    () => {
      let exp = parse_exp(input);
      let s = statics(exp);
      let actual = unused_warnings(s);
      let expected = List.sort(String.compare, expected_unused);
      check(list(string), name, expected, actual);
    },
  );

let tests = (
  "Unused Variable Warnings",
  [
    /* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
       LET BINDINGS
       ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ */
    check_unused("let: used variable", "let x = 1 in x + 1", []),
    check_unused("let: unused variable", "let x = 1 in 2", ["x"]),
    check_unused(
      "let: underscore-prefixed suppresses warning",
      "let _x = 1 in 2",
      [],
    ),
    check_unused(
      "let: multiple bindings, one unused",
      "let x = 1 in let y = 2 in x",
      ["y"],
    ),
    check_unused(
      "let: multiple bindings, both unused",
      "let x = 1 in let y = 2 in 3",
      ["x", "y"],
    ),
    check_unused(
      "let: tuple destructuring, partial use",
      "let (x, y) = (1, 2) in x",
      ["y"],
    ),
    check_unused(
      "let: tuple destructuring, both used",
      "let (x, y) = (1, 2) in x + y",
      [],
    ),
    /* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
       LET: HOLES SUPPRESS WARNINGS
       If the body contains a hole, we don't know
       what it will eventually become — it might use
       the variable. So suppress the warning.
       ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ */
    check_unused(
      "let: hole in body suppresses warning",
      "let x = 1 in 0 + ?",
      [],
    ),
    /* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
       LET: UNDERSCORE PREFIX IN PATTERNS
       ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ */
    check_unused(
      "let: underscore-prefixed pattern suppresses warning",
      "let _x = 1 in 2",
      [],
    ),
    /* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
       FUNCTIONS
       ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ */
    check_unused("fun: used parameter", "fun x -> x + 1", []),
    check_unused("fun: unused parameter", "fun x -> 1", ["x"]),
    check_unused(
      "fun: underscore-prefixed parameter suppresses warning",
      "fun _x -> 1",
      [],
    ),
    /* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
       CASE: BASIC CO-CONTEXT BEHAVIOR
       Each branch's pattern should be checked against
       only its own branch body's co-context.
       ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ */
    check_unused(
      "case: all branches use their variable",
      "let s = 1 in case s | x => x + 1 | y => y + 2 end",
      [],
    ),
    check_unused(
      "case: one branch unused",
      "let s = 1 in case s | x => x + 1 | y => 0 end",
      ["y"],
    ),
    check_unused(
      "case: all branches unused",
      "let s = 1 in case s | x => 0 | y => 1 end",
      ["x", "y"],
    ),
    /* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
       CASE: CO-CONTEXT INDEPENDENCE BETWEEN BRANCHES
       A variable used in branch 1 should NOT suppress
       the warning for the same-named variable in branch 2.
       ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ */
    check_unused(
      "case: same var name, used in first branch only",
      "let s = 1 in case s | x => x | x => 0 end",
      ["x"],
    ),
    /* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
       CASE: OUTER CO-CONTEXT PROPAGATION
       Pattern-bound variables should NOT leak into
       the outer co-context. An outer let binding that
       is only referenced inside a match branch body
       via a same-named pattern var should still be
       considered unused at the outer level.
       ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ */
    check_unused(
      "case: outer variable shadowed by pattern, outer is unused",
      "let x = 1 in let s = 2 in case s | x => x end",
      ["x"],
    ),
    /* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
       CASE: CONSTRUCTOR PATTERNS
       ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ */
    check_unused(
      "case: constructor pattern, payload used",
      "type T = A(Int) + B(Int) in let s : T = A(1) in case s | A(x) => x | B(y) => y end",
      [],
    ),
    check_unused(
      "case: constructor pattern, payload unused",
      "type T = A(Int) + B(Int) in let s : T = A(1) in case s | A(x) => 0 | B(y) => 1 end",
      ["x", "y"],
    ),
    check_unused(
      "case: constructor pattern, one payload unused",
      "type T = A(Int) + B(Int) in let s : T = A(1) in case s | A(x) => x | B(y) => 0 end",
      ["y"],
    ),
  ],
);
