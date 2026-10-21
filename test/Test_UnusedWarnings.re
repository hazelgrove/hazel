open Alcotest;
open Language;
open Test_Statics_Prelude;

/* Extract the set of variable names that have UnusedVar warnings */
let unused_warnings = (s: Statics.Map.t): list(string) =>
  Id.Map.fold(
    (_id, info: Info.t, acc) =>
      switch (Info.warnings_of(info)) {
      | [Pat(UnusedVar(name))] => [name, ...acc]
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
    check_unused(
      "theorem: declaration needs no reference",
      "theorem t = true in 0",
      [],
    ),
    check_unused(
      "theorem: referenced declaration",
      "theorem t = true in t",
      [],
    ),
    check_unused(
      "theorem: shadowed declarations",
      "theorem t = true in theorem t = true in 0",
      [],
    ),
    check_unused(
      "theorem: invalid pattern bindings are exempt too",
      "theorem (x, y) = true in 0",
      [],
    ),
    check_unused(
      "theorem: ordinary bindings still warn",
      "let x = 1 in theorem t = true in fun y -> 0",
      ["x", "y"],
    ),
    check_unused(
      "theorem: exemption does not use an outer binding",
      "let t = 1 in theorem t = true in 0",
      ["t"],
    ),
    check_unused(
      "theorem: statement uses outer binding",
      "let x = 1 in theorem t = x == x in 0",
      [],
    ),
    check_unused("forall: used binder", "forall (x : Int) -> x == x", []),
    check_unused("forall: unused binder", "forall (x : Int) -> true", ["x"]),
    check_unused(
      "forall: tuple partial use",
      "forall (x : Int, y : Int) -> x == x",
      ["y"],
    ),
    check_unused(
      "forall: nested binders both used",
      "forall (x : Int) -> forall (y : Int) -> x == y",
      [],
    ),
    check_unused(
      "forall: nested unused inner binder",
      "forall (x : Int) -> forall (y : Int) -> x == x",
      ["y"],
    ),
    check_unused(
      "forall: shadowed outer binder unused",
      "forall (x : Int) -> forall (x : Int) -> x == x",
      ["x"],
    ),
    check_unused(
      "forall: both shadowed binders unused",
      "forall (x : Int) -> forall (x : Int) -> true",
      ["x", "x"],
    ),
    check_unused(
      "forall: let shadows binder",
      "forall (x : Int) -> let x = 1 in x == x",
      ["x"],
    ),
    check_unused(
      "forall: bound use does not escape",
      "let x = 1 in forall (x : Int) -> x == x",
      ["x"],
    ),
    check_unused(
      "forall: free use reaches outer binding",
      "let x = 1 in forall (y : Int) -> x == y",
      [],
    ),
    check_unused(
      "forall: hole suppresses warning",
      "forall (x : Int) -> ?",
      [],
    ),
    test_case(
      "list theorem example has no warnings",
      `Quick,
      () => {
        /* Standalone copy of the theorem-refactor branch's list example. */
        let input = {|# Lists: map, sum, and associativity. All equations work both ways. #
let inc(n : Int) = n + 1 in
let double(n : Int) = n * 2 in
let sum(items : [Int]) = fold_left(items, int_plus, 0) in

theorem map_fusion =
  forall (items : [Int]) ->
  forall (f : Int -> Int) -> forall (g : Int -> Int) ->
    map(map(items, f), g) == map(items, fun x -> g(f(x)))
in

theorem map_append =
  forall (left : [Int]) -> forall (right : [Int]) ->
  forall (f : Int -> Int) ->
    map(left @ right, f) == map(left, f) @ map(right, f)
in

theorem sum_append =
  forall (left : [Int]) -> forall (right : [Int]) ->
    sum(left @ right) == sum(left) + sum(right)
in

theorem list_associativity =
  forall (a : [Int]) -> forall (b : [Int]) -> forall (c : [Int]) ->
    (a @ b) @ c == a @ (b @ c)
in

theorem add_associativity =
  forall (a : Int) -> forall (b : Int) -> forall (c : Int) ->
    (a + b) + c == a + (b + c)
in

theorem sum_double =
  forall (items : [Int]) ->
    sum(map(items, double)) == double(sum(items))
in

let xs = [1, 2, 3] in
let ys = [4, 5] in
let zs = [6] in

# Start with a small example, then combine the same moves below. #
(
  fuse_maps = map(map(xs, inc), double),
  distribute_map = map(xs @ ys, double),
  split_sum = sum(xs @ ys),
  regroup_lists = (xs @ ys) @ zs,
  regroup_sums = (sum(xs) + sum(ys)) + sum(zs),
  pull_out_double = sum(map(xs, double)),

  # Several routes: fuse maps, distribute over @, split sums, regroup. #
  playground = sum(map(map((xs @ ys) @ zs, inc), double))
)
|};
        let s = statics(parse_exp(input));
        check(int, "static error count", 0, List.length(errors(s)));
        let warnings =
          Id.Map.fold(
            (_id, info, acc) => Info.warnings_of(info) @ acc,
            s,
            [],
          );
        check(int, "warning count", 0, List.length(warnings));
      },
    ),
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
    /* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
       MODULE: EXPRESSION DOT ACCESS
       ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ */
    check_unused(
      "module: used via expression dot access",
      "module M = { let x = 1 } in M.x",
      [],
    ),
    check_unused(
      "module: let binding used via expression dot access",
      "let m = { let x = 1 } in m.x",
      [],
    ),
    /* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
       MODULE: QUALIFIED TYPE ACCESS
       ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ */
    check_unused(
      "module: used via qualified type in pattern annotation",
      "module M = { type T = Int } in let x : M.T = 5 in x",
      [],
    ),
    check_unused(
      "module: used via qualified type in expression ascription",
      "module M = { type T = Int } in (5 : M.T) + 0",
      [],
    ),
    check_unused(
      "module: used via qualified type in type alias",
      "module M = { type T = Int } in type D = M.T in 5",
      [],
    ),
    /* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
       MODULE: UNUSED
       ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ */
    check_unused(
      "module: unused module",
      "module M = { let x = 1 } in 5",
      ["M"],
    ),
    /* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
       SIGNATURES: NO WARNINGS
       ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ */
    check_unused(
      "sig: variables in signatures never have unused warnings",
      "let m : { let x : Int } = { let x = 1 } in m.x",
      [],
    ),
    check_unused(
      "sig: signature vars not flagged even when module is unused",
      "let m : { let x : Int } = { let x = 1 } in 5",
      ["m"],
    ),
    /* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
       TUPLE EXTENSION: CO-CONTEXT PROPAGATION
       The ... operator should propagate co-contexts
       from both operands.
       ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ */
    check_unused(
      "tuple extension: variable used in base of spread",
      "let x = (a=1, b=2) in x ... (a=3)",
      [],
    ),
    check_unused(
      "tuple extension: variable used in extension of spread",
      "let x = 1 in (a=1, b=2) ... (a=x)",
      [],
    ),
    check_unused(
      "tuple extension: fun param used in spread",
      "fun e -> e ... (a=1)",
      [],
    ),
    check_unused(
      "tuple extension: unused variable not in spread",
      "let x = 1 in (a=1, b=2) ... (a=3)",
      ["x"],
    ),
    check_unused(
      "tuple extension: one used one unused",
      "let x = (a=1, b=2) in let y = 1 in x ... (a=3)",
      ["y"],
    ),
    check_unused(
      "tuple extension: both operands use different variables",
      "let x = (a=1, b=2) in let y = 3 in x ... (a=y)",
      [],
    ),
  ],
);
