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

/* The theorem pattern `t`'s own Info, from statics of `input`. */
let theorem_binder = (input, name): Info.pat => {
  let found =
    Id.Map.fold(
      (_id, info: Info.t, acc) =>
        switch (info) {
        | InfoPat({user_term: {term: Var(n), _}, _} as p) when n == name =>
          Some(p)
        | _ => acc
        },
      statics(parse_exp(input)),
      None,
    );
  switch (found) {
  | Some(p) => p
  | None => fail("no pattern info for " ++ name)
  };
};

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
    check_unused(
      "theorem: statement uses outer binding (invalid pattern)",
      "let x = 1 in theorem (a, b) = x == x in 0",
      [],
    ),
    test_case(
      "theorem: exemption does not record its name as a use",
      `Quick,
      () => {
        let t = theorem_binder("theorem t = true in 0", "t");
        check(
          bool,
          "t is not in its own co_ctx",
          false,
          CoCtx.has_any(t.co_ctx, ["t"]),
        );
      },
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
    /* Funext binders name the arbitrary argument of the pointwise goal;
       the proof need not mention them. */
    check_unused(
      "funext: binder not flagged unused in a hole body",
      "theorem t = (fun x -> x) == (fun x -> x) proof funext y => ? in t",
      [],
    ),
    check_unused(
      "theorem with a proof: a statement binder the statement uses",
      "theorem t = forall x : [Int] -> x == x @ [] proof ? in t",
      [],
    ),
    check_unused(
      "theorem with a proof: an unused statement binder still warns",
      "theorem t = forall (x : Int) -> true proof ? in t",
      ["x"],
    ),
    check_unused(
      "proof: induction case binders",
      {|theorem t = forall x : [Int] -> x == x @ [] proof
induction x
| [] => from [] @ [] do step end; from $e do step end
| u :: us => from u :: us @ [] do step end
end in t|},
      [],
    ),
    check_unused(
      "proof: forall and funext binders",
      "theorem t = forall (x : Int) -> (fun a -> a + x) == (fun b -> x + b) proof forall x => funext z => ? in t",
      [],
    ),
    check_unused(
      "proof: fact names",
      "theorem t = forall (x : Int) -> x == 1 ==> x == 1 proof forall x => assume x == 1 as h => alias k = h => ? in t",
      [],
    ),
    check_unused(
      "proof: an ordinary binding still warns",
      "let y = 1 in theorem t = forall (x : Int) -> x == x proof forall x => ? in t",
      ["y"],
    ),
  ],
);
