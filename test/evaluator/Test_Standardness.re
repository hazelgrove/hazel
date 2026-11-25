open Alcotest;
open Language;
open Test_ProofMap.Harness;

/* What a proof can establish and use about `!!`, "is standard". */

let proven = (src: string): bool => {
  let (pm, proof) = run_named("t", src);
  switch (ProofMap.full_status_of_proof(pm, proof)) {
  | Proven => true
  | ProvenModulo(_)
  | Refuted
  | Incomplete => false
  };
};

let named_binder_facts = [
  test_case("forall (x : T !! h) names the fact !!x", `Quick, () =>
    check(
      bool,
      "proven",
      true,
      proven(
        "theorem t = forall x : Bool -> !!x proof generalize x => forall (x : Bool !! h) => rewrite h end in t",
      ),
    )
  ),
  test_case("a function binder's fact says it is total", `Quick, () =>
    check(
      bool,
      "proven",
      true,
      proven(
        "theorem t = forall f : (Int -> Int) -> !!f proof generalize f => forall (f : (Int -> Int) !! hf) => rewrite hf end in t",
      ),
    )
  ),
  test_case("the fact follows a renamed binder", `Quick, () =>
    check(
      bool,
      "proven",
      true,
      proven(
        "theorem t = forall x : Bool -> !!x proof generalize x => forall (y !! h) => rewrite h end in t",
      ),
    )
  ),
  test_case("without a name, the forall step gives no fact", `Quick, () =>
    check(
      bool,
      "not proven",
      false,
      proven(
        "theorem t = forall x : Bool -> !!x proof generalize x => forall x => ? in t",
      ),
    )
  ),
];

/* The routine on a closed program, elaborated: `Some(conditions)` when
 * it shows the program standard wherever they hold. */
let routine = (src: string): option(list(string)) => {
  let ctx = Builtins.ctx_init(Some(Int));
  let (_, elab) =
    Statics.mk(CoreSettings.on, ctx, Test_Evaluator_Prelude.parse_exp(src));
  switch (
    Standardness.check(
      ~ctx,
      ~env=Builtins.env_init,
      ~is_standard=_ => false,
      elab,
    )
  ) {
  | Standard(conditions) => Some(List.map(~f=print_exp, conditions))
  | Unknown(_) => None
  };
};

let judged = (what, expected, src) =>
  test_case(what, `Quick, () =>
    check(option(list(string)), src, expected, routine(src))
  );

let routine_tests = [
  judged("data of standard parts", Some([]), "(1, [2, 3], true)"),
  judged(
    "a total operation on typed operands",
    Some([]),
    "fun (x : Int) -> x + 1",
  ),
  judged(
    "an operand of unknown type is not shown standard",
    None,
    "fun x -> x + 1",
  ),
  judged(
    "a division gives its domain condition",
    Some(["2 != 0"]),
    "fun (x : Int) -> x / 2",
  ),
  judged(
    "a lambda with a condition on its argument isn't standard",
    None,
    "fun (x : Int) -> 1 / x",
  ),
  judged(
    "applied, the lambda's condition takes the argument",
    Some(["2 != 0"]),
    "(fun (x : Int) -> 1 / x)(2)",
  ),
  judged(
    "a structurally recursive function and its application",
    Some([]),
    "let len : [Int] -> Int = fun xs -> case xs | [] => 0 | _ :: t => 1 + len(t) end in len([1, 2])",
  ),
  judged(
    "a recursive function that may loop",
    None,
    "let f = fun (n : Int) -> f(n) in f(1)",
  ),
  judged(
    "a builtin with a known domain condition",
    Some(["2 != 0"]),
    "int_mod(7, 2)",
  ),
  judged(
    "a builtin that may fail some other way",
    None,
    {|int_of_string("1")|},
  ),
  judged(
    "a total builtin on an argument of its type",
    Some([]),
    "fun (x : Float) -> abs_float(x)",
  ),
  judged(
    "a total builtin on an argument of unknown type",
    None,
    "fun x -> abs_float(x)",
  ),
  judged(
    "a standard function on an argument of its domain",
    Some([]),
    "let f : Int -> Int = fun n -> n + 1 in fun (x : Int) -> f(x)",
  ),
  judged(
    "a standard function on an argument of unknown type",
    None,
    "let f : Int -> Int = fun n -> n + 1 in let g = fun (h : Int -> Int) -> fun x -> h(x) in g",
  ),
  judged(
    "a branch's condition holds under its guard",
    Some(["! true ==> 0 != 0"]),
    "if true then 1 else 1 / 0",
  ),
  judged(
    "a guard that implies the condition discharges it",
    Some([]),
    "fun (a : Int) -> fun (b : Int) -> if b == 0 then 0 else a / b",
  ),
  judged(
    "and through `!` and `&&`",
    Some([]),
    "fun (a : Int) -> fun (b : Int) -> if !(b <= 0) && a > 1 then a / b else 0",
  ),
  judged(
    "a guard that doesn't is kept with the condition",
    Some(["9 > 5 ==> 9 - 7 != 0"]),
    "(fun (c : Int) -> if c > 5 then 10 / (c - 7) else 0)(9)",
  ),
  judged(
    "a case's literal rules guard the later rules",
    Some([]),
    "fun (n : Int) -> case n | 0 => 0 | m => 10 / m end",
  ),
  judged(
    "a case covering its scrutinee",
    Some([]),
    "case true | true => 1 | false => 2 end",
  ),
  judged("a case that may not cover", None, "case 1 | 0 => true end"),
  judged(
    "equality of functions",
    None,
    "(fun (x : Int) -> x) == (fun (x : Int) -> x)",
  ),
  judged(
    "a forall with a standard body",
    Some([]),
    "forall (x : Int) -> x + 1 > x",
  ),
];

/* The routine on `goal` in the scope of a theorem after `definitions`,
 * with the environment and typing context the checker has there. */
let routine_in = (definitions: string, goal: string): option(list(string)) => {
  let (pm, proof) =
    run_named(
      "t",
      definitions
      ++ " theorem t = "
      ++ goal
      ++ " == "
      ++ goal
      ++ " proof ? in t",
    );
  switch (ProofMap.lookup(Proof.rep_id(proof), pm)) {
  | Some({incoming: Some(statement), ctx, _}) =>
    switch (Exp.term_of(statement)) {
    | BinOp(Poly(Equals), l, _) =>
      switch (
        Standardness.check(
          ~ctx=SemanticCtx.get_ctx(ctx),
          ~env=SemanticCtx.get_env(ctx),
          ~is_standard=_ => false,
          l,
        )
      ) {
      | Standard(conditions) => Some(List.map(~f=print_exp, conditions))
      | Unknown(_) => None
      }
    | _ => Alcotest.fail("not an equation: " ++ print_exp(statement))
    }
  | _ => Alcotest.fail("no goal recorded")
  };
};

let term_size =
  "type Term = + Var(Int) + Lam(Term) + Ap(Term, Term) in "
  ++ "let size = fun e -> case e | Var(n) => 1 | Lam(b) => 1 + size(b) | Ap(e1, e2) => 1 + size(e1) + size(e2) end in";

/* A library or user function is judged by its code, at its argument's
 * type: structural recursion shows in the patterns it splits on. */
let code_tests = [
  judged(
    "length of a standard list",
    Some([]),
    "fun (xs : [Int]) -> length(xs)",
  ),
  judged(
    "length's result, as a standardness statement",
    Some([]),
    "forall (xs : [Int]) -> !!length(xs)",
  ),
  judged(
    "map of a standard function over a standard list",
    Some([]),
    "fun (xs : [Int]) -> fun (f : Int -> Int) -> map(xs, f)",
  ),
  judged(
    "map of a total lambda",
    Some([]),
    "fun (xs : [Int]) -> map(xs, fun (y : Int) -> y + 1)",
  ),
  judged(
    "map of a lambda that isn't standard",
    None,
    "fun (xs : [Int]) -> map(xs, fun (y : Int) -> 1 / y)",
  ),
  test_case("an unannotated recursive definition", `Quick, () =>
    check(
      option(list(string)),
      "len",
      Some([]),
      routine_in(
        "let len = fun xs -> case xs | [] => 0 | _ :: t => 1 + len(t) end in",
        "len([1, 2])",
      ),
    )
  ),
  test_case("a recursion that isn't structural", `Quick, () =>
    check(
      option(list(string)),
      "count",
      None,
      routine_in(
        "let count = fun n -> if n == 0 then 0 else count(n - 1) in",
        "count(3)",
      ),
    )
  ),
  test_case("a recursion on a number above a bound", `Quick, () =>
    check(
      option(list(string)),
      "count",
      Some([]),
      routine_in(
        "let count = fun n -> if n <= 0 then 0 else count(n - 1) in",
        "count(3)",
      ),
    )
  ),
  test_case("a recursion on a datatype's payloads", `Quick, () =>
    check(
      option(list(string)),
      "size",
      Some([]),
      routine_in(term_size, "(fun (x : Term) -> size(x))"),
    )
  ),
  test_case("the same recursion written with `fix`", `Quick, () =>
    check(
      option(list(string)),
      "size",
      Some([]),
      routine_in(
        "type Term = + Var(Int) + Lam(Term) + Ap(Term, Term) in "
        ++ "let size = fix f -> fun e -> case e | Var(n) => 1 | Lam(b) => 1 + f(b) | Ap(e1, e2) => 1 + f(e1) + f(e2) end in",
        "(fun (x : Term) -> size(x))",
      ),
    )
  ),
  test_case("a tuple parameter typed by the argument", `Quick, () =>
    check(
      option(list(string)),
      "foldl",
      Some([]),
      routine_in(
        "let foldl = fun (xs : [Bool], f, acc : Bool) -> case xs | [] => acc | x :: rest => foldl(rest, f, f(acc, x)) end in "
        ++ "let conj = fun (acc : Bool, x : Bool) -> acc && x in",
        "(fun (xs : [Bool]) -> foldl(xs, conj, true))",
      ),
    )
  ),
  test_case("a case that misses a constructor", `Quick, () =>
    check(
      option(list(string)),
      "size",
      None,
      routine_in(
        "type Term = + Var(Int) + Lam(Term) + Ap(Term, Term) in "
        ++ "let size = fun e -> case e | Var(n) => 1 | Lam(b) => 1 + size(b) end in",
        "(fun (x : Term) -> size(x))",
      ),
    )
  ),
];

/* A partial primitive is standard through its rule, which proofs cite
 * and the routine uses. */
let primitive_rules = [
  test_case("div_standard proves !!(a / b) from b != 0", `Quick, () =>
    check(
      bool,
      "proven",
      true,
      proven(
        "theorem t = forall a : Int -> forall b : Int -> b != 0 ==> !!(a / b) proof assume h = b != 0 => rewrite div_standard end; rewrite h end in t",
      ),
    )
  ),
  judged(
    "the routine leaves div_standard's condition",
    Some(["3 - 1 != 0"]),
    "(fun (y : Int) -> 10 / (y - 1))(3)",
  ),
  judged(
    "a division on an operand of unknown type",
    None,
    "fun x -> fun (y : Int) -> x / y",
  ),
  judged(
    "a conversion's condition",
    Some(["is_finite(1.5)"]),
    "int_of_float(1.5)",
  ),
];

/* The rule `standard : forall x -> !!x` closes a goal `!!e` where `e`'s
 * standardness is shown, and leaves it otherwise. */
let left_by_standard = (src: string): list(string) => {
  let (pm, proof) = run_named("t", src);
  ProofMap.obligations_of_proof(pm, proof)
  |> List.map(~f=(ob: Obligation.t) => print_exp(ob.goal));
};

let closing_goals = [
  test_case("standard closes a goal the routine shows", `Quick, () =>
    check(
      bool,
      "proven",
      true,
      proven(
        "theorem t = forall (x : Int) -> !!(x + 1) proof rewrite standard end in t",
      ),
    )
  ),
  test_case("and one about a quantified function", `Quick, () =>
    check(
      bool,
      "proven",
      true,
      proven(
        "theorem t = forall (f : Int -> Int) -> forall (x : Int) -> !!f(x) proof rewrite standard end in t",
      ),
    )
  ),
  test_case("standard leaves a domain condition", `Quick, () =>
    check(
      list(string),
      "y != 0",
      ["y != 0"],
      left_by_standard(
        "theorem t = forall (y : Int) -> !!(10 / y) proof rewrite standard end in t",
      ),
    )
  ),
  test_case("standard leaves what the routine can't show", `Quick, () =>
    check(
      list(string),
      "!!loop(0)",
      ["!! loop(0)"],
      left_by_standard(
        "let loop = fun (u : Int) -> loop(u) in theorem t = !!loop(0) proof rewrite standard end in t",
      ),
    )
  ),
];

let tests = (
  "Evaluator.Standardness",
  named_binder_facts
  @ routine_tests
  @ code_tests
  @ primitive_rules
  @ closing_goals,
);
