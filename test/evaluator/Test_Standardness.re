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
    "both branches count, whichever is taken",
    Some(["0 != 0"]),
    "if true then 1 else 1 / 0",
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
        "let count = fun n -> if n <= 0 then 0 else count(n - 1) in",
        "count(3)",
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

let tests = (
  "Evaluator.Standardness",
  named_binder_facts @ routine_tests @ code_tests @ primitive_rules,
);
