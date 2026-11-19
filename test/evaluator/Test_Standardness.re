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
      ~env=Environment.empty,
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

let tests = ("Evaluator.Standardness", named_binder_facts @ routine_tests);
