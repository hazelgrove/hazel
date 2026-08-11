open Alcotest;
open Language;

let exp = testable(Fmt.using(Exp.show, Fmt.string), Equality.semantic.exp);

open IdTagged.FreshGrammar;

let tests = (
  "Substitution",
  [
    // Basic variable substitution
    test_case(
      "substitute variable with integer",
      `Quick,
      () => {
        let env = Environment.of_list([("x", Exp.int(42))]);
        let expr = Exp.tuple([Exp.var("x"), Exp.var("x")]);
        let result = Substitution.in_exp(env, expr);
        check(
          exp,
          "x -> 42",
          Exp.tuple([Exp.int(42), Exp.int(42)]),
          result,
        );
      },
    ),
    // Capture avoidance
    test_case(
      "capture avoidance",
      `Quick,
      () => {
        let env =
          Environment.of_list([("x", Exp.var("x")), ("y", Exp.var("x"))]);
        let expr = Exp.fn(Pat.var("x"), Exp.var("y"), None, None);
        let result = Substitution.in_exp(env, expr);
        let expected = Exp.fn(Pat.var("x'"), Exp.var("x"), None, None);
        check(exp, "x -> x in fn x. x", expected, result);
      },
    ),
    // Shadowing
    test_case(
      "shadowing",
      `Quick,
      () => {
        let env =
          Environment.of_list([("x", Exp.int(1)), ("y", Exp.int(2))]);
        let expr =
          Exp.let_(
            Pat.var("x"),
            Exp.int(3),
            Exp.tuple([Exp.var("x"), Exp.var("y")]),
          );
        let result = Substitution.in_exp(env, expr);
        let expected =
          Exp.let_(
            Pat.var("x"),
            Exp.int(3),
            Exp.tuple([Exp.var("x"), Exp.int(2)]),
          );
        check(exp, "x -> 1, y -> 2 in let x = 3 in (x, y)", expected, result);
      },
    ),
    /* Freshening a shadowing binder must not rewrite free refs on the RHS. */
    test_case(
      "shadowing let RHS keeps outer var",
      `Quick,
      () => {
        let env = Environment.of_list([("y", Exp.var("y"))]);
        let expr = Exp.let_(Pat.var("y"), Exp.var("y"), Exp.var("y"));
        let result = Substitution.in_exp(env, expr);
        let expected =
          Exp.let_(Pat.var("y'"), Exp.var("y"), Exp.var("y'"));
        check(exp, "let y = y in y  =>  let y' = y in y'", expected, result);
      },
    ),
    /* A `Sig` type carries patterns, so substituting through a function's
       annotation must reach under it. Statics desugars `Sig` away, so build
       it directly. */
    test_case(
      "substitute through a Sig-annotated function",
      `Quick,
      () => {
        let sig_ty = Language.Typ.fresh(Sig([Sig.sig_let(Pat.var("y"))]));
        let env = Environment.of_list([("x", Exp.int(1))]);
        let expr = Exp.fn(Pat.var("p"), Exp.var("x"), Some(sig_ty), None);
        let result = Substitution.in_exp(env, expr);
        let expected =
          Exp.fn(Pat.var("p"), Exp.int(1), Some(sig_ty), None);
        check(
          exp,
          "substitution reaches under a Sig annotation",
          expected,
          result,
        );
      },
    ),
    // Fixpoints
    test_case(
      "substitute in fixpoint",
      `Quick,
      () => {
        let env =
          Environment.of_list([("f", Exp.var("g")), ("g", Exp.var("h"))]);
        let expr = Exp.fix_f(Pat.var("f"), Exp.var("f"), None);
        let result = Substitution.in_exp(env, expr);
        let expected = Exp.fix_f(Pat.var("f"), Exp.var("f"), None);
        check(exp, "f -> g, g -> h in fix f. f", expected, result);
      },
    ),
  ],
);
