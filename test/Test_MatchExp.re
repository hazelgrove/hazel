open Alcotest;

let parse_exp = (s: string) => {
  switch (Haz3lcore.Parser.to_term(s)) {
  | Some(e) => e
  | None => Alcotest.fail("Failed to parse expression: " ++ s)
  };
};

let match_check = (~alphas=[], ~ctx_in=[], exp_r, exp, expected, ()) => {
  let exp_r' = parse_exp(exp_r);
  let exp' = parse_exp(exp);
  check(
    testable(
      Fmt.using(
        fun
        | None => "None"
        | Some(x) => Language.MatchExp.show_match_ctx(x),
        Fmt.string,
      ),
      Option.equal(
        List.for_all2(((str1, opt1), (str2, opt2)) =>
          str1 == str2 && Option.equal(Language.Exp.fast_equal, opt1, opt2)
        ),
      ),
    ),
    exp_r ++ " against " ++ exp,
    expected,
    Language.MatchExp.match_exp(
      ~alphas,
      ~exp_env=Language.ClosureEnvironment.empty,
      ~exp_r_ctx=ctx_in,
      exp_r',
      exp',
    ),
  );
};

open Language.IdTagged.FreshGrammar;
open Exp;

let tests = [
  (
    "MatchExp",
    [
      test_case(
        "Match a variable",
        `Quick,
        match_check(
          ~ctx_in=[("x", None)],
          "x",
          "y",
          Some([("x", Some(var("y")))]),
        ),
      ),
      test_case(
        "Doesn't resolve if different",
        `Quick,
        match_check(~ctx_in=[("x", None)], "(x, x)", "(y, z)", None),
      ),
      test_case(
        "Does resolve if same",
        `Quick,
        match_check(
          ~ctx_in=[("x", None)],
          "(x, x)",
          "(y, y)",
          Some([("x", Some(var("y")))]),
        ),
      ),
      test_case(
        "alpha equivalence",
        `Quick,
        match_check(
          ~ctx_in=[("x", None)],
          "fun u -> (x, u)",
          "fun v -> (y, v)",
          Some([("x", Some(var("y")))]),
        ),
      ),
      test_case(
        "Let alpha equivalence",
        `Quick,
        match_check(
          ~ctx_in=[("x", None)],
          "let u = 1 in x",
          "let v = 1 in 5",
          Some([("x", Some(int(5)))]),
        ),
      ),
      test_case(
        "Shadowing",
        `Quick,
        match_check(
          ~ctx_in=[("x", None)],
          "let x = 1 in x",
          "let x = 1 in x",
          Some([("x", None)]),
        ),
      ),
      test_case(
        "Shadow alpha equivalence",
        `Quick,
        match_check(
          ~ctx_in=[("x", None)],
          "let x = 1 in x",
          "let y = 1 in y",
          Some([("x", None)]),
        ),
      ),
      test_case(
        "deep alpha equivalence",
        `Quick,
        match_check(
          ~ctx_in=[("x", None)],
          "fun (x,y,(z,w)) -> (x,y,z,w)",
          "fun (a,b,(c,d)) -> (a,b,c,d)",
          Some([("x", None)]),
        ),
      ),
      test_case(
        "ignores casts",
        `Quick,
        match_check("x", "x: Int", Some([])),
      ),
      test_case(
        "Nested function alpha equivalence",
        `Quick,
        match_check(
          ~ctx_in=[("x", None)],
          "fun u -> fun v -> (x, u, v)",
          "fun a -> fun b -> (y, a, b)",
          Some([("x", Some(var("y")))]),
        ),
      ),
      test_case(
        "FixF alpha equivalence",
        `Quick,
        match_check(
          ~ctx_in=[("x", None)],
          "fix f -> fun n -> x",
          "fix g -> fun m -> y",
          Some([("x", Some(var("y")))]),
        ),
      ),
      test_case(
        "Match expression alpha equivalence",
        `Quick,
        match_check(
          ~ctx_in=[("x", None)],
          "case z | u => (x, u) end",
          "case z | v => (y, v) end",
          Some([("x", Some(var("y")))]),
        ),
      ),
    ],
  ),
];
