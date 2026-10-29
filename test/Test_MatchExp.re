open Alcotest;

let parse_exp = (s: string) => {
  switch (Haz3lcore.Parser.to_term(s, ~root=Exp)) {
  | Some(e) => e
  | None => Alcotest.fail("Failed to parse expression: " ++ s)
  };
};

let match_check =
    (
      ~info_map=Language.Statics.Map.empty,
      ~alphas=[],
      ~ctx_in=[],
      exp_r,
      exp,
      expected,
      (),
    ) => {
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
        List.for_all2(((str1, (typ1, opt1)), (str2, (typ2, opt2))) =>
          str1 == str2
          && Language.Typ.fast_equal(typ1, typ2)
          && Option.equal(Language.Exp.fast_equal, opt1, opt2)
        ),
      ),
    ),
    exp_r ++ " against " ++ exp,
    expected,
    Language.MatchExp.match_exp(
      ~info_map,
      ~alphas,
      ~exp_env=Language.Environment.empty,
      ~exp_r_ctx=ctx_in,
      exp_r',
      exp',
    ),
  );
};

open Language.IdTagged.FreshGrammar;
open Exp;

let hole_typ = Typ.unknown(Internal);

let tests = [
  (
    "MatchExp",
    [
      test_case(
        "Match a variable",
        `Quick,
        match_check(
          ~ctx_in=[("x", (hole_typ, None))],
          "x",
          "y",
          Some([("x", (hole_typ, Some(var("y"))))]),
        ),
      ),
      test_case(
        "Doesn't resolve if different",
        `Quick,
        match_check(
          ~ctx_in=[("x", (hole_typ, None))],
          "(x, x)",
          "(y, z)",
          None,
        ),
      ),
      test_case(
        "Does resolve if same",
        `Quick,
        match_check(
          ~ctx_in=[("x", (hole_typ, None))],
          "(x, x)",
          "(y, y)",
          Some([("x", (hole_typ, Some(var("y"))))]),
        ),
      ),
      test_case(
        "alpha equivalence",
        `Quick,
        match_check(
          ~ctx_in=[("x", (hole_typ, None))],
          "fun u -> (x, u)",
          "fun v -> (y, v)",
          Some([("x", (hole_typ, Some(var("y"))))]),
        ),
      ),
      test_case(
        "Let alpha equivalence",
        `Quick,
        match_check(
          ~ctx_in=[("x", (hole_typ, None))],
          "let u = 1 in x",
          "let v = 1 in 5",
          Some([("x", (hole_typ, Some(int(5))))]),
        ),
      ),
      test_case(
        "Shadowing",
        `Quick,
        match_check(
          ~ctx_in=[("x", (hole_typ, None))],
          "let x = 1 in x",
          "let x = 1 in x",
          Some([("x", (hole_typ, None))]),
        ),
      ),
      test_case(
        "Shadow alpha equivalence",
        `Quick,
        match_check(
          ~ctx_in=[("x", (hole_typ, None))],
          "let x = 1 in x",
          "let y = 1 in y",
          Some([("x", (hole_typ, None))]),
        ),
      ),
      test_case(
        "deep alpha equivalence",
        `Quick,
        match_check(
          ~ctx_in=[("x", (hole_typ, None))],
          "fun (x,y,(z,w)) -> (x,y,z,w)",
          "fun (a,b,(c,d)) -> (a,b,c,d)",
          Some([("x", (hole_typ, None))]),
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
          ~ctx_in=[("x", (hole_typ, None))],
          "fun u -> fun v -> (x, u, v)",
          "fun a -> fun b -> (y, a, b)",
          Some([("x", (hole_typ, Some(var("y"))))]),
        ),
      ),
      test_case(
        "FixF alpha equivalence",
        `Quick,
        match_check(
          ~ctx_in=[("x", (hole_typ, None))],
          "fix f -> fun n -> x",
          "fix g -> fun m -> y",
          Some([("x", (hole_typ, Some(var("y"))))]),
        ),
      ),
      test_case(
        "Match expression alpha equivalence",
        `Quick,
        match_check(
          ~ctx_in=[("x", (hole_typ, None))],
          "case z | u => (x, u) end",
          "case z | v => (y, v) end",
          Some([("x", (hole_typ, Some(var("y"))))]),
        ),
      ),
      test_case(
        "Match equality with shadowing 1",
        `Quick,
        match_check(
          ~ctx_in=[("x", (hole_typ, None))],
          "x == x",
          "[x] == [x]",
          Some([("x", (hole_typ, Some(list_lit([var("x")]))))]),
        ),
      ),
      test_case(
        "Match equality with shadowing 2",
        `Quick,
        match_check(
          ~ctx_in=[("x", (hole_typ, None))],
          "x == x",
          "x == x",
          Some([("x", (hole_typ, Some(var("x"))))]),
        ),
      ),
    ],
  ),
  (
    "ProofHacks",
    [
      test_case("Parenthesized forall body is an equality rule", `Quick, () =>
        List.iter(
          src =>
            check(
              Alcotest.bool,
              src,
              true,
              switch (Language.ProofRule.exp_to_rule(parse_exp(src))) {
              | {conclusion: Equality(_), _} => true
              | {conclusion: Other(_), _} => false
              },
            ),
          [
            "forall n -> n + 0 == n",
            "forall n -> (n + 0 == n)",
            "(forall n -> (n + 0 == n))",
            "^^fold(forall n -> ^^fold(n + 0 == n))",
          ],
        )
      ),
      test_case("exp_to_pat keeps every pattern-like form", `Quick, () =>
        List.iter(
          src => {
            /* The same text read as a pattern: the parameter of a lambda */
            let pat =
              switch (parse_exp("fun " ++ src ++ " -> ?").term) {
              | Fun(p, _, _, _) => p
              | _ => fail("expected a lambda for: " ++ src)
              };
            let converted = Language.ProofHacks.exp_to_pat(parse_exp(src));
            check(
              Alcotest.bool,
              src
              ++ " gives "
              ++ Language.Pat.show(converted)
              ++ " not "
              ++ Language.Pat.show(pat),
              true,
              Language.Pat.fast_equal(converted, pat),
            );
          },
          ["_", "(a=x, _)", "^^fold(x)", "Some((x, _))"],
        )
      ),
      test_case(
        "Folded sub-pattern yields one inductive hypothesis", `Quick, () =>
        List.iter(
          pat_src => {
            let src =
              "case (? : (rec t -> +Z+S(t))) | "
              ++ pat_src
              ++ " => 0 | _ => 1 end";
            let exp = parse_exp(src);
            let (m, _) =
              Language.Statics.mk(
                Language.CoreSettings.on,
                Language.Builtins.ctx_init(Some(Int)),
                exp,
              );
            let pat =
              switch (exp.term) {
              | Match(_, [(p, _), ..._]) => p
              | _ => fail("expected a case expression: " ++ src)
              };
            let t =
              switch (
                Language.Statics.Map.ty_of(Language.Pat.rep_id(pat), m)
              ) {
              | Some(t) => t
              | None => fail("no type for pattern: " ++ src)
              };
            check(
              Alcotest.int,
              pat_src,
              1,
              List.length(
                Language.ProofHacks.get_inductive_hypotheses(m, t, pat),
              ),
            );
          },
          ["S(n)", "S((n))", "S(^^fold(n))"],
        )
      ),
    ],
  ),
];
