/* Tests for the proof helpers in ProofRule and ProofHacks that look through
   Parens and Projector wrappers. */

open Alcotest;

let parse_exp = (s: string) => {
  switch (Haz3lcore.Parser.to_term(s, ~root=Exp)) {
  | Some(e) => e
  | None => Alcotest.fail("Failed to parse expression: " ++ s)
  };
};

let tests = (
  "ProofHacks",
  [
    test_case("Parenthesized forall body is an equality rule", `Quick, () =>
      List.iter(
        ~f=
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
        ~f=
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
    test_case("Folded sub-pattern yields one inductive hypothesis", `Quick, () =>
      List.iter(
        ~f=
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
);
