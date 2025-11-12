open Alcotest;
open Language;

let tests = (
  "PatternMatch",
  IdTagged.FreshGrammar.[
    test_case(
      "Labeled Tuple with casts",
      `Quick,
      () => {
        let pat =
          Pat.(
            tuple([
              tup_label(label("var"), var("a")),
              tup_label(label("val"), var("b")),
            ])
          );
        let expression =
          Exp.(
            asc(
              tuple([
                asc(
                  tup_label(
                    label("var"),
                    asc(string("get_acne"), Typ.unknown(Internal)),
                  ),
                  Typ.unknown(Internal),
                ),
                asc(
                  tup_label(
                    label("val"),
                    asc(bool(true), Typ.unknown(Internal)),
                  ),
                  Typ.unknown(Internal),
                ),
              ]),
              Typ.(
                prod([
                  tup_label(label("var"), unknown(SynSwitch)),
                  tup_label(label("val"), unknown(SynSwitch)),
                ])
              ),
            )
          );
        let matches: PatternMatch.match_result =
          PatternMatch.matches(~ty_env=Environment.empty, pat, expression).
            matches;

        let equal_match_result =
            (r1: PatternMatch.match_result, r2: PatternMatch.match_result)
            : bool =>
          switch (r1, r2) {
          | (DoesNotMatch, DoesNotMatch) => true
          | (IndetMatch, IndetMatch) => true
          | (Matches(env1), Matches(env2)) =>
            List.equal(
              Environment.equal_binding(Language.Exp.fast_equal),
              List.sort(compare, env1),
              List.sort(compare, env2),
            )
          | _ => false
          };

        check(
          testable(PatternMatch.pp_match_result, equal_match_result),
          "Labeled Tuple with casts",
          Matches([
            ("b", Exp.(bool(true))),
            ("a", Exp.(string("get_acne"))),
          ]),
          matches,
        );
      },
    ),
  ],
);
