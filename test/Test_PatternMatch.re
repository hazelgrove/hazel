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
                    asc(
                      string("get_acne"),
                      Typ.unknown(Internal |> Prov.fresh),
                    ),
                  ),
                  Typ.unknown(Internal |> Prov.fresh),
                ),
                asc(
                  tup_label(
                    label("val"),
                    asc(bool(true), Typ.unknown(Internal |> Prov.fresh)),
                  ),
                  Typ.unknown(Internal |> Prov.fresh),
                ),
              ]),
              Typ.(
                prod([
                  tup_label(label("var"), unknown(SynSwitch |> Prov.fresh)),
                  tup_label(label("val"), unknown(SynSwitch |> Prov.fresh)),
                ])
              ),
            )
          );
        let matches: PatternMatch.match_result =
          PatternMatch.matches(pat, expression).matches;

        check(
          testable(
            PatternMatch.pp_match_result,
            Unboxing.equal_unboxed(
              VarBstMap.Ordered.equal_t_(TermBase.Exp.fast_equal),
            ),
          ),
          "Labeled Tuple with casts",
          Matches(
            Environment.of_list([
              ("b", Exp.(bool(true))),
              ("a", Exp.(string("get_acne"))),
            ]),
          ),
          matches,
        );
      },
    ),
  ],
);
