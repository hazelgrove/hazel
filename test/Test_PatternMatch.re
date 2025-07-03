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
          PatternMatch.matches(pat, expression).matches;
        print_endline([%derive.show: PatternMatch.match_result](matches));

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
