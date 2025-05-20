open Alcotest;
open Haz3lcore;

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
            cast(
              tuple([
                cast(
                  tup_label(
                    label("var"),
                    cast(
                      string("get_acne"),
                      Typ.string(),
                      Typ.unknown(Internal),
                    ),
                  ),
                  Typ.(tup_label(unknown(Internal), unknown(Internal))),
                  Typ.unknown(Internal),
                ),
                cast(
                  tup_label(
                    label("val"),
                    cast(bool(true), Typ.bool(), Typ.unknown(Internal)),
                  ),
                  Typ.(tup_label(unknown(Internal), unknown(Internal))),
                  Typ.unknown(Internal),
                ),
              ]),
              Typ.(prod([unknown(Internal), unknown(Internal)])),
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
