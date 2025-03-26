open Alcotest;
open Haz3lcore;

let tests = (
  "PatternMatch",
  IdTagged.FreshGrammar.[
    test_case(
      "Labeled Tuple with casts",
      `Quick,
      () => {
        let foo: PatternMatch.matches_and_closures =
          PatternMatch.matches(
            Pat.(
              tuple([
                tup_label(label("var"), var("a")),
                tup_label(label("val"), var("b")),
              ])
            ),
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
            ),
          );

        ();
      },
    ),
  ],
);
