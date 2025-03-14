open Haz3lcore;
open Alcotest;
open Introduce;

open IdTagged.FreshGrammar;

let exp = testable(Fmt.using(DHExp.show, Fmt.string), DHExp.fast_equal);

let tests = [
  (
    "Introduce.introduce_expression",
    [
      test_case("Arrow type", `Quick, () => {
        check(
          option(exp),
          "Function",
          Some(Exp.(fn(Pat.empty_hole(), empty_hole(), None, None))),
          introduce_expression(Typ.(arrow(int(), int()))),
        )
      }),
      test_case(
        "Product types",
        `Quick,
        () => {
          check(
            option(exp),
            "Cardinality 0",
            Some(Exp.(tuple([]))),
            introduce_expression(Typ.(prod([]))),
          );
          check(
            option(exp),
            "Cardinality 2",
            Some(Exp.(tuple([empty_hole(), empty_hole()]))),
            introduce_expression(Typ.(prod([int(), int()]))),
          );
          check(
            option(exp),
            "Cardinality 3",
            Some(Exp.(tuple([empty_hole(), empty_hole(), empty_hole()]))),
            introduce_expression(Typ.(prod([int(), int(), int()]))),
          );
          check(
            option(exp),
            "Cardinality 4",
            Some(
              Exp.(
                tuple([
                  empty_hole(),
                  empty_hole(),
                  empty_hole(),
                  empty_hole(),
                ])
              ),
            ),
            introduce_expression(Typ.(prod([int(), int(), int(), int()]))),
          );
          check(
            option(exp),
            "Cardinality 5",
            Some(
              Exp.(
                tuple([
                  empty_hole(),
                  empty_hole(),
                  empty_hole(),
                  empty_hole(),
                  empty_hole(),
                ])
              ),
            ),
            introduce_expression(
              Typ.(prod([int(), int(), int(), int(), int()])),
            ),
          );
        },
      ),
    ],
  ),
];
