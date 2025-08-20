/**
 * This file contains tests to validate the `MakeTerm` module's ability to convert
 * zippers into expressions.
 */
open Alcotest;
module Fresh = Language.IdTagged.FreshGrammar;
let exp_typ =
  testable(
    Fmt.using(Language.Exp.show, Fmt.string),
    Language.Exp.fast_equal,
  );

let parse_exp = (s: string) => {
  switch (Haz3lcore.Parser.to_term(s)) {
  | Some(e) => e
  | None => Alcotest.fail("Failed to parse expression: " ++ s)
  };
};
let exp_check = (expected, actual) =>
  check(exp_typ, actual, expected, parse_exp(actual));

let tests =
  Fresh.(
    "MakeTerm",
    Exp.[
      test_case("Integer Literal", `Quick, () => exp_check(int(0), "0")),
      test_case("Float literal", `Quick, () =>
        exp_check(float(2.000000), "2.000000")
      ),
      test_case("Empty Hole", `Quick, () => exp_check(empty_hole(), "?")),
      test_case("Free Variable", `Quick, () => exp_check(var("x"), "x")),
      test_case("Parenthesized Expression", `Quick, () =>
        exp_check(parens(int(0)), "(0)")
      ),
      test_case("Floating operation", `Quick, () =>
        exp_check(bin_op(Float(Plus), float(1.0), float(2.0)), "1. +. 2.")
      ),
      test_case("Let Expression", `Quick, () =>
        exp_check(let_(Pat.var("x"), int(1), var("x")), "let x = 1 in x")
      ),
      test_case("Function Application", `Quick, () =>
        exp_check(ap(Forward, var("f"), var("x")), "f(x)")
      ),
      test_case("Named Function Definition", `Quick, () =>
        exp_check(
          let_(
            Pat.var("f"),
            fn(Pat.var("x"), var("x"), None, None), // It seems as though the function naming happens during elaboration and not during parsing
            int(1),
          ),
          "let f = fun x -> x in 1",
        )
      ),
      test_case("Incomplete Function Definition", `Quick, () =>
        exp_check(
          let_(
            Pat.empty_hole(),
            fn(Pat.var("x"), empty_hole(), None, None),
            empty_hole(),
          ),
          "let    = fun x ->   in  ",
        )
      ),
      test_case("Constructor", `Quick, () =>
        exp_check(constructor("A", None), "A")
      ),
      test_case("Type Alias", `Quick, () =>
        exp_check(
          ty_alias(TPat.var("x"), Typ.int(), int(1)),
          "type x = Int in 1",
        )
      ),
      test_case("Singleton Labled Tuple ascription in let", `Quick, () =>
        exp_check(
          let_(
            Pat.asc(
              Pat.var("x"),
              Typ.(parens(prod([tup_label(label("l"), string())]))),
            ),
            parens(string("a")),
            var("x"),
          ),
          "let x : (l=String) = (\"a\") in x",
        )
      ),
      test_case("Assigning labeled tuple to variable", `Quick, () =>
        exp_check(
          let_(
            Pat.var("x"),
            parens(tuple([tup_label(label("l"), int(32))])),
            let_(
              Pat.(
                asc(
                  var("y"),
                  Typ.(parens(prod([tup_label(label("l"), int())]))),
                )
              ),
              var("x"),
              var("y"),
            ),
          ),
          "let x = (l=32) in
             let y : (l=Int) = x in y",
        )
      ),
      test_case("Multiple labels tuple", `Quick, () =>
        exp_check(
          parens(
            tuple([
              tup_label(label("l"), int(32)),
              tup_label(label("l2"), string("")),
            ]),
          ),
          {|(l=32, l2="")|},
        )
      ),
      test_case("Multiple labels in let tuple", `Quick, () =>
        exp_check(
          let_(
            Pat.(
              asc(
                var("x"),
                Typ.(
                  parens(
                    prod([
                      tup_label(label("l"), int()),
                      tup_label(label("l2"), string()),
                    ]),
                  )
                ),
              )
            ),
            parens(
              tuple([
                tup_label(label("l"), int(32)),
                tup_label(label("l2"), string("")),
              ]),
            ),
            var("x"),
          ),
          {|let x : (l=Int, l2=String) = (l=32, l2="") in x|},
        )
      ),
      test_case("Malformed label in singleton tuple", `Quick, () =>
        exp_check(
          parens(tuple([tup_label(multi_hole([Exp(int(1))]), int(3))])),
          "(1=3)",
        )
      ),
      test_case("Scientific notation floating point", `Quick, () =>
        exp_check(float(1.2e30), "1.2e30")
      ),
      test_case("Livelit name parsing", `Quick, () =>
        exp_check(livelit_name("slider"), "^slider")
      ),
      test_case("Livelit ap parsing", `Quick, () =>
        exp_check(
          ap(Forward, livelit_name("slider"), int(50)),
          "^slider(50)",
        )
      ),
    ],
  );
