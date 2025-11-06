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
            fn(Pat.var("x"), var("x")), // It seems as though the function naming happens during elaboration and not during parsing
            int(1),
          ),
          "let f = fun x -> x in 1",
        )
      ),
      test_case("Incomplete Function Definition", `Quick, () =>
        exp_check(
          let_(
            Pat.empty_hole(),
            fn(Pat.var("x"), empty_hole()),
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
      test_case("Singleton Labeled Tuple ascription in let", `Quick, () =>
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
      test_case("Unparenthesized labeled tuple element in list", `Quick, () => {
        exp_check(
          list_lit([tuple([tup_label(label("l"), int(32))]), int(1)]),
          {|[l=32, 1]|},
        )
      }),
      test_case("Malformed label in singleton tuple", `Quick, () =>
        exp_check(
          parens(tuple([tup_label(multi_hole([Exp(int(1))]), int(3))])),
          "(1=3)",
        )
      ),
      test_case("Quoted label in tuple", `Quick, () =>
        exp_check(tuple([tup_label(label("a"), int(3))]), "(`a`=3)")
      ),
      test_case("Quoted label in projection", `Quick, () =>
        exp_check(dot(empty_hole(), label("a")), "?.`a`")
      ),
      test_case(
        "Quoted label with non-alpha characters",
        `Quick,
        () => {
          exp_check(dot(empty_hole(), label("a-b_c")), "?.`a-b_c`");
          exp_check(
            tuple([
              tup_label(label(" "), int(1)),
              tup_label(label(""), int(2)),
            ]),
            "(` `=1, ``=2)",
          );
          exp_check(
            tuple([tup_label(label("multi word label"), int(1))]),
            "(`multi word label`=1)",
          );
        },
      ),
      test_case(
        "Tuple extension and function application precedence", `Quick, () =>
        exp_check(
          tuple_extension(
            tuple([]),
            ap(Forward, var("from_lvs"), list_lit([])),
          ),
          {|() ... from_lvs([])|},
        )
      ),
      test_case("Quoted label in pattern", `Quick, () =>
        exp_check(
          fn(
            Pat.(tuple([tup_label(label("a"), empty_hole())])),
            empty_hole(),
          ),
          {|fun (`a`=?) -> ?|},
        )
      ),
      test_case("Quoted label in type", `Quick, () =>
        exp_check(
          ty_alias(
            TPat.var("t"),
            Typ.(prod([tup_label(label("a"), Typ.int())])),
            empty_hole(),
          ),
          {|type t = (`a`=Int) in ?|},
        )
      ),
      test_case("Dot projection with Quoted label", `Quick, () =>
        exp_check(dot(empty_hole(), label("a")), "? . `a`")
      ),
      test_case("Scientific notation floating point", `Quick, () =>
        exp_check(float(1.2e30), "1.2e30")
      ),
      test_case("group_by_label flips variable to label sort", `Quick, () => {
        exp_check(
          ap(
            Forward,
            var("group_by_label"),
            tuple([label("l"), list_lit([])]),
          ),
          {|group_by_label(`l`,  [])|},
        )
      }),
      test_case("Livelit name parsing", `Quick, () =>
        exp_check(livelit_name("slider"), "^slider")
      ),
      test_case("Livelit ap parsing", `Quick, () =>
        exp_check(
          ap(Forward, livelit_name("slider"), int(50)),
          "^slider(50)",
        )
      ),
      test_case("Product extension in ascription", `Quick, () =>
        exp_check(
          asc(
            empty_hole(),
            Typ.(
              prod_extension(
                prod([tup_label(label("a"), int()), string()]),
                prod([
                  tup_label(label("b"), int()),
                  tup_label(label("c"), float()),
                ]),
              )
            ),
          ),
          "? : (a=Int, String) ... (b=Int, c=Float)",
        )
      ),
      test_case("Singleton unlabeled tuple", `Quick, () =>
        exp_check(
          tuple([tup_label(explicit_non_label(), int(1))]),
          "(_ = 1)",
        )
      ),
      test_case("Multiple unlabeled tuple entries", `Quick, () =>
        exp_check(
          tuple([
            tup_label(explicit_non_label(), int(1)),
            tup_label(explicit_non_label(), int(2)),
            tup_label(explicit_non_label(), int(3)),
          ]),
          "(_ = 1, _ = 2, _ = 3)",
        )
      ),
      test_case("Explicit unlabeled in type", `Quick, () =>
        exp_check(
          asc(
            empty_hole(),
            Typ.(prod([tup_label(explicit_non_label(), int())])),
          ),
          "? : (_=Int)",
        )
      ),
      test_case("Explicit unlabeled in pattern", `Quick, () =>
        exp_check(
          let_(
            Pat.(tuple([tup_label(explicit_non_label(), var("x"))])),
            tuple([tup_label(explicit_non_label(), int(1))]),
            var("x"),
          ),
          {|let (_=x) = (_=1) in x|},
        )
      ),
    ],
  );
