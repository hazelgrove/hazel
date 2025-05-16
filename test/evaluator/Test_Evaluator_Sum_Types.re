open Alcotest;
open Haz3lcore;
open Test_Evaluator_Prelude;
module PGrammar =
  Grammar.Factory({
    type t = list(Grammar.exp_t(unit));
    let default_value = (): list(Grammar.exp_t(unit)) => [];
  });

open IdTagged.FreshGrammar;
open Exp;

let skip_current_unboxing_error = (err: string, expression: string) =>
  test_case(err ++ " (Unboxing Error)", `Quick, () => {
    [@warning "-21"]
    {
      // Currently fails https://github.com/hazelgrove/hazel/issues/1588
      Alcotest.skip();
      let exp = parse_and_evaluate(expression);
      check(pass, err, exp, exp);
    }
  });

let tests = (
  "Evaluator Sum Types",
  [
    test_case("Casted constructor", `Quick, () => {
      evaluation_test(
        {|A :(+A +B +C)|},
        constructor(
          "A",
          Some(
            Some(
              Typ.(
                sum([
                  Variant("A", [], None),
                  Variant("B", [], None),
                  Variant("C", [], None),
                ])
              ),
            ),
          ),
        ),
        elaborate(parse_exp({|A :(+A +B +C)|})),
      )
    }),
    test_case(
      "Invalid constructor match",
      `Quick,
      () => {
        let invalid_constructor_match =
          elaborate(
            let_(Pat.(constructor("T", Some(None))), int(1), empty_hole()),
          );
        try(
          evaluation_test(
            "let T = 1 in ?",
            invalid_constructor_match,
            invalid_constructor_match,
          )
        ) {
        | Haz3lcore.EvaluatorError.Exception(_) as exn =>
          print_endline("Caught exception: " ++ Printexc.to_string(exn));
          Alcotest.fail(
            "Invalid constructor match should not throw an exception",
          );
        };
      },
    ),
    skip_current_unboxing_error(
      "InvalidBoxSumConstructor",
      "let B : (+B( )) = ? in ?",
    ),
    skip_current_unboxing_error(
      "InvalidBoxedListLit",
      "type g = + On in let [] = On in",
    ),
    skip_current_unboxing_error(
      "InvalidBoxedListCons",
      "let (_:: []) = type y = + B in B in ?",
    ),
    skip_current_unboxing_error(
      "InvalidBoxedBoolLit",
      "type y = + B(Float) in if B then false else A",
    ),
    skip_current_unboxing_error(
      "InvalidBoxedTuple",
      "let () = type x = + A in A in ?",
    ),
    skip_current_unboxing_error(
      "InvalidBoxedTypfun",
      "type y = + B in case true  | a => B end @<?> ",
    ),
    skip_current_unboxing_error(
      "InvalidBoxedSumConstructor",
      "type x = + A(Float) in let A = a in 0",
    ),
    skip_current_unboxing_error(
      "InvalidBoxedStringLit",
      {|type y = + A in ""++A|},
    ),
    skip_current_unboxing_error("InvalidBoxedIntLit", "type y = + A in -A"),
  ],
);
