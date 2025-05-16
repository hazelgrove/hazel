open Alcotest;
open Haz3lcore;
open Test_Evaluator_Prelude;
open IdTagged.FreshGrammar;
open Exp;

let tests = (
  "Evaluator.Case",
  [
    test_case("Case expression with constructors of different type", `Quick, () => {
      evaluation_test(
        "mismatched types",
        match(
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
          [
            (
              Pat.(
                asc(
                  constructor(
                    "A",
                    Some(Some(Typ.(sum([Variant("A", [], None)])))),
                  ),
                  Typ.(sum([Variant("A", [], None)])),
                )
              ),
              bool(true),
            ),
            (Pat.(wild()), bool(false)),
          ],
        ),
        elaborate(
          parse_exp(
            {|let match  = fun x->
          case x
            | (A :(+A)) => true
            | _ => false
          end in match(A :(+A +B +C))|},
          ),
        ),
      )
    }),
    test_case("Case expression with constructors with payloads", `Quick, () => {
      parse_and_evaluate_test(
        "1",
        {|type T = +A(String) in case A("yo")
          | A(n) => 1
        end|},
      )
    }),
    test_case("Case expression with constructors with payloads", `Quick, () => {
      parse_and_evaluate_test(
        "1",
        {|
type Exp =
+ Var(String) in
let go: Exp -> Int =
  fun e ->
    case e
      | Var(n) => 1
 end in

go(Var("yo"))|},
      )
    }),
    test_case(
      "Case expression with constructors with payloads and recursive types",
      `Quick,
      () => {
      parse_and_evaluate_test(
        "1",
        {|
type Exp =
+ Var(String) +B(Exp) in
let go: Exp -> Int =
  fun e ->
    case e
      | Var(n) => 1
 end in

go(Var("yo"))|},
      )
    }),
    test_case("Inconsistent pattern ascription in case expression", `Quick, () =>
      parse_and_evaluate_test(
        {|1 : String|},
        {|case 1
          | (x : String) =>x
        end
        |},
      )
    ),
    test_case("Inconsistent pattern match", `Quick, () =>
      parse_and_evaluate_test(
        {|case 1
          | ("hello") => false
          | (x : String) => true
        end|},
        {|case 1
          | ("hello") => false
          | (x : String) => true
        end|},
      )
    ),
    test_case("Unevaluated if closure", `Quick, () =>
      evaluation_test(
        "let x = 5 in if ? then x else x",
        if_(empty_hole(), int(5), int(5)),
        let_(
          Pat.(var("x")),
          int(5),
          if_(empty_hole(), var("x"), var("x")),
        ),
      )
    ),
    test_case("Indet case passes casts through", `Quick, () => {
      parse_and_evaluate_test(
        {|(case ?
    | 1 => true : String
    | _ => false : String
    end)|},
        {|(case ?
    | 1 => true
    | _ => false
    end) : String|},
      )
    }),
  ],
);
