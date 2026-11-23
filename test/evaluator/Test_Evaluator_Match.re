open Alcotest;
open Language;
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
                    Variant("A", ConstructorMap.empty_variant_ann, None),
                    Variant("B", ConstructorMap.empty_variant_ann, None),
                    Variant("C", ConstructorMap.empty_variant_ann, None),
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
                    Some(
                      Some(
                        Typ.(
                          sum([
                            Variant(
                              "A",
                              ConstructorMap.empty_variant_ann,
                              None,
                            ),
                          ])
                        ),
                      ),
                    ),
                  ),
                  Typ.(
                    sum([
                      Variant("A", ConstructorMap.empty_variant_ann, None),
                    ])
                  ),
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
    test_case("Ascriptions collapse lazily through if", `Quick, () =>
      parse_and_evaluate_test(
        {|if ? then ? : String else ? : String|},
        {|(if ? then ? else ?) : String : String|},
      )
    ),
    /* A stuck result passed back through a recursive function's casts
       carries one cast, not one per call (#1874). */
    test_case(
      "A hole returned through recursion keeps one ascription",
      `Quick,
      () => {
        parse_and_evaluate_test(
          {|? : Int|},
          {|let f : Int -> Int = fun n -> if n == 0 then ? else f(n - 1) in f(5)|},
        );
        parse_and_evaluate_test(
          {|? : String|},
          {|let f : Int -> String = fun n -> if n == 0 then ? else f(n - 1) in f(5)|},
        );
        parse_and_evaluate_test(
          {|? : Int|},
          {|let f : ? -> ? = fun n -> if n == 0 then ? else (f(n - 1) : Int) in f(5)|},
        );
      },
    ),
    test_case(
      "Stuck results built up through recursion keep one ascription per hole",
      `Quick,
      () => {
        parse_and_evaluate_test(
          {|1 + (1 + (1 + (1 + ?)))|},
          {|let f : Int -> Int = fun n -> if n == 0 then ? else 1 + f(n - 1) in f(4)|},
        );
        parse_and_evaluate_test(
          {|1 + ((? : Int) + 3)|},
          {|let sum : [?] -> ? = fun xs ->
              case xs
              | [] => 0
              | x :: xs => x + sum(xs)
              end
            in sum([1, ?, 3])|},
        );
        parse_and_evaluate_test(
          {|1 + ((? : Int) + (3 + ((? : Int) + 5)))|},
          {|let sum : [Int] -> Int = fun xs ->
              case xs
              | [] => 0
              | x :: xs => x + sum(xs)
              end
            in sum([1, ?, 3, ?, 5])|},
        );
      },
    ),
    test_case("Indet case passes ascriptions through", `Quick, () => {
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
    test_case("Indet case of different arity tuples", `Quick, () => {
      parse_and_evaluate_test(
        {|(case ()
          | (1, 2) => true
          | _ => false
          end)|},
        {|(case ()
          | (1, 2) => true
          | _ => false
          end)|},
      )
    }),
    test_case("Preservation of type with unknown in if expression", `Quick, () => {
      full_preservation_test(parse_exp({|if false then 1 else ("" : ?)|}))
    }),
    test_case(
      "Preservation of type with unknown in case expression", `Quick, () => {
      full_preservation_test(
        parse_exp(
          {|case 2
      | 1 => true
      | _ => "hello" : ?
      |},
        ),
      )
    }),
  ],
);
