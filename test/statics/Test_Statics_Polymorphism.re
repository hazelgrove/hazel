open Alcotest;
open Test_Statics_Prelude;
open FTemp;
open Typ;

let tests = [
  test_case("Example error annotations", `Quick, () => {
    annotated_tree_test(
      "Inconsistent expectation on plus",
      FIError.Exp.(
        bin_op(
          Int(Plus),
          int(1),
          string(
            ~ann=
              Some(
                FTemp.Typ.(
                  Exp(
                    Common(
                      Inconsistent(
                        Expectation({
                          ana: int(),
                          syn: string(),
                        }),
                      ),
                    ),
                  )
                ),
              ),
            "hello",
          ),
        )
      ),
    )
  }),
  fully_consistent_typecheck(
    "Forall alpha equivalent in ascription",
    {|let x : poly a -> a = in (x : poly b -> b)|},
    FTemp.Typ.(Some(poly(TPat.var("b"), var("b")))),
  ),
  fully_consistent_typecheck(
    "Forall alpha equivalent in let",
    {|let x : poly a -> a = in let y : poly b -> b = x in 1|},
    Some(int()),
  ),
  inconsistent_typecheck(
    "Polymorphic Equality type inconsistency 1",
    {| 1 == 1. |} |> parse_exp,
  ),
  inconsistent_typecheck(
    "Polymorphic Equality type inconsistency 2",
    {| "" == false |} |> parse_exp,
  ),
  inconsistent_typecheck(
    "Polymorphic Equality type inconsistency 3",
    {| ("1", false) == (1, true) |} |> parse_exp,
  ),
  inconsistent_typecheck(
    "Polymorphic Equality type inconsistency 4",
    {| [1.] == [1] |} |> parse_exp,
  ),
  inconsistent_typecheck(
    "Polymorphic Equality type inconsistency 5",
    {| [1.,   ] == [ , 1] |} |> parse_exp,
  ),
  inconsistent_typecheck(
    "Polymorphic Equality type inconsistency 6",
    {| type T1 = +B+C in type T = A+B in A == C |} |> parse_exp,
  ),
  inconsistent_typecheck(
    "Polymorphic Equality type contains arrow 1",
    {| string_compare == string_compare |} |> parse_exp,
  ),
  inconsistent_typecheck(
    "Polymorphic Equality type contains arrow 2",
    {| let f = fun x -> x in f == f |} |> parse_exp,
  ),
  inconsistent_typecheck(
    "Polymorphic Equality type contains arrow 3",
    {| let f = fun x -> x in let a = (1, [f]) in a == a |} |> parse_exp,
  ),
  inconsistent_typecheck(
    "Polymorphic Equality type contains arrow 4",
    {| let a : +A+B(Int->Int) = in a == a |} |> parse_exp,
  ),
  inconsistent_typecheck(
    "Polymorphic Equality type contains arrow 5",
    {| let a : rec R -> A(Int->Int) + R = in a == a |} |> parse_exp,
  ),
  inconsistent_typecheck(
    "Polymorphic Equality type contains arrow 6",
    {| let a = typfun A -> fun x : A -> x in a == a |} |> parse_exp,
  ),
  inconsistent_typecheck(
    "Polymorphic Equality type contains arrow 7",
    {| type T2 = +A(Int->Int)+B in B == B |} |> parse_exp,
  ),
];
