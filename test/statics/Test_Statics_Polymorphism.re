open Alcotest;
open Test_Statics_Prelude;
open FTemp;
open Typ;

let tests = (
  "Statics.Polymorphism",
  [
    test_case("Example error annotations", `Quick, () => {
      annotated_tree_test(
        "Inconsistent expectation on plus",
        Typ.int(),
        FIError.Exp.(
          bin_op(
            Int(Plus),
            int(1),
            string(
              ~ann=
                Some(
                  Marks([
                    ExpectationMismatch({
                      ana: FTemp.Typ.int(),
                      syn: FTemp.Typ.string(),
                    }),
                  ]),
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
    /* Test_Evaluator_TypAp pins the result this type has to agree with. */
    fully_consistent_typecheck(
      "typfun binder shadows a type alias, so the body's `y` is not Int (#1717)",
      {|type y = Int in typfun y -> (? : y)|},
      FTemp.Typ.(Some(poly(TPat.var("y"), var("y")))),
    ),
    test_case(
      "A type application keeps an alias argument unexpanded",
      `Quick,
      () => {
        let source = {|type Color = + Red + Green in
let id = typfun A -> fun (x : A) -> x in
let y : Int = id@<Color> in y|};
        check(
          Alcotest.list(testable_typ),
          source,
          [arrow(var("Color"), var("Color"))],
          statics(parse_exp(source))
          |> errors
          |> List.concat_map(snd)
          |> List.filter_map(
               fun
               | Language.Mark.ExpectationMismatch({syn, _}) => Some(syn)
               | _ => None,
             ),
        );
      },
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
    /* The binder is bound at its own id, so a use of the variable finds it. */
    test_case(
      "A poly-bound type variable's use resolves to its binder",
      `Quick,
      () => {
        let map = statics(parse_exp({|let f : poly a -> a -> a = ? in f|}));
        let binders =
          Language.Id.Map.fold(
            (id, info, acc) =>
              switch (info) {
              | Language.Info.InfoTPat({user_term: {term: Var("a"), _}, _}) => [
                  id,
                  ...acc,
                ]
              | _ => acc
              },
            map,
            [],
          );
        let sites =
          Language.Id.Map.fold(
            (_, info, acc) =>
              switch (info) {
              | Language.Info.InfoTyp({user_term: {term: Var("a"), _}, _}) => [
                  Language.Info.get_binding_site(info),
                  ...acc,
                ]
              | _ => acc
              },
            map,
            [],
          );
        check(Alcotest.int, "two uses", 2, List.length(sites));
        check(
          Alcotest.bool,
          "each use finds the binder",
          true,
          List.for_all(
            fun
            | Some(id) =>
              List.exists(b => Language.Id.compare(id, b) == 0, binders)
            | None => false,
            sites,
          ),
        );
      },
    ),
  ],
);
