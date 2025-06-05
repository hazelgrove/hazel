open Test_Statics_Prelude;
open Alcotest;
open FTemp;
open Typ;

let tests = [
  fully_consistent_typecheck(
    "Tuple extension",
    {|(a=0, 1, b=2) ... (a=1, 3, c=4)|},
    Some(
      prod([
        tup_label(label("a"), int()),
        int(),
        tup_label(label("b"), int()),
        int(),
        tup_label(label("c"), int()),
      ]),
    ),
  ),
  fully_consistent_typecheck(
    "Tuple extension with type alias",
    {|type Person = (name=String, age=Int) in
      type Date = (year=Int, month=Int, day=Int) in

      let p : Person = in
      let d : Date = in
      p ... d|},
    Some(
      prod([
        tup_label(label("name"), string()),
        tup_label(label("age"), int()),
        tup_label(label("year"), int()),
        tup_label(label("month"), int()),
        tup_label(label("day"), int()),
      ]),
    ),
  ),
  test_case("Tuple extension with non-tuple args", `Quick, () =>
    annotated_tree_test(
      "1 ... 2",
      FIError.(
        Exp.(
          tuple_extension(
            ~ann=Some(Exp(Common(NoType(TupleExtensionRequiresTuples)))),
            int(1),
            int(2),
          )
        )
      ),
    )
  ),
];
