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
  fully_consistent_typecheck(
    "Melt operation with elements of the same type",
    "melt((a=1, b=2, c=3, d=4))",
    Some(
      list(
        prod([
          tup_label(label("label"), string()),
          tup_label(label("value"), int()),
        ]),
      ),
    ),
  ),
  fully_consistent_typecheck(
    "Melt operation with type alias and autolabels",
    {|type Entry =(name=String, age=Int, quiz1=Int, quiz2=Int, midterm=Int, quiz3=Int, quiz4=Int, final=Int) in
      melt(("bob",   12, 8, 9, 77, 7, 9, 87) : Entry)|},
    Some(
      list(
        prod([
          tup_label(label("label"), string()),
          tup_label(label("value"), unknown(Internal)),
        ]),
      ),
    ),
  ),
  test_case("Melt operation with missing labels", `Quick, () =>
    annotated_tree_test(
      "melt(1,2)",
      FIError.Exp.(
        ap(
          ~ann=
            Some(
              Exp(
                LabelsRequired(
                  Typ.(
                    list(
                      prod([
                        tup_label(label("label"), string()),
                        tup_label(label("value"), unknown(Internal)),
                      ]),
                    )
                  ),
                ),
              ),
            ),
          Forward,
          var("melt"),
          tuple([int(1), int(2)]),
        )
      ),
    )
  ),
  test_case("Melt operation applied to non-tuple", `Quick, () =>
    annotated_tree_test(
      "melt(1)",
      FIError.Exp.(
        ap(
          ~ann=
            Some(
              Exp(
                LabelsRequired(
                  Typ.(
                    list(
                      prod([
                        tup_label(label("label"), string()),
                        tup_label(label("value"), unknown(Internal)),
                      ]),
                    )
                  ),
                ),
              ),
            ),
          Forward,
          var("melt"),
          int(1),
        )
      ),
    )
  ),
  test_case("Melt operation applied to value with unknown type", `Quick, () =>
    annotated_tree_test(
      "melt(?)",
      FIError.Exp.(ap(Forward, var("melt"), empty_hole())),
    )
  ),
];
