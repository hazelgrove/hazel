open Test_Statics_Prelude;
open Alcotest;
open FTemp;
open Typ;
// TODO do versions with variables where appropriate and do reverse partial application
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
      unknown(Internal),
      FIError.(
        Exp.(
          tuple_extension(
            ~ann=Some(Exp(TupleExtensionRequiresTuples)),
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
      "melt(1, 2)",
      list(
        prod([
          tup_label(label("label"), string()),
          tup_label(label("value"), unknown(Internal)),
        ]),
      ),
      FIError.Exp.(
        ap(
          ~ann=
            Some(
              Exp(
                BuiltinError(
                  MeltMissingLabelsOnTuple(
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
      list(
        prod([
          tup_label(label("label"), string()),
          tup_label(label("value"), unknown(Internal)),
        ]),
      ),
      FIError.Exp.(
        ap(
          ~ann=
            Some(
              Exp(
                BuiltinError(
                  MeltMissingLabelsOnTuple(
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
      list(
        prod([
          tup_label(label("label"), string()),
          tup_label(label("value"), unknown(Internal)),
        ]),
      ),
      FIError.Exp.(ap(Forward, var("melt"), empty_hole())),
    )
  ),
  test_case("project_labels with appropriate labels", `Quick, () =>
    annotated_tree_test(
      {|project_labels((a=1, b=true, c=3), 'a', 'b')|},
      prod([int(), bool()]),
      FIError.Exp.(
        ap(
          Forward,
          var("project_labels"),
          tuple([
            tuple([
              tup_label(label("a"), int(1)),
              tup_label(label("b"), bool(true)),
              tup_label(label("c"), int(3)),
            ]),
            label("a"),
            label("b"),
          ]),
        )
      ),
    )
  ),
  test_case("project_labels with more appropriate labels", `Quick, () =>
    annotated_tree_test(
      {|project_labels((a=1,b=2,c=3), 'c', 'a', 'c')|},
      prod([int(), int(), int()]),
      FIError.Exp.(
        ap(
          Forward,
          var("project_labels"),
          tuple([
            tuple([
              tup_label(label("a"), int(1)),
              tup_label(label("b"), int(2)),
              tup_label(label("c"), int(3)),
            ]),
            label("c"),
            label("a"),
            label("c"),
          ]),
        )
      ),
    )
  ),
  test_case("project_labels with non-label", `Quick, () =>
    annotated_tree_test(
      {|project_labels((a=1, b=true, c=3), 'a', 3, 'c')|},
      prod([int(), unknown(Internal), int()]),
      FIError.Exp.(
        ap(
          Forward,
          var("project_labels"),
          tuple([
            tuple([
              tup_label(label("a"), int(1)),
              tup_label(label("b"), bool(true)),
              tup_label(label("c"), int(3)),
            ]),
            label("a"),
            int(
              ~ann=Some(Exp(Common(NoType(BadLabel(Exp(Exp.int(3))))))),
              3,
            ),
            label("c"),
          ]),
        )
      ),
    )
  ),
  test_case("project_labels with holes for labels", `Quick, () =>
    annotated_tree_test(
      {|project_labels((a=1, b="", c=true), 'a', ?, 'c')|},
      prod([int(), unknown(Internal), bool()]),
      FIError.Exp.(
        ap(
          Forward,
          var("project_labels"),
          tuple([
            tuple([
              tup_label(label("a"), int(1)),
              tup_label(label("b"), string("")),
              tup_label(label("c"), bool(true)),
            ]),
            label("a"),
            empty_hole(),
            label("c"),
          ]),
        )
      ),
    )
  ),
  test_case("project_labels with label not in tuple", `Quick, () =>
    annotated_tree_test(
      {|project_labels((a=1, b=true, c=3), 'd')|},
      unknown(Internal),
      FIError.Exp.(
        ap(
          ~ann=Some(Exp(BuiltinError(ProjectLabelsMissingLabels(["d"])))),
          Forward,
          var("project_labels"),
          tuple([
            tuple([
              tup_label(label("a"), int(1)),
              tup_label(label("b"), bool(true)),
              tup_label(label("c"), int(3)),
            ]),
            label("d"),
          ]),
        )
      ),
    )
  ),
  test_case("project_labels with a single tuple and no labels", `Quick, () =>
    annotated_tree_test(
      {|project_labels((1, 2, 3))|},
      unknown(Internal),
      FIError.Exp.(
        ap(
          Forward,
          var("project_labels"),
          tuple([
            int(~ann=Some(Exp(BuiltinError(ArgumentMustBeTuple))), 1),
            int(
              ~ann=Some(Exp(Common(NoType(BadLabel(Exp(Exp.int(2))))))),
              2,
            ),
            int(
              ~ann=Some(Exp(Common(NoType(BadLabel(Exp(Exp.int(3))))))),
              3,
            ),
          ]),
        )
      ),
    )
  ),
  test_case("project_labels called with single arg", `Quick, () =>
    annotated_tree_test(
      {|project_labels(1)|},
      unknown(Internal),
      FIError.Exp.(
        ap(
          Forward,
          var("project_labels"),
          int(~ann=Some(Exp(BuiltinError(ArgumentMustBeTuple))), 1),
        )
      ),
    )
  ),
  test_case("project_labels with no args", `Quick, () =>
    annotated_tree_test(
      {|project_labels()|},
      unknown(Internal),
      FIError.Exp.(
        ap(
          Forward,
          var("project_labels"),
          tuple(~ann=Some(Exp(BuiltinError(ArgumentMustBeTuple))), []),
        )
      ),
    )
  ),
  test_case("project_labels with first arg unknown type", `Quick, () =>
    annotated_tree_test(
      {|project_labels(?, 'a')|},
      unknown(Internal),
      FIError.Exp.(
        ap(
          Forward,
          var("project_labels"),
          tuple([empty_hole(), label("a")]),
        )
      ),
    )
  ),
  test_case("project_labels with deferral as first arg", `Quick, () => {
    [@warning "-21"]
    {
      Alcotest.skip();
      annotated_tree_test(
        {|project_labels(_, 'a', 'b')|},
        unknown(Internal),
        FIError.Exp.(
          deferred_ap(
            var("project_labels"),
            [deferral(InAp), label("a"), label("b")],
          )
        ),
      );
    }
  }),
  test_case("project_labels with deferral in subsequent args", `Quick, () => {
    [@warning "-21"]
    {
      Alcotest.skip();
      annotated_tree_test(
        {|project_labels((a=1, b=true, c=3), 'a', _, 'c')|},
        prod([int(), unknown(Internal), int()]),
        FIError.Exp.(
          ap(
            Forward,
            var("project_labels"),
            tuple([
              tuple([
                tup_label(label("a"), int(1)),
                tup_label(label("b"), bool(true)),
                tup_label(label("c"), int(3)),
              ]),
              label("a"),
              deferral(
                ~ann=
                  Some(
                    Exp(
                      Common(NoType(BadLabel(Exp(Exp.deferral(InAp))))),
                    ),
                  ),
                InAp,
              ),
              label("c"),
            ]),
          )
        ),
      );
    }
  }),
  test_case("select_labels with appropriate labels", `Quick, () =>
    annotated_tree_test(
      {|select_labels((a=1, b=true, c=3), 'a', 'b')|},
      prod([tup_label(label("a"), int()), tup_label(label("b"), bool())]),
      FIError.Exp.(
        ap(
          Forward,
          var("select_labels"),
          tuple([
            tuple([
              tup_label(label("a"), int(1)),
              tup_label(label("b"), bool(true)),
              tup_label(label("c"), int(3)),
            ]),
            label("a"),
            label("b"),
          ]),
        )
      ),
    )
  ),
  test_case("select_labels with more appropriate labels", `Quick, () =>
    annotated_tree_test(
      {|select_labels((a=1,b=2,c=3), 'c', 'a', 'c')|},
      prod([
        tup_label(label("c"), int()),
        tup_label(label("a"), int()),
        tup_label(label("c"), int()),
      ]),
      FIError.Exp.(
        ap(
          Forward,
          var("select_labels"),
          tuple([
            tuple([
              tup_label(label("a"), int(1)),
              tup_label(label("b"), int(2)),
              tup_label(label("c"), int(3)),
            ]),
            label("c"),
            label("a"),
            label("c"),
          ]),
        )
      ),
    )
  ),
  test_case("select_labels with non-label", `Quick, () =>
    annotated_tree_test(
      {|select_labels((a=1, b=true, c=3), 'a', 3, 'c')|},
      prod([
        tup_label(label("a"), int()),
        unknown(Internal),
        tup_label(label("c"), int()),
      ]),
      FIError.Exp.(
        ap(
          Forward,
          var("select_labels"),
          tuple([
            tuple([
              tup_label(label("a"), int(1)),
              tup_label(label("b"), bool(true)),
              tup_label(label("c"), int(3)),
            ]),
            label("a"),
            int(
              ~ann=Some(Exp(Common(NoType(BadLabel(Exp(Exp.int(3))))))),
              3,
            ),
            label("c"),
          ]),
        )
      ),
    )
  ),
  test_case("select_labels with holes for labels", `Quick, () =>
    annotated_tree_test(
      {|select_labels((a=1, b="", c=true), 'a', ?, 'c')|},
      prod([
        tup_label(label("a"), int()),
        unknown(Internal),
        tup_label(label("c"), bool()),
      ]),
      FIError.Exp.(
        ap(
          Forward,
          var("select_labels"),
          tuple([
            tuple([
              tup_label(label("a"), int(1)),
              tup_label(label("b"), string("")),
              tup_label(label("c"), bool(true)),
            ]),
            label("a"),
            empty_hole(),
            label("c"),
          ]),
        )
      ),
    )
  ),
  test_case("select_labels with label not in tuple", `Quick, () =>
    annotated_tree_test(
      {|select_labels((a=1, b=true, c=3), 'd')|},
      unknown(Internal),
      FIError.Exp.(
        ap(
          ~ann=Some(Exp(BuiltinError(ProjectLabelsMissingLabels(["d"])))),
          Forward,
          var("select_labels"),
          tuple([
            tuple([
              tup_label(label("a"), int(1)),
              tup_label(label("b"), bool(true)),
              tup_label(label("c"), int(3)),
            ]),
            label("d"),
          ]),
        )
      ),
    )
  ),
  test_case("select_labels with a single tuple and no labels", `Quick, () =>
    annotated_tree_test(
      {|select_labels((1, 2, 3))|},
      unknown(Internal),
      FIError.Exp.(
        ap(
          Forward,
          var("select_labels"),
          tuple([
            int(~ann=Some(Exp(BuiltinError(ArgumentMustBeTuple))), 1),
            int(
              ~ann=Some(Exp(Common(NoType(BadLabel(Exp(Exp.int(2))))))),
              2,
            ),
            int(
              ~ann=Some(Exp(Common(NoType(BadLabel(Exp(Exp.int(3))))))),
              3,
            ),
          ]),
        )
      ),
    )
  ),
  test_case("select_labels called with single arg", `Quick, () =>
    annotated_tree_test(
      {|select_labels(1)|},
      unknown(Internal),
      FIError.Exp.(
        ap(
          Forward,
          var("select_labels"),
          int(~ann=Some(Exp(BuiltinError(ArgumentMustBeTuple))), 1),
        )
      ),
    )
  ),
  test_case("select_labels with no args", `Quick, () =>
    annotated_tree_test(
      {|select_labels()|},
      unknown(Internal),
      FIError.Exp.(
        ap(
          Forward,
          var("select_labels"),
          tuple(~ann=Some(Exp(BuiltinError(ArgumentMustBeTuple))), []),
        )
      ),
    )
  ),
  test_case("select_labels with first arg unknown type", `Quick, () =>
    annotated_tree_test(
      {|select_labels(?, 'a')|},
      unknown(Internal),
      FIError.Exp.(
        ap(
          Forward,
          var("select_labels"),
          tuple([empty_hole(), label("a")]),
        )
      ),
    )
  ),
  test_case("select_labels with deferral as first arg", `Quick, () => {
    [@warning "-21"]
    {
      Alcotest.skip();
      annotated_tree_test(
        {|select_labels(_, 'a', 'b')|},
        unknown(Internal),
        FIError.Exp.(
          deferred_ap(
            var("select_labels"),
            [deferral(InAp), label("a"), label("b")],
          )
        ),
      );
    }
  }),
  test_case("select_labels with deferral in subsequent args", `Quick, () => {
    [@warning "-21"]
    {
      Alcotest.skip();
      annotated_tree_test(
        {|select_labels((a=1, b=true, c=3), 'a', _, 'c')|},
        prod([int(), unknown(Internal), int()]),
        FIError.Exp.(
          ap(
            Forward,
            var("select_labels"),
            tuple([
              tuple([
                tup_label(label("a"), int(1)),
                tup_label(label("b"), bool(true)),
                tup_label(label("c"), int(3)),
              ]),
              label("a"),
              deferral(
                ~ann=
                  Some(
                    Exp(
                      Common(NoType(BadLabel(Exp(Exp.deferral(InAp))))),
                    ),
                  ),
                InAp,
              ),
              label("c"),
            ]),
          )
        ),
      );
    }
  }),
  fully_consistent_typecheck(
    "primitive_pivot with single tuple",
    {|primitive_pivot([(a="hello", b=3, c=4)], 'a')|},
    Some(unknown(Internal)),
  ),
  fully_consistent_typecheck(
    "primitive_pivot with multiple tuples",
    {|primitive_pivot([(a="hello", b=3, c=4), (a="World", b=2, c=2)], 'a')|},
    Some(unknown(Internal)),
  ),
  test_case("primitive_pivot with missing label", `Quick, () =>
    annotated_tree_test(
      {|primitive_pivot([(a="hello", b=3)], 'c')|},
      unknown(Internal),
      FIError.Exp.(
        ap(
          ~ann=Some(Exp(BuiltinError(MissingLabels(["c"])))),
          Forward,
          var("primitive_pivot"),
          tuple([
            list_lit([
              tuple([
                tup_label(label("a"), string("hello")),
                tup_label(label("b"), int(3)),
              ]),
            ]),
            label("c"),
          ]),
        )
      ),
    )
  ),
  test_case("primitive_pivot with non-string pivot field", `Quick, () =>
    annotated_tree_test(
      {|primitive_pivot([(a=1, b=3)], 'a')|},
      unknown(Internal),
      FIError.Exp.(
        ap(
          ~ann=Some(Exp(BuiltinError(PivotLabelIsNotString(Typ.int())))),
          Forward,
          var("primitive_pivot"),
          tuple([
            list_lit([
              tuple([
                tup_label(label("a"), int(1)),
                tup_label(label("b"), int(3)),
              ]),
            ]),
            label("a"),
          ]),
        )
      ),
    )
  ),
  test_case("primitive_pivot with non-label second argument", `Quick, () =>
    annotated_tree_test(
      {|primitive_pivot([(a="hello", b=3)], 5)|},
      unknown(Internal),
      FIError.Exp.(
        ap(
          Forward,
          var("primitive_pivot"),
          tuple([
            list_lit([
              tuple([
                tup_label(label("a"), string("hello")),
                tup_label(label("b"), int(3)),
              ]),
            ]),
            int(
              ~ann=Some(Exp(Common(NoType(BadLabel(Exp(Exp.int(5))))))),
              5,
            ),
          ]),
        )
      ),
    )
  ),
  test_case("primitive_pivot with non-list first argument", `Quick, () =>
    annotated_tree_test(
      {|primitive_pivot(5, 'a')|},
      unknown(Internal),
      FIError.Exp.(
        ap(
          Forward,
          var("primitive_pivot"),
          tuple([
            int(
              ~ann=Some(Exp(BuiltinError(PivotFirstArgNotListOfTuples))),
              5,
            ),
            label("a"),
          ]),
        )
      ),
    )
  ),
  test_case("primitive_pivot with extra arguments", `Quick, () =>
    annotated_tree_test(
      {|primitive_pivot([(a="hello", b=3)], 'a', 'b')|},
      unknown(Internal),
      FIError.Exp.(
        ap(
          Forward,
          var("primitive_pivot"),
          tuple(
            ~ann=
              Some(
                Exp(
                  Common(
                    Inconsistent(
                      Typ.(
                        Expectation({
                          ana:
                            prod([
                              list(unknown(Internal)),
                              unknown(Internal),
                            ]),
                          syn:
                            prod([
                              list(
                                prod([
                                  tup_label(label("a"), string()),
                                  tup_label(label("b"), int()),
                                ]),
                              ),
                              label("a"),
                              label("b"),
                            ]),
                        })
                      ),
                    ),
                  ),
                ),
              ),
            [
              list_lit([
                tuple([
                  tup_label(label("a"), string("hello")),
                  tup_label(label("b"), int(3)),
                ]),
              ]),
              label("a"),
              label("b"),
            ],
          ),
        )
      ),
    )
  ),
];
