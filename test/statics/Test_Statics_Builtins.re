open Test_Statics_Prelude;
open Alcotest;
open FTemp;
open Typ;
// TODO do versions with variables where appropriate and do reverse partial application

module MeltOperation = {
  let tests = [
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
    test_case("Melt operation with hole in tuple label position", `Quick, () =>
      annotated_tree_test(
        "melt((?=1, b=2, c=3))",
        list(
          prod([
            tup_label(label("label"), string()),
            tup_label(label("value"), int()),
          ]),
        ),
        FIError.Exp.(
          ap(
            Forward,
            var("melt"),
            tuple([
              tup_label(empty_hole(), int(1)),
              tup_label(label("b"), int(2)),
              tup_label(label("c"), int(3)),
            ]),
          )
        ),
      )
    ),
  ];
};

module ProjectLabels = {
  let tests = [
    test_case("project_labels with appropriate labels", `Quick, () =>
      annotated_tree_test(
        {|project_labels((a=1,b=true,c=3.0), 'c', 'a', 'c')|},
        prod([float(), int(), float()]),
        FIError.Exp.(
          ap(
            Forward,
            var("project_labels"),
            tuple([
              tuple([
                tup_label(label("a"), int(1)),
                tup_label(label("b"), bool(true)),
                tup_label(label("c"), float(3.0)),
              ]),
              label("c"),
              label("a"),
              label("c"),
            ]),
          )
        ),
      )
    ),
    fully_consistent_typecheck(
      "project_labels with single appropriate labels",
      {|project_labels((a=1, b=true, c=3), 'a')|},
      Some(int()),
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
                ~ann=
                  Some(Exp(Common(NoType(BadLabel(Exp(Exp.int(3))))))),
                3,
              ),
              label("c"),
            ]),
          )
        ),
      )
    ),
    fully_consistent_typecheck(
      "project_labels with holes for labels",
      {|project_labels((a=1, b="", c=true), 'a', ?, 'c')|},
      Some(prod([int(), unknown(Internal), bool()])),
    ),
    test_case("project_labels with label not in tuple", `Quick, () =>
      annotated_tree_test(
        {|project_labels((a=1, b=true, c=3), 'd')|},
        unknown(Internal),
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
              label(
                ~ann=Some(Exp(Common(NoType(InvalidLabel("d"))))),
                "d",
              ),
            ]),
          )
        ),
      )
    ),
    test_case("project_labels with a single tuple and no labels", `Quick, () =>
      annotated_tree_test(
        {|project_labels(1, 2, 3)|},
        prod([unknown(Internal), unknown(Internal)]),
        FIError.Exp.(
          ap(
            Forward,
            var("project_labels"),
            tuple([
              int(~ann=Some(Exp(BuiltinError(ArgumentMustBeTuple))), 1),
              int(
                ~ann=
                  Some(Exp(Common(NoType(BadLabel(Exp(Exp.int(2))))))),
                2,
              ),
              int(
                ~ann=
                  Some(Exp(Common(NoType(BadLabel(Exp(Exp.int(3))))))),
                3,
              ),
            ]),
          )
        ),
      )
    ),
    test_case(
      "project_labels with a single tuple in parens and no labels", `Quick, () =>
      annotated_tree_test(
        {|project_labels((1, 2, 3))|},
        unknown(Internal),
        FIError.Exp.(
          ap(
            ~ann=Some(Exp(BuiltinError(AtLeast2Arguments))),
            Forward,
            var("project_labels"),
            parens(tuple([int(1), int(2), int(3)])),
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
            ~ann=Some(Exp(BuiltinError(AtLeast2Arguments))),
            Forward,
            var("project_labels"),
            int(1),
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
            ~ann=Some(Exp(BuiltinError(AtLeast2Arguments))), // Has to be on the ap to show up
            Forward,
            var("project_labels"),
            tuple([]),
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
    test_case("project_labels with hole in tuple label position", `Quick, () =>
      annotated_tree_test(
        {|project_labels((?=1, b=true, c=3), 'b', 'c')|},
        prod([bool(), int()]),
        FIError.Exp.(
          ap(
            Forward,
            var("project_labels"),
            tuple([
              tuple([
                tup_label(empty_hole(), int(1)),
                tup_label(label("b"), bool(true)),
                tup_label(label("c"), int(3)),
              ]),
              label("b"),
              label("c"),
            ]),
          )
        ),
      )
    ),
    test_case("project labels with a single labeled entry", `Quick, () => {
      annotated_tree_test(
        {|project_labels(b=3)|},
        unknown(Internal),
        FIError.Exp.(
          ap(
            ~ann=Some(Exp(BuiltinError(AtLeast2Arguments))),
            Forward,
            var("project_labels"),
            tuple([tup_label(label("b"), int(3))]),
          )
        ),
      )
    }),
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
  ];
};

module SelectLabels = {
  let tests = [
    test_case("select_labels with appropriate labels", `Quick, () =>
      annotated_tree_test(
        {|select_labels((a=1, b=true, c=3), 'a', 'b')|},
        prod([
          tup_label(label("a"), int()),
          tup_label(label("b"), bool()),
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
                ~ann=
                  Some(Exp(Common(NoType(BadLabel(Exp(Exp.int(3))))))),
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
            Forward,
            var("select_labels"),
            tuple([
              tuple([
                tup_label(label("a"), int(1)),
                tup_label(label("b"), bool(true)),
                tup_label(label("c"), int(3)),
              ]),
              label(
                ~ann=Some(Exp(Common(NoType(InvalidLabel("d"))))),
                "d",
              ),
            ]),
          )
        ),
      )
    ),
    test_case("select_labels with a single tuple and no labels", `Quick, () =>
      annotated_tree_test(
        {|select_labels(1, 2, 3)|},
        prod([unknown(Internal), unknown(Internal)]),
        FIError.Exp.(
          ap(
            Forward,
            var("select_labels"),
            tuple([
              int(~ann=Some(Exp(BuiltinError(ArgumentMustBeTuple))), 1),
              int(
                ~ann=
                  Some(Exp(Common(NoType(BadLabel(Exp(Exp.int(2))))))),
                2,
              ),
              int(
                ~ann=
                  Some(Exp(Common(NoType(BadLabel(Exp(Exp.int(3))))))),
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
            ~ann=Some(Exp(BuiltinError(AtLeast2Arguments))),
            Forward,
            var("select_labels"),
            int(1),
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
            ~ann=Some(Exp(BuiltinError(AtLeast2Arguments))),
            Forward,
            var("select_labels"),
            tuple([]),
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
    test_case("select_labels with hole in tuple label position", `Quick, () =>
      annotated_tree_test(
        {|select_labels((?=1, b=true, c=3), 'b', 'c')|},
        prod([
          tup_label(label("b"), bool()),
          tup_label(label("c"), int()),
        ]),
        FIError.Exp.(
          ap(
            Forward,
            var("select_labels"),
            tuple([
              tuple([
                tup_label(empty_hole(), int(1)),
                tup_label(label("b"), bool(true)),
                tup_label(label("c"), int(3)),
              ]),
              label("b"),
              label("c"),
            ]),
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
  ];
};

module PrimitivePivot = {
  let tests = [
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
            Forward,
            var("primitive_pivot"),
            tuple([
              list_lit([
                tuple([
                  tup_label(label("a"), string("hello")),
                  tup_label(label("b"), int(3)),
                ]),
              ]),
              label(
                ~ann=Some(Exp(Common(NoType(InvalidLabel("c"))))),
                "c",
              ),
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
            Forward,
            var("primitive_pivot"),
            tuple([
              list_lit([
                tuple([
                  tup_label(label("a"), int(1)),
                  tup_label(label("b"), int(3)),
                ]),
              ]),
              label(
                ~ann=
                  Some(
                    Exp(BuiltinError(PivotLabelIsNotString(Typ.int()))),
                  ),
                "a",
              ),
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
                ~ann=
                  Some(Exp(Common(NoType(BadLabel(Exp(Exp.int(5))))))),
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
                ~ann=Some(Exp(BuiltinError(ArgumentMustBeListOfTuples))),
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
            ~ann=Some(Exp(BuiltinError(Exactly2Arguments))),
            Forward,
            var("primitive_pivot"),
            tuple([
              list_lit([
                tuple([
                  tup_label(label("a"), string("hello")),
                  tup_label(label("b"), int(3)),
                ]),
              ]),
              label("a"),
              label("b"),
            ]),
          )
        ),
      )
    ),
    test_case("primitive_pivot with hole in tuple label position", `Quick, () =>
      annotated_tree_test(
        {|primitive_pivot([(a="hello", ?=3, c=4)], 'a')|},
        unknown(Internal),
        FIError.Exp.(
          ap(
            Forward,
            var("primitive_pivot"),
            tuple([
              list_lit([
                tuple([
                  tup_label(label("a"), string("hello")),
                  tup_label(empty_hole(), int(3)),
                  tup_label(label("c"), int(4)),
                ]),
              ]),
              label("a"),
            ]),
          )
        ),
      )
    ),
    test_case("primitive pivot with unknown type in first arg", `Quick, () =>
      annotated_tree_test(
        {|primitive_pivot(?, 'a')|},
        unknown(Internal),
        FIError.Exp.(
          ap(
            Forward,
            var("primitive_pivot"),
            tuple([empty_hole(), label("a")]),
          )
        ),
      )
    ),
    test_case(
      "primitive pivot with ascription to unknown in label position",
      `Quick,
      () =>
      annotated_tree_test(
        {|primitive_pivot([(a="hello", b=3)], 'a' : ?)|},
        unknown(Internal),
        FIError.(
          Exp.(
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
                asc(
                  ~ann=
                    Some(
                      Exp(
                        Common(
                          NoType(
                            BadLabel(
                              Exp(
                                FTemp.(
                                  Exp.(
                                    asc(label("a"), Typ.unknown(Internal))
                                  )
                                ),
                              ),
                            ),
                          ),
                        ),
                      ),
                    ),
                  label("a"),
                  Typ.unknown(Internal),
                ),
              ]),
            )
          )
        ),
      )
    ),
  ];
};

module OmitLabels = {
  let tests = [
    test_case("omit_labels with appropriate labels", `Quick, () =>
      annotated_tree_test(
        {|omit_labels((a=1, b=true, c=3), 'a', 'b')|},
        prod([tup_label(label("c"), int())]),
        FIError.Exp.(
          ap(
            Forward,
            var("omit_labels"),
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
    test_case("omit_labels with non-label", `Quick, () =>
      annotated_tree_test(
        {|omit_labels((a=1, b=true, c=3), 'a', 3, 'c')|},
        prod([tup_label(label("b"), bool())]),
        FIError.Exp.(
          ap(
            Forward,
            var("omit_labels"),
            tuple([
              tuple([
                tup_label(label("a"), int(1)),
                tup_label(label("b"), bool(true)),
                tup_label(label("c"), int(3)),
              ]),
              label("a"),
              int(
                ~ann=
                  Some(Exp(Common(NoType(BadLabel(Exp(Exp.int(3))))))),
                3,
              ),
              label("c"),
            ]),
          )
        ),
      )
    ),
    test_case("omit_labels with holes for labels to omit", `Quick, () =>
      annotated_tree_test(
        {|omit_labels((a=1, b="", c=true), 'a', ?, 'c')|},
        prod([tup_label(label("b"), string())]), // I don't know what we want here. We could just return unknown
        FIError.Exp.(
          ap(
            Forward,
            var("omit_labels"),
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
    test_case("omit_labels with label not in tuple", `Quick, () =>
      annotated_tree_test(
        {|omit_labels((a=1, b=true, c=3), 'd')|},
        unknown(Internal),
        FIError.Exp.(
          ap(
            ~ann=
              Some(Exp(BuiltinError(ProjectLabelsMissingLabels(["d"])))),
            Forward,
            var("omit_labels"),
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
    test_case("omit_labels with hole in tuple label position", `Quick, () =>
      annotated_tree_test(
        {|omit_labels((?=1, b=true, c=3), 'b', 'c')|},
        prod([tup_label(empty_hole(), int())]),
        FIError.Exp.(
          ap(
            Forward,
            var("omit_labels"),
            tuple([
              tuple([
                tup_label(empty_hole(), int(1)),
                tup_label(label("b"), bool(true)),
                tup_label(label("c"), int(3)),
              ]),
              label("b"),
              label("c"),
            ]),
          )
        ),
      )
    ),
    test_case("omit_labels with a single tuple and no labels", `Quick, () =>
      annotated_tree_test(
        {|omit_labels((1, 2, 3))|},
        unknown(Internal),
        FIError.Exp.(
          ap(
            Forward,
            var("omit_labels"),
            tuple([
              int(~ann=Some(Exp(BuiltinError(ArgumentMustBeTuple))), 1),
              int(
                ~ann=
                  Some(Exp(Common(NoType(BadLabel(Exp(Exp.int(2))))))),
                2,
              ),
              int(
                ~ann=
                  Some(Exp(Common(NoType(BadLabel(Exp(Exp.int(3))))))),
                3,
              ),
            ]),
          )
        ),
      )
    ),
    test_case("omit_labels called with single arg", `Quick, () =>
      annotated_tree_test(
        {|omit_labels(1)|},
        unknown(Internal),
        FIError.Exp.(
          ap(
            Forward,
            var("omit_labels"),
            int(~ann=Some(Exp(BuiltinError(ArgumentMustBeTuple))), 1),
          )
        ),
      )
    ),
    test_case("omit_labels with no args", `Quick, () =>
      annotated_tree_test(
        {|omit_labels()|},
        unknown(Internal),
        FIError.Exp.(
          ap(
            Forward,
            var("omit_labels"),
            tuple(~ann=Some(Exp(BuiltinError(ArgumentMustBeTuple))), []),
          )
        ),
      )
    ),
    test_case("omit_labels with first arg unknown type", `Quick, () =>
      annotated_tree_test(
        {|omit_labels(?, 'a')|},
        unknown(Internal),
        FIError.Exp.(
          ap(
            Forward,
            var("omit_labels"),
            tuple([empty_hole(), label("a")]),
          )
        ),
      )
    ),
  ];
};

module DropLabels = {
  let tests = [
    fully_consistent_typecheck(
      "Drop labels with some labels",
      {|drop_labels((a=1, b=2.0, true, d=""))|},
      Some(list(prod([int(), float(), bool(), string()]))),
    ),
    fully_consistent_typecheck(
      "Drop labels with type alias and autolabels",
      {|type Entry =(name=String, age=Int, quiz1=Int, quiz2=Int, midterm=Int, quiz3=Int, quiz4=Int, final=Int) in
        drop_labels(("bob",   12, 8, 9, 77, 7, 9, 87) : Entry)|},
      Some(
        list(
          prod([string(), int(), int(), int(), int(), int(), int(), int()]),
        ),
      ),
    ),
    test_case("Drop labels operation with no labels", `Quick, () =>
      annotated_tree_test(
        "drop_labels(1, 2)",
        list(prod([int(), int()])),
        FIError.Exp.(
          ap(Forward, var("drop_labels"), tuple([int(1), int(2)]))
        ),
      )
    ),
    test_case("Drop labels applied to non-tuple", `Quick, () =>
      annotated_tree_test(
        "drop_labels(1)",
        unknown(Internal),
        FIError.Exp.(
          ap(
            ~ann=Some(Exp(BuiltinError(ArgumentMustBeTuple))),
            Forward,
            var("drop_labels"),
            int(1),
          )
        ),
      )
    ),
    test_case(
      "Drop labels operation applied to value with unknown type", `Quick, () =>
      annotated_tree_test(
        "drop_labels(?)",
        unknown(Internal),
        FIError.Exp.(ap(Forward, var("drop_labels"), empty_hole())),
      )
    ),
    test_case(
      "Drop labels operation with hole in tuple label position", `Quick, () =>
      annotated_tree_test(
        "drop_labels((?=1, b=2, c=3))",
        list(prod([int(), int(), int()])),
        FIError.Exp.(
          ap(
            Forward,
            var("drop_labels"),
            tuple([
              tup_label(empty_hole(), int(1)),
              tup_label(label("b"), int(2)),
              tup_label(label("c"), int(3)),
            ]),
          )
        ),
      )
    ),
  ];
};

let tests =
  MeltOperation.tests
  @ ProjectLabels.tests
  @ SelectLabels.tests
  @ PrimitivePivot.tests
  @ OmitLabels.tests
  @ DropLabels.tests;
