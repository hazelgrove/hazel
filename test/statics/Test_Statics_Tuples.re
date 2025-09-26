open Alcotest;
open Test_Statics_Prelude;
open FTemp;
open Typ;
module TupleExtension = {
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
              int(~ann=Some(Exp(TupleExtensionRequiresTuples)), 1),
              int(~ann=Some(Exp(TupleExtensionRequiresTuples)), 2),
            )
          )
        ),
      )
    ),
    test_case("Tuple extension with hole", `Quick, () =>
      annotated_tree_test(
        "? ... (3, 4)",
        unknown(Internal),
        FIError.Exp.(
          tuple_extension(empty_hole(), tuple([int(3), int(4)]))
        ),
      )
    ),
    test_case("Tuple extension with hole in label position", `Quick, () =>
      annotated_tree_test(
        "(?=1, 2) ... (3, 4)",
        prod([tup_label(empty_hole(), int()), int(), int(), int()]),
        FIError.Exp.(
          tuple_extension(
            tuple([tup_label(empty_hole(), int(1))]),
            tuple([int(2), int(3), int(4)]),
          )
        ),
      )
    ),
  ];
};

module ProductProjection = {
  let tests = [
    fully_consistent_typecheck(
      "Consistent Type-level product projection",
      {|type T = (a=Int, String) in 1 : T.a |},
      Some(
        prod_projection(
          prod([tup_label(label("a"), int()), string()]),
          label("a"),
        ),
      ),
    ),
    test_case("Inconsistent Type-level product projection", `Quick, () => {
      annotated_tree_test(
        {|type T = (a=Int, String) in "" : T.a |},
        Typ.(
          prod_projection(
            prod([tup_label(label("a"), int()), string()]),
            label("a"),
          )
        ),
        FIError.(
          Exp.(
            ty_alias(
              TPat.var("T"),
              Typ.(prod([tup_label(label("a"), int()), string()])),
              asc(
                string(
                  ~ann=
                    FTemp.Typ.(
                      Some(
                        Exp(
                          Common(
                            Inconsistent(
                              Expectation({
                                ana: prod_projection(var("T"), label("a")),
                                syn: string(),
                              }),
                            ),
                          ),
                        ),
                      )
                    ),
                  "",
                ),
                Typ.(prod_projection(var("T"), label("a"))),
              ),
            )
          )
        ),
      )
    }),
    test_case("Missing label projection", `Quick, () => {
      FIError.(
        annotated_tree_test(
          {|type T = (a=Int, String) in 1 : T.b |},
          prod_projection(
            prod([tup_label(label("a"), int()), string()]),
            label("b"),
          ),
          FIError.Exp.(
            ty_alias(
              TPat.var("T"),
              Typ.(prod([tup_label(label("a"), int()), string()])),
              asc(
                int(1),
                Typ.(
                  prod_projection(
                    var("T"),
                    label(~ann=Some(Typ(InvalidLabel("b", ["a"]))), "b"),
                  )
                ),
              ),
            )
          ),
        )
      )
    }),
  ];
};

module ExplicitlyUnlabeledTuples = {
  let tests = [
    fully_consistent_typecheck(
      ~normalize=true,
      "Explicitly unlabeled tuple in let binding",
      {|(_=1) : (_=Int)|},
      Some(prod([int()])),
    ),
    fully_consistent_typecheck(
      ~normalize=true,
      "Multiple elements explicitly unlabeled",
      {|(_=1, _="") : (_=Int, _=String)|},
      Some(prod([int(), string()])),
    ),
    fully_consistent_typecheck(
      ~normalize=true,
      "Explicitly unlabeled elements with implicit type",
      {|(_=1,_="") : (Int, String)|},
      Some(prod([int(), string()])),
    ),
    fully_consistent_typecheck(
      ~normalize=true,
      "Implicitly unlabeled elements with explicitly unlabeled types",
      {|(1,"") : (_=Int, _=String)|},
      Some(prod([int(), string()])),
    ),
  ];
};
let tests = (
  "Statics.Tuples",
  [
    test_case(
      "Typechecking fails for unlabeled variable being assigned to labeled tuple",
      `Quick,
      () => {
      annotated_tree_test(
        "let x = (1, 2) in let y : (a=Int, b=Int) = x in y",
        Typ.prod([
          tup_label(label("a"), int()),
          tup_label(label("b"), int()),
        ]),
        FIError.(
          Exp.(
            let_(
              Pat.(var("x")),
              parens(tuple([int(1), int(2)])),
              let_(
                Pat.(
                  asc(
                    var("y"),
                    Typ.(
                      parens(
                        prod([
                          tup_label(label("a"), int()),
                          tup_label(label("b"), int()),
                        ]),
                      )
                    ),
                  )
                ),
                var(
                  ~ann=
                    Some(
                      FTemp.Typ.(
                        Exp(
                          Common(
                            Inconsistent(
                              Expectation({
                                ana:
                                  parens(
                                    prod([
                                      tup_label(label("a"), int()),
                                      tup_label(label("b"), int()),
                                    ]),
                                  ),
                                syn: prod([int(), int()]),
                              }),
                            ),
                          ),
                        )
                      ),
                    ),
                  "x",
                ),
                var("y"),
              ),
            )
          )
        ),
      )
    }),
    fully_consistent_typecheck(
      "Assigning labeled tuple to variable",
      "let x = (l=32) in let y : (l=Int) = x in y",
      Some(prod([tup_label(label("l"), int())])),
    ),
    fully_consistent_typecheck(
      "Singleton Labeled Tuple ascription in let",
      "let x : (l=String) = (\"a\") in x",
      Some(prod([tup_label(label("l"), string())])),
    ),
    test_case(
      "Singleton Labeled Tuple ascription in let with wrong type should fail",
      `Quick,
      () => {
      annotated_tree_test(
        "let x : (l=String) = 1 in x",
        prod([tup_label(label("l"), string())]),
        FIError.(
          Exp.(
            let_(
              Pat.(
                asc(
                  var("x"),
                  Typ.(parens(prod([tup_label(label("l"), string())]))),
                )
              ),
              int(
                ~ann=
                  Some(
                    FTemp.Typ.(
                      Exp(
                        Common(
                          Inconsistent(
                            Expectation({
                              ana: string(),
                              syn: int(),
                            }),
                          ),
                        ),
                      )
                    ),
                  ),
                1,
              ),
              var("x"),
            )
          )
        ),
      )
    }),
    fully_consistent_typecheck(
      "Singleton Labeled Tuple with specified label",
      "let x : (l=String) = (l=\"a\") in x",
      Some(prod([tup_label(label("l"), string())])),
    ),
    fully_consistent_typecheck(
      "Labeled tuple with multiple labels",
      {|(l=32, l2="")|},
      Some(
        prod([
          tup_label(label("l"), int()),
          tup_label(label("l2"), string()),
        ]),
      ),
    ),
    fully_consistent_typecheck(
      "Let statement that adds labels during elaboration",
      {|let x : (name=String, age=Int)= ("Bob", 20) in x|},
      Some(
        prod([
          tup_label(label("name"), string()),
          tup_label(label("age"), int()),
        ]),
      ),
    ),
    fully_consistent_typecheck(
      "Duplicate singleton labels",
      {|let y : (l=(l=Int)) = (l=1) in y|},
      Some(
        prod([
          tup_label(
            label("l"),
            parens(prod([tup_label(label("l"), int())])),
          ),
        ]),
      ),
    ),
    fully_consistent_typecheck(
      "Reconstructed labeled tuple without values",
      {|let x : (l=|},
      Some(unknown(Internal)),
    ),
    fully_consistent_typecheck(
      "Singleton labeled argument let with unknown type",
      {|let x : (a=?) = (a=1) in x|},
      Some(prod([tup_label(label("a"), unknown(Hole(EmptyHole)))])),
    ),
    fully_consistent_typecheck(
      "nested different singleton labeled arguments",
      {|let x : (b=c=String) = b="" in x|},
      Some(
        prod([
          tup_label(label("b"), prod([tup_label(label("c"), string())])),
        ]),
      ),
    ),
    fully_consistent_typecheck(
      "nested different singleton labeled arguments",
      {|let x : (a=b=c=?) = b=? in x|},
      Some(
        prod([
          tup_label(
            label("a"),
            prod([
              tup_label(
                label("b"),
                prod([tup_label(label("c"), unknown(Hole(EmptyHole)))]),
              ),
            ]),
          ),
        ]),
      ),
    ),
    fully_consistent_typecheck(
      "Singleton labeled argument function application with unknown type",
      {|(fun a=x->x)(a=1)|},
      Some(unknown(Internal)),
    ),
    fully_consistent_typecheck(
      "Singleton labeled argument function application with no labeled param",
      {|(fun a=x->x)(1)|},
      Some(unknown(Internal)),
    ),
    fully_consistent_typecheck(
      "Singleton labeled argument not labeled in pattern",
      {|let x : (a=Int) -> Int = fun a -> a in x(2)|},
      Some(int()),
    ),
    test_case("Unknown label in last position", `Quick, () => {
      annotated_tree_test(
        {|(1, 1.2, z="hello") : (a=Int, b=Float, String)|},
        prod([
          tup_label(label("a"), int()),
          tup_label(label("b"), float()),
          string(),
        ]),
        FIError.(
          Exp.(
            asc(
              parens(
                ~ann=
                  Some(
                    FTemp.Typ.(
                      Exp(
                        Common(
                          Inconsistent(
                            Expectation({
                              ana:
                                prod([
                                  tup_label(label("a"), int()),
                                  tup_label(label("b"), float()),
                                  string(),
                                ]),
                              syn:
                                prod([
                                  tup_label(label("a"), int()),
                                  tup_label(label("b"), float()),
                                  tup_label(label("z"), string()),
                                ]),
                            }),
                          ),
                        ),
                      )
                    ),
                  ),
                tuple(
                  ~ann=
                    Some(
                      Exp(
                        Common(
                          TupleLabelError({
                            malformed_labels: [],
                            duplicate_labels: [],
                            invalid_labels: ["z"],
                            typ:
                              FTemp.Typ.(
                                prod([
                                  tup_label(label("a"), int()),
                                  tup_label(label("b"), float()),
                                  tup_label(label("z"), string()),
                                ])
                              ),
                          }),
                        ),
                      ),
                    ),
                  [
                    int(1),
                    float(1.2),
                    tup_label(
                      ~ann=
                        Some(
                          FTemp.Typ.(
                            Exp(
                              Common(
                                TupleLabelError({
                                  malformed_labels: [],
                                  duplicate_labels: [],
                                  invalid_labels: ["z"],
                                  typ: tup_label(label("z"), string()),
                                }),
                              ),
                            )
                          ),
                        ),
                      label(
                        ~ann=
                          Some(
                            Exp(
                              Common(
                                NoType(InvalidLabel("z", ["a", "b"])),
                              ),
                            ),
                          ),
                        "z",
                      ),
                      string("hello"),
                    ),
                  ],
                ),
              ),
              Typ.(
                parens(
                  prod([
                    tup_label(label("a"), int()),
                    tup_label(label("b"), float()),
                    string(),
                  ]),
                )
              ),
            )
          )
        ),
      )
    }),
    test_case("Duplicate label synthesis", `Quick, () => {
      annotated_tree_test(
        {|(a="hello", a=3)|},
        prod([tup_label(label("a"), unknown(Internal))]),
        FIError.(
          Exp.(
            parens(
              tuple(
                ~ann=
                  Some(
                    FTemp.Typ.(
                      Exp(
                        Common(
                          TupleLabelError({
                            malformed_labels: [],
                            duplicate_labels: ["a", "a"],
                            invalid_labels: [],
                            typ:
                              prod([
                                tup_label(label("a"), unknown(Internal)),
                              ]),
                          }),
                        ),
                      )
                    ),
                  ),
                [
                  tup_label(
                    ~ann=
                      Some(
                        FTemp.Typ.(
                          Exp(
                            Common(
                              TupleLabelError({
                                malformed_labels: [],
                                duplicate_labels: ["a"],
                                invalid_labels: [],
                                typ: tup_label(label("a"), string()),
                              }),
                            ),
                          )
                        ),
                      ),
                    label(
                      ~ann=
                        Some(
                          FTemp.Typ.(
                            Exp(Common(DuplicateLabel("a", label("a"))))
                          ),
                        ),
                      "a",
                    ),
                    string("hello"),
                  ),
                  tup_label(
                    ~ann=
                      Some(
                        FTemp.Typ.(
                          Exp(
                            Common(
                              TupleLabelError({
                                malformed_labels: [],
                                duplicate_labels: ["a"],
                                invalid_labels: [],
                                typ: tup_label(label("a"), int()),
                              }),
                            ),
                          )
                        ),
                      ),
                    label(
                      ~ann=
                        Some(
                          FTemp.Typ.(
                            Exp(Common(DuplicateLabel("a", label("a"))))
                          ),
                        ),
                      "a",
                    ),
                    int(3),
                  ),
                ],
              ),
            )
          )
        ),
      )
    }),
    test_case("Bad label projection", `Quick, () => {
      annotated_tree_test(
        {|(1, 2) . 1|},
        unknown(Internal),
        FIError.(
          Exp.(
            dot(
              ~ann=
                Some(
                  Exp(
                    Common(
                      NoType(
                        BadLabel(
                          Exp(FTemp.Exp.(multi_hole([Exp(int(1))]))),
                        ),
                      ),
                    ),
                  ),
                ),
              tuple([int(1), int(2)]),
              multi_hole([Exp(int(1))]),
            )
          )
        ),
      )
    }),
    test_case("Singleton Bad label synthesis", `Quick, () => {
      annotated_tree_test(
        {|(1="hello")|},
        prod([tup_label(unknown(Internal), string())]),
        FIError.(
          Exp.(
            parens(
              tuple(
                ~ann=
                  Some(
                    FTemp.(
                      Exp(
                        Common(
                          TupleLabelError({
                            malformed_labels: [
                              Exp.(Exp(multi_hole([Exp(label("1"))]))),
                            ],
                            duplicate_labels: [],
                            invalid_labels: [],
                            typ:
                              Typ.(
                                prod([
                                  tup_label(unknown(Internal), string()),
                                ])
                              ),
                          }),
                        ),
                      )
                    ),
                  ),
                [
                  tup_label(
                    ~ann=
                      Some(
                        FTemp.(
                          Exp(
                            Common(
                              TupleLabelError({
                                malformed_labels: [
                                  Exp.(Exp(multi_hole([Exp(label("1"))]))),
                                ],
                                duplicate_labels: [],
                                invalid_labels: [],
                                typ:
                                  Typ.(
                                    tup_label(unknown(Internal), string())
                                  ),
                              }),
                            ),
                          )
                        ),
                      ),
                    multi_hole(
                      ~ann=
                        Some(
                          Exp(
                            Common(
                              NoType(
                                BadLabel(
                                  FTemp.Exp.(
                                    Exp(multi_hole([Exp(label("1"))]))
                                  ),
                                ),
                              ),
                            ),
                          ),
                        ),
                      [
                        Exp(
                          label(
                            ~ann=
                              Some(
                                Exp(
                                  Common(NoType(UnexpectedLabelSort("1"))),
                                ),
                              ), // Has UnexpectedLabelSort because the label is wrapped in a multi-hole
                            "1",
                          ),
                        ),
                      ],
                    ),
                    string("hello"),
                  ),
                ],
              ),
            )
          )
        ),
      )
    }),
    test_case("Bad label synthesis", `Quick, () => {
      annotated_tree_test(
        {|(1="hello", a=3)|},
        prod([
          tup_label(unknown(Internal), string()),
          tup_label(label("a"), int()),
        ]),
        FIError.(
          Exp.(
            parens(
              tuple(
                ~ann=
                  Some(
                    FTemp.(
                      Exp(
                        Common(
                          TupleLabelError({
                            malformed_labels: [
                              Exp.(Exp(multi_hole([Exp(int(1))]))),
                            ],
                            duplicate_labels: [],
                            invalid_labels: [],
                            typ:
                              Typ.(
                                prod([
                                  tup_label(unknown(Internal), string()),
                                  tup_label(label("a"), int()),
                                ])
                              ),
                          }),
                        ),
                      )
                    ),
                  ),
                [
                  tup_label(
                    ~ann=
                      Some(
                        FTemp.(
                          Exp(
                            Common(
                              TupleLabelError({
                                malformed_labels: [
                                  Exp.(Exp(multi_hole([Exp(int(1))]))),
                                ],
                                duplicate_labels: [],
                                invalid_labels: [],
                                typ:
                                  Typ.(
                                    tup_label(unknown(Internal), string())
                                  ),
                              }),
                            ),
                          )
                        ),
                      ),
                    multi_hole(
                      ~ann=
                        FTemp.(
                          Some(
                            Exp(
                              Common(
                                NoType(
                                  BadLabel(
                                    Exp.(Exp(multi_hole([Exp(int(1))]))),
                                  ),
                                ),
                              ),
                            ),
                          )
                        ),
                      [Exp(int(1))],
                    ),
                    string("hello"),
                  ),
                  tup_label(label("a"), int(3)),
                ],
              ),
            )
          )
        ),
      )
    }),
    test_case("Extra Label", `Quick, () => {
      annotated_tree_test(
        {|let extra_label : (Int, a=String) = (c=1, a="hello") in true|},
        bool(),
        FIError.(
          Exp.(
            let_(
              Pat.(
                asc(
                  var("extra_label"),
                  Typ.(
                    parens(prod([int(), tup_label(label("a"), string())]))
                  ),
                )
              ),
              parens(
                ~ann=
                  Some(
                    Exp(
                      Common(
                        Inconsistent(
                          FTemp.Typ.(
                            Expectation({
                              ana:
                                parens(
                                  prod([
                                    int(),
                                    tup_label(label("a"), string()),
                                  ]),
                                ),
                              syn:
                                prod([
                                  tup_label(label("c"), int()),
                                  tup_label(label("a"), string()),
                                ]),
                            })
                          ),
                        ),
                      ),
                    ),
                  ),
                tuple(
                  ~ann=
                    Some(
                      Exp(
                        Common(
                          TupleLabelError({
                            malformed_labels: [],
                            duplicate_labels: [],
                            invalid_labels: ["c"],
                            typ:
                              FTemp.Typ.(
                                prod([
                                  tup_label(label("c"), int()),
                                  tup_label(label("a"), string()),
                                ])
                              ),
                          }),
                        ),
                      ),
                    ),
                  [
                    {
                      tup_label(
                        ~ann=
                          Some(
                            Exp(
                              Common(
                                TupleLabelError({
                                  malformed_labels: [],
                                  duplicate_labels: [],
                                  invalid_labels: ["c"],
                                  typ:
                                    FTemp.Typ.(tup_label(label("c"), int())),
                                }),
                              ),
                            ),
                          ),
                        label(
                          ~ann=
                            Some(
                              Exp(
                                Common(NoType(InvalidLabel("c", ["a"]))),
                              ),
                            ),
                          "c",
                        ),
                        int(1),
                      );
                    },
                    tup_label(label("a"), string("hello")),
                  ],
                ),
              ),
              bool(true),
            )
          )
        ),
      )
    }),
    test_case("tuple ascribed to non-tuple", `Quick, () => {
      annotated_tree_test(
        {|(a=1, b=2) : Int|},
        int(),
        FIError.(
          Exp.(
            asc(
              parens(
                tuple(
                  ~ann=
                    Some(
                      Exp(
                        Common(
                          Inconsistent(
                            FTemp.Typ.(
                              Expectation({
                                ana: int(),
                                syn:
                                  prod([
                                    tup_label(label("a"), int()),
                                    tup_label(label("b"), int()),
                                  ]),
                              })
                            ),
                          ),
                        ),
                      ),
                    ),
                  [
                    tup_label(label("a"), int(1)),
                    tup_label(label("b"), int(2)),
                  ],
                ),
              ),
              Typ.int(),
            )
          )
        ),
      )
    }),
    fully_consistent_typecheck(
      "Projection from list of labeled tuples",
      {|[(a=1, b=false), (a=2, b=true)].a|},
      Some(list(int())),
    ),
    fully_consistent_typecheck(
      "Projection of labeled tuple with annotation",
      {|((a=1) : (a=Int)).a|},
      Some(int()),
    ),
    fully_consistent_typecheck(
      "Projection of labeled tuple list with annotation",
      {|([(a=1)] : [(a=Int)]).a|},
      Some(list(int())),
    ),
    test_case("Label not in tuple", `Quick, () => {
      annotated_tree_test(
        {|'a'|},
        unknown(Internal),
        FIError.(
          Exp.(
            label(
              ~ann=Some(Exp(Common(NoType(UnexpectedLabelSort("a"))))),
              "a",
            )
          )
        ),
      )
    }),
    fully_consistent_typecheck(
      "Projection of unknown",
      {|((a=1) : ?) .a|},
      Some(unknown(Internal)),
    ),
    fully_consistent_typecheck(
      "Projection of list of unknown",
      {|([(a=1) : ?]).a|},
      Some(list(unknown(Internal))),
    ),
  ]
  @ TupleExtension.tests
  @ ProductProjection.tests
  @ ExplicitlyUnlabeledTuples.tests,
);
