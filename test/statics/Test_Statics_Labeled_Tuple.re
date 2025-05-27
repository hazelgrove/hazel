open Alcotest;
open Test_Statics_Prelude;
open FTemp;
open Typ;

let tests = [
  test_case(
    "Typechecking fails for unlabeled variable being assigned to labeled tuple",
    `Quick,
    () => {
    annotated_tree_test(
      "let x = (1, 2) in let y : (a=Int, b=Int) = x in y",
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
  test_case(
    "Typechecking fails for unlabeled variable being assigned to labeled tuple",
    `Quick,
    () => {
    annotated_tree_test(
      "let y : String = true",
      FIError.(
        Exp.(
          let_(
            Pat.(asc(var("y"), Typ.(string()))),
            bool(
              ~ann=
                Some(
                  FTemp.Typ.(
                    Exp(
                      Common(
                        Inconsistent(
                          Expectation({
                            ana: string(),
                            syn: bool(),
                          }),
                        ),
                      ),
                    )
                  ),
                ),
              true,
            ),
            var("y"),
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
      "",
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
    "Singleton Labled Tuple with specified label",
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
                      ~ann=Some(Exp(Common(NoType(InvalidLabel("z"))))),
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
                              prod([tup_label(unknown(Internal), string())])
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
                                Typ.(tup_label(unknown(Internal), string())),
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
                    [Exp(label("1"))],
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
                                Typ.(tup_label(unknown(Internal), string())),
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
                        ~ann=Some(Exp(Common(NoType(InvalidLabel("c"))))),
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
  test_case("tuple with cast to non-tuple", `Quick, () => {
    annotated_tree_test(
      {|(a=1, b=2) : Int|},
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
];
