open Alcotest;
open Haz3lcore;

let testable_typ =
  testable(Fmt.using(t => t |> Typ.show, Fmt.string), Typ.fast_equal);

let eq_info_error_exp = (a: Info.error_exp, b: Info.error_exp) => {
  switch (a, b) {
  | (Common(DuplicateLabel(l, ty)), Common(DuplicateLabel(r, ty2))) =>
    l == r && Typ.fast_equal(ty, ty2)
  | (Common(NoType(BadLabel(a))), Common(NoType(BadLabel(b)))) =>
    Any.fast_equal(a, b)
  | (Common(NoType(InvalidLabel(a))), Common(NoType(InvalidLabel(b)))) =>
    a == b
  | (
      Common(Inconsistent(Expectation({ana: a1, syn: a2}))),
      Common(Inconsistent(Expectation({ana: b1, syn: b2}))),
    ) =>
    Typ.fast_equal(a1, b1) && Typ.fast_equal(a2, b2)
  | (Common(TupleLabelError(err)), Common(TupleLabelError(err'))) =>
    List.equal(Any.fast_equal, err.malformed_labels, err'.malformed_labels)
    && List.equal(String.equal, err.duplicate_labels, err'.duplicate_labels)
    && List.equal(String.equal, err.invalid_labels, err'.invalid_labels)
    && Typ.fast_equal(err.typ, err'.typ)
  | _ =>
    Alcotest.fail(
      "Not implemented for "
      ++ Info.show_error_exp(a)
      ++ " and "
      ++ Info.show_error_exp(b),
    )
  };
};

let eq_info_error = (a: Info.error, b: Info.error) => {
  switch (a, b) {
  | (Exp(a), Exp(b)) => eq_info_error_exp(a, b)
  | _ =>
    Alcotest.fail(
      "Not implemented for "
      ++ Info.show_error(a)
      ++ " and "
      ++ Info.show_error(b),
    )
  };
};
let testable_info_error_exp =
  testable(Fmt.using(Info.show_error_exp, Fmt.string), eq_info_error_exp);

let testable_error: testable(Info.error) =
  testable(Fmt.using(Info.show_error, Fmt.string), (==));

let statics = Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)));

let parse_exp = (s: string) => {
  switch (MakeTerm.parse_exp(s)) {
  | Some(e) => e
  | None => Alcotest.fail("Failed to parse expression: " ++ s)
  };
};

let annotate_static_errors = (exp: TermBase.exp_t, info_map: Statics.Map.t) => {
  Grammar.map_exp_annotation(
    ({ids, _}: IdTagged.IdTag.t) => {
      let new_info = Id.Map.find_opt(List.hd(ids), info_map);
      Option.bind(new_info, Info.error_of);
    },
    exp,
  );
};

let annotated_exp: testable(Grammar.exp_t(option(Info.error))) =
  testable(
    Fmt.using(
      [%derive.show: Grammar.exp_t(option(Info.error))],
      Fmt.string,
    ),
    Grammar.equal_exp_t(Option.equal(eq_info_error)),
  );

let fresh = (exp: Grammar.exp_t(unit)): TermBase.exp_t => {
  Grammar.map_exp_annotation(
    (_annotation): IdTagged.IdTag.t => {
      let id = Id.mk();
      {ids: [id]};
    },
    exp,
  );
};

let annotated_tree_test = (name, expected) => {
  let term = fresh(Grammar.map_exp_annotation(_ => (), expected));
  let annotated: Grammar.exp_t(option(Info.error)) =
    annotate_static_errors(term, statics(term));

  Alcotest.check(annotated_exp, name, expected, annotated);
};

// Get the type from the statics
let type_of = f => {
  IdTagged.rep_id(f)
  |> Id.Map.find_opt(_, statics(f))
  |> Option.bind(
       _,
       fun
       | InfoExp(e) => Some(e.ty)
       | _ => None,
     );
};

let inconsistent_typecheck = (name, exp) => {
  test_case(
    name,
    `Quick,
    () => {
      let s = statics(exp);

      let errors = List.map(snd, Statics.Map.errors(s));

      Alcotest.check(
        neg(list(testable_error)),
        "Missing Static Errors",
        [],
        errors,
      );
    },
  );
};
let fully_consistent_typecheck = (name, serialized, expected) => {
  test_case(
    name,
    `Quick,
    () => {
      let exp = parse_exp(serialized);
      let s = statics(exp);
      let errors = List.map(snd, Statics.Map.errors(s));
      Alcotest.check(list(testable_error), "Static Errors", [], errors);
      Alcotest.check(
        Alcotest.option(testable_typ),
        serialized,
        expected,
        type_of(exp),
      );
    },
  );
};
module FIError =
  Grammar.Factory({
    type t = option(Info.error);
    let default_value = () => None;
  });
module FTemp =
  Grammar.Factory({
    type t = IdTagged.IdTag.t;
    let default_value = (): IdTagged.IdTag.t => {ids: [Id.invalid]};
  });

let qcheck_statics_does_not_crash =
  QCheck.Test.make(
    ~name="Statics does not crash",
    ~count=10000,
    QCheck_Util.arb_exp(~minimal_idents=true, 50),
    exp => {
    switch (statics(exp)) {
    | _m => true
    | exception Stack_overflow => true // TODO https://github.com/hazelgrove/hazel/issues/1622
    | exception (Failure(f) as e) =>
      switch (f) {
      | "Type join of ap" => true // TODO https://github.com/hazelgrove/hazel/issues/1459
      | "normalize exceeded 1000 recursive calls" => true // TODO https://github.com/hazelgrove/hazel/issues/1622?issue=hazelgrove%7Chazel%7C1623
      | "weak_head_normalize exceeded 1000 recursive calls" => true // TODO https://github.com/hazelgrove/hazel/issues/1621
      | "Recursion limit exceeded in all_ctrs_of_typ" => true // TODO https://github.com/hazelgrove/hazel/issues/1624
      | _
          when
            String.starts_with(
              ~prefix="all_ctrs_of_type called with a non-normalized type:",
              f,
            ) =>
        true // https://github.com/hazelgrove/hazel/issues/1626
      | _ => raise(e)
      }
    }
  });

let skip_known_bug = (message: string, expression: string) =>
  test_case("Known Bug: " ++ message, `Quick, () => {
    [@warning "-21"]
    {
      let uexp = parse_exp(expression);
      Alcotest.skip();
      let _ = statics(uexp);
      ();
    }
  });

let tests = (
  "Statics",
  FTemp.(
    Typ.[
      fully_consistent_typecheck(
        "Function with unknown param",
        "fun x -> 4 + 5",
        Some(arrow(unknown(Internal), int())),
      ),
      fully_consistent_typecheck(
        "Function with known param",
        "fun x : Int -> 4 + 5",
        Some(arrow(int(), int())),
      ),
      fully_consistent_typecheck(
        "Function with labeled param",
        "fun (a=x) -> 4",
        Some(
          arrow(prod([tup_label(label("a"), unknown(Internal))]), int()),
        ),
      ),
      fully_consistent_typecheck(
        "bifunction",
        "fun x : Int, y: Int -> x + y",
        Some(arrow(prod([int(), int()]), int())),
      ),
      fully_consistent_typecheck(
        "bifunction",
        "fun x : Int, y: Int -> x + y",
        Some(arrow(prod([int(), int()]), int())),
      ),
      fully_consistent_typecheck(
        "function application",
        "float_of_int(1)",
        Some(float()),
      ),
      fully_consistent_typecheck(
        "function deferral",
        "string_sub(\"hello\", 1, _)",
        Some(arrow(int(), string())),
      ),
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
                    cast(
                      var("y"),
                      Typ.(
                        parens(
                          prod([
                            tup_label(label("a"), int()),
                            tup_label(label("b"), int()),
                          ]),
                        )
                      ),
                      Typ.unknown(Internal),
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
                Pat.(
                  cast(var("y"), Typ.(string()), Typ.(unknown(Internal)))
                ),
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
        "Singleton Labled Tuple ascription in let",
        "let x : (l=String) = (\"a\") in x",
        Some(prod([tup_label(label("l"), string())])),
      ),
      test_case(
        "Singleton Labled Tuple ascription in let with wrong type should fail",
        `Quick,
        () => {
        annotated_tree_test(
          "",
          FIError.(
            Exp.(
              let_(
                Pat.(
                  cast(
                    var("x"),
                    Typ.(parens(prod([tup_label(label("l"), string())]))),
                    Typ.unknown(Internal),
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
            tup_label(
              label("b"),
              prod([tup_label(label("c"), string())]),
            ),
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
              cast(
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
                            Some(Exp(Common(NoType(InvalidLabel("z"))))),
                          "z",
                        ),
                        string("hello"),
                      ),
                    ],
                  ),
                ),
                Typ.unknown(Internal),
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
                                    Exp.(
                                      Exp(multi_hole([Exp(label("1"))]))
                                    ),
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
          FIError.(
            Exp.(
              let_(
                Pat.(
                  cast(
                    var("extra_label"),
                    Typ.(
                      parens(
                        prod([int(), tup_label(label("a"), string())]),
                      )
                    ),
                    Typ.unknown(Internal),
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
                                      FTemp.Typ.(
                                        tup_label(label("c"), int())
                                      ),
                                  }),
                                ),
                              ),
                            ),
                          label(
                            ~ann=
                              Some(
                                Exp(Common(NoType(InvalidLabel("c")))),
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
      test_case("tuple with cast to non-tuple", `Quick, () => {
        annotated_tree_test(
          {|(a=1, b=2) : Int|},
          FIError.(
            Exp.(
              cast(
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
                Typ.unknown(Internal),
                Typ.int(),
              )
            )
          ),
        )
      }),
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
        "Forall alpha equivalent in cast",
        {|let x : forall a -> a = in (x : forall b -> b)|},
        FTemp.Typ.(Some(forall(TPat.var("b"), var("b")))),
      ),
      fully_consistent_typecheck(
        "Forall alpha equivalent in let",
        {|let x : forall a -> a = in let y : forall b -> b = x in 1|},
        Some(int()),
      ),
      fully_consistent_typecheck(
        "Fixpoint in function position",
        {|(fix f : (Int -> Int) -> fun x -> x + 1)(3)|},
        Some(int()),
      ),
      fully_consistent_typecheck(
        "nested_sum_constructors",
        {|
case (? : (rec t -> +Z+S(t)))
  | S(S(x)) => 1
  | _ => 2
end
        |},
        Some(int()),
      ),
      skip_known_bug(
        "Typ.weak_head_normalize infinite recursion", // https://github.com/hazelgrove/hazel/issues/1621
        "type y = y in type ? = y in ?",
      ),
      skip_known_bug(
        "Coverage.all_ctrs_of_typ infinite recursion", // https://github.com/hazelgrove/hazel/issues/1624
        "fun ((()): ((rec x -> (rec y -> x)))) -> []",
      ),
      skip_known_bug(
        "all_ctrs_of_type called with a non-normalized type", // https://github.com/hazelgrove/hazel/issues/1626
        {|fun (?: (Float((+ A(Bool))))) -> ""|},
      ),
      skip_known_bug(
        "Type join of ap", // https://github.com/hazelgrove/hazel/issues/1459
        "type x = Int(Float) in let y : x =  1",
      ),
      QCheck_alcotest.to_alcotest(qcheck_statics_does_not_crash),
    ]
  ),
);
