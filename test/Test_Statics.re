open Alcotest;
open Haz3lcore;

let testable_typ = testable(Fmt.using(Typ.show, Fmt.string), Typ.fast_equal);

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

let statics = Statics.mk(CoreSettings.on, Builtins.ctx_init);

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
let no_error_exp =
    (e: Grammar.exp_term(option(Info.error)))
    : Grammar.exp_t(option(Info.error)) => {
  {term: e, annotation: None};
};
let no_error_pat =
    (p: Grammar.pat_term(option(Info.error)))
    : Grammar.pat_t(option(Info.error)) => {
  {term: p, annotation: None};
};
let no_error_typ =
    (t: Grammar.typ_term(option(Info.error)))
    : Grammar.typ_t(option(Info.error)) => {
  {term: t, annotation: None};
};
let error_exp =
    (err, e: Grammar.exp_term(option(Info.error)))
    : Grammar.exp_t(option(Info.error)) => {
  {term: e, annotation: Some(err)};
};
let fresh = (exp: Grammar.exp_t(unit)): TermBase.exp_t => {
  Grammar.map_exp_annotation(
    (_annotation): IdTagged.IdTag.t => {
      let id = Id.mk();
      {ids: [id], copied: false};
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

      let errors = List.map(snd, Statics.collect_errors(s));

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
      let errors = List.map(snd, Statics.collect_errors(s));
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
    let default_value = (): IdTagged.IdTag.t => {
      ids: [Id.invalid],
      copied: false,
    };
  });
let tests = (
  "Statics",
  FTemp.[
    fully_consistent_typecheck(
      "Function with unknown param",
      "fun x -> 4 + 5",
      Some(ty_arrow(ty_unknown(Internal), ty_int())),
    ),
    fully_consistent_typecheck(
      "Function with known param",
      "fun x : Int -> 4 + 5",
      Some(ty_arrow(ty_int(), ty_int())),
    ),
    fully_consistent_typecheck(
      "Function with labeled param",
      "fun (a=x) -> 4",
      Some(
        ty_arrow(
          ty_prod([ty_tup_label(ty_label("a"), ty_unknown(Internal))]),
          ty_int(),
        ),
      ),
    ),
    fully_consistent_typecheck(
      "bifunction",
      "fun x : Int, y: Int -> x + y",
      Some(ty_arrow(ty_prod([ty_int(), ty_int()]), ty_int())),
    ),
    fully_consistent_typecheck(
      "bifunction",
      "fun x : Int, y: Int -> x + y",
      Some(ty_arrow(ty_prod([ty_int(), ty_int()]), ty_int())),
    ),
    fully_consistent_typecheck(
      "function application",
      "float_of_int(1)",
      Some(ty_float()),
    ),
    fully_consistent_typecheck(
      "function deferral",
      "string_sub(\"hello\", 1, _)",
      Some(ty_arrow(ty_int(), ty_string())),
    ),
    test_case(
      "Typechecking fails for unlabeled variable being assigned to labeled tuple",
      `Quick,
      () => {
      annotated_tree_test(
        "let x = (1, 2) in let y : (a=Int, b=Int) = x in y",
        no_error_exp(
          Let(
            no_error_pat(Var("x")),
            no_error_exp(
              Parens(
                Tuple([Int(1) |> no_error_exp, Int(2) |> no_error_exp])
                |> no_error_exp,
              ),
            ),
            no_error_exp(
              Let(
                no_error_pat(
                  Cast(
                    no_error_pat(Var("y")),
                    no_error_typ(
                      Parens(
                        Prod([
                          no_error_typ(
                            TupLabel(
                              Label("a") |> no_error_typ,
                              Int |> no_error_typ,
                            ),
                          ),
                          TupLabel(
                            Label("b") |> no_error_typ,
                            Int |> no_error_typ,
                          )
                          |> no_error_typ,
                        ])
                        |> no_error_typ,
                      ),
                    ),
                    Unknown(Internal) |> no_error_typ,
                  ),
                ),
                error_exp(
                  Exp(
                    Common(
                      Inconsistent(
                        Expectation({
                          ana:
                            ty_parens(
                              ty_prod([
                                ty_tup_label(ty_label("a"), ty_int()),
                                ty_tup_label(ty_label("b"), ty_int()),
                              ]),
                            ),
                          syn: ty_prod([ty_int(), ty_int()]),
                        }),
                      ),
                    ),
                  ),
                  Var("x"),
                ),
                no_error_exp(Var("y")),
              ),
            ),
          ),
        ),
      )
    }),
    test_case(
      "Typechecking fails for unlabeled variable being assigned to labeled tuple",
      `Quick,
      () => {
      annotated_tree_test(
        "let y : String = true",
        no_error_exp(
          Let(
            Cast(
              Var("y") |> no_error_pat,
              String |> no_error_typ,
              Unknown(Internal) |> no_error_typ,
            )
            |> no_error_pat,
            error_exp(
              FTemp.(
                Exp(
                  Common(
                    Inconsistent(
                      Expectation({ana: ty_string(), syn: ty_bool()}),
                    ),
                  ),
                )
              ),
              Bool(true),
            ),
            Var("y") |> no_error_exp,
          ),
        ),
      )
    }),
    fully_consistent_typecheck(
      "Assigning labeled tuple to variable",
      "let x = (l=32) in let y : (l=Int) = x in y",
      Some(ty_prod([ty_tup_label(ty_label("l"), ty_int())])),
    ),
    fully_consistent_typecheck(
      "Singleton Labled Tuple ascription in let",
      "let x : (l=String) = (\"a\") in x",
      Some(ty_prod([ty_tup_label(ty_label("l"), ty_string())])),
    ),
    test_case(
      "Singleton Labled Tuple ascription in let with wrong type should fail",
      `Quick,
      () => {
      annotated_tree_test(
        "",
        Let(
          Cast(
            Var("x") |> no_error_pat,
            Parens(
              Prod([
                TupLabel(Label("l") |> no_error_typ, String |> no_error_typ)
                |> no_error_typ,
              ])
              |> no_error_typ,
            )
            |> no_error_typ,
            Unknown(Internal) |> no_error_typ,
          )
          |> no_error_pat,
          error_exp(
            FTemp.(
              Exp(
                Common(
                  Inconsistent(
                    Expectation({ana: ty_string(), syn: ty_int()}),
                  ),
                ),
              )
            ),
            Int(1),
          ),
          Var("x") |> no_error_exp,
        )
        |> no_error_exp,
      )
    }),
    fully_consistent_typecheck(
      "Singleton Labled Tuple with specified label",
      "let x : (l=String) = (l=\"a\") in x",
      Some(ty_prod([ty_tup_label(ty_label("l"), ty_string())])),
    ),
    fully_consistent_typecheck(
      "Labeled tuple with multiple labels",
      {|(l=32, l2="")|},
      Some(
        ty_prod([
          ty_tup_label(ty_label("l"), ty_int()),
          ty_tup_label(ty_label("l2"), ty_string()),
        ]),
      ),
    ),
    fully_consistent_typecheck(
      "Let statement that adds labels during elaboration",
      {|let x : (name=String, age=Int)= ("Bob", 20) in x|},
      Some(
        ty_prod([
          ty_tup_label(ty_label("name"), ty_string()),
          ty_tup_label(ty_label("age"), ty_int()),
        ]),
      ),
    ),
    fully_consistent_typecheck(
      "Duplicate singleton labels",
      {|let y : (l=(l=Int)) = (l=1) in y|},
      Some(
        ty_prod([
          ty_tup_label(
            ty_label("l"),
            ty_parens(ty_prod([ty_tup_label(ty_label("l"), ty_int())])),
          ),
        ]),
      ),
    ),
    fully_consistent_typecheck(
      "Reconstructed labeled tuple without values",
      {|let x : (l=|},
      Some(ty_unknown(Internal)),
    ),
    fully_consistent_typecheck(
      "Singleton labeled argument let with unknown type",
      {|let x : (a=?) = (a=1) in x|},
      Some(
        ty_prod([
          ty_tup_label(ty_label("a"), ty_unknown(Hole(EmptyHole))),
        ]),
      ),
    ),
    fully_consistent_typecheck(
      "nested different singleton labeled arguments",
      {|let x : (b=c=String) = b="" in x|},
      Some(
        ty_prod([
          ty_tup_label(
            ty_label("b"),
            ty_prod([ty_tup_label(ty_label("c"), ty_string())]),
          ),
        ]),
      ),
    ),
    fully_consistent_typecheck(
      "nested different singleton labeled arguments",
      {|let x : (a=b=c=?) = b=? in x|},
      Some(
        ty_prod([
          ty_tup_label(
            ty_label("a"),
            ty_prod([
              ty_tup_label(
                ty_label("b"),
                ty_prod([
                  ty_tup_label(ty_label("c"), ty_unknown(Hole(EmptyHole))),
                ]),
              ),
            ]),
          ),
        ]),
      ),
    ),
    fully_consistent_typecheck(
      "Singleton labeled argument function application with unknown type",
      {|(fun a=x->x)(a=1)|},
      Some(ty_unknown(Internal)),
    ),
    fully_consistent_typecheck(
      "Singleton labeled argument function application with no labeled param",
      {|(fun a=x->x)(1)|},
      Some(ty_unknown(Internal)),
    ),
    fully_consistent_typecheck(
      "Singleton labeled argument not labeled in pattern",
      {|let x : (a=Int) -> Int = fun a -> a in x(2)|},
      Some(ty_int()),
    ),
    test_case("Unknown label in last position", `Quick, () => {
      annotated_tree_test(
        {|(1, 1.2, z="hello") : (a=Int, b=Float, String)|},
        no_error_exp(
          Cast(
            error_exp(
              FTemp.(
                Exp(
                  Common(
                    Inconsistent(
                      Expectation({
                        ana:
                          ty_prod([
                            ty_tup_label(ty_label("a"), ty_int()),
                            ty_tup_label(ty_label("b"), ty_float()),
                            ty_string(),
                          ]),
                        syn:
                          ty_prod([
                            ty_tup_label(ty_label("a"), ty_int()),
                            ty_tup_label(ty_label("b"), ty_float()),
                            ty_tup_label(ty_label("z"), ty_string()),
                          ]),
                      }),
                    ),
                  ),
                )
              ),
              Parens(
                error_exp(
                  Exp(
                    Common(
                      TupleLabelError({
                        malformed_labels: [],
                        duplicate_labels: [],
                        invalid_labels: ["z"],
                        typ:
                          ty_prod([
                            ty_tup_label(ty_label("a"), ty_int()),
                            ty_tup_label(ty_label("b"), ty_float()),
                            ty_tup_label(ty_label("z"), ty_string()),
                          ]),
                      }),
                    ),
                  ),
                  Tuple([
                    no_error_exp(Int(1)),
                    no_error_exp(Float(1.2)),
                    error_exp(
                      FTemp.(
                        Exp(
                          Common(
                            TupleLabelError({
                              malformed_labels: [],
                              duplicate_labels: [],
                              invalid_labels: ["z"],
                              typ: ty_tup_label(ty_label("z"), ty_string()),
                            }),
                          ),
                        )
                      ),
                      TupLabel(
                        error_exp(
                          Exp(Common(NoType(InvalidLabel("z")))),
                          Label("z"),
                        ),
                        no_error_exp(String("hello")),
                      ),
                    ),
                  ]),
                ),
              ),
            ),
            no_error_typ(Unknown(Internal)),
            no_error_typ(
              Parens(
                no_error_typ(
                  Prod([
                    TupLabel(no_error_typ(Label("a")), no_error_typ(Int))
                    |> no_error_typ,
                    TupLabel(no_error_typ(Label("b")), no_error_typ(Float))
                    |> no_error_typ,
                    no_error_typ(String),
                  ]),
                ),
              ),
            ),
          ),
        ),
      )
    }),
    test_case("Duplicate label synthesis", `Quick, () => {
      annotated_tree_test(
        {|(a="hello", a=3)|},
        no_error_exp(
          Parens(
            error_exp(
              FTemp.(
                Exp(
                  Common(
                    TupleLabelError({
                      malformed_labels: [],
                      duplicate_labels: ["a", "a"],
                      invalid_labels: [],
                      typ:
                        ty_prod([
                          ty_tup_label(ty_label("a"), ty_unknown(Internal)),
                        ]),
                    }),
                  ),
                )
              ),
              Tuple([
                error_exp(
                  FTemp.(
                    Exp(
                      Common(
                        TupleLabelError({
                          malformed_labels: [],
                          duplicate_labels: ["a"],
                          invalid_labels: [],
                          typ: ty_tup_label(ty_label("a"), ty_string()),
                        }),
                      ),
                    )
                  ),
                  TupLabel(
                    error_exp(
                      FTemp.(
                        Exp(Common(DuplicateLabel("a", ty_label("a"))))
                      ),
                      Label("a"),
                    ),
                    no_error_exp(String("hello")),
                  ),
                ),
                error_exp(
                  FTemp.(
                    Exp(
                      Common(
                        TupleLabelError({
                          malformed_labels: [],
                          duplicate_labels: ["a"],
                          invalid_labels: [],
                          typ: ty_tup_label(ty_label("a"), ty_int()),
                        }),
                      ),
                    )
                  ),
                  TupLabel(
                    FTemp.(
                      error_exp(
                        Exp(Common(DuplicateLabel("a", ty_label("a")))),
                        Label("a"),
                      )
                    ),
                    no_error_exp(Int(3)),
                  ),
                ),
              ]),
            ),
          ),
        ),
      )
    }),
    test_case("Bad label projection", `Quick, () => {
      annotated_tree_test(
        {|(1, 2) . 1|},
        error_exp(
          Exp(
            Common(
              NoType(
                BadLabel(
                  Exp(MultiHole([Exp(Int(1) |> Exp.fresh)]) |> Exp.fresh),
                ),
              ),
            ),
          ),
          Dot(
            Tuple([no_error_exp(Int(1)), no_error_exp(Int(2))])
            |> no_error_exp,
            no_error_exp(MultiHole([Exp(no_error_exp(Int(1)))])),
          ),
        ),
      )
    }),
    test_case("Singleton Bad label synthesis", `Quick, () => {
      annotated_tree_test(
        {|(1="hello")|},
        no_error_exp(
          Parens(
            error_exp(
              FTemp.(
                Exp(
                  Common(
                    TupleLabelError({
                      malformed_labels: [
                        Exp(multi_hole([Exp(label("1"))])),
                      ],
                      duplicate_labels: [],
                      invalid_labels: [],
                      typ:
                        ty_prod([
                          ty_tup_label(ty_unknown(Internal), ty_string()),
                        ]),
                    }),
                  ),
                )
              ),
              Tuple([
                error_exp(
                  FTemp.(
                    Exp(
                      Common(
                        TupleLabelError({
                          malformed_labels: [
                            Exp(multi_hole([Exp(label("1"))])),
                          ],
                          duplicate_labels: [],
                          invalid_labels: [],
                          typ:
                            ty_tup_label(ty_unknown(Internal), ty_string()),
                        }),
                      ),
                    )
                  ),
                  TupLabel(
                    error_exp(
                      Exp(
                        Common(
                          NoType(
                            BadLabel(Exp(multi_hole([Exp(label("1"))]))),
                          ),
                        ),
                      ),
                      MultiHole([Exp(no_error_exp(Label("1")))]),
                    ),
                    no_error_exp(String("hello")),
                  ),
                ),
              ]),
            ),
          ),
        ),
      )
    }),
    test_case("Bad label synthesis", `Quick, () => {
      annotated_tree_test(
        {|(1="hello", a=3)|},
        no_error_exp(
          Parens(
            error_exp(
              Exp(
                Common(
                  TupleLabelError({
                    malformed_labels: [
                      Exp(
                        MultiHole([Exp(Int(1) |> Exp.fresh)]) |> Exp.fresh,
                      ),
                    ],
                    duplicate_labels: [],
                    invalid_labels: [],
                    typ:
                      Prod([
                        TupLabel(
                          Unknown(Internal) |> Typ.temp,
                          String |> Typ.temp,
                        )
                        |> Typ.temp,
                        TupLabel(Label("a") |> Typ.temp, Int |> Typ.temp)
                        |> Typ.temp,
                      ])
                      |> Typ.temp,
                  }),
                ),
              ),
              Tuple([
                error_exp(
                  Exp(
                    Common(
                      TupleLabelError({
                        malformed_labels: [
                          Exp(
                            MultiHole([Exp(Int(1) |> Exp.fresh)])
                            |> Exp.fresh,
                          ),
                        ],
                        duplicate_labels: [],
                        invalid_labels: [],
                        typ:
                          TupLabel(
                            Unknown(Internal) |> Typ.temp,
                            String |> Typ.temp,
                          )
                          |> Typ.temp,
                      }),
                    ),
                  ),
                  TupLabel(
                    error_exp(
                      Exp(
                        Common(
                          NoType(
                            BadLabel(
                              Exp(
                                MultiHole([Exp(Int(1) |> Exp.fresh)])
                                |> Exp.fresh,
                              ),
                            ),
                          ),
                        ),
                      ),
                      MultiHole([Exp(no_error_exp(Int(1)))]),
                    ),
                    no_error_exp(String("hello")),
                  ),
                ),
                no_error_exp(
                  TupLabel(
                    no_error_exp(Label("a")),
                    no_error_exp(Int(3)),
                  ),
                ),
              ]),
            ),
          ),
        ),
      )
    }),
    test_case("Extra Label", `Quick, () => {
      annotated_tree_test(
        {|let extra_label : (Int, a=String) = (c=1, a="hello") in true|},
        FIError.(
          let_(
            pat_cast(
              pat_var("extra_label"),
              ty_parens(
                ty_prod([
                  ty_int(),
                  ty_tup_label(ty_label("a"), ty_string()),
                ]),
              ),
              no_error_typ(Unknown(Internal)),
            ),
            parens(
              ~ann=
                Some(
                  Exp(
                    Common(
                      Inconsistent(
                        FTemp.(
                          Expectation({
                            ana:
                              ty_parens(
                                ty_prod([
                                  ty_int(),
                                  ty_tup_label(ty_label("a"), ty_string()),
                                ]),
                              ),
                            syn:
                              ty_prod([
                                ty_tup_label(ty_label("c"), ty_int()),
                                ty_tup_label(ty_label("a"), ty_string()),
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
                            FTemp.(
                              ty_prod([
                                ty_tup_label(ty_label("c"), ty_int()),
                                ty_tup_label(ty_label("a"), ty_string()),
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
                                  FTemp.(
                                    ty_tup_label(ty_label("c"), ty_int())
                                  ),
                              }),
                            ),
                          ),
                        ),
                      error_exp(
                        Exp(Common(NoType(InvalidLabel("c")))),
                        Label("c"),
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
        ),
      )
    }),
    test_case("tuple with cast to non-tuple", `Quick, () => {
      annotated_tree_test(
        {|(a=1, b=2) : Int|},
        FIError.(
          cast(
            parens(
              tuple(
                ~ann=
                  Some(
                    Exp(
                      Common(
                        Inconsistent(
                          FTemp.(
                            Expectation({
                              ana: ty_int(),
                              syn:
                                ty_prod([
                                  ty_tup_label(ty_label("a"), ty_int()),
                                  ty_tup_label(ty_label("b"), ty_int()),
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
            ty_unknown(Internal),
            ty_int(),
          )
        ),
      )
    }),
    test_case("Example error annotations", `Quick, () => {
      annotated_tree_test(
        "Inconsistent expectation on plus",
        FIError.(
          bin_op(
            Int(Plus),
            int(1),
            string(
              ~ann=
                Some(
                  FTemp.(
                    Exp(
                      Common(
                        Inconsistent(
                          Expectation({ana: ty_int(), syn: ty_string()}),
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
      FTemp.(Some(ty_forall(tpat_var("b"), ty_var("b")))),
    ),
    fully_consistent_typecheck(
      "Forall alpha equivalent in let",
      {|let x : forall a -> a = in let y : forall b -> b = x in 1|},
      Some(ty_int()),
    ),
  ],
);
