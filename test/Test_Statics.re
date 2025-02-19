open Alcotest;
open Haz3lcore;

let testable_typ = testable(Fmt.using(Typ.show, Fmt.string), Typ.fast_equal);

let testable_info_error_exp =
  testable(Fmt.using(Info.show_error_exp, Fmt.string), Info.equal_error_exp);

let testable_list_uuidm = testable(Fmt.list(Uuidm.pp), (==));

let status_exp: testable(Info.status_exp) =
  testable(Fmt.using(Info.show_status_exp, Fmt.string), (==));
let testable_error: testable(Info.error) =
  testable(Fmt.using(Info.show_error, Fmt.string), (==));

let statics = Statics.mk(CoreSettings.on, Builtins.ctx_init);
let alco_check = Alcotest.option(testable_typ) |> Alcotest.check;

let parse_exp = (s: string) => {
  switch (MakeTerm.parse_exp(s)) {
  | Some(e) => e
  | None => Alcotest.fail("Failed to parse expression: " ++ s)
  };
};

let info_error_of_id = (f: Exp.t, id: Id.t) => {
  Statics.get_error_at(statics(f), id);
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
      let errors = List.map(snd, Id.Map.to_list(Statics.collect_errors(s)));
      Alcotest.check(
        neg(list(testable_error)),
        "Missing Static Errors",
        [],
        errors,
      );
    },
  );
};
let fully_consistent_typecheck = (name, serialized, expected, exp) => {
  test_case(
    name,
    `Quick,
    () => {
      let s = statics(exp);
      let errors = List.map(snd, Id.Map.to_list(Statics.collect_errors(s)));
      Alcotest.check(list(testable_error), "Static Errors", [], errors);
      alco_check(serialized, expected, type_of(exp));
    },
  );
};

let reusable_id = Id.mk();
let unlabeled_tuple_to_labeled_fails =
  test_case(
    "Typechecking fails for unlabeled variable being assigned to labeled tuple",
    `Quick,
    () =>
    Alcotest.check(
      Alcotest.option(testable_info_error_exp),
      "let x = (1, 2) in  let y : (a=Int, b=Int) = x in y",
      Some(
        Common(
          Inconsistent(
            Expectation({
              ana:
                Parens(
                  Prod([
                    TupLabel(Label("a") |> Typ.fresh, Int |> Typ.fresh)
                    |> Typ.fresh,
                    TupLabel(Label("b") |> Typ.fresh, Int |> Typ.fresh)
                    |> Typ.fresh,
                  ])
                  |> Typ.fresh,
                )
                |> Typ.fresh,
              syn: Prod([Int |> Typ.fresh, Int |> Typ.fresh]) |> Typ.fresh,
            }),
          ),
        ),
      ),
      info_error_of_id(
        Let(
          Var("x") |> Pat.fresh,
          Parens(
            Tuple([Int(1) |> Exp.fresh, Int(2) |> Exp.fresh]) |> Exp.fresh,
          )
          |> Exp.fresh,
          Let(
            Cast(
              Var("y") |> Pat.fresh,
              Parens(
                Prod([
                  TupLabel(Label("a") |> Typ.fresh, Int |> Typ.fresh)
                  |> Typ.fresh,
                  TupLabel(Label("b") |> Typ.fresh, Int |> Typ.fresh)
                  |> Typ.fresh,
                ])
                |> Typ.fresh,
              )
              |> Typ.fresh,
              Unknown(Internal) |> Typ.fresh,
            )
            |> Pat.fresh,
            {ids: [reusable_id], term: Var("x"), copied: false},
            Var("y") |> Exp.fresh,
          )
          |> Exp.fresh,
        )
        |> Exp.fresh,
        reusable_id,
      ),
    )
  );

let simple_inconsistency =
  test_case(
    "Typechecking fails for unlabeled variable being assigned to labeled tuple",
    `Quick,
    () =>
    Alcotest.check(
      Alcotest.option(testable_info_error_exp),
      "let y : String = true",
      Some(
        Common(
          Inconsistent(
            Expectation({ana: String |> Typ.fresh, syn: Bool |> Typ.fresh}),
          ),
        ),
      ),
      info_error_of_id(
        Let(
          Cast(
            Var("y") |> Pat.fresh,
            String |> Typ.fresh,
            Unknown(Internal) |> Typ.fresh,
          )
          |> Pat.fresh,
          {ids: [reusable_id], term: Bool(true), copied: false},
          Var("y") |> Exp.fresh,
        )
        |> Exp.fresh,
        reusable_id,
      ),
    )
  );

let unapplied_function = () =>
  alco_check(
    "Unknown param",
    Some(Term.Fresh.(tarrow(tunknown(Internal), tint()))),
    type_of(
      Fun(
        Var("x") |> Pat.fresh,
        BinOp(Int(Plus), Int(4) |> Exp.fresh, Int(5) |> Exp.fresh)
        |> Exp.fresh,
        None,
        None,
      )
      |> Exp.fresh,
    ),
  );

let tests = (
  "Statics",
  Typ.Fresh.[
    fully_consistent_typecheck(
      "Function with unknown param",
      "x => 4 + 5",
      Some(tarrow(tunknown(Internal), tint())),
      Fun(
        Var("x") |> Pat.fresh,
        BinOp(Int(Plus), Int(4) |> Exp.fresh, Int(5) |> Exp.fresh)
        |> Exp.fresh,
        None,
        None,
      )
      |> Exp.fresh,
    ),
    fully_consistent_typecheck(
      "Function with known param",
      "x : Int => 4 + 5",
      Some(tarrow(tint(), tint())),
      Fun(
        Cast(Var("x") |> Pat.fresh, tint(), tunknown(Internal)) |> Pat.fresh,
        BinOp(Int(Plus), Int(4) |> Exp.fresh, Int(5) |> Exp.fresh)
        |> Exp.fresh,
        None,
        None,
      )
      |> Exp.fresh,
    ),
    fully_consistent_typecheck(
      "Function with labeled param",
      "fun (a=x) -> 4",
      Some(
        tarrow(
          tprod([ttup_label(tlabel("a"), tunknown(Internal))]),
          tint(),
        ),
      ),
      Fun(
        Parens(
          Tuple([
            TupLabel(Label("a") |> Pat.fresh, Var("x") |> Pat.fresh)
            |> Pat.fresh,
          ])
          |> Pat.fresh,
        )
        |> Pat.fresh,
        Int(4) |> Exp.fresh,
        None,
        None,
      )
      |> Exp.fresh,
    ),
    fully_consistent_typecheck(
      "bifunction",
      "x : Int, y: Int => x + y",
      Some(tarrow(tprod([tint(), tint()]), tint())),
      Fun(
        Tuple([
          Cast(Var("x") |> Pat.fresh, tint(), tunknown(Internal))
          |> Pat.fresh,
          Cast(Var("y") |> Pat.fresh, tint(), tunknown(Internal))
          |> Pat.fresh,
        ])
        |> Pat.fresh,
        BinOp(Int(Plus), Var("x") |> Exp.fresh, Var("y") |> Exp.fresh)
        |> Exp.fresh,
        None,
        None,
      )
      |> Exp.fresh,
    ),
    fully_consistent_typecheck(
      "bifunction",
      "x : Int, y: Int => x + y",
      Some(tarrow(tprod([tint(), tint()]), tint())),
      Fun(
        Tuple([
          Cast(Var("x") |> Pat.fresh, tint(), tunknown(Internal))
          |> Pat.fresh,
          Cast(Var("y") |> Pat.fresh, tint(), tunknown(Internal))
          |> Pat.fresh,
        ])
        |> Pat.fresh,
        BinOp(Int(Plus), Var("x") |> Exp.fresh, Var("y") |> Exp.fresh)
        |> Exp.fresh,
        None,
        None,
      )
      |> Exp.fresh,
    ),
    fully_consistent_typecheck(
      "function application",
      "float_of_int(1)",
      Some(tfloat()),
      Ap(Forward, Var("float_of_int") |> Exp.fresh, Int(1) |> Exp.fresh)
      |> Exp.fresh,
    ),
    fully_consistent_typecheck(
      "function deferral",
      "string_sub(\"hello\", 1, _)",
      Some(tarrow(tint(), tstring())),
      DeferredAp(
        Var("string_sub") |> Exp.fresh,
        [
          String("hello") |> Exp.fresh,
          Int(1) |> Exp.fresh,
          Deferral(InAp) |> Exp.fresh,
        ],
      )
      |> Exp.fresh,
    ),
    unlabeled_tuple_to_labeled_fails,
    simple_inconsistency,
    fully_consistent_typecheck(
      "Assigning labeled tuple to variable",
      "let x = (l=32) in let y : (l=Int) = x in y",
      Some(
        Prod([
          TupLabel(Label("l") |> Typ.fresh, Int |> Typ.fresh) |> Typ.fresh,
        ])
        |> Typ.fresh,
      ),
      Let(
        Var("x") |> Pat.fresh,
        Parens(
          Tuple([
            TupLabel(Label("l") |> Exp.fresh, Int(32) |> Exp.fresh)
            |> Exp.fresh,
          ])
          |> Exp.fresh,
        )
        |> Exp.fresh,
        Let(
          Cast(
            Var("y") |> Pat.fresh,
            Parens(
              Prod([
                TupLabel(Label("l") |> Typ.fresh, Int |> Typ.fresh)
                |> Typ.fresh,
              ])
              |> Typ.fresh,
            )
            |> Typ.fresh,
            Unknown(Internal) |> Typ.fresh,
          )
          |> Pat.fresh,
          Var("x") |> Exp.fresh,
          Var("y") |> Exp.fresh,
        )
        |> Exp.fresh,
      )
      |> Exp.fresh,
    ),
    fully_consistent_typecheck(
      "Singleton Labled Tuple ascription in let",
      "let x : (l=String) = (\"a\") in x",
      Some(
        Prod([
          TupLabel(Label("l") |> Typ.fresh, String |> Typ.fresh) |> Typ.fresh,
        ])
        |> Typ.fresh,
      ),
      Let(
        Cast(
          Var("x") |> Pat.fresh,
          Parens(
            Prod([
              TupLabel(Label("l") |> Typ.fresh, String |> Typ.fresh)
              |> Typ.fresh,
            ])
            |> Typ.fresh,
          )
          |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
        )
        |> Pat.fresh,
        Parens(String("a") |> Exp.fresh) |> Exp.fresh,
        Var("x") |> Exp.fresh,
      )
      |> Exp.fresh,
    ),
    inconsistent_typecheck(
      "Singleton Labled Tuple ascription in let with wrong type should fail",
      Let(
        Cast(
          Var("x") |> Pat.fresh,
          Parens(
            Prod([
              TupLabel(Label("l") |> Typ.fresh, String |> Typ.fresh)
              |> Typ.fresh,
            ])
            |> Typ.fresh,
          )
          |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
        )
        |> Pat.fresh,
        Int(1) |> Exp.fresh,
        Var("x") |> Exp.fresh,
      )
      |> Exp.fresh,
    ),
    fully_consistent_typecheck(
      "Singleton Labled Tuple with specified label",
      "let x : (l=String) = (l=\"a\") in x",
      Some(
        Prod([
          TupLabel(Label("l") |> Typ.fresh, String |> Typ.fresh) |> Typ.fresh,
        ])
        |> Typ.fresh,
      ),
      Let(
        Cast(
          Var("x") |> Pat.fresh,
          Parens(
            Prod([
              TupLabel(Label("l") |> Typ.fresh, String |> Typ.fresh)
              |> Typ.fresh,
            ])
            |> Typ.fresh,
          )
          |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
        )
        |> Pat.fresh,
        Parens(
          Tuple([
            TupLabel(Label("l") |> Exp.fresh, String("a") |> Exp.fresh)
            |> Exp.fresh,
          ])
          |> Exp.fresh,
        )
        |> Exp.fresh,
        Var("x") |> Exp.fresh,
      )
      |> Exp.fresh,
    ),
    fully_consistent_typecheck(
      "Labeled tuple with multiple labels",
      {|(l=32, l2="")|},
      Some(
        Prod([
          TupLabel(Label("l") |> Typ.fresh, Int |> Typ.fresh) |> Typ.fresh,
          TupLabel(Label("l2") |> Typ.fresh, String |> Typ.fresh)
          |> Typ.fresh,
        ])
        |> Typ.fresh,
      ),
      Parens(
        Tuple([
          TupLabel(Label("l") |> Exp.fresh, Int(32) |> Exp.fresh)
          |> Exp.fresh,
          TupLabel(Label("l2") |> Exp.fresh, String("") |> Exp.fresh)
          |> Exp.fresh,
        ])
        |> Exp.fresh,
      )
      |> Exp.fresh,
    ),
    fully_consistent_typecheck(
      "Let statement that adds labels during elaboration",
      {|let x : (name=String, age=Int)= ("Bob", 20) |},
      Some(
        Prod([
          TupLabel(Label("name") |> Typ.fresh, String |> Typ.fresh)
          |> Typ.fresh,
          TupLabel(Label("age") |> Typ.fresh, Int |> Typ.fresh) |> Typ.fresh,
        ])
        |> Typ.fresh,
      ),
      Let(
        Cast(
          Var("x") |> Pat.fresh,
          Parens(
            Prod([
              TupLabel(Label("name") |> Typ.fresh, String |> Typ.fresh)
              |> Typ.fresh,
              TupLabel(Label("age") |> Typ.fresh, Int |> Typ.fresh)
              |> Typ.fresh,
            ])
            |> Typ.fresh,
          )
          |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
        )
        |> Pat.fresh,
        Parens(
          Tuple([String("Bob") |> Exp.fresh, Int(20) |> Exp.fresh])
          |> Exp.fresh,
        )
        |> Exp.fresh,
        Var("x") |> Exp.fresh,
      )
      |> Exp.fresh,
    ),
    fully_consistent_typecheck(
      "Duplicate singleton labels",
      {|let y : (l=(l=Int)) = (l=1) in y|},
      Some(
        Prod([
          TupLabel(
            Label("l") |> Typ.fresh,
            Parens(
              Prod([
                TupLabel(Label("l") |> Typ.fresh, Int |> Typ.fresh)
                |> Typ.fresh,
              ])
              |> Typ.fresh,
            )
            |> Typ.fresh,
          )
          |> Typ.fresh,
        ])
        |> Typ.fresh,
      ),
      parse_exp({|let y : (l=(l=Int)) = (l=1) in y|}),
    ),
    fully_consistent_typecheck(
      "Reconstructed labeled tuple without values",
      {|let x : (l=|},
      Some(Unknown(Internal) |> Typ.fresh),
      Let(
        Cast(
          Var("x") |> Pat.fresh,
          Parens(
            Prod([
              TupLabel(
                Label("l") |> Typ.fresh,
                Unknown(Hole(EmptyHole)) |> Typ.fresh,
              )
              |> Typ.fresh,
            ])
            |> Typ.fresh,
          )
          |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
        )
        |> Pat.fresh,
        EmptyHole |> Exp.fresh,
        EmptyHole |> Exp.fresh,
      )
      |> Exp.fresh,
    ),
    fully_consistent_typecheck(
      "Singleton labeled argument let with unknown type",
      {|let x : (a=?) = (a=1) in x|},
      Some(
        Prod([
          TupLabel(
            Label("a") |> Typ.fresh,
            Unknown(Hole(EmptyHole)) |> Typ.fresh,
          )
          |> Typ.fresh,
        ])
        |> Typ.fresh,
      ),
      parse_exp({|let x : (a=?) = (a=1) in x|}),
    ),
    fully_consistent_typecheck(
      "nested different singleton labeled arguments",
      {|let x : (b=c=String) = b="" in x|},
      Some(
        Prod([
          TupLabel(
            Label("b") |> Typ.fresh,
            Prod([
              TupLabel(Label("c") |> Typ.fresh, String |> Typ.fresh)
              |> Typ.fresh,
            ])
            |> Typ.fresh,
          )
          |> Typ.fresh,
        ])
        |> Typ.fresh,
      ),
      parse_exp({|let x : (b=c=String) = b="" in x|}),
    ),
    fully_consistent_typecheck(
      "nested different singleton labeled arguments",
      {|let x : (a=b=c=?) = b=? in x|},
      Some(
        Prod([
          TupLabel(
            Label("a") |> Typ.fresh,
            Prod([
              TupLabel(
                Label("b") |> Typ.fresh,
                Prod([
                  TupLabel(
                    Label("c") |> Typ.fresh,
                    Unknown(Hole(EmptyHole)) |> Typ.fresh,
                  )
                  |> Typ.fresh,
                ])
                |> Typ.fresh,
              )
              |> Typ.fresh,
            ])
            |> Typ.fresh,
          )
          |> Typ.fresh,
        ])
        |> Typ.fresh,
      ),
      parse_exp({|let x : (a=b=c=?) = b=? in x|}),
    ),
    fully_consistent_typecheck(
      "Singleton labeled argument function application with unknown type",
      {|(fun a=x->x)(a=1)|},
      Some(tunknown(Internal)),
      Ap(
        Forward,
        Fun(
          Tuple([
            TupLabel(Label("a") |> Pat.fresh, Var("x") |> Pat.fresh)
            |> Pat.fresh,
          ])
          |> Pat.fresh,
          Var("x") |> Exp.fresh,
          None,
          None,
        )
        |> Exp.fresh,
        Tuple([
          TupLabel(Label("a") |> Exp.fresh, Int(1) |> Exp.fresh)
          |> Exp.fresh,
        ])
        |> Exp.fresh,
      )
      |> Exp.fresh,
    ),
    fully_consistent_typecheck(
      "Singleton labeled argument function application with no labeled param",
      {|(fun a=x->x)(1)|},
      Some(tunknown(Internal)),
      Ap(
        Forward,
        Fun(
          Tuple([
            TupLabel(Label("a") |> Pat.fresh, Var("x") |> Pat.fresh)
            |> Pat.fresh,
          ])
          |> Pat.fresh,
          Var("x") |> Exp.fresh,
          None,
          None,
        )
        |> Exp.fresh,
        Tuple([
          TupLabel(Label("a") |> Exp.fresh, Int(1) |> Exp.fresh)
          |> Exp.fresh,
        ])
        |> Exp.fresh,
      )
      |> Exp.fresh,
    ),
    fully_consistent_typecheck(
      "Singleton labeled argument not labeled in pattern",
      {|let x : (a=Int) -> Int = fun a -> a in x(2)|},
      Some(tint()),
      parse_exp("let x : (a=Int) -> Int = fun a -> a in x(2)"),
    ),
    inconsistent_typecheck(
      "Unknown label in last postition for expression",
      parse_exp(
        {|let x : (a=Int, b=Float, String) = (1, 1.2, z="hello") in |},
      ),
    ),
    test_case(
      "Duplicate label synthesis",
      `Quick,
      () => {
        let exp = parse_exp({|(a="hello", a=3)|});

        let (l1, l2, tl1, tl2, tuple) =
          switch (exp.term) {
          | Parens(
              {
                term:
                  Tuple([
                    {term: TupLabel({term: Label(_), _} as l1, _), _} as tl1,
                    {term: TupLabel({term: Label(_), _} as l2, _), _} as tl2,
                  ]),
                _,
              } as tuple,
            ) => (
              l1,
              l2,
              tl1,
              tl2,
              tuple,
            )
          | _ => Alcotest.fail("Unexpected form")
          };

        let s = statics(exp);

        check(
          option(testable_info_error_exp),
          "Tuple Error",
          Some(
            Common(
              TupleLabelError({
                malformed_labels: [],
                duplicate_labels: ["a", "a"],
                invalid_labels: [],
                typ:
                  Prod([
                    TupLabel(
                      Label("a") |> Typ.temp,
                      Unknown(Internal) |> Typ.temp,
                    )
                    |> Typ.temp,
                  ])
                  |> Typ.temp,
              }),
            ),
          ),
          Statics.get_error_at(s, IdTagged.rep_id(tuple)),
        );

        check(
          option(testable_info_error_exp),
          "TupLabel1 Error",
          Some(
            Common(
              TupleLabelError({
                malformed_labels: [],
                duplicate_labels: ["a"],
                invalid_labels: [],
                typ:
                  TupLabel(Label("a") |> Typ.temp, String |> Typ.temp)
                  |> Typ.temp,
              }),
            ),
          ),
          Statics.get_error_at(s, IdTagged.rep_id(tl1)),
        );
        check(
          option(testable_info_error_exp),
          "TupLabel2 Error",
          Some(
            Common(
              TupleLabelError({
                malformed_labels: [],
                duplicate_labels: ["a"],
                invalid_labels: [],
                typ:
                  TupLabel(Label("a") |> Typ.temp, Int |> Typ.temp)
                  |> Typ.temp,
              }),
            ),
          ),
          Statics.get_error_at(s, IdTagged.rep_id(tl2)),
        );

        check(
          option(testable_info_error_exp),
          "Duplicate Label Error 1",
          Some(Common(DuplicateLabel("a", Label("a") |> Typ.temp))),
          Statics.get_error_at(s, IdTagged.rep_id(l1)),
        );
        check(
          option(testable_info_error_exp),
          "Duplicate Label Error 2",
          Some(Common(DuplicateLabel("a", Label("a") |> Typ.temp))),
          Statics.get_error_at(s, IdTagged.rep_id(l2)),
        );
      },
    ),
    test_case(
      "Bad label Projection",
      `Quick,
      () => {
        let exp = parse_exp({|(1, 2) . 1|});

        let label =
          switch (exp.term) {
          | Dot(_, _ as l) => l
          | _ => Alcotest.fail("Unexpected form")
          };

        let s = statics(exp);

        check(
          option(testable_info_error_exp),
          "Tuple",
          Some(Common(NoType(BadLabel(Exp(label))))),
          Statics.get_error_at(s, IdTagged.rep_id(exp)),
        );
      },
    ),
    test_case(
      "Singleton Bad label synthesis",
      `Quick,
      () => {
        let exp = parse_exp({|(1="hello")|});

        let (l1, tl1, tuple) =
          switch (exp.term) {
          | Parens(
              {
                term:
                  Tuple([
                    {
                      term:
                        TupLabel(
                          {term: MultiHole([Exp({term: Int(1), _})]), _} as l1,
                          _,
                        ),
                      _,
                    } as tl1,
                  ]),
                _,
              } as tuple,
            ) => (
              l1,
              tl1,
              tuple,
            )
          | _ => Alcotest.fail("Unexpected form")
          };

        let s = statics(exp);

        check(
          option(testable_info_error_exp),
          "Tuple",
          Some(
            Common(
              TupleLabelError({
                malformed_labels: [Exp(l1)],
                duplicate_labels: [],
                invalid_labels: [],
                typ:
                  Prod([
                    TupLabel(
                      Unknown(Internal) |> Typ.temp,
                      String |> Typ.temp,
                    )
                    |> Typ.temp,
                  ])
                  |> Typ.temp,
              }),
            ),
          ),
          Statics.get_error_at(s, IdTagged.rep_id(tuple)),
        );

        check(
          option(testable_info_error_exp),
          "TupLabel1",
          Some(
            Common(
              TupleLabelError({
                malformed_labels: [Exp(l1)],
                invalid_labels: [],
                duplicate_labels: [],
                typ:
                  TupLabel(Unknown(Internal) |> Typ.temp, String |> Typ.temp)
                  |> Typ.temp,
              }),
            ),
          ),
          Statics.get_error_at(s, IdTagged.rep_id(tl1)),
        );

        check(
          option(testable_info_error_exp),
          "Label",
          Some(Common(NoType(BadLabel(Exp(l1))))),
          Statics.get_error_at(s, IdTagged.rep_id(l1)),
        );
      },
    ),
    test_case(
      "Bad label synthesis",
      `Quick,
      () => {
        let exp = parse_exp({|(1="hello", a=3)|});

        let (l1, l2, tl1, tl2, tuple) =
          switch (exp.term) {
          | Parens(
              {
                term:
                  Tuple([
                    {
                      term:
                        TupLabel(
                          {term: MultiHole([Exp({term: Int(1), _})]), _} as l1,
                          _,
                        ),
                      _,
                    } as tl1,
                    {term: TupLabel({term: Label(_), _} as l2, _), _} as tl2,
                  ]),
                _,
              } as tuple,
            ) => (
              l1,
              l2,
              tl1,
              tl2,
              tuple,
            )
          | _ => Alcotest.fail("Unexpected form")
          };

        let s = statics(exp);

        check(
          option(testable_info_error_exp),
          "Tuple Error Free",
          Some(
            Common(
              TupleLabelError({
                malformed_labels: [Exp(l1)],
                invalid_labels: [],
                duplicate_labels: [],
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
          Statics.get_error_at(s, IdTagged.rep_id(tuple)),
        );

        check(
          option(testable_info_error_exp),
          "TupLabel1 ",
          Some(
            Common(
              TupleLabelError({
                malformed_labels: [Exp(l1)],
                invalid_labels: [],
                duplicate_labels: [],
                typ:
                  TupLabel(Unknown(Internal) |> Typ.temp, String |> Typ.temp)
                  |> Typ.temp,
              }),
            ),
          ),
          Statics.get_error_at(s, IdTagged.rep_id(tl1)),
        );
        check(
          option(testable_info_error_exp),
          "TupLabel2 Error Free",
          None,
          Statics.get_error_at(s, IdTagged.rep_id(tl2)),
        );

        check(
          option(testable_info_error_exp),
          "Label Error malformed label",
          Some(Common(NoType(BadLabel(Exp(l1))))),
          Statics.get_error_at(s, IdTagged.rep_id(l1)),
        );
        check(
          option(testable_info_error_exp),
          "Label 2 Error Free",
          None,
          Statics.get_error_at(s, IdTagged.rep_id(l2)),
        );
      },
    ),
    test_case(
      "extra label",
      `Quick,
      () => {
        let exp =
          parse_exp(
            {|let extra_label : (Int, a=String) = (c=1, a="hello") in true|},
          );

        let (_typ, tuple, tl1, tl2, int_ty) =
          switch (exp.term) {
          | Let(
              {
                term:
                  Cast(
                    _,
                    {term: Parens({term: Prod([int_ty, _]), _}), _} as typ,
                    _,
                  ),
                _,
              },
              {term: Parens({term: Tuple([tl1, tl2]), _} as tuple), _},
              _,
            ) => (
              typ,
              tuple,
              tl1,
              tl2,
              int_ty,
            )
          | _ =>
            Alcotest.fail("Unexpected form: " ++ [%derive.show: Exp.t](exp))
          };

        let s = statics(exp);

        check(
          option(testable_info_error_exp),
          "Tuple Label1 Error",
          Some(
            Common(
              TupleLabelError({
                malformed_labels: [],
                duplicate_labels: [],
                invalid_labels: ["c"],
                typ: TupLabel(Label("c") |> Typ.temp, int_ty) |> Typ.temp,
              }),
            ),
          ),
          Statics.get_error_at(s, IdTagged.rep_id(tl1)),
        );
        check(
          option(testable_info_error_exp),
          "Tuple Label2 Error",
          None,
          Statics.get_error_at(s, IdTagged.rep_id(tl2)),
        );
        check(
          option(testable_info_error_exp),
          "Tuple Error",
          Some(
            Common(
              TupleLabelError({
                malformed_labels: [],
                duplicate_labels: [],
                invalid_labels: ["c"],
                typ:
                  Prod([
                    TupLabel(Label("c") |> Typ.temp, int_ty) |> Typ.temp,
                    TupLabel(Label("a") |> Typ.temp, String |> Typ.temp)
                    |> Typ.temp,
                  ])
                  |> Typ.temp,
              }),
            ),
          ),
          Statics.get_error_at(s, IdTagged.rep_id(tuple)),
        );
      },
    ),
    test_case(
      "tuple with cast to non-tuple",
      `Quick,
      () => {
        let exp = parse_exp({|(a=1, b=2) : Int|});

        let tuple =
          switch (exp.term) {
          | Cast({term: Parens({term: Tuple(_), _} as tuple), _}, _, _) => tuple
          | _ => Alcotest.fail("Unexpected form")
          };

        let s = statics(exp);

        check(
          option(testable_info_error_exp),
          "Tuple Error",
          Some(
            Common(
              Inconsistent(
                Expectation({
                  syn:
                    Prod([
                      TupLabel(Label("a") |> Typ.temp, Int |> Typ.temp)
                      |> Typ.temp,
                      TupLabel(Label("b") |> Typ.temp, Int |> Typ.temp)
                      |> Typ.temp,
                    ])
                    |> Typ.temp,
                  ana: Int |> Typ.temp,
                }),
              ),
            ),
          ),
          Statics.get_error_at(s, IdTagged.rep_id(tuple)),
        );
      },
    ),
    fully_consistent_typecheck(
      "Forall alpha equivalent in cast",
      {|let x : forall a -> a = in (x : forall b -> b)|},
      Some(
        Forall(Var("b") |> TPat.fresh, Var("b") |> Typ.fresh) |> Typ.fresh,
      ),
      parse_exp({|let x : forall a -> a = in (x : forall b -> b)|}),
    ),
    fully_consistent_typecheck(
      "Forall alpha equivalent in let",
      {|let x : forall a -> a = in let y : forall b -> b = x in 1|},
      Some(Typ.Fresh.tint()),
      parse_exp(
        {|let x : forall a -> a = in let y : forall b -> b = x in 1|},
      ),
    ),
  ],
);
