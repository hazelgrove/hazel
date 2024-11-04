open Alcotest;
open Haz3lcore;

let testable_typ = testable(Fmt.using(Typ.show, Fmt.string), Typ.fast_equal);
let testable_status_exp =
  testable(
    Fmt.using(Info.show_status_exp, Fmt.string),
    // TODO: Fix this
    (a, b) => {
    switch (a, b) {
    | (
        InHole(Common(Inconsistent(Expectation({ana: a1, syn: a2})))),
        InHole(Common(Inconsistent(Expectation({ana: b1, syn: b2})))),
      ) =>
      Typ.fast_equal(a1, b1) && Typ.fast_equal(a2, b2)
    | _ => false
    }
  });

let status_exp: testable(Info.status_exp) =
  testable(Fmt.using(Info.show_status_exp, Fmt.string), (==));
module FreshId = {
  let arrow = (a, b) => Arrow(a, b) |> Typ.fresh;
  let unknown = a => Unknown(a) |> Typ.fresh;
  let int = Typ.fresh(Int);
  let float = Typ.fresh(Float);
  let prod = a => Prod(a) |> Typ.fresh;
  let label = a => Label(a) |> Typ.fresh;
  let tup_label = (a, b) => TupLabel(a, b) |> Typ.fresh;
  let string = Typ.fresh(String);
};
let ids = List.init(12, _ => Id.mk());
let id_at = x => x |> List.nth(ids);
let statics = Statics.mk(CoreSettings.on, Builtins.ctx_init);
let alco_check = Alcotest.option(testable_typ) |> Alcotest.check;
let parse_exp = (s: string) =>
  MakeTerm.from_zip_for_sem(Option.get(Printer.zipper_of_string(s))).term;

let info_of_id = (~statics_map=?, f: UExp.t, id: Id.t) => {
  // print_endline(
  //   "Map: " ++ [%derive.show: option(Statics.Map.t)](statics_map),
  // );
  let s =
    switch (statics_map) {
    | Some(s) => s
    | None => statics(f)
    };
  switch (Id.Map.find(id, s)) {
  | InfoExp(ie) => Some(ie)
  | _ => None
  };
};

// Get the type from the statics
let type_of = (~statics_map=?, f) => {
  Option.map(
    (ie: Info.exp) => ie.ty,
    info_of_id(~statics_map?, f, IdTagged.rep_id(f)),
  );
};

let inconsistent_typecheck = (name, _serialized, exp) => {
  test_case(
    name,
    `Quick,
    () => {
      let s = statics(exp);
      let errors =
        List.map(
          (id: Id.t) => {
            let info = Id.Map.find(id, s);
            switch (info) {
            | InfoExp(ie) => ie.status
            | _ => fail("Expected InfoExp")
            };
          },
          Statics.Map.error_ids(s),
        );
      if (errors == []) {
        fail("Expected errors");
      };
      print_endline(
        "Errors: " ++ [%derive.show: list(Info.status_exp)](errors),
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
      let errors =
        List.map(
          (id: Id.t) => {
            let info = Id.Map.find(id, s);
            switch (info) {
            | InfoExp(ie) => ie.status
            | _ => fail("Expected InfoExp")
            };
          },
          Statics.Map.error_ids(s),
        );
      Alcotest.check(list(status_exp), "Static Errors", [], errors);
      alco_check(serialized, expected, type_of(~statics_map=s, exp));
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
      Alcotest.option(testable_status_exp),
      "let x = (1, 2) in  let y : (a=Int, b=Int) = x in y",
      Some(
        InHole(
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
      ),
      Option.map(
        (ie: Info.exp) => ie.status,
        info_of_id(
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
      ),
    )
  );

let simple_inconsistency =
  test_case(
    "Typechecking fails for unlabeled variable being assigned to labeled tuple",
    `Quick,
    () =>
    Alcotest.check(
      Alcotest.option(testable_status_exp),
      "let y : String = true",
      Some(
        InHole(
          Common(
            Inconsistent(
              Expectation({ana: String |> Typ.fresh, syn: Bool |> Typ.fresh}),
            ),
          ),
        ),
      ),
      Option.map(
        (ie: Info.exp) => ie.status,
        info_of_id(
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
      ),
    )
  );

let unapplied_function = () =>
  alco_check(
    "Unknown param",
    Some(FreshId.(arrow(unknown(Internal), int))),
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

let tests =
  FreshId.[
    fully_consistent_typecheck(
      "Function with unknown param",
      "x => 4 + 5",
      Some(arrow(unknown(Internal), int)),
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
      Some(arrow(int, int)),
      Fun(
        Cast(Var("x") |> Pat.fresh, int, unknown(Internal)) |> Pat.fresh,
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
      Some(arrow(prod([tup_label(label("a"), unknown(Internal))]), int)),
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
      Some(arrow(prod([int, int]), int)),
      Fun(
        Tuple([
          Cast(Var("x") |> Pat.fresh, int, unknown(Internal)) |> Pat.fresh,
          Cast(Var("y") |> Pat.fresh, int, unknown(Internal)) |> Pat.fresh,
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
      Some(arrow(prod([int, int]), int)),
      Fun(
        Tuple([
          Cast(Var("x") |> Pat.fresh, int, unknown(Internal)) |> Pat.fresh,
          Cast(Var("y") |> Pat.fresh, int, unknown(Internal)) |> Pat.fresh,
        ])
        |> Pat.fresh,
        BinOp(Int(Plus), Var("x") |> Exp.fresh, Var("y") |> Exp.fresh)
        |> Exp.fresh,
        None,
        None,
      )
      |> Exp.fresh,
    ),
    // fully_consistent_typecheck(
    //   "function application",
    //   "float_of_int(1)",
    //   Some(float),
    //   Ap(Forward, Var("float_of_int") |> Exp.fresh, Int(1) |> Exp.fresh)
    //   |> Exp.fresh,
    // ),
    // fully_consistent_typecheck(
    //   "function deferral",
    //   "string_sub(\"hello\", 1, _)",
    //   Some(arrow(int, string)),
    //   DeferredAp(
    //     Var("string_sub") |> Exp.fresh,
    //     [
    //       String("hello") |> Exp.fresh,
    //       Int(1) |> Exp.fresh,
    //       Deferral(InAp) |> Exp.fresh,
    //     ],
    //   )
    //   |> Exp.fresh,
    // ),
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
        Parens(String("a") |> Exp.fresh) |> Exp.fresh, // TODO Need to assert there's no inconsistency in this branch
        Var("x") |> Exp.fresh,
      )
      |> Exp.fresh,
    ),
    inconsistent_typecheck(
      "Singleton Labled Tuple ascription in let with wrong type should fail",
      "let x : (l=String) = 1 in x",
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
        Int(1) |> Exp.fresh, // TODO Need to assert there's no inconsistency in this branch
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
        |> Exp.fresh, // TODO Need to assert there's no inconsistency in this branch
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
      Some(unknown(Internal)),
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
      Some(int),
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
  ];
