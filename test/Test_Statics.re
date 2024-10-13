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

let info_of_id = (f: UExp.t, id: Id.t) => {
  let s = statics(f);
  switch (Id.Map.find(id, s)) {
  | InfoExp(ie) => Some(ie)
  | _ => None
  };
};

// Get the type from the statics
let type_of = f => {
  Option.map((ie: Info.exp) => ie.ty, info_of_id(f, IdTagged.rep_id(f)));
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
    test_case("Function with unknown param", `Quick, () =>
      alco_check(
        "x => 4 + 5",
        Some(arrow(unknown(Internal), int)),
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
      )
    ),
    test_case("Function with known param", `Quick, () =>
      alco_check(
        "x : Int => 4 + 5",
        Some(arrow(int, int)),
        type_of(
          Fun(
            Cast(Var("x") |> Pat.fresh, int, unknown(Internal)) |> Pat.fresh,
            BinOp(Int(Plus), Int(4) |> Exp.fresh, Int(5) |> Exp.fresh)
            |> Exp.fresh,
            None,
            None,
          )
          |> Exp.fresh,
        ),
      )
    ),
    test_case("Function with labeled param", `Quick, () =>
      alco_check(
        "fun (a=x) -> 4",
        Some(
          arrow(prod([tup_label(label("a"), unknown(Internal))]), int),
        ),
        type_of(
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
      )
    ),
    test_case("bifunction", `Quick, () =>
      alco_check(
        "x : Int, y: Int => x + y",
        Some(arrow(prod([int, int]), int)),
        type_of(
          Fun(
            Tuple([
              Cast(Var("x") |> Pat.fresh, int, unknown(Internal))
              |> Pat.fresh,
              Cast(Var("y") |> Pat.fresh, int, unknown(Internal))
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
      )
    ),
    test_case("function application", `Quick, () =>
      alco_check(
        "float_of_int(1)",
        Some(float),
        type_of(
          Ap(Forward, Var("float_of_int") |> Exp.fresh, Int(1) |> Exp.fresh)
          |> Exp.fresh,
        ),
      )
    ),
    test_case("function deferral", `Quick, () =>
      alco_check(
        "string_sub(\"hello\", 1, _)",
        Some(arrow(int, string)),
        type_of(
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
      )
    ),
    unlabeled_tuple_to_labeled_fails,
    simple_inconsistency,
    test_case("Assigning labeled tuple to variable", `Quick, () => {
      alco_check(
        "let x = (l=32) in
           let y : (l=Int) = x in y",
        Some(
          Prod([
            TupLabel(Label("l") |> Typ.fresh, Int |> Typ.fresh) |> Typ.fresh,
          ])
          |> Typ.fresh,
        ),
        type_of(
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
      )
    }),
    test_case("Singleton Labled Tuple ascription in let", `Quick, () => {
      alco_check(
        "let x : (l=String) = (\"a\") in x",
        Some(
          Prod([
            TupLabel(Label("l") |> Typ.fresh, String |> Typ.fresh)
            |> Typ.fresh,
          ])
          |> Typ.fresh,
        ),
        type_of(
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
      )
    }),
  ];
