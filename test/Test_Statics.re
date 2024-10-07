open Alcotest;
open Haz3lcore;

let testable_typ = testable(Fmt.using(Typ.show, Fmt.string), Typ.fast_equal);
module FreshId = {
  let arrow = (a, b) => Arrow(a, b) |> Typ.fresh;
  let unknown = a => Unknown(a) |> Typ.fresh;
  let int = Typ.fresh(Int);
  let float = Typ.fresh(Float);
  let prod = a => Prod(a) |> Typ.fresh;
  let string = Typ.fresh(String);
};
let ids = List.init(12, _ => Id.mk());
let id_at = x => x |> List.nth(ids);
let statics = Statics.mk(CoreSettings.on, Builtins.ctx_init);
let alco_check = Alcotest.option(testable_typ) |> Alcotest.check;

// Get the type from the statics
let type_of = f => {
  let s = statics(f);
  switch (Id.Map.find(IdTagged.rep_id(f), s)) {
  | InfoExp({ty, _}) => Some(ty)
  | _ => None
  };
};

let unapplied_function = () =>
  alco_check(
    "Unknown param",
    Some(FreshId.(arrow(unknown(Internal), int))),
    type_of(
      Fun(
        VarPat("x") |> Pat.fresh,
        BinOp(Int(Plus), IntLit(4) |> Exp.fresh, IntLit(5) |> Exp.fresh)
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
            VarPat("x") |> Pat.fresh,
            BinOp(
              Int(Plus),
              IntLit(4) |> Exp.fresh,
              IntLit(5) |> Exp.fresh,
            )
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
            CastPat(VarPat("x") |> Pat.fresh, int, unknown(Internal))
            |> Pat.fresh,
            BinOp(
              Int(Plus),
              IntLit(4) |> Exp.fresh,
              IntLit(5) |> Exp.fresh,
            )
            |> Exp.fresh,
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
            TuplePat([
              CastPat(VarPat("x") |> Pat.fresh, int, unknown(Internal))
              |> Pat.fresh,
              CastPat(VarPat("y") |> Pat.fresh, int, unknown(Internal))
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
          Ap(
            Forward,
            Var("float_of_int") |> Exp.fresh,
            IntLit(1) |> Exp.fresh,
          )
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
              StringLit("hello") |> Exp.fresh,
              IntLit(1) |> Exp.fresh,
              Deferral(InAp) |> Exp.fresh,
            ],
          )
          |> Exp.fresh,
        ),
      )
    ),
  ];
