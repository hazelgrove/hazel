open Alcotest;
open Haz3lcore;
let dhexp_typ = testable(Fmt.using(Exp.show, Fmt.string), DHExp.fast_equal);

let ids = List.init(12, _ => Id.mk());
let id_at = x => x |> List.nth(ids);
let statics = Statics.mk(CoreSettings.on, Builtins.ctx_init);

// Get the type from the statics
let type_of = f => {
  let s = statics(f);
  switch (Id.Map.find(IdTagged.rep_id(f), s)) {
  | InfoExp({ty, _}) => Some(ty)
  | _ => None
  };
};

let int_evaluation =
  Evaluator.evaluate(Builtins.env_init, {d: Exp.Int(8) |> Exp.fresh});

let evaluation_test = (msg, expected, unevaluated) =>
  check(
    dhexp_typ,
    msg,
    expected,
    Evaluator.Result.unbox(
      snd(Evaluator.evaluate(Builtins.env_init, {d: unevaluated})),
    ),
  );

let test_int = () =>
  evaluation_test("8", Int(8) |> Exp.fresh, Int(8) |> Exp.fresh);

let test_sum = () =>
  evaluation_test(
    "4 + 5",
    Int(9) |> Exp.fresh,
    BinOp(Int(Plus), Int(4) |> Exp.fresh, Int(5) |> Exp.fresh) |> Exp.fresh,
  );

let tests = [
  test_case("Integer literal", `Quick, test_int),
  test_case("Integer sum", `Quick, test_sum),
];
