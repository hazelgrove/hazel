open Tezt;
open Tezt.Base;
open Haz3lcore;

let register_exp_test =
    (
      title,
      tags,
      m: Statics.map,
      uexp: Term.UExp.t,
      dhexp: DHExp.t,
      delta: Delta.t,
    ) =>
  Test.register(
    ~__FILE__, ~title, ~tags=["hazelcore", "skelparser"] @ tags, () =>
    switch (Elaborator.dhexp_of_uexp(m, uexp)) {
    | None => Test.fail("Elaborator failed: got None")
    | Some((d, dl)) =>
      if (d != dhexp) {
        Test.fail("Elaborator failed: Incorrect DHExp");
      } else if (!Delta.equal(Delta.cmp, dl, delta)) {
        Test.fail("Elaborator failed: Incorrect Delta");
      } else {
        unit;
      }
    }
  );

//let list_to_map = (l: list(int, Statics.t), m: Statics.map): Statics.map =>
//l |> List.to_seq |> Statics.map.of_seq;

let u1: Term.UExp.t = {ids: [0], term: Int(8)};
let m1: Statics.map = Statics.mk_map(u1);
let () =
  register_exp_test(
    "Single integer test",
    [],
    m1,
    u1,
    IntLit(8),
    Delta.empty,
  );

let u2: Term.UExp.t = {ids: [0], term: EmptyHole};
let m2: Statics.map = Statics.mk_map(u2);
let () =
  register_exp_test(
    "Empty hole test",
    [],
    m2,
    u2,
    EmptyHole(0, 0),
    Delta.add(
      0,
      (
        ExpressionHole,
        Unknown(TypeHole),
        Builtins.ctx(Builtins.Pervasives.builtins),
      ),
      Delta.empty,
    ),
  );

let u3: Term.UExp.t = {ids: [0], term: Var("x")};
let m3: Statics.map = Statics.mk_map(u3);
let d3: DHExp.t = NonEmptyHole(TypeInconsistent, 0, 0, FreeVar(0, 0, "x"));
let () =
  register_exp_test(
    "Free variable test",
    [],
    m3,
    u3,
    d3,
    Delta.add(
      0,
      (
        ExpressionHole,
        Unknown(TypeHole),
        Builtins.ctx(Builtins.Pervasives.builtins),
      ),
      Delta.empty,
    ),
  );

let u4: Term.UExp.t = {
  ids: [0],
  term:
    BinOp(
      Int(Plus),
      {ids: [1], term: Int(3)},
      {ids: [2], term: Var("x")},
    ),
};
let m4: Statics.map = Statics.mk_map(u4);
let d4: DHExp.t = NonEmptyHole(TypeInconsistent, 2, 0, FreeVar(2, 0, "x"));
let () =
  register_exp_test(
    "Free variable binary operation",
    [],
    m4,
    u4,
    BinIntOp(Plus, IntLit(3), d4),
    Delta.add(
      2,
      (ExpressionHole, Int, Builtins.ctx(Builtins.Pervasives.builtins)),
      Delta.empty,
    ),
  );
