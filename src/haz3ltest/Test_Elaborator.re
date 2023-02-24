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

let u5: Term.UExp.t = {
  ids: [0],
  term:
    BinOp(
      Int(Plus),
      {ids: [1], term: Var("y")},
      {ids: [2], term: Var("x")},
    ),
};
let m5: Statics.map = Statics.mk_map(u5);
let d5a: DHExp.t = NonEmptyHole(TypeInconsistent, 1, 0, FreeVar(1, 0, "y"));
let d5b: DHExp.t = NonEmptyHole(TypeInconsistent, 2, 0, FreeVar(2, 0, "x"));
let () =
  register_exp_test(
    "Two free variable binary operation",
    [],
    m5,
    u5,
    BinIntOp(Plus, d5a, d5b),
    Delta.add(
      2,
      (ExpressionHole, Int, Builtins.ctx(Builtins.Pervasives.builtins)),
      Delta.add(
        1,
        (ExpressionHole, Int, Builtins.ctx(Builtins.Pervasives.builtins)),
        Delta.empty,
      ),
    ),
  );

let u6: Term.UExp.t = {
  ids: [0],
  term:
    Fun(
      {ids: [1], term: Var("x")},
      {
        ids: [4],
        term:
          BinOp(
            Int(Plus),
            {ids: [5], term: Var("x")},
            {ids: [6], term: Var("y")},
          ),
      },
    ),
};
let m6: Statics.map = Statics.mk_map(u6);
let d6a: DHExp.t = NonEmptyHole(TypeInconsistent, 6, 0, FreeVar(6, 0, "y"));
let d6b: DHExp.t =
  Fun(
    Var("x"),
    Unknown(Internal),
    BinIntOp(Plus, Cast(BoundVar("x"), Unknown(Internal), Int), d6a),
    None,
  );
let () =
  register_exp_test(
    "Function with free variable",
    [],
    m6,
    u6,
    d6b,
    Delta.add(
      6,
      (
        ExpressionHole,
        Int,
        VarMap.concat(
          Builtins.ctx(Builtins.Pervasives.builtins),
          [VarEntry({name: "x", id: 1, typ: Unknown(Internal)})],
        ),
      ),
      Delta.empty,
    ),
  );

let u7: Term.UExp.t = {
  ids: [0],
  term:
    Ap(
      {
        ids: [1],
        term:
          Fun(
            {ids: [2], term: Var("x")},
            {
              ids: [3],
              term:
                BinOp(
                  Int(Plus),
                  {ids: [4], term: Int(4)},
                  {ids: [5], term: Var("x")},
                ),
            },
          ),
      },
      {ids: [6], term: Var("y")},
    ),
};
let m7: Statics.map = Statics.mk_map(u7);
let d7a: DHExp.t = NonEmptyHole(TypeInconsistent, 6, 0, FreeVar(6, 0, "y"));
let d7b: DHExp.t =
  Ap(
    Fun(
      Var("x"),
      Unknown(Internal),
      BinIntOp(
        Plus,
        IntLit(4),
        Cast(BoundVar("x"), Unknown(Internal), Int),
      ),
      None,
    ),
    d7a,
  );

let () =
  register_exp_test(
    "Function applied to free variable",
    [],
    m7,
    u7,
    d7b,
    Delta.add(
      6,
      (
        ExpressionHole,
        Unknown(Internal),
        Builtins.ctx(Builtins.Pervasives.builtins),
      ),
      Delta.empty,
    ),
  );
