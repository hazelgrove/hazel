open Tezt;
open Tezt.Base;
open Haz3lcore;

let register_exp_test =
    (title, tags, m: Statics.map, uexp: Term.UExp.t, dhexp: DHExp.t) =>
  Test.register(
    ~__FILE__, ~title, ~tags=["hazelcore", "elaborator"] @ tags, () =>
    switch (Elaborator.dhexp_of_uexp(m, uexp)) {
    | None => Test.fail("Elaborator failed: Got None")
    | Some(d) =>
      if (d != dhexp) {
        Test.fail("Elaborator failed: Incorrect DHExp");
      } else {
        unit;
      }
    }
  );

let u1: Term.UExp.t = {ids: [0], term: Int(8)};
let m1: Statics.map = Statics.mk_map(u1);
let () = register_exp_test("Single integer test", [], m1, u1, IntLit(8));

let u2: Term.UExp.t = {ids: [0], term: EmptyHole};
let m2: Statics.map = Statics.mk_map(u2);
let () = register_exp_test("Empty hole test", [], m2, u2, EmptyHole(0, 0));

let u3: Term.UExp.t = {ids: [0], term: Var("x")};
let m3: Statics.map = Statics.mk_map(u3);
let d3: DHExp.t = NonEmptyHole(TypeInconsistent, 0, 0, FreeVar(0, 0, "x"));
let () = register_exp_test("Free variable test", [], m3, u3, d3);

let u4: Term.UExp.t = {
  ids: [0],
  term:
    Let(
      {
        ids: [1],
        term:
          Tuple([
            {ids: [2], term: Var("a")},
            {ids: [3], term: Var("b")},
          ]),
      },
      {
        ids: [4],
        term:
          Tuple([{ids: [5], term: Int(4)}, {ids: [6], term: Int(6)}]),
      },
      {
        ids: [7],
        term:
          BinOp(
            Int(Minus),
            {ids: [8], term: Var("a")},
            {ids: [9], term: Var("b")},
          ),
      },
    ),
};
let m4: Statics.map = Statics.mk_map(u4);
let d4: DHExp.t =
  Let(
    Tuple([Var("a"), Var("b")]),
    Tuple([IntLit(4), IntLit(6)]),
    BinIntOp(Minus, BoundVar("a"), BoundVar("b")),
  );
let () = register_exp_test("Let expression", [], m4, u4, d4);

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
    "Two free variables in binary operation",
    [],
    m5,
    u5,
    BinIntOp(Plus, d5a, d5b),
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
let () = register_exp_test("Function with free variable", [], m6, u6, d6b);

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
  register_exp_test("Function applied to free variable", [], m7, u7, d7b);

let u8a: Term.UExp.t = {
  ids: [1],
  term:
    BinOp(
      Int(Equals),
      {ids: [2], term: Int(4)},
      {ids: [3], term: Int(3)},
    ),
};
let u8b: Term.UExp.t = {
  ids: [0],
  term:
    Match(
      u8a,
      [
        ({ids: [6], term: Bool(true)}, {ids: [4], term: Int(24)}),
        ({ids: [7], term: Bool(false)}, {ids: [5], term: Bool(false)}),
      ],
    ),
};
let m8: Statics.map = Statics.mk_map(u8b);
let d8scrut: DHExp.t = BinIntOp(Equals, IntLit(4), IntLit(3));
let d8rules =
  DHExp.[
    Rule(BoolLit(true), IntLit(24)),
    Rule(BoolLit(false), BoolLit(false)),
  ];
let d8a: DHExp.t = InconsistentBranches(0, 0, Case(d8scrut, d8rules, 0));
let d8b: DHExp.t = NonEmptyHole(TypeInconsistent, 0, 0, d8a);
let () = register_exp_test("Inconsistent branches", [], m8, u8b, d8b);
