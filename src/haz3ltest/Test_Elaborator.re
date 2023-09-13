open Tezt;
open Tezt.Base;
open Haz3lcore;

let register_exp_test = (title, uexp: Term.UExp.t, dhexp: DHExp.t) => {
  let m = Statics.mk_map(uexp);
  Test.register(~__FILE__, ~title, ~tags=["hazelcore", "elaborator"], () =>
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
};

let ids = List.init(12, _x => Id.mk());
let id_at = x => x |> List.nth(ids);

let u1: Term.UExp.t = {ids: [id_at(0)], term: Int(8)};
register_exp_test("Single integer", u1, IntLit(8));

let u2: Term.UExp.t = {ids: [id_at(0)], term: EmptyHole};
register_exp_test("Empty hole", u2, EmptyHole(id_at(0), 0));

let u3: Term.UExp.t = {
  ids: [id_at(0)],
  term: Parens({ids: [id_at(1)], term: Var("y")}),
};
let d3: DHExp.t =
  NonEmptyHole(TypeInconsistent, id_at(1), 0, FreeVar(id_at(1), 0, "y"));
register_exp_test("Free variable in parentheses", u3, d3);

let u4: Term.UExp.t = {
  ids: [id_at(0)],
  term:
    Let(
      {
        ids: [id_at(1)],
        term:
          Tuple([
            {ids: [id_at(2)], term: Var("a")},
            {ids: [id_at(3)], term: Var("b")},
          ]),
      },
      {
        ids: [id_at(4)],
        term:
          Tuple([
            {ids: [id_at(5)], term: Int(4)},
            {ids: [id_at(6)], term: Int(6)},
          ]),
      },
      {
        ids: [id_at(7)],
        term:
          BinOp(
            Int(Minus),
            {ids: [id_at(8)], term: Var("a")},
            {ids: [id_at(9)], term: Var("b")},
          ),
      },
    ),
};
let d4: DHExp.t =
  Let(
    Tuple([Var("a"), Var("b")]),
    Tuple([IntLit(4), IntLit(6)]),
    BinIntOp(Minus, BoundVar("a"), BoundVar("b")),
  );
register_exp_test("Let expression", u4, d4);

let u5: Term.UExp.t = {
  ids: [id_at(0)],
  term:
    BinOp(
      Int(Plus),
      {ids: [id_at(1)], term: Bool(false)},
      {ids: [id_at(2)], term: Var("y")},
    ),
};
let d5: DHExp.t =
  BinIntOp(
    Plus,
    NonEmptyHole(TypeInconsistent, id_at(1), 0, BoolLit(false)),
    NonEmptyHole(TypeInconsistent, id_at(2), 0, FreeVar(id_at(2), 0, "y")),
  );
register_exp_test("Badly typed binary operation", u5, d5);

let u6: Term.UExp.t = {
  ids: [id_at(0)],
  term:
    If(
      {ids: [id_at(1)], term: Bool(false)},
      {ids: [id_at(2)], term: Int(8)},
      {ids: [id_at(3)], term: Int(6)},
    ),
};
let d6: DHExp.t =
  ConsistentCase(
    Case(
      BoolLit(false),
      DHExp.[
        Rule(BoolLit(true), IntLit(8)),
        Rule(BoolLit(false), IntLit(6)),
      ],
      0,
    ),
  );
register_exp_test("If statement", u6, d6);

let u7: Term.UExp.t = {
  ids: [id_at(0)],
  term:
    Ap(
      {
        ids: [id_at(1)],
        term:
          Fun(
            {ids: [id_at(2)], term: Var("x")},
            {
              ids: [id_at(3)],
              term:
                BinOp(
                  Int(Plus),
                  {ids: [id_at(4)], term: Int(4)},
                  {ids: [id_at(5)], term: Var("x")},
                ),
            },
          ),
      },
      {ids: [id_at(6)], term: Var("y")},
    ),
};
let d7: DHExp.t =
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
    NonEmptyHole(TypeInconsistent, id_at(6), 0, FreeVar(id_at(6), 0, "y")),
  );
register_exp_test("Function applied to free variable", u7, d7);

let u8: Term.UExp.t = {
  ids: [id_at(0)],
  term:
    Match(
      {
        ids: [id_at(1)],
        term:
          BinOp(
            Int(Equals),
            {ids: [id_at(2)], term: Int(4)},
            {ids: [id_at(3)], term: Int(3)},
          ),
      },
      [
        (
          {ids: [id_at(6)], term: Bool(true)},
          {ids: [id_at(4)], term: Int(24)},
        ),
        (
          {ids: [id_at(7)], term: Bool(false)},
          {ids: [id_at(5)], term: Bool(false)},
        ),
      ],
    ),
};
let d8scrut: DHExp.t = BinIntOp(Equals, IntLit(4), IntLit(3));
let d8rules =
  DHExp.[
    Rule(BoolLit(true), IntLit(24)),
    Rule(BoolLit(false), BoolLit(false)),
  ];
let d8a: DHExp.t =
  InconsistentBranches(id_at(0), 0, Case(d8scrut, d8rules, 0));
let d8b: DHExp.t = NonEmptyHole(TypeInconsistent, id_at(0), 0, d8a);
register_exp_test("Inconsistent branches", u8, d8b);

let u9: Term.UExp.t = {
  ids: [id_at(0)],
  term:
    Let(
      {
        ids: [id_at(1)],
        term:
          TypeAnn(
            {ids: [id_at(2)], term: Var("f")},
            {
              ids: [id_at(3)],
              term:
                Arrow(
                  {ids: [id_at(4)], term: Int},
                  {ids: [id_at(5)], term: Int},
                ),
            },
          ),
      },
      {
        ids: [id_at(6)],
        term:
          Fun(
            {ids: [id_at(7)], term: Var("x")},
            {
              ids: [id_at(8)],
              term:
                BinOp(
                  Int(Plus),
                  {ids: [id_at(9)], term: Int(1)},
                  {ids: [id_at(10)], term: Var("x")},
                ),
            },
          ),
      },
      {ids: [id_at(11)], term: Int(55)},
    ),
};
let d9: DHExp.t =
  Let(
    Var("f"),
    FixF(
      "f",
      Arrow(Int, Int),
      Fun(
        Var("x"),
        Int,
        BinIntOp(Plus, IntLit(1), BoundVar("x")),
        Some("f"),
      ),
    ),
    IntLit(55),
  );
register_exp_test("Let expression for function", u9, d9);
