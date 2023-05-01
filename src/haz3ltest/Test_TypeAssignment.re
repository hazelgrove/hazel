open Tezt;
open Tezt.Base;
open Haz3lcore;

let register_exp_test =
    (title, tags, ctx: Ctx.t, dl: Delta.t, dh: DHExp.t, ty: Typ.t) => {
  Test.register(
    ~__FILE__, ~title, ~tags=["hazelcore", "type_assignment"] @ tags, () => {
    switch (TypeAssignment.typ_of_dhexp(ctx, dl, dh)) {
    | None => Test.fail("Type assignment got None")
    | Some(tyr) =>
      if (tyr != ty) {
        Test.fail("Type assignment got incorrect type");
      } else {
        unit;
      }
    }
  });
};

let dh1: DHExp.t = IntLit(5);
let () = register_exp_test("Integer literal", [], [], Delta.empty, dh1, Int);

let dh2: DHExp.t = BoundVar("x");
let ve1: Ctx.entry = VarEntry({name: "y", id: 0, typ: Bool});
let ve2: Ctx.entry = VarEntry({name: "x", id: 1, typ: Float});
let () =
  register_exp_test(
    "Bound variable",
    [],
    [ve1, ve2],
    Delta.empty,
    dh2,
    Float,
  );

let dh3a: DHExp.t = BinIntOp(Plus, BoundVar("y"), IntLit(3));
let dh3b: DHExp.t = Fun(Var("y"), Int, dh3a, None);
let () =
  register_exp_test(
    "Function type assignment",
    [],
    [],
    Delta.empty,
    dh3b,
    Arrow(Int, Int),
  );

let dh4a: DHExp.t = Cast(EmptyHole(0, 0), Unknown(Internal), Int);
let dh4b: DHExp.t = BinIntOp(Plus, dh4a, IntLit(4));
let dl4: Delta.t =
  Delta.add(0, (ExpressionHole, Unknown(Internal), []), Delta.empty);
let () = register_exp_test("Bin op with empty hole", [], [], dl4, dh4b, Int);

let dh5scrut: DHExp.t = BoolLit(false);
let dh5rules =
  DHExp.[
    Rule(BoolLit(true), IntLit(5)),
    Rule(BoolLit(false), IntLit(6)),
  ];
let dh5: DHExp.t = ConsistentCase(Case(dh5scrut, dh5rules, 0));
let () =
  register_exp_test(
    "Consistent case statement",
    [],
    [],
    Delta.empty,
    dh5,
    Int,
  );

let dh6scrut: DHExp.t = BinIntOp(Equals, IntLit(4), IntLit(4));
let dh6rules =
  DHExp.[
    Rule(BoolLit(true), IntLit(3)),
    Rule(BoolLit(false), BoolLit(true)),
  ];
let dh6: DHExp.t = InconsistentBranches(0, 0, Case(dh6scrut, dh6rules, 0));
let dl6: Delta.t =
  Delta.add(0, (ExpressionHole, Unknown(Internal), []), Delta.empty);
let () =
  register_exp_test(
    "Inconsistent case statement",
    [],
    [],
    dl6,
    dh6,
    Unknown(Internal),
  );
