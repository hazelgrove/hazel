open Prop;

let of_ghost: Rule.t => Prop.deduction(Prop.t) =
  rule => {
    let (!): Prop.term => Prop.t = IdTagged.fresh;
    let e = () => !Var("e");
    let e1 = () => !Var("e₁");
    let e2 = () => !Var("e₂");
    let e3 = () => !Var("e₃");
    let v = () => !Var("v");
    let v1 = () => !Var("v₁");
    let v2 = () => !Var("v₂");
    let v3 = () => !Var("v₃");
    let t = () => !TVar("t");
    let a = () => !TVar("a");
    let b = () => !TVar("b");
    let c = () => !TVar("c");
    let p = () => !TVar("p");
    let t1 = () => !TVar("t₁");
    let t2 = () => !TVar("t₂");
    let x = () => !Var("x");
    let xp = () => !Pat("x");
    let ex = () => !Var("[v₁/x]e₁");
    let yp = () => !Pat("y");
    let n = () => !NumLit(7);
    let gamma = () => !Var("Γ");
    let ctx = () => !Ctx([gamma()]);
    let ctx_x = () => !Ctx([gamma(), !HasTy(!Var("x"), t1())]);
    let ctx_y = () => !Ctx([gamma(), !HasTy(!Var("y"), t2())]);
    let ctx_xy = () =>
      !Ctx([gamma(), !HasTy(!Var("y"), t2()), !HasTy(!Var("x"), t1())]);
    let ctx_a = () => !Ctx([gamma(), a()]);
    let ctx_b = () => !Ctx([gamma(), b()]);
    switch (rule) {
    | A_Subsumption =>
      let prems = [!Entail(ctx(), !Syn(e(), t()))];
      let concl = !Entail(ctx(), !Ana(e(), t()));
      {concl, prems};
    | E_Val =>
      let prems = [!Val(v())];
      let concl = !Eval(v(), v());
      {concl, prems};
    | S_Num =>
      let concl = !Entail(ctx(), !Syn(n(), !Num));
      {concl, prems: []};
    | T_Num =>
      let concl = !Entail(ctx(), !HasTy(n(), !Num));
      {concl, prems: []};
    | V_Num =>
      let concl = !Val(n());
      {concl, prems: []};
    | S_True =>
      let concl = !Entail(ctx(), !Syn(!True, !Bool));
      {concl, prems: []};
    | T_True =>
      let concl = !Entail(ctx(), !HasTy(!True, !Bool));
      {concl, prems: []};
    | V_True =>
      let concl = !Val(!True);
      {concl, prems: []};
    | S_False =>
      let concl = !Entail(ctx(), !Syn(!False, !Bool));
      {concl, prems: []};
    | T_False =>
      let concl = !Entail(ctx(), !HasTy(!False, !Bool));
      {concl, prems: []};
    | V_False =>
      let concl = !Val(!False);
      {concl, prems: []};
    | S_Triv =>
      let concl = !Entail(ctx(), !Syn(!Triv, !Unit));
      {concl, prems: []};
    | T_Triv =>
      let concl = !Entail(ctx(), !HasTy(!Triv, !Unit));
      {concl, prems: []};
    | V_Triv =>
      let concl = !Val(!Triv);
      {concl, prems: []};
    | S_Neg =>
      let prems = [!Entail(ctx(), !Syn(e(), !Num))];
      let concl = !Entail(ctx(), !Syn(!UnOp(!OpNeg, e()), !Num));
      {concl, prems};
    | T_Neg =>
      let prems = [!Entail(ctx(), !HasTy(e(), !Num))];
      let concl = !Entail(ctx(), !HasTy(!UnOp(!OpNeg, e()), !Num));
      {concl, prems};
    | E_Neg =>
      let prems = [!Eval(e(), !NumLit(-7))];
      let concl = !Eval(!UnOp(!OpNeg, e()), n());
      {concl, prems};
    | S_Plus =>
      let prems = [
        !Entail(ctx(), !Ana(e1(), !Num)),
        !Entail(ctx(), !Ana(e2(), !Num)),
      ];
      let concl = !Entail(ctx(), !Syn(!BinOp(!OpPlus, e1(), e2()), !Num));
      {concl, prems};
    | T_Plus =>
      let prems = [
        !Entail(ctx(), !HasTy(e1(), !Num)),
        !Entail(ctx(), !HasTy(e2(), !Num)),
      ];
      let concl = !Entail(ctx(), !HasTy(!BinOp(!OpPlus, e1(), e2()), !Num));
      {concl, prems};
    | E_Plus =>
      let prems = [!Eval(e1(), !NumLit(3)), !Eval(e2(), !NumLit(4))];
      let concl = !Eval(!BinOp(!OpPlus, e1(), e2()), n());
      {concl, prems};
    | S_Minus =>
      let prems = [
        !Entail(ctx(), !Ana(e1(), !Num)),
        !Entail(ctx(), !Ana(e2(), !Num)),
      ];
      let concl = !Entail(ctx(), !Syn(!BinOp(!OpMinus, e1(), e2()), !Num));
      {concl, prems};
    | T_Minus =>
      let prems = [
        !Entail(ctx(), !HasTy(e1(), !Num)),
        !Entail(ctx(), !HasTy(e2(), !Num)),
      ];
      let concl =
        !Entail(ctx(), !HasTy(!BinOp(!OpMinus, e1(), e2()), !Num));
      {concl, prems};
    | E_Minus =>
      let prems = [!Eval(e1(), !NumLit(10)), !Eval(e2(), !NumLit(3))];
      let concl = !Eval(!BinOp(!OpMinus, e1(), e2()), n());
      {concl, prems};
    | S_Times =>
      let prems = [
        !Entail(ctx(), !Ana(e1(), !Num)),
        !Entail(ctx(), !Ana(e2(), !Num)),
      ];
      let concl = !Entail(ctx(), !Syn(!BinOp(!OpTimes, e1(), e2()), !Num));
      {concl, prems};
    | T_Times =>
      let prems = [
        !Entail(ctx(), !HasTy(e1(), !Num)),
        !Entail(ctx(), !HasTy(e2(), !Num)),
      ];
      let concl =
        !Entail(ctx(), !HasTy(!BinOp(!OpTimes, e1(), e2()), !Num));
      {concl, prems};
    | E_Times =>
      let prems = [!Eval(e1(), !NumLit(1)), !Eval(e2(), !NumLit(7))];
      let concl = !Eval(!BinOp(!OpTimes, e1(), e2()), n());
      {concl, prems};
    | S_Lt =>
      let prems = [
        !Entail(ctx(), !Ana(e1(), !Num)),
        !Entail(ctx(), !Ana(e2(), !Num)),
      ];
      let concl = !Entail(ctx(), !Syn(!BinOp(!OpLt, e1(), e2()), !Bool));
      {concl, prems};
    | T_Lt =>
      let prems = [
        !Entail(ctx(), !HasTy(e1(), !Num)),
        !Entail(ctx(), !HasTy(e2(), !Num)),
      ];
      let concl = !Entail(ctx(), !HasTy(!BinOp(!OpLt, e1(), e2()), !Bool));
      {concl, prems};
    | E_Lt_T =>
      let prems = [!Eval(e1(), !NumLit(2)), !Eval(e2(), !NumLit(4))];
      let concl = !Eval(!BinOp(!OpLt, e1(), e2()), !True);
      {concl, prems};
    | E_Lt_F =>
      let prems = [!Eval(e1(), !NumLit(7)), !Eval(e2(), !NumLit(5))];
      let concl = !Eval(!BinOp(!OpLt, e1(), e2()), !False);
      {concl, prems};
    | S_Gt =>
      let prems = [
        !Entail(ctx(), !Ana(e1(), !Num)),
        !Entail(ctx(), !Ana(e2(), !Num)),
      ];
      let concl = !Entail(ctx(), !Syn(!BinOp(!OpGt, e1(), e2()), !Bool));
      {concl, prems};
    | T_Gt =>
      let prems = [
        !Entail(ctx(), !HasTy(e1(), !Num)),
        !Entail(ctx(), !HasTy(e2(), !Num)),
      ];
      let concl = !Entail(ctx(), !HasTy(!BinOp(!OpGt, e1(), e2()), !Bool));
      {concl, prems};
    | E_Gt_T =>
      let prems = [!Eval(e1(), !NumLit(5)), !Eval(e2(), !NumLit(3))];
      let concl = !Eval(!BinOp(!OpGt, e1(), e2()), !True);
      {concl, prems};
    | E_Gt_F =>
      let prems = [!Eval(e1(), !NumLit(2)), !Eval(e2(), !NumLit(7))];
      let concl = !Eval(!BinOp(!OpGt, e1(), e2()), !False);
      {concl, prems};
    | S_Eq =>
      let prems = [
        !Entail(ctx(), !Ana(e1(), !Num)),
        !Entail(ctx(), !Ana(e2(), !Num)),
      ];
      let concl = !Entail(ctx(), !Syn(!BinOp(!OpEq, e1(), e2()), !Bool));
      {concl, prems};
    | T_Eq =>
      let prems = [
        !Entail(ctx(), !HasTy(e1(), !Num)),
        !Entail(ctx(), !HasTy(e2(), !Num)),
      ];
      let concl = !Entail(ctx(), !HasTy(!BinOp(!OpEq, e1(), e2()), !Bool));
      {concl, prems};
    | E_Eq_T =>
      let prems = [!Eval(e1(), n()), !Eval(e2(), n())];
      let concl = !Eval(!BinOp(!OpEq, e1(), e2()), !True);
      {concl, prems};
    | E_Eq_F =>
      let prems = [!Eval(e1(), n()), !Eval(e2(), !NumLit(8))];
      let concl = !Eval(!BinOp(!OpEq, e1(), e2()), !False);
      {concl, prems};
    | S_If =>
      let prems = [
        !Entail(ctx(), !Ana(e1(), !Bool)),
        !Entail(ctx(), !Syn(e2(), t())),
        !Entail(ctx(), !Syn(e3(), t())),
      ];
      let concl = !Entail(ctx(), !Syn(!If(e1(), e2(), e3()), t()));
      {concl, prems};
    | A_If =>
      let prems = [
        !Entail(ctx(), !Ana(e1(), !Bool)),
        !Entail(ctx(), !Ana(e2(), t())),
        !Entail(ctx(), !Ana(e3(), t())),
      ];
      let concl = !Entail(ctx(), !Ana(!If(e1(), e2(), e3()), t()));
      {concl, prems};
    | T_If =>
      let prems = [
        !Entail(ctx(), !HasTy(e1(), !Bool)),
        !Entail(ctx(), !HasTy(e2(), t())),
        !Entail(ctx(), !HasTy(e3(), t())),
      ];
      let concl = !Entail(ctx(), !HasTy(!If(e1(), e2(), e3()), t()));
      {concl, prems};
    | E_If_T =>
      let prems = [!Eval(e1(), !True), !Eval(e2(), v2())];
      let concl = !Eval(!If(e1(), e2(), e3()), v2());
      {concl, prems};
    | E_If_F =>
      let prems = [!Eval(e1(), !False), !Eval(e3(), v3())];
      let concl = !Eval(!If(e1(), e2(), e3()), v3());
      {concl, prems};
    | S_Var =>
      let concl =
        !Entail(!Ctx([!HasTy(x(), t1()), !Var("...")]), !Syn(x(), t1()));
      {concl, prems: []};
    | T_Var =>
      let concl =
        !
          Entail(
            !Ctx([!HasTy(x(), t1()), !Var("...")]),
            !HasTy(x(), t1()),
          );
      {concl, prems: []};
    | S_LetAnn =>
      let prems = [
        !Entail(ctx(), !Syn(e1(), t1())),
        !Entail(ctx_x(), !Syn(e2(), t())),
      ];
      let concl =
        !Entail(ctx(), !Syn(!LetAnn(xp(), t1(), e1(), e2()), t()));
      {concl, prems};
    | A_LetAnn =>
      let prems = [
        !Entail(ctx(), !Syn(e1(), t1())),
        !Entail(ctx_x(), !Ana(e2(), t())),
      ];
      let concl =
        !Entail(ctx(), !Ana(!LetAnn(xp(), t1(), e1(), e2()), t()));
      {concl, prems};
    | T_LetAnn =>
      let prems = [
        !Entail(ctx(), !HasTy(e1(), t1())),
        !Entail(ctx_x(), !HasTy(e2(), t())),
      ];
      let concl =
        !Entail(ctx(), !HasTy(!LetAnn(xp(), t1(), e1(), e2()), t()));
      {concl, prems};
    | S_Let =>
      let prems = [
        !Entail(ctx(), !Ana(e1(), t1())),
        !Entail(ctx_x(), !Syn(e2(), t())),
      ];
      let concl = !Entail(ctx(), !Syn(!Let(xp(), e1(), e2()), t()));
      {concl, prems};
    | A_Let =>
      let prems = [
        !Entail(ctx(), !Ana(e1(), t1())),
        !Entail(ctx_x(), !Ana(e2(), t())),
      ];
      let concl = !Entail(ctx(), !Ana(!Let(xp(), e1(), e2()), t()));
      {concl, prems};
    | T_Let =>
      let prems = [
        !Entail(ctx(), !HasTy(e1(), t1())),
        !Entail(ctx_x(), !HasTy(e2(), t())),
      ];
      let concl = !Entail(ctx(), !HasTy(!Let(xp(), e1(), e2()), t()));
      {concl, prems};
    | E_Let =>
      let prems = [!Eval(e1(), v1()), !Eval(ex(), v2())];
      let concl = !Eval(!Let(xp(), e1(), e2()), v2());
      {concl, prems};
    | S_FunAnn =>
      let prems = [!Entail(ctx_x(), !Syn(e1(), t2()))];
      let concl =
        !
          Entail(
            ctx(),
            !Syn(!FunAnn(xp(), t1(), e1()), !Arrow(t1(), t2())),
          );
      {concl, prems};
    | A_FunAnn =>
      let prems = [!Entail(ctx_x(), !Ana(e1(), t2()))];
      let concl =
        !
          Entail(
            ctx(),
            !Ana(!FunAnn(xp(), t1(), e1()), !Arrow(t1(), t2())),
          );
      {concl, prems};
    | T_FunAnn =>
      let prems = [!Entail(ctx_x(), !HasTy(e1(), t2()))];
      let concl =
        !
          Entail(
            ctx(),
            !HasTy(!FunAnn(xp(), t1(), e1()), !Arrow(t1(), t2())),
          );
      {concl, prems};
    | A_Fun =>
      let prems = [!Entail(ctx_x(), !Ana(e1(), t2()))];
      let concl =
        !Entail(ctx(), !Ana(!Fun(xp(), e1()), !Arrow(t1(), t2())));
      {concl, prems};
    | T_Fun =>
      let prems = [!Entail(ctx_x(), !HasTy(e1(), t2()))];
      let concl =
        !Entail(ctx(), !HasTy(!Fun(xp(), e1()), !Arrow(t1(), t2())));
      {concl, prems};
    | V_Fun =>
      let concl = !Val(!Fun(xp(), e()));
      {concl, prems: []};
    | T_Fix =>
      let prems = [!Entail(ctx_x(), !HasTy(e(), t1()))];
      let concl = !Entail(ctx(), !HasTy(!Fix(xp(), e()), t1()));
      {concl, prems};
    | T_FixAnn =>
      let prems = [!Entail(ctx_x(), !HasTy(e(), t1()))];
      let concl = !Entail(ctx(), !HasTy(!FixAnn(xp(), t1(), e()), t1()));
      {concl, prems};
    | E_Fix =>
      let prems = [!Eval(ex(), v())];
      let concl = !Eval(!Fix(xp(), e1()), v());
      {concl, prems};
    | S_Ap =>
      let prems = [
        !Entail(ctx(), !Syn(e1(), !Arrow(t1(), t2()))),
        !Entail(ctx(), !Ana(e2(), t1())),
      ];
      let concl = !Entail(ctx(), !Syn(!Ap(e1(), e2()), t2()));
      {concl, prems};
    | T_Ap =>
      let prems = [
        !Entail(ctx(), !HasTy(e1(), !Arrow(t1(), t2()))),
        !Entail(ctx(), !HasTy(e2(), t1())),
      ];
      let concl = !Entail(ctx(), !HasTy(!Ap(e1(), e2()), t2()));
      {concl, prems};
    | E_Ap =>
      let prems = [
        !Eval(e1(), !Fun(xp(), e())),
        !Eval(e2(), v1()),
        !Eval(ex(), v()),
      ];
      let concl = !Eval(!Ap(e1(), e2()), v());
      {concl, prems};
    | S_Pair =>
      let prems = [
        !Entail(ctx(), !Syn(e1(), t1())),
        !Entail(ctx(), !Syn(e2(), t2())),
      ];
      let concl =
        !Entail(ctx(), !Syn(!Pair(e1(), e2()), !Prod(t1(), t2())));
      {concl, prems};
    | A_Pair =>
      let prems = [
        !Entail(ctx(), !Ana(e1(), t1())),
        !Entail(ctx(), !Ana(e2(), t2())),
      ];
      let concl =
        !Entail(ctx(), !Ana(!Pair(e1(), e2()), !Prod(t1(), t2())));
      {concl, prems};
    | T_Pair =>
      let prems = [
        !Entail(ctx(), !HasTy(e1(), t1())),
        !Entail(ctx(), !HasTy(e2(), t2())),
      ];
      let concl =
        !Entail(ctx(), !HasTy(!Pair(e1(), e2()), !Prod(t1(), t2())));
      {concl, prems};
    | E_Pair =>
      let prems = [!Eval(e1(), v1()), !Eval(e2(), v2())];
      let concl = !Eval(!Pair(e1(), e2()), !Pair(v1(), v2()));
      {concl, prems};
    | V_Pair =>
      let prems = [!Val(v1()), !Val(v2())];
      let concl = !Val(!Pair(v1(), v2()));
      {concl, prems};
    | S_LetPair =>
      let prems = [
        !Entail(ctx(), !Syn(e1(), !Prod(t1(), t2()))),
        !Entail(ctx_xy(), !Syn(e2(), t())),
      ];
      let concl =
        !Entail(ctx(), !Syn(!LetPair(xp(), yp(), e1(), e2()), t()));
      {concl, prems};
    | A_LetPair =>
      let prems = [
        !Entail(ctx(), !Syn(e1(), !Prod(t1(), t2()))),
        !Entail(ctx_xy(), !Ana(e2(), t())),
      ];
      let concl =
        !Entail(ctx(), !Ana(!LetPair(xp(), yp(), e1(), e2()), t()));
      {concl, prems};
    | T_LetPair =>
      let prems = [
        !Entail(ctx(), !HasTy(e1(), !Prod(t1(), t2()))),
        !Entail(ctx_xy(), !HasTy(e2(), t())),
      ];
      let concl =
        !Entail(ctx(), !HasTy(!LetPair(xp(), yp(), e1(), e2()), t()));
      {concl, prems};
    | E_LetPair =>
      let prems = [
        !Eval(e1(), !Pair(v1(), v2())),
        !Eval(!Var("[v2()/y][v1()/x()]e1"), v()),
      ];
      let concl = !Eval(!LetPair(xp(), yp(), e1(), e2()), v());
      {concl, prems};
    | S_PrjL =>
      let prems = [!Entail(ctx(), !Syn(e(), !Prod(t1(), t2())))];
      let concl = !Entail(ctx(), !Syn(!PrjL(e()), t1()));
      {concl, prems};
    | T_PrjL =>
      let prems = [!Entail(ctx(), !HasTy(e(), !Prod(t1(), t2())))];
      let concl = !Entail(ctx(), !HasTy(!PrjL(e()), t1()));
      {concl, prems};
    | E_PrjL =>
      let prems = [!Eval(e(), !Pair(v1(), v2()))];
      let concl = !Eval(!PrjL(e()), v1());
      {concl, prems};
    | S_PrjR =>
      let prems = [!Entail(ctx(), !Syn(e(), !Prod(t1(), t2())))];
      let concl = !Entail(ctx(), !Syn(!PrjR(e()), t2()));
      {concl, prems};
    | T_PrjR =>
      let prems = [!Entail(ctx(), !HasTy(e(), !Prod(t1(), t2())))];
      let concl = !Entail(ctx(), !HasTy(!PrjR(e()), t2()));
      {concl, prems};
    | E_PrjR =>
      let prems = [!Eval(e(), !Pair(v1(), v2()))];
      let concl = !Eval(!PrjR(e()), v2());
      {concl, prems};
    | A_InjL =>
      let prems = [!Entail(ctx(), !Ana(e(), t1()))];
      let concl = !Entail(ctx(), !Ana(!InjL(e()), !Sum(t1(), t2())));
      {concl, prems};
    | T_InjL =>
      let prems = [!Entail(ctx(), !HasTy(e(), t1()))];
      let concl = !Entail(ctx(), !HasTy(!InjL(e()), !Sum(t1(), t2())));
      {concl, prems};
    | E_InjL =>
      let prems = [!Eval(e(), v())];
      let concl = !Eval(!InjL(e()), !InjL(v()));
      {concl, prems};
    | V_InjL =>
      let prems = [!Val(v())];
      let concl = !Val(!InjL(v()));
      {concl, prems};
    | A_InjR =>
      let prems = [!Entail(ctx(), !Ana(e(), t2()))];
      let concl = !Entail(ctx(), !Ana(!InjR(e()), !Sum(t1(), t2())));
      {concl, prems};
    | T_InjR =>
      let prems = [!Entail(ctx(), !HasTy(e(), t2()))];
      let concl = !Entail(ctx(), !HasTy(!InjR(e()), !Sum(t1(), t2())));
      {concl, prems};
    | E_InjR =>
      let prems = [!Eval(e(), v())];
      let concl = !Eval(!InjR(e()), !InjR(v()));
      {concl, prems};
    | V_InjR =>
      let prems = [!Val(v())];
      let concl = !Val(!InjR(v()));
      {concl, prems};
    | A_Case =>
      let prems = [
        !Entail(ctx(), !Syn(e(), !Sum(t1(), t2()))),
        !Entail(ctx_x(), !Ana(e1(), t())),
        !Entail(ctx_y(), !Ana(e2(), t())),
      ];
      let concl =
        !Entail(ctx(), !Ana(!Case(e(), xp(), e1(), yp(), e2()), t()));
      {concl, prems};
    | S_Case =>
      let prems = [
        !Entail(ctx(), !Syn(e(), !Sum(t1(), t2()))),
        !Entail(ctx_x(), !Syn(e1(), t())),
        !Entail(ctx_y(), !Syn(e2(), t())),
      ];
      let concl =
        !Entail(ctx(), !Syn(!Case(e(), xp(), e1(), yp(), e2()), t()));
      {concl, prems};
    | T_Case =>
      let prems = [
        !Entail(ctx(), !HasTy(e(), !Sum(t1(), t2()))),
        !Entail(ctx_x(), !HasTy(e1(), t())),
        !Entail(ctx_y(), !HasTy(e2(), t())),
      ];
      let concl =
        !Entail(ctx(), !HasTy(!Case(e(), xp(), e1(), yp(), e2()), t()));
      {concl, prems};
    | E_Case_L =>
      let prems = [!Eval(e(), !InjL(v1())), !Eval(ex(), v())];
      let concl = !Eval(!Case(e(), xp(), e1(), yp(), e2()), v());
      {concl, prems};
    | E_Case_R =>
      let prems = [
        !Eval(e(), !InjR(v2())),
        !Eval(!Var("[v2()/y]e2"), v()),
      ];
      let concl = !Eval(!Case(e(), xp(), e1(), yp(), e2()), v());
      {concl, prems};
    | T_Roll =>
      let prems = [
        !Entail(ctx(), !HasTy(e(), !Var("[rec a() is t()/a()]t"))),
      ];
      let concl = !Entail(ctx(), !HasTy(!Roll(e()), !Rec(a(), t())));
      {concl, prems};
    | E_Roll =>
      let prems = [!Eval(e(), v())];
      let concl = !Eval(!Roll(e()), !Roll(v()));
      {concl, prems};
    | V_Roll =>
      let prems = [!Val(v())];
      let concl = !Val(!Roll(v()));
      {concl, prems};
    | T_Unroll =>
      let prems = [!Entail(ctx(), !HasTy(e(), !Rec(a(), t())))];
      let concl =
        !
          Entail(
            ctx(),
            !HasTy(!Unroll(e()), !Var("[rec a() is t()/a()]t")),
          );
      {concl, prems};
    | E_Unroll =>
      let prems = [!Eval(e(), !Roll(v()))];
      let concl = !Eval(!Unroll(e()), v());
      {concl, prems};
    | Assumption =>
      let concl = !Entail(!Ctx([!Var("Γ"), !TVar("p")]), p());
      {concl, prems: []};
    | And_I =>
      let prems = [!Entail(ctx(), a()), !Entail(ctx(), b())];
      let concl = !Entail(ctx(), !And(a(), b()));
      {concl, prems};
    | And_E_L =>
      let prems = [!Entail(ctx(), !And(a(), b()))];
      let concl = !Entail(ctx(), a());
      {concl, prems};
    | And_E_R =>
      let prems = [!Entail(ctx(), !And(a(), b()))];
      let concl = !Entail(ctx(), b());
      {concl, prems};
    | Or_I_L =>
      let prems = [!Entail(ctx(), a())];
      let concl = !Entail(ctx(), !Or(a(), b()));
      {concl, prems};
    | Or_I_R =>
      let prems = [!Entail(ctx(), b())];
      let concl = !Entail(ctx(), !Or(a(), b()));
      {concl, prems};
    | Or_E =>
      let prems = [
        !Entail(ctx(), !Or(a(), b())),
        !Entail(ctx_a(), c()),
        !Entail(ctx_b(), c()),
      ];
      let concl = !Entail(ctx(), c());
      {concl, prems};
    | Implies_I =>
      let prems = [!Entail(ctx_a(), b())];
      let concl = !Entail(ctx(), !Impl(a(), b()));
      {concl, prems};
    | Implies_E =>
      let prems = [!Entail(ctx(), !Impl(a(), b())), !Entail(ctx(), a())];
      let concl = !Entail(ctx(), b());
      {concl, prems};
    | Truth_I =>
      let concl = !Entail(ctx(), !Truth);
      {concl, prems: []};
    | Falsity_E =>
      let prems = [!Entail(ctx(), !Falsity)];
      let concl = !Entail(ctx(), p());
      {concl, prems};
    };
  };
