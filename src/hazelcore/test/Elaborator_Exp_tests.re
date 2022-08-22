/* Logs.set_reporter(Log.reporter(Format.std_formatter)); */
/* Logs.set_level(Some(Logs.Warning)); */

let elaborator_exp_syn_elab_simple =
    (
      ~ctx: Context.t=InitialContext.ctx,
      ~delta: Delta.t=Delta.empty,
      e: UHExp.t,
      want_d: DHExp.t,
      want_ty: HTyp.t,
    )
    : bool => {
  let got = Elaborator_Exp.syn_elab(ctx, delta, e);
  let result =
    switch (got) {
    | DoesNotElaborate => false
    | Elaborates(got_d, got_ty, _) => got_d == want_d && got_ty == want_ty
    };
  if (!result) {
    Format.printf(
      "\nERROR in %s\n\ninput:\n%s\n\nwant d:\n%s\n\nwant ty:\n%s\n\ngot:\n%s\n\n",
      __FUNCTION__,
      Sexplib.Sexp.to_string_hum(UHExp.sexp_of_t(e)),
      Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(want_d)),
      Sexplib.Sexp.to_string_hum(HTyp.sexp_of_t(want_ty)),
      Sexplib.Sexp.to_string_hum(
        Elaborator_Exp.ElaborationResult.sexp_of_t(got),
      ),
    );
  };
  result;
};

let elaborator_exp_ana_elab_simple =
    (
      ~ctx: Context.t=InitialContext.ctx,
      ~delta: Delta.t=Delta.empty,
      e: UHExp.t,
      want_d: DHExp.t,
      ty: HTyp.t,
    )
    : bool => {
  let got = Elaborator_Exp.ana_elab(ctx, delta, e, ty);
  let result =
    switch (got) {
    | DoesNotElaborate => false
    | Elaborates(got_d, got_ty, _) =>
      got_d == want_d && HTyp.consistent(ctx, got_ty, ty)
    };
  if (!result) {
    Format.printf(
      "\nERROR in %s\n\ninput:\n%s\n\nwant d:\n%s\n\nty:\n%s\n\ngot:\n%s\n\n",
      __FUNCTION__,
      Sexplib.Sexp.to_string_hum(UHExp.sexp_of_t(e)),
      Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(want_d)),
      Sexplib.Sexp.to_string_hum(HTyp.sexp_of_t(ty)),
      Sexplib.Sexp.to_string_hum(
        Elaborator_Exp.ElaborationResult.sexp_of_t(got),
      ),
    );
  };
  result;
};

let id_gen0 = IDGen.init;
let (u0, id_gen1) = IDGen.next_hole(id_gen0);
let (u1, id_gen2) = IDGen.next_hole(id_gen1);

/* Simple Forms */

/* ?  =>  ?  :  ? */
let%test _ =
  elaborator_exp_syn_elab_simple(
    UHExp.Block.wrap(UHExp.EmptyHole(u0)),
    DHExp.EmptyHole(u0, 0, VarMap.empty),
    HTyp.hole(),
  );

/* ?  <=  ?  :  ? */
let%test _ =
  elaborator_exp_ana_elab_simple(
    UHExp.Block.wrap(UHExp.EmptyHole(u0)),
    DHExp.EmptyHole(u0, 0, VarMap.empty),
    HTyp.hole(),
  );

/* 123  =>  123  :  Int */
let%test _ =
  elaborator_exp_syn_elab_simple(
    UHExp.Block.wrap(IntLit(NotInHole, "123")),
    DHExp.IntLit(123),
    HTyp.int(),
  );

/* 123  <=  Int */
let%test _ =
  elaborator_exp_ana_elab_simple(
    UHExp.Block.wrap(IntLit(NotInHole, "123")),
    DHExp.IntLit(123),
    HTyp.int(),
  );

/* 1.2  =>  Float */
let%test _ =
  elaborator_exp_syn_elab_simple(
    UHExp.Block.wrap(FloatLit(NotInHole, "1.2")),
    DHExp.FloatLit(1.2),
    HTyp.float(),
  );

/* 1.2  <=  Float */
let%test _ =
  elaborator_exp_ana_elab_simple(
    UHExp.Block.wrap(FloatLit(NotInHole, "1.2")),
    DHExp.FloatLit(1.2),
    HTyp.float(),
  );

/* true  =>  Bool */
let%test _ =
  elaborator_exp_syn_elab_simple(
    UHExp.Block.wrap(BoolLit(NotInHole, true)),
    DHExp.BoolLit(true),
    HTyp.bool(),
  );

/* true  <=  Bool */
let%test _ =
  elaborator_exp_ana_elab_simple(
    UHExp.Block.wrap(BoolLit(NotInHole, true)),
    DHExp.BoolLit(true),
    HTyp.bool(),
  );

/* false  =>  Bool */
let%test _ =
  elaborator_exp_syn_elab_simple(
    UHExp.Block.wrap(BoolLit(NotInHole, false)),
    DHExp.BoolLit(false),
    HTyp.bool(),
  );

/* false  <=  Bool */
let%test _ =
  elaborator_exp_ana_elab_simple(
    UHExp.Block.wrap(BoolLit(NotInHole, false)),
    DHExp.BoolLit(false),
    HTyp.bool(),
  );

/* \x.?  =>  \x:?.?  :  ? -> ? */
let%test _ = {
  let p = OpSeq.wrap(UHPat.var("x"));
  let e = UHExp.Block.wrap(EmptyHole(u0));
  let ctx = Context.add_var(InitialContext.ctx, "x", HTyp.hole());
  let gamma = VarMap.extend(VarMap.empty, ("x", DHExp.BoundVar("x")));
  elaborator_exp_syn_elab_simple(
    UHExp.Block.wrap(Fun(NotInHole, p, e)),
    DHExp.Fun(DHPat.Var("x"), (ctx, HTyp.hole()), EmptyHole(u0, 0, gamma)),
    HTyp.arrow(HTyp.hole(), HTyp.hole()),
  );
};

/* \x.?  <=  \x:?.?  :  ? -> ? */
let%test _ = {
  let p = OpSeq.wrap(UHPat.var("x"));
  let e = UHExp.Block.wrap(EmptyHole(u0));
  let ctx = Context.add_var(InitialContext.ctx, "x", HTyp.hole());
  let gamma = VarMap.extend(VarMap.empty, ("x", DHExp.BoundVar("x")));
  elaborator_exp_syn_elab_simple(
    UHExp.Block.wrap(Fun(NotInHole, p, e)),
    DHExp.Fun(DHPat.Var("x"), (ctx, HTyp.hole()), EmptyHole(u0, 0, gamma)),
    HTyp.arrow(HTyp.hole(), HTyp.hole()),
  );
};

/* \x.\y.?  =>  \x:?.\y:?.?  :  ? -> (? -> ?) */
let%test _ = {
  let p_x = OpSeq.wrap(UHPat.var("x"));
  let p_y = OpSeq.wrap(UHPat.var("y"));
  let e_y = UHExp.Block.wrap(EmptyHole(u0));
  let f_y = UHExp.Block.wrap(Fun(NotInHole, p_y, e_y));
  let ctx_x = Context.add_var(InitialContext.ctx, "x", HTyp.hole());
  let ctx_y = Context.add_var(ctx_x, "y", HTyp.hole());
  let gamma = VarMap.extend(VarMap.empty, ("x", DHExp.BoundVar("x")));
  let gamma = VarMap.extend(gamma, ("y", DHExp.BoundVar("y")));
  let d_y =
    DHExp.Fun(Var("y"), (ctx_y, HTyp.hole()), EmptyHole(u0, 0, gamma));
  elaborator_exp_syn_elab_simple(
    UHExp.Block.wrap(Fun(NotInHole, p_x, f_y)),
    Fun(DHPat.Var("x"), (ctx_x, HTyp.hole()), d_y),
    HTyp.arrow(HTyp.hole(), HTyp.arrow(HTyp.hole(), HTyp.hole())),
  );
};

/* \x.\y.?  <=  \x:?.\y:?.?  :  ? -> (? -> ?) */
let%test _ = {
  let p_x = OpSeq.wrap(UHPat.var("x"));
  let p_y = OpSeq.wrap(UHPat.var("y"));
  let e_y = UHExp.Block.wrap(EmptyHole(u0));
  let f_y = UHExp.Block.wrap(Fun(NotInHole, p_y, e_y));
  let ctx_x = Context.add_var(InitialContext.ctx, "x", HTyp.hole());
  let ctx_y = Context.add_var(ctx_x, "y", HTyp.hole());
  let gamma = VarMap.extend(VarMap.empty, ("x", DHExp.BoundVar("x")));
  let gamma = VarMap.extend(gamma, ("y", DHExp.BoundVar("y")));
  let d_y =
    DHExp.Fun(Var("y"), (ctx_y, HTyp.hole()), EmptyHole(u0, 0, gamma));
  elaborator_exp_syn_elab_simple(
    UHExp.Block.wrap(Fun(NotInHole, p_x, f_y)),
    Fun(Var("x"), (ctx_x, HTyp.hole()), d_y),
    HTyp.arrow(HTyp.hole(), HTyp.arrow(HTyp.hole(), HTyp.hole())),
  );
};

/* let x : ? | ? = ? in x  =>  let x : ? | ? = ? in x  :  ? | ? */
let%test _ = {
  let ty = UHTyp.mk_OpSeq(S(Hole, A(Sum, S(Hole, E))));
  let p = UHPat.TypeAnn(NotInHole, UHPat.var("x"), ty);
  elaborator_exp_syn_elab_simple(
    [
      LetLine(OpSeq.wrap(p), UHExp.Block.wrap(EmptyHole(u0))),
      ExpLine(OpSeq.wrap(UHExp.var("x"))),
    ],
    Let(Var("x"), EmptyHole(u0, 0, VarMap.empty), BoundVar("x")),
    HTyp.sum(HTyp.hole(), HTyp.hole()),
  );
};

/* let x : ? | ? = ? in x  <=  let x : ? | ? = ? in x  :  ? | ? */
let%test _ = {
  let ty = UHTyp.mk_OpSeq(S(Hole, A(Sum, S(Hole, E))));
  let p = UHPat.TypeAnn(NotInHole, UHPat.var("x"), ty);
  elaborator_exp_ana_elab_simple(
    [
      LetLine(OpSeq.wrap(p), UHExp.Block.wrap(EmptyHole(u0))),
      ExpLine(OpSeq.wrap(UHExp.var("x"))),
    ],
    Let(Var("x"), EmptyHole(u0, 0, VarMap.empty), BoundVar("x")),
    HTyp.sum(HTyp.hole(), HTyp.hole()),
  );
};

/* ?, ?  =>  ?, ?  :  ?, ? */
let%test _ = {
  let e =
    UHExp.mk_OpSeq(S(EmptyHole(u0), A(Comma, S(EmptyHole(u1), E))));
  elaborator_exp_syn_elab_simple(
    UHExp.Block.wrap'(e),
    DHExp.Pair(
      EmptyHole(u0, 0, VarMap.empty),
      EmptyHole(u1, 0, VarMap.empty),
    ),
    HTyp.product([HTyp.hole(), HTyp.hole()]),
  );
};

/* ?, ?  <=  ?, ?  :  ?, ? */
let%test _ = {
  let e =
    UHExp.mk_OpSeq(S(EmptyHole(u0), A(Comma, S(EmptyHole(u1), E))));
  elaborator_exp_ana_elab_simple(
    UHExp.Block.wrap'(e),
    DHExp.Pair(
      EmptyHole(u0, 0, VarMap.empty),
      EmptyHole(u1, 0, VarMap.empty),
    ),
    HTyp.product([HTyp.hole(), HTyp.hole()]),
  );
};

/* []  =>  []  :  [?] */
let%test _ =
  elaborator_exp_syn_elab_simple(
    UHExp.Block.wrap(UHExp.ListNil(NotInHole)),
    DHExp.ListNil((InitialContext.ctx, HTyp.hole())),
    HTyp.list(HTyp.hole()),
  );

/* []  <=  []  :  [?] */
let%test _ =
  elaborator_exp_ana_elab_simple(
    UHExp.Block.wrap(UHExp.ListNil(NotInHole)),
    DHExp.ListNil((InitialContext.ctx, HTyp.hole())),
    HTyp.list(HTyp.hole()),
  );

/* ? :: ?  =>  [?]  :  [?] */
let%test _ = {
  let e = UHExp.mk_OpSeq(S(EmptyHole(u0), A(Cons, S(EmptyHole(u1), E))));
  elaborator_exp_syn_elab_simple(
    UHExp.Block.wrap'(e),
    DHExp.Cons(
      EmptyHole(u0, 0, VarMap.empty),
      EmptyHole(u1, 0, VarMap.empty),
    ),
    HTyp.list(HTyp.hole()),
  );
};

/* ? :: ?  <=  [?]  :  [?] */
let%test _ = {
  let e = UHExp.mk_OpSeq(S(EmptyHole(u0), A(Cons, S(EmptyHole(u1), E))));
  elaborator_exp_ana_elab_simple(
    UHExp.Block.wrap'(e),
    DHExp.Cons(
      EmptyHole(u0, 0, VarMap.empty),
      EmptyHole(u1, 0, VarMap.empty),
    ),
    HTyp.list(HTyp.hole()),
  );
};

/* 123 :: ?  =>  123 :: ?  :  [Int] */
let%test _ = {
  let e =
    UHExp.mk_OpSeq(
      S(IntLit(NotInHole, "123"), A(Cons, S(EmptyHole(u0), E))),
    );
  elaborator_exp_ana_elab_simple(
    UHExp.Block.wrap'(e),
    DHExp.Cons(IntLit(123), EmptyHole(u0, 0, VarMap.empty)),
    HTyp.list(HTyp.int()),
  );
};

/* 123 :: ?  <=  [Int] */
let%test _ = {
  let e =
    UHExp.mk_OpSeq(
      S(IntLit(NotInHole, "123"), A(Cons, S(EmptyHole(u0), E))),
    );
  elaborator_exp_ana_elab_simple(
    UHExp.Block.wrap'(e),
    DHExp.Cons(IntLit(123), EmptyHole(u0, 0, VarMap.empty)),
    HTyp.list(HTyp.int()),
  );
};

/* type ? = ? in ?  =>  type ? = ? in ?  :  ? */
let%test _ = {
  let tp = TPat.EmptyHole;
  let ty = UHTyp.mk_OpSeq(S(Hole, E));
  elaborator_exp_syn_elab_simple(
    [TyAliasLine(tp, ty), ExpLine(OpSeq.wrap(UHExp.EmptyHole(u0)))],
    TyAlias(
      tp,
      (InitialContext.ctx, HTyp.hole()),
      EmptyHole(u0, 0, VarMap.empty),
    ),
    HTyp.hole(),
  );
};

/* type ? = ? in ?  <=  type ? = ? in ?  :  ? */
let%test _ = {
  let tp = TPat.EmptyHole;
  let ty = UHTyp.mk_OpSeq(S(Hole, E));
  elaborator_exp_ana_elab_simple(
    [TyAliasLine(tp, ty), ExpLine(OpSeq.wrap(UHExp.EmptyHole(u0)))],
    TyAlias(
      tp,
      (InitialContext.ctx, HTyp.hole()),
      EmptyHole(u0, 0, VarMap.empty),
    ),
    HTyp.hole(),
  );
};

/* type t = ? in ?  =>  type t = ? in ?  :  ? */
let%test _ = {
  let tp = TPat.TyVar(NotInHole, "t");
  let ty = UHTyp.mk_OpSeq(S(Hole, E));
  elaborator_exp_syn_elab_simple(
    [TyAliasLine(tp, ty), ExpLine(OpSeq.wrap(UHExp.EmptyHole(u0)))],
    TyAlias(
      tp,
      (InitialContext.ctx, HTyp.hole()),
      EmptyHole(u0, 0, VarMap.empty),
    ),
    HTyp.hole(),
  );
};

/* type t = ? in ?  <=  type t = ? in ?  :  ? */
let%test _ = {
  let tp = TPat.TyVar(NotInHole, "t");
  let ty = UHTyp.mk_OpSeq(S(Hole, E));
  elaborator_exp_ana_elab_simple(
    [TyAliasLine(tp, ty), ExpLine(OpSeq.wrap(UHExp.EmptyHole(u0)))],
    TyAlias(
      tp,
      (InitialContext.ctx, HTyp.hole()),
      EmptyHole(u0, 0, VarMap.empty),
    ),
    HTyp.hole(),
  );
};

/* type t = ? in let x : t = ? in x  =>  type t = ? in let x : t = ? in x  :  ? */
let%test _ = {
  let tp = TPat.TyVar(NotInHole, "t");
  let ty = UHTyp.mk_OpSeq(S(Hole, E));
  let p = UHPat.mk_OpSeq(S(TypeAnn(NotInHole, UHPat.var("x"), ty), E));
  let ctx = InitialContext.ctx;
  elaborator_exp_syn_elab_simple(
    [
      TyAliasLine(tp, ty),
      LetLine(p, UHExp.Block.wrap(UHExp.EmptyHole(u0))),
      ExpLine(UHExp.mk_OpSeq(S(UHExp.var("x"), E))),
    ],
    TyAlias(
      tp,
      (ctx, HTyp.hole()),
      DHExp.Let(
        DHPat.Var("x"),
        DHExp.EmptyHole(u0, 0, VarMap.empty),
        DHExp.BoundVar("x"),
      ),
    ),
    HTyp.hole(),
  );
};

/* type t = ? in let x : t = ? in x  <=  type t = ? in let x = ? in x  :  ? */
let%test _ = {
  let tp = TPat.TyVar(NotInHole, "t");
  let ty = UHTyp.mk_OpSeq(S(Hole, E));
  let p = UHPat.mk_OpSeq(S(TypeAnn(NotInHole, UHPat.var("x"), ty), E));
  elaborator_exp_ana_elab_simple(
    [
      TyAliasLine(tp, ty),
      LetLine(p, UHExp.Block.wrap(UHExp.EmptyHole(u0))),
      ExpLine(UHExp.mk_OpSeq(S(UHExp.var("x"), E))),
    ],
    TyAlias(
      tp,
      (InitialContext.ctx, HTyp.hole()),
      Let(Var("x"), EmptyHole(u0, 0, VarMap.empty), BoundVar("x")),
    ),
    HTyp.hole(),
  );
};

/* type t = Int in let x : t = ? in x  =>  type t = Int in let x = ? in x  :  Int */
let%test _ = {
  let tp = TPat.TyVar(NotInHole, "t");
  let ty = UHTyp.mk_OpSeq(S(Int, E));
  let p = UHPat.mk_OpSeq(S(TypeAnn(NotInHole, UHPat.var("x"), ty), E));
  elaborator_exp_syn_elab_simple(
    UHExp.[
      TyAliasLine(tp, ty),
      LetLine(p, UHExp.Block.wrap(UHExp.EmptyHole(u0))),
      ExpLine(UHExp.mk_OpSeq(S(UHExp.var("x"), E))),
    ],
    TyAlias(
      tp,
      (InitialContext.ctx, HTyp.int()),
      Let(Var("x"), EmptyHole(u0, 0, VarMap.empty), BoundVar("x")),
    ),
    HTyp.int(),
  );
};

/* type t = Int in let x : t = ? in x  <=  type t = Int in let x = ? in x  :  Int */
let%test _ = {
  let tp = TPat.TyVar(NotInHole, "t");
  let ty = UHTyp.mk_OpSeq(S(Int, E));
  let p = UHPat.mk_OpSeq(S(TypeAnn(NotInHole, UHPat.var("x"), ty), E));
  elaborator_exp_ana_elab_simple(
    UHExp.[
      TyAliasLine(tp, ty),
      LetLine(p, UHExp.Block.wrap(UHExp.EmptyHole(u0))),
      ExpLine(UHExp.mk_OpSeq(S(UHExp.var("x"), E))),
    ],
    TyAlias(
      tp,
      (InitialContext.ctx, HTyp.int()),
      Let(Var("x"), EmptyHole(u0, 0, VarMap.empty), BoundVar("x")),
    ),
    HTyp.int(),
  );
};

/* type t = 123 in ?  =>  type t = 123 in ?  : ? */
let%test _ = {
  let ty = UHTyp.mk_OpSeq(S(InvalidText(u0, "123"), E));
  elaborator_exp_syn_elab_simple(
    UHExp.[
      TyAliasLine(TPat.TyVar(NotInHole, "t"), ty),
      ExpLine(UHExp.mk_OpSeq(S(EmptyHole(u1), E))),
    ],
    TyAlias(
      TPat.TyVar(NotInHole, "t"),
      (InitialContext.ctx, HTyp.invalid_text(u0, "123")),
      EmptyHole(u1, 0, VarMap.empty),
    ),
    HTyp.hole(),
  );
};

/* type t = 123 in ?  <=  type t = 123 in ?  : ? */
let%test _ = {
  let tp = TPat.TyVar(NotInHole, "t");
  let ty = UHTyp.mk_OpSeq(S(InvalidText(u0, "123"), E));
  elaborator_exp_ana_elab_simple(
    UHExp.[
      TyAliasLine(tp, ty),
      ExpLine(UHExp.mk_OpSeq(S(EmptyHole(u1), E))),
    ],
    TyAlias(
      tp,
      (InitialContext.ctx, HTyp.invalid_text(u0, "123")),
      EmptyHole(u1, 0, VarMap.empty),
    ),
    HTyp.hole(),
  );
};

/* type t = type in 123   =>  type t = type in 123  :  Int */
let%test _ = {
  let tp = TPat.TyVar(NotInHole, "t");
  let ty = UHTyp.mk_OpSeq(S(TyVar(InHole(Reserved, u0), "type"), E));
  elaborator_exp_syn_elab_simple(
    UHExp.[
      TyAliasLine(tp, ty),
      ExpLine(UHExp.mk_OpSeq(S(IntLit(NotInHole, "123"), E))),
    ],
    TyAlias(
      tp,
      (InitialContext.ctx, HTyp.tyvarhole(Reserved, u0, "type")),
      IntLit(123),
    ),
    HTyp.int(),
  );
};

/* type t = type in 123   <=  type t = type in 123  :  Int */
let%test _ = {
  let tp = TPat.TyVar(NotInHole, "t");
  let ty = UHTyp.mk_OpSeq(S(TyVar(InHole(Reserved, u0), "type"), E));
  elaborator_exp_ana_elab_simple(
    UHExp.[
      TyAliasLine(tp, ty),
      ExpLine(UHExp.mk_OpSeq(S(IntLit(NotInHole, "123"), E))),
    ],
    TyAlias(
      tp,
      (InitialContext.ctx, HTyp.tyvarhole(Reserved, u0, "type")),
      IntLit(123),
    ),
    HTyp.int(),
  );
};

/* let x : t = ? in x  =>  let x = ? in x  :  t */
let%test _ = {
  let ty = UHTyp.mk_OpSeq(S(TyVar(InHole(Unbound, u0), "t"), E));
  let p = UHPat.mk_OpSeq(S(TypeAnn(NotInHole, UHPat.var("x"), ty), E));
  elaborator_exp_syn_elab_simple(
    UHExp.[
      LetLine(p, UHExp.Block.wrap(UHExp.EmptyHole(u1))),
      ExpLine(UHExp.mk_OpSeq(S(UHExp.var("x"), E))),
    ],
    DHExp.Let(Var("x"), EmptyHole(u1, 0, VarMap.empty), BoundVar("x")),
    HTyp.tyvarhole(Unbound, u0, "t"),
  );
};

/* let x : t = ? in x  <=  let x = ? in x  :  t */
let%test _ = {
  let ty = UHTyp.mk_OpSeq(S(TyVar(InHole(Unbound, u0), "t"), E));
  let p = UHPat.mk_OpSeq(S(TypeAnn(NotInHole, UHPat.var("x"), ty), E));
  elaborator_exp_ana_elab_simple(
    UHExp.[
      LetLine(p, UHExp.Block.wrap(UHExp.EmptyHole(u1))),
      ExpLine(UHExp.mk_OpSeq(S(UHExp.var("x"), E))),
    ],
    DHExp.Let(Var("x"), EmptyHole(u1, 0, VarMap.empty), BoundVar("x")),
    HTyp.tyvarhole(Unbound, u0, "t"),
  );
};
