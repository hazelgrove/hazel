/* Logs.set_reporter(Log.reporter(Format.std_formatter)); */
/* Logs.set_level(Some(Logs.Warning)); */

let ap_opt =
    (f: ('a, 'a) => 'bool, a_opt: option('a), b_opt: option('a)): bool =>
  switch (a_opt, b_opt) {
  | (Some(a), Some(b)) => f(a, b)
  | (None, _)
  | (_, None) => false
  };

let statics_exp_syn_simple =
    (~ctx: Context.t=InitialContext.ctx, e: UHExp.t, want_ty: HTyp.t): bool => {
  let want = Some(want_ty);
  let got = Statics_Exp.syn(ctx, e);
  let result = ap_opt(HTyp.equivalent(ctx), want, got);
  if (!result) {
    Format.printf(
      "\nERROR in %s\n\ninput:\n%s\n\nwant:\n%s\n\ngot:\n%s\n\n",
      __FUNCTION__,
      Sexplib.Sexp.to_string_hum(UHExp.sexp_of_t(e)),
      Sexplib.Sexp.to_string_hum(
        Sexplib.Std.sexp_of_option(HTyp.sexp_of_t, want),
      ),
      Sexplib.Sexp.to_string_hum(
        Sexplib.Std.sexp_of_option(HTyp.sexp_of_t, got),
      ),
    );
  };
  result;
};

let statics_exp_ana_simple =
    (~ctx: Context.t=InitialContext.ctx, e: UHExp.t, ty: HTyp.t): bool => {
  let got = Statics_Exp.ana(ctx, e, ty);
  let result = ap_opt((==), got, Some());
  if (!result) {
    Format.printf(
      "\nERROR in %s:\n%s\n\ngot:\n%s\n\n",
      __FUNCTION__,
      Sexplib.Sexp.to_string_hum(UHExp.sexp_of_t(e)),
      Sexplib.Sexp.to_string_hum(
        Sexplib.Std.sexp_of_option(Sexplib.Std.sexp_of_unit, got),
      ),
    );
  };
  result;
};

let id_gen0 = IDGen.init;
let (u0, id_gen1) = IDGen.next_hole(id_gen0);
let (u1, id_gen2) = IDGen.next_hole(id_gen1);

/* Simple Forms */

/* ?  =>  ? */
let%test _ =
  statics_exp_syn_simple(UHExp.Block.wrap(EmptyHole(u0)), HTyp.hole());

/* ?  <=  ? */
let%test _ =
  statics_exp_ana_simple(UHExp.Block.wrap(EmptyHole(u0)), HTyp.hole());

/* 123  =>  Int */
let%test _ =
  statics_exp_syn_simple(
    UHExp.Block.wrap(IntLit(NotInHole, "123")),
    HTyp.int(),
  );

/* 123  <=  Int */
let%test _ =
  statics_exp_ana_simple(
    UHExp.Block.wrap(IntLit(NotInHole, "123")),
    HTyp.int(),
  );

/* 1.2  =>  Float */
let%test _ =
  statics_exp_syn_simple(
    UHExp.Block.wrap(FloatLit(NotInHole, "1.2")),
    HTyp.float(),
  );

/* 1.2  <=  Float */
let%test _ =
  statics_exp_ana_simple(
    UHExp.Block.wrap(FloatLit(NotInHole, "1.2")),
    HTyp.float(),
  );

/* true  =>  Bool */
let%test _ =
  statics_exp_syn_simple(
    UHExp.Block.wrap(BoolLit(NotInHole, true)),
    HTyp.bool(),
  );

/* true  <=  Bool */
let%test _ =
  statics_exp_ana_simple(
    UHExp.Block.wrap(BoolLit(NotInHole, true)),
    HTyp.bool(),
  );

/* false  =>  Bool */
let%test _ =
  statics_exp_syn_simple(
    UHExp.Block.wrap(BoolLit(NotInHole, false)),
    HTyp.bool(),
  );

/* false  <=  Bool */
let%test _ =
  statics_exp_ana_simple(
    UHExp.Block.wrap(BoolLit(NotInHole, false)),
    HTyp.bool(),
  );

/* \x.?  =>  ? -> ? */
let%test _ = {
  let p = OpSeq.wrap(UHPat.var("x"));
  let e = UHExp.Block.wrap(EmptyHole(u0));
  let f = UHExp.Block.wrap(Fun(NotInHole, p, e));
  statics_exp_syn_simple(f, HTyp.arrow(HTyp.hole(), HTyp.hole()));
};

/* \x.?  <=  ? -> ? */
let%test _ = {
  let p = OpSeq.wrap(UHPat.var("x"));
  let e = UHExp.Block.wrap(EmptyHole(u0));
  let f = UHExp.Block.wrap(Fun(NotInHole, p, e));
  statics_exp_ana_simple(f, HTyp.arrow(HTyp.hole(), HTyp.hole()));
};

/* \x.\y.?  =>  ? -> (? -> ?) */
let%test _ = {
  let p_x = OpSeq.wrap(UHPat.var("x"));
  let p_y = OpSeq.wrap(UHPat.var("y"));
  let e_y = UHExp.Block.wrap(EmptyHole(u0));
  let f_y = UHExp.Block.wrap(Fun(NotInHole, p_y, e_y));
  let f_x = UHExp.Block.wrap(Fun(NotInHole, p_x, f_y));
  statics_exp_syn_simple(
    f_x,
    HTyp.arrow(HTyp.hole(), HTyp.arrow(HTyp.hole(), HTyp.hole())),
  );
};

/* \x.\y.?  <=  ? -> (? -> ?) */
let%test _ = {
  let p_x = OpSeq.wrap(UHPat.var("x"));
  let p_y = OpSeq.wrap(UHPat.var("y"));
  let e_y = UHExp.Block.wrap(EmptyHole(u0));
  let f_y = UHExp.Block.wrap(Fun(NotInHole, p_y, e_y));
  let f_x = UHExp.Block.wrap(Fun(NotInHole, p_x, f_y));
  statics_exp_ana_simple(
    f_x,
    HTyp.arrow(HTyp.hole(), HTyp.arrow(HTyp.hole(), HTyp.hole())),
  );
};

/* let x : ? | ? = ? in x  =>  ? | ? */
let%test _ = {
  let ty = UHTyp.mk_OpSeq(S(Hole, A(Sum, S(Hole, E))));
  let p = UHPat.TypeAnn(NotInHole, UHPat.var("x"), ty);
  statics_exp_syn_simple(
    [
      LetLine(OpSeq.wrap(p), UHExp.Block.wrap(EmptyHole(u0))),
      UHExp.ExpLine(OpSeq.wrap(UHExp.var("x"))),
    ],
    HTyp.sum(HTyp.hole(), HTyp.hole()),
  );
};

/* let x : ? | ? = ? in x  <=  ? | ? */
let%test _ = {
  let ty = UHTyp.mk_OpSeq(S(Hole, A(Sum, S(Hole, E))));
  let p = UHPat.TypeAnn(NotInHole, UHPat.var("x"), ty);
  statics_exp_ana_simple(
    [
      LetLine(OpSeq.wrap(p), UHExp.Block.wrap(EmptyHole(u0))),
      UHExp.ExpLine(OpSeq.wrap(UHExp.var("x"))),
    ],
    HTyp.sum(HTyp.hole(), HTyp.hole()),
  );
};

/* ?, ?  =>  ?, ? */
let%test _ = {
  let e =
    UHExp.mk_OpSeq(S(EmptyHole(u0), A(Comma, S(EmptyHole(u1), E))));
  statics_exp_syn_simple(
    UHExp.Block.wrap'(e),
    HTyp.product([HTyp.hole(), HTyp.hole()]),
  );
};

/* ?, ?  <=  ?, ? */
let%test _ = {
  let e =
    UHExp.mk_OpSeq(S(EmptyHole(u0), A(Comma, S(EmptyHole(u1), E))));
  statics_exp_ana_simple(
    UHExp.Block.wrap'(e),
    HTyp.product([HTyp.hole(), HTyp.hole()]),
  );
};

/* []  =>  [?] */
let%test _ =
  statics_exp_syn_simple(
    UHExp.Block.wrap(UHExp.ListNil(NotInHole)),
    HTyp.list(HTyp.hole()),
  );

/* []  <=  [?] */
let%test _ =
  statics_exp_ana_simple(
    UHExp.Block.wrap(UHExp.ListNil(NotInHole)),
    HTyp.list(HTyp.hole()),
  );

/* ? :: ?  =>  [?] */
let%test _ = {
  let e = UHExp.mk_OpSeq(S(EmptyHole(u0), A(Cons, S(EmptyHole(u1), E))));
  statics_exp_syn_simple(UHExp.Block.wrap'(e), HTyp.list(HTyp.hole()));
};

/* ? :: ?  <=  [?] */
let%test _ = {
  let e = UHExp.mk_OpSeq(S(EmptyHole(u0), A(Cons, S(EmptyHole(u1), E))));
  statics_exp_ana_simple(UHExp.Block.wrap'(e), HTyp.list(HTyp.hole()));
};

/* 123 :: ?  =>  [Int] */
let%test _ = {
  let e =
    UHExp.mk_OpSeq(
      S(IntLit(NotInHole, "123"), A(Cons, S(EmptyHole(u0), E))),
    );
  statics_exp_syn_simple(UHExp.Block.wrap'(e), HTyp.list(HTyp.int()));
};

/* 123 :: ?  <=  [Int] */
let%test _ = {
  let e =
    UHExp.mk_OpSeq(
      S(IntLit(NotInHole, "123"), A(Cons, S(EmptyHole(u0), E))),
    );
  statics_exp_ana_simple(UHExp.Block.wrap'(e), HTyp.list(HTyp.int()));
};

/* type ? = ? in ?  =>  ? */
let%test _ = {
  let tp = TPat.EmptyHole;
  let ty = UHTyp.mk_OpSeq(S(Hole, E));
  statics_exp_syn_simple(
    UHExp.[TyAliasLine(tp, ty), ExpLine(OpSeq.wrap(UHExp.EmptyHole(u0)))],
    HTyp.hole(),
  );
};

/* type ? = ? in ?  <=  ? */
let%test _ = {
  let tp = TPat.EmptyHole;
  let ty = UHTyp.mk_OpSeq(S(Hole, E));
  statics_exp_ana_simple(
    UHExp.[TyAliasLine(tp, ty), ExpLine(OpSeq.wrap(UHExp.EmptyHole(u0)))],
    HTyp.hole(),
  );
};

/* type t = ? in ?  =>  ? */
let%test _ = {
  let tp = TPat.TyVar(TPat.Status.NotInHole, "t");
  let ty = UHTyp.mk_OpSeq(S(Hole, E));
  statics_exp_syn_simple(
    UHExp.[TyAliasLine(tp, ty), ExpLine(OpSeq.wrap(UHExp.EmptyHole(u0)))],
    HTyp.hole(),
  );
};

/* type t = ? in ?  <=  ? */
let%test _ = {
  let tp = TPat.TyVar(TPat.Status.NotInHole, "t");
  let ty = UHTyp.mk_OpSeq(S(Hole, E));
  statics_exp_ana_simple(
    UHExp.[TyAliasLine(tp, ty), ExpLine(OpSeq.wrap(UHExp.EmptyHole(u0)))],
    HTyp.hole(),
  );
};

/* type t = ? in let x : t = ? in x  =>  ? */
let%test _ = {
  let ty = UHTyp.mk_OpSeq(S(Hole, E));
  let p = UHPat.mk_OpSeq(S(TypeAnn(NotInHole, UHPat.var("x"), ty), E));
  statics_exp_syn_simple(
    UHExp.[
      TyAliasLine(TPat.TyVar(NotInHole, "t"), ty),
      LetLine(p, UHExp.Block.wrap(UHExp.EmptyHole(u0))),
      ExpLine(UHExp.mk_OpSeq(S(UHExp.var("x"), E))),
    ],
    HTyp.hole(),
  );
};

/* type t = ? in let x : t = ? in x  <=  ? */
let%test _ = {
  let ty = UHTyp.mk_OpSeq(S(Hole, E));
  let p = UHPat.mk_OpSeq(S(TypeAnn(NotInHole, UHPat.var("x"), ty), E));
  statics_exp_ana_simple(
    UHExp.[
      TyAliasLine(TPat.TyVar(NotInHole, "t"), ty),
      LetLine(p, UHExp.Block.wrap(UHExp.EmptyHole(u1))),
      ExpLine(UHExp.mk_OpSeq(S(UHExp.var("x"), E))),
    ],
    HTyp.hole(),
  );
};

/* type t = Int in let x : t = ? in x  =>  Int */
let%test _ = {
  let ty = UHTyp.mk_OpSeq(S(Int, E));
  let p = UHPat.mk_OpSeq(S(TypeAnn(NotInHole, UHPat.var("x"), ty), E));
  statics_exp_syn_simple(
    UHExp.[
      TyAliasLine(TPat.TyVar(NotInHole, "t"), ty),
      LetLine(p, UHExp.Block.wrap(UHExp.EmptyHole(u1))),
      ExpLine(UHExp.mk_OpSeq(S(UHExp.var("x"), E))),
    ],
    HTyp.int(),
  );
};

/* type t = Int in let x : t = ? in x  <=  Int */
let%test _ = {
  let ty = UHTyp.mk_OpSeq(S(Int, E));
  let p = UHPat.mk_OpSeq(S(TypeAnn(NotInHole, UHPat.var("x"), ty), E));
  statics_exp_ana_simple(
    UHExp.[
      TyAliasLine(TPat.TyVar(NotInHole, "t"), ty),
      LetLine(p, UHExp.Block.wrap(UHExp.EmptyHole(u1))),
      ExpLine(UHExp.mk_OpSeq(S(UHExp.var("x"), E))),
    ],
    HTyp.int(),
  );
};

/* type t = 123 in ?  =>  ? */
let%test _ = {
  let ty = UHTyp.mk_OpSeq(S(TyVar(InHole(InvalidName, u0), "123"), E));
  statics_exp_syn_simple(
    UHExp.[
      TyAliasLine(TPat.TyVar(NotInHole, "t"), ty),
      ExpLine(UHExp.mk_OpSeq(S(EmptyHole(u1), E))),
    ],
    HTyp.hole(),
  );
};

/* type t = 123 in ?  <=  ? */
let%test _ = {
  let ty = UHTyp.mk_OpSeq(S(TyVar(InHole(InvalidName, u0), "123"), E));
  statics_exp_ana_simple(
    UHExp.[
      TyAliasLine(TPat.TyVar(NotInHole, "t"), ty),
      ExpLine(UHExp.mk_OpSeq(S(EmptyHole(u1), E))),
    ],
    HTyp.hole(),
  );
};

/* type t = type in 123   =>  Int */
let%test _ = {
  let ty = UHTyp.mk_OpSeq(S(TyVar(InHole(Reserved, u0), "type"), E));
  statics_exp_syn_simple(
    UHExp.[
      TyAliasLine(TPat.TyVar(NotInHole, "t"), ty),
      ExpLine(UHExp.mk_OpSeq(S(IntLit(NotInHole, "123"), E))),
    ],
    HTyp.int(),
  );
};

/* type t = type in 123   <=  Int */
let%test _ = {
  let ty = UHTyp.mk_OpSeq(S(TyVar(InHole(Reserved, u0), "type"), E));
  statics_exp_ana_simple(
    UHExp.[
      TyAliasLine(TPat.TyVar(NotInHole, "t"), ty),
      ExpLine(UHExp.mk_OpSeq(S(IntLit(NotInHole, "123"), E))),
    ],
    HTyp.int(),
  );
};

/* let x : t = ? in x  =>  t */
let%test _ = {
  let ty = UHTyp.mk_OpSeq(S(TyVar(InHole(Unbound, u0), "t"), E));
  let p = UHPat.mk_OpSeq(S(TypeAnn(NotInHole, UHPat.var("x"), ty), E));
  statics_exp_syn_simple(
    UHExp.[
      LetLine(p, UHExp.Block.wrap(UHExp.EmptyHole(u1))),
      ExpLine(UHExp.mk_OpSeq(S(UHExp.var("x"), E))),
    ],
    HTyp.tyvarhole(Unbound, u0, "t"),
  );
};

/* let x : t = ? in x  <=  t */
let%test _ = {
  let ty = UHTyp.mk_OpSeq(S(TyVar(InHole(Unbound, u0), "t"), E));
  let p = UHPat.mk_OpSeq(S(TypeAnn(NotInHole, UHPat.var("x"), ty), E));
  statics_exp_ana_simple(
    UHExp.[
      LetLine(p, UHExp.Block.wrap(UHExp.EmptyHole(u1))),
      ExpLine(UHExp.mk_OpSeq(S(UHExp.var("x"), E))),
    ],
    HTyp.tyvarhole(Unbound, u0, "t"),
  );
};
