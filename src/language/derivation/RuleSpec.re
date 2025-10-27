open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type test = RuleFormula.t(bool);

module Spec = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('a, 'b) = {
    prems: list('a),
    concl: 'a,
    tests: list('b),
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Spec.t(Drv.Exp.t, test);

module M_Id = {
  module M_Id =
    DrvGrammar.M({
      [@deriving (show({with_path: false}), sexp, yojson, eq)]
      type t('a, 'b) = 'a;
    });
  include M_Id;
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type exp_t = M_Id.exp_t(unit);
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type pat_t = M_Id.pat_t(unit);
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type typ_t = M_Id.typ_t(unit);
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type tpat_t = M_Id.tpat_t(unit);
};

let of_spec: Rule.t => Spec.t(M_Id.exp_t, RuleFormula.M_Id.t(bool, unit)) = {
  module TestSymbolMap =
    SymbolMap.M({
      type exp = RuleFormula.M_Id.t(Drv.Exp.t, unit);
      type pat = RuleFormula.M_Id.t(Drv.Pat.t, unit);
      type typ = RuleFormula.M_Id.t(Drv.Typ.t, unit);
      type tpat = RuleFormula.M_Id.t(Drv.TPat.t, unit);
      let exp: string => exp = s => LookUpExp(s);
      let pat: string => pat = s => LookUpPat(s);
      let typ: string => typ = s => LookUpTyp(s);
      let tpat: string => tpat = s => LookUpTPat(s);
    });
  module T = TestSymbolMap;
  open M_Id;
  module SymbolMap =
    SymbolMap.M({
      type exp = exp_t;
      type pat = pat_t;
      type typ = typ_t;
      type tpat = tpat_t;
      let exp: string => exp = s => Var(s);
      let pat: string => pat = s => Var(s);
      let typ: string => typ = s => Var(s);
      let tpat: string => tpat = s => Var(s);
    });
  open SymbolMap;
  let val_ = v => Val(v);
  let eval = (e, v) => Eval(e, v);
  let entail = (ctx, p) => Entail(ctx, p);
  let type_ = (ctx, t) => entail(ctx, Type(t));
  let has = (ctx, x, t) => entail(ctx, HasType(x, t));
  let syn = (ctx, x, t) => entail(ctx, Syn(x, t));
  let ana = (ctx, x, t) => entail(ctx, Ana(x, t));
  let case = (e, p1, e1, p2, e2) => Case(e, InjL(p1), e1, InjR(p2), e2);

  fun
  | C_Refl => {
      concl: Consistent(t, t),
      prems: [],
      tests: [],
    }
  | C_UnkL => {
      concl: Consistent(TypHole, t),
      prems: [],
      tests: [],
    }
  | C_UnkR => {
      concl: Consistent(t, TypHole),
      prems: [],
      tests: [],
    }
  | C_Sum => {
      concl: Consistent(Sum(t1, t2), Sum(t1', t2')),
      prems: [Consistent(t1, t1'), Consistent(t2, t2')],
      tests: [],
    }
  | C_Prod => {
      concl: Consistent(Prod(t1, t2), Prod(t1', t2')),
      prems: [Consistent(t1, t1'), Consistent(t2, t2')],
      tests: [],
    }
  | C_Arrow => {
      concl: Consistent(Arrow(t_in, t_out), Arrow(t_in', t_out')),
      prems: [Consistent(t_in, t_in'), Consistent(t_out, t_out')],
      tests: [],
    }
  | MS_Hole => {
      concl: MatchedSum(TypHole, Sum(TypHole, TypHole)),
      prems: [],
      tests: [],
    }
  | MS_Sum => {
      concl: MatchedSum(Sum(t1, t2), Sum(t1, t2)),
      prems: [],
      tests: [],
    }
  | MP_Hole => {
      concl: MatchedProd(TypHole, Prod(TypHole, TypHole)),
      prems: [],
      tests: [],
    }
  | MP_Prod => {
      concl: MatchedProd(Prod(t1, t2), Prod(t1, t2)),
      prems: [],
      tests: [],
    }
  | MA_Hole => {
      concl: MatchedArrow(TypHole, Arrow(TypHole, TypHole)),
      prems: [],
      tests: [],
    }
  | MA_Arrow => {
      concl: MatchedArrow(Arrow(t_in, t_out), Arrow(t_in, t_out)),
      prems: [],
      tests: [],
    }
  | TV_Num => {
      concl: type_(delta, Num),
      prems: [],
      tests: [],
    }
  | TV_Bool => {
      concl: type_(delta, Bool),
      prems: [],
      tests: [],
    }
  | TV_Unit => {
      concl: type_(delta, Unit),
      prems: [],
      tests: [],
    }
  | TV_Arrow => {
      concl: type_(delta, Arrow(t1, t2)),
      prems: [type_(delta, t1), type_(delta, t2)],
      tests: [],
    }
  | TV_Prod => {
      concl: type_(delta, Prod(t1, t2)),
      prems: [type_(delta, t1), type_(delta, t2)],
      tests: [],
    }
  | TV_Sum => {
      concl: type_(delta, Sum(t1, t2)),
      prems: [type_(delta, t1), type_(delta, t2)],
      tests: [],
    }
  | TV_Rec => {
      concl: type_(delta, Rec(tpat, t)),
      prems: [type_(delta', t)],
      tests:
        T.[
          EqCtx(
            UnboxCtx(delta'),
            Cons(Type(TypVar(UnboxTPatVar(tpat))), UnboxCtx(delta)),
          ),
        ],
    }
  | TV_TVar => {
      concl: type_(delta, t),
      prems: [],
      tests: T.[Mem(Type(t), UnboxCtx(delta))],
    }
  | S_Hole => {
      concl: syn(gamma, ExpHole, TypHole),
      prems: [],
      tests: [],
    }
  | T_True => {
      concl: has(gamma, True, Bool),
      prems: [],
      tests: [],
    }
  | S_True => {
      concl: syn(gamma, True, Bool),
      prems: [],
      tests: [],
    }
  | V_True => {
      concl: val_(True),
      prems: [],
      tests: [],
    }
  | T_False => {
      concl: has(gamma, False, Bool),
      prems: [],
      tests: [],
    }
  | S_False => {
      concl: syn(gamma, False, Bool),
      prems: [],
      tests: [],
    }
  | V_False => {
      concl: val_(False),
      prems: [],
      tests: [],
    }
  | T_If => {
      concl: has(gamma, If(e, e1, e2), t),
      prems: [has(gamma, e, Bool), has(gamma, e1, t), has(gamma, e2, t)],
      tests: [],
    }
  | S_If => {
      concl: syn(gamma, If(e, e1, e2), t),
      prems: [ana(gamma, e, Bool), syn(gamma, e1, t), syn(gamma, e2, t)],
      tests: [],
    }
  | S_If_GT => {
      concl: syn(gamma, If(e, e1, e2), t),
      prems: [ana(gamma, e, Bool), has(gamma, e1, t1), has(gamma, e2, t2)],
      tests: T.[EqTyp(t, Glb(t1, t2))],
    }
  | A_If => {
      concl: ana(gamma, If(e, e1, e2), t),
      prems: [ana(gamma, e, Bool), ana(gamma, e1, t), ana(gamma, e2, t)],
      tests: [],
    }
  | E_If_T => {
      concl: eval(If(e, e1, e2), v1),
      prems: [eval(e, True), eval(e1, v1)],
      tests: [],
    }
  | E_If_F => {
      concl: eval(If(e, e1, e2), v2),
      prems: [eval(e, False), eval(e2, v2)],
      tests: [],
    }
  | T_Num => {
      concl: has(gamma, n, Num),
      prems: [],
      tests: T.[Ignore(UnboxNumLit(n))],
    }
  | S_Num => {
      concl: syn(gamma, n, Num),
      prems: [],
      tests: T.[Ignore(UnboxNumLit(n))],
    }
  | V_Num => {
      concl: val_(n),
      prems: [],
      tests: T.[Ignore(UnboxNumLit(n))],
    }
  | E_Num => {
      concl: eval(n, n),
      prems: [],
      tests: T.[Ignore(UnboxNumLit(n))],
    }
  | T_Neg => {
      concl: has(gamma, Neg(e), Num),
      prems: [has(gamma, e, Num)],
      tests: [],
    }
  | S_Neg => {
      concl: syn(gamma, Neg(e), Num),
      prems: [ana(gamma, e, Num)],
      tests: [],
    }
  | E_Neg => {
      concl: eval(Neg(e), n'),
      prems: [eval(e, n)],
      tests: T.[Eq(UnboxNumLit(n'), Neg(UnboxNumLit(n)))],
    }
  | T_Plus => {
      concl: has(gamma, BinOp(Plus, e1, e2), Num),
      prems: [has(gamma, e1, Num), has(gamma, e2, Num)],
      tests: [],
    }
  | S_Plus => {
      concl: syn(gamma, BinOp(Plus, e1, e2), Num),
      prems: [ana(gamma, e1, Num), ana(gamma, e2, Num)],
      tests: [],
    }
  | E_Plus => {
      concl: eval(BinOp(Plus, e1, e2), n'),
      prems: [eval(e1, n1), eval(e2, n2)],
      tests:
        T.[Eq(UnboxNumLit(n'), Plus(UnboxNumLit(n1), UnboxNumLit(n2)))],
    }
  | T_Minus => {
      concl: has(gamma, BinOp(Minus, e1, e2), Num),
      prems: [has(gamma, e1, Num), has(gamma, e2, Num)],
      tests: [],
    }
  | S_Minus => {
      concl: syn(gamma, BinOp(Minus, e1, e2), Num),
      prems: [ana(gamma, e1, Num), ana(gamma, e2, Num)],
      tests: [],
    }
  | E_Minus => {
      concl: eval(BinOp(Minus, e1, e2), n'),
      prems: [eval(e1, n1), eval(e2, n2)],
      tests:
        T.[Eq(UnboxNumLit(n'), Minus(UnboxNumLit(n1), UnboxNumLit(n2)))],
    }
  | T_Times => {
      concl: has(gamma, BinOp(Times, e1, e2), Num),
      prems: [has(gamma, e1, Num), has(gamma, e2, Num)],
      tests: [],
    }
  | S_Times => {
      concl: syn(gamma, BinOp(Times, e1, e2), Num),
      prems: [ana(gamma, e1, Num), ana(gamma, e2, Num)],
      tests: [],
    }
  | E_Times => {
      concl: eval(BinOp(Times, e1, e2), n'),
      prems: [eval(e1, n1), eval(e2, n2)],
      tests:
        T.[Eq(UnboxNumLit(n'), Times(UnboxNumLit(n1), UnboxNumLit(n2)))],
    }
  | T_Lt => {
      concl: has(gamma, BinOp(Lt, e1, e2), Bool),
      prems: [has(gamma, e1, Num), has(gamma, e2, Num)],
      tests: [],
    }
  | S_Lt => {
      concl: syn(gamma, BinOp(Lt, e1, e2), Bool),
      prems: [ana(gamma, e1, Num), ana(gamma, e2, Num)],
      tests: [],
    }
  | E_Lt_T => {
      concl: eval(BinOp(Lt, e1, e2), True),
      prems: [eval(e1, n1), eval(e2, n2)],
      tests: T.[Lt(UnboxNumLit(n1), UnboxNumLit(n2))],
    }
  | E_Lt_F => {
      concl: eval(BinOp(Lt, e1, e2), False),
      prems: [eval(e1, n1), eval(e2, n2)],
      tests: T.[NotLt(UnboxNumLit(n1), UnboxNumLit(n2))],
    }
  | T_Gt => {
      concl: has(gamma, BinOp(Gt, e1, e2), Bool),
      prems: [has(gamma, e1, Num), has(gamma, e2, Num)],
      tests: [],
    }
  | S_Gt => {
      concl: syn(gamma, BinOp(Gt, e1, e2), Bool),
      prems: [ana(gamma, e1, Num), ana(gamma, e2, Num)],
      tests: [],
    }
  | E_Gt_T => {
      concl: eval(BinOp(Gt, e1, e2), True),
      prems: [eval(e1, n1), eval(e2, n2)],
      tests: T.[Gt(UnboxNumLit(n1), UnboxNumLit(n2))],
    }
  | E_Gt_F => {
      concl: eval(BinOp(Gt, e1, e2), False),
      prems: [eval(e1, n1), eval(e2, n2)],
      tests: T.[NotGt(UnboxNumLit(n1), UnboxNumLit(n2))],
    }
  | T_Eq => {
      concl: has(gamma, BinOp(Eq, e1, e2), Bool),
      prems: [has(gamma, e1, Num), has(gamma, e2, Num)],
      tests: [],
    }
  | S_Eq => {
      concl: syn(gamma, BinOp(Eq, e1, e2), Bool),
      prems: [ana(gamma, e1, Num), ana(gamma, e2, Num)],
      tests: [],
    }
  | E_Eq_T => {
      concl: eval(BinOp(Eq, e1, e2), True),
      prems: [eval(e1, n), eval(e2, n)],
      tests: T.[Eq(UnboxNumLit(n1), UnboxNumLit(n2))],
    }
  | E_Eq_F => {
      concl: eval(BinOp(Eq, e1, e2), False),
      prems: [eval(e1, n1), eval(e2, n2)],
      tests: T.[NotEq(UnboxNumLit(n1), UnboxNumLit(n2))],
    }
  | T_Var => {
      concl: has(gamma, ex, t),
      prems: [],
      tests:
        T.[Mem(HasType(ExpVar(UnboxExpVar(ex)), t), UnboxCtx(gamma))],
    }
  | S_Var => {
      concl: syn(gamma, ex, t),
      prems: [],
      tests:
        T.[Mem(HasType(ExpVar(UnboxExpVar(ex)), t), UnboxCtx(gamma))],
    }
  | T_LetAnn => {
      concl: has(gamma, Let(Cast(x, t_def), e_def, e_body), t),
      prems: [has(gamma, e_def, t_def), has(gamma', e_body, t)],
      tests: T.[Ignore(UnboxPatVar(x))],
    }
  | T_LetAnn_TV => {
      concl: has(gamma, Let(Cast(x, t_def), e_def, e_body), t),
      prems: [
        type_(delta, t_def),
        has(gamma, e_def, t_def),
        has(gamma', e_body, t),
      ],
      tests:
        T.[
          EqCtx(
            UnboxCtx(gamma'),
            Cons(HasType(ExpVar(UnboxPatVar(x)), t_def), UnboxCtx(gamma)),
          ),
          Subset(UnboxCtx(delta), UnboxCtx(gamma)),
        ],
    }
  | S_LetAnn => {
      concl: syn(gamma, Let(Cast(x, t_def), e_def, e_body), t),
      prems: [ana(gamma, e_def, t_def), syn(gamma', e_body, t)],
      tests: T.[Ignore(UnboxPatVar(x))],
    }
  | A_LetAnn => {
      concl: ana(gamma, Let(Cast(x, t_def), e_def, e_body), t),
      prems: [ana(gamma, e_def, t_def), ana(gamma', e_body, t)],
      tests: T.[Ignore(UnboxPatVar(x))],
    }
  | T_Let => {
      concl: has(gamma, Let(x, e_def, e_body), t),
      prems: [has(gamma, e_def, t_def), has(gamma', e_body, t)],
      tests:
        T.[
          EqCtx(
            UnboxCtx(gamma'),
            Cons(HasType(ExpVar(UnboxPatVar(x)), t_def), UnboxCtx(gamma)),
          ),
        ],
    }
  | S_Let => {
      concl: syn(gamma, Let(x, e_def, e_body), t),
      prems: [syn(gamma, e_def, t_def), syn(gamma', e_body, t)],
      tests:
        T.[
          EqCtx(
            UnboxCtx(gamma'),
            Cons(HasType(ExpVar(UnboxPatVar(x)), t_def), UnboxCtx(gamma)),
          ),
        ],
    }
  | A_Let => {
      concl: ana(gamma, Let(x, e_def, e_body), t),
      prems: [syn(gamma, e_def, t_def), ana(gamma', e_body, t)],
      tests:
        T.[
          EqCtx(
            UnboxCtx(gamma'),
            Cons(HasType(ExpVar(UnboxPatVar(x)), t_def), UnboxCtx(gamma)),
          ),
        ],
    }
  | E_Let => {
      concl: eval(Let(x, e_def, e_body), v),
      prems: [eval(e_def, v_def), eval(e_body', v)],
      tests: T.[EqExp(e_body', Subst(v_def, UnboxPatVar(x), e_body))],
    }
  | T_FunAnn => {
      concl: has(gamma, Fun(Cast(x, t_in), e_body), Arrow(t_in, t_out)),
      prems: [has(gamma', e_body, t_out)],
      tests:
        T.[
          EqCtx(
            UnboxCtx(gamma'),
            Cons(HasType(ExpVar(UnboxPatVar(x)), t_in), UnboxCtx(gamma)),
          ),
        ],
    }
  | T_FunAnn_TV => {
      concl: has(gamma, Fun(Cast(x, t_in), e_body), Arrow(t_in, t_out)),
      prems: [type_(delta, t_in), has(gamma', e_body, t_out)],
      tests:
        T.[
          EqCtx(
            UnboxCtx(gamma'),
            Cons(HasType(ExpVar(UnboxPatVar(x)), t_in), UnboxCtx(gamma)),
          ),
          Subset(UnboxCtx(delta), UnboxCtx(gamma)),
        ],
    }
  | S_FunAnn => {
      concl: syn(gamma, Fun(Cast(x, t_in), e_body), Arrow(t_in, t_out)),
      prems: [syn(gamma', e_body, t_out)],
      tests:
        T.[
          EqCtx(
            UnboxCtx(gamma'),
            Cons(HasType(ExpVar(UnboxPatVar(x)), t_in), UnboxCtx(gamma)),
          ),
        ],
    }
  | A_FunAnn => {
      concl: ana(gamma, Fun(Cast(x, t_in), e_body), Arrow(t_in, t_out)),
      prems: [ana(gamma', e_body, t_out)],
      tests:
        T.[
          EqCtx(
            UnboxCtx(gamma'),
            Cons(HasType(ExpVar(UnboxPatVar(x)), t_in), UnboxCtx(gamma)),
          ),
        ],
    }
  | A_FunAnn_GT => {
      concl: ana(gamma, Fun(Cast(x, t_in'), e_body), t),
      prems: [
        MatchedArrow(t, Arrow(t_in, t_out)),
        Consistent(t_in', t_in),
        ana(gamma', e_body, t_out),
      ],
      tests:
        T.[
          EqCtx(
            UnboxCtx(gamma'),
            Cons(HasType(ExpVar(UnboxPatVar(x)), t_in'), UnboxCtx(gamma)),
          ),
        ],
    }
  | T_Fun => {
      concl: has(gamma, Fun(x, e_body), Arrow(t_in, t_out)),
      prems: [has(gamma', e_body, t_out)],
      tests:
        T.[
          EqCtx(
            UnboxCtx(gamma'),
            Cons(HasType(ExpVar(UnboxPatVar(x)), t_in), UnboxCtx(gamma)),
          ),
        ],
    }
  | A_Fun => {
      concl: ana(gamma, Fun(x, e_body), Arrow(t_in, t_out)),
      prems: [ana(gamma', e_body, t_out)],
      tests:
        T.[
          EqCtx(
            UnboxCtx(gamma'),
            Cons(HasType(ExpVar(UnboxPatVar(x)), t_in), UnboxCtx(gamma)),
          ),
        ],
    }
  | A_Fun_GT => {
      concl: ana(gamma, Fun(x, e_body), t),
      prems: [
        MatchedArrow(t, Arrow(t_in, t_out)),
        ana(gamma', e_body, t_out),
      ],
      tests:
        T.[
          EqCtx(
            UnboxCtx(gamma'),
            Cons(HasType(ExpVar(UnboxPatVar(x)), t_in), UnboxCtx(gamma)),
          ),
        ],
    }
  | V_Fun => {
      concl: val_(Fun(x, e_body)),
      prems: [],
      tests: T.[Ignore(UnboxPatVar(x))],
    }
  | T_Ap => {
      concl: has(gamma, Ap(e1, e2), t_out),
      prems: [has(gamma, e1, Arrow(t_in, t_out)), has(gamma, e2, t_in)],
      tests: [],
    }
  | S_Ap => {
      concl: syn(gamma, Ap(e1, e2), t_out),
      prems: [syn(gamma, e1, Arrow(t_in, t_out)), ana(gamma, e2, t_in)],
      tests: [],
    }
  | S_Ap_GT => {
      concl: syn(gamma, Ap(e1, e2), t_out),
      prems: [
        syn(gamma, e1, t),
        MatchedArrow(t, Arrow(t_in, t_out)),
        ana(gamma, e2, t_in),
      ],
      tests: [],
    }
  | E_Ap => {
      concl: eval(Ap(e1, e2), v),
      prems: [eval(e1, Fun(x, e_body)), eval(e2, v2), eval(e_body', v)],
      tests: T.[EqExp(e_body', Subst(v2, UnboxPatVar(x), e_body))],
    }
  | T_Triv => {
      concl: has(gamma, Triv, Unit),
      prems: [],
      tests: [],
    }
  | S_Triv => {
      concl: syn(gamma, Triv, Unit),
      prems: [],
      tests: [],
    }
  | V_Triv => {
      concl: val_(Triv),
      prems: [],
      tests: [],
    }
  | T_Pair => {
      concl: has(gamma, Pair(e1, e2), Prod(t1, t2)),
      prems: [has(gamma, e1, t1), has(gamma, e2, t2)],
      tests: [],
    }
  | S_Pair => {
      concl: syn(gamma, Pair(e1, e2), Prod(t1, t2)),
      prems: [syn(gamma, e1, t1), syn(gamma, e2, t2)],
      tests: [],
    }
  | A_Pair => {
      concl: ana(gamma, Pair(e1, e2), Prod(t1, t2)),
      prems: [ana(gamma, e1, t1), ana(gamma, e2, t2)],
      tests: [],
    }
  | A_Pair_GT => {
      concl: ana(gamma, Pair(e1, e2), t),
      prems: [
        MatchedProd(t, Prod(t1, t2)),
        ana(gamma, e1, t1),
        ana(gamma, e2, t2),
      ],
      tests: [],
    }
  | V_Pair => {
      concl: val_(Pair(v1, v2)),
      prems: [val_(v1), val_(v2)],
      tests: [],
    }
  | E_Pair => {
      concl: eval(Pair(e1, e2), Pair(v1, v2)),
      prems: [eval(e1, v1), eval(e2, v2)],
      tests: [],
    }
  | T_LetPair => {
      concl: has(gamma, Let(Pair(x, y), e_def, e_body), t),
      prems: [has(gamma, e_def, Prod(t1, t2)), has(gamma', e_body, t)],
      tests:
        T.[
          EqCtx(
            UnboxCtx(gamma'),
            Cons(
              HasType(ExpVar(UnboxPatVar(y)), t2),
              Cons(HasType(ExpVar(UnboxPatVar(x)), t1), UnboxCtx(gamma)),
            ),
          ),
        ],
    }
  | S_LetPair => {
      concl: syn(gamma, Let(Pair(x, y), e_def, e_body), t),
      prems: [syn(gamma, e_def, Prod(t1, t2)), syn(gamma', e_body, t)],
      tests:
        T.[
          EqCtx(
            UnboxCtx(gamma'),
            Cons(
              HasType(ExpVar(UnboxPatVar(y)), t2),
              Cons(HasType(ExpVar(UnboxPatVar(x)), t1), UnboxCtx(gamma)),
            ),
          ),
        ],
    }
  | S_LetPair_GT => {
      concl: syn(gamma, Let(Pair(x, y), e_def, e_body), t'),
      prems: [
        syn(gamma, e_def, t_def),
        MatchedProd(t_def, Prod(t1, t2)),
        syn(gamma', e_body, t),
      ],
      tests:
        T.[
          EqCtx(
            UnboxCtx(gamma'),
            Cons(
              HasType(ExpVar(UnboxPatVar(y)), t2),
              Cons(HasType(ExpVar(UnboxPatVar(x)), t1), UnboxCtx(gamma)),
            ),
          ),
          EqTyp(t', Glb(t1', t2')),
        ],
    }
  | A_LetPair => {
      concl: ana(gamma, Let(Pair(x, y), e_def, e_body), t),
      prems: [syn(gamma, e_def, Prod(t1, t2)), ana(gamma', e_body, t)],
      tests:
        T.[
          EqCtx(
            UnboxCtx(gamma'),
            Cons(
              HasType(ExpVar(UnboxPatVar(y)), t2),
              Cons(HasType(ExpVar(UnboxPatVar(x)), t1), UnboxCtx(gamma)),
            ),
          ),
        ],
    }
  | A_LetPair_GT => {
      concl: ana(gamma, Let(Pair(x, y), e_def, e_body), t),
      prems: [
        syn(gamma, e_def, t_def),
        MatchedProd(t_def, Prod(t1, t2)),
        ana(gamma', e_body, t),
      ],
      tests:
        T.[
          EqCtx(
            UnboxCtx(gamma'),
            Cons(
              HasType(ExpVar(UnboxPatVar(y)), t2),
              Cons(HasType(ExpVar(UnboxPatVar(x)), t1), UnboxCtx(gamma)),
            ),
          ),
        ],
    }
  | E_LetPair => {
      concl: eval(Let(Pair(x, y), e_def, e_body), v),
      prems: [eval(e_def, Pair(v1, v2)), eval(e_body', v)],
      tests:
        T.[
          EqExp(
            e_body',
            Subst(v1, UnboxPatVar(x), Subst(v2, UnboxPatVar(y), e_body)),
          ),
        ],
    }
  | T_PrjL => {
      concl: has(gamma, PrjL(e), t1),
      prems: [has(gamma, e, Prod(t1, t2))],
      tests: [],
    }
  | S_PrjL => {
      concl: syn(gamma, PrjL(e), t1),
      prems: [syn(gamma, e, Prod(t1, t2))],
      tests: [],
    }
  | S_PrjL_GT => {
      concl: syn(gamma, PrjL(e), t1),
      prems: [syn(gamma, e, t), MatchedProd(t, Prod(t1, t2))],
      tests: [],
    }
  | E_PrjL => {
      concl: eval(PrjL(e), v1),
      prems: [eval(e, Pair(v1, v2))],
      tests: [],
    }
  | T_PrjR => {
      concl: has(gamma, PrjR(e), t2),
      prems: [has(gamma, e, Prod(t1, t2))],
      tests: [],
    }
  | S_PrjR => {
      concl: syn(gamma, PrjR(e), t2),
      prems: [syn(gamma, e, Prod(t1, t2))],
      tests: [],
    }
  | S_PrjR_GT => {
      concl: syn(gamma, PrjR(e), t2),
      prems: [syn(gamma, e, t), MatchedProd(t, Prod(t1, t2))],
      tests: [],
    }
  | E_PrjR => {
      concl: eval(PrjR(e), v2),
      prems: [eval(e, Pair(v1, v2))],
      tests: [],
    }
  | T_InjL => {
      concl: has(gamma, InjL(e), Sum(t1, t2)),
      prems: [has(gamma, e, t1)],
      tests: [],
    }
  | A_InjL => {
      concl: ana(gamma, InjL(e), Sum(t1, t2)),
      prems: [ana(gamma, e, t1)],
      tests: [],
    }
  | A_InjL_GT => {
      concl: ana(gamma, InjL(e), t),
      prems: [MatchedSum(t, Sum(t1, t2)), ana(gamma, e, t1)],
      tests: [],
    }
  | V_InjL => {
      concl: val_(InjL(e)),
      prems: [val_(e)],
      tests: [],
    }
  | E_InjL => {
      concl: eval(InjL(e), InjL(v)),
      prems: [eval(e, v)],
      tests: [],
    }
  | T_InjR => {
      concl: has(gamma, InjR(e), Sum(t1, t2)),
      prems: [has(gamma, e, t2)],
      tests: [],
    }
  | A_InjR => {
      concl: ana(gamma, InjR(e), Sum(t1, t2)),
      prems: [ana(gamma, e, t2)],
      tests: [],
    }
  | A_InjR_GT => {
      concl: ana(gamma, InjR(e), t),
      prems: [MatchedSum(t, Sum(t1, t2)), ana(gamma, e, t2)],
      tests: [],
    }
  | V_InjR => {
      concl: val_(InjR(e)),
      prems: [val_(e)],
      tests: [],
    }
  | E_InjR => {
      concl: eval(InjR(e), InjR(v)),
      prems: [eval(e, v)],
      tests: [],
    }
  | T_Case => {
      concl: has(gamma, case(e, x, e1, y, e2), t),
      prems: [
        has(gamma, e, Sum(t1, t2)),
        has(gamma', e1, t),
        has(gamma'', e2, t),
      ],
      tests:
        T.[
          EqCtx(
            UnboxCtx(gamma'),
            Cons(HasType(ExpVar(UnboxPatVar(x)), t1), UnboxCtx(gamma)),
          ),
          EqCtx(
            UnboxCtx(gamma''),
            Cons(HasType(ExpVar(UnboxPatVar(y)), t2), UnboxCtx(gamma)),
          ),
        ],
    }
  | S_Case => {
      concl: syn(gamma, case(e, x, e1, y, e2), t),
      prems: [
        syn(gamma, e, Sum(t1, t2)),
        syn(gamma', e1, t),
        syn(gamma'', e2, t),
      ],
      tests:
        T.[
          EqCtx(
            UnboxCtx(gamma'),
            Cons(HasType(ExpVar(UnboxPatVar(x)), t1), UnboxCtx(gamma)),
          ),
          EqCtx(
            UnboxCtx(gamma''),
            Cons(HasType(ExpVar(UnboxPatVar(y)), t2), UnboxCtx(gamma)),
          ),
        ],
    }
  | S_Case_GT => {
      concl: syn(gamma, case(e, x, e1, y, e2), t'),
      prems: [
        syn(gamma, e, t),
        MatchedSum(t, Sum(t1, t2)),
        syn(gamma', e1, t1'),
        syn(gamma'', e2, t2'),
      ],
      tests:
        T.[
          EqCtx(
            UnboxCtx(gamma'),
            Cons(HasType(ExpVar(UnboxPatVar(x)), t1), UnboxCtx(gamma)),
          ),
          EqCtx(
            UnboxCtx(gamma''),
            Cons(HasType(ExpVar(UnboxPatVar(y)), t2), UnboxCtx(gamma)),
          ),
          EqTyp(t', Glb(t1', t2')),
        ],
    }
  | A_Case => {
      concl: ana(gamma, case(e, x, e1, y, e2), t),
      prems: [
        syn(gamma, e, Sum(t1, t2)),
        ana(gamma', e1, t),
        ana(gamma'', e2, t),
      ],
      tests:
        T.[
          EqCtx(
            UnboxCtx(gamma'),
            Cons(HasType(ExpVar(UnboxPatVar(x)), t1), UnboxCtx(gamma)),
          ),
          EqCtx(
            UnboxCtx(gamma''),
            Cons(HasType(ExpVar(UnboxPatVar(y)), t2), UnboxCtx(gamma)),
          ),
        ],
    }
  | A_Case_GT => {
      concl: ana(gamma, case(e, x, e1, y, e2), t),
      prems: [
        syn(gamma, e, t'),
        MatchedSum(t', Sum(t1, t2)),
        ana(gamma', e1, t1'),
        ana(gamma'', e2, t2'),
      ],
      tests:
        T.[
          EqCtx(
            UnboxCtx(gamma'),
            Cons(HasType(ExpVar(UnboxPatVar(x)), t1), UnboxCtx(gamma)),
          ),
          EqCtx(
            UnboxCtx(gamma''),
            Cons(HasType(ExpVar(UnboxPatVar(y)), t2), UnboxCtx(gamma)),
          ),
        ],
    }
  | E_Case_L => {
      concl: eval(case(e, x, e1, y, e2), v1),
      prems: [eval(e, InjL(v)), eval(e1', v1)],
      tests: T.[EqExp(e1', Subst(v, UnboxPatVar(x), e1))],
    }
  | E_Case_R => {
      concl: eval(case(e, x, e1, y, e2), v2),
      prems: [eval(e, InjR(v)), eval(e2', v2)],
      tests: T.[EqExp(e2', Subst(v, UnboxPatVar(y), e2))],
    }
  | T_FixAnn => {
      concl: has(gamma, Fix(Cast(x, t), e), t),
      prems: [has(gamma', e, t)],
      tests: [],
    }
  | T_FixAnn_TV => {
      concl: has(gamma, Fix(Cast(x, t), e), t),
      prems: [type_(delta, t), has(gamma', e, t)],
      tests:
        T.[
          EqCtx(
            UnboxCtx(gamma'),
            Cons(HasType(ExpVar(UnboxPatVar(x)), t), UnboxCtx(gamma)),
          ),
          Subset(UnboxCtx(delta), UnboxCtx(gamma)),
        ],
    }
  | T_Fix => {
      concl: has(gamma, Fix(x, e), t),
      prems: [has(gamma', e, t)],
      tests:
        T.[
          EqCtx(
            UnboxCtx(gamma'),
            Cons(HasType(ExpVar(UnboxPatVar(x)), t), UnboxCtx(gamma)),
          ),
        ],
    }
  | E_Fix => {
      concl: eval(Fix(x, e_body), v),
      prems: [eval(e', v)],
      tests: T.[EqExp(e', Subst(Fix(x, e_body), UnboxPatVar(x), e_body))],
    }
  | T_Roll => {
      concl: has(gamma, Roll(e), Rec(tpat, t_body)),
      prems: [has(gamma, e, t_body')],
      tests:
        T.[
          EqTyp(
            t_body',
            SubstTy(Rec(tpat, t_body), UnboxTPatVar(tpat), t_body),
          ),
        ],
    }
  | V_Roll => {
      concl: val_(Roll(e)),
      prems: [val_(e)],
      tests: [],
    }
  | E_Roll => {
      concl: eval(Roll(e), Roll(v)),
      prems: [eval(e, v)],
      tests: [],
    }
  | T_Unroll => {
      concl: has(gamma, Unroll(e), t_body'),
      prems: [has(gamma, e, Rec(tpat, t_body))],
      tests:
        T.[
          EqTyp(
            t_body',
            SubstTy(Rec(tpat, t_body), UnboxTPatVar(tpat), t_body),
          ),
        ],
    }
  | E_Unroll => {
      concl: eval(Unroll(e), v),
      prems: [eval(e, Roll(v))],
      tests: [],
    }
  | A_Subsumption => {
      concl: ana(gamma, e, t),
      prems: [syn(gamma, e, t)],
      tests: [],
    }
  | A_Subsumption_GT => {
      concl: ana(gamma, e, t),
      prems: [syn(gamma, e, t'), Consistent(t', t)],
      tests: [],
    }
  | E_Val => {
      concl: eval(e, e),
      prems: [val_(e)],
      tests: [],
    }
  | Assumption => {
      concl: entail(gamma, a),
      prems: [],
      tests: T.[Mem(a, UnboxCtx(gamma))],
    }
  | And_I => {
      concl: entail(gamma, And(a, b)),
      prems: [entail(gamma, a), entail(gamma, b)],
      tests: [],
    }
  | And_E_L => {
      concl: entail(gamma, a),
      prems: [entail(gamma, And(a, b))],
      tests: [],
    }
  | And_E_R => {
      concl: entail(gamma, b),
      prems: [entail(gamma, And(a, b))],
      tests: [],
    }
  | Or_I_L => {
      concl: entail(gamma, Or(a, b)),
      prems: [entail(gamma, a)],
      tests: [],
    }
  | Or_I_R => {
      concl: entail(gamma, Or(a, b)),
      prems: [entail(gamma, b)],
      tests: [],
    }
  | Or_E => {
      concl: entail(gamma, c),
      prems: [
        entail(gamma, Or(a, b)),
        entail(gamma', c),
        entail(gamma'', c),
      ],
      tests:
        T.[
          EqCtx(UnboxCtx(gamma'), Cons(a, UnboxCtx(gamma))),
          EqCtx(UnboxCtx(gamma''), Cons(b, UnboxCtx(gamma))),
        ],
    }
  | Implies_I => {
      concl: entail(gamma, Impl(a, b)),
      prems: [entail(gamma', b)],
      tests: T.[EqCtx(UnboxCtx(gamma'), Cons(a, UnboxCtx(gamma)))],
    }
  | Implies_E => {
      concl: entail(gamma, b),
      prems: [entail(gamma, Impl(a, b)), entail(gamma, a)],
      tests: [],
    }
  | Truth_I => {
      concl: entail(gamma, Truth),
      prems: [],
      tests: [],
    }
  | Falsity_E => {
      concl: entail(gamma, a),
      prems: [entail(gamma, Falsity)],
      tests: [],
    };
};

let of_spec: Rule.t => t =
  rule => {
    let rec tag_exp_term: M_Id.exp_t => Drv.Exp.term =
      fun
      | Hole(_) => failwith("Hole in spec")
      | Quote(_) => failwith("Quote in spec")
      | Var(x) => Var(x)
      | Parens(e) => Parens(tag_exp(e))
      | Val(e) => Val(tag_exp(e))
      | Eval(e1, e2) => Eval(tag_exp(e1), tag_exp(e2))
      | Entail(e1, e2) => Entail(tag_exp(e1), tag_exp(e2))
      | Consistent(t1, t2) => Consistent(tag_typ(t1), tag_typ(t2))
      | MatchedArrow(t1, t2) => MatchedArrow(tag_typ(t1), tag_typ(t2))
      | MatchedProd(t1, t2) => MatchedProd(tag_typ(t1), tag_typ(t2))
      | MatchedSum(t1, t2) => MatchedSum(tag_typ(t1), tag_typ(t2))
      | Ctx(es) => Ctx(List.map(tag_exp, es))
      | Cons(e1, e2) => Cons(tag_exp(e1), tag_exp(e2))
      | Concat(e1, e2) => Concat(tag_exp(e1), tag_exp(e2))
      | Type(t) => Type(tag_typ(t))
      | HasType(e, t) => HasType(tag_exp(e), tag_typ(t))
      | Syn(e, t) => Syn(tag_exp(e), tag_typ(t))
      | Ana(e, t) => Ana(tag_exp(e), tag_typ(t))
      | And(e1, e2) => And(tag_exp(e1), tag_exp(e2))
      | Or(e1, e2) => Or(tag_exp(e1), tag_exp(e2))
      | Impl(e1, e2) => Impl(tag_exp(e1), tag_exp(e2))
      | Truth => Truth
      | Falsity => Falsity
      | Tuple(es) => Tuple(List.map(tag_exp, es))
      | NumLit(n) => NumLit(n)
      | Neg(e) => Neg(tag_exp(e))
      | BinOp(op, e1, e2) => BinOp(op, tag_exp(e1), tag_exp(e2))
      | True => True
      | False => False
      | If(e1, e2, e3) => If(tag_exp(e1), tag_exp(e2), tag_exp(e3))
      | Let(p, e1, e2) => Let(tag_pat(p), tag_exp(e1), tag_exp(e2))
      | Fix(p, e) => Fix(tag_pat(p), tag_exp(e))
      | Fun(p, e) => Fun(tag_pat(p), tag_exp(e))
      | Ap(e1, e2) => Ap(tag_exp(e1), tag_exp(e2))
      | Pair(e1, e2) => Pair(tag_exp(e1), tag_exp(e2))
      | Triv => Triv
      | PrjL(e) => PrjL(tag_exp(e))
      | PrjR(e) => PrjR(tag_exp(e))
      | InjL(e) => InjL(tag_exp(e))
      | InjR(e) => InjR(tag_exp(e))
      | Case(e, x, e1, y, e2) =>
        Case(
          tag_exp(e),
          tag_pat(x),
          tag_exp(e1),
          tag_pat(y),
          tag_exp(e2),
        )
      | Roll(e) => Roll(tag_exp(e))
      | Unroll(e) => Unroll(tag_exp(e))
      | ExpHole => ExpHole
    and tag_exp = e => e |> tag_exp_term |> IdTagged.fresh
    and tag_pat_term: M_Id.pat_t => Drv.Pat.term =
      fun
      | Hole(_) => failwith("Hole in spec")
      | Quote(_) => failwith("Quote in spec")
      | Var(x) => Var(x)
      | Parens(p) => Parens(tag_pat(p))
      | Cast(p, t) => Cast(tag_pat(p), tag_typ(t))
      | InjL(p) => InjL(tag_pat(p))
      | InjR(p) => InjR(tag_pat(p))
      | Pair(p1, p2) => Pair(tag_pat(p1), tag_pat(p2))
    and tag_pat = p => p |> tag_pat_term |> IdTagged.fresh
    and tag_typ_term: M_Id.typ_t => Drv.Typ.term =
      fun
      | Hole(_) => failwith("Hole in spec")
      | Quote(_) => failwith("Quote in spec")
      | Var(x) => Var(x)
      | Parens(t) => Parens(tag_typ(t))
      | Num => Num
      | Bool => Bool
      | Arrow(t1, t2) => Arrow(tag_typ(t1), tag_typ(t2))
      | Prod(t1, t2) => Prod(tag_typ(t1), tag_typ(t2))
      | Unit => Unit
      | Sum(t1, t2) => Sum(tag_typ(t1), tag_typ(t2))
      | Rec(p, t) => Rec(tag_tpat(p), tag_typ(t))
      | TypHole => TypHole
    and tag_typ = t => t |> tag_typ_term |> IdTagged.fresh
    and tag_tpat_term: M_Id.tpat_t => Drv.TPat.term =
      fun
      | Hole(_) => failwith("Hole in spec")
      | Quote(_) => failwith("Quote in spec")
      | Var(x) => Var(x)
    and tag_tpat = p => p |> tag_tpat_term |> IdTagged.fresh;
    let Spec.{concl, prems, tests} = of_spec(rule);
    {
      concl: tag_exp(concl),
      prems: List.map(tag_exp, prems),
      tests: List.map(RuleFormula.M_Id.tag, tests),
    };
  };
