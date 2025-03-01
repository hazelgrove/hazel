open Util;

/**
  This module describles the speculation of rules for:
  1) Unboxing
    e.g. Unboxing `let x = 1 in x` results in: `x`(pat), `1`(exp), `x`(exp)
    - An error occurs if unboxing is not possible.
  2) Unifying
    e.g. if_ unboxing reveals that `f` is unified with `fun x -> x`, any
    subsequent attempt to unify `f` with a value not equal to `fun x -> x`
    will result in an error.
  This module does not include `checking` functionalities, such as:
  e.g. Calculations like verifying NumLit(1) + NumLit(2) = NumLit(3) and
  subst(3, x, let y = 1 in x) = (let y = 1 in 3).
  Refer to `RuleTest.re` for checking functionalities.
 */
module Formula = {
  open DrvTermBase;
  type ctx_t = list(exp_t);

  type t('a) =
    | LookUpExp(Var.t): t(exp_t)
    | LookUpPat(Var.t): t(pat_t)
    | LookUpTyp(Var.t): t(typ_t)
    | LookUpTPat(Var.t): t(tpat_t)
    | UnboxCtx(t(exp_t)): t(ctx_t)
    | UnboxNumLit(t(exp_t)): t(int)
    | UnboxExpVar(t(exp_t)): t(Var.t)
    | UnboxPatVar(t(pat_t)): t(Var.t)
    | UnboxTypVar(t(typ_t)): t(Var.t)
    | UnboxTPatVar(t(tpat_t)): t(Var.t)
    | ExpVar(t(Var.t)): t(exp_t)
    | HasType(t(exp_t), t(typ_t)): t(exp_t)
    | Type(t(typ_t)): t(exp_t)
    | Fix(t(pat_t), t(exp_t)): t(exp_t)
    | Subst(t(exp_t), t(Var.t), t(exp_t)): t(exp_t)
    | Ctx(t(ctx_t)): t(exp_t)
    | Cons(t(exp_t), t(ctx_t)): t(ctx_t)
    | Neg(t(int)): t(int)
    | Plus(t(int), t(int)): t(int)
    | Minus(t(int), t(int)): t(int)
    | Times(t(int), t(int)): t(int)
    | TypVar(t(Var.t)): t(typ_t)
    | Rec(t(tpat_t), t(typ_t)): t(typ_t)
    | Glb(t(typ_t), t(typ_t)): t(typ_t)
    | SubstTy(t(typ_t), t(Var.t), t(typ_t)): t(typ_t)
    | Ignore(t('a)): t(bool)
    | Gt(t(int), t(int)): t(bool)
    | Lt(t(int), t(int)): t(bool)
    | Eq(t(int), t(int)): t(bool)
    | NotGt(t(int), t(int)): t(bool)
    | NotLt(t(int), t(int)): t(bool)
    | NotEq(t(int), t(int)): t(bool)
    | EqExp(t(exp_t), t(exp_t)): t(bool)
    | EqCtx(t(ctx_t), t(ctx_t)): t(bool)
    | EqTyp(t(typ_t), t(typ_t)): t(bool)
    | Mem(t(exp_t), t(ctx_t)): t(bool)
    | Subset(t(ctx_t), t(ctx_t)): t(bool);

  let rec get_symbols: type a. t(a) => list(string) =
    fun
    | LookUpExp(s) => [s]
    | LookUpPat(s) => [s]
    | LookUpTyp(s) => [s]
    | LookUpTPat(s) => [s]
    | UnboxCtx(a) => get_symbols(a)
    | UnboxNumLit(a) => get_symbols(a)
    | UnboxExpVar(a) => get_symbols(a)
    | UnboxPatVar(a) => get_symbols(a)
    | UnboxTypVar(a) => get_symbols(a)
    | UnboxTPatVar(a) => get_symbols(a)
    | ExpVar(a) => get_symbols(a)
    | HasType(a, b) => get_symbols(a) @ get_symbols(b)
    | Type(a) => get_symbols(a)
    | Fix(a, b) => get_symbols(a) @ get_symbols(b)
    | Subst(a, b, c) => get_symbols(a) @ get_symbols(b) @ get_symbols(c)
    | Ctx(a) => get_symbols(a)
    | Cons(a, b) => get_symbols(a) @ get_symbols(b)
    | Neg(a) => get_symbols(a)
    | Plus(a, b) => get_symbols(a) @ get_symbols(b)
    | Minus(a, b) => get_symbols(a) @ get_symbols(b)
    | Times(a, b) => get_symbols(a) @ get_symbols(b)
    | TypVar(a) => get_symbols(a)
    | Rec(a, b) => get_symbols(a) @ get_symbols(b)
    | Glb(a, b) => get_symbols(a) @ get_symbols(b)
    | SubstTy(a, b, c) => get_symbols(a) @ get_symbols(b) @ get_symbols(c)
    | Ignore(a) => get_symbols(a)
    | Gt(a, b) => get_symbols(a) @ get_symbols(b)
    | Lt(a, b) => get_symbols(a) @ get_symbols(b)
    | Eq(a, b) => get_symbols(a) @ get_symbols(b)
    | NotGt(a, b) => get_symbols(a) @ get_symbols(b)
    | NotLt(a, b) => get_symbols(a) @ get_symbols(b)
    | NotEq(a, b) => get_symbols(a) @ get_symbols(b)
    | EqExp(a, b) => get_symbols(a) @ get_symbols(b)
    | EqCtx(a, b) => get_symbols(a) @ get_symbols(b)
    | EqTyp(a, b) => get_symbols(a) @ get_symbols(b)
    | Mem(a, b) => get_symbols(a) @ get_symbols(b)
    | Subset(a, b) => get_symbols(a) @ get_symbols(b);
};

[@deriving (show({with_path: false}), sexp, yojson)]
type test = [@opaque] Formula.t(bool);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  prems: list(Drv.Exp.t),
  concl: Drv.Exp.t,
  tests: list(test),
};

let of_spec: Rule.t => t = {
  module TestSymbolMap =
    SymbolMap.M({
      type exp = Formula.t(Drv.Exp.t);
      type pat = Formula.t(Drv.Pat.t);
      type typ = Formula.t(Drv.Typ.t);
      type tpat = Formula.t(Drv.TPat.t);
      let exp: string => exp = s => LookUpExp(s);
      let pat: string => pat = s => LookUpPat(s);
      let typ: string => typ = s => LookUpTyp(s);
      let tpat: string => tpat = s => LookUpTPat(s);
    });
  module T = TestSymbolMap;

  module SymbolMap =
    SymbolMap.M({
      type exp = Drv.Exp.t;
      type pat = Drv.Pat.t;
      type typ = Drv.Typ.t;
      type tpat = Drv.TPat.t;
      let exp: string => exp = s => Var(s) |> Drv.Exp.fresh;
      let pat: string => pat = s => Var(s) |> Drv.Pat.fresh;
      let typ: string => typ = s => Var(s) |> Drv.Typ.fresh;
      let tpat: string => tpat = s => Var(s) |> Drv.TPat.fresh;
    });
  open SymbolMap;

  // JDMT
  let val_ = v => Val(v) |> Drv.Exp.fresh;
  let eval = (e, v) => Eval(e, v) |> Drv.Exp.fresh;
  let entail = (ctx, p) => Entail(ctx, p) |> Drv.Exp.fresh;
  let consistent = (t1, t2) => Consistent(t1, t2) |> Drv.Exp.fresh;
  let matched_arrow = (t1, t2) => MatchedArrow(t1, t2) |> Drv.Exp.fresh;
  let matched_prod = (t1, t2) => MatchedProd(t1, t2) |> Drv.Exp.fresh;
  let matched_sum = (t1, t2) => MatchedSum(t1, t2) |> Drv.Exp.fresh;
  // PROP (In JDMT form)
  let type_ = (ctx, t) => entail(ctx, Type(t) |> Drv.Exp.fresh);
  let has = (ctx, x, t) => entail(ctx, HasType(x, t) |> Drv.Exp.fresh);
  let syn = (ctx, x, t) => entail(ctx, Syn(x, t) |> Drv.Exp.fresh);
  let ana = (ctx, x, t) => entail(ctx, Ana(x, t) |> Drv.Exp.fresh);
  // PROP
  let and_ = (e1, e2) => And(e1, e2) |> Drv.Exp.fresh;
  let or_ = (e1, e2) => Or(e1, e2) |> Drv.Exp.fresh;
  let impl = (e1, e2) => Impl(e1, e2) |> Drv.Exp.fresh;
  let truth = () => Truth |> Drv.Exp.fresh;
  let falsity = () => Falsity |> Drv.Exp.fresh;
  // EXP
  let neg = e => Neg(e) |> Drv.Exp.fresh;
  let binop = (op, e1, e2) => BinOp(op, e1, e2) |> Drv.Exp.fresh;
  let true_ = () => True |> Drv.Exp.fresh;
  let false_ = () => False |> Drv.Exp.fresh;
  let if_ = (e, e1, e2) => If(e, e1, e2) |> Drv.Exp.fresh;
  let let_ = (p, e1, e2) => Let(p, e1, e2) |> Drv.Exp.fresh;
  let let_ann = ((p, t), e1, e2) =>
    let_(Cast(p, t) |> Drv.Pat.fresh, e1, e2);
  let let_pair = ((p1, p2), e1, e2) =>
    let_(Pair(p1, p2) |> Drv.Pat.fresh, e1, e2);
  let fix = (p, e) => Fix(p, e) |> Drv.Exp.fresh;
  let fix_ann = ((p, t), e) => fix(Cast(p, t) |> Drv.Pat.fresh, e);
  let fun_ = (p, e) => Fun(p, e) |> Drv.Exp.fresh;
  let fun_ann = ((p, t), e) => fun_(Cast(p, t) |> Drv.Pat.fresh, e);
  let ap = (e1, e2) => Ap(e1, e2) |> Drv.Exp.fresh;
  let pair = (e1, e2) => Pair(e1, e2) |> Drv.Exp.fresh;
  let triv = () => Triv |> Drv.Exp.fresh;
  let prjl = e => PrjL(e) |> Drv.Exp.fresh;
  let prjr = e => PrjR(e) |> Drv.Exp.fresh;
  let injl = e => InjL(e) |> Drv.Exp.fresh;
  let injr = e => InjR(e) |> Drv.Exp.fresh;
  let case = (e, p1, e1, p2, e2) =>
    Case(e, InjL(p1) |> Drv.Pat.fresh, e1, InjR(p2) |> Drv.Pat.fresh, e2)
    |> Drv.Exp.fresh;
  let roll = e => Roll(e) |> Drv.Exp.fresh;
  let unroll = e => Unroll(e) |> Drv.Exp.fresh;
  let exphole = () => ExpHole |> Drv.Exp.fresh;
  // TYP
  let num = () => Num |> Drv.Typ.fresh;
  let bool = () => Bool |> Drv.Typ.fresh;
  let arrow = (t1, t2) => Arrow(t1, t2) |> Drv.Typ.fresh;
  let prod = (t1, t2) => Prod(t1, t2) |> Drv.Typ.fresh;
  let unit = () => Unit |> Drv.Typ.fresh;
  let sum = (t1, t2) => Sum(t1, t2) |> Drv.Typ.fresh;
  let rec_ = (tpat, t) => Rec(tpat, t) |> Drv.Typ.fresh;
  let typhole = () => TypHole |> Drv.Typ.fresh;

  fun
  | C_Refl => {concl: consistent(t, t), prems: [], tests: []}
  | C_UnkL => {concl: consistent(typhole(), t), prems: [], tests: []}
  | C_UnkR => {concl: consistent(t, typhole()), prems: [], tests: []}
  | C_Sum => {
      concl: consistent(sum(t1, t2), sum(t1', t2')),
      prems: [consistent(t1, t1'), consistent(t2, t2')],
      tests: [],
    }
  | C_Prod => {
      concl: consistent(prod(t1, t2), prod(t1', t2')),
      prems: [consistent(t1, t1'), consistent(t2, t2')],
      tests: [],
    }
  | C_Arrow => {
      concl: consistent(arrow(t_in, t_out), arrow(t_in', t_out')),
      prems: [consistent(t_in, t_in'), consistent(t_out, t_out')],
      tests: [],
    }
  | MS_Hole => {
      concl: matched_sum(typhole(), typhole()),
      prems: [],
      tests: [],
    }
  | MS_Sum => {
      concl: matched_sum(sum(t1, t2), sum(t1, t2)),
      prems: [],
      tests: [],
    }
  | MP_Hole => {
      concl: matched_prod(typhole(), typhole()),
      prems: [],
      tests: [],
    }
  | MP_Prod => {
      concl: matched_prod(prod(t1, t2), prod(t1, t2)),
      prems: [],
      tests: [],
    }
  | MA_Hole => {
      concl: matched_arrow(typhole(), typhole()),
      prems: [],
      tests: [],
    }
  | MA_Arrow => {
      concl: matched_arrow(arrow(t_in, t_out), arrow(t_in, t_out)),
      prems: [],
      tests: [],
    }
  | TV_Num => {concl: type_(delta, num()), prems: [], tests: []}
  | TV_Bool => {concl: type_(delta, bool()), prems: [], tests: []}
  | TV_Unit => {concl: type_(delta, unit()), prems: [], tests: []}
  | TV_Arrow => {
      concl: type_(delta, arrow(t1, t2)),
      prems: [type_(delta, t1), type_(delta, t2)],
      tests: [],
    }
  | TV_Prod => {
      concl: type_(delta, prod(t1, t2)),
      prems: [type_(delta, t1), type_(delta, t2)],
      tests: [],
    }
  | TV_Sum => {
      concl: type_(delta, sum(t1, t2)),
      prems: [type_(delta, t1), type_(delta, t2)],
      tests: [],
    }
  | TV_Rec => {
      concl: type_(delta, rec_(tpat, t)),
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
  | S_Hole => {concl: syn(gamma, exphole(), typhole()), prems: [], tests: []}
  | T_True => {concl: has(gamma, true_(), bool()), prems: [], tests: []}
  | S_True => {concl: syn(gamma, true_(), bool()), prems: [], tests: []}
  | V_True => {concl: val_(true_()), prems: [], tests: []}
  | T_False => {concl: has(gamma, false_(), bool()), prems: [], tests: []}
  | S_False => {concl: syn(gamma, false_(), bool()), prems: [], tests: []}
  | V_False => {concl: val_(false_()), prems: [], tests: []}
  | T_If => {
      concl: has(gamma, if_(e, e1, e2), t),
      prems: [has(gamma, e, bool()), has(gamma, e1, t), has(gamma, e2, t)],
      tests: [],
    }
  | S_If => {
      concl: syn(gamma, if_(e, e1, e2), t),
      prems: [ana(gamma, e, bool()), syn(gamma, e1, t), syn(gamma, e2, t)],
      tests: [],
    }
  | S_If_GT => {
      concl: syn(gamma, if_(e, e1, e2), t),
      prems: [
        ana(gamma, e, bool()),
        has(gamma, e1, t1),
        has(gamma, e2, t2),
      ],
      tests: T.[EqTyp(t, Glb(t1, t2))],
    }
  | A_If => {
      concl: ana(gamma, if_(e, e1, e2), t),
      prems: [ana(gamma, e, bool()), ana(gamma, e1, t), ana(gamma, e2, t)],
      tests: [],
    }
  | E_If_T => {
      concl: eval(if_(e, e1, e2), v1),
      prems: [eval(e, true_()), eval(e1, v1)],
      tests: [],
    }
  | E_If_F => {
      concl: eval(if_(e, e1, e2), v2),
      prems: [eval(e, false_()), eval(e2, v2)],
      tests: [],
    }
  | T_Num => {
      concl: has(gamma, n, num()),
      prems: [],
      tests: T.[Ignore(UnboxNumLit(n))],
    }
  | S_Num => {
      concl: syn(gamma, n, num()),
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
      concl: has(gamma, neg(e), num()),
      prems: [has(gamma, e, num())],
      tests: [],
    }
  | S_Neg => {
      concl: syn(gamma, neg(e), num()),
      prems: [ana(gamma, e, num())],
      tests: [],
    }
  | E_Neg => {
      concl: eval(neg(e), n'),
      prems: [eval(e, n)],
      tests: T.[Eq(UnboxNumLit(n'), Neg(UnboxNumLit(n)))],
    }
  | T_Plus => {
      concl: has(gamma, binop(Plus, e1, e2), num()),
      prems: [has(gamma, e1, num()), has(gamma, e2, num())],
      tests: [],
    }
  | S_Plus => {
      concl: syn(gamma, binop(Plus, e1, e2), num()),
      prems: [ana(gamma, e1, num()), ana(gamma, e2, num())],
      tests: [],
    }
  | E_Plus => {
      concl: eval(binop(Plus, e1, e2), n'),
      prems: [eval(e1, n1), eval(e2, n2)],
      tests:
        T.[Eq(UnboxNumLit(n'), Plus(UnboxNumLit(n1), UnboxNumLit(n2)))],
    }
  | T_Minus => {
      concl: has(gamma, binop(Minus, e1, e2), num()),
      prems: [has(gamma, e1, num()), has(gamma, e2, num())],
      tests: [],
    }
  | S_Minus => {
      concl: syn(gamma, binop(Minus, e1, e2), num()),
      prems: [ana(gamma, e1, num()), ana(gamma, e2, num())],
      tests: [],
    }
  | E_Minus => {
      concl: eval(binop(Minus, e1, e2), n'),
      prems: [eval(e1, n1), eval(e2, n2)],
      tests:
        T.[Eq(UnboxNumLit(n'), Minus(UnboxNumLit(n1), UnboxNumLit(n2)))],
    }
  | T_Times => {
      concl: has(gamma, binop(Times, e1, e2), num()),
      prems: [has(gamma, e1, num()), has(gamma, e2, num())],
      tests: [],
    }
  | S_Times => {
      concl: syn(gamma, binop(Times, e1, e2), num()),
      prems: [ana(gamma, e1, num()), ana(gamma, e2, num())],
      tests: [],
    }
  | E_Times => {
      concl: eval(binop(Times, e1, e2), n'),
      prems: [eval(e1, n1), eval(e2, n2)],
      tests:
        T.[Eq(UnboxNumLit(n'), Times(UnboxNumLit(n1), UnboxNumLit(n2)))],
    }
  | T_Lt => {
      concl: has(gamma, binop(Lt, e1, e2), bool()),
      prems: [has(gamma, e1, num()), has(gamma, e2, num())],
      tests: [],
    }
  | S_Lt => {
      concl: syn(gamma, binop(Lt, e1, e2), bool()),
      prems: [ana(gamma, e1, num()), ana(gamma, e2, num())],
      tests: [],
    }
  | E_Lt_T => {
      concl: eval(binop(Lt, e1, e2), true_()),
      prems: [eval(e1, n1), eval(e2, n2)],
      tests: T.[Lt(UnboxNumLit(n1), UnboxNumLit(n2))],
    }
  | E_Lt_F => {
      concl: eval(binop(Lt, e1, e2), false_()),
      prems: [eval(e1, n1), eval(e2, n2)],
      tests: T.[NotLt(UnboxNumLit(n1), UnboxNumLit(n2))],
    }
  | T_Gt => {
      concl: has(gamma, binop(Gt, e1, e2), bool()),
      prems: [has(gamma, e1, num()), has(gamma, e2, num())],
      tests: [],
    }
  | S_Gt => {
      concl: syn(gamma, binop(Gt, e1, e2), bool()),
      prems: [ana(gamma, e1, num()), ana(gamma, e2, num())],
      tests: [],
    }
  | E_Gt_T => {
      concl: eval(binop(Gt, e1, e2), true_()),
      prems: [eval(e1, n1), eval(e2, n2)],
      tests: T.[Gt(UnboxNumLit(n1), UnboxNumLit(n2))],
    }
  | E_Gt_F => {
      concl: eval(binop(Gt, e1, e2), false_()),
      prems: [eval(e1, n1), eval(e2, n2)],
      tests: T.[NotGt(UnboxNumLit(n1), UnboxNumLit(n2))],
    }
  | T_Eq => {
      concl: has(gamma, binop(Eq, e1, e2), bool()),
      prems: [has(gamma, e1, num()), has(gamma, e2, num())],
      tests: [],
    }
  | S_Eq => {
      concl: syn(gamma, binop(Eq, e1, e2), bool()),
      prems: [ana(gamma, e1, num()), ana(gamma, e2, num())],
      tests: [],
    }
  | E_Eq_T => {
      concl: eval(binop(Eq, e1, e2), true_()),
      prems: [eval(e1, n), eval(e2, n)],
      tests: T.[Eq(UnboxNumLit(n1), UnboxNumLit(n2))],
    }
  | E_Eq_F => {
      concl: eval(binop(Eq, e1, e2), false_()),
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
      concl: has(gamma, let_ann((x, t_def), e_def, e_body), t),
      prems: [has(gamma, e_def, t_def), has(gamma', e_body, t)],
      tests: T.[Ignore(UnboxPatVar(x))],
    }
  | T_LetAnn_TV => {
      concl: has(gamma, let_ann((x, t_def), e_def, e_body), t),
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
      concl: syn(gamma, let_ann((x, t_def), e_def, e_body), t),
      prems: [ana(gamma, e_def, t_def), syn(gamma', e_body, t)],
      tests: T.[Ignore(UnboxPatVar(x))],
    }
  | A_LetAnn => {
      concl: ana(gamma, let_ann((x, t_def), e_def, e_body), t),
      prems: [ana(gamma, e_def, t_def), ana(gamma', e_body, t)],
      tests: T.[Ignore(UnboxPatVar(x))],
    }
  | T_Let => {
      concl: has(gamma, let_(x, e_def, e_body), t),
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
      concl: syn(gamma, let_(x, e_def, e_body), t),
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
      concl: ana(gamma, let_(x, e_def, e_body), t),
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
      concl: eval(let_(x, e_def, e_body), v),
      prems: [eval(e_def, v_def), eval(e_body', v)],
      tests: T.[EqExp(e_body', Subst(v_def, UnboxPatVar(x), e_body))],
    }
  | T_FunAnn => {
      concl: has(gamma, fun_ann((x, t_in), e_body), arrow(t_in, t_out)),
      prems: [has(gamma', e_body, t_out)],
      tests: T.[Ignore(UnboxPatVar(x))],
    }
  | T_FunAnn_TV => {
      concl: has(gamma, fun_ann((x, t_in), e_body), arrow(t_in, t_out)),
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
      concl: syn(gamma, fun_ann((x, t_in), e_body), arrow(t_in, t_out)),
      prems: [syn(gamma', e_body, t_out)],
      tests: T.[Ignore(UnboxPatVar(x))],
    }
  | A_FunAnn => {
      concl: ana(gamma, fun_ann((x, t_in), e_body), arrow(t_in, t_out)),
      prems: [ana(gamma', e_body, t_out)],
      tests: T.[Ignore(UnboxPatVar(x))],
    }
  | A_FunAnn_GT => {
      concl: ana(gamma, fun_ann((x, t_in'), e_body), t),
      prems: [
        matched_arrow(t, arrow(t_in, t_out)),
        consistent(t_in', t_in),
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
      concl: has(gamma, fun_(x, e_body), arrow(t_in, t_out)),
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
      concl: ana(gamma, fun_(x, e_body), arrow(t_in, t_out)),
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
      concl: ana(gamma, fun_(x, e_body), t),
      prems: [
        matched_arrow(t, arrow(t_in, t_out)),
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
      concl: val_(fun_(x, e_body)),
      prems: [],
      tests: T.[Ignore(UnboxPatVar(x))],
    }
  | T_Ap => {
      concl: has(gamma, ap(e1, e2), t_out),
      prems: [has(gamma, e1, arrow(t_in, t_out)), has(gamma, e2, t_in)],
      tests: [],
    }
  | S_Ap => {
      concl: syn(gamma, ap(e1, e2), t_out),
      prems: [syn(gamma, e1, arrow(t_in, t_out)), ana(gamma, e2, t_in)],
      tests: [],
    }
  | S_Ap_GT => {
      concl: syn(gamma, ap(e1, e2), t_out),
      prems: [
        syn(gamma, e1, t),
        matched_arrow(t, arrow(t_in, t_out)),
        ana(gamma, e2, t_in),
      ],
      tests: [],
    }
  | E_Ap => {
      concl: eval(ap(e1, e2), v),
      prems: [eval(e1, fun_(x, e_body)), eval(e2, v2), eval(e_body', v)],
      tests: T.[EqExp(e_body', Subst(v2, UnboxPatVar(x), e_body))],
    }
  | T_Triv => {concl: has(gamma, triv(), unit()), prems: [], tests: []}
  | S_Triv => {concl: syn(gamma, triv(), unit()), prems: [], tests: []}
  | V_Triv => {concl: val_(triv()), prems: [], tests: []}
  | T_Pair => {
      concl: has(gamma, pair(e1, e2), prod(t1, t2)),
      prems: [has(gamma, e1, t1), has(gamma, e2, t2)],
      tests: [],
    }
  | S_Pair => {
      concl: syn(gamma, pair(e1, e2), prod(t1, t2)),
      prems: [syn(gamma, e1, t1), syn(gamma, e2, t2)],
      tests: [],
    }
  | A_Pair => {
      concl: ana(gamma, pair(e1, e2), prod(t1, t2)),
      prems: [ana(gamma, e1, t1), ana(gamma, e2, t2)],
      tests: [],
    }
  | A_Pair_GT => {
      concl: ana(gamma, pair(e1, e2), t),
      prems: [
        matched_prod(t, prod(t1, t2)),
        ana(gamma, e1, t1),
        ana(gamma, e2, t2),
      ],
      tests: [],
    }
  | V_Pair => {
      concl: val_(pair(v1, v2)),
      prems: [val_(v1), val_(v2)],
      tests: [],
    }
  | E_Pair => {
      concl: eval(pair(e1, e2), pair(v1, v2)),
      prems: [eval(e1, v1), eval(e2, v2)],
      tests: [],
    }
  | T_LetPair => {
      concl: has(gamma, let_pair((x, y), e_def, e_body), t),
      prems: [has(gamma, e_def, prod(t1, t2)), has(gamma', e_body, t)],
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
      concl: syn(gamma, let_pair((x, y), e_def, e_body), t),
      prems: [syn(gamma, e_def, prod(t1, t2)), syn(gamma', e_body, t)],
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
      concl: syn(gamma, let_pair((x, y), e_def, e_body), t'),
      prems: [
        syn(gamma, e_def, t_def),
        matched_prod(t_def, prod(t1, t2)),
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
      concl: ana(gamma, let_pair((x, y), e_def, e_body), t),
      prems: [syn(gamma, e_def, prod(t1, t2)), ana(gamma', e_body, t)],
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
      concl: ana(gamma, let_pair((x, y), e_def, e_body), t),
      prems: [
        syn(gamma, e_def, t_def),
        matched_prod(t_def, prod(t1, t2)),
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
      concl: eval(let_pair((x, y), e_def, e_body), v),
      prems: [eval(e_def, pair(v1, v2)), eval(e_body', v)],
      tests:
        T.[
          EqExp(
            e_body',
            Subst(v1, UnboxPatVar(x), Subst(v2, UnboxPatVar(y), e_body)),
          ),
        ],
    }
  | T_PrjL => {
      concl: has(gamma, prjl(e), t1),
      prems: [has(gamma, e, prod(t1, t2))],
      tests: [],
    }
  | S_PrjL => {
      concl: syn(gamma, prjl(e), t1),
      prems: [syn(gamma, e, prod(t1, t2))],
      tests: [],
    }
  | S_PrjL_GT => {
      concl: syn(gamma, prjl(e), t1),
      prems: [syn(gamma, e, t), matched_prod(t, prod(t1, t2))],
      tests: [],
    }
  | E_PrjL => {
      concl: eval(prjl(e), v1),
      prems: [eval(e, pair(v1, v2))],
      tests: [],
    }
  | T_PrjR => {
      concl: has(gamma, prjr(e), t2),
      prems: [has(gamma, e, prod(t1, t2))],
      tests: [],
    }
  | S_PrjR => {
      concl: syn(gamma, prjr(e), t2),
      prems: [syn(gamma, e, prod(t1, t2))],
      tests: [],
    }
  | S_PrjR_GT => {
      concl: syn(gamma, prjr(e), t2),
      prems: [syn(gamma, e, t), matched_prod(t, prod(t1, t2))],
      tests: [],
    }
  | E_PrjR => {
      concl: eval(prjr(e), v2),
      prems: [eval(e, pair(v1, v2))],
      tests: [],
    }
  | T_InjL => {
      concl: has(gamma, injl(e), sum(t1, t2)),
      prems: [has(gamma, e, t1)],
      tests: [],
    }
  | A_InjL => {
      concl: ana(gamma, injl(e), sum(t1, t2)),
      prems: [ana(gamma, e, t1)],
      tests: [],
    }
  | A_InjL_GT => {
      concl: ana(gamma, injl(e), t),
      prems: [matched_sum(t, sum(t1, t2)), ana(gamma, e, t1)],
      tests: [],
    }
  | V_InjL => {concl: val_(injl(e)), prems: [val_(e)], tests: []}
  | E_InjL => {
      concl: eval(injl(e), injl(v)),
      prems: [eval(e, v)],
      tests: [],
    }
  | T_InjR => {
      concl: has(gamma, injr(e), sum(t1, t2)),
      prems: [has(gamma, e, t2)],
      tests: [],
    }
  | A_InjR => {
      concl: ana(gamma, injr(e), sum(t1, t2)),
      prems: [ana(gamma, e, t2)],
      tests: [],
    }
  | A_InjR_GT => {
      concl: ana(gamma, injr(e), t),
      prems: [matched_sum(t, sum(t1, t2)), ana(gamma, e, t2)],
      tests: [],
    }
  | V_InjR => {concl: val_(injr(e)), prems: [val_(e)], tests: []}
  | E_InjR => {
      concl: eval(injr(e), injr(v)),
      prems: [eval(e, v)],
      tests: [],
    }
  | T_Case => {
      concl: has(gamma, case(e, x, e1, y, e2), t),
      prems: [
        has(gamma, e, sum(t1, t2)),
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
        syn(gamma, e, sum(t1, t2)),
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
        matched_sum(t, sum(t1, t2)),
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
        syn(gamma, e, sum(t1, t2)),
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
        matched_sum(t', sum(t1, t2)),
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
      prems: [eval(e, injl(v)), eval(e1', v1)],
      tests: T.[EqExp(e1', Subst(v, UnboxPatVar(x), e1))],
    }
  | E_Case_R => {
      concl: eval(case(e, x, e1, y, e2), v2),
      prems: [eval(e, injr(v)), eval(e2', v2)],
      tests: T.[EqExp(e2', Subst(v, UnboxPatVar(y), e2))],
    }
  | T_FixAnn => {
      concl: has(gamma, fix_ann((x, t), e), t),
      prems: [has(gamma', e, t)],
      tests: [],
    }
  | T_FixAnn_TV => {
      concl: has(gamma, fix_ann((x, t), e), t),
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
      concl: has(gamma, fix(x, e), t),
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
      concl: eval(fix(x, e_body), v),
      prems: [eval(e', v)],
      tests: T.[EqExp(e', Subst(Fix(x, e_body), UnboxPatVar(x), e_body))],
    }
  | T_Roll => {
      concl: has(gamma, roll(e), rec_(tpat, t_body)),
      prems: [has(gamma, e, t_body')],
      tests:
        T.[
          EqTyp(
            t_body',
            SubstTy(Rec(tpat, t_body), UnboxTPatVar(tpat), t_body),
          ),
        ],
    }
  | V_Roll => {concl: val_(roll(e)), prems: [val_(e)], tests: []}
  | E_Roll => {
      concl: eval(roll(e), roll(v)),
      prems: [eval(e, v)],
      tests: [],
    }
  | T_Unroll => {
      concl: has(gamma, unroll(e), t_body'),
      prems: [has(gamma, e, rec_(tpat, t_body))],
      tests:
        T.[
          EqTyp(
            t_body',
            SubstTy(Rec(tpat, t_body), UnboxTPatVar(tpat), t_body),
          ),
        ],
    }
  | E_Unroll => {
      concl: eval(unroll(e), v),
      prems: [eval(e, roll(v))],
      tests: [],
    }
  | A_Subsumption => {
      concl: ana(gamma, e, t),
      prems: [syn(gamma, e, t)],
      tests: [],
    }
  | A_Subsumption_GT => {
      concl: ana(gamma, e, t),
      prems: [syn(gamma, e, t'), consistent(t', t)],
      tests: [],
    }
  | E_Val => {concl: eval(e, e), prems: [val_(e)], tests: []}
  | Assumption => {
      concl: entail(gamma, a),
      prems: [],
      tests: T.[Mem(a, UnboxCtx(gamma))],
    }
  | And_I => {
      concl: entail(gamma, and_(a, b)),
      prems: [entail(gamma, a), entail(gamma, b)],
      tests: [],
    }
  | And_E_L => {
      concl: entail(gamma, a),
      prems: [entail(gamma, and_(a, b))],
      tests: [],
    }
  | And_E_R => {
      concl: entail(gamma, b),
      prems: [entail(gamma, and_(a, b))],
      tests: [],
    }
  | Or_I_L => {
      concl: entail(gamma, or_(a, b)),
      prems: [entail(gamma, a)],
      tests: [],
    }
  | Or_I_R => {
      concl: entail(gamma, or_(a, b)),
      prems: [entail(gamma, b)],
      tests: [],
    }
  | Or_E => {
      concl: entail(gamma, c),
      prems: [
        entail(gamma, or_(a, b)),
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
      concl: entail(gamma, impl(a, b)),
      prems: [entail(gamma', b)],
      tests: T.[EqCtx(UnboxCtx(gamma'), Cons(a, UnboxCtx(gamma)))],
    }
  | Implies_E => {
      concl: entail(gamma, b),
      prems: [entail(gamma, impl(a, b)), entail(gamma, a)],
      tests: [],
    }
  | Truth_I => {concl: entail(gamma, truth()), prems: [], tests: []}
  | Falsity_E => {
      concl: entail(gamma, a),
      prems: [entail(gamma, falsity())],
      tests: [],
    };
};
