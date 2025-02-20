/**
  This module describles the speculation of rules for:
  1) Unboxing
    e.g. Unboxing `let x = 1 in x` results in: `x`(pat), `1`(exp), `x`(exp)
    - An error occurs if unboxing is not possible.
  2) Unifying
    e.g. If unboxing reveals that `f` is unified with `fun x -> x`, any
    subsequent attempt to unify `f` with a value not equal to `fun x -> x`
    will result in an error.
  This module does not include `checking` functionalities, such as:
  e.g. Calculations like verifying NumLit(1) + NumLit(2) = NumLit(3) and
  subst(3, x, let y = 1 in x) = (let y = 1 in 3).
  Refer to `RuleTest.re` for checking functionalities.
 */
// No repr for `t` because we will use `of_syntax` to convert `t` to `DrvSyntax.t`
// and use `DrvSyntax.repr`.
module Map = Map.Make(String);

// A `specced` is a pair of a rule spec the real derivation
// syntax that is checked against.
[@deriving (show({with_path: false}), sexp, yojson)]
type specced = (Drv.Any.t, Drv.Any.t);

// A `map` maps a name in Reg to a `specced`.
[@deriving (show({with_path: false}), sexp, yojson)]
type map = [@opaque] Map.t(specced);

[@deriving (show({with_path: false}), sexp, yojson)]
type failure =
  | FailMatch(specced)
  | NotEqual(specced, specced);

let show_linked = ((spec, syntax): specced): string =>
  Printf.sprintf(
    "[*%s*](%s)",
    switch (spec) {
    | Exp({term: Quote(s), _})
    | Pat({term: Quote(s), _})
    | Typ({term: Quote(s), _})
    | TPat({term: Quote(s), _}) => s
    | _ => "term"
    },
    syntax |> Drv.Any.rep_id |> Id.to_string,
  );

let failure_msg = (failure: failure): string =>
  switch (failure) {
  | FailMatch((spec, _) as specced) =>
    Printf.sprintf(
      "Failed to match %s with %s",
      show_linked(specced),
      spec |> Drv.Any.cls_of |> Drv.Any.show_cls,
    )
  | NotEqual(specced1, specced2) =>
    Printf.sprintf(
      "Failed to unify %s and %s",
      show_linked(specced1),
      show_linked(specced2),
    )
  };

exception NeverUsed;
exception Unreachable;

let rec go: ((map, list(failure)), specced) => (map, list(failure)) =
  ((map, res) as info, (spec, syntax) as specced) => {
    let register = s => {
      switch (Map.find_opt(s, map)) {
      | Some((_, syntax') as specced') => (
          map,
          Drv.Any.eq(syntax, syntax')
            ? res : [NotEqual(specced, specced'), ...res],
        )
      | None => (Map.add(s, specced, map), res)
      };
    };
    let failunbox = (map, [FailMatch(specced), ...res]);
    switch (spec, syntax) {
    | (Exp(spec), Exp(syntax)) =>
      switch (Drv.Exp.term_of(spec), Drv.Exp.term_of(syntax)) {
      | (Hole(_), _) => raise(NeverUsed)
      | (Quote(s), _) => register(s)
      | (Var(_), _) => raise(NeverUsed)
      | (Parens(_), _) => raise(NeverUsed)
      | (Tuple(_), _) => raise(NeverUsed)
      | (Val(sa), Val(a)) => info |> go_exp(sa, a)
      | (Val(_), _) => failunbox
      | (Eval(sa, sb), Eval(a, b)) =>
        info |> go_exp(sa, a) |> go_exp(sb, b)
      | (Eval(_), _) => failunbox
      | (Entail(sa, sb), Entail(a, b)) =>
        info |> go_exp(sa, a) |> go_exp(sb, b)
      | (Entail(_), _) => failunbox
      | (Consistent(sa, sb), Consistent(a, b)) =>
        info |> go_typ(sa, a) |> go_typ(sb, b)
      | (Consistent(_), _) => failunbox
      | (MatchedArrow(sa, sb), MatchedArrow(a, b)) =>
        info |> go_typ(sa, a) |> go_typ(sb, b)
      | (MatchedArrow(_), _) => failunbox
      | (MatchedProd(sa, sb), MatchedProd(a, b)) =>
        info |> go_typ(sa, a) |> go_typ(sb, b)
      | (MatchedProd(_), _) => failunbox
      | (MatchedSum(sa, sb), MatchedSum(a, b)) =>
        info |> go_typ(sa, a) |> go_typ(sb, b)
      | (MatchedSum(_), _) => failunbox
      | (Ctx(_), _) => raise(NeverUsed)
      | (Cons(_), _) => raise(NeverUsed)
      | (Concat(_), _) => raise(NeverUsed)
      | (Type(sa), Type(a)) => info |> go_typ(sa, a)
      | (Type(_), _) => failunbox
      | (HasType(sa, sb), HasType(a, b)) =>
        info |> go_exp(sa, a) |> go_typ(sb, b)
      | (HasType(_), _) => failunbox
      | (Syn(sa, sb), Syn(a, b)) => info |> go_exp(sa, a) |> go_typ(sb, b)
      | (Syn(_), _) => failunbox
      | (Ana(sa, sb), Ana(a, b)) => info |> go_exp(sa, a) |> go_typ(sb, b)
      | (Ana(_), _) => failunbox
      | (And(sa, sb), And(a, b)) => info |> go_exp(sa, a) |> go_exp(sb, b)
      | (And(_), _) => failunbox
      | (Or(sa, sb), Or(a, b)) => info |> go_exp(sa, a) |> go_exp(sb, b)
      | (Or(_), _) => failunbox
      | (Impl(sa, sb), Impl(a, b)) =>
        info |> go_exp(sa, a) |> go_exp(sb, b)
      | (Impl(_), _) => failunbox
      | (Truth, Truth) => info
      | (Truth, _) => failunbox
      | (Falsity, Falsity) => info
      | (Falsity, _) => failunbox
      | (NumLit(_), _) => raise(NeverUsed)
      | (Neg(sa), Neg(a)) => info |> go_exp(sa, a)
      | (Neg(_), _) => failunbox
      | (BinOp(sop, sa, sb), BinOp(op, a, b)) when sop == op =>
        info |> go_exp(sa, a) |> go_exp(sb, b)
      | (BinOp(_), _) => failunbox
      | (True, True) => info
      | (True, _) => failunbox
      | (False, False) => info
      | (False, _) => failunbox
      | (If(sa, sb, sc), If(a, b, c)) =>
        info |> go_exp(sa, a) |> go_exp(sb, b) |> go_exp(sc, c)
      | (If(_), _) => failunbox
      | (Let(sp, sa, sb), Let(p, a, b)) =>
        info |> go_pat(sp, p) |> go_exp(sa, a) |> go_exp(sb, b)
      | (Let(_), _) => failunbox
      | (Fix(sp, sa), Fix(p, a)) => info |> go_pat(sp, p) |> go_exp(sa, a)
      | (Fix(_), _) => failunbox
      | (Fun(sp, sa), Fun(p, a)) => info |> go_pat(sp, p) |> go_exp(sa, a)
      | (Fun(_), _) => failunbox
      | (Ap(sa, sb), Ap(a, b)) => info |> go_exp(sa, a) |> go_exp(sb, b)
      | (Ap(_), _) => failunbox
      | (Pair(sa, sb), Pair(a, b)) =>
        info |> go_exp(sa, a) |> go_exp(sb, b)
      | (Pair(_), _) => failunbox
      | (Triv, Triv) => info
      | (Triv, _) => failunbox
      | (PrjL(sa), PrjL(a)) => info |> go_exp(sa, a)
      | (PrjL(_), _) => failunbox
      | (PrjR(sa), PrjR(a)) => info |> go_exp(sa, a)
      | (PrjR(_), _) => failunbox
      | (InjL(sa), InjL(a)) => info |> go_exp(sa, a)
      | (InjL(_), _) => failunbox
      | (InjR(sa), InjR(a)) => info |> go_exp(sa, a)
      | (InjR(_), _) => failunbox
      | (Case(sa, sb, sc, sd, se), Case(a, b, c, d, e)) =>
        info
        |> go_exp(sa, a)
        |> go_pat(sb, b)
        |> go_exp(sc, c)
        |> go_pat(sd, d)
        |> go_exp(se, e)
      | (Case(_), _) => failunbox
      | (Roll(sa), Roll(a)) => info |> go_exp(sa, a)
      | (Roll(_), _) => failunbox
      | (Unroll(sa), Unroll(a)) => info |> go_exp(sa, a)
      | (Unroll(_), _) => failunbox
      | (ExpHole, ExpHole) => info
      | (ExpHole, _) => failunbox
      }
    | (Exp(_), _) => raise(Unreachable)
    | (Pat(spec), Pat(syntax)) =>
      switch (Drv.Pat.term_of(spec), Drv.Pat.term_of(syntax)) {
      | (Hole(_), _) => raise(NeverUsed)
      | (Quote(s), _) => register(s)
      | (Var(_), _) => raise(NeverUsed)
      | (Parens(_), _) => raise(NeverUsed)
      | (Cast(sp, sa), Cast(p, a)) =>
        info |> go_pat(sp, p) |> go_typ(sa, a)
      | (Cast(_), _) => failunbox
      | (InjL(sp), InjL(p)) => info |> go_pat(sp, p)
      | (InjL(_), _) => failunbox
      | (InjR(sp), InjR(p)) => info |> go_pat(sp, p)
      | (InjR(_), _) => failunbox
      | (Pair(sp, sq), Pair(p, q)) =>
        info |> go_pat(sp, p) |> go_pat(sq, q)
      | (Pair(_), _) => failunbox
      }
    | (Pat(_), _) => raise(Unreachable)
    | (Typ(spec), Typ(syntax)) =>
      switch (Drv.Typ.term_of(spec), Drv.Typ.term_of(syntax)) {
      | (Hole(_), _) => raise(NeverUsed)
      | (Quote(s), _) => register(s)
      | (Var(_), _) => raise(NeverUsed)
      | (Parens(_), _) => raise(NeverUsed)
      | (Num, Num) => info
      | (Num, _) => failunbox
      | (Bool, Bool) => info
      | (Bool, _) => failunbox
      | (Arrow(sa, sb), Arrow(a, b)) =>
        info |> go_typ(sa, a) |> go_typ(sb, b)
      | (Arrow(_), _) => failunbox
      | (Prod(sa, sb), Prod(a, b)) =>
        info |> go_typ(sa, a) |> go_typ(sb, b)
      | (Prod(_), _) => failunbox
      | (Unit, Unit) => info
      | (Unit, _) => failunbox
      | (Sum(sa, sb), Sum(a, b)) => info |> go_typ(sa, a) |> go_typ(sb, b)
      | (Sum(_), _) => failunbox
      | (Rec(sp, sa), Rec(p, a)) =>
        info |> go_tpat(sp, p) |> go_typ(sa, a)
      | (Rec(_), _) => failunbox
      | (TypHole, TypHole) => info
      | (TypHole, _) => failunbox
      }
    | (Typ(_), _) => raise(Unreachable)
    | (TPat(spec), TPat(syntax)) =>
      switch (Drv.TPat.term_of(spec), Drv.TPat.term_of(syntax)) {
      | (Hole(_), _) => raise(NeverUsed)
      | (Quote(s), _) => register(s)
      | (Var(_), _) => raise(NeverUsed)
      }
    | (TPat(_), _) => raise(Unreachable)
    };
  }
and go_exp = (spec, syntax) => go(_, (Exp(spec), Exp(syntax)))
and go_pat = (spec, syntax) => go(_, (Pat(spec), Pat(syntax)))
and go_typ = (spec, syntax) => go(_, (Typ(spec), Typ(syntax)))
and go_tpat = (spec, syntax) => go(_, (TPat(spec), TPat(syntax)));

let of_spec = (rule: Rule.t): (Drv.Exp.t, list(Drv.Exp.t)) => {
  module M_Id =
    DrvTermBase.M({
      [@deriving (show({with_path: false}), sexp, yojson)]
      type t('a) = 'a;
    });
  open M_Id;
  let rec tag_exp_term: exp_term => DrvTermBase.exp_term =
    fun
    | Hole(x) => Hole(tag_type_hole(x))
    | Quote(x) => Quote(x)
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
      Case(tag_exp(e), tag_pat(x), tag_exp(e1), tag_pat(y), tag_exp(e2))
    | Roll(e) => Roll(tag_exp(e))
    | Unroll(e) => Unroll(tag_exp(e))
    | ExpHole => ExpHole
  and tag_exp = e => e |> tag_exp_term |> IdTagged.fresh
  and tag_pat_term: pat_term => DrvTermBase.pat_term =
    fun
    | Hole(x) => Hole(tag_type_hole(x))
    | Quote(x) => Quote(x)
    | Var(x) => Var(x)
    | Parens(p) => Parens(tag_pat(p))
    | Cast(p, t) => Cast(tag_pat(p), tag_typ(t))
    | InjL(p) => InjL(tag_pat(p))
    | InjR(p) => InjR(tag_pat(p))
    | Pair(p1, p2) => Pair(tag_pat(p1), tag_pat(p2))
  and tag_pat = p => p |> tag_pat_term |> IdTagged.fresh
  and tag_typ_term: typ_term => DrvTermBase.typ_term =
    fun
    | Hole(x) => Hole(tag_type_hole(x))
    | Quote(x) => Quote(x)
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
  and tag_tpat_term: tpat_term => DrvTermBase.tpat_term =
    fun
    | Hole(x) => Hole(tag_type_hole(x))
    | Quote(x) => Quote(x)
    | Var(x) => Var(x)
  and tag_tpat = p => p |> tag_tpat_term |> IdTagged.fresh
  and tag_type_hole: type_hole => DrvTermBase.type_hole =
    fun
    | AbbrNotVar => AbbrNotVar
    | AbbrNotFound => AbbrNotFound
    | AbbrNotDrvTerm => AbbrNotDrvTerm
    | Invalid(s) => Invalid(s)
    | EmptyHole => EmptyHole
    | MultiHole(es) => MultiHole(List.map(tag_any, es))
  and tag_any =
    fun
    | Exp(e) => Exp(tag_exp(e))
    | Pat(p) => Pat(tag_pat(p))
    | Typ(t) => Typ(tag_typ(t))
    | TPat(p) => TPat(tag_tpat(p));

  module SymbolMap =
    SymbolMap.M({
      type exp = exp_t;
      type pat = pat_t;
      type typ = typ_t;
      type tpat = tpat_t;
      let exp: string => exp = s => Quote(s);
      let pat: string => pat = s => Quote(s);
      let typ: string => typ = s => Quote(s);
      let tpat: string => tpat = s => Quote(s);
    });
  open SymbolMap;

  let entail = (ctx, p) => Entail(ctx, p);
  let type_ = (ctx, t) => entail(ctx, Type(t));
  let has = (ctx, x, t) => entail(ctx, HasType(x, t));
  let syn = (ctx, x, t) => entail(ctx, Syn(x, t));
  let ana = (ctx, x, t) => entail(ctx, Ana(x, t));
  let eval = (e, v) => Eval(e, v);
  let val_ = v => Val(v);

  let (concl_spec, prem_specs) =
    switch (rule) {
    | C_Refl => (Consistent(t, t), [])
    | C_UnkL => (Consistent(TypHole, t), [])
    | C_UnkR => (Consistent(t, TypHole), [])
    | C_Sum => (
        Consistent(Sum(t1, t2), Sum(t1', t2')),
        [Consistent(t1, t1'), Consistent(t2, t2')],
      )
    | C_Prod => (
        Consistent(Prod(t1, t2), Prod(t1', t2')),
        [Consistent(t1, t1'), Consistent(t2, t2')],
      )
    | C_Arrow => (
        Consistent(Arrow(t_in, t_out), Arrow(t_in', t_out')),
        [Consistent(t_in, t_in'), Consistent(t_out, t_out')],
      )
    | MS_Hole => (MatchedSum(TypHole, TypHole), [])
    | MS_Sum => (MatchedSum(Sum(t1, t2), Sum(t1, t2)), [])
    | MP_Hole => (MatchedProd(TypHole, TypHole), [])
    | MP_Prod => (MatchedProd(Prod(t1, t2), Prod(t1, t2)), [])
    | MA_Hole => (MatchedArrow(TypHole, TypHole), [])
    | MA_Arrow => (
        MatchedArrow(Arrow(t_in, t_out), Arrow(t_in, t_out)),
        [],
      )
    | TV_Num => (type_(delta, Num), [])
    | TV_Bool => (type_(delta, Bool), [])
    | TV_Unit => (type_(delta, Unit), [])
    | TV_Arrow => (
        type_(delta, Arrow(t1, t2)),
        [type_(delta, t1), type_(delta, t2)],
      )
    | TV_Prod => (
        type_(delta, Prod(t1, t2)),
        [type_(delta, t1), type_(delta, t2)],
      )
    | TV_Sum => (
        type_(delta, Sum(t1, t2)),
        [type_(delta, t1), type_(delta, t2)],
      )
    | TV_Rec => (type_(delta, Rec(tpat, t)), [type_(delta', t)])
    | TV_TVar => (type_(delta, t), [])
    | S_Hole => (syn(gamma, ExpHole, TypHole), [])
    | T_True => (has(gamma, True, Bool), [])
    | S_True => (syn(gamma, True, Bool), [])
    | V_True => (val_(True), [])
    | T_False => (has(gamma, False, Bool), [])
    | S_False => (syn(gamma, False, Bool), [])
    | V_False => (val_(False), [])
    | T_If => (
        has(gamma, If(e, e1, e2), t),
        [has(gamma, e, Bool), has(gamma, e1, t), has(gamma, e2, t)],
      )
    | S_If => (
        syn(gamma, If(e, e1, e2), t),
        [ana(gamma, e, Bool), syn(gamma, e1, t), syn(gamma, e2, t)],
      )
    | S_If_GT => (
        syn(gamma, If(e, e1, e2), t),
        [ana(gamma, e, Bool), has(gamma, e1, t1), has(gamma, e2, t2)],
      )
    | A_If => (
        ana(gamma, If(e, e1, e2), t),
        [ana(gamma, e, Bool), ana(gamma, e1, t), ana(gamma, e2, t)],
      )
    | E_If_T => (eval(If(e, e1, e2), v1), [eval(e, True), eval(e1, v1)])
    | E_If_F => (eval(If(e, e1, e2), v2), [eval(e, False), eval(e2, v2)])
    | T_Num => (has(gamma, n, Num), [])
    | S_Num => (syn(gamma, n, Num), [])
    | V_Num => (val_(n), [])
    | E_Num => (eval(n, n), [])
    | T_Neg => (has(gamma, Neg(e), Num), [has(gamma, e, Num)])
    | S_Neg => (syn(gamma, Neg(e), Num), [ana(gamma, e, Num)])
    | E_Neg => (eval(Neg(e), n'), [eval(e, n)])
    | T_Plus => (
        has(gamma, BinOp(Plus, e1, e2), Num),
        [has(gamma, e1, Num), has(gamma, e2, Num)],
      )
    | S_Plus => (
        syn(gamma, BinOp(Plus, e1, e2), Num),
        [ana(gamma, e1, Num), ana(gamma, e2, Num)],
      )
    | E_Plus => (
        eval(BinOp(Plus, e1, e2), n'),
        [eval(e1, n1), eval(e2, n2)],
      )
    | T_Minus => (
        has(gamma, BinOp(Minus, e1, e2), Num),
        [has(gamma, e1, Num), has(gamma, e2, Num)],
      )
    | S_Minus => (
        syn(gamma, BinOp(Minus, e1, e2), Num),
        [ana(gamma, e1, Num), ana(gamma, e2, Num)],
      )
    | E_Minus => (
        eval(BinOp(Minus, e1, e2), n'),
        [eval(e1, n1), eval(e2, n2)],
      )
    | T_Times => (
        has(gamma, BinOp(Times, e1, e2), Num),
        [has(gamma, e1, Num), has(gamma, e2, Num)],
      )
    | S_Times => (
        syn(gamma, BinOp(Times, e1, e2), Num),
        [ana(gamma, e1, Num), ana(gamma, e2, Num)],
      )
    | E_Times => (
        eval(BinOp(Times, e1, e2), n'),
        [eval(e1, n1), eval(e2, n2)],
      )
    | T_Lt => (
        has(gamma, BinOp(Lt, e1, e2), Bool),
        [has(gamma, e1, Num), has(gamma, e2, Num)],
      )
    | S_Lt => (
        syn(gamma, BinOp(Lt, e1, e2), Bool),
        [ana(gamma, e1, Num), ana(gamma, e2, Num)],
      )
    | E_Lt_T => (
        eval(BinOp(Lt, e1, e2), True),
        [eval(e1, n1), eval(e2, n2)],
      )
    | E_Lt_F => (
        eval(BinOp(Lt, e1, e2), False),
        [eval(e1, n1), eval(e2, n2)],
      )
    | T_Gt => (
        has(gamma, BinOp(Gt, e1, e2), Bool),
        [has(gamma, e1, Num), has(gamma, e2, Num)],
      )
    | S_Gt => (
        syn(gamma, BinOp(Gt, e1, e2), Bool),
        [ana(gamma, e1, Num), ana(gamma, e2, Num)],
      )
    | E_Gt_T => (
        eval(BinOp(Gt, e1, e2), True),
        [eval(e1, n1), eval(e2, n2)],
      )
    | E_Gt_F => (
        eval(BinOp(Gt, e1, e2), False),
        [eval(e1, n1), eval(e2, n2)],
      )
    | T_Eq => (
        has(gamma, BinOp(Eq, e1, e2), Bool),
        [has(gamma, e1, Num), has(gamma, e2, Num)],
      )
    | S_Eq => (
        syn(gamma, BinOp(Eq, e1, e2), Bool),
        [ana(gamma, e1, Num), ana(gamma, e2, Num)],
      )
    | E_Eq_T => (
        eval(BinOp(Eq, e1, e2), True),
        [eval(e1, n), eval(e2, n)],
      )
    | E_Eq_F => (
        eval(BinOp(Eq, e1, e2), False),
        [eval(e1, n1), eval(e2, n2)],
      )
    | T_Var => (has(gamma, ex, t), [])
    | S_Var => (syn(gamma, ex, t), [])
    | T_LetAnn => (
        has(gamma, Let(Cast(x, t_def), e_def, e_body), t),
        [has(gamma, e_def, t_def), has(gamma', e_body, t)],
      )
    | T_LetAnn_TV => (
        has(gamma, Let(Cast(x, t_def), e_def, e_body), t),
        [
          type_(delta, t_def),
          has(gamma, e_def, t_def),
          has(gamma', e_body, t),
        ],
      )
    | S_LetAnn => (
        syn(gamma, Let(Cast(x, t_def), e_def, e_body), t),
        [ana(gamma, e_def, t_def), syn(gamma', e_body, t)],
      )
    | A_LetAnn => (
        ana(gamma, Let(Cast(x, t_def), e_def, e_body), t),
        [ana(gamma, e_def, t_def), ana(gamma', e_body, t)],
      )
    | T_Let => (
        has(gamma, Let(x, e_def, e_body), t),
        [has(gamma, e_def, t_def), has(gamma', e_body, t)],
      )
    | S_Let => (
        syn(gamma, Let(x, e_def, e_body), t),
        [syn(gamma, e_def, t_def), syn(gamma', e_body, t)],
      )
    | A_Let => (
        ana(gamma, Let(x, e_def, e_body), t),
        [syn(gamma, e_def, t_def), ana(gamma', e_body, t)],
      )
    | E_Let => (
        eval(Let(x, e_def, e_body), v),
        [eval(e_def, v_def), eval(e_body', v)],
      )
    | T_FunAnn => (
        has(gamma, Fun(Cast(x, t_in), e_body), Arrow(t_in, t_out)),
        [has(gamma', e_body, t_out)],
      )
    | T_FunAnn_TV => (
        has(gamma, Fun(Cast(x, t_in), e_body), Arrow(t_in, t_out)),
        [type_(delta, t_in), has(gamma', e_body, t_out)],
      )
    | S_FunAnn => (
        syn(gamma, Fun(Cast(x, t_in), e_body), Arrow(t_in, t_out)),
        [syn(gamma', e_body, t_out)],
      )
    | A_FunAnn => (
        ana(gamma, Fun(Cast(x, t_in), e_body), Arrow(t_in, t_out)),
        [ana(gamma', e_body, t_out)],
      )
    | A_FunAnn_GT => (
        ana(gamma, Fun(Cast(x, t_in'), e_body), t),
        [
          MatchedArrow(t, Arrow(t_in, t_out)),
          Consistent(t_in', t_in),
          ana(gamma', e_body, t_out),
        ],
      )
    | T_Fun => (
        has(gamma, Fun(x, e_body), Arrow(t_in, t_out)),
        [has(gamma', e_body, t_out)],
      )
    | A_Fun => (
        ana(gamma, Fun(x, e_body), Arrow(t_in, t_out)),
        [ana(gamma', e_body, t_out)],
      )
    | A_Fun_GT => (
        ana(gamma, Fun(x, e_body), t),
        [MatchedArrow(t, Arrow(t_in, t_out)), ana(gamma', e_body, t_out)],
      )
    | V_Fun => (val_(Fun(x, e_body)), [])
    | T_Ap => (
        has(gamma, Ap(e1, e2), t_out),
        [has(gamma, e1, Arrow(t_in, t_out)), has(gamma, e2, t_in)],
      )
    | S_Ap => (
        syn(gamma, Ap(e1, e2), t_out),
        [syn(gamma, e1, Arrow(t_in, t_out)), ana(gamma, e2, t_in)],
      )
    | S_Ap_GT => (
        syn(gamma, Ap(e1, e2), t_out),
        [
          syn(gamma, e1, t),
          MatchedArrow(t, Arrow(t_in, t_out)),
          ana(gamma, e2, t_in),
        ],
      )
    | E_Ap => (
        eval(Ap(e1, e2), v),
        [eval(e1, Fun(x, e_body)), eval(e2, v2), eval(e_body', v)],
      )
    | T_Triv => (has(gamma, Triv, Unit), [])
    | S_Triv => (syn(gamma, Triv, Unit), [])
    | V_Triv => (val_(Triv), [])
    | T_Pair => (
        has(gamma, Tuple([e1, e2]), Prod(t1, t2)),
        [has(gamma, e1, t1), has(gamma, e2, t2)],
      )
    | S_Pair => (
        syn(gamma, Tuple([e1, e2]), Prod(t1, t2)),
        [syn(gamma, e1, t1), syn(gamma, e2, t2)],
      )
    | A_Pair => (
        ana(gamma, Tuple([e1, e2]), Prod(t1, t2)),
        [ana(gamma, e1, t1), ana(gamma, e2, t2)],
      )
    | A_Pair_GT => (
        ana(gamma, Tuple([e1, e2]), t),
        [
          MatchedProd(t, Prod(t1, t2)),
          ana(gamma, e1, t1),
          ana(gamma, e2, t2),
        ],
      )
    | V_Pair => (val_(Tuple([v1, v2])), [val_(v1), val_(v2)])
    | E_Pair => (
        eval(Tuple([e1, e2]), Tuple([v1, v2])),
        [eval(e1, v1), eval(e2, v2)],
      )
    | T_LetPair => (
        has(gamma, Let(Pair(x, y), e_def, e_body), t),
        [has(gamma, e_def, Prod(t1, t2)), has(gamma', e_body, t)],
      )
    | S_LetPair => (
        syn(gamma, Let(Pair(x, y), e_def, e_body), t),
        [syn(gamma, e_def, Prod(t1, t2)), syn(gamma', e_body, t)],
      )
    | S_LetPair_GT => (
        syn(gamma, Let(Pair(x, y), e_def, e_body), t),
        [
          syn(gamma, e_def, t_def),
          MatchedProd(t_def, Prod(t1, t2)),
          syn(gamma', e_body, t),
        ],
      )
    | A_LetPair => (
        ana(gamma, Let(Pair(x, y), e_def, e_body), t),
        [syn(gamma, e_def, Prod(t1, t2)), ana(gamma', e_body, t)],
      )
    | A_LetPair_GT => (
        ana(gamma, Let(Pair(x, y), e_def, e_body), t),
        [
          syn(gamma, e_def, t_def),
          MatchedProd(t_def, Prod(t1, t2)),
          ana(gamma', e_body, t),
        ],
      )
    | E_LetPair => (
        eval(Let(Pair(x, y), e_def, e_body), v),
        [eval(e_def, Tuple([v1, v2])), eval(e_body', v)],
      )
    | T_PrjL => (has(gamma, PrjL(e), t1), [has(gamma, e, Prod(t1, t2))])
    | S_PrjL => (syn(gamma, PrjL(e), t1), [syn(gamma, e, Prod(t1, t2))])
    | S_PrjL_GT => (
        syn(gamma, PrjL(e), t1),
        [syn(gamma, e, t), MatchedProd(t, Prod(t1, t2))],
      )
    | E_PrjL => (eval(PrjL(e), v1), [eval(e, Tuple([v1, v2]))])
    | T_PrjR => (has(gamma, PrjR(e), t2), [has(gamma, e, Prod(t1, t2))])
    | S_PrjR => (syn(gamma, PrjR(e), t2), [syn(gamma, e, Prod(t1, t2))])
    | S_PrjR_GT => (
        syn(gamma, PrjR(e), t2),
        [syn(gamma, e, t), MatchedProd(t, Prod(t1, t2))],
      )
    | E_PrjR => (eval(PrjR(e), v2), [eval(e, Tuple([v1, v2]))])
    | T_InjL => (has(gamma, InjL(e), Sum(t1, t2)), [has(gamma, e, t1)])
    | A_InjL => (ana(gamma, InjL(e), Sum(t1, t2)), [ana(gamma, e, t1)])
    | A_InjL_GT => (
        ana(gamma, InjL(e), t),
        [MatchedSum(t, Sum(t1, t2)), ana(gamma, e, t1)],
      )
    | V_InjL => (val_(InjL(e)), [val_(e)])
    | E_InjL => (eval(InjL(e), InjL(v)), [eval(e, v)])
    | T_InjR => (has(gamma, InjR(e), Sum(t1, t2)), [has(gamma, e, t2)])
    | A_InjR => (ana(gamma, InjR(e), Sum(t1, t2)), [ana(gamma, e, t2)])
    | A_InjR_GT => (
        ana(gamma, InjR(e), t),
        [MatchedSum(t, Sum(t1, t2)), ana(gamma, e, t2)],
      )
    | V_InjR => (val_(InjR(e)), [val_(e)])
    | E_InjR => (eval(InjR(e), InjR(v)), [eval(e, v)])
    | T_Case => (
        has(gamma, Case(e, InjL(x), e1, InjR(y), e2), t),
        [
          has(gamma, e, Sum(t1, t2)),
          has(gamma', e1, t),
          has(gamma'', e2, t),
        ],
      )
    | S_Case => (
        syn(gamma, Case(e, InjL(x), e1, InjR(y), e2), t),
        [
          syn(gamma, e, Sum(t1, t2)),
          syn(gamma', e1, t),
          syn(gamma'', e2, t),
        ],
      )
    | S_Case_GT => (
        syn(gamma, Case(e, InjL(x), e1, InjR(y), e2), t'),
        [
          syn(gamma, e, t),
          MatchedSum(t, Sum(t1, t2)),
          syn(gamma', e1, t1'),
          syn(gamma'', e2, t2'),
        ],
      )
    | A_Case => (
        ana(gamma, Case(e, InjL(x), e1, InjR(y), e2), t),
        [
          syn(gamma, e, Sum(t1, t2)),
          ana(gamma', e1, t),
          ana(gamma'', e2, t),
        ],
      )
    | A_Case_GT => (
        ana(gamma, Case(e, InjL(x), e1, InjR(y), e2), t),
        [
          syn(gamma, e, t'),
          MatchedSum(t', Sum(t1, t2)),
          ana(gamma', e1, t1'),
          ana(gamma'', e2, t2'),
        ],
      )
    | E_Case_L => (
        eval(Case(e, InjL(x), e1, InjR(y), e2), v1),
        [eval(e, InjL(v)), eval(e1', v1)],
      )
    | E_Case_R => (
        eval(Case(e, InjL(x), e1, InjR(y), e2), v2),
        [eval(e, InjR(v)), eval(e2', v2)],
      )
    | T_FixAnn => (
        has(gamma, Fix(Cast(x, t), e), t),
        [has(gamma', e, t)],
      )
    | T_FixAnn_TV => (
        has(gamma, Fix(Cast(x, t), e), t),
        [type_(delta, t), has(gamma', e, t)],
      )
    | T_Fix => (has(gamma, Fix(x, e), t), [has(gamma', e, t)])
    | E_Fix => (eval(Fix(x, e_body), v), [eval(e', v)])
    | T_Roll => (
        has(gamma, Roll(e), Rec(tpat, t_body)),
        [has(gamma, e, t_body')],
      )
    | V_Roll => (val_(Roll(e)), [val_(e)])
    | E_Roll => (eval(Roll(e), Roll(v)), [eval(e, v)])
    | T_Unroll => (
        has(gamma, Unroll(e), t_body'),
        [has(gamma, e, Rec(tpat, t_body))],
      )
    | E_Unroll => (eval(Unroll(e), v), [eval(e, Roll(v))])
    | A_Subsumption => (ana(gamma, e, t), [syn(gamma, e, t)])
    | A_Subsumption_GT => (
        ana(gamma, e, t),
        [syn(gamma, e, t'), Consistent(t', t)],
      )
    | E_Val => (eval(e, e), [val_(e)])
    | Assumption => (entail(gamma, a), [])
    | And_I => (
        entail(gamma, And(a, b)),
        [entail(gamma, a), entail(gamma, b)],
      )
    | And_E_L => (entail(gamma, a), [entail(gamma, And(a, b))])
    | And_E_R => (entail(gamma, b), [entail(gamma, And(a, b))])
    | Or_I_L => (entail(gamma, Or(a, b)), [entail(gamma, a)])
    | Or_I_R => (entail(gamma, Or(a, b)), [entail(gamma, b)])
    | Or_E => (
        entail(gamma, c),
        [entail(gamma, Or(a, b)), entail(gamma', c), entail(gamma'', c)],
      )
    | Implies_I => (entail(gamma, Impl(a, b)), [entail(gamma', b)])
    | Implies_E => (
        entail(gamma, b),
        [entail(gamma, Impl(a, b)), entail(gamma, a)],
      )
    | Truth_I => (entail(gamma, Truth), [])
    | Falsity_E => (entail(gamma, a), [entail(gamma, Falsity)])
    };
  (concl_spec |> tag_exp, prem_specs |> List.map(tag_exp));
};
