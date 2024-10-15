module type Wrapper = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('a);
};

module M = (W: Wrapper) => {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    // Top-level
    // - Reg(string)
    // If the name exists in the map, it will compare the terms
    // Otherwise, it will register the term in the map
    | Reg(SymbolMap.key)
    // Judgments
    | Val(t)
    | Eval(t, t)
    | Entail(t, t)
    | Ctx(t) // Copy self
    // Proposition
    | Type(t)
    | HasType(t, t)
    | Syn(t, t)
    | Ana(t, t)
    // ALFA Exp
    | NumLit(t) // Copy self
    | Neg(t)
    | Plus(t, t)
    | Minus(t, t)
    | Times(t, t)
    | Lt(t, t)
    | Gt(t, t)
    | Eq(t, t)
    | True
    | False
    | If(t, t, t)
    | Var(t) // Copy self
    | Let(t, t, t)
    | Fix(t, t)
    | Fun(t, t)
    | Ap(t, t)
    | Pair(t, t)
    | Triv
    | PrjL(t)
    | PrjR(t)
    | InjL(t)
    | InjR(t)
    | Case(t, t, t, t, t)
    | Roll(t)
    | Unroll(t)
    // ALFA Typ
    | Num
    | Bool
    | Arrow(t, t)
    | Prod(t, t)
    | Sum(t, t)
    | Unit
    | TVar(t) // Copy self
    | Rec(t, t)
    // ALFA Pat
    | Pat(t) // Copy self
    | Cast(t, t) // We allow at most one annotation for Let/Fun/Fix
    | PatStrip(t) // Match either Pat or Cast
    | PatPair(t, t)
    | TPat(t) // Copy self
    // Logical Proposition
    | Atom(t) // Copy self
    | And(t, t)
    | Or(t, t)
    | Impl(t, t)
    | Truth
    | Falsity
  and t = W.t(term);
};

module M_IdTagged =
  M({
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t('a) = IdTagged.t('a);
  });

include M_IdTagged;

let rec map_reg = (f: string => string, spec: t): t => {
  let map_reg = map_reg(f);
  let (term, rewrap: term => t) = IdTagged.unwrap(spec);
  let term =
    switch (term) {
    | Reg(s) => Reg(f(s))
    | Val(a) => Val(map_reg(a))
    | Eval(a, b) => Eval(map_reg(a), map_reg(b))
    | Entail(a, b) => Entail(map_reg(a), map_reg(b))
    | Ctx(a) => Ctx(map_reg(a))
    | Type(a) => Type(map_reg(a))
    | HasType(a, b) => HasType(map_reg(a), map_reg(b))
    | Syn(a, b) => Syn(map_reg(a), map_reg(b))
    | Ana(a, b) => Ana(map_reg(a), map_reg(b))
    | NumLit(a) => NumLit(map_reg(a))
    | Neg(a) => Neg(map_reg(a))
    | Plus(a, b) => Plus(map_reg(a), map_reg(b))
    | Minus(a, b) => Minus(map_reg(a), map_reg(b))
    | Times(a, b) => Times(map_reg(a), map_reg(b))
    | Lt(a, b) => Lt(map_reg(a), map_reg(b))
    | Gt(a, b) => Gt(map_reg(a), map_reg(b))
    | Eq(a, b) => Eq(map_reg(a), map_reg(b))
    | True => True
    | False => False
    | If(a, b, c) => If(map_reg(a), map_reg(b), map_reg(c))
    | Var(a) => Var(map_reg(a))
    | Let(a, b, c) => Let(map_reg(a), map_reg(b), map_reg(c))
    | Fix(a, b) => Fix(map_reg(a), map_reg(b))
    | Fun(a, b) => Fun(map_reg(a), map_reg(b))
    | Ap(a, b) => Ap(map_reg(a), map_reg(b))
    | Pair(a, b) => Pair(map_reg(a), map_reg(b))
    | Triv => Triv
    | PrjL(a) => PrjL(map_reg(a))
    | PrjR(a) => PrjR(map_reg(a))
    | InjL(a) => InjL(map_reg(a))
    | InjR(a) => InjR(map_reg(a))
    | Case(a, b, c, d, e) =>
      Case(map_reg(a), map_reg(b), map_reg(c), map_reg(d), map_reg(e))
    | Roll(a) => Roll(map_reg(a))
    | Unroll(a) => Unroll(map_reg(a))
    | Num => Num
    | Bool => Bool
    | Arrow(a, b) => Arrow(map_reg(a), map_reg(b))
    | Prod(a, b) => Prod(map_reg(a), map_reg(b))
    | Sum(a, b) => Sum(map_reg(a), map_reg(b))
    | Unit => Unit
    | TVar(a) => TVar(map_reg(a))
    | Rec(a, b) => Rec(map_reg(a), map_reg(b))
    | Pat(a) => Pat(map_reg(a))
    | Cast(a, b) => Cast(map_reg(a), map_reg(b))
    | PatStrip(a) => PatStrip(map_reg(a))
    | PatPair(a, b) => PatPair(map_reg(a), map_reg(b))
    | TPat(a) => TPat(map_reg(a))
    | Atom(a) => Atom(map_reg(a))
    | And(a, b) => And(map_reg(a), map_reg(b))
    | Or(a, b) => Or(map_reg(a), map_reg(b))
    | Impl(a, b) => Impl(map_reg(a), map_reg(b))
    | Truth => Truth
    | Falsity => Falsity
    };
  term |> rewrap;
};

let rec of_syntax = (spec: t): DrvSyntax.t => {
  let (term, rewrap: DrvSyntax.term => DrvSyntax.t) = IdTagged.unwrap(spec);
  switch (term) {
  | Reg(s) => Atom(s) |> rewrap
  | Val(a) => Val(of_syntax(a)) |> rewrap
  | Eval(a, b) => Eval(of_syntax(a), of_syntax(b)) |> rewrap
  | Entail(a, b) => Entail(of_syntax(a), of_syntax(b)) |> rewrap
  | Type(a) => Type(of_syntax(a)) |> rewrap
  | HasType(a, b) => HasType(of_syntax(a), of_syntax(b)) |> rewrap
  | Syn(a, b) => Syn(of_syntax(a), of_syntax(b)) |> rewrap
  | Ana(a, b) => Ana(of_syntax(a), of_syntax(b)) |> rewrap
  | NumLit(r) => of_syntax(r)
  | Neg(a) => Neg(of_syntax(a)) |> rewrap
  | Plus(a, b) => Plus(of_syntax(a), of_syntax(b)) |> rewrap
  | Minus(a, b) => Minus(of_syntax(a), of_syntax(b)) |> rewrap
  | Times(a, b) => Times(of_syntax(a), of_syntax(b)) |> rewrap
  | Lt(a, b) => Lt(of_syntax(a), of_syntax(b)) |> rewrap
  | Gt(a, b) => Gt(of_syntax(a), of_syntax(b)) |> rewrap
  | Eq(a, b) => Eq(of_syntax(a), of_syntax(b)) |> rewrap
  | True => True |> rewrap
  | False => False |> rewrap
  | If(a, b, c) => If(of_syntax(a), of_syntax(b), of_syntax(c)) |> rewrap
  | Var(r) => of_syntax(r)
  | Let(a, b, c) =>
    Let(of_syntax(a), of_syntax(b), of_syntax(c)) |> rewrap
  | Fix(a, b) => Fix(of_syntax(a), of_syntax(b)) |> rewrap
  | Fun(a, b) => Fun(of_syntax(a), of_syntax(b)) |> rewrap
  | Ap(a, b) => Ap(of_syntax(a), of_syntax(b)) |> rewrap
  | Pair(a, b) => Pair(of_syntax(a), of_syntax(b)) |> rewrap
  | Triv => Triv |> rewrap
  | PrjL(a) => PrjL(of_syntax(a)) |> rewrap
  | PrjR(a) => PrjR(of_syntax(a)) |> rewrap
  | InjL(a) => InjL(of_syntax(a)) |> rewrap
  | InjR(a) => InjR(of_syntax(a)) |> rewrap
  | Case(a, b, c, d, e) =>
    Case(
      of_syntax(a),
      of_syntax(b),
      of_syntax(c),
      of_syntax(d),
      of_syntax(e),
    )
    |> rewrap
  | Roll(a) => Roll(of_syntax(a)) |> rewrap
  | Unroll(a) => Unroll(of_syntax(a)) |> rewrap
  | Num => Num |> rewrap
  | Bool => Bool |> rewrap
  | Arrow(a, b) => Arrow(of_syntax(a), of_syntax(b)) |> rewrap
  | Prod(a, b) => Prod(of_syntax(a), of_syntax(b)) |> rewrap
  | Sum(a, b) => Sum(of_syntax(a), of_syntax(b)) |> rewrap
  | Unit => Unit |> rewrap
  | TVar(r) => of_syntax(r)
  | Rec(a, b) => Rec(of_syntax(a), of_syntax(b)) |> rewrap
  | Pat(r) => of_syntax(r)
  | Cast(a, b) => Cast(of_syntax(a), of_syntax(b)) |> rewrap
  | PatStrip(r) => of_syntax(r)
  | PatPair(a, b) => PatPair(of_syntax(a), of_syntax(b)) |> rewrap
  | TPat(r) => of_syntax(r)
  | Atom(r) => of_syntax(r)
  | And(a, b) => And(of_syntax(a), of_syntax(b)) |> rewrap
  | Or(a, b) => Or(of_syntax(a), of_syntax(b)) |> rewrap
  | Impl(a, b) => Impl(of_syntax(a), of_syntax(b)) |> rewrap
  | Truth => Truth |> rewrap
  | Falsity => Falsity |> rewrap
  | Ctx(r) => of_syntax(r)
  };
};

let show = spec => spec |> of_syntax |> DrvSyntax.show;

module Map = Map.Make(String);

[@deriving (show({with_path: false}), sexp, yojson)]
type specced = (t, DrvSyntax.t);

type map = Map.t(specced);

type failure =
  | FailUnbox(specced)
  | NotEqual(specced, specced);

let show_linked = ((spec, syntax): specced): string =>
  Printf.sprintf(
    "[*%s*](%s)",
    spec |> show,
    syntax |> IdTagged.rep_id |> Id.to_string,
  );

let failure_msg = (failure: failure): string =>
  switch (failure) {
  | FailUnbox(specced) =>
    Printf.sprintf("Failed to unbox %s", show_linked(specced))
  | NotEqual(specced1, specced2) =>
    Printf.sprintf(
      "Failed to unify %s and %s",
      show_linked(specced1),
      show_linked(specced2),
    )
  };

let rec go: ((map, list(failure)), specced) => (map, list(failure)) =
  (info, (spec, syntax) as specced) => {
    let register = (s, (map, res)) => {
      switch (Map.find_opt(s, map)) {
      | Some((_, syntax') as specced') => (
          map,
          DrvSyntax.eq(syntax, syntax')
            ? res : [NotEqual(specced, specced'), ...res],
        )
      | None => (Map.add(s, specced, map), res)
      };
    };
    let failunbox = ((map, res)) => (map, [FailUnbox(specced), ...res]);
    let go = (spec, syntax) => go(_, (spec, syntax));
    switch (IdTagged.term_of(spec), DrvSyntax.term_of(syntax)) {
    // Top-level
    | (Reg(s), _) => info |> register(s)
    // Judgments
    | (Val(ra), Val(a)) => info |> go(ra, a)
    | (Eval(ra, rb), Eval(a, b)) => info |> go(ra, a) |> go(rb, b)
    | (Entail(ra, rb), Entail(a, b)) => info |> go(ra, a) |> go(rb, b)
    | (Ctx(r), Ctx(_)) => info |> go(r, syntax)
    // Proposition
    | (Type(rt), Type(t)) => info |> go(rt, t)
    | (HasType(ta, tb), HasType(a, b)) => info |> go(ta, a) |> go(tb, b)
    | (Syn(ta, tb), Syn(a, b)) => info |> go(ta, a) |> go(tb, b)
    | (Ana(ta, tb), Ana(a, b)) => info |> go(ta, a) |> go(tb, b)
    // ALFA Exp
    | (NumLit(r), NumLit(_)) => info |> go(r, syntax)
    | (NumLit(_), Neg(a)) => info |> go(spec, a) // Note(zhiyao): we allow NumLit to be negated
    | (Neg(ra), Neg(a)) => info |> go(ra, a)
    | (Plus(ra, rb), Plus(a, b)) => info |> go(ra, a) |> go(rb, b)
    | (Minus(ra, rb), Minus(a, b)) => info |> go(ra, a) |> go(rb, b)
    | (Times(ra, rb), Times(a, b)) => info |> go(ra, a) |> go(rb, b)
    | (Lt(ra, rb), Lt(a, b)) => info |> go(ra, a) |> go(rb, b)
    | (Gt(ra, rb), Gt(a, b)) => info |> go(ra, a) |> go(rb, b)
    | (Eq(ra, rb), Eq(a, b)) => info |> go(ra, a) |> go(rb, b)
    | (True, True) => info
    | (False, False) => info
    | (If(ra, rb, rc), If(a, b, c)) =>
      info |> go(ra, a) |> go(rb, b) |> go(rc, c)
    | (Var(r), Var(_)) => info |> go(r, syntax)
    | (Let(ra, rb, rc), Let(a, b, c)) =>
      info |> go(ra, a) |> go(rb, b) |> go(rc, c)
    | (Fix(ra, rb), Fix(a, b)) => info |> go(ra, a) |> go(rb, b)
    | (Fun(ra, rb), Fun(a, b)) => info |> go(ra, a) |> go(rb, b)
    | (Ap(ra, rb), Ap(a, b)) => info |> go(ra, a) |> go(rb, b)
    | (Pair(ra, rb), Pair(a, b)) => info |> go(ra, a) |> go(rb, b)
    | (Triv, Triv) => info
    | (PrjL(ra), PrjL(a)) => info |> go(ra, a)
    | (PrjR(ra), PrjR(a)) => info |> go(ra, a)
    | (InjL(ra), InjL(a)) => info |> go(ra, a)
    | (InjR(ra), InjR(a)) => info |> go(ra, a)
    | (Case(ra, rb, rc, rd, re), Case(a, b, c, d, e)) =>
      info
      |> go(ra, a)
      |> go(rb, b)
      |> go(rc, c)
      |> go(rd, d)
      |> go(re, e)
    | (Roll(ra), Roll(a)) => info |> go(ra, a)
    | (Unroll(ra), Unroll(a)) => info |> go(ra, a)
    // ALFA Typ
    | (Num, Num) => info
    | (Bool, Bool) => info
    | (Arrow(ra, rb), Arrow(a, b)) => info |> go(ra, a) |> go(rb, b)
    | (Prod(ra, rb), Prod(a, b)) => info |> go(ra, a) |> go(rb, b)
    | (Sum(ra, rb), Sum(a, b)) => info |> go(ra, a) |> go(rb, b)
    | (Unit, Unit) => info
    | (TVar(r), TVar(_)) => info |> go(r, syntax)
    | (Rec(ra, rb), Rec(a, b)) => info |> go(ra, a) |> go(rb, b)
    // ALFA Pat
    | (Pat(r), Pat(_)) => info |> go(r, syntax)
    | (Cast(ra, rb), Cast(a, b)) => info |> go(ra, a) |> go(rb, b)
    | (PatStrip(r), Pat(_)) => info |> go(r, syntax)
    | (PatStrip(r), Cast({term: Pat(_), _}, _)) => info |> go(r, syntax)
    | (PatPair(ra, rb), PatPair(a, b)) => info |> go(ra, a) |> go(rb, b)
    // ALFA TPat
    | (TPat(r), TPat(_)) => info |> go(r, syntax)
    // Logical Proposition
    | (Atom(r), Atom(_)) => info |> go(r, syntax)
    | (And(ra, rb), And(a, b)) => info |> go(ra, a) |> go(rb, b)
    | (Or(ra, rb), Or(a, b)) => info |> go(ra, a) |> go(rb, b)
    | (Impl(ra, rb), Impl(a, b)) => info |> go(ra, a) |> go(rb, b)
    | (Truth, Truth) => info
    | (Falsity, Falsity) => info
    | (Val(_), _)
    | (Eval(_), _)
    | (Entail(_), _)
    | (Ctx(_), _)
    | (Type(_), _)
    | (HasType(_), _)
    | (Syn(_), _)
    | (Ana(_), _)
    | (NumLit(_), _)
    | (Neg(_), _)
    | (Plus(_), _)
    | (Minus(_), _)
    | (Times(_), _)
    | (Lt(_), _)
    | (Gt(_), _)
    | (Eq(_), _)
    | (True, _)
    | (False, _)
    | (If(_), _)
    | (Var(_), _)
    | (Let(_), _)
    | (Fix(_), _)
    | (Fun(_), _)
    | (Ap(_), _)
    | (Pair(_), _)
    | (Triv, _)
    | (PrjL(_), _)
    | (PrjR(_), _)
    | (InjL(_), _)
    | (InjR(_), _)
    | (Case(_), _)
    | (Roll(_), _)
    | (Unroll(_), _)
    | (Num, _)
    | (Bool, _)
    | (Arrow(_), _)
    | (Prod(_), _)
    | (Sum(_), _)
    | (Unit, _)
    | (TVar(_), _)
    | (Rec(_), _)
    | (Pat(_), _)
    | (Cast(_), _)
    | (PatStrip(_), _)
    | (PatPair(_), _)
    | (TPat(_), _)
    | (Atom(_), _)
    | (And(_), _)
    | (Or(_), _)
    | (Impl(_), _)
    | (Truth, _)
    | (Falsity, _) => info |> failunbox
    };
  };

let of_spec = (rule: Rule.t): (t, list(t)) => {
  module M_Id =
    M({
      [@deriving (show({with_path: false}), sexp, yojson)]
      type t('a) = 'a;
    });
  open M_Id;
  module SymbolMap =
    SymbolMap.M({
      type target = t;
      let f: string => target = s => Reg(s);
    });
  open SymbolMap;
  let entail = (ctx, p) => Entail(Ctx(ctx), p);
  let type_ = (ctx, t) => entail(ctx, Type(t));
  let has = (ctx, x, t) => entail(ctx, HasType(x, t));
  let syn = (ctx, x, t) => entail(ctx, Syn(x, t));
  let ana = (ctx, x, t) => entail(ctx, Ana(x, t));
  let eval = (e, v) => Eval(e, v);
  let val_ = v => Val(v);
  let (concl_spec, prem_specs) =
    switch (rule) {
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
    | TV_Rec => (type_(delta, Rec(TPat(tpat), t)), [type_(delta', t)])
    | TV_TVar => (type_(delta, TVar(t)), [])
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
    | A_If => (
        ana(gamma, If(e, e1, e2), t),
        [ana(gamma, e, Bool), ana(gamma, e1, t), ana(gamma, e2, t)],
      )
    | E_If_T => (eval(If(e, e1, e2), v1), [eval(e, True), eval(e1, v1)])
    | E_If_F => (eval(If(e, e1, e2), v2), [eval(e, False), eval(e2, v2)])
    | T_Num => (has(gamma, NumLit(n), Num), [])
    | S_Num => (syn(gamma, NumLit(n), Num), [])
    | V_Num => (val_(NumLit(n)), [])
    | T_Neg => (has(gamma, Neg(e), Num), [has(gamma, e, Num)])
    | S_Neg => (syn(gamma, Neg(e), Num), [ana(gamma, e, Num)])
    | E_Neg => (eval(Neg(e), NumLit(n')), [eval(e, NumLit(n))])
    | T_Plus => (
        has(gamma, Plus(e1, e2), Num),
        [has(gamma, e1, Num), has(gamma, e2, Num)],
      )
    | S_Plus => (
        syn(gamma, Plus(e1, e2), Num),
        [ana(gamma, e1, Num), ana(gamma, e2, Num)],
      )
    | E_Plus => (
        eval(Plus(e1, e2), NumLit(n')),
        [eval(e1, NumLit(n1)), eval(e2, NumLit(n2))],
      )
    | T_Minus => (
        has(gamma, Minus(e1, e2), Num),
        [has(gamma, e1, Num), has(gamma, e2, Num)],
      )
    | S_Minus => (
        syn(gamma, Minus(e1, e2), Num),
        [ana(gamma, e1, Num), ana(gamma, e2, Num)],
      )
    | E_Minus => (
        eval(Minus(e1, e2), NumLit(n')),
        [eval(e1, NumLit(n1)), eval(e2, NumLit(n2))],
      )
    | T_Times => (
        has(gamma, Times(e1, e2), Num),
        [has(gamma, e1, Num), has(gamma, e2, Num)],
      )
    | S_Times => (
        syn(gamma, Times(e1, e2), Num),
        [ana(gamma, e1, Num), ana(gamma, e2, Num)],
      )
    | E_Times => (
        eval(Times(e1, e2), NumLit(n')),
        [eval(e1, NumLit(n1)), eval(e2, NumLit(n2))],
      )
    | T_Lt => (
        has(gamma, Lt(e1, e2), Bool),
        [has(gamma, e1, Num), has(gamma, e2, Num)],
      )
    | S_Lt => (
        syn(gamma, Lt(e1, e2), Bool),
        [ana(gamma, e1, Num), ana(gamma, e2, Num)],
      )
    | E_Lt_T => (
        eval(Lt(e1, e2), True),
        [eval(e1, NumLit(n1)), eval(e2, NumLit(n2))],
      )
    | E_Lt_F => (
        eval(Lt(e1, e2), False),
        [eval(e1, NumLit(n1)), eval(e2, NumLit(n2))],
      )
    | T_Gt => (
        has(gamma, Gt(e1, e2), Bool),
        [has(gamma, e1, Num), has(gamma, e2, Num)],
      )
    | S_Gt => (
        syn(gamma, Gt(e1, e2), Bool),
        [ana(gamma, e1, Num), ana(gamma, e2, Num)],
      )
    | E_Gt_T => (
        eval(Gt(e1, e2), True),
        [eval(e1, NumLit(n1)), eval(e2, NumLit(n2))],
      )
    | E_Gt_F => (
        eval(Gt(e1, e2), False),
        [eval(e1, NumLit(n1)), eval(e2, NumLit(n2))],
      )
    | T_Eq => (
        has(gamma, Eq(e1, e2), Bool),
        [has(gamma, e1, Num), has(gamma, e2, Num)],
      )
    | S_Eq => (
        syn(gamma, Eq(e1, e2), Bool),
        [ana(gamma, e1, Num), ana(gamma, e2, Num)],
      )
    | E_Eq_T => (
        eval(Eq(e1, e2), True),
        [eval(e1, NumLit(n)), eval(e2, NumLit(n))],
      )
    | E_Eq_F => (
        eval(Eq(e1, e2), False),
        [eval(e1, NumLit(n1)), eval(e2, NumLit(n2))],
      )
    | T_Var => (has(gamma, Var(x), t), [])
    | S_Var => (syn(gamma, Var(x), t), [])
    | T_LetAnn => (
        has(gamma, Let(Cast(Pat(x), t_def), e_def, e_body), t),
        [has(gamma, e_def, t_def), has(gamma', e_body, t)],
      )
    | T_LetAnn_TV => (
        has(gamma, Let(Cast(Pat(x), t_def), e_def, e_body), t),
        [
          type_(delta, t_def),
          has(gamma, e_def, t_def),
          has(gamma', e_body, t),
        ],
      )
    | S_LetAnn => (
        syn(gamma, Let(Cast(Pat(x), t_def), e_def, e_body), t),
        [ana(gamma, e_def, t_def), syn(gamma', e_body, t)],
      )
    | A_LetAnn => (
        ana(gamma, Let(Cast(Pat(x), t_def), e_def, e_body), t),
        [ana(gamma, e_def, t_def), ana(gamma', e_body, t)],
      )
    | T_Let => (
        has(gamma, Let(Pat(x), e_def, e_body), t),
        [has(gamma, e_def, t_def), has(gamma', e_body, t)],
      )
    | S_Let => (
        syn(gamma, Let(Pat(x), e_def, e_body), t),
        [syn(gamma, e_def, t_def), syn(gamma', e_body, t)],
      )
    | A_Let => (
        ana(gamma, Let(Pat(x), e_def, e_body), t),
        [syn(gamma, e_def, t_def), ana(gamma', e_body, t)],
      )
    | E_Let => (
        eval(Let(PatStrip(x), e_def, e_body), v),
        [eval(e_def, v_def), eval(e_body', v)],
      )
    | T_FunAnn => (
        has(gamma, Fun(Cast(Pat(x), t_in), e_body), Arrow(t_in, t_out)),
        [has(gamma', e_body, t_out)],
      )
    | T_FunAnn_TV => (
        has(gamma, Fun(Cast(Pat(x), t_in), e_body), Arrow(t_in, t_out)),
        [type_(delta, t_in), has(gamma', e_body, t_out)],
      )
    | S_FunAnn => (
        syn(gamma, Fun(Cast(Pat(x), t_in), e_body), Arrow(t_in, t_out)),
        [syn(gamma', e_body, t_out)],
      )
    | A_FunAnn => (
        ana(gamma, Fun(Cast(Pat(x), t_in), e_body), Arrow(t_in, t_out)),
        [ana(gamma', e_body, t_out)],
      )
    | T_Fun => (
        has(gamma, Fun(Pat(x), e_body), Arrow(t_in, t_out)),
        [has(gamma', e_body, t_out)],
      )
    | A_Fun => (
        ana(gamma, Fun(Pat(x), e_body), Arrow(t_in, t_out)),
        [ana(gamma', e_body, t_out)],
      )
    | V_Fun => (val_(Fun(Pat(x), e_body)), [])
    | T_Ap => (
        has(gamma, Ap(e1, e2), t_out),
        [has(gamma, e1, Arrow(t_in, t_out)), has(gamma, e2, t_in)],
      )
    | S_Ap => (
        syn(gamma, Ap(e1, e2), t_out),
        [syn(gamma, e1, Arrow(t_in, t_out)), ana(gamma, e2, t_in)],
      )
    | E_Ap => (
        eval(Ap(e1, e2), v),
        [
          eval(e1, Fun(PatStrip(x), e_body)),
          eval(e2, v2),
          eval(e_body', v),
        ],
      )
    | T_Triv => (has(gamma, Triv, Unit), [])
    | S_Triv => (syn(gamma, Triv, Unit), [])
    | V_Triv => (val_(Triv), [])
    | T_Pair => (
        has(gamma, Pair(e1, e2), Prod(t1, t2)),
        [has(gamma, e1, t1), has(gamma, e2, t2)],
      )
    | S_Pair => (
        syn(gamma, Pair(e1, e2), Prod(t1, t2)),
        [syn(gamma, e1, t1), syn(gamma, e2, t2)],
      )
    | A_Pair => (
        ana(gamma, Pair(e1, e2), Prod(t1, t2)),
        [ana(gamma, e1, t1), ana(gamma, e2, t2)],
      )
    | V_Pair => (val_(Pair(v1, v2)), [val_(v1), val_(v2)])
    | E_Pair => (
        eval(Pair(e1, e2), Pair(v1, v2)),
        [eval(e1, v1), eval(e2, v2)],
      )
    | T_LetPair => (
        has(gamma, Let(PatPair(Pat(x), Pat(y)), e_def, e_body), t),
        [has(gamma, e_def, Prod(t1, t2)), has(gamma', e_body, t)],
      )
    | S_LetPair => (
        syn(gamma, Let(PatPair(Pat(x), Pat(y)), e_def, e_body), t),
        [syn(gamma, e_def, Prod(t1, t2)), syn(gamma', e_body, t)],
      )
    | A_LetPair => (
        ana(gamma, Let(PatPair(Pat(x), Pat(y)), e_def, e_body), t),
        [syn(gamma, e_def, Prod(t1, t2)), ana(gamma', e_body, t)],
      )
    | E_LetPair => (
        eval(Let(PatPair(PatStrip(x), PatStrip(y)), e_def, e_body), v),
        [eval(e_def, Pair(v1, v2)), eval(e_body', v)],
      )
    | T_PrjL => (has(gamma, PrjL(e), t1), [has(gamma, e, Prod(t1, t2))])
    | S_PrjL => (syn(gamma, PrjL(e), t1), [syn(gamma, e, Prod(t1, t2))])
    | E_PrjL => (eval(PrjL(e), v1), [eval(e, Pair(v1, v2))])
    | T_PrjR => (has(gamma, PrjR(e), t2), [has(gamma, e, Prod(t1, t2))])
    | S_PrjR => (syn(gamma, PrjR(e), t2), [syn(gamma, e, Prod(t1, t2))])
    | E_PrjR => (eval(PrjR(e), v2), [eval(e, Pair(v1, v2))])
    | T_InjL => (has(gamma, InjL(e), Sum(t1, t2)), [has(gamma, e, t1)])
    | A_InjL => (ana(gamma, InjL(e), Sum(t1, t2)), [ana(gamma, e, t1)])
    | V_InjL => (val_(InjL(e)), [val_(e)])
    | E_InjL => (eval(InjL(e), InjL(v)), [eval(e, v)])
    | T_InjR => (has(gamma, InjR(e), Sum(t1, t2)), [has(gamma, e, t2)])
    | A_InjR => (ana(gamma, InjR(e), Sum(t1, t2)), [ana(gamma, e, t2)])
    | V_InjR => (val_(InjR(e)), [val_(e)])
    | E_InjR => (eval(InjR(e), InjR(v)), [eval(e, v)])
    | T_Case => (
        has(gamma, Case(e, Pat(x), e1, Pat(y), e2), t),
        [
          has(gamma, e, Sum(t1, t2)),
          has(gamma', e1, t),
          has(gamma'', e2, t),
        ],
      )
    | S_Case => (
        syn(gamma, Case(e, Pat(x), e1, Pat(y), e2), t),
        [
          syn(gamma, e, Sum(t1, t2)),
          syn(gamma', e1, t),
          syn(gamma'', e2, t),
        ],
      )
    | A_Case => (
        ana(gamma, Case(e, Pat(x), e1, Pat(y), e2), t),
        [
          syn(gamma, e, Sum(t1, t2)),
          ana(gamma', e1, t),
          ana(gamma'', e2, t),
        ],
      )
    | E_Case_L => (
        eval(Case(e, Pat(x), e1, Pat(y), e2), v1),
        [eval(e, InjL(v)), eval(e1', v1)],
      )
    | E_Case_R => (
        eval(Case(e, Pat(x), e1, Pat(y), e2), v2),
        [eval(e, InjR(v)), eval(e2', v2)],
      )
    | T_FixAnn => (
        has(gamma, Fix(Cast(Pat(x), t), e), t),
        [has(gamma', e, t)],
      )
    | T_FixAnn_TV => (
        has(gamma, Fix(Cast(Pat(x), t), e), t),
        [type_(delta, t), has(gamma', e, t)],
      )
    | T_Fix => (has(gamma, Fix(Pat(x), e), t), [has(gamma', e, t)])
    | E_Fix => (eval(Fix(PatStrip(x), e_body), v), [eval(e', v)])
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
  let rec fresh_term: term => M_IdTagged.term =
    fun
    | Reg(s) => Reg(s)
    | Val(a) => Val(fresh(a))
    | Eval(a, b) => Eval(fresh(a), fresh(b))
    | Entail(a, b) => Entail(fresh(a), fresh(b))
    | Ctx(a) => Ctx(fresh(a))
    | Type(a) => Type(fresh(a))
    | HasType(a, b) => HasType(fresh(a), fresh(b))
    | Syn(a, b) => Syn(fresh(a), fresh(b))
    | Ana(a, b) => Ana(fresh(a), fresh(b))
    | NumLit(a) => NumLit(fresh(a))
    | Neg(a) => Neg(fresh(a))
    | Plus(a, b) => Plus(fresh(a), fresh(b))
    | Minus(a, b) => Minus(fresh(a), fresh(b))
    | Times(a, b) => Times(fresh(a), fresh(b))
    | Lt(a, b) => Lt(fresh(a), fresh(b))
    | Gt(a, b) => Gt(fresh(a), fresh(b))
    | Eq(a, b) => Eq(fresh(a), fresh(b))
    | True => True
    | False => False
    | If(a, b, c) => If(fresh(a), fresh(b), fresh(c))
    | Var(a) => Var(fresh(a))
    | Let(a, b, c) => Let(fresh(a), fresh(b), fresh(c))
    | Fix(a, b) => Fix(fresh(a), fresh(b))
    | Fun(a, b) => Fun(fresh(a), fresh(b))
    | Ap(a, b) => Ap(fresh(a), fresh(b))
    | Pair(a, b) => Pair(fresh(a), fresh(b))
    | Triv => Triv
    | PrjL(a) => PrjL(fresh(a))
    | PrjR(a) => PrjR(fresh(a))
    | InjL(a) => InjL(fresh(a))
    | InjR(a) => InjR(fresh(a))
    | Case(a, b, c, d, e) =>
      Case(fresh(a), fresh(b), fresh(c), fresh(d), fresh(e))
    | Roll(a) => Roll(fresh(a))
    | Unroll(a) => Unroll(fresh(a))
    | Num => Num
    | Bool => Bool
    | Arrow(a, b) => Arrow(fresh(a), fresh(b))
    | Prod(a, b) => Prod(fresh(a), fresh(b))
    | Sum(a, b) => Sum(fresh(a), fresh(b))
    | Unit => Unit
    | TVar(a) => TVar(fresh(a))
    | Rec(a, b) => Rec(fresh(a), fresh(b))
    | Pat(a) => Pat(fresh(a))
    | Cast(a, b) => Cast(fresh(a), fresh(b))
    | PatStrip(a) => PatStrip(fresh(a))
    | PatPair(a, b) => PatPair(fresh(a), fresh(b))
    | TPat(a) => TPat(fresh(a))
    | Atom(a) => Atom(fresh(a))
    | And(a, b) => And(fresh(a), fresh(b))
    | Or(a, b) => Or(fresh(a), fresh(b))
    | Impl(a, b) => Impl(fresh(a), fresh(b))
    | Truth => Truth
    | Falsity => Falsity
  and fresh = term => term |> fresh_term |> IdTagged.fresh;
  (concl_spec |> fresh, prem_specs |> List.map(fresh));
};
