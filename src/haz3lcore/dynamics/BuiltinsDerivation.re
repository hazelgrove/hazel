open Derivation.Prop;

// The internal representation of Prop.t does not seperate the types. However,
// we could implement rules for frontend usage. The following code is a
// typeclass for Prop.t. Note that the typeclass is not used anywhere. It is
// just for reference.

// ALFA logic
type typ =
  | Num
  | Bool
  | Arrow(typ, typ)
  | Prod(typ, typ)
  | Unit
  | Sum(typ, typ)
  | TVar(string)
  | Rec(tpat, typ)
and tpat =
  | TPat(string);
type expr =
  | NumLit(int)
  | UnOp(unop, expr)
  | BinOp(binop, expr, expr)
  | True
  | False
  | If(expr, expr, expr)
  | Var(string)
  | Let(pat, expr, expr)
  | Fix(pat, expr)
  | Fun(pat, expr)
  | Ap(expr, expr)
  | Pair(expr, expr)
  | Triv
  | PrjL(expr)
  | PrjR(expr)
  | LetPair(pat, pat, expr, expr)
  | InjL(expr)
  | InjR(expr)
  | Case(expr, pat, expr, pat, expr)
  | Roll(expr)
  | Unroll(expr)
and pat =
  | Pat(string)
  | Ann(pat, typ)
and unop =
  | OpNeg
and binop =
  | OpLt
  | OpGt
  | OpEq
  | OpPlus
  | OpMinus
  | OpTimes;

type prop =
  // ALFp exclusive
  | HasType(expr, typ)
  | Syn(expr, typ)
  | Ana(expr, typ)
  // Propositional logic
  | Atom(string)
  | And(prop, prop)
  | Or(prop, prop)
  | Implies(prop, prop)
  | Truth
  | Falsity;

// Judgements (the outermost layer of this syntax)
type judgement =
  | Val(expr)
  | Eval(expr, expr)
  | Entail(list(prop), prop);

// The following code is the implementation of the typeclass for Prop.t.

[@deriving (show({with_path: false}), sexp, yojson)]
type alias =
  | Judgement
  | Type
  | TPat
  | Expr
  | Pat
  | UnOp
  | BinOp
  | Prop;

let alias_fresh: alias => Typ.t = alia => Var(show_alias(alia)) |> Typ.fresh;

let cls_to_arg_typ: cls => list(Typ.t) =
  fun
  | Hole => failwith("impossible")
  // ALFA
  | Sum => [Type |> alias_fresh, Type |> alias_fresh]
  | TVar => [String |> Typ.fresh]
  | Rec => [TPat |> alias_fresh, Type |> alias_fresh]
  | TPat => [String |> Typ.fresh]
  | Fix => [Pat |> alias_fresh, Expr |> alias_fresh]
  | InjL
  | InjR => [Expr |> alias_fresh]
  | Case => [
      Expr |> alias_fresh,
      Pat |> alias_fresh,
      Expr |> alias_fresh,
      Pat |> alias_fresh,
      Expr |> alias_fresh,
    ]
  | Roll
  | Unroll => [Expr |> alias_fresh]
  // ALFp
  | Num
  | Bool => []
  | Arrow
  | Prod => [Type |> alias_fresh, Type |> alias_fresh]
  | Unit => []
  | Pair => [Expr |> alias_fresh, Expr |> alias_fresh]
  | LetPair => [
      Pat |> alias_fresh,
      Pat |> alias_fresh,
      Expr |> alias_fresh,
      Expr |> alias_fresh,
    ]
  | PrjL
  | PrjR => [Expr |> alias_fresh]
  | Triv => []
  | Pat => [String |> Typ.fresh]
  | Ann => [String |> Typ.fresh, Type |> alias_fresh]
  // ALF
  | NumLit => [Int |> Typ.fresh]
  | True
  | False => []
  | If => [Expr |> alias_fresh, Expr |> alias_fresh, Expr |> alias_fresh]
  | Var => [String |> Typ.fresh]
  | Let => [Pat |> alias_fresh, Expr |> alias_fresh, Expr |> alias_fresh]
  | Fun => [Pat |> alias_fresh, Expr |> alias_fresh]
  | Ap => [Expr |> alias_fresh, Expr |> alias_fresh]
  | UnOp => [UnOp |> alias_fresh, Expr |> alias_fresh]
  | BinOp => [BinOp |> alias_fresh, Expr |> alias_fresh, Expr |> alias_fresh]
  | OpNeg
  | OpLt
  | OpGt
  | OpEq
  | OpPlus
  | OpMinus
  | OpTimes => []
  // Propositional logic
  | HasType => [Expr |> alias_fresh, Type |> alias_fresh]
  | Syn => [Expr |> alias_fresh, Type |> alias_fresh]
  | Ana => [Expr |> alias_fresh, Type |> alias_fresh]
  | Atom => [String |> Typ.fresh]
  | And
  | Or
  | Implies => [Prop |> alias_fresh, Prop |> alias_fresh]
  | Truth
  | Falsity => []
  | Ctx => failwith("impossible")
  // Judgements
  | Val => [Expr |> alias_fresh]
  | Eval => [Expr |> alias_fresh, Expr |> alias_fresh]
  | Entail => [List(Prop |> alias_fresh) |> Typ.fresh, Prop |> alias_fresh];

let cls_to_arg_typ: cls => option(Typ.t) =
  cls =>
    switch (cls_to_arg_typ(cls)) {
    | [] => None
    | [ty] => Some(ty)
    | tys => Some(Prod(tys) |> Typ.fresh)
    };

let cls_to_alias: cls => alias =
  fun
  | Hole => failwith("impossible")

  | Num
  | Bool
  | Arrow
  | Prod
  | Unit
  | Sum
  | TVar
  | Rec => Type
  | NumLit
  | UnOp
  | BinOp
  | True
  | False
  | If
  | Var
  | Let
  | Fix
  | Fun
  | Ap
  | Pair
  | Triv
  | PrjL
  | PrjR
  | LetPair
  | InjL
  | InjR
  | Case
  | Roll
  | Unroll => Expr
  | TPat => TPat
  | Pat
  | Ann => Pat
  | OpNeg => UnOp
  | OpLt
  | OpGt
  | OpEq
  | OpPlus
  | OpMinus
  | OpTimes => BinOp
  | HasType
  | Syn
  | Ana
  | Atom
  | And
  | Or
  | Implies
  | Truth
  | Falsity => Prop
  | Ctx => failwith("impossible")
  | Val
  | Eval
  | Entail => Judgement;

let cls_to_typ: cls => Typ.t =
  cls => {
    let alias_typ = cls_to_alias(cls) |> alias_fresh;
    switch (cls_to_arg_typ(cls)) {
    | None => alias_typ
    | Some(arg) => Arrow(arg, alias_typ) |> Typ.fresh
    };
  };

let all: list(cls) = [
  // ALFA logic
  Sum,
  TVar,
  Rec,
  TPat,
  Fix,
  InjL,
  InjR,
  Case,
  Roll,
  Unroll,
  // ALFp logic
  Num,
  Bool,
  Arrow,
  Prod,
  Unit,
  Pair,
  LetPair,
  PrjL,
  PrjR,
  Triv,
  Pat,
  Ann,
  HasType,
  // ALF logic
  NumLit,
  True,
  False,
  If,
  Var,
  Let,
  Fun,
  Ap,
  UnOp,
  BinOp,
  OpNeg,
  OpLt,
  OpGt,
  OpEq,
  OpPlus,
  OpMinus,
  OpTimes,
  // ALFA outer syntax
  Val,
  Eval,
  // Propositional logic
  Atom,
  And,
  Or,
  Implies,
  Truth,
  Falsity,
  Entail,
];

let mk_ctr_entry: cls => Ctx.var_entry =
  term => {name: show_cls(term), id: Id.mk(), typ: cls_to_typ(term)};

let ctr_entries = all |> List.map(mk_ctr_entry);

let mk_variant: cls => ConstructorMap.variant(Typ.t) =
  term => {
    Variant(show_cls(term), [Id.mk()], cls_to_arg_typ(term));
  };

let add_tvar_entries = ctx => {
  let rec add_to_list = (map, cls) => {
    let alias = cls_to_alias(cls);
    switch (map) {
    | [] => [(alias, [cls])]
    | [(alias', cls'), ...rest] when alias == alias' => [
        (alias, [cls, ...cls']),
        ...rest,
      ]
    | [pair, ...rest] => [pair, ...add_to_list(rest, cls)]
    };
  };
  let map = List.fold_left(add_to_list, [], all);
  List.fold_left(
    (ctx, (alias, cls)) => {
      let name = show_alias(alias);
      let ty = Sum(cls |> List.map(mk_variant)) |> Typ.fresh;
      let ty =
        switch (ty.term) {
        | Sum(_) when List.mem(name, Typ.free_vars(ty)) =>
          Typ.Rec(TPat.Var(name) |> IdTagged.fresh, ty) |> Typ.temp
        | _ => ty
        };
      let ctx = Ctx.extend_alias(ctx, name, Id.invalid, ty);
      switch (Typ.get_sum_constructors(ctx, ty)) {
      | Some(sm) => Ctx.add_ctrs(ctx, name, UTyp.rep_id(ty), sm)
      | None => ctx
      };
    },
    ctx,
    map,
  );
};

let to_term: string => option(cls) =
  s =>
    List.fold_left(
      (acc, ctr) =>
        Option.is_none(acc) && show_cls(ctr) == s ? Some(ctr) : acc,
      None,
      all,
    );

let term_of_dhexp: DHExp.t => option(cls) =
  d =>
    switch (DHExp.term_of(d)) {
    | Constructor(ctr, _) => ctr |> to_term
    | _ => None
    };

let rec prop_of_dhexp: DHExp.t => t =
  d => {
    let d = DHExp.strip_casts(d);
    let (fn, arg) =
      switch (DHExp.term_of(d)) {
      | Ap(_, fn, arg) => (fn, Some(arg))
      | _ => (d, None)
      };
    switch (term_of_dhexp(fn)) {
    | None => Hole("Cls not found") |> fresh
    | Some(ctr) => match_dhexp(ctr, arg)
    };
  }

and match_dhexp: (cls, option(DHExp.t)) => t =
  (ctr, arg) => {
    let (let.) = (x, f) =>
      switch (x) {
      | Some(x) => f(x)
      | None => Hole("Argument Error") |> fresh
      };
    let arg =
      switch (arg) {
      | None => []
      | Some(arg) =>
        switch (DHExp.term_of(arg)) {
        | Tuple(args) => args
        | _ => [arg]
        }
      };
    switch (ctr, arg) {
    | (Hole, _)
    | (Ctx, _) => Hole("Unexpected Ctr") |> fresh
    // ALFA logic
    | (Sum, [d1, d2]) =>
      let (e1, e2) = (prop_of_dhexp(d1), prop_of_dhexp(d2));
      Sum(e1, e2) |> fresh;
    | (TVar, [d]) =>
      let. var =
        switch (DHExp.term_of(d)) {
        | String(var) => Some(var)
        | _ => None
        };
      TVar(var) |> fresh;
    | (Rec, [d1, d2]) =>
      let (pat, e) = (prop_of_dhexp(d1), prop_of_dhexp(d2));
      Rec(pat, e) |> fresh;
    | (TPat, [d]) =>
      let. pat =
        switch (DHExp.term_of(d)) {
        | String(pat) => Some(pat)
        | _ => None
        };
      TPat(pat) |> fresh;
    | (Fix, [d1, d2]) =>
      let (pat, e) = (prop_of_dhexp(d1), prop_of_dhexp(d2));
      Fix(pat, e) |> fresh;
    | (InjL, [d]) =>
      let e = prop_of_dhexp(d);
      InjL(e) |> fresh;
    | (InjR, [d]) =>
      let e = prop_of_dhexp(d);
      InjR(e) |> fresh;
    | (Case, [d1, d2, d3, d4, d5]) =>
      let (e1, pat1, e2, pat2, e3) = (
        prop_of_dhexp(d1),
        prop_of_dhexp(d2),
        prop_of_dhexp(d3),
        prop_of_dhexp(d4),
        prop_of_dhexp(d5),
      );
      Case(e1, pat1, e2, pat2, e3) |> fresh;
    | (Roll, [d]) =>
      let e = prop_of_dhexp(d);
      Roll(e) |> fresh;
    | (Unroll, [d]) =>
      let e = prop_of_dhexp(d);
      Unroll(e) |> fresh;
    // ALFp logic
    | (Num, []) => Num |> fresh
    | (Bool, []) => Bool |> fresh
    | (Arrow, [d1, d2]) =>
      let (e1, e2) = (prop_of_dhexp(d1), prop_of_dhexp(d2));
      Arrow(e1, e2) |> fresh;
    | (Prod, [d1, d2]) =>
      let (e1, e2) = (prop_of_dhexp(d1), prop_of_dhexp(d2));
      Prod(e1, e2) |> fresh;
    | (Unit, []) => Unit |> fresh
    | (Pair, [d1, d2]) =>
      let (e1, e2) = (prop_of_dhexp(d1), prop_of_dhexp(d2));
      Pair(e1, e2) |> fresh;
    | (LetPair, [d1, d2, d3, d4]) =>
      let (pat1, pat2, e1, e2) = (
        prop_of_dhexp(d1),
        prop_of_dhexp(d2),
        prop_of_dhexp(d3),
        prop_of_dhexp(d4),
      );
      LetPair(pat1, pat2, e1, e2) |> fresh;
    | (PrjL, [d]) =>
      let e = prop_of_dhexp(d);
      PrjL(e) |> fresh;
    | (PrjR, [d]) =>
      let e = prop_of_dhexp(d);
      PrjR(e) |> fresh;
    | (Triv, []) => Triv |> fresh
    | (Pat, [d]) =>
      let. pat =
        switch (DHExp.term_of(d)) {
        | String(pat) => Some(pat)
        | _ => None
        };
      Pat(pat) |> fresh;
    | (Ann, [d1, d2]) =>
      let. pat =
        switch (DHExp.term_of(d1)) {
        | String(pat) => Some(pat)
        | _ => None
        };
      let ty = prop_of_dhexp(d2);
      Ann(Pat(pat) |> fresh, ty) |> fresh;
    | (HasType, [d1, d2]) =>
      let (e, ty) = (prop_of_dhexp(d1), prop_of_dhexp(d2));
      HasType(e, ty) |> fresh;
    // ALF logic
    | (NumLit, [d]) =>
      let. n =
        switch (DHExp.term_of(d)) {
        | Int(n) => Some(n)
        | _ => None
        };
      NumLit(n) |> fresh;
    | (True, []) => True |> fresh
    | (False, []) => False |> fresh
    | (If, [d1, d2, d3]) =>
      let (e1, e2, e3) = (
        prop_of_dhexp(d1),
        prop_of_dhexp(d2),
        prop_of_dhexp(d3),
      );
      If(e1, e2, e3) |> fresh;
    | (Var, [d]) =>
      let. var =
        switch (DHExp.term_of(d)) {
        | String(var) => Some(var)
        | _ => None
        };
      Var(var) |> fresh;
    | (Let, [d1, d2, d3]) =>
      let (pat, e1, e2) = (
        prop_of_dhexp(d1),
        prop_of_dhexp(d2),
        prop_of_dhexp(d3),
      );
      Let(pat, e1, e2) |> fresh;
    | (Fun, [d1, d2]) =>
      let (pat, e) = (prop_of_dhexp(d1), prop_of_dhexp(d2));
      Fun(pat, e) |> fresh;
    | (Ap, [d1, d2]) =>
      let (e1, e2) = (prop_of_dhexp(d1), prop_of_dhexp(d2));
      Ap(e1, e2) |> fresh;
    | (UnOp, [d1, d2]) =>
      let (op, e) = (prop_of_dhexp(d1), prop_of_dhexp(d2));
      UnOp(op, e) |> fresh;
    | (BinOp, [d1, d2, d3]) =>
      let (op, e1, e2) = (
        prop_of_dhexp(d1),
        prop_of_dhexp(d2),
        prop_of_dhexp(d3),
      );
      BinOp(op, e1, e2) |> fresh;
    | (OpNeg, []) => OpNeg |> fresh
    | (OpLt, []) => OpLt |> fresh
    | (OpGt, []) => OpGt |> fresh
    | (OpEq, []) => OpEq |> fresh
    | (OpPlus, []) => OpPlus |> fresh
    | (OpMinus, []) => OpMinus |> fresh
    | (OpTimes, []) => OpTimes |> fresh
    | (Syn, [d1, d2]) =>
      let (e, ty) = (prop_of_dhexp(d1), prop_of_dhexp(d2));
      Syn(e, ty) |> fresh;
    | (Ana, [d1, d2]) =>
      let (e, ty) = (prop_of_dhexp(d1), prop_of_dhexp(d2));
      Ana(e, ty) |> fresh;
    | (Val, [d]) => Val(prop_of_dhexp(d)) |> fresh
    | (Eval, [d1, d2]) =>
      let (e1, e2) = (prop_of_dhexp(d1), prop_of_dhexp(d2));
      Eval(e1, e2) |> fresh;

    | (Sum, _)
    | (TVar, _)
    | (Rec, _)
    | (TPat, _)
    | (Fix, _)
    | (InjL, _)
    | (InjR, _)
    | (Case, _)
    | (Roll, _)
    | (Unroll, _)
    | (Num, _)
    | (Bool, _)
    | (Arrow, _)
    | (Prod, _)
    | (Unit, _)
    | (Pair, _)
    | (LetPair, _)
    | (PrjL, _)
    | (PrjR, _)
    | (Triv, _)
    | (Pat, _)
    | (Ann, _)
    | (HasType, _)
    | (NumLit, _)
    | (True, _)
    | (False, _)
    | (If, _)
    | (Var, _)
    | (Let, _)
    | (Fun, _)
    | (Ap, _)
    | (UnOp, _)
    | (BinOp, _)
    | (OpNeg, _)
    | (OpLt, _)
    | (OpGt, _)
    | (OpEq, _)
    | (OpPlus, _)
    | (OpMinus, _)
    | (OpTimes, _)
    | (Syn, _)
    | (Ana, _)
    | (Val, _)
    | (Eval, _) => Hole("Argument Error") |> fresh

    | (Atom, [d]) =>
      let. atom =
        switch (DHExp.term_of(d)) {
        | String(atom) => Some(atom)
        | _ => None
        };
      Atom(atom) |> fresh;
    | (And, [d1, d2]) =>
      let (p1, p2) = (prop_of_dhexp(d1), prop_of_dhexp(d2));
      And(p1, p2) |> fresh;
    | (Or, [d1, d2]) =>
      let (p1, p2) = (prop_of_dhexp(d1), prop_of_dhexp(d2));
      Or(p1, p2) |> fresh;
    | (Implies, [d1, d2]) =>
      let (p1, p2) = (prop_of_dhexp(d1), prop_of_dhexp(d2));
      Implies(p1, p2) |> fresh;
    | (Truth, []) => Truth |> fresh
    | (Falsity, []) => Falsity |> fresh
    | (Entail, [d1, d2]) =>
      let. ctx =
        switch (DHExp.term_of(d1)) {
        | ListLit(l) => Some(l)
        | _ => None
        };
      let ctx = List.map(prop_of_dhexp, ctx);
      let p = prop_of_dhexp(d2);
      Entail(Ctx(ctx) |> fresh, p) |> fresh;
    | (Atom, _)
    | (And, _)
    | (Or, _)
    | (Implies, _)
    | (Truth, _)
    | (Falsity, _)
    | (Entail, _) => Hole("Argument Error") |> fresh
    };
  };
