open Util;

exception Unreachable;

/*

 Prop.t is used as the internal representation of a judgement expression. It is
 converted from the evaluation result of an editor. An editor that is requested
 to be evaluated to Prop should have its result be Prop type Constructor.

 */

[@deriving (show({with_path: false}), sexp, yojson)]
type deduction('a) = {
  concl: 'a,
  prems: list('a),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type term =
  | Hole(string) // When DHExp.t not convertable, convert by `e => Hole(DHExp.show(e))`
  // Judgments
  | Val(t)
  | Eval(t, t)
  | Entail(t, t)
  // Proposition
  | HasTy(t, t)
  | Syn(t, t)
  | Ana(t, t)
  | Atom(string)
  | And(t, t)
  | Or(t, t)
  | Impl(t, t)
  | Truth
  | Falsity
  // Ctx
  | Ctx(list(t))
  // ALFA Exp
  | NumLit(int)
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
  | Var(string)
  | Let(t, t, t)
  | LetAnn(t, t, t, t)
  | Fix(t, t)
  | FixAnn(t, t, t)
  | Fun(t, t)
  | FunAnn(t, t, t)
  | Ap(t, t)
  | Pair(t, t)
  | Triv
  | PrjL(t)
  | PrjR(t)
  | LetPair(t, t, t, t)
  | InjL(t)
  | InjR(t)
  | Case(t, t, t, t, t)
  | Roll(t)
  | Unroll(t)
  // ALFA Pat
  | Pat(string)
  // ALFA Typ
  | Num
  | Bool
  | Arrow(t, t)
  | Prod(t, t)
  | Unit
  | Sum(t, t)
  | TVar(string)
  | Rec(t, t)
  // ALFA TPat
  | TPat(string)
and t = IdTagged.t(term);

let fresh = (term: term) => IdTagged.fresh(term);

let temp = (term: term) =>
  IdTagged.{term, ids: [Id.invalid], copied: false};

[@deriving (show({with_path: false}), sexp, yojson)]
type cls =
  | Hole
  | Val
  | Eval
  | Entail
  | HasTy
  | Syn
  | Ana
  | Atom
  | And
  | Or
  | Impl
  | Truth
  | Falsity
  | Ctx
  | NumLit
  | Neg
  | Plus
  | Minus
  | Times
  | Lt
  | Gt
  | Eq
  | True
  | False
  | If
  | Var
  | Let
  | LetAnn
  | Fix
  | FixAnn
  | Fun
  | FunAnn
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
  | Unroll
  | Pat
  | Num
  | Bool
  | Arrow
  | Prod
  | Unit
  | Sum
  | TVar
  | Rec
  | TPat;

module P = Precedence;

let precedence: t => int =
  p =>
    switch (IdTagged.term_of(p)) {
    | Hole(_) => P.max
    | Val(_) => P.filter
    | Eval(_) => P.filter
    | Entail(_) => P.filter
    | HasTy(_) => P.semi
    | Syn(_) => P.semi
    | Ana(_) => P.semi
    | Atom(_) => P.max
    | And(_) => P.and_
    | Or(_) => P.or_
    | Impl(_) => P.ann
    | Truth => P.max
    | Falsity => P.max
    | Ctx(_) => P.max
    // ALFA Exp
    | NumLit(_) => P.max
    | Neg(_) => P.neg
    | Plus(_) => P.plus
    | Minus(_) => P.plus
    | Times(_) => P.mult
    | Lt(_) => P.eqs
    | Gt(_) => P.eqs
    | Eq(_) => P.eqs
    | True => P.max
    | False => P.max
    | If(_) => P.if_
    | Var(_) => P.max
    | Let(_) => P.let_
    | LetAnn(_) => P.let_
    | Fix(_) => P.fun_
    | FixAnn(_) => P.fun_
    | Fun(_) => P.fun_
    | FunAnn(_) => P.fun_
    | Ap(_) => P.ap
    | Pair(_) => P.comma
    | Triv => P.max
    | PrjL(_) => P.ap
    | PrjR(_) => P.ap
    | LetPair(_) => P.let_
    | InjL(_) => P.ap
    | InjR(_) => P.ap
    | Case(_) => P.fun_
    | Roll(_) => P.ap
    | Unroll(_) => P.ap
    | Pat(_) => P.max
    | Num => P.max
    | Bool => P.max
    | Arrow(_) => P.type_arrow
    | Prod(_) => P.type_prod
    | Unit => P.max
    | Sum(_) => P.type_plus
    | TVar(_) => P.max
    | Rec(_) => P.fun_
    | TPat(_) => P.max
    };

let rec repr = (p: int, prop: t): string => {
  let p' = precedence(prop);
  let repr = repr(p');
  let repr_aba = (as_: list(string), bs: list(t)) =>
    Aba.mk(as_, bs)
    |> Aba.join(Fun.id, repr)
    |> String.concat(" ")
    |> String.trim;
  let repr_aba_tight = (as_: list(string), bs: list(t)) =>
    Aba.mk(as_, bs) |> Aba.join(Fun.id, repr) |> String.concat("");
  let repr_binop = (op: string, a: t, b: t) =>
    repr_aba(["", op, ""], [a, b]);
  let repr_preop = (op: string, a: t) => repr_aba([op, ""], [a]);
  let repr_postop = (op: string, a: t) => repr_aba(["", op], [a]);
  (
    switch (IdTagged.term_of(prop)) {
    | Hole(s) => Printf.sprintf("[%s]", s)
    | Atom(s) => s
    | And(a, b) => repr_binop("∧", a, b)
    | Or(a, b) => repr_binop("∨", a, b)
    | Impl(a, b) when IdTagged.term_of(b) == Falsity => repr_postop("¬", a)
    | Impl(a, b) => repr_binop("⊃", a, b)
    | Truth => "⊤"
    | Falsity => "⊥"
    | Ctx(ctx) =>
      if (List.length(ctx) == 0) {
        "·";
      } else {
        ctx
        |> List.map(repr)
        |> String.concat(", ")
        |> Printf.sprintf("[%s]");
      }
    | Entail(a, b) => repr_binop("⊢", a, b)
    | NumLit(i) => string_of_int(i)
    | Val(a) => repr_postop(".val", a)
    | Neg(a) => repr_preop("-", a)
    | Plus(a, b) => repr_binop("+", a, b)
    | Minus(a, b) => repr_binop("-", a, b)
    | Times(a, b) => repr_binop("*", a, b)
    | Lt(a, b) => repr_binop("<", a, b)
    | Gt(a, b) => repr_binop(">", a, b)
    | Eq(a, b) => repr_binop("==", a, b)
    | Eval(a, b) => repr_binop("⇓", a, b)
    | Num => "Num"
    | Bool => "Bool"
    | Arrow(a, b) => repr_binop("→", a, b)
    | Prod(a, b) => repr_binop("×", a, b)
    | Unit => "Unit"
    | Sum(a, b) => repr_binop("+", a, b)
    | TVar(x) => x
    | Rec(x, a) => repr_aba(["rec", "→", ""], [x, a])
    | True => "True"
    | False => "False"
    | If(a, b, c) => repr_aba(["if", "then", "else", ""], [a, b, c])
    | Var(x) => x
    | Let(x, a, b) => repr_aba(["let", "→", "in", ""], [x, a, b])
    | LetAnn(x, t, a, b) =>
      repr_aba(["let", ":", "→", "in", ""], [x, t, a, b])
    | Fix(x, a) => repr_aba(["fix", "→", ""], [x, a])
    | FixAnn(x, t, a) => repr_aba(["fix", ":", "→", ""], [x, t, a])
    | Fun(x, a) => repr_aba(["fun", "→", ""], [x, a])
    | FunAnn(x, t, a) => repr_aba(["fun", ":", "→", ""], [x, t, a])
    | Ap(a, b) => repr_aba_tight(["", "(", ")"], [a, b])
    | Pair(a, b) => repr_aba_tight(["(", ",", ")"], [a, b])
    | Triv => "()"
    | PrjL(a) => repr_postop(".fst", a)
    | PrjR(a) => repr_postop(".snd", a)
    | LetPair(x, y, a, b) =>
      repr_aba(["let (", ",", ") =", "in", ""], [x, y, a, b])
    | InjL(a) => repr_preop("L", a)
    | InjR(a) => repr_preop("R", a)
    | Case(a, x, b, y, c) =>
      repr_aba(
        ["case", "of L", "→", "else R", "→", ""],
        [a, x, b, y, c],
      )
    | Roll(a) => repr_aba_tight(["roll(", ")"], [a])
    | Unroll(a) => repr_aba_tight(["unroll(", ")"], [a])
    | TPat(x) => x
    | Pat(x) => x
    | HasTy(a, b) => repr_binop(":", a, b)
    | Syn(a, b) => repr_binop("⇒", a, b)
    | Ana(a, b) => repr_binop("⇐", a, b)
    }
  )
  |> (p < p' ? Printf.sprintf("(%s)") : Fun.id);
};

let repr = repr(P.min);

let rec eq: (t, t) => bool =
  (a, b) =>
    switch (IdTagged.term_of(a), IdTagged.term_of(b)) {
    | (Hole(_), _) => false
    | (Ctx(as_), Ctx(bs)) =>
      List.length(as_) == List.length(bs) && List.for_all2(eq, as_, bs)
    | (Ctx(_), _) => false
    | (Num, Num) => true
    | (Num, _) => false
    | (Bool, Bool) => true
    | (Bool, _) => false
    | (Arrow(a1, a2), Arrow(b1, b2)) => eq(a1, b1) && eq(a2, b2)
    | (Arrow(_), _) => false
    | (Prod(a1, a2), Prod(b1, b2)) => eq(a1, b1) && eq(a2, b2)
    | (Prod(_), _) => false
    | (Unit, Unit) => true
    | (Unit, _) => false
    | (Sum(a1, a2), Sum(b1, b2)) => eq(a1, b1) && eq(a2, b2)
    | (Sum(_), _) => false
    | (TVar(a), TVar(b)) => String.equal(a, b)
    | (TVar(_), _) => false
    | (Rec(a1, a2), Rec(b1, b2)) => eq(a1, b1) && eq(a2, b2)
    | (Rec(_), _) => false
    | (NumLit(a), NumLit(b)) => Int.equal(a, b)
    | (NumLit(_), _) => false
    | (Neg(a), Neg(b)) => eq(a, b)
    | (Neg(_), _) => false
    | (Plus(a1, a2), Plus(b1, b2)) => eq(a1, b1) && eq(a2, b2)
    | (Plus(_), _) => false
    | (Minus(a1, a2), Minus(b1, b2)) => eq(a1, b1) && eq(a2, b2)
    | (Minus(_), _) => false
    | (Times(a1, a2), Times(b1, b2)) => eq(a1, b1) && eq(a2, b2)
    | (Times(_), _) => false
    | (Lt(a1, a2), Lt(b1, b2)) => eq(a1, b1) && eq(a2, b2)
    | (Lt(_), _) => false
    | (Gt(a1, a2), Gt(b1, b2)) => eq(a1, b1) && eq(a2, b2)
    | (Gt(_), _) => false
    | (Eq(a1, a2), Eq(b1, b2)) => eq(a1, b1) && eq(a2, b2)
    | (Eq(_), _) => false
    | (True, True) => true
    | (True, _) => false
    | (False, False) => true
    | (False, _) => false
    | (If(a1, a2, a3), If(b1, b2, b3)) =>
      eq(a1, b1) && eq(a2, b2) && eq(a3, b3)
    | (If(_), _) => false
    | (Var(a), Var(b)) => String.equal(a, b)
    | (Var(_), _) => false
    | (Let(a1, a2, a3), Let(b1, b2, b3)) =>
      eq(a1, b1) && eq(a2, b2) && eq(a3, b3)
    | (Let(_), _) => false
    | (LetAnn(a1, a2, a3, a4), LetAnn(b1, b2, b3, b4)) =>
      eq(a1, b1) && eq(a2, b2) && eq(a3, b3) && eq(a4, b4)
    | (LetAnn(_), _) => false
    | (Fix(a1, a2), Fix(b1, b2)) => eq(a1, b1) && eq(a2, b2)
    | (Fix(_), _) => false
    | (FixAnn(a1, a2, a3), FixAnn(b1, b2, b3)) =>
      eq(a1, b1) && eq(a2, b2) && eq(a3, b3)
    | (FixAnn(_), _) => false
    | (Fun(a1, a2), Fun(b1, b2)) => eq(a1, b1) && eq(a2, b2)
    | (Fun(_), _) => false
    | (FunAnn(a1, a2, a3), FunAnn(b1, b2, b3)) =>
      eq(a1, b1) && eq(a2, b2) && eq(a3, b3)
    | (FunAnn(_), _) => false
    | (Ap(a1, a2), Ap(b1, b2)) => eq(a1, b1) && eq(a2, b2)
    | (Ap(_), _) => false
    | (Pair(a1, a2), Pair(b1, b2)) => eq(a1, b1) && eq(a2, b2)
    | (Pair(_), _) => false
    | (Triv, Triv) => true
    | (Triv, _) => false
    | (PrjL(a), PrjL(b)) => eq(a, b)
    | (PrjL(_), _) => false
    | (PrjR(a), PrjR(b)) => eq(a, b)
    | (PrjR(_), _) => false
    | (LetPair(a1, a2, a3, a4), LetPair(b1, b2, b3, b4)) =>
      eq(a1, b1) && eq(a2, b2) && eq(a3, b3) && eq(a4, b4)
    | (LetPair(_), _) => false
    | (InjL(a), InjL(b)) => eq(a, b)
    | (InjL(_), _) => false
    | (InjR(a), InjR(b)) => eq(a, b)
    | (InjR(_), _) => false
    | (Case(a1, a2, a3, a4, a5), Case(b1, b2, b3, b4, b5)) =>
      eq(a1, b1) && eq(a2, b2) && eq(a3, b3) && eq(a4, b4) && eq(a5, b5)
    | (Case(_), _) => false
    | (Roll(a), Roll(b)) => eq(a, b)
    | (Roll(_), _) => false
    | (Unroll(a), Unroll(b)) => eq(a, b)
    | (Unroll(_), _) => false
    | (TPat(a), TPat(b)) => String.equal(a, b)
    | (TPat(_), _) => false
    | (Pat(a), Pat(b)) => String.equal(a, b)
    | (Pat(_), _) => false
    | (HasTy(a1, a2), HasTy(b1, b2)) => eq(a1, b1) && eq(a2, b2)
    | (HasTy(_), _) => false
    | (Syn(a1, a2), Syn(b1, b2)) => eq(a1, b1) && eq(a2, b2)
    | (Syn(_), _) => false
    | (Ana(a1, a2), Ana(b1, b2)) => eq(a1, b1) && eq(a2, b2)
    | (Ana(_), _) => false
    | (Atom(a), Atom(b)) => String.equal(a, b)
    | (Atom(_), _) => false
    | (And(a1, a2), And(b1, b2)) => eq(a1, b1) && eq(a2, b2)
    | (And(_), _) => false
    | (Or(a1, a2), Or(b1, b2)) => eq(a1, b1) && eq(a2, b2)
    | (Or(_), _) => false
    | (Impl(a1, a2), Impl(b1, b2)) => eq(a1, b1) && eq(a2, b2)
    | (Impl(_), _) => false
    | (Truth, Truth) => true
    | (Truth, _) => false
    | (Falsity, Falsity) => true
    | (Falsity, _) => false
    | (Val(a), Val(b)) => eq(a, b)
    | (Val(_), _) => false
    | (Eval(a1, a2), Eval(b1, b2)) => eq(a1, b1) && eq(a2, b2)
    | (Eval(_), _) => false
    | (Entail(a1, a2), Entail(b1, b2)) => eq(a1, b1) && eq(a2, b2)
    | (Entail(_), _) => false
    };

let rec subst: (t, string, t) => t =
  (v, x, e) => {
    let (term, rewrap: term => t) = IdTagged.unwrap(e);
    let subst = subst(v, x);
    let is_shadow = (p: t) =>
      switch (IdTagged.term_of(p)) {
      | Pat(x') => String.equal(x', x)
      | _ => false
      };
    let subst' = p => is_shadow(p) ? Fun.id : subst;
    switch (term) {
    | Hole(_)
    | Ctx(_) => e // The usage of Ctx is outside of Exp
    // Typ
    | Num
    | Bool
    | Arrow(_)
    | Prod(_)
    | Unit
    | Sum(_)
    | TVar(_)
    | Rec(_) => e
    // Exp
    | NumLit(_) => e
    | Neg(e) => Neg(subst(e)) |> rewrap
    | Plus(e1, e2) => Plus(subst(e1), subst(e2)) |> rewrap
    | Minus(e1, e2) => Minus(subst(e1), subst(e2)) |> rewrap
    | Times(e1, e2) => Times(subst(e1), subst(e2)) |> rewrap
    | Lt(e1, e2) => Lt(subst(e1), subst(e2)) |> rewrap
    | Gt(e1, e2) => Gt(subst(e1), subst(e2)) |> rewrap
    | Eq(e1, e2) => Eq(subst(e1), subst(e2)) |> rewrap
    | True
    | False => e
    | If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3)) |> rewrap
    | Var(x') => String.equal(x', x) ? v : e
    | Let(x, e1, e2) => Let(x, subst(e1), subst'(x, e2)) |> rewrap
    | LetAnn(x, t, e1, e2) =>
      LetAnn(x, t, subst(e1), subst'(x, e2)) |> rewrap
    | Fix(x, e) => Fix(x, subst'(x, e)) |> rewrap
    | FixAnn(x, t, e) => FixAnn(x, t, subst'(x, e)) |> rewrap
    | Fun(x, e) => Fun(x, subst'(x, e)) |> rewrap
    | FunAnn(x, t, e) => FunAnn(x, t, subst'(x, e)) |> rewrap
    | Ap(e1, e2) => Ap(subst(e1), subst(e2)) |> rewrap
    | Pair(e1, e2) => Pair(subst(e1), subst(e2)) |> rewrap
    | Triv => e
    | PrjL(e) => PrjL(subst(e)) |> rewrap
    | PrjR(e) => PrjR(subst(e)) |> rewrap
    | LetPair(x, y, e1, e2) =>
      LetPair(
        x,
        y,
        subst(e1),
        is_shadow(x) || is_shadow(y) ? e2 : subst(e2),
      )
      |> rewrap
    | InjL(e) => InjL(subst(e)) |> rewrap
    | InjR(e) => InjR(subst(e)) |> rewrap
    | Case(e1, x, e2, y, e3) =>
      Case(subst(e1), x, subst'(x, e2), y, subst'(y, e3)) |> rewrap
    | Roll(e) => Roll(subst(e)) |> rewrap
    | Unroll(e) => Unroll(subst(e)) |> rewrap
    // Meta
    | TPat(_)
    | Pat(_)
    // Proposition
    | HasTy(_)
    | Syn(_)
    | Ana(_) => e
    // Logic
    | Atom(_)
    | And(_)
    | Or(_)
    | Impl(_)
    | Truth
    | Falsity => e
    // Judgments
    | Val(_)
    | Eval(_)
    | Entail(_) => e
    };
  };

let rec subst_ty: (t, string, t) => t =
  (v, x, e) => {
    let (term, rewrap: term => t) = IdTagged.unwrap(e);
    let subst_ty = subst_ty(v, x);
    let is_shadow = (p: t) =>
      switch (IdTagged.term_of(p)) {
      | TPat(x') => String.equal(x', x)
      | _ => false
      };
    let subst_ty' = p => is_shadow(p) ? Fun.id : subst_ty;
    switch (term) {
    | Hole(_)
    | Ctx(_) => e
    // Typ
    | Num
    | Bool => e
    | Arrow(t1, t2) => Arrow(subst_ty(t1), subst_ty(t2)) |> rewrap
    | Prod(t1, t2) => Prod(subst_ty(t1), subst_ty(t2)) |> rewrap
    | Unit => e
    | Sum(t1, t2) => Sum(subst_ty(t1), subst_ty(t2)) |> rewrap
    | TVar(x') => String.equal(x', x) ? v : e
    | Rec(x, t1) => Rec(x, subst_ty'(x, t1)) |> rewrap
    // Exp
    | NumLit(_) => e
    | Neg(e) => Neg(subst_ty(e)) |> rewrap
    | Plus(e1, e2) => Plus(subst_ty(e1), subst_ty(e2)) |> rewrap
    | Minus(e1, e2) => Minus(subst_ty(e1), subst_ty(e2)) |> rewrap
    | Times(e1, e2) => Times(subst_ty(e1), subst_ty(e2)) |> rewrap
    | Lt(e1, e2) => Lt(subst_ty(e1), subst_ty(e2)) |> rewrap
    | Gt(e1, e2) => Gt(subst_ty(e1), subst_ty(e2)) |> rewrap
    | Eq(e1, e2) => Eq(subst_ty(e1), subst_ty(e2)) |> rewrap
    | True
    | False => e
    | If(e1, e2, e3) =>
      If(subst_ty(e1), subst_ty(e2), subst_ty(e3)) |> rewrap
    | Var(_) => e
    | Let(x, e1, e2) => Let(x, subst_ty(e1), subst_ty(e2)) |> rewrap
    | LetAnn(x, t, e1, e2) =>
      LetAnn(x, subst_ty(t), subst_ty(e1), subst_ty(e2)) |> rewrap
    | Fix(x, e) => Fix(x, subst_ty(e)) |> rewrap
    | FixAnn(x, t, e) => FixAnn(x, subst_ty(t), subst_ty(e)) |> rewrap
    | Fun(x, e) => Fun(x, subst_ty(e)) |> rewrap
    | FunAnn(x, t, e) => FunAnn(x, subst_ty(t), subst_ty(e)) |> rewrap
    | Ap(e1, e2) => Ap(subst_ty(e1), subst_ty(e2)) |> rewrap
    | Pair(e1, e2) => Pair(subst_ty(e1), subst_ty(e2)) |> rewrap
    | Triv => e
    | PrjL(e) => PrjL(subst_ty(e)) |> rewrap
    | PrjR(e) => PrjR(subst_ty(e)) |> rewrap
    | LetPair(x, y, e1, e2) =>
      LetPair(subst_ty(x), subst_ty(y), subst_ty(e1), subst_ty(e2))
      |> rewrap
    | InjL(e) => InjL(subst_ty(e)) |> rewrap
    | InjR(e) => InjR(subst_ty(e)) |> rewrap
    | Case(e1, x, e2, y, e3) =>
      Case(
        subst_ty(e1),
        subst_ty(x),
        subst_ty(e2),
        subst_ty(y),
        subst_ty(e3),
      )
      |> rewrap
    | Roll(e) => Roll(subst_ty(e)) |> rewrap
    | Unroll(e) => Unroll(subst_ty(e)) |> rewrap
    // Meta
    | TPat(_)
    | Pat(_) => e
    // Proposition
    | HasTy(_)
    | Syn(_)
    | Ana(_) => e
    // Logic
    | Atom(_)
    | And(_)
    | Or(_)
    | Impl(_)
    | Truth
    | Falsity => e
    // Judgments
    | Val(_)
    | Eval(_)
    | Entail(_) => e
    };
  };

let mem_ctx = p => List.exists(eq(p));

let cons_ctx = p => {
  let cmp = p' => show(p') <= show(p);
  let rec insert =
    fun
    | [] => [p]
    | [hd, ...tl] => cmp(hd) ? [p, hd, ...tl] : [hd, ...insert(tl)];
  insert;
};
