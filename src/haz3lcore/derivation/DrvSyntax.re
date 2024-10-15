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
  | Type(t)
  | HasType(t, t)
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

let fresh: term => t = IdTagged.fresh;

let term_of: t => term = IdTagged.term_of;

let unwrap: t => (term, term => t) = IdTagged.unwrap;

let temp = (term: term) =>
  IdTagged.{term, ids: [Id.invalid], copied: false};

[@deriving (show({with_path: false}), sexp, yojson)]
type cls =
  | Hole
  | Val
  | Eval
  | Entail
  | Type
  | HasType
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

// Note(zhiyao): We have a little bit different precedence for printing
// compared to the parser. This is because we want to make the printed
// syntax more readable, while making writing easier.

let precedence: t => int =
  p =>
    switch (IdTagged.term_of(p)) {
    | Hole(_) => P.max
    | Val(_) => P.filter
    | Eval(_) => P.filter
    | Entail(_) => P.filter
    | Type(_) => P.ann
    | HasType(_) => P.ann
    | Syn(_) => P.ann
    | Ana(_) => P.ann
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
    | Pair(_) => P.max
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
    | Prod(_) => P.type_arrow + 1
    | Unit => P.max
    | Sum(_) => P.type_plus
    | TVar(_) => P.max
    | Rec(_) => P.type_arrow + 2
    | TPat(_) => P.max
    };

let children = (syntax: t): list(t) =>
  switch (term_of(syntax)) {
  | Hole(_) => []
  | Val(a) => [a]
  | Eval(a, b) => [a, b]
  | Entail(a, b) => [a, b]
  | Type(a) => [a]
  | HasType(a, b) => [a, b]
  | Syn(a, b) => [a, b]
  | Ana(a, b) => [a, b]
  | Atom(_) => []
  | And(a, b) => [a, b]
  | Or(a, b) => [a, b]
  | Impl(a, {term: Falsity, _}) => [a]
  | Impl(a, b) => [a, b]
  | Truth => []
  | Falsity => []
  | Ctx(ctx) => ctx
  // ALFA Exp
  | NumLit(_) => []
  | Neg(a) => [a]
  | Plus(a, b) => [a, b]
  | Minus(a, b) => [a, b]
  | Times(a, b) => [a, b]
  | Lt(a, b) => [a, b]
  | Gt(a, b) => [a, b]
  | Eq(a, b) => [a, b]
  | True => []
  | False => []
  | If(a, b, c) => [a, b, c]
  | Var(_) => []
  | Let(a, b, c) => [a, b, c]
  | LetAnn(a, b, c, d) => [a, b, c, d]
  | Fix(a, b) => [a, b]
  | FixAnn(a, b, c) => [a, b, c]
  | Fun(a, b) => [a, b]
  | FunAnn(a, b, c) => [a, b, c]
  | Ap(a, b) => [a, b]
  | Pair(a, b) => [a, b]
  | Triv => []
  | PrjL(a) => [a]
  | PrjR(a) => [a]
  | LetPair(a, b, c, d) => [a, b, c, d]
  | InjL(a) => [a]
  | InjR(a) => [a]
  | Case(a, b, c, d, e) => [a, b, c, d, e]
  | Roll(a) => [a]
  | Unroll(a) => [a]
  // ALFA Pat
  | Pat(_) => []
  // ALFA Typ
  | Num => []
  | Bool => []
  | Arrow(a, b) => [a, b]
  | Prod(a, b) => [a, b]
  | Unit => []
  | Sum(a, b) => [a, b]
  | TVar(_) => []
  | Rec(a, b) => [a, b]
  // ALFA TPat
  | TPat(_) => []
  };

let repr = (~sp: string=" ", p: int, syntax: t): Aba.t(string, t) => {
  let p' = precedence(syntax);
  let tight_start = s =>
    s == "" || List.exists(String.ends_with(s, ~suffix=_), ["(", "¬"]);
  let tight_end = s =>
    s == ""
    || List.exists(String.starts_with(s, ~prefix=_), [")", ",", "."]);
  let mk_parens = labels =>
    labels
    |> ListUtil.map_first(s => p < p' ? "(" ++ s : s)
    |> ListUtil.map_last(s => p < p' ? s ++ ")" : s);
  let op = labels =>
    labels
    |> List.map(s =>
         (tight_end(s) ? "" : sp) ++ s ++ (tight_start(s) ? "" : sp)
       )
    |> ListUtil.map_first(s => String.trim(s) ++ (tight_start(s) ? "" : sp))
    |> ListUtil.map_last(s => (tight_end(s) ? "" : sp) ++ String.trim(s));
  let bin = (labels: list(string)) => op([""] @ labels @ [""]);
  let pre = (labels: list(string)) => op(labels @ [""]);
  let post = (labels: list(string)) => op([""] @ labels);
  let op_sg = (label: string) => [label];
  let bin_sg = (label: string) => bin([label]);
  let pre_sg = (label: string) => pre([label]);
  let post_sg = (label: string) => post([label]);
  let lebals =
    switch (IdTagged.term_of(syntax)) {
    | Hole(s) => "[" ++ s ++ "]" |> op_sg
    | Atom(s) => s |> op_sg
    | And(_) => "∧" |> bin_sg
    | Or(_) => "∨" |> bin_sg
    | Impl(_, {term: Falsity, _}) => "¬" |> pre_sg
    | Impl(_) => "⊃" |> bin_sg
    | Truth => "⊤" |> op_sg
    | Falsity => "⊥" |> op_sg
    | Ctx(ctx) =>
      List.length(ctx) == 0
        ? "·" |> op_sg : List.init(List.length(ctx) - 1, _ => ",") |> bin
    | Entail(_) => "⊢" |> bin_sg
    | NumLit(i) => string_of_int(i) |> op_sg
    | Val(_) => "val" |> post_sg
    | Neg(_) => "-" |> pre_sg
    | Plus(_) => "+" |> bin_sg
    | Minus(_) => "-" |> bin_sg
    | Times(_) => "*" |> bin_sg
    | Lt(_) => "<" |> bin_sg
    | Gt(_) => ">" |> bin_sg
    | Eq(_) => "=?" |> bin_sg
    | Eval(_) => "⇓" |> bin_sg
    | Num => "Num" |> op_sg
    | Bool => "Bool" |> op_sg
    | Arrow(_) => "→" |> bin_sg
    | Prod(_) => "×" |> bin_sg
    | Unit => "Unit" |> op_sg
    | Sum(_) => "+" |> bin_sg
    | TVar(x) => x |> op_sg
    | Rec(_) => ["rec", "is"] |> pre
    | True => "True" |> op_sg
    | False => "False" |> op_sg
    | If(_) => ["if", "then", "else"] |> pre
    | Var(x) => x |> op_sg
    | Let(_) => ["let", "be", "in"] |> pre
    | LetAnn(_) => ["let", ":", "be", "in"] |> pre
    | Fix(_) => ["fix", "→"] |> pre
    | FixAnn(_) => ["fix", ":", "→"] |> pre
    | Fun(_) => ["fun", "→"] |> pre
    | FunAnn(_) => ["fun", ":", "→"] |> pre
    | Ap(_) => ["", " ", ""]
    | Pair(_) => ["(", ",", ")"] |> op
    | Triv => "()" |> op_sg
    | PrjL(_) => ".fst" |> post_sg
    | PrjR(_) => ".snd" |> post_sg
    | LetPair(_) => ["let (", ",", ") be", "in"] |> pre
    | InjL(_) => "L" |> pre_sg
    | InjR(_) => "R" |> pre_sg
    | Case(_) => ["case", "of L(", ") →", "else R(", ") →"] |> pre
    | Roll(_) => ["roll(", ")"] |> op
    | Unroll(_) => ["unroll(", ")"] |> op
    | Pat(s) => s |> op_sg
    | TPat(s) => s |> op_sg
    | Type(_) => "type" |> post_sg
    | HasType(_) => ":" |> bin_sg
    | Syn(_) => "⇒" |> bin_sg
    | Ana(_) => "⇐" |> bin_sg
    };
  (lebals |> mk_parens, children(syntax));
};

let rec show = (p, syntax) =>
  syntax
  |> repr(p)
  |> Aba.join(Fun.id, show(precedence(syntax)))
  |> String.concat("");

let show = show(P.min);

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
    | Type(_)
    | HasType(_)
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
    | Type(_)
    | HasType(_)
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
    | (Rec({term: TPat(a1), _}, a2), Rec({term: TPat(b1), _}, b2)) =>
      let rep_id = TVar(Id.mk() |> Id.show) |> temp;
      eq(subst_ty(rep_id, a1, a2), subst_ty(rep_id, b1, b2));
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
    | (Type(a), Type(b)) => eq(a, b)
    | (Type(_), _) => false
    | (HasType(a1, a2), HasType(b1, b2)) => eq(a1, b1) && eq(a2, b2)
    | (HasType(_), _) => false
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

let rec splice_on_exist = (p, l) =>
  switch (l) {
  | [] => []
  | [hd, ...tl] => eq(p, hd) ? l : splice_on_exist(p, tl)
  };

let mem_ctx = (p, l) => splice_on_exist(p, l) != [];

let rec subset_ctx = (s, l) =>
  switch (s, l) {
  | ([], _) => true
  | (_, []) => false
  | ([hd, ...tl], l) =>
    switch (splice_on_exist(hd, l)) {
    | [] => false
    | [_, ...tl'] => subset_ctx(tl, tl')
    }
  };

// Note(zhiyao): This implementation of cons_ctx is not linear.
let cons_ctx = (ctx, p) => {
  let cmp = p' => show(p) < show(p');
  let eq_key = p' =>
    switch (IdTagged.term_of(p): term, IdTagged.term_of(p'): term) {
    | (HasType(a, _), HasType(b, _)) => eq(a, b)
    | (Syn(a, _), Syn(b, _)) => eq(a, b)
    | (Ana(a, _), Ana(b, _)) => eq(a, b)
    | _ => show(p) == show(p')
    };
  let rec insert =
    fun
    | [] => [p]
    | [hd, ...tl] when eq_key(hd) => [p, ...tl]
    | [hd, ...tl] when cmp(hd) => [p, hd, ...tl]
    | [hd, ...tl] => [hd, ...insert(tl)];
  insert(ctx);
};
