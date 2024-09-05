open Util;

exception Unreachable;

/*

 Prop.t is used as the internal representation of a judgement expression. It is
 converted from the evaluation result of an editor. An editor that is requested
 to be evaluated to Prop should have its result be Prop type Constructor.

 */

// type req('a) =
//     // ALFA Typ
//     | Num: req(unit)
//     | Bool: req(unit)
//     | Arrow(req('a), req('b)): req(('a, 'b))
//     | Prod(req('a), req('b)): req(('a, 'b))
//     | Unit: req(unit)
//     | Sum(req('a), req('b)): req(('a, 'b))
//     | TVar: req(string)
//     | Rec(req('a), req('b)): req(('a, 'b))
//     // ALFA Exp
//     | NumLit: req(int)
//     | UnOp(req('a), req('b)): req(('a, 'b))
//     | BinOp(req('a), req('b), req('c)): req(('a, 'b, 'c))
//     | True: req(unit)
//     | False: req(unit)
//     | If(req('a), req('b), req('c)): req(('a, 'b, 'c))
//     | Var: req(string)
//     | Let(req('a), req('b), req('c)): req(('a, 'b, 'c))
//     | Fix(req('a), req('b)): req(('a, 'b))
//     | Fun(req('a), req('b)): req(('a, 'b))
//     | Ap(req('a), req('b)): req(('a, 'b))
//     | Pair(req('a), req('b)): req(('a, 'b))
//     | Triv: req(unit)
//     | PrjL(req('a)): req('a)
//     | PrjR(req('a)): req('a)
//     | LetPair(req('a), req('b), req('c), req('d)): req(('a, 'b, 'c, 'd))
//     | InjL(req('a)): req('a)
//     | InjR(req('a)): req('a)
//     | Case(req('a), req('b), req('c), req('d), req('e))
//       : req(('a, 'b, 'c, 'd, 'e))
//     | Roll(req('a)): req('a)
//     | Unroll(req('a)): req('a)
//     // ALFA UnOp
//     | OpNeg: req(unit)
//     // ALFA BinOp
//     | OpPlus: req(unit)
//     | OpMinus: req(unit)
//     | OpTimes: req(unit)
//     | OpLt: req(unit)
//     | OpGt: req(unit)
//     | OpEq: req(unit)
//     // ALFA Meta
//     | TPat: req(string)
//     | Pat: req(string)
//     | PatAnn(req('a), req('b)): req(('a, 'b))
//     // ALFA Proposition
//     | HasTy(req('a), req('b)): req(('a, 'b))
//     | Syn(req('a), req('b)): req(('a, 'b))
//     | Ana(req('a), req('b)): req(('a, 'b))
//     // Logical Proposition
//     | And(req('a), req('b)): req(('a, 'b))
//     | Or(req('a), req('b)): req(('a, 'b))
//     | Implies(req('a), req('b)): req(('a, 'b))
//     | Truth: req(unit)
//     | Falsity: req(unit)
//     // Judgments
//     | Val(req('a)): req('a)
//     | Eval(req('a), req('b)): req(('a, 'b))
//     | Entail(req('a), req('b)): req(('a, 'b));

[@deriving (show({with_path: false}), sexp, yojson)]
type term =
  | Hole(string) // When DHExp.t not convertable, convert by `e => Hole(DHExp.show(e))`
  // Ctx
  | Ctx(list(t))
  // ALFA Typ
  | Num
  | Bool
  | Arrow(t, t)
  | Prod(t, t)
  | Unit
  | Sum(t, t)
  | TVar(string)
  | Rec(t, t)
  // ALFA Exp
  | NumLit(int)
  | UnOp(t, t)
  | BinOp(t, t, t)
  | True
  | False
  | If(t, t, t)
  | Var(string)
  | Let(t, t, t)
  | Fix(t, t)
  | Fun(t, t)
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
  // ALFA UnOp
  | OpNeg
  // ALFA BinOp
  | OpPlus
  | OpMinus
  | OpTimes
  | OpLt
  | OpGt
  | OpEq
  // ALFA Meta
  | TPat(string)
  | Pat(string)
  | PatAnn(t, t)
  // ALFA Proposition
  | HasTy(t, t)
  | Syn(t, t)
  | Ana(t, t)
  // Logical Proposition
  | Atom(string) // No corresponding req because we never need to unbox an Atom
  | And(t, t)
  | Or(t, t)
  | Implies(t, t)
  | Truth
  | Falsity
  // Judgments
  | Val(t)
  | Eval(t, t)
  | Entail(t, t)
and t = IdTagged.t(term);

let fresh = (term: term) => IdTagged.fresh(term);

let temp = (term: term) =>
  IdTagged.{term, ids: [Id.invalid], copied: false};

[@deriving (show({with_path: false}), sexp, yojson)]
type cls =
  | Hole
  | Ctx
  | Num
  | Bool
  | Arrow
  | Prod
  | Unit
  | Sum
  | TVar
  | Rec
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
  | Unroll
  | OpNeg
  | OpPlus
  | OpMinus
  | OpTimes
  | OpLt
  | OpGt
  | OpEq
  | TPat
  | Pat
  | PatAnn
  | HasTy
  | Syn
  | Ana
  | Atom
  | And
  | Or
  | Implies
  | Truth
  | Falsity
  | Val
  | Eval
  | Entail;

// let repr: t => string = {
//   let precedence: t => int =
//     fun
//     | Hole(_)
//     | Fail(_)
//     | Atom(_)
//     | Truth
//     | Falsity
//     | Ctx(_)
//     | NumLit(_)
//     | Val(_)
//     | UnOp(_)
//     | BinOp(_)
//     | OpNeg
//     | OpPlus
//     | OpMinus
//     | OpTimes
//     | Eval(_)
//     | Entail(_) => Precedence.entail
//     | And(_) => Precedence.prop_and
//     | Or(_) => Precedence.prop_or
//     | Implies(_) => Precedence.prop_implies;
//   let rec aux = (p: int, prop: t): string => {
//     let p' = precedence(prop);
//     let aux = aux(p');
//     let print_binop = (op: string, a: t, b: t) =>
//       Printf.sprintf("%s %s %s", aux(a), op, aux(b));
//     let print_unop = (op: string, a: t) =>
//       Printf.sprintf("%s%s", op, aux(a));
//     (
//       switch (prop) {
//       | Hole(s) => s
//       // TODO(zhiyao): make it colorful
//       | Fail(_) => ""
//       | Atom(s) => s
//       | And(a, b) => print_binop("∧", a, b)
//       | Or(a, b) => print_binop("∨", a, b)
//       | Implies(a, Falsity) => print_unop("¬", a)
//       | Implies(a, b) => print_binop("⊃", a, b)
//       | Truth => "⊤"
//       | Falsity => "⊥"
//       | Ctx(ctx) =>
//         if (Ctx.length(ctx) == 0) {
//           "·";
//         } else {
//           ctx
//           |> Ctx.map(aux)
//           |> String.concat(", ")
//           |> Printf.sprintf("[%s]");
//         }
//       | Entail(a, b) => print_binop("⊢", a, b)

//       | NumLit(i) => "_" ++ string_of_int(i) ++ "_"
//       | Val(a) => aux(a) ++ " val"
//       | UnOp(op, a) => print_unop(aux(op), a)
//       | BinOp(op, a, b) => print_binop(aux(op), a, b)
//       | OpNeg => "-"
//       | OpPlus => "+"
//       | OpMinus => "-"
//       | OpTimes => "*"
//       | Eval(_) => "=>"
//       }
//     )
//     |> (p < p' ? Printf.sprintf("(%s)") : Fun.id);
//   };
//   aux(0);
// };

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
    | (UnOp(a1, a2), UnOp(b1, b2)) => eq(a1, b1) && eq(a2, b2)
    | (UnOp(_), _) => false
    | (BinOp(a1, a2, a3), BinOp(b1, b2, b3)) =>
      eq(a1, b1) && eq(a2, b2) && eq(a3, b3)
    | (BinOp(_), _) => false
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
    | (Fix(a1, a2), Fix(b1, b2)) => eq(a1, b1) && eq(a2, b2)
    | (Fix(_), _) => false
    | (Fun(a1, a2), Fun(b1, b2)) => eq(a1, b1) && eq(a2, b2)
    | (Fun(_), _) => false
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
    | (OpNeg, OpNeg) => true
    | (OpNeg, _) => false
    | (OpPlus, OpPlus) => true
    | (OpPlus, _) => false
    | (OpMinus, OpMinus) => true
    | (OpMinus, _) => false
    | (OpTimes, OpTimes) => true
    | (OpTimes, _) => false
    | (OpLt, OpLt) => true
    | (OpLt, _) => false
    | (OpGt, OpGt) => true
    | (OpGt, _) => false
    | (OpEq, OpEq) => true
    | (OpEq, _) => false
    | (TPat(a), TPat(b)) => String.equal(a, b)
    | (TPat(_), _) => false
    | (Pat(a), Pat(b)) => String.equal(a, b)
    | (Pat(_), _) => false
    | (PatAnn(a1, a2), PatAnn(b1, b2)) => eq(a1, b1) && eq(a2, b2)
    | (PatAnn(_), _) => false
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
    | (Implies(a1, a2), Implies(b1, b2)) => eq(a1, b1) && eq(a2, b2)
    | (Implies(_), _) => false
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
      (
        switch (IdTagged.term_of(p)) {
        | PatAnn(p, _) => p
        | _ => p
        }
      )
      |> (
        p =>
          switch (IdTagged.term_of(p)) {
          | Pat(x') => String.equal(x', x)
          | _ => false
          }
      );
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
    | UnOp(u, e) => UnOp(u, subst(e)) |> rewrap
    | BinOp(b, e1, e2) => BinOp(b, subst(e1), subst(e2)) |> rewrap
    | True
    | False => e
    | If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3)) |> rewrap
    | Var(x') => String.equal(x', x) ? v : e
    | Let(x, e1, e2) => Let(x, subst(e1), subst'(x, e2)) |> rewrap
    | Fix(x, e) => Fix(x, subst'(x, e)) |> rewrap
    | Fun(x, e) => Fun(x, subst'(x, e)) |> rewrap
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
    // UnOp
    | OpNeg => e
    // BinOp
    | OpPlus
    | OpMinus
    | OpTimes
    | OpLt
    | OpGt
    | OpEq => e
    // Meta
    | TPat(_)
    | Pat(_)
    | PatAnn(_) => e
    // Proposition
    | HasTy(_)
    | Syn(_)
    | Ana(_) => e
    // Logic
    | Atom(_)
    | And(_)
    | Or(_)
    | Implies(_)
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
    | UnOp(u, e) => UnOp(u, subst_ty(e)) |> rewrap
    | BinOp(b, e1, e2) => BinOp(b, subst_ty(e1), subst_ty(e2)) |> rewrap
    | True
    | False => e
    | If(e1, e2, e3) =>
      If(subst_ty(e1), subst_ty(e2), subst_ty(e3)) |> rewrap
    | Var(_) => e
    | Let(x, e1, e2) =>
      Let(subst_ty(x), subst_ty(e1), subst_ty(e2)) |> rewrap
    | Fix(x, e) => Fix(subst_ty(x), subst_ty(e)) |> rewrap
    | Fun(x, e) => Fun(subst_ty(x), subst_ty(e)) |> rewrap
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
    // UnOp
    | OpNeg => e
    // BinOp
    | OpPlus
    | OpMinus
    | OpTimes
    | OpLt
    | OpGt
    | OpEq => e
    // Meta
    | TPat(_)
    | Pat(_) => e
    | PatAnn(x, t) => PatAnn(x, subst_ty(t)) |> rewrap
    // Proposition
    | HasTy(_)
    | Syn(_)
    | Ana(_) => e
    // Logic
    | Atom(_)
    | And(_)
    | Or(_)
    | Implies(_)
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
