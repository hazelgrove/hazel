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

module Prop = {
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
};

module Rule = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | A_Subsumption
    // Type Validity
    // | TV_Num
    // | TV_Bool
    // | TV_Unit
    // | TV_Arrow
    // | TV_Prod
    // | TV_Sum
    // | TV_Rec
    // | TV_TVar
    // Typing
    // - Booleans
    | T_True
    | S_True
    | T_False
    | S_False
    | T_If
    | S_If
    | A_If
    // - Numbers
    | T_Num
    | S_Num
    | T_Neg
    | S_Neg
    | T_Plus
    | S_Plus
    | T_Minus
    | S_Minus
    | T_Times
    | S_Times
    | T_Lt
    | S_Lt
    | T_Gt
    | S_Gt
    | T_Eq
    | S_Eq
    // - Variables
    | T_Var
    | S_Var
    | T_LetAnn
    // | T_LetAnn_TV
    | S_LetAnn
    | A_LetAnn
    | T_Let
    | S_Let
    | A_Let
    // - Functions
    | T_FunAnn
    // | T_FunAnn_TV
    | S_FunAnn
    | A_FunAnn
    | T_Fun
    | A_Fun
    | T_Ap
    | S_Ap
    // - Products
    | T_Triv
    | S_Triv
    | T_Pair
    | S_Pair
    | A_Pair
    | T_LetPair
    | S_LetPair
    | A_LetPair
    | T_PrjL
    | S_PrjL
    | T_PrjR
    | S_PrjR
    // - Sums
    | T_InjL
    | A_InjL
    | T_InjR
    | A_InjR
    | T_Case
    | S_Case
    | A_Case
    // - Fixpoints
    | T_Fix
    | T_FixAnn
    // | T_FixAnn_TV
    // - Recursive
    | T_Roll
    | T_Unroll
    // Values
    | V_Num
    | V_True
    | V_False
    | V_Fun
    | V_Pair
    | V_Triv
    | V_InjL
    | V_InjR
    | V_Roll
    // Evaluation
    // - Value Evaluation
    | E_Val
    // - Booleans
    | E_If_T
    | E_If_F
    // - Numbers
    | E_Neg
    | E_Plus
    | E_Minus
    | E_Times
    | E_Lt_T
    | E_Lt_F
    | E_Gt_T
    | E_Gt_F
    | E_Eq_T
    | E_Eq_F
    // - Variables
    | E_Let
    | E_Ap
    // - Products
    | E_Pair
    | E_PrjL
    | E_PrjR
    | E_LetPair
    // - Sums
    | E_InjL
    | E_InjR
    | E_Case_L
    | E_Case_R
    // - Fixpoints
    | E_Fix
    // - Recursive
    | E_Roll
    | E_Unroll
    // Propositional logic
    | Assumption
    | And_I
    | And_E_L
    | And_E_R
    | Or_I_L
    | Or_I_R
    | Or_E
    | Implies_I
    | Implies_E
    | Truth_I
    | Falsity_E;

  let repr =
    fun
    // Propositional logic
    | Assumption => "Asm."
    | And_I => "∧-I"
    | And_E_L => "∧-E-L"
    | And_E_R => "∧-E-R"
    | Or_I_L => "∨-I-L"
    | Or_I_R => "∨-I-R"
    | Or_E => "∨-E"
    | Implies_I => "⊃-I"
    | Implies_E => "⊃-E"
    | Truth_I => "⊤-I"
    | Falsity_E => "⊥-E"
    | A_Subsumption => "A-Sub"
    | rule => show(rule) |> String.map(c => c == '_' ? '-' : c);

  let prems_num =
    fun
    | A_Subsumption => 1
    | E_Val => 1
    | S_Num
    | T_Num => 0
    | V_Num => 0
    | S_True
    | T_True => 0
    | V_True => 0
    | S_False
    | T_False => 0
    | V_False => 0
    | S_Triv
    | T_Triv => 0
    | V_Triv => 0
    | S_Neg
    | T_Neg => 1
    | E_Neg => 1
    | S_Plus
    | T_Plus => 2
    | E_Plus => 2
    | S_Minus
    | T_Minus => 2
    | E_Minus => 2
    | S_Times
    | T_Times => 2
    | E_Times => 2
    | S_Lt
    | T_Lt => 2
    | E_Lt_T
    | E_Lt_F => 2
    | S_Gt
    | T_Gt => 2
    | E_Gt_T
    | E_Gt_F => 2
    | S_Eq
    | T_Eq => 2
    | E_Eq_T
    | E_Eq_F => 2
    | A_If
    | S_If
    | T_If => 3
    | E_If_T
    | E_If_F => 3
    | S_Var
    | T_Var => 0
    | S_LetAnn
    | A_LetAnn
    | T_LetAnn => 2
    | S_Let
    | A_Let
    | T_Let => 2
    | E_Let => 2
    | S_FunAnn
    | A_FunAnn
    | T_FunAnn => 1
    | A_Fun
    | T_Fun => 1
    | V_Fun => 0
    | T_Fix => 1
    | T_FixAnn => 1
    | E_Fix => 1
    | S_Ap
    | T_Ap => 2
    | E_Ap => 3
    | S_Pair
    | A_Pair
    | T_Pair => 2
    | E_Pair => 2
    | V_Pair => 2
    | S_LetPair
    | A_LetPair
    | T_LetPair => 2
    | E_LetPair => 2
    | S_PrjL
    | T_PrjL => 1
    | E_PrjL => 1
    | S_PrjR
    | T_PrjR => 1
    | E_PrjR => 1
    | A_InjL
    | T_InjL => 1
    | E_InjL => 1
    | V_InjL => 1
    | A_InjR
    | T_InjR => 1
    | E_InjR => 1
    | V_InjR => 1
    | A_Case
    | S_Case
    | T_Case => 3
    | E_Case_L
    | E_Case_R => 2
    | T_Roll => 1
    | E_Roll => 1
    | V_Roll => 1
    | T_Unroll => 1
    | E_Unroll => 1
    // Propositional logic
    | Assumption => 0
    | And_I => 2
    | And_E_L => 1
    | And_E_R => 1
    | Or_I_L => 1
    | Or_I_R => 1
    | Or_E => 3
    | Implies_I => 1
    | Implies_E => 2
    | Truth_I => 0
    | Falsity_E => 1;
};

module Verify = {
  open Prop;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type operation =
    | Neg(t)
    | Plus(t, t)
    | Minus(t, t)
    | Times(t, t)
    | Lt(t)
    | NotLt(t)
    | Gt(t)
    | NotGt(t)
    | Eq(t)
    | NotEq(t)
    | Subst((t, t), t)
    | Subst2((t, t), (t, t), t)
    | SubstTy((t, t), t)
    | Cons(t, t)
    | ConsHasTy((t, t), t)
    | ConsHasTy2((t, t), (t, t), t)
    | Mem(t)
    | MemHasTy(t, t);

  type req('a) =
    // Top-level
    | Get: req(t) // get term
    | TestEq(t): req(unit) // test term equality
    | Test(operation): req(unit) // check term
    // Ctx
    | Ctx: req(list(t))
    // ALFA Typ
    | Num: req(unit)
    | Bool: req(unit)
    | Arrow(req('a), req('b)): req(('a, 'b))
    | Prod(req('a), req('b)): req(('a, 'b))
    | Unit: req(unit)
    | Sum(req('a), req('b)): req(('a, 'b))
    | TVar: req(string)
    | Rec(req('a), req('b)): req(('a, 'b))
    // ALFA Exp
    | NumLit: req(int)
    | UnOp(req('a), req('b)): req(('a, 'b))
    | BinOp(req('a), req('b), req('c)): req(('a, 'b, 'c))
    | True: req(unit)
    | False: req(unit)
    | If(req('a), req('b), req('c)): req(('a, 'b, 'c))
    | Var: req(string)
    | Let(req('a), req('b), req('c)): req(('a, 'b, 'c))
    | Fix(req('a), req('b)): req(('a, 'b))
    | Fun(req('a), req('b)): req(('a, 'b))
    | Ap(req('a), req('b)): req(('a, 'b))
    | Pair(req('a), req('b)): req(('a, 'b))
    | Triv: req(unit)
    | PrjL(req('a)): req('a)
    | PrjR(req('a)): req('a)
    | LetPair(req('a), req('b), req('c), req('d)): req(('a, 'b, 'c, 'd))
    | InjL(req('a)): req('a)
    | InjR(req('a)): req('a)
    | Case(req('a), req('b), req('c), req('d), req('e))
      : req(('a, 'b, 'c, 'd, 'e))
    | Roll(req('a)): req('a)
    | Unroll(req('a)): req('a)
    // ALFA UnOp
    | OpNeg: req(unit)
    // ALFA BinOp
    | OpPlus: req(unit)
    | OpMinus: req(unit)
    | OpTimes: req(unit)
    | OpLt: req(unit)
    | OpGt: req(unit)
    | OpEq: req(unit)
    // ALFA Meta
    | TPat: req(string)
    | Pat: req(string)
    | PatAnn(req('a), req('b)): req(('a, 'b))
    // ALFA Proposition
    | HasTy(req('a), req('b)): req(('a, 'b))
    | Syn(req('a), req('b)): req(('a, 'b))
    | Ana(req('a), req('b)): req(('a, 'b))
    // Logical Proposition
    | And(req('a), req('b)): req(('a, 'b))
    | Or(req('a), req('b)): req(('a, 'b))
    | Implies(req('a), req('b)): req(('a, 'b))
    | Truth: req(unit)
    | Falsity: req(unit)
    // Judgments
    | Val(req('a)): req('a)
    | Eval(req('a), req('b)): req(('a, 'b))
    | Entail(req('a), req('b)): req(('a, 'b));

  [@deriving (show({with_path: false}), sexp, yojson)]
  type failure =
    | PremiseMismatch(int, int) /* expected, actual */
    | FailUnbox(Prop.cls, t)
    | NotEqual(t, t)
    | FailTest(t, operation); // expect {1} equal to {2}

  let (let$) = (x, f) =>
    switch (x) {
    | Ok(x) => f(x)
    | Error(e) => Error(e)
    };


  let rec unbox: type a. (t, req(a)) => result(a, failure) =
    (p, req) => {
      switch (req, IdTagged.term_of(p)) {
      // Top-level
      | (Get, _) => Ok(p)
      | (TestEq(p'), _) => eq(p', p) ? Ok() : Error(NotEqual(p, p'))
      | (Test(op), _) =>
        let$ result = check(p, op);
        result ? Ok() : Error(FailTest(p, op));
      | (Ctx, Ctx(l)) => Ok(l)
      | (Ctx, _) => Error(FailUnbox(Ctx, p))
      | (Sum(ra, rb), Sum(a, b)) =>
        let$ a = unbox(a, ra);
        let$ b = unbox(b, rb);
        Ok((a, b));
      | (Sum(_), _) => Error(FailUnbox(Sum, p))
      | (TVar, TVar(s)) => Ok(s)
      | (TVar, _) => Error(FailUnbox(TVar, p))
      | (Rec(ra, rb), Rec(a, b)) =>
        let$ a = unbox(a, ra);
        let$ b = unbox(b, rb);
        Ok((a, b));
      | (Rec(_), _) => Error(FailUnbox(Rec, p))
      | (TPat, TPat(s)) => Ok(s)
      | (TPat, _) => Error(FailUnbox(TPat, p))
      | (Fix(ra, rb), Fix(a, b)) =>
        let$ a = unbox(a, ra);
        let$ b = unbox(b, rb);
        Ok((a, b));
      | (Fix(_), _) => Error(FailUnbox(Fix, p))
      | (InjL(ra), InjL(a)) =>
        let$ a = unbox(a, ra);
        Ok(a);
      | (InjL(_), _) => Error(FailUnbox(InjL, p))
      | (InjR(ra), InjR(a)) =>
        let$ a = unbox(a, ra);
        Ok(a);
      | (InjR(_), _) => Error(FailUnbox(InjR, p))
      | (Case(ra, rb, rc, rd, re), Case(a, b, c, d, e)) =>
        let$ a = unbox(a, ra);
        let$ b = unbox(b, rb);
        let$ c = unbox(c, rc);
        let$ d = unbox(d, rd);
        let$ e = unbox(e, re);
        Ok((a, b, c, d, e));
      | (Case(_), _) => Error(FailUnbox(Case, p))
      | (Roll(ra), Roll(a)) =>
        let$ a = unbox(a, ra);
        Ok(a);
      | (Roll(_), _) => Error(FailUnbox(Roll, p))
      | (Unroll(ra), Unroll(a)) =>
        let$ a = unbox(a, ra);
        Ok(a);
      | (Unroll(_), _) => Error(FailUnbox(Unroll, p))
      // ALFp
      | (Num, Num) => Ok()
      | (Num, _) => Error(FailUnbox(Num, p))
      | (Bool, Bool) => Ok()
      | (Bool, _) => Error(FailUnbox(Bool, p))
      | (Arrow(ra, rb), Arrow(a, b)) =>
        let$ a = unbox(a, ra);
        let$ b = unbox(b, rb);
        Ok((a, b));
      | (Arrow(_), _) => Error(FailUnbox(Arrow, p))
      | (Prod(ra, rb), Prod(a, b)) =>
        let$ a = unbox(a, ra);
        let$ b = unbox(b, rb);
        Ok((a, b));
      | (Prod(_), _) => Error(FailUnbox(Prod, p))
      | (Unit, Unit) => Ok()
      | (Unit, _) => Error(FailUnbox(Unit, p))
      | (Pair(ra, rb), Pair(a, b)) =>
        let$ a = unbox(a, ra);
        let$ b = unbox(b, rb);
        Ok((a, b));
      | (Pair(_), _) => Error(FailUnbox(Pair, p))
      | (LetPair(ra, rb, rc, rd), LetPair(a, b, c, d)) =>
        let$ a = unbox(a, ra);
        let$ b = unbox(b, rb);
        let$ c = unbox(c, rc);
        let$ d = unbox(d, rd);
        Ok((a, b, c, d));
      | (LetPair(_), _) => Error(FailUnbox(LetPair, p))
      | (PrjL(ra), PrjL(a)) =>
        let$ a = unbox(a, ra);
        Ok(a);
      | (PrjL(_), _) => Error(FailUnbox(PrjL, p))
      | (PrjR(ra), PrjR(a)) =>
        let$ a = unbox(a, ra);
        Ok(a);
      | (PrjR(_), _) => Error(FailUnbox(PrjR, p))
      | (Triv, Triv) => Ok()
      | (Triv, _) => Error(FailUnbox(Triv, p))
      | (Pat, Pat(s)) => Ok(s)
      | (Pat, _) => Error(FailUnbox(Pat, p))
      | (PatAnn(ra, rb), PatAnn(a, b)) =>
        let$ a = unbox(a, ra);
        let$ b = unbox(b, rb);
        Ok((a, b));
      | (PatAnn(_), _) => Error(FailUnbox(PatAnn, p))
      // ALF
      | (True, True) => Ok()
      | (True, _) => Error(FailUnbox(True, p))
      | (False, False) => Ok()
      | (False, _) => Error(FailUnbox(False, p))
      | (If(ra, rb, rc), If(a, b, c)) =>
        let$ a = unbox(a, ra);
        let$ b = unbox(b, rb);
        let$ c = unbox(c, rc);
        Ok((a, b, c));
      | (If(_), _) => Error(FailUnbox(If, p))
      | (Var, Var(s)) => Ok(s)
      | (Var, _) => Error(FailUnbox(Var, p))
      | (Let(ra, rb, rc), Let(a, b, c)) =>
        let$ a = unbox(a, ra);
        let$ b = unbox(b, rb);
        let$ c = unbox(c, rc);
        Ok((a, b, c));
      | (Let(_), _) => Error(FailUnbox(Let, p))
      | (Fun(ra, rb), Fun(a, b)) =>
        let$ a = unbox(a, ra);
        let$ b = unbox(b, rb);
        Ok((a, b));
      | (Fun(_), _) => Error(FailUnbox(Fun, p))
      | (Ap(ra, rb), Ap(a, b)) =>
        let$ a = unbox(a, ra);
        let$ b = unbox(b, rb);
        Ok((a, b));
      | (Ap(_), _) => Error(FailUnbox(Ap, p))
      | (OpLt, OpLt) => Ok()
      | (OpLt, _) => Error(FailUnbox(OpLt, p))
      | (OpGt, OpGt) => Ok()
      | (OpGt, _) => Error(FailUnbox(OpGt, p))
      | (OpEq, OpEq) => Ok()
      | (OpEq, _) => Error(FailUnbox(OpEq, p))
      // AL
      | (NumLit, NumLit(n)) => Ok(n)
      | (NumLit, _) => Error(FailUnbox(NumLit, p))
      | (UnOp(ra, rb), UnOp(a, b)) =>
        let$ a = unbox(a, ra);
        let$ b = unbox(b, rb);
        Ok((a, b));
      | (UnOp(_), _) => Error(FailUnbox(UnOp, p))
      | (BinOp(ra, rb, rc), BinOp(a, b, c)) =>
        let$ a = unbox(a, ra);
        let$ b = unbox(b, rb);
        let$ c = unbox(c, rc);
        Ok((a, b, c));
      | (BinOp(_), _) => Error(FailUnbox(BinOp, p))
      | (OpNeg, OpNeg) => Ok()
      | (OpNeg, _) => Error(FailUnbox(OpNeg, p))
      | (OpPlus, OpPlus) => Ok()
      | (OpPlus, _) => Error(FailUnbox(OpPlus, p))
      | (OpMinus, OpMinus) => Ok()
      | (OpMinus, _) => Error(FailUnbox(OpMinus, p))
      | (OpTimes, OpTimes) => Ok()
      | (OpTimes, _) => Error(FailUnbox(OpTimes, p))
      // ALFA proposition
      | (HasTy(ra, rb), HasTy(a, b)) =>
        let$ a = unbox(a, ra);
        let$ b = unbox(b, rb);
        Ok((a, b));
      | (HasTy(_), _) => Error(FailUnbox(HasTy, p))
      | (Syn(ra, rb), Syn(a, b)) =>
        let$ a = unbox(a, ra);
        let$ b = unbox(b, rb);
        Ok((a, b));
      | (Syn(_), _) => Error(FailUnbox(Syn, p))
      | (Ana(ra, rb), Ana(a, b)) =>
        let$ a = unbox(a, ra);
        let$ b = unbox(b, rb);
        Ok((a, b));
      | (Ana(_), _) => Error(FailUnbox(Ana, p))
      // Propositional logic
      | (And(ra, rb), And(a, b)) =>
        let$ a = unbox(a, ra);
        let$ b = unbox(b, rb);
        Ok((a, b));
      | (And(_), _) => Error(FailUnbox(And, p))
      | (Or(ra, rb), Or(a, b)) =>
        let$ a = unbox(a, ra);
        let$ b = unbox(b, rb);
        Ok((a, b));
      | (Or(_), _) => Error(FailUnbox(Or, p))
      | (Implies(ra, rb), Implies(a, b)) =>
        let$ a = unbox(a, ra);
        let$ b = unbox(b, rb);
        Ok((a, b));
      | (Implies(_), _) => Error(FailUnbox(Implies, p))
      | (Truth, Truth) => Ok()
      | (Truth, _) => Error(FailUnbox(Truth, p))
      | (Falsity, Falsity) => Ok()
      | (Falsity, _) => Error(FailUnbox(Falsity, p))
      // Judgments
      | (Val(ra), Val(a)) =>
        let$ a = unbox(a, ra);
        Ok(a);
      | (Val(_), _) => Error(FailUnbox(Val, p))
      | (Eval(ra, rb), Eval(a, b)) =>
        let$ a = unbox(a, ra);
        let$ b = unbox(b, rb);
        Ok((a, b));
      | (Eval(_), _) => Error(FailUnbox(Eval, p))
      | (Entail(ra, rb), Entail(a, b)) =>
        let$ a = unbox(a, ra);
        let$ b = unbox(b, rb);
        Ok((a, b));
      | (Entail(_), _) => Error(FailUnbox(Entail, p))
      };
    }

  and check: (t, operation) => result(bool, failure) =
    (p, op) => {
      let unbox_num2 = (p1, p2) => {
        let$ n1 = unbox(p1, NumLit);
        let$ n2 = unbox(p2, NumLit);
        Ok((n1, n2));
      };
      let unbox_num3 = (p1, p2, p3) => {
        let$ n1 = unbox(p1, NumLit);
        let$ n2 = unbox(p2, NumLit);
        let$ n3 = unbox(p3, NumLit);
        Ok((n1, n2, n3));
      };
      let unbox_pat = x => {
        let$ x = unbox(x, Pat);
        Ok(Var(x) |> temp);
      };
      switch (op) {
      | Neg(p') =>
        let$ (n, n') = unbox_num2(p, p');
        Ok(n == - n');
      | Plus(p1, p2) =>
        let$ (n, n1, n2) = unbox_num3(p, p1, p2);
        Ok(n == n1 + n2);
      | Minus(p1, p2) =>
        let$ (n, n1, n2) = unbox_num3(p, p1, p2);
        Ok(n == n1 - n2);
      | Times(p1, p2) =>
        let$ (n, n1, n2) = unbox_num3(p, p1, p2);
        Ok(n == n1 * n2);
      | Lt(p') =>
        let$ (n, n') = unbox_num2(p, p');
        Ok(n < n');
      | NotLt(p') =>
        let$ (n, n') = unbox_num2(p, p');
        Ok(n <= n');
      | Gt(p') =>
        let$ (n, n') = unbox_num2(p, p');
        Ok(n > n');
      | NotGt(p') =>
        let$ (n, n') = unbox_num2(p, p');
        Ok(n >= n');
      | Eq(p') =>
        let$ (n, n') = unbox_num2(p, p');
        Ok(n == n');
      | NotEq(p') =>
        let$ (n, n') = unbox_num2(p, p');
        Ok(n != n');
      | Subst((v, x), e) =>
        let$ x = unbox(x, Pat);
        Ok(p == subst(v, x, e));
      | Subst2((v1, x1), (v2, x2), e) =>
        let$ x1 = unbox(x1, Pat);
        let$ x2 = unbox(x2, Pat);
        Ok(p == subst(v2, x2, subst(v1, x1, e)));
      | SubstTy((t, a), e) =>
        let$ a = unbox(a, TVar);
        Ok(p == subst_ty(t, a, e));
      | Cons(e, l) =>
        let$ l = unbox(l, Ctx);
        let l = Ctx(cons_ctx(e, l)) |> temp;
        Ok(p == l);
      | ConsHasTy((pat, t), l) =>
        let$ l = unbox(l, Ctx);
        let$ x = unbox_pat(pat);
        let l = Ctx(cons_ctx(HasTy(x, t) |> temp, l)) |> temp;
        Ok(p == l);
      | ConsHasTy2((pat1, t1), (pat2, t2), l) =>
        let$ l = unbox(l, Ctx);
        let$ x1 = unbox_pat(pat1);
        let$ x2 = unbox_pat(pat2);
        let l = cons_ctx(HasTy(x1, t1) |> temp, l);
        let l = Ctx(cons_ctx(HasTy(x2, t2) |> temp, l)) |> temp;
        Ok(p == l);
      | Mem(e) =>
        let$ l = unbox(p, Ctx);
        Ok(mem_ctx(e, l));
      | MemHasTy(pat, t) =>
        let$ l = unbox(p, Ctx);
        let$ x = unbox_pat(pat);
        Ok(mem_ctx(HasTy(x, t) |> temp, l));
      };
    };

  let expect_prems_num: (Rule.t, list(Prop.t)) => result(int => t, failure) =
    (rule, prems) => {
      let got = List.length(prems);
      let expect = Rule.prems_num(rule);
      expect == got
        ? Ok(x => List.nth(prems, x)) : Error(PremiseMismatch(expect, got));
    };

  let verify =
      (rule: Rule.t, prems: list(t), concl: t): result(unit, failure) => {
    let$ prems = expect_prems_num(rule, prems);
    let __ = Get;
    let (!) = x => TestEq(x);
    let (!!) = x => Test(x);
    let (>>) = unbox;
    switch (rule) {
    // ALFA
    | A_Subsumption =>
      let$ (ctx, (e, t)) = prems(0) >> Entail(__, Syn(__, __));
      let$ _ = concl >> Entail(!ctx, Ana(!e, !t));
      Ok();
    | E_Val =>
      let$ v = prems(0) >> Val(__);
      let$ _ = concl >> Eval(!v, !v);
      Ok();
    | S_Num =>
      let$ _ = concl >> Entail(__, Syn(NumLit, Num));
      Ok();
    | T_Num =>
      let$ _ = concl >> Entail(__, HasTy(NumLit, Num));
      Ok();
    | V_Num =>
      let$ _ = concl >> Val(NumLit);
      Ok();
    | S_True =>
      let$ _ = concl >> Entail(__, Syn(True, Bool));
      Ok();
    | T_True =>
      let$ _ = concl >> Entail(__, HasTy(True, Bool));
      Ok();
    | V_True =>
      let$ _ = concl >> Val(True);
      Ok();
    | S_False =>
      let$ _ = concl >> Entail(__, Syn(False, Bool));
      Ok();
    | T_False =>
      let$ _ = concl >> Entail(__, HasTy(False, Bool));
      Ok();
    | V_False =>
      let$ _ = concl >> Val(False);
      Ok();
    | S_Triv =>
      let$ _ = concl >> Entail(__, Syn(Triv, Unit));
      Ok();
    | T_Triv =>
      let$ _ = concl >> Entail(__, HasTy(Triv, Unit));
      Ok();
    | V_Triv =>
      let$ _ = concl >> Val(Triv);
      Ok();
    | S_Neg =>
      let$ (ctx, (e, _)) = prems(0) >> Entail(__, Syn(__, Num));
      let$ _ = concl >> Entail(!ctx, Syn(UnOp(OpNeg, !e), Num));
      Ok();
    | T_Neg =>
      let$ (ctx, (e, _)) = prems(0) >> Entail(__, HasTy(__, Num));
      let$ _ = concl >> Entail(!ctx, HasTy(UnOp(OpNeg, !e), Num));
      Ok();
    | E_Neg =>
      let$ (e, v) = prems(0) >> Eval(__, __);
      let$ _ = concl >> Eval(UnOp(OpNeg, !e), !!Neg(v));
      Ok();
    | S_Plus =>
      let$ (ctx, (e1, _)) = prems(0) >> Entail(__, Ana(__, Num));
      let$ (_, (e2, _)) = prems(1) >> Entail(!ctx, Ana(__, Num));
      let$ _ = concl >> Entail(!ctx, Syn(BinOp(OpPlus, !e1, !e2), Num));
      Ok();
    | T_Plus =>
      let$ (ctx, (e1, _)) = prems(0) >> Entail(__, HasTy(__, Num));
      let$ (_, (e2, _)) = prems(1) >> Entail(!ctx, HasTy(__, Num));
      let$ _ = concl >> Entail(!ctx, HasTy(BinOp(OpPlus, !e1, !e2), Num));
      Ok();
    | E_Plus =>
      let$ (e1, v1) = prems(0) >> Eval(__, __);
      let$ (e2, v2) = prems(1) >> Eval(__, __);
      let$ _ = concl >> Eval(BinOp(OpPlus, !e1, !e2), !!Plus(v1, v2));
      Ok();
    | S_Minus =>
      let$ (ctx, (e1, _)) = prems(0) >> Entail(__, Ana(__, Num));
      let$ (_, (e2, _)) = prems(1) >> Entail(!ctx, Ana(__, Num));
      let$ _ = concl >> Entail(!ctx, Syn(BinOp(OpMinus, !e1, !e2), Num));
      Ok();
    | T_Minus =>
      let$ (ctx, (e1, _)) = prems(0) >> Entail(__, HasTy(__, Num));
      let$ (_, (e2, _)) = prems(1) >> Entail(!ctx, HasTy(__, Num));
      let$ _ = concl >> Entail(!ctx, HasTy(BinOp(OpMinus, !e1, !e2), Num));
      Ok();
    | E_Minus =>
      let$ (e1, v1) = prems(0) >> Eval(__, __);
      let$ (e2, v2) = prems(1) >> Eval(__, __);
      let$ _ = concl >> Eval(BinOp(OpMinus, !e1, !e2), !!Minus(v1, v2));
      Ok();
    | S_Times =>
      let$ (ctx, (e1, _)) = prems(0) >> Entail(__, Ana(__, Num));
      let$ (_, (e2, _)) = prems(1) >> Entail(!ctx, Ana(__, Num));
      let$ _ = concl >> Entail(!ctx, Syn(BinOp(OpTimes, !e1, !e2), Num));
      Ok();
    | T_Times =>
      let$ (ctx, (e1, _)) = prems(0) >> Entail(__, HasTy(__, Num));
      let$ (_, (e2, _)) = prems(1) >> Entail(!ctx, HasTy(__, Num));
      let$ _ = concl >> Entail(!ctx, HasTy(BinOp(OpTimes, !e1, !e2), Num));
      Ok();
    | E_Times =>
      let$ (e1, v1) = prems(0) >> Eval(__, __);
      let$ (e2, v2) = prems(1) >> Eval(__, __);
      let$ _ = concl >> Eval(BinOp(OpTimes, !e1, !e2), !!Times(v1, v2));
      Ok();
    | S_Lt =>
      let$ (ctx, (e1, _)) = prems(0) >> Entail(__, Ana(__, Num));
      let$ (_, (e2, _)) = prems(1) >> Entail(!ctx, Ana(__, Num));
      let$ _ = concl >> Entail(!ctx, Syn(BinOp(OpLt, !e1, !e2), Bool));
      Ok();
    | T_Lt =>
      let$ (ctx, (e1, _)) = prems(0) >> Entail(__, HasTy(__, Num));
      let$ (_, (e2, _)) = prems(1) >> Entail(!ctx, HasTy(__, Num));
      let$ _ = concl >> Entail(!ctx, HasTy(BinOp(OpLt, !e1, !e2), Bool));
      Ok();
    | E_Lt_T =>
      let$ (e1, v1) = prems(0) >> Eval(__, __);
      let$ (e2, _) = prems(1) >> Eval(__, !!Lt(v1));
      let$ _ = concl >> Eval(BinOp(OpLt, !e1, !e2), True);
      Ok();
    | E_Lt_F =>
      let$ (e1, v1) = prems(0) >> Eval(__, __);
      let$ (e2, _) = prems(1) >> Eval(__, !!NotLt(v1));
      let$ _ = concl >> Eval(BinOp(OpLt, !e1, !e2), False);
      Ok();
    | S_Gt =>
      let$ (ctx, (e1, _)) = prems(0) >> Entail(__, Ana(__, Num));
      let$ (_, (e2, _)) = prems(1) >> Entail(!ctx, Ana(__, Num));
      let$ _ = concl >> Entail(!ctx, Syn(BinOp(OpGt, !e1, !e2), Bool));
      Ok();
    | T_Gt =>
      let$ (ctx, (e1, _)) = prems(0) >> Entail(__, HasTy(__, Num));
      let$ (_, (e2, _)) = prems(1) >> Entail(!ctx, HasTy(__, Num));
      let$ _ = concl >> Entail(!ctx, HasTy(BinOp(OpGt, !e1, !e2), Bool));
      Ok();
    | E_Gt_T =>
      let$ (e1, v1) = prems(0) >> Eval(__, __);
      let$ (e2, _) = prems(1) >> Eval(__, !!Gt(v1));
      let$ _ = concl >> Eval(BinOp(OpGt, !e1, !e2), True);
      Ok();
    | E_Gt_F =>
      let$ (e1, v1) = prems(0) >> Eval(__, __);
      let$ (e2, _) = prems(1) >> Eval(__, !!NotGt(v1));
      let$ _ = concl >> Eval(BinOp(OpGt, !e1, !e2), False);
      Ok();
    | S_Eq =>
      let$ (ctx, (e1, _)) = prems(0) >> Entail(__, Ana(__, Num));
      let$ (_, (e2, _)) = prems(1) >> Entail(!ctx, Ana(__, Num));
      let$ _ = concl >> Entail(!ctx, Syn(BinOp(OpEq, !e1, !e2), Bool));
      Ok();
    | T_Eq =>
      let$ (ctx, (e1, _)) = prems(0) >> Entail(__, HasTy(__, Num));
      let$ (_, (e2, _)) = prems(1) >> Entail(!ctx, HasTy(__, Num));
      let$ _ = concl >> Entail(!ctx, HasTy(BinOp(OpEq, !e1, !e2), Bool));
      Ok();
    | E_Eq_T =>
      let$ (e1, v1) = prems(0) >> Eval(__, __);
      let$ (e2, _) = prems(1) >> Eval(__, !!Eq(v1));
      let$ _ = concl >> Eval(BinOp(OpEq, !e1, !e2), True);
      Ok();
    | E_Eq_F =>
      let$ (e1, v1) = prems(0) >> Eval(__, __);
      let$ (e2, _) = prems(1) >> Eval(__, !!NotEq(v1));
      let$ _ = concl >> Eval(BinOp(OpEq, !e1, !e2), False);
      Ok();
    | S_If =>
      let$ (ctx, (e_cond, _)) = prems(0) >> Entail(__, Ana(__, Bool));
      let$ (_, (e_then, t)) = prems(1) >> Entail(!ctx, Syn(__, __));
      let$ (_, (e_else, _)) = prems(2) >> Entail(!ctx, Syn(__, !t));
      let$ _ =
        concl >> Entail(!ctx, Syn(If(!e_cond, !e_then, !e_else), !t));
      Ok();
    | A_If =>
      let$ (ctx, (e_cond, _)) = prems(0) >> Entail(__, Ana(__, Bool));
      let$ (_, (e_then, t)) = prems(1) >> Entail(!ctx, Ana(__, __));
      let$ (_, (e_else, _)) = prems(2) >> Entail(!ctx, Ana(__, !t));
      let$ _ =
        concl >> Entail(!ctx, Ana(If(!e_cond, !e_then, !e_else), !t));
      Ok();
    | T_If =>
      let$ (ctx, (e_cond, _)) = prems(0) >> Entail(__, HasTy(__, Bool));
      let$ (_, (e_then, t)) = prems(1) >> Entail(!ctx, HasTy(__, __));
      let$ (_, (e_else, _)) = prems(2) >> Entail(!ctx, HasTy(__, !t));
      let$ _ =
        concl >> Entail(!ctx, HasTy(If(!e_cond, !e_then, !e_else), !t));
      Ok();
    | E_If_T =>
      let$ (e_cond, _) = prems(0) >> Eval(__, True);
      let$ (e_then, v) = prems(1) >> Eval(__, __);
      let$ _ = concl >> Eval(If(!e_cond, !e_then, __), !v);
      Ok();
    | E_If_F =>
      let$ (e_cond, _) = prems(0) >> Eval(__, False);
      let$ (e_else, v) = prems(1) >> Eval(__, __);
      let$ _ = concl >> Eval(If(!e_cond, __, !e_else), !v);
      Ok();
    | S_Var =>
      let$ (ctx, (x, t)) = concl >> Entail(__, Syn(__, __));
      let$ _ = ctx >> !!MemHasTy(x, t);
      Ok();
    | T_Var =>
      let$ (ctx, p) = concl >> Entail(__, __);
      // TODO: The same as rule Assumption, we make it different by
      // checking p is a HasTy proposition
      let$ _ = p >> HasTy(__, __);
      let$ _ = ctx >> !!Mem(p);
      Ok();
    | S_LetAnn =>
      let$ (ctx, (((x, t_def), e_def, e_body), t)) =
        concl >> Entail(__, Syn(Let(PatAnn(__, __), __, __), __));
      let ctx' = ConsHasTy((x, t_def), ctx);
      let$ _ = prems(0) >> Entail(!ctx, Syn(!e_def, !t_def));
      let$ _ = prems(1) >> Entail(!!ctx', Syn(!e_body, !t));
      Ok();
    | A_LetAnn =>
      let$ (ctx, (((x, t_def), e_def, e_body), t)) =
        concl >> Entail(__, Ana(Let(PatAnn(__, __), __, __), __));
      let ctx' = ConsHasTy((x, t_def), ctx);
      let$ _ = prems(0) >> Entail(!ctx, Syn(!e_def, !t_def));
      let$ _ = prems(1) >> Entail(!!ctx', Ana(!e_body, !t));
      Ok();
    | T_LetAnn =>
      let$ (ctx, (((x, t_def), e_def, e_body), t)) =
        concl >> Entail(__, HasTy(Let(PatAnn(__, __), __, __), __));
      let ctx' = ConsHasTy((x, t_def), ctx);
      let$ _ = prems(0) >> Entail(!ctx, Syn(!e_def, !t_def));
      let$ _ = prems(1) >> Entail(!!ctx', HasTy(!e_body, !t));
      Ok();
    | S_Let =>
      let$ (ctx, ((x, e_def, e_body), t)) =
        concl >> Entail(__, Syn(Let(__, __, __), __));
      let$ (_, (_, t_def)) = prems(0) >> Entail(!ctx, Ana(!e_def, __));
      let ctx' = ConsHasTy((x, t_def), ctx);
      let$ _ = prems(1) >> Entail(!!ctx', Syn(!e_body, !t));
      Ok();
    | A_Let =>
      let$ (ctx, ((x, e_def, e_body), t)) =
        concl >> Entail(__, Ana(Let(__, __, __), __));
      let$ (_, (_, t_def)) = prems(0) >> Entail(!ctx, Ana(!e_def, __));
      let ctx' = ConsHasTy((x, t_def), ctx);
      let$ _ = prems(1) >> Entail(!!ctx', Ana(!e_body, !t));
      Ok();
    | T_Let =>
      let$ (ctx, ((x, e_def, e_body), t)) =
        concl >> Entail(__, HasTy(Let(__, __, __), __));
      let$ (_, (_, t_def)) = prems(0) >> Entail(!ctx, Ana(!e_def, __));
      let ctx' = ConsHasTy((x, t_def), ctx);
      let$ _ = prems(1) >> Entail(!!ctx', HasTy(!e_body, !t));
      Ok();
    | E_Let =>
      let$ ((x, e_def, e_body), v) = concl >> Eval(Let(__, __, __), __);
      let$ (_, v_def) = prems(0) >> Eval(!e_def, __);
      let$ _ = prems(1) >> Eval(!!Subst((v_def, x), e_body), !v);
      Ok();
    | S_FunAnn =>
      let$ (ctx, (((x, t_in), e_body), (t_in', t_out))) =
        concl >> Entail(__, Syn(Fun(PatAnn(__, __), __), Arrow(__, __)));
      let$ _ = t_in' >> !t_in;
      let ctx' = ConsHasTy((x, t_in), ctx);
      let$ _ = prems(0) >> Entail(!!ctx', Syn(!e_body, !t_out));
      Ok();
    | A_FunAnn =>
      let$ (ctx, (((x, t_in), e_body), (t_in', t_out))) =
        concl >> Entail(__, Ana(Fun(PatAnn(__, __), __), Arrow(__, __)));
      let$ _ = t_in' >> !t_in;
      let ctx' = ConsHasTy((x, t_in), ctx);
      let$ _ = prems(0) >> Entail(!!ctx', Ana(!e_body, !t_out));
      Ok();
    | T_FunAnn =>
      let$ (ctx, (((x, t_in), e_body), (t_in', t_out))) =
        concl >> Entail(__, HasTy(Fun(PatAnn(__, __), __), Arrow(__, __)));
      let$ _ = t_in' >> !t_in;
      let ctx' = ConsHasTy((x, t_in), ctx);
      let$ _ = prems(0) >> Entail(!!ctx', HasTy(!e_body, !t_out));
      Ok();
    | A_Fun =>
      let$ (ctx, ((x, e_body), (t_in, t_out))) =
        concl >> Entail(__, Ana(Fun(__, __), Arrow(__, __)));
      let ctx' = ConsHasTy((x, t_in), ctx);
      let$ _ = prems(0) >> Entail(!!ctx', Ana(!e_body, !t_out));
      Ok();
    | T_Fun =>
      let$ (ctx, ((x, e_body), (t_in, t_out))) =
        concl >> Entail(__, HasTy(Fun(__, __), Arrow(__, __)));
      let ctx' = ConsHasTy((x, t_in), ctx);
      let$ _ = prems(0) >> Entail(!!ctx', HasTy(!e_body, !t_out));
      Ok();
    | V_Fun =>
      let$ _ = concl >> Val(Fun(__, __));
      Ok();
    | T_Fix =>
      let$ (ctx, ((x, e), t)) =
        concl >> Entail(__, HasTy(Fix(__, __), __));
      let _ = prems(0) >> Entail(!!ConsHasTy((x, t), ctx), HasTy(!e, !t));
      Ok();
    | T_FixAnn =>
      let$ (ctx, (((x, t), e), t')) =
        concl >> Entail(__, HasTy(Fix(PatAnn(__, __), __), __));
      let$ _ = t >> !t';
      let _ = prems(0) >> Entail(!!ConsHasTy((x, t), ctx), HasTy(!e, !t));
      Ok();
    | E_Fix =>
      let$ (e, v) = concl >> Eval(__, __);
      let$ (x, e_body) = e >> Fix(__, __);
      let$ _ = prems(0) >> Eval(!!Subst((e, x), e_body), !v);
      Ok();
    | S_Ap =>
      let$ (ctx, ((e_fun, e_arg), t_out)) =
        concl >> Entail(__, Syn(Ap(__, __), __));
      let$ (_, (_, (t_in, _))) =
        prems(0) >> Entail(!ctx, Syn(!e_fun, Arrow(__, !t_out)));
      let$ _ = prems(1) >> Entail(!ctx, Ana(!e_arg, !t_in));
      Ok();
    | T_Ap =>
      let$ (ctx, ((e_fun, e_arg), t_out)) =
        concl >> Entail(__, HasTy(Ap(__, __), __));
      let$ (_, (_, (t_in, _))) =
        prems(0) >> Entail(!ctx, HasTy(!e_fun, Arrow(__, !t_out)));
      let$ _ = prems(1) >> Entail(!ctx, HasTy(!e_arg, !t_in));
      Ok();
    | E_Ap =>
      let$ ((e_fun, e_arg), v) = concl >> Eval(Ap(__, __), __);
      let$ (_, (x, e_body)) = prems(0) >> Eval(!e_fun, Fun(__, __));
      let$ (_, v_arg) = prems(1) >> Eval(!e_arg, __);
      let$ _ = prems(3) >> Eval(!!Subst((v_arg, x), e_body), !v);
      Ok();
    | S_Pair =>
      let$ (ctx, (el, tl)) = prems(0) >> Entail(__, Syn(__, __));
      let$ (_, (er, tr)) = prems(1) >> Entail(!ctx, Syn(__, __));
      let$ _ = concl >> Entail(!ctx, Syn(Pair(!el, !er), Prod(!tl, !tr)));
      Ok();
    | A_Pair =>
      let$ (ctx, (el, tl)) = prems(0) >> Entail(__, Ana(__, __));
      let$ (_, (er, tr)) = prems(1) >> Entail(!ctx, Ana(__, __));
      let$ _ = concl >> Entail(!ctx, Ana(Pair(!el, !er), Prod(!tl, !tr)));
      Ok();
    | T_Pair =>
      let$ (ctx, (el, tl)) = prems(0) >> Entail(__, HasTy(__, __));
      let$ (_, (er, tr)) = prems(1) >> Entail(!ctx, HasTy(__, __));
      let$ _ =
        concl >> Entail(!ctx, HasTy(Pair(!el, !er), Prod(!tl, !tr)));
      Ok();
    | E_Pair =>
      let$ (el, vl) = prems(0) >> Eval(__, __);
      let$ (er, vr) = prems(1) >> Eval(__, __);
      let$ _ = concl >> Eval(Pair(!el, !er), Pair(!vl, !vr));
      Ok();
    | V_Pair =>
      let$ _ = concl >> Val(Pair(__, __));
      Ok();
    | S_LetPair =>
      let$ (ctx, ((x, y, e_def, e_body), t)) =
        concl >> Entail(__, Syn(LetPair(__, __, __, __), __));
      let$ (_, (_, (tl, tr))) =
        prems(0) >> Entail(!ctx, Syn(!e_def, Prod(__, __)));
      let ctx_xy = ConsHasTy2((x, tl), (y, tr), ctx);
      let$ _ = prems(1) >> Entail(!!ctx_xy, Syn(!e_body, !t));
      Ok();
    | A_LetPair =>
      let$ (ctx, ((x, y, e_def, e_body), t)) =
        concl >> Entail(__, Ana(LetPair(__, __, __, __), __));
      let$ (_, (_, (tl, tr))) =
        prems(0) >> Entail(!ctx, Syn(!e_def, Prod(__, __)));
      let ctx_xy = ConsHasTy2((x, tl), (y, tr), ctx);
      let$ _ = prems(1) >> Entail(!!ctx_xy, Ana(!e_body, !t));
      Ok();
    | T_LetPair =>
      let$ (ctx, ((x, y, e_def, e_body), t)) =
        concl >> Entail(__, HasTy(LetPair(__, __, __, __), __));
      let$ (_, (_, (tl, tr))) =
        prems(0) >> Entail(!ctx, HasTy(!e_def, Prod(__, __)));
      let ctx_xy = ConsHasTy2((x, tl), (y, tr), ctx);
      let$ _ = prems(1) >> Entail(!!ctx_xy, HasTy(!e_body, !t));
      Ok();
    | E_LetPair =>
      let$ ((x, y, e_def, e_body), v) =
        concl >> Eval(LetPair(__, __, __, __), __);
      let$ (_, (vl, vr)) = prems(0) >> Eval(!e_def, Pair(__, __));
      let e_body = Subst2((vl, x), (vr, y), e_body);
      let$ _ = prems(1) >> Eval(!!e_body, !v);
      Ok();
    | S_PrjL =>
      let$ (ctx, (e, tl)) = concl >> Entail(__, Syn(PrjL(__), __));
      let$ _ = prems(0) >> Entail(!ctx, Syn(!e, Prod(!tl, __)));
      Ok();
    | T_PrjL =>
      let$ (ctx, (e, tl)) = concl >> Entail(__, HasTy(PrjL(__), __));
      let$ _ = prems(0) >> Entail(!ctx, HasTy(!e, Prod(!tl, __)));
      Ok();
    | E_PrjL =>
      let$ (e, vl) = concl >> Eval(PrjL(__), __);
      let$ _ = prems(0) >> Eval(!e, Pair(!vl, __));
      Ok();
    | S_PrjR =>
      let$ (ctx, (e, tr)) = concl >> Entail(__, Syn(PrjR(__), __));
      let$ _ = prems(0) >> Entail(!ctx, Syn(!e, Prod(__, !tr)));
      Ok();
    | T_PrjR =>
      let$ (ctx, (e, tr)) = concl >> Entail(__, HasTy(PrjR(__), __));
      let$ _ = prems(0) >> Entail(!ctx, HasTy(!e, Prod(__, !tr)));
      Ok();
    | E_PrjR =>
      let$ (e, vr) = concl >> Eval(PrjR(__), __);
      let$ _ = prems(0) >> Eval(!e, Pair(__, !vr));
      Ok();
    | A_InjL =>
      let$ (ctx, (e, tl)) = prems(0) >> Entail(__, Ana(__, __));
      let$ _ = concl >> Entail(!ctx, Ana(InjL(!e), Sum(!tl, __)));
      Ok();
    | T_InjL =>
      let$ (ctx, (e, tl)) = prems(0) >> Entail(__, HasTy(__, __));
      let$ _ = concl >> Entail(!ctx, HasTy(InjL(!e), Sum(!tl, __)));
      Ok();
    | E_InjL =>
      let$ (e, v) = prems(0) >> Eval(__, __);
      let$ _ = concl >> Eval(InjL(!e), InjL(!v));
      Ok();
    | V_InjL =>
      let$ e = prems(0) >> Val(__);
      let$ _ = concl >> Val(InjL(!e));
      Ok();
    | A_InjR =>
      let$ (ctx, (e, tr)) = prems(0) >> Entail(__, Ana(__, __));
      let$ _ = concl >> Entail(!ctx, Ana(InjR(!e), Sum(__, !tr)));
      Ok();
    | T_InjR =>
      let$ (ctx, (e, tr)) = prems(0) >> Entail(__, HasTy(__, __));
      let$ _ = concl >> Entail(!ctx, HasTy(InjR(!e), Sum(__, !tr)));
      Ok();
    | E_InjR =>
      let$ (e, v) = prems(0) >> Eval(__, __);
      let$ _ = concl >> Eval(InjR(!e), InjR(!v));
      Ok();
    | V_InjR =>
      let$ e = prems(0) >> Val(__);
      let$ _ = concl >> Val(InjR(!e));
      Ok();
    | A_Case =>
      let$ (ctx, ((e_scrut, x, el, y, er), t)) =
        concl >> Entail(__, Ana(Case(__, __, __, __, __), __));
      let$ (_, (_, (tl, tr))) =
        prems(0) >> Entail(!ctx, Syn(!e_scrut, Sum(__, __)));
      let$ _ =
        prems(1) >> Entail(!!ConsHasTy((x, tl), ctx), Ana(!el, !t));
      let$ _ =
        prems(2) >> Entail(!!ConsHasTy((y, tr), ctx), Ana(!er, !t));
      Ok();
    | S_Case =>
      let$ (ctx, ((e_scrut, x, el, y, er), t)) =
        concl >> Entail(__, Syn(Case(__, __, __, __, __), __));
      let$ (_, (_, (tl, tr))) =
        prems(0) >> Entail(!ctx, Syn(!e_scrut, Sum(__, __)));
      let$ _ =
        prems(1) >> Entail(!!ConsHasTy((x, tl), ctx), Syn(!el, !t));
      let$ _ =
        prems(2) >> Entail(!!ConsHasTy((y, tr), ctx), Syn(!er, !t));
      Ok();
    | T_Case =>
      let$ (ctx, ((e_scrut, x, el, y, er), t)) =
        concl >> Entail(__, HasTy(Case(__, __, __, __, __), __));
      let$ (_, (_, (tl, tr))) =
        prems(0) >> Entail(!ctx, HasTy(!e_scrut, Sum(__, __)));
      let$ _ =
        prems(1) >> Entail(!!ConsHasTy((x, tl), ctx), HasTy(!el, !t));
      let$ _ =
        prems(2) >> Entail(!!ConsHasTy((y, tr), ctx), HasTy(!er, !t));
      Ok();
    | E_Case_L =>
      let$ ((e_scrut, x, el, _y, _er), v) =
        concl >> Eval(Case(__, __, __, __, __), __);
      let$ (_, v_data) = prems(0) >> Eval(!e_scrut, InjL(__));
      let$ _ = prems(1) >> Eval(!!Subst((v_data, x), el), !v);
      Ok();
    | E_Case_R =>
      let$ ((e_scrut, _x, _el, y, er), v) =
        concl >> Eval(Case(__, __, __, __, __), __);
      let$ (_, v_data) = prems(0) >> Eval(!e_scrut, InjR(__));
      let$ _ = prems(1) >> Eval(!!Subst((v_data, y), er), !v);
      Ok();
    | T_Roll =>
      let$ (ctx, (e_body, t)) = concl >> Entail(__, HasTy(Roll(__), __));
      let$ (a, t_body) = t >> Rec(__, __);
      let$ _ =
        prems(0)
        >> Entail(!ctx, HasTy(!e_body, !!SubstTy((t, a), t_body)));
      Ok();
    | E_Roll =>
      let$ (e, v) = concl >> Eval(Roll(__), Roll(__));
      let$ _ = prems(0) >> Eval(!e, !v);
      Ok();
    | V_Roll =>
      let$ e = prems(0) >> Val(__);
      let$ _ = concl >> Val(Roll(!e));
      Ok();
    | T_Unroll =>
      let$ (ctx, (e, t)) = prems(0) >> Entail(__, HasTy(__, __));
      let$ (a, t_body) = t >> Rec(__, __);
      let$ _ =
        concl
        >> Entail(!ctx, HasTy(Unroll(!e), !!SubstTy((t, a), t_body)));
      Ok();
    | E_Unroll =>
      let$ (e, v) = prems(0) >> Eval(__, Roll(__));
      let$ _ = concl >> Eval(Unroll(!e), !v);
      Ok();
    | Assumption =>
      let$ (ctx, p) = concl >> Entail(__, __);
      let$ _ = ctx >> !!Mem(p);
      Ok();
    | And_I =>
      let$ (ctx, (a, b)) = concl >> Entail(__, And(__, __));
      let$ _ = prems(0) >> Entail(!ctx, !a);
      let$ _ = prems(1) >> Entail(!ctx, !b);
      Ok();
    | And_E_L =>
      let$ (ctx, a) = concl >> Entail(__, __);
      let$ _ = prems(0) >> Entail(!ctx, And(!a, __));
      Ok();
    | And_E_R =>
      let$ (ctx, b) = concl >> Entail(__, __);
      let$ _ = prems(0) >> Entail(!ctx, And(__, !b));
      Ok();
    | Or_I_L =>
      let$ (ctx, (a, _b)) = concl >> Entail(__, Or(__, __));
      let$ _ = prems(0) >> Entail(!ctx, !a);
      Ok();
    | Or_I_R =>
      let$ (ctx, (_a, b)) = concl >> Entail(__, Or(__, __));
      let$ _ = prems(0) >> Entail(!ctx, !b);
      Ok();
    | Or_E =>
      let$ (ctx, c) = concl >> Entail(__, __);
      let$ (_, (a, b)) = prems(0) >> Entail(!ctx, Or(__, __));
      let$ _ = prems(1) >> Entail(!!Cons(a, ctx), !c);
      let$ _ = prems(2) >> Entail(!!Cons(b, ctx), !c);
      Ok();
    | Implies_I =>
      let$ (ctx, (a, b)) = concl >> Entail(__, Implies(__, __));
      let$ _ = prems(0) >> Entail(!!Cons(a, ctx), !b);
      Ok();
    | Implies_E =>
      let$ (ctx, (a, b)) = prems(0) >> Entail(__, Implies(__, __));
      let$ _ = prems(1) >> Entail(!ctx, !a);
      let$ _ = concl >> Entail(!ctx, !b);
      Ok();
    | Truth_I =>
      let$ _ = concl >> Entail(__, Truth);
      Ok();
    | Falsity_E =>
      let$ (ctx, _) = concl >> Entail(__, __);
      let$ _ = prems(0) >> Entail(!ctx, Falsity);
      Ok();
    };
  };
};
