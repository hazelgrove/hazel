open Util;

/*

 Prop.t is used as the internal representation of a judgement expression. It is
 converted from the evaluation result of an editor. An editor that is requested
 to be evaluated to Prop should have its result be Prop type Constructor.

 */

// // ALFA logic
// type typ =
//   | Num
//   | Bool
//   | Arrow(typ, typ)
//   | Prod(typ, typ)
//   | Unit
//   | Sum(typ, typ)
//   | TVar(string)
//   | Rec(tpat, typ)
// and tpat =
//   | TPat(string);
// type expr =
//   | NumLit(int)
//   | UnOp(unop, expr)
//   | BinOp(binop, expr, expr)
//   | True
//   | False
//   | If(expr, expr, expr)
//   | Var(string)
//   | Let(pat, expr, expr)
//   | Fix(pat, expr)
//   | Fun(pat, expr)
//   | Ap(expr, expr)
//   | Pair(expr, expr)
//   | Triv
//   | PrjL(expr)
//   | PrjR(expr)
//   | LetPair(pat, pat, expr, expr)
//   | InjL(expr)
//   | InjR(expr)
//   | Case(expr, pat, expr, pat, expr)
//   | Roll(expr)
//   | Unroll(expr)
// and pat =
//   | Pat(string)
//   | PatAnn(string, typ)
// and unop =
//   | OpNeg
// and binop =
//   | OpLt
//   | OpGt
//   | OpEq
//   | OpPlus
//   | OpMinus
//   | OpTimes;

module Prop = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Hole(string)
    // When DHExp.t not convertable, convert by `e => Hole(DHExp.show(e))`
    // ALFA
    | Sum(t, t)
    | TVar(string)
    | Rec(t, t)
    | TPat(string)
    | Fix(t, t)
    | InjL(t)
    | InjR(t)
    | Case(t, t, t, t, t)
    | Roll(t)
    | Unroll(t)
    // ALFp
    | Num
    | Bool
    | Arrow(t, t)
    | Prod(t, t)
    | Unit
    | Pair(t, t)
    | LetPair(t, t, t, t)
    | PrjL(t)
    | PrjR(t)
    | Triv
    | Pat(string)
    | PatAnn(string, t)
    // ALF
    | True
    | False
    | If(t, t, t)
    | Var(string)
    | Let(t, t, t)
    | Fun(t, t)
    | Ap(t, t)
    | OpLt
    | OpGt
    | OpEq
    // AL
    | NumLit(int)
    | UnOp(t, t)
    | BinOp(t, t, t)
    | OpNeg
    | OpPlus
    | OpMinus
    | OpTimes
    // ALFA outers
    | HasType(t, t)
    | Syn(t, t)
    | Ana(t, t)
    | Val(t)
    | Eval(t, t)
    // Propositional logic
    | Atom(string)
    | And(t, t)
    | Or(t, t)
    | Implies(t, t)
    | Truth
    | Falsity
    | Ctx(list(t))
    | Entail(t, t)
  and t = IdTagged.t(term);

  let fresh = (term: term) => IdTagged.fresh(term);

  let temp = (term: term) =>
    IdTagged.{term, ids: [Id.invalid], copied: false};

  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Hole
    // ALFA
    | Sum
    | TVar
    | Rec
    | TPat
    | Fix
    | InjL
    | InjR
    | Case
    | Roll
    | Unroll
    // ALFp
    | Num
    | Bool
    | Arrow
    | Prod
    | Unit
    | Pair
    | LetPair
    | PrjL
    | PrjR
    | Triv
    | Pat
    | PatAnn
    // ALF
    | True
    | False
    | If
    | Var
    | Let
    | Fun
    | Ap
    | OpLt
    | OpGt
    | OpEq
    // AL
    | NumLit
    | UnOp
    | BinOp
    | OpNeg
    | OpPlus
    | OpMinus
    | OpTimes
    // ALFA outers
    | HasType
    | Syn
    | Ana
    | Val
    | Eval
    // Propositional logic
    | Atom
    | And
    | Or
    | Implies
    | Truth
    | Falsity
    | Ctx
    | Entail;

  let of_cls: t => cls =
    t =>
      switch (IdTagged.term_of(t)) {
      | Hole(_) => Hole
      // ALFA
      | Sum(_) => Sum
      | TVar(_) => TVar
      | Rec(_) => Rec
      | TPat(_) => TPat
      | Fix(_) => Fix
      | InjL(_) => InjL
      | InjR(_) => InjR
      | Case(_) => Case
      | Roll(_) => Roll
      | Unroll(_) => Unroll
      // ALFp
      | Num => Num
      | Bool => Bool
      | Arrow(_) => Arrow
      | Prod(_) => Prod
      | Unit => Unit
      | Pair(_) => Pair
      | LetPair(_) => LetPair
      | PrjL(_) => PrjL
      | PrjR(_) => PrjR
      | Triv => Triv
      | Pat(_) => Pat
      | PatAnn(_) => PatAnn
      // ALF
      | True => True
      | False => False
      | If(_) => If
      | Var(_) => Var
      | Let(_) => Let
      | Fun(_) => Fun
      | Ap(_) => Ap
      | OpLt => OpLt
      | OpGt => OpGt
      | OpEq => OpEq
      // AL
      | NumLit(_) => NumLit
      | UnOp(_) => UnOp
      | BinOp(_) => BinOp
      | OpNeg => OpNeg
      | OpPlus => OpPlus
      | OpMinus => OpMinus
      | OpTimes => OpTimes
      // ALFA outers
      | HasType(_) => HasType
      | Syn(_) => Syn
      | Ana(_) => Ana
      | Val(_) => Val
      | Eval(_) => Eval
      // Propositional logic
      | Atom(_) => Atom
      | And(_) => And
      | Or(_) => Or
      | Implies(_) => Implies
      | Truth => Truth
      | Falsity => Falsity
      | Ctx(_) => Ctx
      | Entail(_) => Entail
      };

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
  //         if (List.length(ctx) == 0) {
  //           "·";
  //         } else {
  //           ctx
  //           |> List.map(aux)
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
      // ALFA
      | (Sum(a1, a2), Sum(b1, b2)) => eq(a1, b1) && eq(a2, b2)
      | (TVar(a), TVar(b)) => String.equal(a, b)
      | (Rec(a1, a2), Rec(b1, b2)) => eq(a1, b1) && eq(a2, b2)
      | (TPat(a), TPat(b)) => String.equal(a, b)
      | (Fix(a1, a2), Fix(b1, b2)) => eq(a1, b1) && eq(a2, b2)
      | (InjL(a), InjL(b))
      | (InjR(a), InjR(b)) => eq(a, b)
      | (Case(a1, a2, a3, a4, a5), Case(b1, b2, b3, b4, b5)) =>
        eq(a1, b1) && eq(a2, b2) && eq(a3, b3) && eq(a4, b4) && eq(a5, b5)
      | (Roll(a), Roll(b))
      | (Unroll(a), Unroll(b)) => eq(a, b)
      | (Sum(_), _)
      | (TVar(_), _)
      | (Rec(_), _)
      | (TPat(_), _)
      | (Fix(_), _)
      | (InjL(_), _)
      | (InjR(_), _)
      | (Case(_), _)
      | (Roll(_), _)
      | (Unroll(_), _) => false
      // ALFp
      | (Num, Num)
      | (Bool, Bool) => true
      | (Arrow(a1, a2), Arrow(b1, b2))
      | (Prod(a1, a2), Prod(b1, b2)) => eq(a1, b1) && eq(a2, b2)
      | (Unit, Unit) => true
      | (Pair(a1, a2), Pair(b1, b2)) => eq(a1, b1) && eq(a2, b2)
      | (LetPair(a1, a2, a3, a4), LetPair(b1, b2, b3, b4)) =>
        eq(a1, b1) && eq(a2, b2) && eq(a3, b3) && eq(a4, b4)
      | (PrjL(a), PrjL(b))
      | (PrjR(a), PrjR(b)) => eq(a, b)
      | (Triv, Triv) => true
      | (Pat(a), Pat(b)) => String.equal(a, b)
      | (PatAnn(a1, a2), PatAnn(b1, b2)) =>
        String.equal(a1, b1) && eq(a2, b2)
      | (Num, _)
      | (Bool, _)
      | (Arrow(_), _)
      | (Prod(_), _)
      | (Unit, _)
      | (Pair(_), _)
      | (LetPair(_), _)
      | (PrjL(_), _)
      | (PrjR(_), _)
      | (Triv, _)
      | (Pat(_), _)
      | (PatAnn(_), _) => false
      // ALF
      | (True, True)
      | (False, False) => true
      | (If(a1, a2, a3), If(b1, b2, b3)) =>
        eq(a1, b1) && eq(a2, b2) && eq(a3, b3)
      | (Var(a), Var(b)) => String.equal(a, b)
      | (Let(a1, a2, a3), Let(b1, b2, b3)) =>
        eq(a1, b1) && eq(a2, b2) && eq(a3, b3)
      | (Fun(a1, a2), Fun(b1, b2)) => eq(a1, b1) && eq(a2, b2)
      | (Ap(a1, a2), Ap(b1, b2)) => eq(a1, b1) && eq(a2, b2)
      | (OpLt, OpLt)
      | (OpGt, OpGt)
      | (OpEq, OpEq) => true
      | (True, _)
      | (False, _)
      | (If(_), _)
      | (Var(_), _)
      | (Let(_), _)
      | (Fun(_), _)
      | (Ap(_), _)
      | (OpLt, _)
      | (OpGt, _)
      | (OpEq, _) => false
      // AL
      | (NumLit(a), NumLit(b)) => Int.equal(a, b)
      | (UnOp(a1, a2), UnOp(b1, b2)) => eq(a1, b1) && eq(a2, b2)
      | (BinOp(a1, a2, a3), BinOp(b1, b2, b3)) =>
        eq(a1, b1) && eq(a2, b2) && eq(a3, b3)
      | (OpNeg, OpNeg)
      | (OpPlus, OpPlus)
      | (OpMinus, OpMinus)
      | (OpTimes, OpTimes) => true
      | (Val(a), Val(b)) => eq(a, b)
      | (Eval(a1, a2), Eval(b1, b2)) => eq(a1, b1) && eq(a2, b2)
      | (NumLit(_), _)
      | (UnOp(_), _)
      | (BinOp(_), _)
      | (OpNeg, _)
      | (OpPlus, _)
      | (OpMinus, _)
      | (OpTimes, _) => false
      // ALFA outers
      | (HasType(a1, a2), HasType(b1, b2)) => eq(a1, b1) && eq(a2, b2)
      | (Syn(a1, a2), Syn(b1, b2)) => eq(a1, b1) && eq(a2, b2)
      | (Ana(a1, a2), Ana(b1, b2)) => eq(a1, b1) && eq(a2, b2)
      | (HasType(_), _)
      | (Syn(_), _)
      | (Ana(_), _)
      | (Val(_), _)
      | (Eval(_), _) => false
      // Propositional logic
      | (Atom(a), Atom(b)) => String.equal(a, b)
      | (And(a1, a2), And(b1, b2))
      | (Or(a1, a2), Or(b1, b2))
      | (Implies(a1, a2), Implies(b1, b2)) => eq(a1, b1) && eq(a2, b2)
      | (Truth, Truth)
      | (Falsity, Falsity) => true
      | (Ctx(a), Ctx(b)) => List.for_all2(eq, a, b)
      | (Entail(a1, a2), Entail(b1, b2)) => eq(a1, b1) && eq(a2, b2)
      | (Atom(_), _)
      | (And(_), _)
      | (Or(_), _)
      | (Implies(_), _)
      | (Truth, _)
      | (Falsity, _)
      | (Ctx(_), _)
      | (Entail(_), _) => false
      };

  let rec subst: (t, string, t) => t =
    (v, x, e) => {
      let (term, rewrap: term => t) = IdTagged.unwrap(e);
      let subst = subst(v, x);
      let is_shadow = (p: t) =>
        switch (IdTagged.term_of(p)) {
        | Pat(x')
        | PatAnn(x', _) => String.equal(x', x)
        | _ => false
        };
      let subst' = p => is_shadow(p) ? Fun.id : subst;
      switch (term) {
      | Hole(_) => e
      // ALFA
      | Sum(_)
      | TVar(_)
      | Rec(_)
      | TPat(_) => e
      | Fix(x', e1) => Fix(x', subst'(x', e1)) |> rewrap
      | InjL(e1) => InjL(subst(e1)) |> rewrap
      | InjR(e1) => InjR(subst(e1)) |> rewrap
      | Case(e1, x1, e2, x2, e3) =>
        Case(subst(e1), x1, subst'(x1, e2), x2, subst'(x2, e3)) |> rewrap
      | Roll(e1) => Roll(subst(e1)) |> rewrap
      | Unroll(e1) => Unroll(subst(e1)) |> rewrap
      // ALFp
      | Num
      | Bool
      | Arrow(_)
      | Prod(_)
      | Unit => e
      | Pair(e1, e2) => Pair(subst(e1), subst(e2)) |> rewrap
      | LetPair(x1, x2, e1, e2) =>
        LetPair(
          x1,
          x2,
          subst(e1),
          is_shadow(x1) || is_shadow(x2) ? e2 : subst(e2),
        )
        |> rewrap
      | PrjL(e1) => PrjL(subst(e1)) |> rewrap
      | PrjR(e1) => PrjR(subst(e1)) |> rewrap
      | Triv
      | Pat(_)
      | PatAnn(_) => e
      // ALF
      | True
      | False => e
      | If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3)) |> rewrap
      | Var(x') => String.equal(x', x) ? v : e
      | Let(x', e1, e2) => Let(x', subst(e1), subst'(x', e2)) |> rewrap
      | Fun(x', e1) => Fun(x', subst'(x', e1)) |> rewrap
      | Ap(e1, e2) => Ap(subst(e1), subst(e2)) |> rewrap
      | OpLt
      | OpGt
      | OpEq => e
      // AL
      | NumLit(_) => e
      | UnOp(op, e1) => UnOp(op, subst(e1)) |> rewrap
      | BinOp(op, e1, e2) => BinOp(op, subst(e1), subst(e2)) |> rewrap
      | OpNeg
      | OpPlus
      | OpMinus
      | OpTimes => e
      // ALFA outers (not supported)
      | HasType(_)
      | Syn(_)
      | Ana(_)
      | Val(_)
      | Eval(_) => e
      // Propositional logic (not supported)
      | Atom(_)
      | And(_)
      | Or(_)
      | Implies(_)
      | Truth
      | Falsity
      | Ctx(_)
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
      | Hole(_) => e
      // ALFA
      | Sum(t1, t2) => Sum(subst_ty(t1), subst_ty(t2)) |> rewrap
      | TVar(x') => String.equal(x', x) ? v : e
      | Rec(tp1, t1) => Rec(tp1, subst_ty'(tp1, t1)) |> rewrap
      | TPat(_) => e
      | Fix(p1, e1) => Fix(subst_ty(p1), subst_ty(e1)) |> rewrap
      | InjL(e1) => InjL(subst_ty(e1)) |> rewrap
      | InjR(e1) => InjR(subst_ty(e1)) |> rewrap
      | Case(e1, p1, e2, p2, e3) =>
        Case(
          subst_ty(e1),
          subst_ty(p1),
          subst_ty(e2),
          subst_ty(p2),
          subst_ty(e3),
        )
        |> rewrap
      | Roll(e1) => Roll(subst_ty(e1)) |> rewrap
      | Unroll(e1) => Unroll(subst_ty(e1)) |> rewrap
      // ALFp
      | Num
      | Bool => e
      | Arrow(t1, t2) => Arrow(subst_ty(t1), subst_ty(t2)) |> rewrap
      | Prod(t1, t2) => Prod(subst_ty(t1), subst_ty(t2)) |> rewrap
      | Unit => e
      | Pair(e1, e2) => Pair(subst_ty(e1), subst_ty(e2)) |> rewrap
      | LetPair(p1, p2, e1, e2) =>
        LetPair(subst_ty(p1), subst_ty(p2), subst_ty(e1), subst_ty(e2))
        |> rewrap
      | PrjL(e1) => PrjL(subst_ty(e1)) |> rewrap
      | PrjR(e1) => PrjR(subst_ty(e1)) |> rewrap
      | Triv => e
      | Pat(_) => e
      | PatAnn(x', t1) => PatAnn(x', subst_ty(t1)) |> rewrap
      // ALF
      | True
      | False => e
      | If(e1, e2, e3) =>
        If(subst_ty(e1), subst_ty(e2), subst_ty(e3)) |> rewrap
      | Var(_) => e
      | Let(p1, e1, e2) =>
        Let(subst_ty(p1), subst_ty(e1), subst_ty(e2)) |> rewrap
      | Fun(p1, e1) => Fun(subst_ty(p1), subst_ty(e1)) |> rewrap
      | Ap(e1, e2) => Ap(subst_ty(e1), subst_ty(e2)) |> rewrap
      | OpLt
      | OpGt
      | OpEq => e
      // AL
      | NumLit(_) => e
      | UnOp(op, e1) => UnOp(op, subst_ty(e1)) |> rewrap
      | BinOp(op, e1, e2) =>
        BinOp(op, subst_ty(e1), subst_ty(e2)) |> rewrap
      | OpNeg
      | OpPlus
      | OpMinus
      | OpTimes => e
      // ALFA outers (not supported)
      | HasType(_)
      | Syn(_)
      | Ana(_)
      | Val(_)
      | Eval(_) => e
      // Propositional logic (not supported)
      | Atom(_)
      | And(_)
      | Or(_)
      | Implies(_)
      | Truth
      | Falsity
      | Ctx(_)
      | Entail(_) => e
      };
    };

  let in_ctx = (ctx: list(t), prop: t) => {
    List.exists(eq(prop), ctx);
  };

  let extend_ctx = (ctx: list(t), prop: t) => {
    let rec insert = (ctx, prop) =>
      switch (ctx) {
      | [] => [prop]
      | [hd, ...tl] =>
        if (show(hd) <= show(prop)) {
          [prop, ...ctx];
        } else {
          [hd, ...insert(tl, prop)];
        }
      };
    insert(ctx, prop);
  };
};

module Rule = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    // (future) ALFA + Type Validation + Type Equivalence
    // (future) ALFA + Type Validation
    // ALFA
    | A_InjL
    | A_InjR
    | E_InjL
    | E_InjR
    | A_Case
    | S_Case
    | E_Case_L
    | E_Case_R
    | E_Fix
    | T_Roll
    | T_Unroll
    | E_Roll
    | E_Unroll
    // Bidirectional Type System
    | S_Num
    | S_True
    | S_False
    | A_Subsumption
    | S_Var
    | A_Fun
    | S_FunAnn
    | A_FunAnn
    | S_Ap
    | S_Neg
    | S_Plus
    | S_Minus
    | S_Times
    | S_Lt
    | S_Gt
    | S_Eq
    | S_LetAnn
    | A_LetAnn
    | S_Let
    | A_Let
    | S_Pair
    | A_Pair
    | S_LetPair
    | A_LetPair
    | S_PrjL
    | S_PrjR
    | S_Triv
    | A_If
    | S_If
    // ALFp
    | T_Var
    | T_Let
    | T_LetAnn
    | T_Num
    | T_True
    | T_False
    | T_Neg
    | T_Plus
    | T_Minus
    | T_Times
    | T_Lt
    | T_Gt
    | T_Eq
    | T_If
    | T_Fun
    | T_FunAnn
    | T_Ap
    | E_Pair
    | V_Pair
    | T_Pair
    | E_PrjL
    | E_PrjR
    | T_PrjL
    | T_PrjR
    | E_LetPair
    | T_LetPair
    | V_Triv
    | T_Triv
    // ALF
    | V_Num
    | V_True
    | V_False
    | V_Fun
    | E_Val
    | E_Lt_T
    | E_Lt_F
    | E_Gt_T
    | E_Gt_F
    | E_Eq_T
    | E_Eq_F
    | E_If_T
    | E_If_F
    | E_Let
    | E_Ap
    // AL
    // V_NumLit (deprecated)
    // E_NumLit (deprecated)
    | E_Neg
    | E_Plus
    | E_Minus
    | E_Times
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
    // ALFA
    | A_InjL => "A-InjL"
    | A_InjR => "A-InjR"
    | E_InjL => "E-InjL"
    | E_InjR => "E-InjR"
    | A_Case => "A-Case"
    | S_Case => "S-Case"
    | E_Case_L => "E-Case-L"
    | E_Case_R => "E-Case-R"
    | E_Fix => "E-Fix"
    | T_Roll => "T-Roll"
    | T_Unroll => "T-Unroll"
    | E_Roll => "E-Roll"
    | E_Unroll => "E-Unroll"
    // Bidirectional Type System
    | S_Num => "S-Num"
    | S_True => "S-True"
    | S_False => "S-False"
    | A_Subsumption => "A-Subsumption"
    | S_Var => "S-Var"
    | A_Fun => "A-Fun"
    | S_FunAnn => "S-FunAnn"
    | A_FunAnn => "A-FunAnn"
    | S_Ap => "S-Ap"
    | S_Neg => "S-Neg"
    | S_Plus => "S-Plus"
    | S_Minus => "S-Minus"
    | S_Times => "S-Times"
    | S_Lt => "S-Lt"
    | S_Gt => "S-Gt"
    | S_Eq => "S-Eq"
    | S_LetAnn => "S-LetAnn"
    | A_LetAnn => "A-LetAnn"
    | S_Let => "S-Let"
    | A_Let => "A-Let"
    | S_Pair => "S-Pair"
    | A_Pair => "A-Pair"
    | S_LetPair => "S-LetPair"
    | A_LetPair => "A-LetPair"
    | S_PrjL => "S-PrjL"
    | S_PrjR => "S-PrjR"
    | S_Triv => "S-Triv"
    | A_If => "A-If"
    | S_If => "S-If"
    // ALFp
    | T_Var => "T-Var"
    | T_Let => "T-Let"
    | T_LetAnn => "T-LetAnn"
    | T_Num => "T-Num"
    | T_True => "T-True"
    | T_False => "T-False"
    | T_Neg => "T-Neg"
    | T_Plus => "T-Plus"
    | T_Minus => "T-Minus"
    | T_Times => "T-Times"
    | T_Lt => "T-Lt"
    | T_Gt => "T-Gt"
    | T_Eq => "T-Eq"
    | T_If => "T-If"
    | T_Fun => "T-Fun"
    | T_FunAnn => "T-FunAnn"
    | T_Ap => "T-Ap"
    | E_Pair => "E-Pair"
    | V_Pair => "V-Pair"
    | T_Pair => "T-Pair"
    | E_PrjL => "E-PrjL"
    | E_PrjR => "E-PrjR"
    | T_PrjL => "T-PrjL"
    | T_PrjR => "T-PrjR"
    | E_LetPair => "E-LetPair"
    | T_LetPair => "T-LetPair"
    | V_Triv => "V-Triv"
    | T_Triv => "T-Triv"
    // ALF
    | V_Num => "V-Num"
    | V_True => "V-True"
    | V_False => "V-False"
    | V_Fun => "V-Fun"
    | E_Val => "E-Val"
    | E_Lt_T => "E-Lt-T"
    | E_Lt_F => "E-Lt-F"
    | E_Gt_T => "E-Gt-T"
    | E_Gt_F => "E-Gt-F"
    | E_Eq_T => "E-Eq-T"
    | E_Eq_F => "E-Eq-F"
    | E_If_T => "E-If-T"
    | E_If_F => "E-If-F"
    | E_Let => "E-Let"
    | E_Ap => "E-Ap"
    // AL
    // | V_NumLit => "V-NumLit"
    // | E_NumLit => "E-NumLit"
    | E_Neg => "E-Neg"
    | E_Plus => "E-Plus"
    | E_Minus => "E-Minus"
    | E_Times => "E-Times"
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
    | Falsity_E => "⊥-E";

  let prems_num =
    fun
    // ALFA
    | A_InjL => 1
    | A_InjR => 1
    | E_InjL => 1
    | E_InjR => 1
    | A_Case => 3
    | S_Case => 3
    | E_Case_L => 2
    | E_Case_R => 2
    | E_Fix => 1
    | T_Roll => 1
    | T_Unroll => 1
    | E_Roll => 1
    | E_Unroll => 1
    // Bidirectional Type System
    | S_Num => 0
    | S_True => 0
    | S_False => 0
    | A_Subsumption => 1
    | S_Var => 0
    | A_Fun => 1
    | S_FunAnn => 1
    | A_FunAnn => 1
    | S_Ap => 2
    | S_Neg => 1
    | S_Plus => 2
    | S_Minus => 2
    | S_Times => 2
    | S_Lt => 2
    | S_Gt => 2
    | S_Eq => 2
    | S_LetAnn => 2
    | A_LetAnn => 2
    | S_Let => 2
    | A_Let => 2
    | S_Pair => 2
    | A_Pair => 2
    | S_LetPair => 2
    | A_LetPair => 2
    | S_PrjL => 1
    | S_PrjR => 1
    | S_Triv => 0
    | A_If => 3
    | S_If => 3
    // ALFp
    | T_Var => 0
    | T_Let => 2
    | T_LetAnn => 2
    | T_Num => 0
    | T_True => 0
    | T_False => 0
    | T_Neg => 1
    | T_Plus => 2
    | T_Minus => 2
    | T_Times => 2
    | T_Lt => 2
    | T_Gt => 2
    | T_Eq => 2
    | T_If => 3
    | T_Fun => 1
    | T_FunAnn => 1
    | T_Ap => 2
    | E_Pair => 2
    | V_Pair => 2
    | T_Pair => 2
    | E_PrjL => 1
    | E_PrjR => 1
    | T_PrjL => 1
    | T_PrjR => 1
    | E_LetPair => 2
    | T_LetPair => 2
    | V_Triv => 0
    | T_Triv => 0
    // ALF
    | V_Num => 0
    | V_True => 0
    | V_False => 0
    | V_Fun => 0
    | E_Val => 1
    | E_Lt_T => 2
    | E_Lt_F => 2
    | E_Gt_T => 2
    | E_Gt_F => 2
    | E_Eq_T => 2
    | E_Eq_F => 2
    | E_If_T => 2
    | E_If_F => 2
    | E_Let => 2
    | E_Ap => 3
    // AL
    // | V_NumLit => 0
    // | E_NumLit => 0
    | E_Neg => 1
    | E_Plus => 2
    | E_Minus => 2
    | E_Times => 2
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

  let all =
    [
      A_InjL,
      A_InjR,
      E_InjL,
      E_InjR,
      A_Case,
      S_Case,
      E_Case_L,
      E_Case_R,
      E_Fix,
      T_Roll,
      T_Unroll,
      E_Roll,
      E_Unroll,
    ]
    @ [
      T_Var,
      T_Let,
      T_LetAnn,
      T_Num,
      T_True,
      T_False,
      T_Neg,
      T_Plus,
      T_Minus,
      T_Times,
      T_Lt,
      T_Gt,
      T_Eq,
      T_If,
      T_Fun,
      T_FunAnn,
      T_Ap,
      E_Pair,
      V_Pair,
      T_Pair,
      E_PrjL,
      E_PrjR,
      T_PrjL,
      T_PrjR,
      E_LetPair,
      T_LetPair,
      V_Triv,
      T_Triv,
    ]
    @ [
      V_Num,
      V_True,
      V_False,
      V_Fun,
      E_Val,
      E_Lt_T,
      E_Lt_F,
      E_Gt_T,
      E_Gt_F,
      E_Eq_T,
      E_Eq_F,
      E_If_T,
      E_If_F,
      E_Let,
      E_Ap,
    ]
    @ [
      // V_NumLit,
      // E_NumLit,
      E_Neg,
      E_Plus,
      E_Minus,
      E_Times,
    ]
    @ [
      Assumption,
      And_I,
      And_E_L,
      And_E_R,
      Or_I_L,
      Or_I_R,
      Or_E,
      Implies_I,
      Implies_E,
      Truth_I,
      Falsity_E,
    ];
};

module Verify = {
  open Prop;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t_op =
    | Just(t)
    | UnOp(unop, t)
    | BinOp(binop, t, t)
    | Subst(t, t, t_op) // [v/x]e
    | SubstTy(t, t, t_op) // [t/a]e
    | VarHasType(t, t) // e : t
    | ExtendCtx(t_op, t_op) // Γ, e
  and unop =
    | Neg // -n
  and binop =
    | Plus // n1 + n2
    | Minus // n1 - n2
    | Times // n1 * n2
    | Lt // n1 < n2
    | Gt // n1 > n2
    | Eq; // n1 == n2

  [@deriving (show({with_path: false}), sexp, yojson)]
  type failure =
    | PremiseMismatch(int, int) /* expected, actual */
    | FailUnbox(Prop.cls, t)
    | NotEqual(Prop.cls, t, t_op)
    | NotInCtx(t_op, t); // in rear case, the element go under operation (S-Var)

  type unbox_req('a) =
    // ALFA
    | Sum: unbox_req((t, t))
    | TVar: unbox_req(string)
    | Rec: unbox_req((t, t))
    | TPat: unbox_req(string)
    | Fix: unbox_req((t, t))
    | InjL: unbox_req(t)
    | InjR: unbox_req(t)
    | Case: unbox_req((t, t, t, t, t))
    | Roll: unbox_req(t)
    | Unroll: unbox_req(t)
    // ALFp
    | Num: unbox_req(unit)
    | Bool: unbox_req(unit)
    | Arrow: unbox_req((t, t))
    | Prod: unbox_req((t, t))
    | Unit: unbox_req(unit)
    | Pair: unbox_req((t, t))
    | LetPair: unbox_req((t, t, t, t))
    | PrjL: unbox_req(t)
    | PrjR: unbox_req(t)
    | Triv: unbox_req(unit)
    | Pat: unbox_req(string)
    | PatAnn: unbox_req((string, t))
    // ALF
    | True: unbox_req(unit)
    | False: unbox_req(unit)
    | If: unbox_req((t, t, t))
    | Var: unbox_req(string)
    | Let: unbox_req((t, t, t))
    | Fun: unbox_req((t, t))
    | Ap: unbox_req((t, t))
    | OpLt: unbox_req(unit)
    | OpGt: unbox_req(unit)
    | OpEq: unbox_req(unit)
    // AL
    | NumLit: unbox_req(int)
    | UnOp: unbox_req((t, t))
    | BinOp: unbox_req((t, t, t))
    | OpNeg: unbox_req(unit)
    | OpPlus: unbox_req(unit)
    | OpMinus: unbox_req(unit)
    | OpTimes: unbox_req(unit)
    // ALFA outers
    | HasType: unbox_req((t, t))
    | Syn: unbox_req((t, t))
    | Ana: unbox_req((t, t))
    | Val: unbox_req(t)
    | Eval: unbox_req((t, t))
    // Propositional logic
    | And: unbox_req((t, t))
    | Or: unbox_req((t, t))
    | Implies: unbox_req((t, t))
    | Truth: unbox_req(unit)
    | Falsity: unbox_req(unit)
    | Entail: unbox_req((t, t))
    | Ctx: unbox_req(list(t));

  let cls_of_req: type a. unbox_req(a) => Prop.cls =
    fun
    // ALFA
    | Sum => Sum
    | TVar => TVar
    | Rec => Rec
    | TPat => TPat
    | Fix => Fix
    | InjL => InjL
    | InjR => InjR
    | Case => Case
    | Roll => Roll
    | Unroll => Unroll
    // ALFp
    | Num => Num
    | Bool => Bool
    | Arrow => Arrow
    | Prod => Prod
    | Unit => Unit
    | Pair => Pair
    | LetPair => LetPair
    | PrjL => PrjL
    | PrjR => PrjR
    | Triv => Triv
    | Pat => Pat
    | PatAnn => PatAnn
    // ALF
    | True => True
    | False => False
    | If => If
    | Var => Var
    | Let => Let
    | Fun => Fun
    | Ap => Ap
    | OpLt => OpLt
    | OpGt => OpGt
    | OpEq => OpEq
    // AL
    | NumLit => NumLit
    | UnOp => UnOp
    | BinOp => BinOp
    | OpNeg => OpNeg
    | OpPlus => OpPlus
    | OpMinus => OpMinus
    | OpTimes => OpTimes
    // ALFA outers
    | HasType => HasType
    | Syn => Syn
    | Ana => Ana
    | Val => Val
    | Eval => Eval
    // Propositional logic
    | And => And
    | Or => Or
    | Implies => Implies
    | Truth => Truth
    | Falsity => Falsity
    | Entail => Entail
    | Ctx => Ctx;

  let unbox: type a. (unbox_req(a), t) => result(a, failure) =
    (req, p) => {
      switch (req, IdTagged.term_of(p)) {
      // ALFA
      | (Sum, Sum(a, b)) => Ok((a, b))
      | (TVar, TVar(a)) => Ok(a)
      | (Rec, Rec(a, b)) => Ok((a, b))
      | (TPat, TPat(a)) => Ok(a)
      | (Fix, Fix(a, b)) => Ok((a, b))
      | (InjL, InjL(a)) => Ok(a)
      | (InjR, InjR(a)) => Ok(a)
      | (Case, Case(a, b, c, d, e)) => Ok((a, b, c, d, e))
      | (Roll, Roll(a)) => Ok(a)
      | (Unroll, Unroll(a)) => Ok(a)
      // ALFp
      | (Num, Num) => Ok()
      | (Bool, Bool) => Ok()
      | (Arrow, Arrow(a, b)) => Ok((a, b))
      | (Prod, Prod(a, b)) => Ok((a, b))
      | (Unit, Unit) => Ok()
      | (Pair, Pair(a, b)) => Ok((a, b))
      | (LetPair, LetPair(a, b, c, d)) => Ok((a, b, c, d))
      | (PrjL, PrjL(a)) => Ok(a)
      | (PrjR, PrjR(a)) => Ok(a)
      | (Triv, Triv) => Ok()
      | (Pat, Pat(a)) => Ok(a)
      | (Pat, PatAnn(a, _)) => Ok(a) // PatAnn is a subtype of Pat
      | (PatAnn, PatAnn(a, b)) => Ok((a, b))
      // ALF
      | (True, True) => Ok()
      | (False, False) => Ok()
      | (If, If(a, b, c)) => Ok((a, b, c))
      | (Var, Var(a)) => Ok(a)
      | (Let, Let(a, b, c)) => Ok((a, b, c))
      | (Fun, Fun(a, b)) => Ok((a, b))
      | (Ap, Ap(a, b)) => Ok((a, b))
      | (OpLt, OpLt) => Ok()
      | (OpGt, OpGt) => Ok()
      | (OpEq, OpEq) => Ok()
      // AL
      | (NumLit, NumLit(a)) => Ok(a)
      | (UnOp, UnOp(a, b)) => Ok((a, b))
      | (BinOp, BinOp(a, b, c)) => Ok((a, b, c))
      | (OpNeg, OpNeg) => Ok()
      | (OpPlus, OpPlus) => Ok()
      | (OpMinus, OpMinus) => Ok()
      | (OpTimes, OpTimes) => Ok()
      // ALFA outers
      | (HasType, HasType(a, b)) => Ok((a, b))
      | (Syn, Syn(a, b)) => Ok((a, b))
      | (Ana, Ana(a, b)) => Ok((a, b))
      | (Val, Val(a)) => Ok(a)
      | (Eval, Eval(a, b)) => Ok((a, b))
      // Propositional logic
      | (And, And(a, b)) => Ok((a, b))
      | (Or, Or(a, b)) => Ok((a, b))
      | (Implies, Implies(a, b)) => Ok((a, b))
      | (Truth, Truth) => Ok()
      | (Falsity, Falsity) => Ok()
      | (Entail, Entail(a, b)) => Ok((a, b))
      | (Ctx, Ctx(a)) => Ok(a)

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
      | (PatAnn, _)
      | (True, _)
      | (False, _)
      | (If, _)
      | (Var, _)
      | (Let, _)
      | (Fun, _)
      | (Ap, _)
      | (OpLt, _)
      | (OpGt, _)
      | (OpEq, _)
      | (NumLit, _)
      | (UnOp, _)
      | (BinOp, _)
      | (OpNeg, _)
      | (OpPlus, _)
      | (OpMinus, _)
      | (OpTimes, _)
      | (HasType, _)
      | (Syn, _)
      | (Ana, _)
      | (Val, _)
      | (Eval, _)
      | (And, _)
      | (Or, _)
      | (Implies, _)
      | (Truth, _)
      | (Falsity, _)
      | (Entail, _)
      | (Ctx, _) => Error(FailUnbox(cls_of_req(req), p))
      };
    };

  let (let$) = (x, f) =>
    switch (x) {
    | Ok(x) => f(x)
    | Error(e) => Error(e)
    };

  let rec go: t_op => result(t, failure) =
    a =>
      switch (a) {
      | Just(p) => Ok(p)
      | UnOp(op, e) =>
        let$ n = unbox(NumLit, e);
        switch (op) {
        | Neg => Ok(NumLit(- n) |> temp)
        };
      | BinOp(op, e1, e2) =>
        let$ n1 = unbox(NumLit, e1);
        let$ n2 = unbox(NumLit, e2);
        switch (op) {
        | Plus => Ok(NumLit(n1 + n2) |> temp)
        | Minus => Ok(NumLit(n1 - n2) |> temp)
        | Times => Ok(NumLit(n1 * n2) |> temp)
        | Lt => Ok((n1 < n2 ? True : False) |> temp)
        | Gt => Ok((n1 > n2 ? True : False) |> temp)
        | Eq => Ok((n1 == n2 ? True : False) |> temp)
        };
      | Subst(v, x, e) =>
        let$ x = unbox(Pat, x);
        let$ e = go(e);
        Ok(subst(v, x, e));
      | SubstTy(t, a, e) =>
        let$ a = unbox(TPat, a);
        let$ e = go(e);
        Ok(subst_ty(t, a, e));
      | ExtendCtx(ctx, e) =>
        let$ ctx = go(ctx);
        let$ ctx = unbox(Ctx, ctx);
        let$ e = go(e);
        Ok(Ctx(extend_ctx(ctx, e)) |> temp);
      | VarHasType(pat, t) =>
        let$ x = unbox(Pat, pat);
        Ok(HasType(Var(x) |> temp, t) |> temp);
      };

  // NotEqual
  let expect_eq_op: (t, t_op) => result(unit, failure) =
    (a, b_op) => {
      let$ b = go(b_op);
      eq(a, b) ? Ok() : Error(NotEqual(of_cls(a), a, b_op));
    };

  let expect_eq: (t, t) => result(unit, failure) =
    (a, b) => expect_eq_op(a, Just(b));

  let expect_in_ctx_op: (t_op, t) => result(unit, failure) =
    (p_op, ctx) => {
      let$ p = go(p_op);
      let$ pl = unbox(Ctx, ctx);
      in_ctx(pl, p) ? Ok() : Error(NotInCtx(p_op, ctx));
    };

  // NotInCtx
  let expect_in_ctx: (t, t) => result(unit, failure) =
    (p, ctx) => expect_in_ctx_op(Just(p), ctx);

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
    switch (rule) {
    // ALFA
    | A_InjL =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (el, t) = unbox(Ana, p);
      let$ e = unbox(InjL, el);
      let$ (_, tr) = unbox(Sum, t);
      let$ (ctx', p) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e', tr') = unbox(Syn, p);
      let$ _ = expect_eq(e', e);
      let$ _ = expect_eq(tr', tr);
      Ok();
    | A_InjR =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (er, t) = unbox(Ana, p);
      let$ e = unbox(InjR, er);
      let$ (tl, _) = unbox(Sum, t);
      let$ (ctx', p) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e', tl') = unbox(Syn, p);
      let$ _ = expect_eq(e', e);
      let$ _ = expect_eq(tl', tl);
      Ok();
    | E_InjL =>
      let$ (el, vl') = unbox(Eval, concl);
      let$ e = unbox(InjL, el);
      let$ v' = unbox(InjL, vl');
      let$ (e', v) = unbox(Eval, prems(0));
      let$ _ = expect_eq(e', e);
      let$ _ = expect_eq(v', v);
      Ok();
    | E_InjR =>
      let$ (er, vr') = unbox(Eval, concl);
      let$ e = unbox(InjR, er);
      let$ v' = unbox(InjR, vr');
      let$ (e', v) = unbox(Eval, prems(0));
      let$ _ = expect_eq(e', e);
      let$ _ = expect_eq(v', v);
      Ok();
    | A_Case =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(Ana, p);
      let$ (e_scrut, px, el, py, er) = unbox(Case, e);
      let$ (ctx', p) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e_scrut', ts) = unbox(Syn, p);
      let$ _ = expect_eq(e_scrut', e_scrut);
      let$ (tl, tr) = unbox(Sum, ts);
      let$ (ctx_x', p) = unbox(Entail, prems(1));
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(px, tl));
      let$ _ = expect_eq_op(ctx_x', ctx_x);
      let$ (el', t') = unbox(Ana, p);
      let$ _ = expect_eq(el', el);
      let$ _ = expect_eq(t', t);
      let$ (ctx_y', p) = unbox(Entail, prems(2));
      let ctx_y = ExtendCtx(Just(ctx), VarHasType(py, tr));
      let$ _ = expect_eq_op(ctx_y', ctx_y);
      let$ (er', t') = unbox(Ana, p);
      let$ _ = expect_eq(er', er);
      let$ _ = expect_eq(t', t);
      Ok();
    | S_Case =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(Syn, p);
      let$ (e_scrut, px, el, py, er) = unbox(Case, e);
      let$ (ctx', p) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e_scrut', ts) = unbox(Syn, p);
      let$ _ = expect_eq(e_scrut', e_scrut);
      let$ (tl, tr) = unbox(Sum, ts);
      let$ (ctx_x', p) = unbox(Entail, prems(1));
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(px, tl));
      let$ _ = expect_eq_op(ctx_x', ctx_x);
      let$ (el', t') = unbox(Syn, p);
      let$ _ = expect_eq(el', el);
      let$ _ = expect_eq(t', t);
      let$ (ctx_y', p) = unbox(Entail, prems(2));
      let ctx_y = ExtendCtx(Just(ctx), VarHasType(py, tr));
      let$ _ = expect_eq_op(ctx_y', ctx_y);
      let$ (er', t') = unbox(Syn, p);
      let$ _ = expect_eq(er', er);
      let$ _ = expect_eq(t', t);
      Ok();
    | E_Case_L =>
      let$ (e, v') = unbox(Eval, concl);
      let$ (e_scrut, px, el, _py, _er) = unbox(Case, e);
      let$ x = unbox(Pat, px);
      let$ (e_scrut', vl) = unbox(Eval, prems(0));
      let$ _ = expect_eq(e_scrut', e_scrut);
      let$ v_data = unbox(InjL, vl);
      let$ (el', v) = unbox(Eval, prems(1));
      let$ _ = expect_eq(el', subst(v_data, x, el));
      let$ _ = expect_eq(v', v);
      Ok();
    | E_Case_R =>
      let$ (e, v') = unbox(Eval, concl);
      let$ (e_scrut, _px, _el, py, er) = unbox(Case, e);
      let$ (e_scrut', vr) = unbox(Eval, prems(0));
      let$ _ = expect_eq(e_scrut', e_scrut);
      let$ v_data = unbox(InjR, vr);
      let$ (er', v) = unbox(Eval, prems(1));
      let er = Subst(v_data, py, Just(er));
      let$ _ = expect_eq_op(er', er);
      let$ _ = expect_eq(v', v);
      Ok();
    | E_Fix =>
      let$ (e, v') = unbox(Eval, concl);
      let$ (pat, e_body) = unbox(Fix, e);
      let$ (e_body', v) = unbox(Eval, prems(0));
      let e_body = Subst(e, pat, Just(e_body));
      let$ _ = expect_eq_op(e_body', e_body);
      let$ _ = expect_eq(v', v);
      Ok();
    | T_Roll =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(HasType, p);
      let$ e_body = unbox(Roll, e);
      let$ (tpat, t_body) = unbox(Rec, t);
      let$ (ctx', p) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e_body', t_body') = unbox(HasType, p);
      let$ _ = expect_eq(e_body', e_body);
      let t_body = SubstTy(t, tpat, Just(t_body));
      let$ _ = expect_eq_op(t_body', t_body);
      Ok();
    | T_Unroll =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t_body') = unbox(HasType, p);
      let$ e = unbox(Unroll, e);
      let$ (ctx', p) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e', t) = unbox(HasType, p);
      let$ _ = expect_eq(e', e);
      let$ (tpat, t_body) = unbox(Rec, t);
      let t_body = SubstTy(t, tpat, Just(t_body));
      let$ _ = expect_eq_op(t_body', t_body);
      Ok();
    | E_Roll =>
      let$ (e, v') = unbox(Eval, concl);
      let$ e = unbox(Roll, e);
      let$ v' = unbox(Roll, v');
      let$ (e', v) = unbox(Eval, prems(0));
      let$ _ = expect_eq(e', e);
      let$ _ = expect_eq(v', v);
      Ok();
    | E_Unroll =>
      let$ (e, v') = unbox(Eval, concl);
      let$ e = unbox(Unroll, e);
      let$ (e', v) = unbox(Eval, prems(0));
      let$ _ = expect_eq(e', e);
      let$ v = unbox(Roll, v);
      let$ _ = expect_eq(v', v);
      Ok();
    // Bidirectional Type System
    | S_Num =>
      let$ (_, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(Syn, p);
      let$ _ = unbox(NumLit, e);
      let$ _ = unbox(Num, t);
      Ok();
    | S_True =>
      let$ (_, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(Syn, p);
      let$ _ = unbox(True, e);
      let$ _ = unbox(Bool, t);
      Ok();
    | S_False =>
      let$ (_, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(Syn, p);
      let$ _ = unbox(False, e);
      let$ _ = unbox(Bool, t);
      Ok();
    | A_Subsumption =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(Ana, p);
      let$ (ctx', p) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e', t') = unbox(Syn, p);
      let$ _ = expect_eq(e', e);
      let$ _ = expect_eq(t', t);
      Ok();
    | S_Var =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (x, t) = unbox(Syn, p);
      // The same as rule Assumption
      // Note: this is a rare usage of `expect_in_ctx_op`
      let p = VarHasType(x, t);
      let$ _ = expect_in_ctx_op(p, ctx);
      Ok();
    | A_Fun =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(Ana, p);
      let$ (pat, e_body) = unbox(Fun, e);
      let$ (t_in, t_out) = unbox(Arrow, t);
      let$ (ctx_x', p) = unbox(Entail, prems(0));
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(pat, t_in));
      let$ _ = expect_eq_op(ctx_x', ctx_x);
      let$ (e_body', t_out') = unbox(Ana, p);
      let$ _ = expect_eq(e_body', e_body);
      let$ _ = expect_eq(t_out', t_out);
      Ok();
    | S_FunAnn =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(Syn, p);
      let$ (pat, e_body) = unbox(Fun, e);
      let$ (_, t_in) = unbox(PatAnn, pat);
      let$ (t_in', t_out) = unbox(Arrow, t);
      let$ _ = expect_eq(t_in', t_in);
      let$ (ctx_x', p) = unbox(Entail, prems(0));
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(pat, t_in));
      let$ _ = expect_eq_op(ctx_x', ctx_x);
      let$ (e_body', t_out') = unbox(Syn, p);
      let$ _ = expect_eq(e_body', e_body);
      let$ _ = expect_eq(t_out', t_out);
      Ok();
    | A_FunAnn =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(Ana, p);
      let$ (pat, e_body) = unbox(Fun, e);
      let$ (_, t_in) = unbox(PatAnn, pat);
      let$ (t_in', t_out) = unbox(Arrow, t);
      let$ _ = expect_eq(t_in', t_in);
      let$ (ctx_x', p) = unbox(Entail, prems(0));
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(pat, t_in));
      let$ _ = expect_eq_op(ctx_x', ctx_x);
      let$ (e_body', t_out') = unbox(Ana, p);
      let$ _ = expect_eq(e_body', e_body);
      let$ _ = expect_eq(t_out', t_out);
      Ok();
    | S_Ap =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t_out) = unbox(Syn, p);
      let$ (e_fun, e_arg) = unbox(Ap, e);
      let$ (ctx', p) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e_fun', t') = unbox(Syn, p);
      let$ _ = expect_eq(e_fun', e_fun);
      let$ (t_in, t_out') = unbox(Arrow, t');
      let$ _ = expect_eq(t_out', t_out);
      let$ (ctx', p) = unbox(Entail, prems(1));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e_arg', t_in') = unbox(Ana, p);
      let$ _ = expect_eq(e_arg', e_arg);
      let$ _ = expect_eq(t_in', t_in);
      Ok();
    | S_Neg =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(Syn, p);
      let$ (op, e1) = unbox(UnOp, e);
      let$ _ = unbox(OpNeg, op);
      let$ _ = unbox(Num, t);
      let$ (ctx', p) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e1', t) = unbox(Ana, p);
      let$ _ = expect_eq(e1', e1);
      let$ _ = unbox(Num, t);
      Ok();
    | S_Plus
    | S_Minus
    | S_Times
    | S_Lt
    | S_Gt
    | S_Eq =>
      let req_op =
        switch (rule) {
        | S_Plus => OpPlus
        | S_Minus => OpMinus
        | S_Times => OpTimes
        | S_Lt => OpLt
        | S_Gt => OpGt
        | S_Eq => OpEq
        | _ => failwith("impossible")
        };
      let req_ty =
        switch (rule) {
        | S_Plus
        | S_Minus
        | S_Times => Num
        | S_Lt
        | S_Gt
        | S_Eq => Bool
        | _ => failwith("impossible")
        };
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(Syn, p);
      let$ (op, e1, e2) = unbox(BinOp, e);
      let$ _ = unbox(req_op, op);
      let$ _ = unbox(req_ty, t);
      let$ (ctx', p) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e1', t) = unbox(Ana, p);
      let$ _ = expect_eq(e1', e1);
      let$ _ = unbox(Num, t);
      let$ (ctx', p) = unbox(Entail, prems(1));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e2', t) = unbox(Ana, p);
      let$ _ = expect_eq(e2', e2);
      let$ _ = unbox(Num, t);
      Ok();
    | S_LetAnn =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t2') = unbox(Syn, p);
      let$ (pat, e1, e2) = unbox(Let, e);
      let$ (_, t1) = unbox(PatAnn, pat);
      let$ (ctx', p1) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e1', t1') = unbox(Ana, p1);
      let$ _ = expect_eq(e1', e1);
      let$ _ = expect_eq(t1', t1);
      let$ (ctx_x', p2) = unbox(Entail, prems(1));
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(pat, t1));
      let$ _ = expect_eq_op(ctx_x', ctx_x);
      let$ (e2', t2) = unbox(Syn, p2);
      let$ _ = expect_eq(e2', e2);
      let$ _ = expect_eq(t2', t2);
      Ok();
    | A_LetAnn =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t2') = unbox(Ana, p);
      let$ (pat, e1, e2) = unbox(Let, e);
      let$ (_, t1) = unbox(PatAnn, pat);
      let$ (ctx', p1) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e1', t1') = unbox(Ana, p1);
      let$ _ = expect_eq(e1', e1);
      let$ _ = expect_eq(t1', t1);
      let$ (ctx_x', p2) = unbox(Entail, prems(1));
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(pat, t1));
      let$ _ = expect_eq_op(ctx_x', ctx_x);
      let$ (e2', t2) = unbox(Ana, p2);
      let$ _ = expect_eq(e2', e2);
      let$ _ = expect_eq(t2', t2);
      Ok();
    | S_Let =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t2') = unbox(Syn, p);
      let$ (pat, e1, e2) = unbox(Let, e);
      let$ (ctx', p1) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e1', t1) = unbox(Syn, p1);
      let$ _ = expect_eq(e1', e1);
      let$ (ctx_x', p2) = unbox(Entail, prems(1));
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(pat, t1));
      let$ _ = expect_eq_op(ctx_x', ctx_x);
      let$ (e2', t2) = unbox(Syn, p2);
      let$ _ = expect_eq(e2', e2);
      let$ _ = expect_eq(t2', t2);
      Ok();
    | A_Let =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t2') = unbox(Ana, p);
      let$ (pat, e1, e2) = unbox(Let, e);
      let$ (ctx', p1) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e1', t1) = unbox(Syn, p1);
      let$ _ = expect_eq(e1', e1);
      let$ (ctx_x', p2) = unbox(Entail, prems(1));
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(pat, t1));
      let$ _ = expect_eq_op(ctx_x', ctx_x);
      let$ (e2', t2) = unbox(Ana, p2);
      let$ _ = expect_eq(e2', e2);
      let$ _ = expect_eq(t2', t2);
      Ok();
    | S_Pair =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(Syn, p);
      let$ (e1, e2) = unbox(Pair, e);
      let$ (t1, t2) = unbox(Prod, t);
      let$ (ctx', p) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e1', t1') = unbox(Syn, p);
      let$ _ = expect_eq(e1', e1);
      let$ _ = expect_eq(t1', t1);
      let$ (ctx', p) = unbox(Entail, prems(1));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e2', t2') = unbox(Syn, p);
      let$ _ = expect_eq(e2', e2);
      let$ _ = expect_eq(t2', t2);
      Ok();
    | A_Pair =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(Ana, p);
      let$ (e1, e2) = unbox(Pair, e);
      let$ (t1, t2) = unbox(Prod, t);
      let$ (ctx', p) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e1', t1') = unbox(Ana, p);
      let$ _ = expect_eq(e1', e1);
      let$ _ = expect_eq(t1', t1);
      let$ (ctx', p) = unbox(Entail, prems(1));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e2', t2') = unbox(Ana, p);
      let$ _ = expect_eq(e2', e2);
      let$ _ = expect_eq(t2', t2);
      Ok();
    | S_LetPair =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(Syn, p);
      let$ (px, py, e1, e2) = unbox(LetPair, e);
      let$ (ctx', p) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e1', t1) = unbox(Syn, p);
      let$ _ = expect_eq(e1', e1);
      let$ (tx, ty) = unbox(Prod, t1);
      let$ (ctx_x_y', p) = unbox(Entail, prems(1));
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(px, tx));
      let ctx_x_y = ExtendCtx(ctx_x, VarHasType(py, ty));
      let$ _ = expect_eq_op(ctx_x_y', ctx_x_y);
      let$ (e2', t') = unbox(Syn, p);
      let$ _ = expect_eq(e2', e2);
      let$ _ = expect_eq(t', t);
      Ok();
    | A_LetPair =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(Syn, p);
      let$ (px, py, e1, e2) = unbox(LetPair, e);
      let$ (ctx', p) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e1', t1) = unbox(Ana, p);
      let$ _ = expect_eq(e1', e1);
      let$ (tx, ty) = unbox(Prod, t1);
      let$ (ctx_x_y', p) = unbox(Entail, prems(1));
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(px, tx));
      let ctx_x_y = ExtendCtx(ctx_x, VarHasType(py, ty));
      let$ _ = expect_eq_op(ctx_x_y', ctx_x_y);
      let$ (e2', t') = unbox(Syn, p);
      let$ _ = expect_eq(e2', e2);
      let$ _ = expect_eq(t', t);
      Ok();
    | S_PrjL =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t1) = unbox(Syn, p);
      let$ e = unbox(PrjL, e);
      let$ (ctx', p) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e', t) = unbox(Syn, p);
      let$ _ = expect_eq(e', e);
      let$ (t1', _) = unbox(Prod, t);
      let$ _ = expect_eq(t1', t1);
      Ok();
    | S_PrjR =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t2) = unbox(Syn, p);
      let$ e = unbox(PrjR, e);
      let$ (ctx', p) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e', t) = unbox(Syn, p);
      let$ _ = expect_eq(e', e);
      let$ (_, t2') = unbox(Prod, t);
      let$ _ = expect_eq(t2', t2);
      Ok();
    | S_Triv =>
      let$ (_, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(Syn, p);
      let$ _ = unbox(Triv, e);
      let$ _ = unbox(Unit, t);
      Ok();
    | A_If =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(Ana, p);
      let$ (e1, e2, e3) = unbox(If, e);
      let$ (ctx', p) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e1', t1) = unbox(Ana, p);
      let$ _ = expect_eq(e1', e1);
      let$ _ = unbox(Bool, t1);
      let$ (ctx', p) = unbox(Entail, prems(1));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e2', t') = unbox(Ana, p);
      let$ _ = expect_eq(e2', e2);
      let$ _ = expect_eq(t', t);
      let$ (ctx', p) = unbox(Entail, prems(2));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e3', t') = unbox(Ana, p);
      let$ _ = expect_eq(e3', e3);
      let$ _ = expect_eq(t', t);
      Ok();
    | S_If =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(Syn, p);
      let$ (e1, e2, e3) = unbox(If, e);
      let$ (ctx', p) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e1', t1) = unbox(Ana, p);
      let$ _ = expect_eq(e1', e1);
      let$ _ = unbox(Bool, t1);
      let$ (ctx', p) = unbox(Entail, prems(1));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e2', t') = unbox(Syn, p);
      let$ _ = expect_eq(e2', e2);
      let$ _ = expect_eq(t', t);
      let$ (ctx', p) = unbox(Entail, prems(2));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e3', t') = unbox(Syn, p);
      let$ _ = expect_eq(e3', e3);
      let$ _ = expect_eq(t', t);
      Ok();
    // ALFp
    | T_Var =>
      // The same as rule Assumption
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (_, _) = unbox(HasType, p);
      let$ _ = expect_in_ctx(p, ctx);
      Ok();
    | T_Let =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t2') = unbox(HasType, p);
      let$ (pat, e1, e2) = unbox(Let, e);
      let$ (ctx', p1) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e1', t1) = unbox(HasType, p1);
      let$ _ = expect_eq(e1', e1);
      let$ (ctx_x', p2) = unbox(Entail, prems(1));
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(pat, t1));
      let$ _ = expect_eq_op(ctx_x', ctx_x);
      let$ (e2', t2) = unbox(HasType, p2);
      let$ _ = expect_eq(e2', e2);
      let$ _ = expect_eq(t2', t2);
      Ok();
    | T_LetAnn =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t2') = unbox(HasType, p);
      let$ (pat, e1, e2) = unbox(Let, e);
      let$ (_, t1) = unbox(PatAnn, pat);
      let$ (ctx', p1) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e1', t1') = unbox(HasType, p1);
      let$ _ = expect_eq(e1', e1);
      let$ _ = expect_eq(t1', t1);
      let$ (ctx_x', p2) = unbox(Entail, prems(1));
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(pat, t1));
      let$ _ = expect_eq_op(ctx_x', ctx_x);
      let$ (e2', t2) = unbox(HasType, p2);
      let$ _ = expect_eq(e2', e2);
      let$ _ = expect_eq(t2', t2);
      Ok();
    | T_Num =>
      let$ (_, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(HasType, p);
      let$ _ = unbox(NumLit, e);
      let$ _ = unbox(Num, t);
      Ok();
    | T_True =>
      let$ (_, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(HasType, p);
      let$ _ = unbox(True, e);
      let$ _ = unbox(Bool, t);
      Ok();
    | T_False =>
      let$ (_, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(HasType, p);
      let$ _ = unbox(False, e);
      let$ _ = unbox(Bool, t);
      Ok();
    | T_Neg =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(HasType, p);
      let$ (op, e1) = unbox(UnOp, e);
      let$ _ = unbox(OpNeg, op);
      let$ _ = unbox(Num, t);
      let$ (ctx', p) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e1', t) = unbox(HasType, p);
      let$ _ = expect_eq(e1', e1);
      let$ _ = unbox(Num, t);
      Ok();
    | T_Plus
    | T_Minus
    | T_Times
    | T_Lt
    | T_Gt
    | T_Eq =>
      let req_op =
        switch (rule) {
        | T_Plus => OpPlus
        | T_Minus => OpMinus
        | T_Times => OpTimes
        | T_Lt => OpLt
        | T_Gt => OpGt
        | T_Eq => OpEq
        | _ => failwith("impossible")
        };
      let req_ty =
        switch (rule) {
        | T_Plus
        | T_Minus
        | T_Times => Num
        | T_Lt
        | T_Gt
        | T_Eq => Bool
        | _ => failwith("impossible")
        };
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(HasType, p);
      let$ (op, e1, e2) = unbox(BinOp, e);
      let$ _ = unbox(req_op, op);
      let$ _ = unbox(req_ty, t);
      let$ (ctx', p) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e1', t) = unbox(HasType, p);
      let$ _ = expect_eq(e1', e1);
      let$ _ = unbox(Num, t);
      let$ (ctx', p) = unbox(Entail, prems(1));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e2', t) = unbox(HasType, p);
      let$ _ = expect_eq(e2', e2);
      let$ _ = unbox(Num, t);
      Ok();
    | T_If =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(HasType, p);
      let$ (e1, e2, e3) = unbox(If, e);
      let$ (ctx', p) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e1', t1) = unbox(HasType, p);
      let$ _ = expect_eq(e1', e1);
      let$ _ = unbox(Bool, t1);
      let$ (ctx', p) = unbox(Entail, prems(1));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e2', t') = unbox(HasType, p);
      let$ _ = expect_eq(e2', e2);
      let$ _ = expect_eq(t', t);
      let$ (ctx', p) = unbox(Entail, prems(2));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e3', t') = unbox(HasType, p);
      let$ _ = expect_eq(e3', e3);
      let$ _ = expect_eq(t', t);
      Ok();
    | T_Fun =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(HasType, p);
      let$ (pat, e) = unbox(Fun, e);
      let$ (t_in, t_out) = unbox(Arrow, t);
      let$ (ctx_x', p) = unbox(Entail, prems(0));
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(pat, t_in));
      let$ _ = expect_eq_op(ctx_x', ctx_x);
      let$ (e', t_out') = unbox(HasType, p);
      let$ _ = expect_eq(e', e);
      let$ _ = expect_eq(t_out', t_out);
      Ok();
    | T_FunAnn =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(HasType, p);
      let$ (pat, e) = unbox(Fun, e);
      let$ (_, t_in) = unbox(PatAnn, pat);
      let$ (t_in', t_out) = unbox(Arrow, t);
      let$ _ = expect_eq(t_in', t_in);
      let$ (ctx_x', p) = unbox(Entail, prems(0));
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(pat, t_in));
      let$ _ = expect_eq_op(ctx_x', ctx_x);
      let$ (e', t_out') = unbox(HasType, p);
      let$ _ = expect_eq(e', e);
      let$ _ = expect_eq(t_out', t_out);
      Ok();
    | T_Ap =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t_out) = unbox(HasType, p);
      let$ (e1, e2) = unbox(Ap, e);
      let$ (ctx', p) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e1', t') = unbox(HasType, p);
      let$ _ = expect_eq(e1', e1);
      let$ (t_in, t_out') = unbox(Arrow, t');
      let$ _ = expect_eq(t_out', t_out);
      let$ (ctx', p) = unbox(Entail, prems(1));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e2', t_in') = unbox(HasType, p);
      let$ _ = expect_eq(e2', e2);
      let$ _ = expect_eq(t_in', t_in);
      Ok();
    | E_Pair =>
      let$ (e, v') = unbox(Eval, concl);
      let$ (e1, e2) = unbox(Pair, e);
      let$ (v1', v2') = unbox(Pair, v');
      let$ (e1', v1) = unbox(Eval, prems(0));
      let$ _ = expect_eq(e1', e1);
      let$ _ = expect_eq(v1', v1);
      let$ (e2', v2) = unbox(Eval, prems(1));
      let$ _ = expect_eq(e2', e2);
      let$ _ = expect_eq(v2', v2);
      Ok();
    | V_Pair =>
      let$ e = unbox(Val, concl);
      let$ (v1, v2) = unbox(Pair, e);
      let$ v1' = unbox(Val, prems(0));
      let$ _ = expect_eq(v1', v1);
      let$ v2' = unbox(Val, prems(1));
      let$ _ = expect_eq(v2', v2);
      Ok();
    | T_Pair =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(HasType, p);
      let$ (e1, e2) = unbox(Pair, e);
      let$ (t1, t2) = unbox(Prod, t);
      let$ (ctx', p) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e1', t1') = unbox(HasType, p);
      let$ _ = expect_eq(e1', e1);
      let$ _ = expect_eq(t1', t1);
      let$ (ctx', p) = unbox(Entail, prems(1));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e2', t2') = unbox(HasType, p);
      let$ _ = expect_eq(e2', e2);
      let$ _ = expect_eq(t2', t2);
      Ok();
    | E_PrjL =>
      let$ (e, v1') = unbox(Eval, concl);
      let$ e = unbox(PrjL, e);
      let$ (e', v) = unbox(Eval, prems(0));
      let$ _ = expect_eq(e', e);
      let$ (v1, _) = unbox(Pair, v);
      let$ _ = expect_eq(v1', v1);
      Ok();
    | E_PrjR =>
      let$ (e, v2') = unbox(Eval, concl);
      let$ e = unbox(PrjR, e);
      let$ (e', v) = unbox(Eval, prems(0));
      let$ _ = expect_eq(e', e);
      let$ (_, v2) = unbox(Pair, v);
      let$ _ = expect_eq(v2', v2);
      Ok();
    | T_PrjL =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t1) = unbox(HasType, p);
      let$ e = unbox(PrjL, e);
      let$ (ctx', p) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e', t) = unbox(HasType, p);
      let$ _ = expect_eq(e', e);
      let$ (t1', _) = unbox(Prod, t);
      let$ _ = expect_eq(t1', t1);
      Ok();
    | T_PrjR =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t2) = unbox(HasType, p);
      let$ e = unbox(PrjR, e);
      let$ (ctx', p) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e', t) = unbox(HasType, p);
      let$ _ = expect_eq(e', e);
      let$ (_, t2') = unbox(Prod, t);
      let$ _ = expect_eq(t2', t2);
      Ok();
    | E_LetPair =>
      let$ (e, v') = unbox(Eval, concl);
      let$ (px, py, e1, e2) = unbox(LetPair, e);
      let$ (e1', v1) = unbox(Eval, prems(0));
      let$ _ = expect_eq(e1', e1);
      let$ (vx, vy) = unbox(Pair, v1);
      let$ (e2', v) = unbox(Eval, prems(1));
      let e2 = Subst(vy, py, Just(e2));
      let e2 = Subst(vx, px, e2);
      let$ _ = expect_eq_op(e2', e2);
      let$ _ = expect_eq(v', v);
      Ok();
    | T_LetPair =>
      let$ (ctx, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(HasType, p);
      let$ (px, py, e1, e2) = unbox(LetPair, e);
      let$ (ctx', p) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ (e1', t1) = unbox(HasType, p);
      let$ _ = expect_eq(e1', e1);
      let$ (tx, ty) = unbox(Prod, t1);
      let$ (ctx_x_y', p) = unbox(Entail, prems(1));
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(px, tx));
      let ctx_x_y = ExtendCtx(ctx_x, VarHasType(py, ty));
      let$ _ = expect_eq_op(ctx_x_y', ctx_x_y);
      let$ (e2', t') = unbox(HasType, p);
      let$ _ = expect_eq(e2', e2);
      let$ _ = expect_eq(t', t);
      Ok();
    | V_Triv =>
      let$ e = unbox(Val, concl);
      let$ _ = unbox(Triv, e);
      Ok();
    | T_Triv =>
      let$ (_, p) = unbox(Entail, concl);
      let$ (e, t) = unbox(HasType, p);
      let$ _ = unbox(Triv, e);
      let$ _ = unbox(Unit, t);
      Ok();
    // ALF
    | V_Num =>
      let$ e = unbox(Val, concl);
      let$ _ = unbox(NumLit, e);
      Ok();
    | V_True =>
      let$ e = unbox(Val, concl);
      let$ _ = unbox(True, e);
      Ok();
    | V_False =>
      let$ e = unbox(Val, concl);
      let$ _ = unbox(False, e);
      Ok();
    | V_Fun =>
      let$ e = unbox(Val, concl);
      let$ _ = unbox(Fun, e);
      Ok();
    | E_Val =>
      let$ (e, v') = unbox(Eval, concl);
      let$ v = unbox(Val, prems(0));
      let$ _ = expect_eq(v, e);
      let$ _ = expect_eq(v', v);
      Ok();
    | E_Lt_T
    | E_Lt_F
    | E_Gt_T
    | E_Gt_F
    | E_Eq_T
    | E_Eq_F =>
      let (req_bool, req_op, binop) =
        switch (rule) {
        | E_Lt_T => (True, OpLt, Lt)
        | E_Lt_F => (False, OpLt, Lt)
        | E_Gt_T => (True, OpGt, Gt)
        | E_Gt_F => (False, OpGt, Gt)
        | E_Eq_T => (True, OpEq, Eq)
        | E_Eq_F => (False, OpEq, Eq)
        | _ => failwith("impossible")
        };
      let$ (e, v') = unbox(Eval, concl);
      let$ _ = unbox(req_bool, v');
      let$ (op, e1, e2) = unbox(BinOp, e);
      let$ _ = unbox(req_op, op);
      let$ (e1', v1) = unbox(Eval, prems(0));
      let$ _ = expect_eq(e1', e1);
      let$ (e2', v2) = unbox(Eval, prems(1));
      let$ _ = expect_eq(e2', e2);
      let v: t_op = BinOp(binop, v1, v2);
      let$ _ = expect_eq_op(v', v);
      Ok();
    | E_If_T =>
      let$ (e, v2') = unbox(Eval, concl);
      let$ (e1, e2, _e3) = unbox(If, e);
      let$ (e1', vt) = unbox(Eval, prems(0));
      let$ _ = expect_eq(e1', e1);
      let$ _ = unbox(True, vt);
      let$ (e2', v2) = unbox(Eval, prems(1));
      let$ _ = expect_eq(e2', e2);
      let$ _ = expect_eq(v2', v2);
      Ok();
    | E_If_F =>
      let$ (e, v3') = unbox(Eval, concl);
      let$ (e1, _e2, e3) = unbox(If, e);
      let$ (e1', vf) = unbox(Eval, prems(0));
      let$ _ = expect_eq(e1', e1);
      let$ _ = unbox(False, vf);
      let$ (e3', v3) = unbox(Eval, prems(1));
      let$ _ = expect_eq(e3', e3);
      let$ _ = expect_eq(v3', v3);
      Ok();
    | E_Let =>
      let$ (e, v2') = unbox(Eval, concl);
      let$ (p, e1, e2) = unbox(Let, e);
      let$ (e1', v1) = unbox(Eval, prems(0));
      let$ _ = expect_eq(e1', e1);
      let$ (e2', v2) = unbox(Eval, prems(1));
      let e2 = Subst(v1, p, Just(e2));
      let$ _ = expect_eq_op(e2', e2);
      let$ _ = expect_eq(v2', v2);
      Ok();
    | E_Ap =>
      let$ (e, v') = unbox(Eval, concl);
      let$ (e1, e2) = unbox(Ap, e);
      let$ (e1', vf) = unbox(Eval, prems(0));
      let$ _ = expect_eq(e1', e1);
      let$ (p, e_body) = unbox(Fun, vf);
      let$ (e2', v2) = unbox(Eval, prems(1));
      let$ _ = expect_eq(e2', e2);
      let$ (e_body', v) = unbox(Eval, prems(2));
      let e_body = Subst(v2, p, Just(e_body));
      let$ _ = expect_eq_op(e_body', e_body);
      let$ _ = expect_eq(v', v);
      Ok();
    // | V_NumLit =>
    //   let$ nl = unbox(Val, concl);
    //   let$ _ = unbox(NumLit, nl);
    //   Ok();
    // | E_NumLit =>
    //   let$ (e, v') = unbox(Eval, concl);
    //   let$ _ = expect_eq(v', e);
    //   Ok();
    | E_Neg =>
      let (req_op, unop) =
        switch (rule) {
        | E_Neg => (OpNeg, Neg)
        | _ => failwith("impossible")
        };
      let$ (e, v') = unbox(Eval, concl);
      let$ (op, e) = unbox(UnOp, e);
      let$ _ = unbox(req_op, op);
      let$ (e', v) = unbox(Eval, prems(0));
      let$ _ = expect_eq(e', e);
      let v: t_op = UnOp(unop, v);
      let$ _ = expect_eq_op(v', v);
      Ok();
    | E_Plus
    | E_Minus
    | E_Times =>
      let (req_op, binop) =
        switch (rule) {
        | E_Plus => (OpPlus, Plus)
        | E_Minus => (OpMinus, Minus)
        | E_Times => (OpTimes, Times)
        | _ => failwith("impossible")
        };
      let$ (e, v') = unbox(Eval, concl);
      let$ (op, e1, e2) = unbox(BinOp, e);
      let$ _ = unbox(req_op, op);
      let$ (e1', v1) = unbox(Eval, prems(0));
      let$ _ = expect_eq(e1', e1);
      let$ (e2', v2) = unbox(Eval, prems(1));
      let$ _ = expect_eq(e2', e2);
      let v: t_op = BinOp(binop, v1, v2);
      let$ _ = expect_eq_op(v', v);
      Ok();
    | Assumption =>
      let$ (ctx, prop) = unbox(Entail, concl);
      let$ _ = expect_in_ctx(prop, ctx);
      Ok();
    | And_I =>
      let$ (ctx, prop) = unbox(Entail, concl);
      let$ (a, b) = unbox(And, prop);
      let$ (ctx', a') = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ _ = expect_eq(a', a);
      let$ (ctx', b') = unbox(Entail, prems(1));
      let$ _ = expect_eq(ctx', ctx);
      let$ _ = expect_eq(b', b);
      Ok();
    | And_E_L =>
      let$ (ctx, a) = unbox(Entail, concl);
      let$ (ctx', p) = unbox(Entail, prems(0));
      let$ (a', _) = unbox(And, p);
      let$ _ = expect_eq(ctx', ctx);
      let$ _ = expect_eq(a', a);
      Ok();
    | And_E_R =>
      let$ (ctx, b) = unbox(Entail, concl);
      let$ (ctx', p) = unbox(Entail, prems(0));
      let$ (_, b') = unbox(And, p);
      let$ _ = expect_eq(ctx', ctx);
      let$ _ = expect_eq(b', b);
      Ok();
    | Or_I_L =>
      let$ (ctx, prop) = unbox(Entail, concl);
      let$ (a, _) = unbox(Or, prop);
      let$ (ctx', a') = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ _ = expect_eq(a', a);
      Ok();
    | Or_I_R =>
      let$ (ctx, prop) = unbox(Entail, concl);
      let$ (_, b) = unbox(Or, prop);
      let$ (ctx', b') = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ _ = expect_eq(b', b);
      Ok();
    | Or_E =>
      let$ (ctx, c) = unbox(Entail, concl);
      let$ (ctx', prop) = unbox(Entail, prems(0));
      let$ (a, b) = unbox(Or, prop);
      let$ _ = expect_eq(ctx, ctx');
      let$ (ctx_a', c') = unbox(Entail, prems(1));
      let ctx_a = ExtendCtx(Just(ctx), Just(a));
      let$ _ = expect_eq_op(ctx_a', ctx_a);
      let$ _ = expect_eq(c', c);
      let$ (ctx_b', c') = unbox(Entail, prems(2));
      let ctx_b = ExtendCtx(Just(ctx), Just(b));
      let$ _ = expect_eq_op(ctx_b', ctx_b);
      let$ _ = expect_eq(c', c);
      Ok();
    | Implies_I =>
      let$ (ctx, prop) = unbox(Entail, concl);
      let$ (a, b) = unbox(Implies, prop);
      let$ (ctx_a', b') = unbox(Entail, prems(0));
      let ctx_a = ExtendCtx(Just(ctx), Just(a));
      let$ _ = expect_eq_op(ctx_a', ctx_a);
      let$ _ = expect_eq(b', b);
      Ok();
    | Implies_E =>
      let$ (ctx, b) = unbox(Entail, concl);
      let$ (ctx', prop) = unbox(Entail, prems(0));
      let$ (a, b') = unbox(Implies, prop);
      let$ _ = expect_eq(ctx', ctx);
      let$ _ = expect_eq(b', b);
      let$ (ctx', a') = unbox(Entail, prems(1));
      let$ _ = expect_eq(ctx', ctx);
      let$ _ = expect_eq(a', a);
      Ok();
    | Truth_I =>
      let$ (_, prop) = unbox(Entail, concl);
      let$ _ = unbox(Truth, prop);
      Ok();
    | Falsity_E =>
      let$ (ctx, _) = unbox(Entail, concl);
      let$ (ctx', prop) = unbox(Entail, prems(0));
      let$ _ = expect_eq(ctx', ctx);
      let$ _ = unbox(Falsity, prop);
      Ok();
    };
  };
};
