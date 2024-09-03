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
    | Ann(t, t) // PatAnn(string, typ)
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
    | Ann
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
      | Ann(_) => Ann
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
      | (Ann(a1, a2), Ann(b1, b2)) => eq(a1, b1) && eq(a2, b2)
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
      | (Ann(_), _) => false
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
        | Pat(x') => String.equal(x', x)
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
      | Pat(_) => e
      | Ann(e1, t) => Ann(subst(e1), t) |> rewrap
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
      | Ann(e1, t1) => Ann(subst_ty(e1), subst_ty(t1)) |> rewrap
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

  type req('a) =
    | T: req(t)
    | Ctx: req(list(t))
    // ALFA
    | Sum(req('a), req('b)): req(('a, 'b))
    | TVar: req(string)
    | Rec(req('a), req('b)): req(('a, 'b))
    | TPat: req(string)
    | Fix(req('a), req('b)): req(('a, 'b))
    | InjL(req('a)): req('a)
    | InjR(req('a)): req('a)
    | Case(req('a), req('b), req('c), req('d), req('e))
      : req(('a, 'b, 'c, 'd, 'e))
    | Roll(req('a)): req('a)
    | Unroll(req('a)): req('a)
    // ALFp
    | Num: req(unit)
    | Bool: req(unit)
    | Arrow(req('a), req('b)): req(('a, 'b))
    | Prod(req('a), req('b)): req(('a, 'b))
    | Unit: req(unit)
    | Pair(req('a), req('b)): req(('a, 'b))
    | LetPair(req('a), req('b), req('c), req('d)): req(('a, 'b, 'c, 'd))
    | PrjL(req('a)): req('a)
    | PrjR(req('a)): req('a)
    | Triv: req(unit)
    | Pat: req(string)
    | Ann(req('a), req('b)): req(('a, 'b))
    // ALF
    | True: req(unit)
    | False: req(unit)
    | If(req('a), req('b), req('c)): req(('a, 'b, 'c))
    | Var: req(string)
    | Let(req('a), req('b), req('c)): req(('a, 'b, 'c))
    | Fun(req('a), req('b)): req(('a, 'b))
    | Ap(req('a), req('b)): req(('a, 'b))
    | OpLt: req(unit)
    | OpGt: req(unit)
    | OpEq: req(unit)
    // AL
    | NumLit: req(int)
    | UnOp(req('a), req('b)): req(('a, 'b))
    | BinOp(req('a), req('b), req('c)): req(('a, 'b, 'c))
    | OpNeg: req(unit)
    | OpPlus: req(unit)
    | OpMinus: req(unit)
    | OpTimes: req(unit)
    // ALFA proposition
    | HasType(req('a), req('b)): req(('a, 'b))
    | Syn(req('a), req('b)): req(('a, 'b))
    | Ana(req('a), req('b)): req(('a, 'b))
    // Propositional logic
    | And(req('a), req('b)): req(('a, 'b))
    | Or(req('a), req('b)): req(('a, 'b))
    | Implies(req('a), req('b)): req(('a, 'b))
    | Truth: req(unit)
    | Falsity: req(unit)
    // Judgments
    | Val(req('a)): req('a)
    | Eval(req('a), req('b)): req(('a, 'b))
    | Entail(req('a), req('b)): req(('a, 'b));

  let cls_of_req: type a. req(a) => Prop.cls =
    fun
    | T => failwith("cls_of_req: T")
    | Ctx => Ctx
    // ALFA
    | Sum(_) => Sum
    | TVar => TVar
    | Rec(_) => Rec
    | TPat => TPat
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
    | Pat => Pat
    | Ann(_) => Ann
    // ALF
    | True => True
    | False => False
    | If(_) => If
    | Var => Var
    | Let(_) => Let
    | Fun(_) => Fun
    | Ap(_) => Ap
    | OpLt => OpLt
    | OpGt => OpGt
    | OpEq => OpEq
    // AL
    | NumLit => NumLit
    | UnOp(_) => UnOp
    | BinOp(_) => BinOp
    | OpNeg => OpNeg
    | OpPlus => OpPlus
    | OpMinus => OpMinus
    | OpTimes => OpTimes
    // ALFA proposition
    | HasType(_) => HasType
    | Syn(_) => Syn
    | Ana(_) => Ana
    // Propositional logic
    | And(_) => And
    | Or(_) => Or
    | Implies(_) => Implies
    | Truth => Truth
    | Falsity => Falsity
    // Judgments
    | Val(_) => Val
    | Eval(_) => Eval
    | Entail(_) => Entail;

  let (let$) = (x, f) =>
    switch (x) {
    | Ok(x) => f(x)
    | Error(e) => Error(e)
    };

  let rec unbox: type a. (t, req(a)) => result(a, failure) =
    (p, req) => {
      switch (req, IdTagged.term_of(p)) {
      | (T, _) => Ok(p)
      | (Ctx, Ctx(l)) => Ok(l)
      | (Ctx, _) => Error(FailUnbox(Ctx, p))
      // ALFA
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
      | (Ann(ra, rb), Ann(a, b)) =>
        let$ a = unbox(a, ra);
        let$ b = unbox(b, rb);
        Ok((a, b));
      | (Ann(_), _) => Error(FailUnbox(Ann, p))
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
      | (HasType(ra, rb), HasType(a, b)) =>
        let$ a = unbox(a, ra);
        let$ b = unbox(b, rb);
        Ok((a, b));
      | (HasType(_), _) => Error(FailUnbox(HasType, p))
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
    };

  let rec go: t_op => result(t, failure) =
    a =>
      switch (a) {
      | Just(p) => Ok(p)
      | UnOp(op, e) =>
        let$ n = unbox(e, NumLit);
        switch (op) {
        | Neg => Ok(NumLit(- n) |> temp)
        };
      | BinOp(op, e1, e2) =>
        let$ n1 = unbox(e1, NumLit);
        let$ n2 = unbox(e2, NumLit);
        switch (op) {
        | Plus => Ok(NumLit(n1 + n2) |> temp)
        | Minus => Ok(NumLit(n1 - n2) |> temp)
        | Times => Ok(NumLit(n1 * n2) |> temp)
        | Lt => Ok((n1 < n2 ? True : False) |> temp)
        | Gt => Ok((n1 > n2 ? True : False) |> temp)
        | Eq => Ok((n1 == n2 ? True : False) |> temp)
        };
      | Subst(v, x, e) =>
        let$ x = unbox(x, Pat);
        let$ e = go(e);
        Ok(subst(v, x, e));
      | SubstTy(t, a, e) =>
        let$ a = unbox(a, TPat);
        let$ e = go(e);
        Ok(subst_ty(t, a, e));
      | ExtendCtx(ctx, e) =>
        let$ ctx = go(ctx);
        let$ ctx = unbox(ctx, Ctx);
        let$ e = go(e);
        Ok(Ctx(extend_ctx(ctx, e)) |> temp);
      | VarHasType(pat, t) =>
        let$ x = unbox(pat, Pat);
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

  let expect_eq_batch: (list(t), list(t)) => result(unit, failure) =
    List.fold_left2(
      (acc, a, b) => {
        let$ _ = acc;
        expect_eq(a, b);
      },
      Ok(),
    );

  let expect_in_ctx_op: (t_op, t) => result(unit, failure) =
    (p_op, ctx) => {
      let$ p = go(p_op);
      let$ pl = unbox(ctx, Ctx);
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
    let concl = unbox(concl, _);
    let prems = i => unbox(prems(i));
    switch (rule) {
    // ALFA
    | A_InjL =>
      let$ (ctx, (e, (tl, _tr))) =
        Sum(T, Ana(InjL(T), Sum(T, T))) |> concl;
      let$ (ctx', (e', tl')) = Entail(T, Ana(T, T)) |> prems(0);
      let$ _ = expect_eq_batch([ctx, e, tl], [ctx', e', tl']);
      Ok();
    | A_InjR =>
      let$ (ctx, (e, (_tl, tr))) =
        Sum(T, Ana(InjR(T), Sum(T, T))) |> concl;
      let$ (ctx', (e', tr')) = Entail(T, Ana(T, T)) |> prems(0);
      let$ _ = expect_eq_batch([ctx, e, tr], [ctx', e', tr']);
      Ok();
    | E_InjL =>
      let$ (e, v') = Eval(InjL(T), InjL(T)) |> concl;
      let$ (e', v) = Eval(T, T) |> prems(0);
      let$ _ = expect_eq_batch([e, v], [e', v']);
      Ok();
    | E_InjR =>
      let$ (e, v') = Eval(InjR(T), InjR(T)) |> concl;
      let$ (e', v) = Eval(T, T) |> prems(0);
      let$ _ = expect_eq_batch([e, v], [e', v']);
      Ok();
    | A_Case =>
      let$ (ctx, ((e_scrut, x, el, y, er), t)) =
        Entail(T, Ana(Case(T, T, T, T, T), T)) |> concl;
      let$ (ctx', (e_scrut', (tl, tr))) =
        Entail(T, Syn(T, Sum(T, T))) |> prems(0);
      let$ _ = expect_eq_batch([ctx, e_scrut], [ctx', e_scrut']);
      let$ (ctx_x', (el', t')) = Entail(T, Ana(T, T)) |> prems(1);
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(x, tl));
      let$ _ = expect_eq_op(ctx_x', ctx_x);
      let$ _ = expect_eq_batch([el, t], [el', t']);
      let$ (ctx_y', (er', t')) = Entail(T, Ana(T, T)) |> prems(2);
      let ctx_y = ExtendCtx(Just(ctx), VarHasType(y, tr));
      let$ _ = expect_eq_op(ctx_y', ctx_y);
      let$ _ = expect_eq_batch([er, t], [er', t']);
      Ok();
    | S_Case =>
      let$ (ctx, ((e_scrut, x, el, y, er), t)) =
        Entail(T, Syn(Case(T, T, T, T, T), T)) |> concl;
      let$ (ctx', (e_scrut', (tl, tr))) =
        Entail(T, Syn(T, Sum(T, T))) |> prems(0);
      let$ _ = expect_eq_batch([ctx, e_scrut], [ctx', e_scrut']);
      let$ (ctx_x', (el', t')) = Entail(T, Syn(T, T)) |> prems(1);
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(x, tl));
      let$ _ = expect_eq_op(ctx_x', ctx_x);
      let$ _ = expect_eq_batch([el, t], [el', t']);
      let$ (ctx_y', (er', t')) = Entail(T, Syn(T, T)) |> prems(2);
      let ctx_y = ExtendCtx(Just(ctx), VarHasType(y, tr));
      let$ _ = expect_eq_op(ctx_y', ctx_y);
      let$ _ = expect_eq_batch([er, t], [er', t']);
      Ok();
    | E_Case_L =>
      let$ ((e_scrut, x, el, _y, _er), v') =
        Eval(Case(T, T, T, T, T), T) |> concl;
      let$ (e_scrut', v_data) = Eval(T, InjL(T)) |> prems(0);
      let$ _ = expect_eq(e_scrut', e_scrut);
      let el = Subst(v_data, x, Just(el));
      let$ (el', v) = Eval(T, T) |> prems(1);
      let$ _ = expect_eq_op(el', el);
      let$ _ = expect_eq(v', v);
      Ok();
    | E_Case_R =>
      let$ ((e_scrut, _x, _el, y, er), v') =
        Eval(Case(T, T, T, T, T), T) |> concl;
      let$ (e_scrut', v_data) = Eval(T, InjR(T)) |> prems(0);
      let$ _ = expect_eq(e_scrut', e_scrut);
      let er = Subst(v_data, y, Just(er));
      let$ (er', v) = Eval(T, T) |> prems(1);
      let$ _ = expect_eq_op(er', er);
      let$ _ = expect_eq(v', v);
      Ok();
    | E_Fix =>
      let$ (e, v') = Eval(T, T) |> concl;
      let$ (x, e_body) = Fix(T, T) |> unbox(e);
      let e_body = Subst(e, x, Just(e_body));
      let$ (e_body', v) = Eval(T, T) |> prems(0);
      let$ _ = expect_eq_op(e_body', e_body);
      let$ _ = expect_eq(v', v);
      Ok();
    | T_Roll =>
      let$ (ctx, (e_body, t)) = Entail(T, HasType(Roll(T), T)) |> concl;
      let$ (a, t_body) = Rec(T, T) |> unbox(t);
      let t_body = SubstTy(t, a, Just(t_body));
      let$ (ctx', (e_body', t_body')) =
        Entail(T, HasType(T, T)) |> prems(0);
      let$ _ = expect_eq_batch([ctx, e_body], [ctx', e_body']);
      let$ _ = expect_eq_op(t_body', t_body);
      Ok();
    | T_Unroll =>
      let$ (ctx, (e, t_body')) =
        Entail(T, HasType(Unroll(T), T)) |> concl;
      let$ (ctx', (e', t)) = Entail(T, HasType(T, T)) |> prems(0);
      let$ _ = expect_eq_batch([ctx, e], [ctx', e']);
      let$ (a, t_body) = Rec(T, T) |> unbox(t);
      let t_body = SubstTy(t, a, Just(t_body));
      let$ _ = expect_eq_op(t_body', t_body);
      Ok();
    | E_Roll =>
      let$ (e, v') = Eval(Roll(T), Roll(T)) |> concl;
      let$ (e', v) = Eval(T, T) |> prems(0);
      let$ _ = expect_eq_batch([e, v], [e', v']);
      Ok();
    | E_Unroll =>
      let$ (e, v') = Eval(Unroll(T), T) |> concl;
      let$ (e', v) = Eval(T, Roll(T)) |> prems(0);
      let$ _ = expect_eq_batch([e, v], [e', v']);
      Ok();
    // Bidirectional Type System
    | S_Num =>
      let$ _ = Entail(T, Syn(NumLit, Num)) |> concl;
      Ok();
    | S_True =>
      let$ _ = Entail(T, Syn(True, Bool)) |> concl;
      Ok();
    | S_False =>
      let$ _ = Entail(T, Syn(False, Bool)) |> concl;
      Ok();
    | A_Subsumption =>
      let$ (ctx, (e, t)) = Entail(T, Ana(T, T)) |> concl;
      let$ (ctx', (e', t')) = Entail(T, Syn(T, T)) |> prems(0);
      let$ _ = expect_eq_batch([ctx, e, t], [ctx', e', t']);
      Ok();
    | S_Var =>
      let$ (ctx, (x, t)) = Entail(T, Syn(T, T)) |> concl;
      // TODO: This is not correct
      let p = VarHasType(x, t);
      let$ _ = expect_in_ctx_op(p, ctx);
      Ok();
    | A_Fun =>
      let$ (ctx, ((x, e_body), (t_in, t_out))) =
        Entail(T, Ana(Fun(T, T), Arrow(T, T))) |> concl;
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(x, t_in));
      let$ (ctx_x', (e_body', t_out')) = Entail(T, Ana(T, T)) |> prems(0);
      let$ _ = expect_eq_op(ctx_x', ctx_x);
      let$ _ = expect_eq_batch([e_body, t_out], [e_body', t_out']);
      Ok();
    | S_FunAnn =>
      let$ (ctx, (((x, t_in), e_body), (t_in', t_out))) =
        Entail(T, Syn(Fun(Ann(T, T), T), Arrow(T, T))) |> concl;
      let$ _ = expect_eq(t_in', t_in);
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(x, t_in));
      let$ (ctx_x', (e_body', t_out')) = Entail(T, Syn(T, T)) |> prems(0);
      let$ _ = expect_eq_op(ctx_x', ctx_x);
      let$ _ = expect_eq_batch([e_body, t_out], [e_body', t_out']);
      Ok();
    | A_FunAnn =>
      let$ (ctx, (((x, t_in), e_body), (t_in', t_out))) =
        Entail(T, Ana(Fun(Ann(T, T), T), Arrow(T, T))) |> concl;
      let$ _ = expect_eq(t_in', t_in);
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(x, t_in));
      let$ (ctx_x', (e_body', t_out')) = Entail(T, Ana(T, T)) |> prems(0);
      let$ _ = expect_eq_op(ctx_x', ctx_x);
      let$ _ = expect_eq_batch([e_body, t_out], [e_body', t_out']);
      Ok();
    | S_Ap =>
      let$ (ctx, ((e_fun, e_arg), t_out)) =
        Entail(T, Syn(Ap(T, T), T)) |> concl;
      let$ (ctx', (e_fun', (t_in, t_out'))) =
        Entail(T, Syn(T, Arrow(T, T))) |> prems(0);
      let$ _ = expect_eq_batch([ctx, e_fun, t_out], [ctx', e_fun', t_out']);
      let$ (ctx', (e_arg', t_in')) = Entail(T, Ana(T, T)) |> prems(1);
      let$ _ = expect_eq_batch([ctx, e_arg, t_in], [ctx', e_arg', t_in']);
      Ok();
    | S_Neg =>
      let$ (ctx, ((_, e), _)) =
        Entail(T, Syn(UnOp(OpNeg, T), Num)) |> concl;
      let$ (ctx', (e', _)) = Entail(T, Ana(T, Num)) |> prems(0);
      let$ _ = expect_eq_batch([ctx, e], [ctx', e']);
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
      let$ (ctx, ((_, el, er), _)) =
        Entail(T, Syn(BinOp(req_op, T, T), req_ty)) |> concl;
      let$ (ctx', (el', _)) = Entail(T, Ana(T, Num)) |> prems(0);
      let$ _ = expect_eq_batch([ctx, el], [ctx', el']);
      let$ (ctx', (er', _)) = Entail(T, Ana(T, Num)) |> prems(1);
      let$ _ = expect_eq_batch([ctx, er], [ctx', er']);
      Ok();
    | S_LetAnn =>
      let$ (ctx, (((x, t_def), e_def, e_body), t)) =
        Entail(T, Syn(Let(Ann(T, T), T, T), T)) |> concl;
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(x, t_def));
      let$ (ctx', (e_def', t_def')) = Entail(T, Ana(T, T)) |> prems(0);
      let$ _ = expect_eq_batch([ctx, e_def, t], [ctx', e_def', t_def']);
      let$ (ctx_x', (e_body', t')) = Entail(T, Syn(T, T)) |> prems(1);
      let$ _ = expect_eq_op(ctx_x', ctx_x);
      let$ _ = expect_eq_batch([e_body, t], [e_body', t']);
      Ok();
    | A_LetAnn =>
      let$ (ctx, (((x, t_def), e_def, e_body), t)) =
        Entail(T, Ana(Let(Ann(T, T), T, T), T)) |> concl;
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(x, t_def));
      let$ (ctx', (e_def', t_def')) = Entail(T, Ana(T, T)) |> prems(0);
      let$ _ = expect_eq_batch([ctx, e_def, t_def], [ctx', e_def', t_def']);
      let$ (ctx_x', (e_body', t')) = Entail(T, Ana(T, T)) |> prems(1);
      let$ _ = expect_eq_op(ctx_x', ctx_x);
      let$ _ = expect_eq_batch([e_body, t], [e_body', t']);
      Ok();
    | S_Let =>
      let$ (ctx, ((x, e_def, e_body), t)) =
        Entail(T, Syn(Let(T, T, T), T)) |> concl;
      let$ (ctx', (e_def', t_def)) = Entail(T, Syn(T, T)) |> prems(0);
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(x, t_def));
      let$ _ = expect_eq_batch([ctx, e_def], [ctx', e_def']);
      let$ (ctx_x', (e_body', t')) = Entail(T, Syn(T, T)) |> prems(1);
      let$ _ = expect_eq_op(ctx_x', ctx_x);
      let$ _ = expect_eq_batch([e_body, t], [e_body', t']);
      Ok();
    | A_Let =>
      let$ (ctx, ((x, e_def, e_body), t)) =
        Entail(T, Ana(Let(T, T, T), T)) |> concl;
      let$ (ctx', (e_def', t_def)) = Entail(T, Syn(T, T)) |> prems(0);
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(x, t_def));
      let$ _ = expect_eq_batch([ctx, e_def], [ctx', e_def']);
      let$ (ctx_x', (e_body', t')) = Entail(T, Ana(T, T)) |> prems(1);
      let$ _ = expect_eq_op(ctx_x', ctx_x);
      let$ _ = expect_eq_batch([e_body, t], [e_body', t']);
      Ok();
    | S_Pair =>
      let$ (ctx, ((el, er), (tl, tr))) =
        Entail(T, Syn(Pair(T, T), Prod(T, T))) |> concl;
      let$ (ctx', (el', tl')) = Entail(T, Syn(T, T)) |> prems(0);
      let$ _ = expect_eq_batch([ctx, el, tl], [ctx', el', tl']);
      let$ (ctx', (er', tr')) = Entail(T, Syn(T, T)) |> prems(1);
      let$ _ = expect_eq_batch([ctx, er, tr], [ctx', er', tr']);
      Ok();
    | A_Pair =>
      let$ (ctx, ((el, er), (tl, tr))) =
        Entail(T, Ana(Pair(T, T), Prod(T, T))) |> concl;
      let$ (ctx', (el', tl')) = Entail(T, Ana(T, T)) |> prems(0);
      let$ _ = expect_eq_batch([ctx, el, tl], [ctx', el', tl']);
      let$ (ctx', (er', tr')) = Entail(T, Ana(T, T)) |> prems(1);
      let$ _ = expect_eq_batch([ctx, er, tr], [ctx', er', tr']);
      Ok();
    | S_LetPair =>
      let$ (ctx, ((x, y, e_def, e_body), t)) =
        Entail(T, Syn(LetPair(T, T, T, T), T)) |> concl;
      let$ (ctx', (e_def', (tl, tr))) =
        Entail(T, Syn(T, Prod(T, T))) |> prems(0);
      let$ _ = expect_eq_batch([ctx, e_def], [ctx', e_def']);
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(x, tl));
      let ctx_x_y = ExtendCtx(ctx_x, VarHasType(y, tr));
      let$ (ctx_x_y', (e_body', t')) = Entail(T, Syn(T, T)) |> prems(1);
      let$ _ = expect_eq_op(ctx_x_y', ctx_x_y);
      let$ _ = expect_eq_batch([e_body, t], [e_body', t']);
      Ok();
    | A_LetPair =>
      let$ (ctx, ((x, y, e_def, e_body), t)) =
        Entail(T, Ana(LetPair(T, T, T, T), T)) |> concl;
      let$ (ctx', (e_def', (tl, tr))) =
        Entail(T, Syn(T, Prod(T, T))) |> prems(0);
      let$ _ = expect_eq_batch([ctx, e_def], [ctx', e_def']);
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(x, tl));
      let ctx_x_y = ExtendCtx(ctx_x, VarHasType(y, tr));
      let$ (ctx_x_y', (e_body', t')) = Entail(T, Ana(T, T)) |> prems(1);
      let$ _ = expect_eq_op(ctx_x_y', ctx_x_y);
      let$ _ = expect_eq_batch([e_body, t], [e_body', t']);
      Ok();
    | S_PrjL =>
      let$ (ctx, (e, tl)) = Entail(T, Syn(PrjL(T), T)) |> concl;
      let$ (ctx', (e', (tl', _tr))) =
        Entail(T, Syn(T, Prod(T, T))) |> prems(0);
      let$ _ = expect_eq_batch([ctx, e, tl], [ctx', e', tl']);
      Ok();
    | S_PrjR =>
      let$ (ctx, (e, tr)) = Entail(T, Syn(PrjR(T), T)) |> concl;
      let$ (ctx', (e', (_tl, tr'))) =
        Entail(T, Syn(T, Prod(T, T))) |> prems(0);
      let$ _ = expect_eq_batch([ctx, e, tr], [ctx', e', tr']);
      Ok();
    | S_Triv =>
      let$ _ = Entail(T, Syn(Triv, Unit)) |> concl;
      Ok();
    | A_If =>
      let$ (ctx, ((e_cond, e_then, e_else), t)) =
        Entail(T, Ana(If(T, T, T), T)) |> concl;
      let$ (ctx', (e_cond', _)) = Entail(T, Ana(T, Bool)) |> prems(0);
      let$ _ = expect_eq_batch([ctx, e_cond], [ctx', e_cond']);
      let$ (ctx', (e_then', t')) = Entail(T, Ana(T, T)) |> prems(1);
      let$ _ = expect_eq_batch([ctx, e_then, t], [ctx', e_then', t']);
      let$ (ctx', (e_else', t')) = Entail(T, Ana(T, T)) |> prems(2);
      let$ _ = expect_eq_batch([ctx, e_else, t], [ctx', e_else', t']);
      Ok();
    | S_If =>
      let$ (ctx, ((e_cond, e_then, e_else), t)) =
        Entail(T, Syn(If(T, T, T), T)) |> concl;
      let$ (ctx', (e_cond', _)) = Entail(T, Ana(T, Bool)) |> prems(0);
      let$ _ = expect_eq_batch([ctx, e_cond], [ctx', e_cond']);
      let$ (ctx', (e_then', t')) = Entail(T, Syn(T, T)) |> prems(1);
      let$ _ = expect_eq_batch([ctx, e_then, t], [ctx', e_then', t']);
      let$ (ctx', (e_else', t')) = Entail(T, Syn(T, T)) |> prems(2);
      let$ _ = expect_eq_batch([ctx, e_else, t], [ctx', e_else', t']);
      Ok();
    // ALFp
    | T_Var =>
      // TODO: The same as rule Assumption
      let$ (ctx, p) = Entail(T, T) |> concl;
      let$ _ = HasType(T, T) |> unbox(p);
      let$ _ = expect_in_ctx(p, ctx);
      Ok();
    | T_Let =>
      let$ (ctx, ((x, e_def, e_body), t)) =
        Entail(T, HasType(Let(T, T, T), T)) |> concl;
      let$ (ctx', (e_def', t_def)) = Entail(T, HasType(T, T)) |> prems(0);
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(x, t_def));
      let$ _ = expect_eq_batch([ctx, e_def], [ctx', e_def']);
      let$ (ctx_x', (e_body', t')) = Entail(T, HasType(T, T)) |> prems(1);
      let$ _ = expect_eq_op(ctx_x', ctx_x);
      let$ _ = expect_eq_batch([e_body, t], [e_body', t']);
      Ok();
    | T_LetAnn =>
      let$ (ctx, (((x, t_def), e_def, e_body), t)) =
        Entail(T, HasType(Let(Ann(T, T), T, T), T)) |> concl;
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(x, t_def));
      let$ (ctx', (e_def', t_def')) =
        Entail(T, HasType(T, T)) |> prems(0);
      let$ _ = expect_eq_batch([ctx, e_def, t], [ctx', e_def', t_def']);
      let$ (ctx_x', (e_body', t')) = Entail(T, HasType(T, T)) |> prems(1);
      let$ _ = expect_eq_op(ctx_x', ctx_x);
      let$ _ = expect_eq_batch([e_body, t], [e_body', t']);
      Ok();
    | T_Num =>
      let$ _ = Entail(T, HasType(NumLit, Num)) |> concl;
      Ok();
    | T_True =>
      let$ _ = Entail(T, HasType(True, Bool)) |> concl;
      Ok();
    | T_False =>
      let$ _ = Entail(T, HasType(False, Bool)) |> concl;
      Ok();
    | T_Neg =>
      let$ (ctx, ((_, e), _)) =
        Entail(T, HasType(UnOp(OpNeg, T), Num)) |> concl;
      let$ (ctx', (e', _)) = Entail(T, HasType(T, Num)) |> prems(0);
      let$ _ = expect_eq_batch([ctx, e], [ctx', e']);
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
      let$ (ctx, ((_, el, er), _)) =
        Entail(T, HasType(BinOp(req_op, T, T), req_ty)) |> concl;
      let$ (ctx', (el', _)) = Entail(T, HasType(T, Num)) |> prems(0);
      let$ _ = expect_eq_batch([ctx, el], [ctx', el']);
      let$ (ctx', (er', _)) = Entail(T, HasType(T, Num)) |> prems(1);
      let$ _ = expect_eq_batch([ctx, er], [ctx', er']);
      Ok();
    | T_If =>
      let$ (ctx, ((e_cond, e_then, e_else), t)) =
        Entail(T, HasType(If(T, T, T), T)) |> concl;
      let$ (ctx', (e_cond', _)) = Entail(T, HasType(T, Bool)) |> prems(0);
      let$ _ = expect_eq_batch([ctx, e_cond], [ctx', e_cond']);
      let$ (ctx', (e_then', t')) = Entail(T, HasType(T, T)) |> prems(1);
      let$ _ = expect_eq_batch([ctx, e_then, t], [ctx', e_then', t']);
      let$ (ctx', (e_else', t')) = Entail(T, HasType(T, T)) |> prems(2);
      let$ _ = expect_eq_batch([ctx, e_else, t], [ctx', e_else', t']);
      Ok();
    | T_Fun =>
      let$ (ctx, ((x, e_body), (t_in, t_out))) =
        Entail(T, HasType(Fun(T, T), Arrow(T, T))) |> concl;
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(x, t_in));
      let$ (ctx_x', (e_body', t_out')) =
        Entail(T, HasType(T, T)) |> prems(0);
      let$ _ = expect_eq_op(ctx_x', ctx_x);
      let$ _ = expect_eq_batch([e_body, t_out], [e_body', t_out']);
      Ok();
    | T_FunAnn =>
      let$ (ctx, (((x, t_in), e_body), (t_in', t_out))) =
        Entail(T, HasType(Fun(Ann(T, T), T), Arrow(T, T))) |> concl;
      let$ _ = expect_eq(t_in', t_in);
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(x, t_in));
      let$ (ctx_x', (e_body', t_out')) =
        Entail(T, HasType(T, T)) |> prems(0);
      let$ _ = expect_eq_op(ctx_x', ctx_x);
      let$ _ = expect_eq_batch([e_body, t_out], [e_body', t_out']);
      Ok();
    | T_Ap =>
      let$ (ctx, ((e_fun, e_arg), t_out)) =
        Entail(T, HasType(Ap(T, T), T)) |> concl;
      let$ (ctx', (e_fun', (t_in, t_out'))) =
        Entail(T, HasType(T, Arrow(T, T))) |> prems(0);
      let$ _ = expect_eq_batch([ctx, e_fun, t_out], [ctx', e_fun', t_out']);
      let$ (ctx', (e_arg', t_in')) = Entail(T, HasType(T, T)) |> prems(1);
      let$ _ = expect_eq_batch([ctx, e_arg, t_in], [ctx', e_arg', t_in']);
      Ok();
    | E_Pair =>
      let$ ((el, er), (v1', v2')) =
        Eval(Pair(T, T), Pair(T, T)) |> concl;
      let$ (el', v1) = Eval(T, T) |> prems(0);
      let$ _ = expect_eq_batch([el, v1'], [el', v1]);
      let$ (er', v2) = Eval(T, T) |> prems(1);
      let$ _ = expect_eq_batch([er, v2'], [er', v2]);
      Ok();
    | V_Pair =>
      let$ (v1, v2) = Val(Pair(T, T)) |> concl;
      let$ v1' = Val(T) |> prems(0);
      let$ _ = expect_eq(v1, v1');
      let$ v2' = Val(T) |> prems(1);
      let$ _ = expect_eq(v2, v2');
      Ok();
    | T_Pair =>
      let$ (ctx, ((el, er), (tl, tr))) =
        Entail(T, HasType(Pair(T, T), Prod(T, T))) |> concl;
      let$ (ctx', (el', tl')) = Entail(T, HasType(T, T)) |> prems(0);
      let$ _ = expect_eq_batch([ctx, el, tl], [ctx', el', tl']);
      let$ (ctx', (er', tr')) = Entail(T, HasType(T, T)) |> prems(1);
      let$ _ = expect_eq_batch([ctx, er, tr], [ctx', er', tr']);
      Ok();
    | E_PrjL =>
      let$ (e, vl') = Eval(PrjL(T), T) |> concl;
      let$ (e', (vl, _vr)) = Eval(T, Pair(T, T)) |> prems(0);
      let$ _ = expect_eq_batch([e, vl'], [e', vl]);
      Ok();
    | E_PrjR =>
      let$ (e, vr') = Eval(PrjR(T), T) |> concl;
      let$ (e', (_vl, vr)) = Eval(T, Pair(T, T)) |> prems(0);
      let$ _ = expect_eq_batch([e, vr'], [e', vr]);
      Ok();
    | T_PrjL =>
      let$ (ctx, (e, tl)) = Entail(T, HasType(PrjL(T), T)) |> concl;
      let$ (ctx', (e', (tl', _tr))) =
        Entail(T, HasType(T, Prod(T, T))) |> prems(0);
      let$ _ = expect_eq_batch([ctx, e, tl], [ctx', e', tl']);
      Ok();
    | T_PrjR =>
      let$ (ctx, (e, tr)) = Entail(T, HasType(PrjR(T), T)) |> concl;
      let$ (ctx', (e', (_tl, tr'))) =
        Entail(T, HasType(T, Prod(T, T))) |> prems(0);
      let$ _ = expect_eq_batch([ctx, e, tr], [ctx', e', tr']);
      Ok();
    | E_LetPair =>
      let$ ((x, y, e_def, e_body), v') =
        Eval(LetPair(T, T, T, T), T) |> concl;
      let$ (e_def', (vl, vr)) = Eval(T, Pair(T, T)) |> prems(0);
      let$ _ = expect_eq(e_def', e_def);
      let e_body = Subst(vr, y, Subst(vl, x, Just(e_body)));
      let$ (e_body', v) = Eval(T, T) |> prems(1);
      let$ _ = expect_eq_op(e_body', e_body);
      let$ _ = expect_eq(v', v);
      Ok();
    | T_LetPair =>
      let$ (ctx, ((x, y, e_def, e_body), t)) =
        Entail(T, HasType(LetPair(T, T, T, T), T)) |> concl;
      let$ (ctx', (e_def', (tl, tr))) =
        Entail(T, HasType(T, Prod(T, T))) |> prems(0);
      let$ _ = expect_eq_batch([ctx, e_def], [ctx', e_def']);
      let ctx_x = ExtendCtx(Just(ctx), VarHasType(x, tl));
      let ctx_x_y = ExtendCtx(ctx_x, VarHasType(y, tr));
      let$ (ctx_x_y', (e_body', t')) =
        Entail(T, HasType(T, T)) |> prems(1);
      let$ _ = expect_eq_op(ctx_x_y', ctx_x_y);
      let$ _ = expect_eq_batch([e_body, t], [e_body', t']);
      Ok();
    | V_Triv =>
      let$ _ = Val(Triv) |> concl;
      Ok();
    | T_Triv =>
      let$ _ = Entail(T, HasType(Triv, Unit)) |> concl;
      Ok();
    // ALF
    | V_Num =>
      let$ _ = Val(NumLit) |> concl;
      Ok();
    | V_True =>
      let$ _ = Val(True) |> concl;
      Ok();
    | V_False =>
      let$ _ = Val(False) |> concl;
      Ok();
    | V_Fun =>
      let$ _ = Val(Fun(T, T)) |> concl;
      Ok();
    | E_Val =>
      let$ (e, v') = Eval(T, T) |> concl;
      let$ v = Val(T) |> prems(0);
      let$ _ = expect_eq_batch([e, v], [v, v']);
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
      let$ ((_, e1, e2), _) = Eval(BinOp(req_op, T, T), req_bool) |> concl;
      let$ (e1', v1) = Eval(T, T) |> prems(0);
      let$ _ = expect_eq(e1', e1);
      let$ (e2', v2) = Eval(T, T) |> prems(1);
      let$ _ = expect_eq(e2', e2);
      let _v: t_op = BinOp(binop, v1, v2);
      // TODO: Check if the result is correct
      // let$ _ = expect_eq_op(v', v);
      Ok();
    | E_If_T =>
      let$ ((e_cond, e_then, _e_else), v') = Eval(If(T, T, T), T) |> concl;
      let$ (e_cond', _) = Eval(T, True) |> prems(0);
      let$ _ = expect_eq(e_cond', e_cond);
      let$ (e_then', v) = Eval(T, T) |> prems(1);
      let$ _ = expect_eq(e_then', e_then);
      let$ _ = expect_eq(v', v);
      Ok();
    | E_If_F =>
      let$ ((e_cond, _e_then, e_else), v') = Eval(If(T, T, T), T) |> concl;
      let$ (e_cond', _) = Eval(T, False) |> prems(0);
      let$ _ = expect_eq(e_cond', e_cond);
      let$ (e_else', v) = Eval(T, T) |> prems(1);
      let$ _ = expect_eq(e_else', e_else);
      let$ _ = expect_eq(v', v);
      Ok();
    | E_Let =>
      let$ ((x, e_def, e_body), v') = Eval(Let(T, T, T), T) |> concl;
      let$ (e_def', v_def) = Eval(T, T) |> prems(0);
      let$ _ = expect_eq(e_def', e_def);
      let e_body = Subst(v_def, x, Just(e_body));
      let$ (e_body', v) = Eval(T, T) |> prems(1);
      let$ _ = expect_eq_op(e_body', e_body);
      let$ _ = expect_eq(v', v);
      Ok();
    | E_Ap =>
      let$ ((e_fun, e_arg), v') = Eval(Ap(T, T), T) |> concl;
      let$ (e_fun', (x, e_body)) = Eval(T, Fun(T, T)) |> prems(0);
      let$ _ = expect_eq(e_fun', e_fun);
      let$ (e_arg', v_arg) = Eval(T, T) |> prems(1);
      let$ _ = expect_eq(e_arg', e_arg);
      let e_body = Subst(v_arg, x, Just(e_body));
      let$ (e_body', v) = Eval(T, T) |> prems(2);
      let$ _ = expect_eq_op(e_body', e_body);
      let$ _ = expect_eq(v', v);
      Ok();
    // AL
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
      let$ ((_, e), v') = Eval(UnOp(req_op, T), T) |> concl;
      let$ (e', v) = Eval(T, T) |> prems(0);
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
      let$ ((_, e1, e2), v') = Eval(BinOp(req_op, T, T), T) |> concl;
      let$ (e1', v1) = Eval(T, T) |> prems(0);
      let$ _ = expect_eq(e1', e1);
      let$ (e2', v2) = Eval(T, T) |> prems(1);
      let$ _ = expect_eq(e2', e2);
      let v: t_op = BinOp(binop, v1, v2);
      let$ _ = expect_eq_op(v', v);
      Ok();
    | Assumption =>
      let$ (ctx, p) = Entail(T, T) |> concl;
      let$ _ = expect_in_ctx(p, ctx);
      Ok();
    | And_I =>
      let$ (ctx, (a, b)) = Entail(T, And(T, T)) |> concl;
      let$ (ctx', a') = Entail(T, T) |> prems(0);
      let$ _ = expect_eq(ctx', ctx);
      let$ _ = expect_eq(a', a);
      let$ (ctx', b') = Entail(T, T) |> prems(1);
      let$ _ = expect_eq(ctx', ctx);
      let$ _ = expect_eq(b', b);
      Ok();
    | And_E_L =>
      let$ (ctx, a) = Entail(T, T) |> concl;
      let$ (ctx', (a', _)) = Entail(T, And(T, T)) |> prems(0);
      let$ _ = expect_eq(ctx', ctx);
      let$ _ = expect_eq(a', a);
      Ok();
    | And_E_R =>
      let$ (ctx, b) = Entail(T, T) |> concl;
      let$ (ctx', (_, b')) = Entail(T, And(T, T)) |> prems(0);
      let$ _ = expect_eq(ctx', ctx);
      let$ _ = expect_eq(b', b);
      Ok();
    | Or_I_L =>
      let$ (ctx, (a, _)) = Entail(T, Or(T, T)) |> concl;
      let$ (ctx', a') = Entail(T, T) |> prems(0);
      let$ _ = expect_eq(ctx', ctx);
      let$ _ = expect_eq(a', a);
      Ok();
    | Or_I_R =>
      let$ (ctx, (_, b)) = Entail(T, Or(T, T)) |> concl;
      let$ (ctx', b') = Entail(T, T) |> prems(0);
      let$ _ = expect_eq(ctx', ctx);
      let$ _ = expect_eq(b', b);
      Ok();
    | Or_E =>
      let$ (ctx, c) = Entail(T, T) |> concl;
      let$ (ctx', (a, b)) = Entail(T, Or(T, T)) |> prems(0);
      let$ _ = expect_eq(ctx, ctx');
      let$ (ctx_a', c') = Entail(T, T) |> prems(1);
      let ctx_a = ExtendCtx(Just(ctx), Just(a));
      let$ _ = expect_eq_op(ctx_a', ctx_a);
      let$ _ = expect_eq(c', c);
      let$ (ctx_b', c') = Entail(T, T) |> prems(2);
      let ctx_b = ExtendCtx(Just(ctx), Just(b));
      let$ _ = expect_eq_op(ctx_b', ctx_b);
      let$ _ = expect_eq(c', c);
      Ok();
    | Implies_I =>
      let$ (ctx, (a, b)) = Entail(T, Implies(T, T)) |> concl;
      let$ (ctx_a', b') = Entail(T, T) |> prems(0);
      let ctx_a = ExtendCtx(Just(ctx), Just(a));
      let$ _ = expect_eq_op(ctx_a', ctx_a);
      let$ _ = expect_eq(b', b);
      Ok();
    | Implies_E =>
      let$ (ctx, b) = Entail(T, T) |> concl;
      let$ (ctx', (a, b')) = Entail(T, Implies(T, T)) |> prems(0);
      let$ _ = expect_eq(ctx', ctx);
      let$ _ = expect_eq(b', b);
      let$ (ctx', a') = Entail(T, T) |> prems(1);
      let$ _ = expect_eq(ctx', ctx);
      let$ _ = expect_eq(a', a);
      Ok();
    | Truth_I =>
      let$ _ = Entail(T, Truth) |> concl;
      Ok();
    | Falsity_E =>
      let$ (ctx, _) = Entail(T, T) |> concl;
      let$ (ctx', _) = Entail(T, Falsity) |> prems(0);
      let$ _ = expect_eq(ctx', ctx);
      Ok();
    };
  };
};
