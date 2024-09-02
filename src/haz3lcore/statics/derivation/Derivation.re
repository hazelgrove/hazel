open Util;

/*

 Prop.t is used as the internal representation of a judgement expression. It is
 converted from the evaluation result of an editor. An editor that is requested
 to be evaluated to Prop should have its result be Prop type Constructor.

 */

// // ALF logic
// type expr =
//   | NumLit(int)
//   | True
//   | False
//   | UnOp(unop, expr)
//   | BinOp(binop, expr, expr)
//   | If(expr, expr, expr)
//   | Var(pat)
//   | Let(pat, expr, expr)
//   | Fun(pat, expr)
//   | Ap(expr, expr)
// and pat =
//   | PVar(string)
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
    // ALF
    | True
    | False
    | If(t, t, t)
    | Var(string)
    | Let(string, t, t)
    | Fun(string, t)
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

  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Hole
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
      // ALF
      | (True, True)
      | (False, False) => true
      | (If(a1, a2, a3), If(b1, b2, b3)) =>
        eq(a1, b1) && eq(a2, b2) && eq(a3, b3)
      | (Var(a), Var(b)) => String.equal(a, b)
      | (Let(a1, a2, a3), Let(b1, b2, b3)) =>
        String.equal(a1, b1) && eq(a2, b2) && eq(a3, b3)
      | (Fun(a1, a2), Fun(b1, b2)) => String.equal(a1, b1) && eq(a2, b2)
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
      | (OpTimes, _)
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
      // let subst_shadow = x' => (x' == x ? Fun.id : subst);
      switch (term) {
      | Hole(_) => e
      // ALF
      | True
      | False => e
      | If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3)) |> rewrap
      | Var(x') => String.equal(x', x) ? v : e
      | Let(x', e1, e2) =>
        Let(x', subst(e1), String.equal(x', x) ? e2 : subst(e2)) |> rewrap
      | Fun(x', e1) =>
        Fun(x', String.equal(x', x) ? e1 : subst(e1)) |> rewrap
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

  let extend_ctx = (ctx: list(t), prop: t) =>
    if (in_ctx(ctx, prop)) {
      ctx;
    } else {
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

    // | V_NumLit => "V-NumLit"
    // | E_NumLit => "E-NumLit"
    | E_Neg => "E-Neg"
    | E_Plus => "E-Plus"
    | E_Minus => "E-Minus"
    | E_Times => "E-Times"

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

    // | V_NumLit => 0
    // | E_NumLit => 0
    | E_Neg => 1
    | E_Plus => 2
    | E_Minus => 2
    | E_Times => 2

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
  type failure = (term, list(Id.t))
  and term =
    // Common
    | PremiseMismatch(int, int) /* expected, actual */
    | FailUnbox(Prop.cls)
    | NotEqual(Prop.cls)
    // AL
    | UnOpArithError(Prop.cls)
    | BinOpArithError(Prop.cls)
    // Propositional logic (might be removed)
    | NotInCtx
    | FailCtxExtend;

  let rep_ids = List.map(IdTagged.rep_id);

  type unbox_req('a) =
    // ALF
    | True: unbox_req(unit)
    | False: unbox_req(unit)
    | If: unbox_req((t, t, t))
    | Var: unbox_req(string)
    | Let: unbox_req((string, t, t))
    | Fun: unbox_req((string, t))
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
    | Val => Val
    | UnOp => UnOp
    | BinOp => BinOp
    | OpNeg => OpNeg
    | OpPlus => OpPlus
    | OpMinus => OpMinus
    | OpTimes => OpTimes
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
      | (Val, Val(a)) => Ok(a)
      | (UnOp, UnOp(a, b)) => Ok((a, b))
      | (BinOp, BinOp(a, b, c)) => Ok((a, b, c))
      | (OpNeg, OpNeg) => Ok()
      | (OpPlus, OpPlus) => Ok()
      | (OpMinus, OpMinus) => Ok()
      | (OpTimes, OpTimes) => Ok()
      | (Eval, Eval(a, b)) => Ok((a, b))
      // Propositional logic
      | (And, And(a, b)) => Ok((a, b))
      | (Or, Or(a, b)) => Ok((a, b))
      | (Implies, Implies(a, b)) => Ok((a, b))
      | (Truth, Truth) => Ok()
      | (Falsity, Falsity) => Ok()
      | (Entail, Entail(a, b)) => Ok((a, b))
      | (Ctx, Ctx(a)) => Ok(a)

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
      | (Val, _)
      | (Eval, _)
      | (And, _)
      | (Or, _)
      | (Implies, _)
      | (Truth, _)
      | (Falsity, _)
      | (Entail, _)
      | (Ctx, _) => Error((FailUnbox(cls_of_req(req)), rep_ids([p])))
      };
    };

  let (let$) = (x, f) =>
    switch (x) {
    | Ok(x) => f(x)
    | Error(e) => Error(e)
    };

  // NotEqual
  let expect_eq: (t, t) => result(unit, failure) =
    (a, b) =>
      eq(a, b) ? Ok() : Error((NotEqual(of_cls(a)), rep_ids([a, b])));

  // NotInCtx
  let expect_in_ctx: (t, t) => result(unit, failure) =
    (p, ctx) => {
      let$ pl = unbox(Ctx, ctx);
      in_ctx(pl, p) ? Ok() : Error((NotInCtx, rep_ids([p, ctx])));
    };

  // FailCtxExtend
  let expect_eq_after_extend: (t, t, t) => result(unit, failure) =
    (ctx_a, ctx, a) => {
      let$ pl_a = unbox(Ctx, ctx_a);
      let$ pl = unbox(Ctx, ctx);
      let ctx_a_expected = extend_ctx(pl, a);
      List.for_all2(eq, ctx_a_expected, pl_a)
        ? Ok() : Error((FailCtxExtend, rep_ids([ctx_a, ctx, a])));
    };

  let expect_unop_arith: (cls, t, t) => result(unit, failure) =
    (op, v', v) => {
      let$ n = unbox(NumLit, v);
      let$ n' = unbox(NumLit, v');
      switch (op) {
      | OpNeg when n' == - n => Ok()
      | _ => Error((UnOpArithError(op), rep_ids([v', v])))
      };
    };

  let expect_binop_arith: (Prop.cls, t, t, t) => result(unit, failure) =
    (op, v', v1, v2) => {
      let$ n1 = unbox(NumLit, v1);
      let$ n2 = unbox(NumLit, v2);
      let$ n' = unbox(NumLit, v');
      switch (op) {
      | OpPlus when n' == n1 + n2 => Ok()
      | OpMinus when n' == n1 - n2 => Ok()
      | OpTimes when n' == n1 * n2 => Ok()
      | _ => Error((BinOpArithError(op), rep_ids([v', v1, v2])))
      };
    };

  let expect_compare: (Prop.cls, bool, t, t) => result(unit, failure) =
    (op, res, v1, v2) => {
      let$ n1 = unbox(NumLit, v1);
      let$ n2 = unbox(NumLit, v2);
      switch (op) {
      | OpLt when n1 < n2 == res => Ok()
      | OpGt when n1 > n2 == res => Ok()
      | OpEq when n1 == n2 == res => Ok()
      // TODO(zhiyao): add inspect to Bool
      | _ => Error((BinOpArithError(op), rep_ids([v1, v2])))
      };
    };

  let expect_prems_num: (Rule.t, list(Prop.t)) => result(int => t, failure) =
    (rule, prems) => {
      let got = List.length(prems);
      let expect = Rule.prems_num(rule);
      expect == got
        ? Ok(x => List.nth(prems, x))
        : Error((PremiseMismatch(expect, got), []));
    };

  let verify =
      (rule: Rule.t, prems: list(t), concl: t): result(unit, failure) => {
    let$ prems = expect_prems_num(rule, prems);
    print_endline("concl: " ++ show(concl));
    switch (rule) {
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
      let (req_bool, req_op) =
        switch (rule) {
        | E_Lt_T => (True, OpLt)
        | E_Lt_F => (False, OpLt)
        | E_Gt_T => (True, OpGt)
        | E_Gt_F => (False, OpGt)
        | E_Eq_T => (True, OpEq)
        | E_Eq_F => (False, OpEq)
        | _ => failwith("impossible")
        };
      let$ (e, v') = unbox(Eval, concl);
      let$ _ = unbox(req_bool, v');
      let$ (op, e1, e2) = unbox(BinOp, e);
      let$ _ = unbox(req_op, op);
      let$ (e1', v1) = unbox(Eval, prems(0));
      let$ _ = expect_eq(e1', e1);
      let$ n1 = unbox(NumLit, v1);
      let$ (e2', v2) = unbox(Eval, prems(1));
      let$ _ = expect_eq(e2', e2);
      let$ n2 = unbox(NumLit, v2);
      switch (rule) {
      | E_Lt_T when n1 < n2 => Ok()
      | E_Gt_T when n1 > n2 => Ok()
      | E_Eq_T when n1 == n2 => Ok()
      | E_Lt_F when !(n1 < n2) => Ok()
      | E_Gt_F when !(n1 > n2) => Ok()
      | E_Eq_F when !(n1 == n2) => Ok()
      | _ => Error((BinOpArithError(of_cls(op)), rep_ids([v', v1, v2])))
      };
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
      let$ (x, e1, e2) = unbox(Let, e);
      let$ (e1', v1) = unbox(Eval, prems(0));
      let$ _ = expect_eq(e1', e1);
      let$ (e2', v2) = unbox(Eval, prems(1));
      // TODO(zhiyao): implement subst
      let$ _ = expect_eq(e2', subst(v1, x, e2));
      let$ _ = expect_eq(v2', v2);
      Ok();
    | E_Ap =>
      let$ (e, v') = unbox(Eval, concl);
      let$ (e1, e2) = unbox(Ap, e);
      let$ (e1', vf) = unbox(Eval, prems(0));
      let$ _ = expect_eq(e1', e1);
      let$ (x, e_body) = unbox(Fun, vf);
      let$ (e2', v2) = unbox(Eval, prems(1));
      let$ _ = expect_eq(e2', e2);
      let$ (e_body', v) = unbox(Eval, prems(2));
      let$ _ = expect_eq(e_body', subst(v2, x, e_body));
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
      let$ (e, v') = unbox(Eval, concl);
      let$ (op, e) = unbox(UnOp, e);
      let$ _ = unbox(OpNeg, op);
      let$ (e', v) = unbox(Eval, prems(0));
      let$ _ = expect_eq(e', e);
      let$ _ = expect_unop_arith(OpNeg, v', v);
      Ok();
    | E_Plus
    | E_Minus
    | E_Times =>
      let req_op =
        switch (rule) {
        | E_Plus => OpPlus
        | E_Minus => OpMinus
        | E_Times => OpTimes
        | _ => failwith("impossible")
        };
      let$ (e, v') = unbox(Eval, concl);
      let$ n' = unbox(NumLit, v');
      let$ (op, e1, e2) = unbox(BinOp, e);
      let$ _ = unbox(req_op, op);
      let$ (e1', v1) = unbox(Eval, prems(0));
      let$ _ = expect_eq(e1', e1);
      let$ n1 = unbox(NumLit, v1);
      let$ (e2', v2) = unbox(Eval, prems(1));
      let$ _ = expect_eq(e2', e2);
      let$ n2 = unbox(NumLit, v2);
      switch (rule) {
      | E_Plus when n' == n1 + n2 => Ok()
      | E_Minus when n' == n1 - n2 => Ok()
      | E_Times when n' == n1 * n2 => Ok()
      | _ => Error((BinOpArithError(of_cls(op)), rep_ids([v', v1, v2])))
      };

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
      let$ _ = expect_eq_after_extend(ctx_a', ctx, a);
      let$ _ = expect_eq(c', c);
      let$ (ctx_b', c') = unbox(Entail, prems(2));
      let$ _ = expect_eq_after_extend(ctx_b', ctx, b);
      let$ _ = expect_eq(c', c);
      Ok();
    | Implies_I =>
      let$ (ctx, prop) = unbox(Entail, concl);
      let$ (a, b) = unbox(Implies, prop);
      let$ (ctx_a', b') = unbox(Entail, prems(0));
      let$ _ = expect_eq_after_extend(ctx_a', ctx, a);
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
