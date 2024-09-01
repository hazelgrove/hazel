open Util;

/*

 Prop.t is used as the internal representation of a judgement expression. It is
 converted from the evaluation result of an editor. An editor that is requested
 to be evaluated to Prop should have its result be Prop type Constructor.

 */

module Prop = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Hole(string)
    // When DHExp.t not convertable, convert by `e => Hole(DHExp.show(e))`
    | NumLit(int)
    | Val(t)
    | UnOp(t, t)
    | BinOp(t, t, t)
    | OpNeg
    | OpPlus
    | OpMinus
    | OpTimes
    | Eval(t, t)
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
    | NumLit
    | Val
    | UnOp
    | BinOp
    | OpNeg
    | OpPlus
    | OpMinus
    | OpTimes
    | Eval
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

      | Atom(_) => Atom
      | And(_) => And
      | Or(_) => Or
      | Implies(_) => Implies
      | Truth => Truth
      | Falsity => Falsity
      | Ctx(_) => Ctx
      | Entail(_) => Entail

      | NumLit(_) => NumLit
      | Val(_) => Val
      | UnOp(_) => UnOp
      | BinOp(_) => BinOp
      | OpNeg => OpNeg
      | OpPlus => OpPlus
      | OpMinus => OpMinus
      | OpTimes => OpTimes
      | Eval(_) => Eval
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

      | (NumLit(a), NumLit(b)) => Int.equal(a, b)
      | (Val(a), Val(b)) => eq(a, b)
      | (UnOp(a1, a2), UnOp(b1, b2)) => eq(a1, b1) && eq(a2, b2)
      | (BinOp(a1, a2, a3), BinOp(b1, b2, b3)) =>
        eq(a1, b1) && eq(a2, b2) && eq(a3, b3)
      | (OpNeg, OpNeg)
      | (OpPlus, OpPlus)
      | (OpMinus, OpMinus)
      | (OpTimes, OpTimes) => true
      | (Eval(a1, a2), Eval(b1, b2)) => eq(a1, b1) && eq(a2, b2)
      | (NumLit(_), _)
      | (Val(_), _)
      | (UnOp(_), _)
      | (BinOp(_), _)
      | (OpNeg, _)
      | (OpPlus, _)
      | (OpMinus, _)
      | (OpTimes, _)
      | (Eval(_), _) => false

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
    | V_NumLit
    | E_NumLit
    | E_Neg
    | E_Plus
    | E_Minus
    | E_Times
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
    | V_NumLit => "V-NumLit"
    | E_NumLit => "E-NumLit"
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
    | V_NumLit => 0
    | E_NumLit => 0
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
    [V_NumLit, E_NumLit, E_Neg, E_Plus, E_Minus, E_Times]
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
    | PremiseMismatch(int, int) /* expected, actual */
    | UnOpArithError(Prop.cls)
    | BinOpArithError(Prop.cls)
    | FailUnbox(Prop.cls)
    | NotEqual(Prop.cls)
    | NotInCtx
    | FailCtxExtend;

  let rep_ids = List.map(IdTagged.rep_id);

  type unbox_req('a) =
    | NumLit: unbox_req(int)
    | Val: unbox_req(t)
    | UnOp: unbox_req((t, t))
    | BinOp: unbox_req((t, t, t))
    | OpNeg: unbox_req(unit)
    | OpPlus: unbox_req(unit)
    | OpMinus: unbox_req(unit)
    | OpTimes: unbox_req(unit)
    | Eval: unbox_req((t, t))
    | And: unbox_req((t, t))
    | Or: unbox_req((t, t))
    | Implies: unbox_req((t, t))
    | Truth: unbox_req(unit)
    | Falsity: unbox_req(unit)
    | Entail: unbox_req((t, t))
    | Ctx: unbox_req(list(t));

  let cls_of_req: type a. unbox_req(a) => Prop.cls =
    fun
    | NumLit => NumLit
    | Val => Val
    | UnOp => UnOp
    | BinOp => BinOp
    | OpNeg => OpNeg
    | OpPlus => OpPlus
    | OpMinus => OpMinus
    | OpTimes => OpTimes

    | Eval => Eval
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
      | (And, And(a, b)) => Ok((a, b))
      | (Or, Or(a, b)) => Ok((a, b))
      | (Implies, Implies(a, b)) => Ok((a, b))
      | (Truth, Truth) => Ok()
      | (Falsity, Falsity) => Ok()
      | (Entail, Entail(a, b)) => Ok((a, b))
      | (Ctx, Ctx(a)) => Ok(a)

      | (NumLit, NumLit(a)) => Ok(a)
      | (Val, Val(a)) => Ok(a)
      | (UnOp, UnOp(a, b)) => Ok((a, b))
      | (BinOp, BinOp(a, b, c)) => Ok((a, b, c))
      | (OpNeg, OpNeg) => Ok()
      | (OpPlus, OpPlus) => Ok()
      | (OpMinus, OpMinus) => Ok()
      | (OpTimes, OpTimes) => Ok()
      | (Eval, Eval(a, b)) => Ok((a, b))
      | (And, _)
      | (Or, _)
      | (Implies, _)
      | (Truth, _)
      | (Falsity, _)
      | (Entail, _)
      | (Ctx, _)
      | (NumLit, _)
      | (Val, _)
      | (UnOp, _)
      | (BinOp, _)
      | (OpNeg, _)
      | (OpPlus, _)
      | (OpMinus, _)
      | (OpTimes, _)
      | (Eval, _) => Error((FailUnbox(cls_of_req(req)), rep_ids([p])))
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
    | V_NumLit =>
      let$ nl = unbox(Val, concl);
      let$ _ = unbox(NumLit, nl);
      Ok();
    | E_NumLit =>
      let$ (e, v') = unbox(Eval, concl);
      let$ _ = expect_eq(v', e);
      Ok();
    | E_Neg =>
      let$ (e, v') = unbox(Eval, concl);
      let$ (op, e) = unbox(UnOp, e);
      let$ _ = unbox(OpNeg, op);
      let$ (e', v) = unbox(Eval, prems(0));
      let$ _ = expect_eq(e', e);
      let$ _ = expect_unop_arith(OpNeg, v', v);
      Ok();
    | E_Plus =>
      let$ (e, v') = unbox(Eval, concl);
      let$ (op, e1, e2) = unbox(BinOp, e);
      let$ _ = unbox(OpPlus, op);
      let$ (e1', v1) = unbox(Eval, prems(0));
      let$ _ = expect_eq(e1', e1);
      let$ (e2', v2) = unbox(Eval, prems(1));
      let$ _ = expect_eq(e2', e2);
      let$ _ = expect_binop_arith(OpPlus, v', v1, v2);
      Ok();
    | E_Minus =>
      let$ (e, v') = unbox(Eval, concl);
      let$ (op, e1, e2) = unbox(BinOp, e);
      let$ _ = unbox(OpMinus, op);
      let$ (e1', v1) = unbox(Eval, prems(0));
      let$ _ = expect_eq(e1', e1);
      let$ (e2', v2) = unbox(Eval, prems(1));
      let$ _ = expect_eq(e2', e2);
      let$ _ = expect_binop_arith(OpMinus, v', v1, v2);
      Ok();
    | E_Times =>
      let$ (e, v') = unbox(Eval, concl);
      let$ (op, e1, e2) = unbox(BinOp, e);
      let$ _ = unbox(OpTimes, op);
      let$ (e1', v1) = unbox(Eval, prems(0));
      let$ _ = expect_eq(e1', e1);
      let$ (e2', v2) = unbox(Eval, prems(1));
      let$ _ = expect_eq(e2', e2);
      let$ _ = expect_binop_arith(OpTimes, v', v1, v2);
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
      let$ _ = unbox(Truth, prop);
      Ok();
    };
  };
};
