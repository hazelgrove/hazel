open Util;

/*

 Prop.t is used as the internal representation of a judgement expression. It is
 converted from the evaluation result of an editor. An editor that is requested
 to be evaluated to Prop should have its result be Prop type Constructor.

 */

module Prop = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type failure =
    | FailUnbox(int) // Cannot unbox {0}
    | NotEqual(int) // {0} does not equal to {1}
    | NotInCtx(int) // {0} does not in {1}
    | FailCtxExtend(int); // {0} does not equal to {1} extended by {2}

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Atom(string)
    | And(t, t)
    | Or(t, t)
    | Implies(t, t)
    | Truth
    | Falsity
    | Ctx(list(t))
    | Entail(t, t)
    // When DHExp.t not convertable, convert by `e => Hole(DHExp.show(e))`
    | Hole(string)
    // When verifying the deduction and find an locatable error.
    | Fail(t, failure)

  and cls =
    | Atom
    | And
    | Or
    | Implies
    | Truth
    | Falsity
    | Ctx
    | Entail;

  let of_cls: t => cls =
    fun
    | Atom(_) => Atom
    | And(_) => And
    | Or(_) => Or
    | Implies(_) => Implies
    | Truth => Truth
    | Falsity => Falsity
    | Ctx(_) => Ctx
    | Entail(_) => Entail
    | Hole(_) => failwith("No cls for Hole")
    | Fail(_) => failwith("No cls for Fail");

  let repr: t => string = {
    let precedence: t => int =
      fun
      | Atom(_)
      | Truth
      | Falsity
      | Ctx(_)
      | Hole(_)
      | Fail(_)
      | Entail(_) => Precedence.entail
      | And(_) => Precedence.prop_and
      | Or(_) => Precedence.prop_or
      | Implies(_) => Precedence.prop_implies;
    let rec aux = (p: int, prop: t): string => {
      let p' = precedence(prop);
      let aux = aux(p');
      let print_binop = (op: string, a: t, b: t) =>
        Printf.sprintf("%s %s %s", aux(a), op, aux(b));
      let print_unop = (op: string, a: t) =>
        Printf.sprintf("%s%s", op, aux(a));
      (
        switch (prop) {
        | Atom(s) => s
        | And(a, b) => print_binop("∧", a, b)
        | Or(a, b) => print_binop("∨", a, b)
        | Implies(a, Falsity) => print_unop("¬", a)
        | Implies(a, b) => print_binop("⊃", a, b)
        | Truth => "⊤"
        | Falsity => "⊥"
        | Ctx(ctx) =>
          if (List.length(ctx) == 0) {
            "·";
          } else {
            ctx
            |> List.map(aux)
            |> String.concat(", ")
            |> Printf.sprintf("[%s]");
          }
        | Entail(a, b) => print_binop("⊢", a, b)
        | Hole(s) => s
        // TODO(zhiyao): make it colorful
        | Fail(_) => ""
        }
      )
      |> (p < p' ? Printf.sprintf("(%s)") : Fun.id);
    };
    aux(0);
  };

  let eq: (t, t) => bool =
    (a, b) =>
      switch (a, b) {
      | (Atom(_), _)
      | (And(_), _)
      | (Or(_), _)
      | (Implies(_), _)
      | (Truth, _)
      | (Falsity, _)
      | (Ctx(_), _)
      | (Entail(_), _) => a == b
      | (Hole(_), _) => false
      | (Fail(_), _) => failwith("Prop.eq: impossible")
      };

  // This function is used when we find an failure, we will wrap DProp.t back
  // to a Prop.t, but we will have multiple sources of located failure. So we
  // use this function to merge them to a whole Prop.t.
  //
  // We should expect for each argument that, except Fail(_), anything else
  // should be consistent.
  let rec merge: (t, t) => t =
    (a, b) =>
      switch (a, b) {
      | (Fail(a, f), b) => Fail(merge(a, b), f)
      | (a, Fail(b, f)) => Fail(merge(a, b), f)
      | (Atom(_), _) => a
      | (And(a1, a2), And(b1, b2)) => And(merge(a1, b1), merge(a2, b2))
      | (Or(a1, a2), Or(b1, b2)) => Or(merge(a1, b1), merge(a2, b2))
      | (Implies(a1, a2), Implies(b1, b2)) =>
        Implies(merge(a1, b1), merge(a2, b2))
      | (Truth, _) => Truth
      | (Falsity, _) => Falsity
      | (Ctx(la), Ctx(lb)) => Ctx(List.map2(merge, la, lb))
      | (Entail(a1, a2), Entail(b1, b2)) =>
        Entail(merge(a1, b1), merge(a2, b2))
      | (And(_), _)
      | (Or(_), _)
      | (Implies(_), _)
      | (Ctx(_), _)
      | (Entail(_), _)
      | (Hole(_), _) => failwith("DProp.merge: impossible")
      // Hole(_) will always be wrapped with Fail(_)
      };

  let extend_ctx = (ctx: list(t), prop: t) =>
    if (List.mem(prop, ctx)) {
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

  let all = [
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

module Deduction = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    prems: list(Prop.t),
    concl: Prop.t,
  };

  let merge: (t, t) => t =
    (d1, d2) => {
      prems: List.map2(Prop.merge, d1.prems, d2.prems),
      concl: Prop.merge(d1.concl, d2.concl),
    };

  let merge: (t, option(t)) => option(t) =
    (d1, d2) =>
      switch (d2) {
      | Some(d2) => Some(merge(d1, d2))
      | None => Some(d1)
      };
};

module DeductionVerified = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    deduction: Deduction.t,
    failure: option(failure),
  }
  and failure =
    | PremiseMismatch(int, int) /* expected, actual */
    | FailUnbox(Prop.cls)
    | NotEqual(Prop.cls)
    | NotInCtx
    | FailCtxExtend;

  let to_prop_failure: (int, failure) => Prop.failure =
    i =>
      fun
      | PremiseMismatch(_) => failwith("impossible")
      | FailUnbox(_) => FailUnbox(i)
      | NotEqual(_) => NotEqual(i)
      | NotInCtx => NotInCtx(i)
      | FailCtxExtend => FailCtxExtend(i);
};

module DProp = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    now: Prop.t,
    ctr: Prop.t => Deduction.t,
  };

  let mk: Deduction.t => (list(t), t) =
    ({prems, concl}) => (
      List.mapi(
        (i, p) =>
          {now: p, ctr: x => {concl, prems: ListUtil.put_nth(i, x, prems)}},
        prems,
      ),
      {now: concl, ctr: x => {prems, concl: x}},
    );

  let mk_error: (list(t), DeductionVerified.failure) => DeductionVerified.t =
    (dpl, failure) => {
      let deduction =
        dpl
        |> List.mapi((i, {now, ctr}) => {
             let failure = DeductionVerified.to_prop_failure(i, failure);
             ctr(Fail(now, failure));
           })
        |> List.fold_left((acc, p) => Deduction.merge(p, acc), None)
        |> OptUtil.get_or_raise(Invalid_argument("DProp.merge"));
      let failure = Some(failure);
      {deduction, failure};
    };

  // FailUnbox
  type unbox_req('a) =
    | And: unbox_req((t, t))
    | Or: unbox_req((t, t))
    | Implies: unbox_req((t, t))
    | Truth: unbox_req(unit)
    | Falsity: unbox_req(unit)
    | Entail: unbox_req((t, t))
    | Ctx: unbox_req(list(t));

  let of_cls: type a. unbox_req(a) => Prop.cls =
    fun
    | And => And
    | Or => Or
    | Implies => Implies
    | Truth => Truth
    | Falsity => Falsity
    | Entail => Entail
    | Ctx => Ctx;

  let unbox: type a. (unbox_req(a), t) => result(a, DeductionVerified.t) =
    (req, {now, ctr} as dp) => {
      switch (req, now) {
      | (And, And(a, b)) =>
        Ok((
          {now: a, ctr: x => And(x, b) |> ctr},
          {now: b, ctr: x => And(a, x) |> ctr},
        ))
      | (Or, Or(a, b)) =>
        Ok((
          {now: a, ctr: x => Or(x, b) |> ctr},
          {now: b, ctr: x => Or(a, x) |> ctr},
        ))
      | (Implies, Implies(a, b)) =>
        Ok((
          {now: a, ctr: x => Implies(x, b) |> ctr},
          {now: b, ctr: x => Implies(a, x) |> ctr},
        ))
      | (Truth, Truth) => Ok()
      | (Falsity, Falsity) => Ok()
      | (Entail, Entail(a, b)) =>
        Ok((
          {now: a, ctr: x => Entail(x, b) |> ctr},
          {now: b, ctr: x => Entail(a, x) |> ctr},
        ))
      | (Ctx, Ctx(ctx)) =>
        Ok(
          List.mapi(
            (i, x) =>
              {now: x, ctr: x => Ctx(ListUtil.put_nth(i, x, ctx)) |> ctr},
            ctx,
          ),
        )
      | (And, _)
      | (Or, _)
      | (Implies, _)
      | (Truth, _)
      | (Falsity, _)
      | (Entail, _)
      | (Ctx, _) => Error(mk_error([dp], FailUnbox(of_cls(req))))
      };
    };

  let (let$) = (x, f) =>
    switch (x) {
    | Ok(x) => f(x)
    | Error(e) => Error(e)
    };

  // NotEqual
  let expect_eq: (t, t) => result(unit, DeductionVerified.t) =
    (a, b) =>
      if (Prop.eq(a.now, b.now)) {
        Ok();
      } else {
        Error(mk_error([a, b], NotEqual(Prop.of_cls(a.now))));
      };

  // NotInCtx
  let expect_in_ctx: (t, t) => result(unit, DeductionVerified.t) =
    (p, ctx) => {
      let$ pl = unbox(Ctx, ctx);
      if (List.mem(p.now, List.map(x => x.now, pl))) {
        Ok();
      } else {
        Error(mk_error([p, ctx], NotInCtx));
      };
    };

  // FailCtxExtend
  let expect_eq_after_extend: (t, t, t) => result(unit, DeductionVerified.t) =
    (ctx_a, ctx, a) => {
      let$ pl_a = unbox(Ctx, ctx_a);
      let$ pl = unbox(Ctx, ctx);
      let ctx_a_expected = Prop.extend_ctx(List.map(p => p.now, pl), a.now);
      if (List.for_all2(Prop.eq, ctx_a_expected, List.map(p => p.now, pl_a))) {
        Ok();
      } else {
        Error(mk_error([ctx_a, ctx, a], FailCtxExtend));
      };
    };

  let verify =
      (rule: Rule.t, prems: int => t, concl: t)
      : result(unit, DeductionVerified.t) => {
    switch (rule) {
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

  let verify = (rule: Rule.t, deduction: Deduction.t): DeductionVerified.t => {
    let (prems, concl) = mk(deduction);
    let got = List.length(prems);
    let expect = Rule.prems_num(rule);
    if (expect != got) {
      {deduction, failure: Some(PremiseMismatch(expect, got))};
    } else {
      switch (verify(rule, i => List.nth(prems, i), concl)) {
      | Ok () => {deduction, failure: None}
      | Error(res) => res
      };
    };
  };
};
