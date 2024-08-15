open Util;
open Derivation;

[@deriving (show({with_path: false}), sexp, yojson)]
type pos =
  | Concl
  | Prems(int);

[@deriving (show({with_path: false}), sexp, yojson)]
type bind('a) = {
  pos,
  value: 'a,
};

let copy_pos = ({pos, _}, value) => {pos, value};

let dupl_pos = (p, (a, b)) => (copy_pos(p, a), copy_pos(p, b));

module DerivationError = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | PremiseMismatch(int, int) /* expected, actual */
    | NotInContext(bind(Prop.t), bind(Ctx.t))
    | MisMatch(mismatch)
    | NotEqual(notequal)
    | CtxNotEqualAfterExtend(bind(Ctx.t), bind(Ctx.t), bind(Prop.t)) /* expected, actual, prop */
    | External(string)
  and mismatch =
    | Prop(Prop.cls, bind(Prop.t)) /* expected, actual */
    | Judgement(Judgement.cls, bind(Judgement.t))
  and notequal =
    | Prop(bind(Prop.t), bind(Prop.t)) /* expected, actual */
    | Ctx(bind(Ctx.t), bind(Ctx.t))
    | Judgement(bind(Judgement.t), bind(Judgement.t));

  let repr: t => string =
    fun
    | PremiseMismatch(expect, got) =>
      Printf.sprintf("Expected %d prems, got %d", expect, got)
    | NotInContext(prop, ctx) =>
      Printf.sprintf(
        "Prop %s [%s] not in ctx %s [%s]",
        Prop.repr(prop.value),
        show_pos(prop.pos),
        Ctx.repr(ctx.value),
        show_pos(ctx.pos),
      )
    | MisMatch(m) => {
        let msg = Printf.sprintf("Expected %s %s, got %s [%s]");
        switch (m) {
        | Prop(cls, prop) =>
          msg(
            "Prop",
            Prop.show_cls(cls),
            Prop.repr(prop.value),
            show_pos(prop.pos),
          )
        | Judgement(cls, jdmt) =>
          msg(
            "Judgement",
            Judgement.show_cls(cls),
            Judgement.repr(jdmt.value),
            show_pos(jdmt.pos),
          )
        };
      }
    | NotEqual(n) => {
        let msg = Printf.sprintf("%s %s [%s] not equal to %s [%s]");
        switch (n) {
        | Prop(a, b) =>
          msg(
            "Prop",
            Prop.repr(a.value),
            show_pos(a.pos),
            Prop.repr(b.value),
            show_pos(b.pos),
          )
        | Ctx(a, b) =>
          msg(
            "Ctx",
            Ctx.repr(a.value),
            show_pos(a.pos),
            Ctx.repr(b.value),
            show_pos(b.pos),
          )
        | Judgement(a, b) =>
          msg(
            "Judgement",
            Judgement.repr(a.value),
            show_pos(a.pos),
            Judgement.repr(b.value),
            show_pos(b.pos),
          )
        };
      }
    | CtxNotEqualAfterExtend(extended, original, prop) =>
      Printf.sprintf(
        "Ctx %s [%s] not equal to %s [%s] extended with %s[%s]",
        Ctx.repr(extended.value),
        show_pos(extended.pos),
        Ctx.repr(original.value),
        show_pos(original.pos),
        Prop.repr(prop.value),
        show_pos(prop.pos),
      )
    | External(e) => e;
  let repr = (e: t): string => "Error: " ++ repr(e);
};

module Prop = {
  include Prop;

  type unbox_req('a) =
    | And: unbox_req((bind(t), bind(t)))
    | Or: unbox_req((bind(t), bind(t)))
    | Implies: unbox_req((bind(t), bind(t)))
    | Truth: unbox_req(unit)
    | Falsity: unbox_req(unit);

  let unbox: type a. (unbox_req(a), bind(t)) => result(a, DerivationError.t) =
    (req, p) => {
      let mk_error = (cls: cls): result(a, DerivationError.t) =>
        Error(MisMatch(Prop(cls, p)));
      let dupl_pos = dupl_pos(p);
      switch (req, p.value) {
      | (And, And(a, b)) => Ok((a, b) |> dupl_pos)
      | (Or, Or(a, b)) => Ok((a, b) |> dupl_pos)
      | (Implies, Implies(a, b)) => Ok((a, b) |> dupl_pos)
      | (Truth, Truth) => Ok()
      | (Falsity, Falsity) => Ok()
      | (And, _) => mk_error(And)
      | (Or, _) => mk_error(Or)
      | (Implies, _) => mk_error(Implies)
      | (Truth, _) => mk_error(Truth)
      | (Falsity, _) => mk_error(Falsity)
      };
    };

  let expect_eq = (a: bind(t), b: bind(t)): result(unit, DerivationError.t) =>
    if (eq(a.value, b.value)) {
      Ok();
    } else {
      Error(NotEqual(Prop(a, b)));
    };
};

module Ctx = {
  include Ctx;

  let expect_in_ctx =
      (p: bind(Prop.t), c: bind(t)): result(unit, DerivationError.t) =>
    if (List.mem(p.value, c.value)) {
      Ok();
    } else {
      Error(NotInContext(p, c));
    };

  let expect_eq = (a: bind(t), b: bind(t)): result(unit, DerivationError.t) =>
    if (eq(a.value, b.value)) {
      Ok();
    } else {
      Error(NotEqual(Ctx(a, b)));
    };

  let expect_eq_after_extend =
      (a: bind(t), b: bind(t), p: bind(Prop.t))
      : result(unit, DerivationError.t) =>
    if (eq(extend(a.value, p.value), b.value)) {
      Ok();
    } else {
      Error(CtxNotEqualAfterExtend(a, b, p));
    };
};

module Judgement = {
  include Judgement;

  type unbox_req('a) =
    | Entail: unbox_req((bind(Ctx.t), bind(Prop.t)));

  let unbox: type a. (unbox_req(a), bind(t)) => result(a, DerivationError.t) =
    (req, p) => {
      let dupl_pos = dupl_pos(p);
      switch (req, p.value) {
      | (Entail, Entail(c, p)) => Ok((c, p) |> dupl_pos)
      };
    };

  let expect_eq = (a: bind(t), b: bind(t)): result(unit, DerivationError.t) =>
    if (eq(a.value, b.value)) {
      Ok();
    } else {
      Error(NotEqual(Judgement(a, b)));
    };
};

module Premises = {
  open Judgement;

  let expect_num =
      (n: int, prems: list(t)): result(int => bind(t), DerivationError.t) =>
    if (List.length(prems) == n) {
      Ok(i => {pos: Prems(i), value: List.nth(prems, i)});
    } else {
      Error(PremiseMismatch(n, List.length(prems)));
    };
};

let verify = (rule: Rule.t, concl: Judgement.t, prems: list(Judgement.t)) => {
  let (let$) = (x, f) =>
    switch (x) {
    | Ok(x) => f(x)
    | Error(e) => Error(e)
    };
  let$ concl = Ok({pos: Concl, value: concl});
  let$ prems = Premises.expect_num(Rule.prems_num(rule), prems);
  switch (rule) {
  | Assumption =>
    let$ (ctx, prop) = Judgement.unbox(Entail, concl);
    let$ _ = Ctx.expect_in_ctx(prop, ctx);
    Ok();
  | And_I =>
    let$ (ctx, prop) = Judgement.unbox(Entail, concl);
    let$ (a, b) = Prop.unbox(And, prop);
    let$ (ctx', a') = Judgement.unbox(Entail, prems(0));
    let$ _ = Ctx.expect_eq(ctx, ctx');
    let$ _ = Prop.expect_eq(a, a');
    let$ (ctx', b') = Judgement.unbox(Entail, prems(1));
    let$ _ = Ctx.expect_eq(ctx, ctx');
    let$ _ = Prop.expect_eq(b, b');
    Ok();
  | And_E_L =>
    let$ (ctx, a) = Judgement.unbox(Entail, concl);
    let$ (ctx', prop') = Judgement.unbox(Entail, prems(0));
    let$ (a', _) = Prop.unbox(And, prop');
    let$ _ = Ctx.expect_eq(ctx, ctx');
    let$ _ = Prop.expect_eq(a, a');
    Ok();
  | And_E_R =>
    let$ (ctx, b) = Judgement.unbox(Entail, concl);
    let$ (ctx', prop') = Judgement.unbox(Entail, prems(0));
    let$ (_, b') = Prop.unbox(And, prop');
    let$ _ = Ctx.expect_eq(ctx, ctx');
    let$ _ = Prop.expect_eq(b, b');
    Ok();
  | Or_I_L =>
    let$ (ctx, prop) = Judgement.unbox(Entail, concl);
    let$ (a, _) = Prop.unbox(Or, prop);
    let$ (ctx', a') = Judgement.unbox(Entail, prems(0));
    let$ _ = Ctx.expect_eq(ctx, ctx');
    let$ _ = Prop.expect_eq(a, a');
    Ok();
  | Or_I_R =>
    let$ (ctx, prop) = Judgement.unbox(Entail, concl);
    let$ (_, b) = Prop.unbox(Or, prop);
    let$ (ctx', b') = Judgement.unbox(Entail, prems(0));
    let$ _ = Ctx.expect_eq(ctx, ctx');
    let$ _ = Prop.expect_eq(b, b');
    Ok();
  | Or_E =>
    let$ (ctx, c) = Judgement.unbox(Entail, concl);
    let$ (ctx', prop) = Judgement.unbox(Entail, prems(0));
    let$ (a, b) = Prop.unbox(Or, prop);
    let$ _ = Ctx.expect_eq(ctx, ctx');
    let$ (ctx_a', c') = Judgement.unbox(Entail, prems(1));
    let$ _ = Ctx.expect_eq_after_extend(ctx, ctx_a', a);
    let$ _ = Prop.expect_eq(c, c');
    let$ (ctx_b', c') = Judgement.unbox(Entail, prems(2));
    let$ _ = Ctx.expect_eq_after_extend(ctx, ctx_b', b);
    let$ _ = Prop.expect_eq(c, c');
    Ok();
  | Implies_I =>
    let$ (ctx, prop) = Judgement.unbox(Entail, concl);
    let$ (a, b) = Prop.unbox(Implies, prop);
    let$ (ctx_a', b') = Judgement.unbox(Entail, prems(0));
    let$ _ = Ctx.expect_eq_after_extend(ctx, ctx_a', a);
    let$ _ = Prop.expect_eq(b, b');
    Ok();
  | Implies_E =>
    let$ (ctx, b) = Judgement.unbox(Entail, concl);
    let$ (ctx', prop) = Judgement.unbox(Entail, prems(0));
    let$ (a, b') = Prop.unbox(Implies, prop);
    let$ _ = Ctx.expect_eq(ctx, ctx');
    let$ _ = Prop.expect_eq(b, b');
    let$ (ctx', a') = Judgement.unbox(Entail, prems(1));
    let$ _ = Ctx.expect_eq(ctx, ctx');
    let$ _ = Prop.expect_eq(a, a');
    Ok();
  | Truth_I =>
    let$ (_, prop) = Judgement.unbox(Entail, concl);
    let$ _ = Prop.unbox(Truth, prop);
    Ok();
  | Falsity_E =>
    let$ (ctx, _) = Judgement.unbox(Entail, concl);
    let$ (ctx', prop) = Judgement.unbox(Entail, prems(0));
    let$ _ = Ctx.expect_eq(ctx, ctx');
    let$ _ = Prop.unbox(Truth, prop);
    Ok();
  };
};

include DerivationError;
