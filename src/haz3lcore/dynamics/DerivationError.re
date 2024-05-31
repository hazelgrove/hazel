open Sexplib.Std;
open Derivation;

module Location: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Conclusion
    | Premise(int);

  let repr: t => string;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Conclusion
    | Premise(int);

  let repr =
    fun
    | Conclusion => "Conclusion"
    | Premise(i) => i |> Printf.sprintf("Premise %d");
};

[@deriving (show({with_path: false}), sexp, yojson)]
type bind('a) = {
  location: Location.t,
  value: 'a,
};

module MisMatchCommon: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Prop(Prop.cls, bind(Prop.t))
    | Judgement(Judgement.cls, bind(Judgement.t));

  let repr: t => string;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Prop(Prop.cls, bind(Prop.t))
    | Judgement(Judgement.cls, bind(Judgement.t));

  let msg = Printf.sprintf("Expected %s %s, got %s [%s]");

  let repr =
    fun
    | Prop(v, p) =>
      msg(
        "Prop",
        Prop.cls_repr(v),
        Prop.repr(p.value),
        Location.repr(p.location),
      )
    | Judgement(v, j) =>
      msg(
        "Judgement",
        Judgement.cls_repr(v),
        Judgement.repr(j.value),
        Location.repr(j.location),
      );
};

module NotEqualCommon = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Prop(bind(Prop.t), bind(Prop.t)) /* expected, actual */
    | Ctx(bind(Ctx.t), bind(Ctx.t))
    | Judgement(bind(Judgement.t), bind(Judgement.t));

  let msg = Printf.sprintf("%s %s [%s] not equal to %s [%s]");

  let repr =
    fun
    | Prop(a, b) =>
      msg(
        "Prop",
        Prop.repr(a.value),
        Location.repr(a.location),
        Prop.repr(b.value),
        Location.repr(b.location),
      )
    | Ctx(a, b) =>
      msg(
        "Ctx",
        Ctx.repr(a.value),
        Location.repr(a.location),
        Ctx.repr(b.value),
        Location.repr(b.location),
      )
    | Judgement(a, b) =>
      msg(
        "Judgement",
        Judgement.repr(a.value),
        Location.repr(a.location),
        Judgement.repr(b.value),
        Location.repr(b.location),
      );
};

module VerErr: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | PremiseMismatch(int, int) /* expected, actual */
    | NotInContext(bind(Prop.t), bind(Ctx.t))
    | MisMatch(MisMatchCommon.t)
    | NotEqual(NotEqualCommon.t)
    | CtxNotEqualAfterExtend(bind(Ctx.t), bind(Ctx.t), bind(Prop.t)) /* expected, actual, prop */
    | External(string);

  let repr: t => string;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | PremiseMismatch(int, int) /* expected, actual */
    | NotInContext(bind(Prop.t), bind(Ctx.t))
    | MisMatch(MisMatchCommon.t)
    | NotEqual(NotEqualCommon.t)
    | CtxNotEqualAfterExtend(bind(Ctx.t), bind(Ctx.t), bind(Prop.t)) /* expected, actual, prop */
    | External(string);

  let repr =
    fun
    | PremiseMismatch(e, a) =>
      Printf.sprintf("Expected %d premises, got %d", e, a)
    | NotInContext(p, c) =>
      Printf.sprintf(
        "Prop %s [%s] not in ctx %s [%s]",
        Prop.repr(p.value),
        Location.repr(p.location),
        Ctx.repr(c.value),
        Location.repr(c.location),
      )
    | MisMatch(m) => MisMatchCommon.repr(m)
    | NotEqual(n) => NotEqualCommon.repr(n)
    | CtxNotEqualAfterExtend(a, b, p) =>
      Printf.sprintf(
        "Ctx %s [%s] not equal to %s [%s] extended with %s[%s]",
        Ctx.repr(a.value),
        Location.repr(a.location),
        Ctx.repr(b.value),
        Location.repr(b.location),
        Prop.repr(p.value),
        Location.repr(p.location),
      )
    | External(e) => e;

  let repr = (e: t): string => "Error: " ++ repr(e);
};

module PropVer = {
  open Prop;

  let expect = (v: cls, p): result(bind(t), VerErr.t) =>
    if (cls_match(v, p.value)) {
      Ok(p);
    } else {
      Error(MisMatch(Prop(v, p)));
    };

  let bind_unit =
    fun
    | Ok(_) => Ok()
    | Error(e) => Error(e);

  let bind_unzip = (res: result(bind(t), VerErr.t)) =>
    switch (res) {
    | Ok(p) =>
      Ok(
        switch (p.value) {
        | And(a, b)
        | Or(a, b)
        | Implies(a, b) => (
            {location: p.location, value: a},
            {location: p.location, value: b},
          )
        | _ => raise(UnReachable)
        },
      )
    | Error(e) => Error(e)
    };

  let expect_Atom = p => expect(Cls_Atom, p);
  let expect_And = p => expect(Cls_And, p) |> bind_unzip;
  let expect_Or = p => expect(Cls_Or, p) |> bind_unzip;
  let expect_Implies = p => expect(Cls_Implies, p) |> bind_unzip;
  let expect_Truth = p => expect(Cls_Truth, p) |> bind_unit;
  let expect_Falsity = p => expect(Cls_Falsity, p) |> bind_unit;

  let expect_eq = (a: bind(t), b: bind(t)): result(unit, VerErr.t) =>
    if (eq(a.value, b.value)) {
      Ok();
    } else {
      Error(NotEqual(Prop(a, b)));
    };
};

module CtxVer = {
  open Ctx;

  let expect_in_ctx = (p: bind(Prop.t), c: bind(t)): result(unit, VerErr.t) =>
    if (List.mem(p.value, c.value)) {
      Ok();
    } else {
      Error(NotInContext(p, c));
    };

  let expect_eq = (a: bind(t), b: bind(t)): result(unit, VerErr.t) =>
    if (eq(a.value, b.value)) {
      Ok();
    } else {
      Error(NotEqual(Ctx(a, b)));
    };

  let expect_eq_after_extend =
      (a: bind(t), b: bind(t), p: bind(Prop.t)): result(unit, VerErr.t) =>
    if (eq(extend(a.value, p.value), b.value)) {
      Ok();
    } else {
      Error(CtxNotEqualAfterExtend(a, b, p));
    };
};

module JudgementVer = {
  open Judgement;

  let expect = (v: cls, j: bind(t)): result(bind(t), VerErr.t) =>
    if (cls_match(v, j.value)) {
      Ok(j);
    } else {
      Error(MisMatch(Judgement(v, j)));
    };

  let bind_unzip = (res: result(bind(t), VerErr.t)) =>
    switch (res) {
    | Ok(j) =>
      Ok(
        switch (just(j.value)) {
        | Entail(c, p) => (
            {location: j.location, value: c},
            {location: j.location, value: p},
          )
        },
      )
    | Error(e) => Error(e)
    };

  let expect_Entail = j => expect(Cls_Entail, j) |> bind_unzip;

  let expect_eq = (a: bind(t), b: bind(t)): result(unit, VerErr.t) =>
    if (eq(a.value, b.value)) {
      Ok();
    } else {
      Error(NotEqual(Judgement(a, b)));
    };
};

module PremiseVer = {
  open Judgement;

  let expect_len =
      (premises: list(t), n: int): result(int => bind(t), VerErr.t) =>
    if (List.length(premises) == n) {
      Ok(i => {location: Premise(i), value: List.nth(premises, i)});
    } else {
      Error(PremiseMismatch(n, List.length(premises)));
    };
};

module RuleVer: {
  let verify:
    (Rule.t, Judgement.t, list(Judgement.t)) => result(unit, VerErr.t);
} = {
  open Rule;
  let (let$) = (x, f) =>
    switch (x) {
    | Ok(x) => f(x)
    | Error(e) => Error(e)
    };

  let verify =
      (rule: t, conclusion: Judgement.t, premises: list(Judgement.t)) => {
    let conclusion = {location: Conclusion, value: conclusion};
    switch (rule) {
    | Assumption =>
      let$ _ = PremiseVer.expect_len(premises, 0);
      let$ (ctx, prop) = JudgementVer.expect_Entail(conclusion);
      let$ () = CtxVer.expect_in_ctx(prop, ctx);
      Ok();
    | And_I =>
      let$ p = PremiseVer.expect_len(premises, 2);
      let$ (ctx, prop) = JudgementVer.expect_Entail(conclusion);
      let$ (a, b) = PropVer.expect_And(prop);
      let$ (ctx', a') = JudgementVer.expect_Entail(p(0));
      let$ () = CtxVer.expect_eq(ctx, ctx');
      let$ () = PropVer.expect_eq(a, a');
      let$ (ctx', b') = JudgementVer.expect_Entail(p(1));
      let$ () = CtxVer.expect_eq(ctx, ctx');
      let$ () = PropVer.expect_eq(b, b');
      Ok();
    | And_E_L =>
      let$ p = PremiseVer.expect_len(premises, 1);
      let$ (ctx, a) = JudgementVer.expect_Entail(conclusion);
      let$ (ctx', prop') = JudgementVer.expect_Entail(p(0));
      let$ (a', _) = PropVer.expect_And(prop');
      let$ () = CtxVer.expect_eq(ctx, ctx');
      let$ () = PropVer.expect_eq(a, a');
      Ok();
    | And_E_R =>
      let$ p = PremiseVer.expect_len(premises, 1);
      let$ (ctx, b) = JudgementVer.expect_Entail(conclusion);
      let$ (ctx', prop') = JudgementVer.expect_Entail(p(0));
      let$ (_, b') = PropVer.expect_And(prop');
      let$ () = CtxVer.expect_eq(ctx, ctx');
      let$ () = PropVer.expect_eq(b, b');
      Ok();
    | Or_I_L =>
      let$ p = PremiseVer.expect_len(premises, 1);
      let$ (ctx, prop) = JudgementVer.expect_Entail(conclusion);
      let$ (a, _) = PropVer.expect_Or(prop);
      let$ (ctx', a') = JudgementVer.expect_Entail(p(0));
      let$ () = CtxVer.expect_eq(ctx, ctx');
      let$ () = PropVer.expect_eq(a, a');
      Ok();
    | Or_I_R =>
      let$ p = PremiseVer.expect_len(premises, 1);
      let$ (ctx, prop) = JudgementVer.expect_Entail(conclusion);
      let$ (_, b) = PropVer.expect_Or(prop);
      let$ (ctx', b') = JudgementVer.expect_Entail(p(0));
      let$ () = CtxVer.expect_eq(ctx, ctx');
      let$ () = PropVer.expect_eq(b, b');
      Ok();
    | Or_E =>
      let$ p = PremiseVer.expect_len(premises, 3);
      let$ (ctx, c) = JudgementVer.expect_Entail(conclusion);
      let$ (ctx', prop) = JudgementVer.expect_Entail(p(0));
      let$ (a, b) = PropVer.expect_Or(prop);
      let$ () = CtxVer.expect_eq(ctx, ctx');
      let$ (ctx_a', c') = JudgementVer.expect_Entail(p(1));
      let$ () = CtxVer.expect_eq_after_extend(ctx, ctx_a', a);
      let$ () = PropVer.expect_eq(c, c');
      let$ (ctx_b', c') = JudgementVer.expect_Entail(p(2));
      let$ () = CtxVer.expect_eq_after_extend(ctx, ctx_b', b);
      let$ () = PropVer.expect_eq(c, c');
      Ok();
    | Implies_I =>
      let$ p = PremiseVer.expect_len(premises, 1);
      let$ (ctx, prop) = JudgementVer.expect_Entail(conclusion);
      let$ (a, b) = PropVer.expect_Implies(prop);
      let$ (ctx_a', b') = JudgementVer.expect_Entail(p(0));
      let$ () = CtxVer.expect_eq_after_extend(ctx, ctx_a', a);
      let$ () = PropVer.expect_eq(b, b');
      Ok();
    | Implies_E =>
      let$ p = PremiseVer.expect_len(premises, 2);
      let$ (ctx, b) = JudgementVer.expect_Entail(conclusion);
      let$ (ctx', prop) = JudgementVer.expect_Entail(p(0));
      let$ (a, b') = PropVer.expect_Implies(prop);
      let$ () = CtxVer.expect_eq(ctx, ctx');
      let$ () = PropVer.expect_eq(b, b');
      let$ (ctx', a') = JudgementVer.expect_Entail(p(1));
      let$ () = CtxVer.expect_eq(ctx, ctx');
      let$ () = PropVer.expect_eq(a, a');
      Ok();
    | Truth_I =>
      let$ _ = PremiseVer.expect_len(premises, 0);
      let$ (_, prop) = JudgementVer.expect_Entail(conclusion);
      let$ () = PropVer.expect_Truth(prop);
      Ok();
    | Falsity_E =>
      let$ p = PremiseVer.expect_len(premises, 1);
      let$ (ctx, _) = JudgementVer.expect_Entail(conclusion);
      let$ (ctx', prop) = JudgementVer.expect_Entail(p(0));
      let$ () = CtxVer.expect_eq(ctx, ctx');
      let$ () = PropVer.expect_Falsity(prop);
      Ok();
    };
  };
};
