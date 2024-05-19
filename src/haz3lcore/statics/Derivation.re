open Sexplib.Std;
// open Util.OptUtil.Syntax;

exception UnReachable;

module Prop: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Atom(string)
    | And(t, t)
    | Or(t, t)
    | Implies(t, t)
    | Truth
    | Falsity
  and cls =
    | Cls_Atom
    | Cls_And
    | Cls_Or
    | Cls_Implies
    | Cls_Truth
    | Cls_Falsity;
  let precedence: t => int;
  let repr: t => string;
  let cls_repr: cls => string;
  let cls_match: (cls, t) => bool;
  let eq: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Atom(string)
    | And(t, t)
    | Or(t, t)
    | Implies(t, t)
    | Truth
    | Falsity
  and cls =
    | Cls_Atom
    | Cls_And
    | Cls_Or
    | Cls_Implies
    | Cls_Truth
    | Cls_Falsity;

  let precedence = (prop: t): int =>
    switch (prop) {
    | Atom(_)
    | Truth
    | Falsity => Precedence.entail
    | And(_, _) => Precedence.prop_and
    | Or(_, _) => Precedence.prop_or
    | Implies(_, _) => Precedence.prop_implies
    };

  let rec repr_helper = (p, prop) => {
    let precedence = precedence(prop);
    let parathesize = (s: string) =>
      if (p < precedence) {
        Printf.sprintf("(%s)", s);
      } else {
        s;
      };
    let print_binop = (op: string, a: t, b: t) =>
      Printf.sprintf(
        "%s %s %s",
        repr_helper(precedence, a),
        op,
        repr_helper(precedence, b),
      )
      |> parathesize;
    switch (prop) {
    | Atom(s) => s
    | And(a, b) => print_binop("∧", a, b)
    | Or(a, b) => print_binop("∨", a, b)
    | Implies(a, b) =>
      if (b == Falsity) {
        Printf.sprintf("¬%s", repr_helper(precedence, a));
      } else {
        print_binop("⊃", a, b);
      }
    | Truth => "⊤"
    | Falsity => "⊥"
    };
  };
  let repr = repr_helper(Precedence.entail);

  let cls_repr = (v: cls) =>
    switch (v) {
    | Cls_Atom => "Atom"
    | Cls_And => "And(∧)"
    | Cls_Or => "Or(∨)"
    | Cls_Implies => "Implies(⊃)"
    | Cls_Truth => "Truth(⊤)"
    | Cls_Falsity => "Falsity(⊥)"
    };

  let cls_match = (v: cls) =>
    fun
    | Atom(_) => v == Cls_Atom
    | And(_) => v == Cls_And
    | Or(_) => v == Cls_Or
    | Implies(_) => v == Cls_Implies
    | Truth => v == Cls_Truth
    | Falsity => v == Cls_Falsity;

  let eq = (a: t, b: t) => a == b;
};

module Ctx: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(Prop.t);
  let repr: t => string;
  let eq: (t, t) => bool;
  let extend: (t, Prop.t) => t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(Prop.t);

  let repr = ctx =>
    if (List.length(ctx) == 0) {
      "·";
    } else {
      String.concat(", ", List.map(Prop.repr, ctx));
    };
  let eq = (a: t, b: t) => a == b;
  let extend = (ctx, prop) =>
    if (List.mem(prop, ctx)) {
      ctx;
    } else {
      ctx @ [prop];
    };
};

module Judgement: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Entail(Ctx.t, Prop.t);
  type cls =
    | Cls_Entail;
  let repr: t => string;
  let cls_repr: cls => string;
  let cls_match: (cls, t) => bool;
  let eq: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Entail(Ctx.t, Prop.t);
  type cls =
    | Cls_Entail;

  let repr =
    fun
    | Entail(ctx, prop) =>
      Printf.sprintf("%s ⊢ %s", Ctx.repr(ctx), Prop.repr(prop));

  let cls_repr = (v: cls) =>
    switch (v) {
    | Cls_Entail => "Entail(⊢)"
    };
  let cls_match = (v: cls) =>
    fun
    | Entail(_) => v == Cls_Entail;
  let eq = (a: t, b: t) => a == b;
};

module Rule = {
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
    | Assumption => "assumption"
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
};

module Derivation = {
  type t =
    | D(d)
  and d = (Judgement.t, Rule.t, list(t));

  let rec repr_helper = (indent: int, d: t): string => {
    let indent_str = String.make(indent, ' ');
    switch (d) {
    | [@implicit_arity] D(judgement, rule, derivations) =>
      Printf.sprintf(
        "%s\n%s<%s>\n%s%s",
        repr_list(indent + 2, derivations),
        indent_str,
        Rule.repr(rule),
        indent_str,
        Judgement.repr(judgement),
      )
    };
  }
  and repr_list = (indent: int, ds: list(t)): string =>
    String.concat("", List.map(d => repr_helper(indent, d), ds));

  let repr = repr_helper(0);
  let premises = ([@implicit_arity] D(_, _, ds)) =>
    List.map(([@implicit_arity] D(j, _, _)) => j, ds);
};

module MarkedDerivation = {
  type t =
    | Correct(d)
    | Incorrect(d, string)
  and d = (Judgement.t, Rule.t, list(t));

  let rec repr_helper = (indent: int, d: t): string => {
    let indent_str = String.make(indent, ' ');
    switch (d) {
    | [@implicit_arity] Correct(judgement, rule, derivations) =>
      Printf.sprintf(
        "%s\n%s<%s>✅\n%s%s",
        repr_list(indent + 2, derivations),
        indent_str,
        Rule.repr(rule),
        indent_str,
        Judgement.repr(judgement),
      )
    | Incorrect((judgement, rule, derivations), msg) =>
      Printf.sprintf(
        "%s\n%s<%s>❌: %s\n%s%s",
        repr_list(indent + 2, derivations),
        indent_str,
        Rule.repr(rule),
        msg,
        indent_str,
        Judgement.repr(judgement),
      )
    };
  }
  and repr_list = (indent: int, ds: list(t)): string =>
    String.concat("", List.map(d => repr_helper(indent, d), ds));

  let repr = repr_helper(0);

  let rec fold = (f, acc, d) =>
    switch (d) {
    | [@implicit_arity] Correct(_, _, ds) =>
      List.fold_left(fold(f), f(acc, d), ds)
    | Incorrect(_) => f(acc, d)
    };

  let correct =
    fun
    | Correct(_) => true
    | _ => false;
  let all_correct = fold((acc, d) => acc && correct(d), true);
};

module Location = {
  type t =
    | Conclusion
    | Premise(int);

  let repr =
    fun
    | Conclusion => "Conclusion"
    | Premise(i) => Printf.sprintf("Premise %d", i);
};

type bind('a) = {
  location: Location.t,
  value: 'a,
};

module MisMatchCommon = {
  type t =
    | Prop(Prop.cls, bind(Prop.t))
    | Judgement(Judgement.cls, bind(Judgement.t));

  let msg = Printf.sprintf("Expected %s %s, got %s [%s]");

  let repr =
    fun
    | Prop(v, p) =>
      msg(
        "Proposition",
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
  type t =
    | Prop(bind(Prop.t), bind(Prop.t)) /* expected, actual */
    | Ctx(bind(Ctx.t), bind(Ctx.t))
    | Judgement(bind(Judgement.t), bind(Judgement.t));

  let msg = Printf.sprintf("%s %s [%s] not equal to %s [%s]");

  let repr =
    fun
    | Prop(a, b) =>
      msg(
        "Proposition",
        Prop.repr(a.value),
        Location.repr(a.location),
        Prop.repr(b.value),
        Location.repr(b.location),
      )
    | Ctx(a, b) =>
      msg(
        "Context",
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

module VerErr = {
  type t =
    | PremiseMismatch(int, int) /* expected, actual */
    | NotInContext(bind(Prop.t), bind(Ctx.t))
    | MisMatch(MisMatchCommon.t)
    | NotEqual(NotEqualCommon.t)
    | CtxNotEqualAfterExtend(bind(Ctx.t), bind(Ctx.t), bind(Prop.t)); /* expected, actual, prop */

  let repr =
    fun
    | PremiseMismatch(e, a) =>
      Printf.sprintf("Expected %d premises, got %d", e, a)
    | NotInContext(p, c) =>
      Printf.sprintf(
        "Proposition %s [%s] not in context %s [%s]",
        Prop.repr(p.value),
        Location.repr(p.location),
        Ctx.repr(c.value),
        Location.repr(c.location),
      )
    | MisMatch(m) => MisMatchCommon.repr(m)
    | NotEqual(n) => NotEqualCommon.repr(n)
    | CtxNotEqualAfterExtend(a, b, p) =>
      Printf.sprintf(
        "Context %s [%s] not equal to %s [%s] after extending with %s[%s]",
        Ctx.repr(a.value),
        Location.repr(a.location),
        Ctx.repr(b.value),
        Location.repr(b.location),
        Prop.repr(p.value),
        Location.repr(p.location),
      );
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
        switch (j.value) {
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
