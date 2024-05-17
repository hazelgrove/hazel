open Sexplib.Std;

module rec Prop: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Atom(string)
    | And(t, t)
    | Or(t, t)
    | Implies(t, t)
    | Truth
    | Falsity;
  let eq: (t, t) => bool;
  let repr: t => string;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Atom(string)
    | And(t, t)
    | Or(t, t)
    | Implies(t, t)
    | Truth
    | Falsity;
  let eq = (x, y) => x == y;

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
  let repr = ctx => ctx |> List.map(Prop.repr) |> String.concat(", ");
  let eq = (a, b) => a == b;
  let extend = (ctx, prop) =>
    if (ctx |> List.mem(prop)) {
      ctx;
    } else {
      ctx @ [prop];
    };
};

module Judgement: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Entail(Ctx.t, Prop.t);
  let repr: t => string;
  let eq: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Entail(Ctx.t, Prop.t);
  let repr = (Entail(ctx, prop)) =>
    Printf.sprintf("%s ⊢ %s", Ctx.repr(ctx), Prop.repr(prop));
  let eq = (a, b) => a == b;
};
