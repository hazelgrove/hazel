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
  let rec repr =
    fun
    | Atom(s) => s
    | And(a, b) => Printf.sprintf("%s ∧ %s", repr(a), repr(b))
    | Or(a, b) => Printf.sprintf("%s ∨ %s", repr(a), repr(b))
    | Implies(a, b) =>
      if (b == Falsity) {
        Printf.sprintf("¬%s", repr(a));
      } else {
        Printf.sprintf("%s ⊃ %s", repr(a), repr(b));
      }
    | Truth => "⊤"
    | Falsity => "⊥";
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
