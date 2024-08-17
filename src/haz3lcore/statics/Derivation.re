open Util;

exception UnReachable;

module Prop = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Atom(string)
    | And(t, t)
    | Or(t, t)
    | Implies(t, t)
    | Truth
    | Falsity
  and cls =
    | Atom
    | And
    | Or
    | Implies
    | Truth
    | Falsity;

  let repr: t => string = {
    let precedence: t => int =
      fun
      | Atom(_)
      | Truth
      | Falsity => Precedence.entail
      | And(_, _) => Precedence.prop_and
      | Or(_, _) => Precedence.prop_or
      | Implies(_, _) => Precedence.prop_implies;
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
        }
      )
      |> (p < p' ? Printf.sprintf("(%s)") : Fun.id);
    };
    aux(0);
  };

  let eq: (t, t) => bool = (a, b) => a == b;
};

module Ctx = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(Prop.t);

  let repr = ctx =>
    if (List.length(ctx) == 0) {
      "·";
    } else {
      ctx
      |> List.map(Prop.repr)
      |> String.concat(", ")
      |> Printf.sprintf("[%s]");
    };

  let eq: (t, t) => bool = (a, b) => a == b;

  let extend = (ctx, prop) =>
    if (List.mem(prop, ctx)) {
      ctx;
    } else {
      let rec insert = (ctx, prop) =>
        switch (ctx) {
        | [] => [prop]
        | [hd, ...tl] =>
          if (Prop.show(hd) <= Prop.show(prop)) {
            [prop, ...ctx];
          } else {
            [hd, ...insert(tl, prop)];
          }
        };
      insert(ctx, prop);
    };
};

module Judgement = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Entail(Ctx.t, Prop.t);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Entail;

  let repr: t => string =
    fun
    | Entail(ctx, prop) =>
      Printf.sprintf("%s ⊢ %s", Ctx.repr(ctx), Prop.repr(prop));

  let eq: (t, t) => bool = (a, b) => a == b;
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
