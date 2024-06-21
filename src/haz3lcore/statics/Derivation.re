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
        s |> Printf.sprintf("(%s)");
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
  let repr = repr_helper(0);

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
      ctx |> List.map(Prop.repr) |> String.concat(", ");
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
    | Verified(j)
    | Wrong(j)
    | Partial(j)
  and j =
    | Entail(Ctx.t, Prop.t);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Cls_Entail;
  let just: t => j;
  let repr: t => string;
  let cls_repr: cls => string;
  let cls_match: (cls, t) => bool;
  let eq: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Verified(j)
    | Wrong(j)
    | Partial(j)
  and j =
    | Entail(Ctx.t, Prop.t);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Cls_Entail;
  let just = (v: t) =>
    switch (v) {
    | Verified(j) => j
    | Wrong(j) => j
    | Partial(j) => j
    };
  let repr = (v: t) => {
    let symbol =
      switch (v) {
      | Verified(_) => "✅"
      | Wrong(_) => "❌"
      | Partial(_) => "⚠️"
      };
    v
    |> just
    |> (
      fun
      | Entail(ctx, prop) =>
        Printf.sprintf(
          "%s %s ⊢ %s",
          symbol,
          Ctx.repr(ctx),
          Prop.repr(prop),
        )
    );
  };
  let cls_repr =
    fun
    | Cls_Entail => "Entail(⊢)";
  let cls_match = (c: cls, v: t) =>
    v
    |> just
    |> (
      fun
      | Entail(_, _) => c == Cls_Entail
    );
  let eq = (a: t, b: t) => a == b;
};

module Rule: {
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
  let repr: t => string;
  let prem_num: t => int;
  let for_each: (t => 'a) => list('a);
  let of_string: string => t;
} = {
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

  let of_string = (s: string) =>
    switch (s) {
    | "Assumption" => Assumption
    | "And_I" => And_I
    | "And_E_L" => And_E_L
    | "And_E_R" => And_E_R
    | "Or_I_L" => Or_I_L
    | "Or_I_R" => Or_I_R
    | "Or_E" => Or_E
    | "Implies_I" => Implies_I
    | "Implies_E" => Implies_E
    | "Truth_I" => Truth_I
    | "Falsity_E" => Falsity_E
    | _ => failwith("invalid rule")
    };

  let prem_num =
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

  let for_each = f =>
    [
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
    ]
    |> List.map(f);
};
