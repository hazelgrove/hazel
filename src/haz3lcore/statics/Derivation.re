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
    | Entail(Ctx.t, Prop.t);
  [@deriving (show({with_path: false}), sexp, yojson)]
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
  [@deriving (show({with_path: false}), sexp, yojson)]
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

module Derivation: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | D(d)
  and d = (Judgement.t, Rule.t, list(t));

  let repr: t => string;
  let premises: t => list(Judgement.t);
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
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
    ds |> List.map(d => repr_helper(indent, d)) |> String.concat("");

  let repr = repr_helper(0);
  let premises = ([@implicit_arity] D(_, _, ds)) =>
    ds |> List.map(([@implicit_arity] D(j, _, _)) => j);
};

module MarkedDerivation: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Correct(d)
    | Incorrect(d, string)
  and d = (Judgement.t, Rule.t, list(t));

  let repr: t => string;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
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
    ds |> List.map(d => repr_helper(indent, d)) |> String.concat("");

  let repr = repr_helper(0);
  // let rec fold = (f, acc, d) =>
  //   switch (d) {
  //   | [@implicit_arity] Correct(_, _, ds) =>
  //     List.fold_left(fold(f), f(acc, d), ds)
  //   | Incorrect(_) => f(acc, d)
  //   };
  // let correct =
  //   fun
  //   | Correct(_) => true
  //   | Incorrect(_) => false;
  // let all_correct = fold((acc, d) => acc && correct(d), true);
};
