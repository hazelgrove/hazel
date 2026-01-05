open Util;

module Exp = TermBase.Exp;
module Pat = TermBase.Pat;
module Typ = TermBase.Typ;
module TPat = TermBase.TPat;
module Rul = TermBase.Rul;
module Any = TermBase.Any;

/*
    ________  __  _____   __   ____________  __
   / __/ __ \/ / / / _ | / /  /  _/_  __/\ \/ /
  / _// /_/ / /_/ / __ |/ /___/ /  / /    \  /
 /___/\___\_\____/_/ |_/____/___/ /_/     /_/

                "Equality was a mistake" - Andrew Blinn, 2025

 This module aims to capture the many different versions of equality that we need in Hazel under
 one function with many parameters. It is rare that a simple (==) check will suffice, since two
 expressions with the same content could have different ids. We also often need to do things like:

     - ignore parantheses or other wrapping forms
     - not traverse big environments for performance reasons
     - have the equality check look up free variables in an environment

 At the bottom of this file, you will find two convenience records, syntactic and semantic equality,
 which hopefuly provide sensible defaults for most use cases.
 */

type equality = {
  exp: (Exp.t, Exp.t) => bool,
  pat: (Pat.t, Pat.t) => bool,
  typ: (Typ.t, Typ.t) => bool,
  tpat: (TPat.t, TPat.t) => bool,
  rul: (Rul.t, Rul.t) => bool,
  any: (Any.t, Any.t) => bool,
};

module Alphas = {
  type t = list((string, string));

  let singleton = (x, y) => [(x, y)];
  let empty: t = [];

  type is_equiv =
    | Free
    | Equiv
    | NotEquiv;

  // Note[Matt]: Thomas told me to do this
  let rec are_alpha_equiv: (string, string, t) => is_equiv =
    (x, y, alphas) =>
      switch (alphas) {
      | [] => Free
      | [(a, b), ..._] when a == x => b == y ? Equiv : NotEquiv
      | [(_, b), ..._] when b == y => NotEquiv
      | [(_, _), ...rest] => are_alpha_equiv(x, y, rest)
      };

  let combine: (t, t) => t = (@);
};

type settings = {
  type_alpha: bool, // Alpha equivalence over type variables
  exp_alpha: bool, // Alpha equivalence over expression variables
  ignore_parens: bool,
  ignore_probes: bool,
  ignore_ascriptions: bool,
  ignore_dynamic_errors: bool,
  ignore_function_types: bool,
  ignore_constructor_types: bool,
  ignore_function_names: bool,
  ignore_explicit_unlabelling: bool,
  closures_by_id: bool, /* Currently "false" option is not implemented.
                           compares closures by their IDs to save time
                           traversing through massive closures */
  ignore_filters: bool,
  ignore_unknown_provenance: bool, // Treats all holes as equal, including multiholes, emptyholes, invalid and synswitch
  use_expr_wildcards: option((Environment.t(Exp.t), Exp.t) => bool), // In order to turn this setting on, you must provide a function that decides whether something is a value (i.e. whether it matches $v)
  ignore_fixpoints: bool, // Hideously unsound, used to hide function steps in the stepper
  free_var_handler: option((Alphas.t, string, Exp.t) => bool), // Note[Matt]: to be used in MatchExp
  /* The following two options shouldn't really be `settings' but they're
     packaged with settings because they remain the same throughout a single
      equality check */
  env1: option(Environment.t(Exp.t)), // The environment to look up variables on the left in
  env2: option(Environment.t(Exp.t)) // The environment to look up variables on the right in
};

let equality =
    (
      {
        type_alpha,
        exp_alpha,
        ignore_parens,
        ignore_probes,
        ignore_ascriptions,
        ignore_dynamic_errors,
        ignore_function_types,
        ignore_constructor_types,
        ignore_function_names,
        ignore_explicit_unlabelling,
        closures_by_id,
        ignore_filters,
        ignore_unknown_provenance,
        use_expr_wildcards,
        ignore_fixpoints,
        free_var_handler,
        env1,
        env2,
      }: settings,
    )
    : equality => {
  if (!closures_by_id) {
    failwith("full closure equality has not been implemented yet");
  } else {
    ();
  };

  let rec exp =
          (alphas_exp: Alphas.t, alphas_typ: Alphas.t, e1: Exp.t, e2: Exp.t) => {
    let exp' = exp(alphas_exp, alphas_typ);
    let pat' = pat(alphas_exp, alphas_typ);
    let typ' = typ(alphas_exp, alphas_typ);
    let filter' = filter(alphas_exp, alphas_typ);
    let any' = any(alphas_exp, alphas_typ);
    switch (e1 |> Grammar.Annotated.term_of, e2 |> Grammar.Annotated.term_of) {
    // Wrappers when ignored: unwrap. These cases must come first.
    | (DynamicErrorHole(x, _), _) when ignore_dynamic_errors => exp'(x, e2)
    | (_, DynamicErrorHole(x, _)) when ignore_dynamic_errors => exp'(e1, x)
    | (Parens(x), _) when ignore_parens => exp'(x, e2)
    | (_, Parens(x)) when ignore_parens => exp'(e1, x)
    | (Probe(x, _), _) when ignore_probes => exp'(x, e2)
    | (_, Probe(x, _)) when ignore_probes => exp'(e1, x)
    | (Asc(x, _), _) when ignore_ascriptions => exp'(x, e2)
    | (_, Asc(x, _)) when ignore_ascriptions => exp'(e1, x)
    | (Filter(_, x), _) when ignore_filters => exp'(x, e2)
    | (_, Filter(_, x)) when ignore_filters => exp'(e1, x)
    | (TupLabel({term: ExplicitNonlabel, _}, e1), _)
        when ignore_explicit_unlabelling =>
      exp'(e1, e2)
    | (_, TupLabel({term: ExplicitNonlabel, _}, e2))
        when ignore_explicit_unlabelling =>
      exp'(e1, e2)

    // Expression Wildcards:
    | (Constructor("$v", _), _) when Option.is_some(use_expr_wildcards) =>
      let check_value = Option.get(use_expr_wildcards);
      check_value(Option.value(env2, ~default=Environment.empty), e2);
    | (EmptyHole, _) when Option.is_some(use_expr_wildcards) => true
    | (Constructor("$e", _), _) when Option.is_some(use_expr_wildcards) =>
      true

    /* These variable cases are quite complicated because they account for a lot of concerns.
        * 1. Alpha equivalence :  if either of the variables are bound, we need to check if they are alpha equivalent.
        * 2. Environment lookups:  if either of the variables are free but given in the environment we need to look them up.
        * 3. Free variable handler:  if the variable on the left is free, we give it to the free variable handler
        *    which is used by match_exp to handle matching variables.
     */
    | (Var(x), Var(y)) =>
      OptUtil.Syntax.(
        switch (Alphas.are_alpha_equiv(x, y, alphas_exp)) {
        | Equiv => true
        | NotEquiv => false // At least one of the variables is bound, so ctx won't help.
        | Free =>
          // Both variables are free, so we first check ctxs, and then use the free_var_handler if provided.
          let lookup1 = {
            let* env1 = env1;
            Environment.lookup(env1, x);
          };
          let lookup2 = {
            let* env2 = env2;
            Environment.lookup(env2, y);
          };
          switch (lookup1, lookup2) {
          | (Some(v1), Some(v2)) => exp'(v1, v2)
          | (Some(v1), None) => exp'(v1, e2)
          | (None, Some(v2)) => exp'(e1, v2)
          | (None, None) =>
            switch (free_var_handler) {
            | Some(handler) => handler(alphas_exp, x, e2)
            | None => x == y // If no handler, just check if they are equal.
            }
          };
        }
      )
    | (Var(x), _) =>
      open OptUtil.Syntax;
      let lookup1 = {
        let* env1 = env1;
        Environment.lookup(env1, x);
      };
      switch (lookup1) {
      | Some(v1) => exp'(v1, e2)
      | None =>
        switch (free_var_handler) {
        | Some(handler) => handler(alphas_exp, x, e2)
        | None => false // If no handler, just check if they are equal.
        }
      };
    | (_, Var(y)) =>
      open OptUtil.Syntax;
      let lookup2 = {
        let* env2 = env2;
        Environment.lookup(env2, y);
      };
      switch (lookup2) {
      | Some(v2) => exp'(e1, v2)
      | None => false
      };

    // Wrappers otherwise: compare.
    | (DynamicErrorHole(x, err1), DynamicErrorHole(y, err2)) =>
      err1 == err2 && exp'(x, y)
    | (DynamicErrorHole(_), _) => false
    | (Parens(x), Parens(y)) => exp'(x, y)
    | (Parens(_), _) => false
    | (Probe(x, tag1), Probe(y, tag2)) => tag1 == tag2 && exp'(x, y)
    | (Probe(_), _) => false
    | (Asc(x, t1), Asc(y, t2)) => typ'(t1, t2) && exp'(x, y)
    | (Asc(_), _) => false
    | (Filter(f1, x), Filter(f2, y)) => filter'(f1, f2) && exp'(x, y)
    | (Filter(_), _) => false

    // Forms with expression binders
    | (FixF(p1, e1, c1), FixF(p2, e2, c2)) =>
      switch (pat'(p1, p2)) {
      | Some(alphas_exp') =>
        exp(Alphas.combine(alphas_exp', alphas_exp), alphas_typ, e1, e2)
        && (
          closures_by_id
            ? Option.equal(Environment.id_equal, c1, c2)
            : Option.equal(
                failwith(
                  "full closure equality has not been implemented yet",
                ),
                c1,
                c2,
              )
        )
      | None => false
      }
    | (FixF(_, e, _), _) when ignore_fixpoints => exp'(e, e2)
    | (_, FixF(_, e, _)) when ignore_fixpoints => exp'(e1, e)
    | (FixF(_, _, _), _) => false
    | (Fun(p1, e1, t1, f1), Fun(p2, e2, t2, f2)) =>
      switch (pat'(p1, p2)) {
      | Some(alphas_exp') =>
        exp(Alphas.combine(alphas_exp', alphas_exp), alphas_typ, e1, e2)
        && (ignore_function_types || Option.equal(typ', t1, t2))
        && (ignore_function_names || f1 == f2)
      | None => false
      }
    | (Fun(_), _) => false
    | (Let(p1, e1, e2), Let(p2, e3, e4)) =>
      switch (pat'(p1, p2)) {
      | Some(alphas_exp') =>
        exp(alphas_exp, alphas_typ, e1, e3)
        && exp(Alphas.combine(alphas_exp', alphas_exp), alphas_typ, e2, e4)
      | None => false
      }
    | (Let(_, _, _), _) => false
    | (Theorem(p1, e1, e2), Theorem(p2, e3, e4)) =>
      switch (pat'(p1, p2)) {
      | Some(alphas_exp') =>
        exp(alphas_exp, alphas_typ, e1, e3)
        && exp(Alphas.combine(alphas_exp', alphas_exp), alphas_typ, e2, e4)
      | None => false
      }
    | (Theorem(_, _, _), _) => false

    // Forms with type binders
    | (TypFun(tp1, e1, _), TypFun(tp2, e2, _)) =>
      switch (tpat(tp1, tp2)) {
      | Some(alphas_typ') =>
        exp(alphas_exp, Alphas.combine(alphas_typ', alphas_typ), e1, e2)
      | None => false
      }
    | (TypFun(_, _, _), _) => false
    | (TyAlias(tp1, t1, e1), TyAlias(tp2, t2, e2)) =>
      switch (tpat(tp1, tp2)) {
      | Some(alphas_typ') =>
        typ'(t1, t2)
        && exp(alphas_exp, Alphas.combine(alphas_typ', alphas_typ), e1, e2)
      | None => false
      }
    | (TyAlias(_, _, _), _) => false
    | (Forall(p1, e1), Forall(p2, e2)) =>
      switch (pat'(p1, p2)) {
      | Some(alphas_exp') =>
        exp(Alphas.combine(alphas_exp', alphas_exp), alphas_typ, e1, e2)
      | None => false
      }
    | (Forall(_, _), _) => false

    // Forms with environments. (Note fix also has an environment and is handled above.)
    | (Closure(env1, e1), Closure(env2, e2)) when closures_by_id =>
      Environment.id_equal(env1, env2) && exp'(e1, e2)
    | (Closure(_, _), Closure(_, _)) =>
      failwith("full closure equality has not been implemented yet")
    | (Closure(_), _) => false

    // Constructors: might ignore constructor types.
    | (Constructor(c1, _), Constructor(c2, _))
        when ignore_constructor_types == true =>
      c1 == c2
    | (Constructor(c1, Some(Some(ty1))), Constructor(c2, Some(Some(ty2)))) =>
      c1 == c2 && typ'(ty1, ty2)
    | (Constructor(c1, Some(None)), Constructor(c2, Some(None)))
    | (Constructor(c1, None), Constructor(c2, None)) => c1 == c2
    | (Constructor(_), _) => false

    // Holes: equal if provenance is ignored
    | (
        EmptyHole | MultiHole(_) | Invalid(_),
        EmptyHole | MultiHole(_) | Invalid(_),
      )
        when ignore_unknown_provenance =>
      true
    | (EmptyHole, EmptyHole) => true
    | (EmptyHole, _) => false
    | (MultiHole(xs1), MultiHole(xs2))
        when List.length(xs1) == List.length(xs2) =>
      List.equal(any', xs1, xs2)
    | (MultiHole(_), _) => false
    | (Invalid(s1), Invalid(s2)) => s1 == s2
    | (Invalid(_), _) => false

    // Other forms: compare.
    | (Undefined, Undefined) => true
    | (Undefined, _) => false
    | (Deferral(pos1), Deferral(pos2)) => pos1 == pos2
    | (Deferral(_), _) => false
    | (Atom(c1), Atom(c2)) => c1 == c2
    | (Atom(_), _) => false
    | (Label(l1), Label(l2)) => l1 == l2
    | (Label(_), _) => false
    | (LivelitName(s1), LivelitName(s2)) => s1 == s2
    | (LivelitName(_), _) => false
    | (Tuple(xs1), Tuple(xs2)) when List.length(xs1) == List.length(xs2) =>
      List.equal(exp', xs1, xs2)
    | (Tuple(_), _) => false
    | (ListLit(xs1), ListLit(xs2)) when List.length(xs1) == List.length(xs2) =>
      List.equal(exp', xs1, xs2)
    | (ListLit(_), _) => false
    | (Use(t1, e1), Use(t2, e2)) => typ'(t1, t2) && exp'(e1, e2)
    | (Use(_, _), _) => false
    | (Ap(d1, e11, e12), Ap(d2, e21, e22)) =>
      d1 == d2 && exp'(e11, e21) && exp'(e12, e22)
    | (Ap(_, _, _), _) => false
    | (TypAp(e1, t1), TypAp(e2, t2)) => exp'(e1, e2) && typ'(t1, t2)
    | (TypAp(_, _), _) => false
    | (DeferredAp(e1, es1), DeferredAp(e2, es2))
        when List.length(es1) == List.length(es2) =>
      exp'(e1, e2) && List.equal(exp', es1, es2)
    | (DeferredAp(_, _), _) => false
    | (If(e11, e12, e13), If(e21, e22, e23)) =>
      exp'(e11, e21) && exp'(e12, e22) && exp'(e13, e23)
    | (If(_, _, _), _) => false
    | (Seq(e1, e2), Seq(e3, e4)) => exp'(e1, e3) && exp'(e2, e4)
    | (Seq(_, _), _) => false
    | (Test(e1), Test(e2)) => exp'(e1, e2)
    | (Test(_), _) => false
    | (HintedTest(e1, e2), HintedTest(e3, e4)) =>
      exp'(e1, e3) && exp'(e2, e4)
    | (HintedTest(_, _), _) => false
    | (TupLabel(label1, d1'), TupLabel(label2, d2')) =>
      exp'(label1, label2) && exp'(d1', d2')
    | (TupLabel(_, _), _) => false
    | (ExplicitNonlabel, ExplicitNonlabel) => true
    | (ExplicitNonlabel, _) => false
    | (Dot(e11, e12), Dot(e21, e22)) => exp'(e11, e21) && exp'(e12, e22)
    | (Dot(_, _), _) => false
    | (TupleExtension(e1, e2), TupleExtension(e1', e2')) =>
      exp'(e1, e1') && exp'(e2, e2')
    | (TupleExtension(_), _) => false
    | (UnOp(op1, e1), UnOp(op2, e2)) => op1 == op2 && exp'(e1, e2)
    | (UnOp(_, _), _) => false
    | (BinOp(op1, e11, e12), BinOp(op2, e21, e22)) =>
      op1 == op2 && exp'(e11, e21) && exp'(e12, e22)
    | (BinOp(_, _, _), _) => false
    | (BuiltinFun(f1), BuiltinFun(f2)) => f1 == f2
    | (BuiltinFun(_), _) => false
    | (Match(e1, rs1), Match(e2, rs2))
        when List.length(rs1) == List.length(rs2) =>
      let rec match_rules = (rs1, rs2) =>
        switch (rs1, rs2) {
        | ([], []) => true
        | ([(p1, e1), ...rest1], [(p2, e2), ...rest2]) =>
          switch (pat'(p1, p2)) {
          | Some(alphas_exp') =>
            exp(Alphas.combine(alphas_exp', alphas_exp), alphas_typ, e1, e2)
            && match_rules(rest1, rest2)
          | None => false
          }
        | _ => false
        };
      exp'(e1, e2) && match_rules(rs1, rs2);
    | (Match(_, _), _) => false
    | (Cons(e11, e12), Cons(e21, e22)) => exp'(e11, e21) && exp'(e12, e22)
    | (Cons(_, _), _) => false
    | (ListConcat(e11, e12), ListConcat(e21, e22)) =>
      exp'(e11, e21) && exp'(e12, e22)
    | (ListConcat(_, _), _) => false
    | (ProofObject(e1), ProofObject(e2)) => exp'(e1, e2)
    | (ProofObject(_), _) => false
    };
  }
  and pat =
      (alphas_exp: Alphas.t, alphas_typ: Alphas.t, p1: Pat.t, p2: Pat.t)
      : option(Alphas.t) => {
    let pat' = pat(alphas_exp, alphas_typ);
    let any' = any(alphas_exp, alphas_typ);
    switch (p1 |> Grammar.Annotated.term_of, p2 |> Grammar.Annotated.term_of) {
    // Wrappers when ignored: unwrap.
    | (Probe(x, _), _) when ignore_probes => pat'(x, p2)
    | (_, Probe(x, _)) when ignore_probes => pat'(p1, x)
    | (Parens(x), _) when ignore_parens => pat'(x, p2)
    | (_, Parens(x)) when ignore_parens => pat'(p1, x)
    | (Asc(x, _), _) when ignore_ascriptions => pat'(x, p2)
    | (_, Asc(x, _)) when ignore_ascriptions => pat'(p1, x)

    // Wrappers otherwise: compare.
    | (Probe(x, tag1), Probe(y, tag2)) when tag1 == tag2 => pat'(x, y)
    | (Probe(_), _) => None
    | (Parens(x), Parens(y)) => pat'(x, y)
    | (Parens(_), _) => None
    | (Asc(x, _), Asc(y, _)) => pat'(x, y)
    | (Asc(_), _) => None

    // Variables: special case depending on alpha equivalence.
    | (Var(x), Var(y)) when exp_alpha => Some(Alphas.singleton(x, y))
    | (Var(x), Var(y)) when x == y => Some(Alphas.singleton(x, x))
    | (Var(_), _) => None

    // Constructors: might ignore constructor types.
    | (Constructor(c1, _), Constructor(c2, _))
        when ignore_constructor_types == true && c1 == c2 =>
      Some(Alphas.empty)
    | (Constructor(c1, Some(Some(ty1))), Constructor(c2, Some(Some(ty2))))
        when c1 == c2 && typ(alphas_exp, alphas_typ, ty1, ty2) =>
      Some(Alphas.empty)
    | (Constructor(c1, Some(None)), Constructor(c2, Some(None)))
    | (Constructor(c1, None), Constructor(c2, None)) when c1 == c2 =>
      Some(Alphas.empty)
    | (Constructor(_, _), _) => None

    // Holes: equal if provenance is ignored
    | (
        EmptyHole | MultiHole(_) | Invalid(_),
        EmptyHole | MultiHole(_) | Invalid(_),
      )
        when ignore_unknown_provenance =>
      Some(Alphas.empty)
    | (EmptyHole, EmptyHole) => Some(Alphas.empty)
    | (EmptyHole, _) => None
    | (Invalid(s1), Invalid(s2)) when s1 == s2 => Some(Alphas.empty)
    | (Invalid(_), _) => None
    | (MultiHole(xs1), MultiHole(xs2))
        when
          List.length(xs1) == List.length(xs2) && List.equal(any', xs1, xs2) =>
      Some(Alphas.empty)
    | (MultiHole(_), _) => None

    // Other forms
    | (Wild, Wild) => Some(Alphas.empty)
    | (Wild, _) => None
    | (Atom(c1), Atom(c2)) when c1 == c2 => Some(Alphas.empty)
    | (Atom(_), _) => None
    | (Label(l1), Label(l2)) when l1 == l2 => Some(Alphas.empty)
    | (Label(_), _) => None
    | (Tuple(xs1), Tuple(xs2)) when List.length(xs1) == List.length(xs2) =>
      ListUtil.fold_left_opt(
        (alphas, (x, y)) =>
          pat'(x, y) |> Option.map(Alphas.combine(_, alphas)),
        Alphas.empty,
        List.combine(xs1, xs2),
      )
    | (Tuple(_), _) => None
    | (ListLit(xs1), ListLit(xs2)) when List.length(xs1) == List.length(xs2) =>
      ListUtil.fold_left_opt(
        (alphas, (x, y)) =>
          pat'(x, y) |> Option.map(Alphas.combine(_, alphas)),
        Alphas.empty,
        List.combine(xs1, xs2),
      )
    | (ListLit(_), _) => None
    | (Cons(p1, p2), Cons(p3, p4)) =>
      open OptUtil.Syntax;
      let* alphas1 = pat'(p1, p3);
      let* alphas2 = pat'(p2, p4);
      Some(Alphas.combine(alphas1, alphas2));
    | (Cons(_, _), _) => None
    | (TupLabel(label1, d1'), TupLabel(label2, d2')) =>
      open OptUtil.Syntax;
      let* alphas1 = pat'(label1, label2);
      let* alphas2 = pat'(d1', d2');
      Some(Alphas.combine(alphas1, alphas2));
    | (TupLabel(_, _), _) => None
    | (ExplicitNonlabel, ExplicitNonlabel) => Some(Alphas.empty)
    | (ExplicitNonlabel, _) => None
    | (Ap(p1, p2), Ap(p3, p4)) =>
      open OptUtil.Syntax;
      let* alphas1 = pat'(p1, p3);
      let* alphas2 = pat'(p2, p4);
      Some(Alphas.combine(alphas1, alphas2));
    | (Ap(_, _), _) => None
    };
  }
  and typ =
      (alphas_exp: Alphas.t, alphas_typ: Alphas.t, t1: Typ.t, t2: Typ.t): bool => {
    // This function takes alphas_exp for the theorem keyword branches which have expressions in types.
    let exp' = exp(alphas_exp, alphas_typ);
    let typ' = typ(alphas_exp, alphas_typ);
    let typ_prov' = typ_prov(alphas_exp, alphas_typ);
    let tpat' = tpat;
    switch (t1 |> Grammar.Annotated.term_of, t2 |> Grammar.Annotated.term_of) {
    // Wrappers when ignored: unwrap.
    | (Parens(x), _) when ignore_parens => typ'(x, t2)
    | (_, Parens(x)) when ignore_parens => typ'(t1, x)
    | (TupLabel({term: ExplicitNonlabel, _}, t1), _)
        when ignore_explicit_unlabelling =>
      typ'(t1, t2)
    | (_, TupLabel({term: ExplicitNonlabel, _}, t2))
        when ignore_explicit_unlabelling =>
      typ'(t1, t2)

    // Wrappers otherwise: compare.
    | (Parens(x), Parens(y)) => typ'(x, y)
    | (Parens(_), _) => false

    // Forms with type binders
    | (Rec(tp1, t1), Rec(tp2, t2)) =>
      switch (tpat'(tp1, tp2)) {
      | Some(alphas_typ') =>
        typ(alphas_exp, Alphas.combine(alphas_typ', alphas_typ), t1, t2)
      | None => false
      }
    | (Rec(_, _), _) => false
    | (Poly(tp1, t1), Poly(tp2, t2)) =>
      switch (tpat'(tp1, tp2)) {
      | Some(alphas_typ') =>
        typ(alphas_exp, Alphas.combine(alphas_typ', alphas_typ), t1, t2)
      | None => false
      }
    | (Poly(_, _), _) => false

    // Type variables: special case depending on alpha equivalence.
    | (Var(x), Var(y)) =>
      switch (Alphas.are_alpha_equiv(x, y, alphas_typ)) {
      | Equiv => true
      | Free => x == y
      | NotEquiv => false
      }
    | (Var(_), _) => false

    // Holes: equal if provenance is ignored
    | (Unknown(_), Unknown(_)) when ignore_unknown_provenance => true
    | (Unknown({term: p1, _}), Unknown({term: p2, _})) => typ_prov'(p1, p2)
    | (Unknown(_), _) => false
    // Other forms: compare.
    | (Atom(a1), Atom(a2)) => a1 == a2
    | (Atom(_), _) => false
    | (Label(l1), Label(l2)) => l1 == l2
    | (Label(_), _) => false
    | (List(ty1), List(ty2)) => typ'(ty1, ty2)
    | (List(_), _) => false
    | (Prod(tys1), Prod(tys2)) when List.length(tys1) == List.length(tys2) =>
      List.equal(typ', tys1, tys2)
    | (Prod(_), _) => false
    | (Arrow(t11, t12), Arrow(t21, t22)) =>
      typ'(t11, t21) && typ'(t12, t22)
    | (Arrow(_, _), _) => false
    | (Sum(variants1), Sum(variants2)) =>
      /* Does not normalize the types. */
      ConstructorMap.equal(typ', variants1, variants2)
    | (Sum(_), _) => false
    | (TupLabel(label1, t1'), TupLabel(label2, t2')) =>
      typ'(label1, label2) && typ'(t1', t2')
    | (TupLabel(_, _), _) => false
    | (ExplicitNonlabel, ExplicitNonlabel) => true
    | (ExplicitNonlabel, _) => false
    | (ProdProjection(t1, t2), ProdProjection(t1', t2')) =>
      typ'(t1, t1') && typ'(t2, t2')
    | (ProdProjection(_), _) => false
    | (ProdExtension(t1, t2), ProdExtension(t1', t2')) =>
      typ'(t1, t1') && typ'(t2, t2')
    | (ProdExtension(_), _) => false
    | (ProofOf(e1), ProofOf(e2)) => exp'(e1, e2)
    | (ProofOf(_), _) => false
    };
  }
  and typ_prov =
      (
        alphas_exp: Alphas.t,
        alphas_typ: Alphas.t,
        p1: Prov.term,
        p2: Prov.term,
      )
      : bool => {
    let typ_prov' = typ_prov(alphas_exp, alphas_typ);
    let any' = any(alphas_exp, alphas_typ);
    switch (p1, p2) {
    | (SynSwitch, SynSwitch) => true
    | (SynSwitch, _) => false
    | (Hole(Invalid(s1)), Hole(Invalid(s2))) => s1 == s2
    | (Hole(Invalid(_)), _) => false
    | (Hole(EmptyHole), Hole(EmptyHole)) => true
    | (Hole(EmptyHole), _) => false
    | (Hole(CycleHole), Hole(CycleHole)) => true
    | (Hole(CycleHole), _) => false
    | (Hole(MultiHole(xs1)), Hole(MultiHole(xs2)))
        when List.length(xs1) == List.length(xs2) =>
      List.equal(any', xs1, xs2)
    | (Hole(MultiHole(_)), _) => false
    | (Internal, Internal) => true
    | (Internal, _) => false
    | (TupLabelArg(p1'), TupLabelArg(p2'))
    | (TupLabel(p1'), TupLabel(p2'))
    | (RForall(p1'), RForall(p2'))
    | (MList(p1'), MList(p2'))
    | (RArrow(p1'), LArrow(p2'))
    | (LArrow(p1'), LArrow(p2')) => typ_prov'(p1', p2')
    | (NProduct(n1, p1'), NProduct(n2, p2')) when n1 == n2 =>
      typ_prov'(p1', p2')
    | (Meet(m1, m2), Meet(m3, m4)) =>
      typ_prov'(m1 |> Prov.term_of, m3 |> Prov.term_of)
      && typ_prov'(m2 |> Prov.term_of, m4 |> Prov.term_of)
      || typ_prov'(m1 |> Prov.term_of, m4 |> Prov.term_of)
      && typ_prov'(m2 |> Prov.term_of, m3 |> Prov.term_of)
    | (Meet(_), _)
    | (TupLabelArg(_), _)
    | (TupLabel(_), _)
    | (RForall(_), _)
    | (MList(_), _)
    | (NProduct(_), _)
    | (LArrow(_), _)
    | (RArrow(_), _) => false
    };
  }
  and tpat = (tp1: TPat.t, tp2: TPat.t): option(Alphas.t) => {
    switch (
      tp1 |> Grammar.Annotated.term_of,
      tp2 |> Grammar.Annotated.term_of,
    ) {
    // Variables: special case depending on alpha equivalence.
    | (Var(x), Var(y)) when type_alpha => Some(Alphas.singleton(x, y))
    | (Var(x), Var(y)) when x == y => Some(Alphas.singleton(x, x))
    | (Var(_), _) => None

    // Holes: equal if provenance is ignored
    | (
        EmptyHole | MultiHole(_) | Invalid(_),
        EmptyHole | MultiHole(_) | Invalid(_),
      )
        when ignore_unknown_provenance =>
      Some(Alphas.empty)
    | (EmptyHole, EmptyHole) => Some(Alphas.empty)
    | (EmptyHole, _) => None
    | (Invalid(s1), Invalid(s2)) when s1 == s2 => Some(Alphas.empty)
    | (Invalid(_), _) => None
    | (MultiHole(xs1), MultiHole(xs2))
        when
          List.length(xs1) == List.length(xs2)
          && List.equal((_, _) => true, xs1, xs2) =>
      Some(Alphas.empty)
    | (MultiHole(_), _) => None
    };
  }
  and rul =
      (alphas_exp: Alphas.t, alphas_typ: Alphas.t, r1: Rul.t, r2: Rul.t): bool => {
    let pat' = pat(alphas_exp, alphas_typ);
    let exp' = exp(alphas_exp, alphas_typ);
    switch (r1 |> Grammar.Annotated.term_of, r2 |> Grammar.Annotated.term_of) {
    | (Rules(e1, rls1), Rules(e2, rls2))
        when List.length(rls1) == List.length(rls2) =>
      exp'(e1, e2)
      && List.for_all2(
           ((p1, e1), (p2, e2)) =>
             switch (pat'(p1, p2)) {
             | Some(alphas_exp') =>
               exp(
                 Alphas.combine(alphas_exp', alphas_exp),
                 alphas_typ,
                 e1,
                 e2,
               )
             | None => false
             },
           rls1,
           rls2,
         )
    | (Rules(_, _), _) => false

    // Holes: equal if provenance is ignored
    | (MultiHole(_) | Invalid(_), MultiHole(_) | Invalid(_))
        when ignore_unknown_provenance =>
      true
    | (Invalid(s1), Invalid(s2)) => s1 == s2
    | (Invalid(_), _) => false
    | (MultiHole(xs1), MultiHole(xs2))
        when
          List.length(xs1) == List.length(xs2)
          && List.equal((_, _) => true, xs1, xs2) =>
      true
    | (MultiHole(_), _) => false
    };
  }
  and filter =
      (
        alphas_exp: Alphas.t,
        alphas_typ: Alphas.t,
        f1: TermBase.StepperFilterKind.t,
        f2: TermBase.StepperFilterKind.t,
      )
      : bool => {
    let exp' = exp(alphas_exp, alphas_typ);
    switch (f1, f2) {
    | (Filter({pat: pat1, act: act1}), Filter({pat: pat2, act: act2})) =>
      exp'(pat1, pat2) && act1 == act2
    | (Filter(_), _) => false
    | (Residue(_), Residue(_)) => f1 == f2
    | (Residue(_), _) => false
    };
  }
  and any =
      (alphas_exp: Alphas.t, alphas_typ: Alphas.t, a1: Any.t, a2: Any.t): bool => {
    switch (a1, a2) {
    | (Exp(e1), Exp(e2)) => exp(alphas_exp, alphas_typ, e1, e2)
    | (Exp(_), _) => false
    | (Pat(p1), Pat(p2)) =>
      pat(alphas_exp, alphas_typ, p1, p2) |> Option.is_some
    | (Pat(_), _) => false
    | (Typ(t1), Typ(t2)) => typ(alphas_exp, alphas_typ, t1, t2)
    | (Typ(_), _) => false
    | (Rul(r1), Rul(r2)) => rul(alphas_exp, alphas_typ, r1, r2)
    | (Rul(_), _) => false
    | (TPat(tp1), TPat(tp2)) => tpat(tp1, tp2) |> Option.is_some
    | (TPat(_), _) => false
    | (Any (), Any ()) => true
    | (Any (), _) => false
    };
  };

  {
    exp: exp(Alphas.empty, Alphas.empty),
    pat: (p1, p2) =>
      pat(Alphas.empty, Alphas.empty, p1, p2) |> Option.is_some,
    typ: typ(Alphas.empty, Alphas.empty),
    tpat: (tp1, tp2) => tpat(tp1, tp2) |> Option.is_some,
    rul: rul(Alphas.empty, Alphas.empty),
    any: any(Alphas.empty, Alphas.empty),
  };
};

// Useful defaults

let syntactic_settings = {
  type_alpha: false,
  exp_alpha: false,
  ignore_parens: false,
  ignore_dynamic_errors: false,
  ignore_probes: false,
  ignore_ascriptions: false,
  ignore_function_types: false,
  ignore_constructor_types: false,
  ignore_function_names: false,
  ignore_explicit_unlabelling: false,
  closures_by_id: true,
  ignore_filters: false,
  ignore_unknown_provenance: false,
  use_expr_wildcards: None,
  ignore_fixpoints: false,
  free_var_handler: None,
  env1: None,
  env2: None,
};

let syntactic = equality(syntactic_settings);

let semantic_settings = {
  type_alpha: true,
  exp_alpha: true,
  ignore_parens: true,
  ignore_dynamic_errors: false,
  ignore_probes: true,
  ignore_ascriptions: false,
  ignore_function_types: false,
  ignore_constructor_types: false,
  ignore_function_names: true,
  ignore_explicit_unlabelling: true,
  closures_by_id: true, // Ideally substitute all closures before using semantic equality
  ignore_filters: true,
  ignore_unknown_provenance: true,
  use_expr_wildcards: None,
  ignore_fixpoints: false,
  free_var_handler: None,
  env1: None,
  env2: None,
};

let semantic = equality(semantic_settings);
