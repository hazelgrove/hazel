open Util;

/* A theorem statement read as a rewrite rule: `forall x where g ->
 * A ==> l == r` is the rule `l |-> r` (and back) over the binder `x`,
 * with the assumptions `g` and `A`. */

[@deriving (show({with_path: false}), sexp, yojson)]
type conclusion =
  | Equality(Exp.t, Exp.t)
  /* A boolean conclusion `P`, read as the equation `P == true` (see
   * `BoolReading`). Not collapsed into `Equality(P, true)` because
   * display states the reading rather than show an equation nobody
   * wrote, and because the reverse (`true` |-> `P`) direction is gated
   * separately (`Rewrite.matches`'s `~reverse`). `exp_to_rule` never
   * produces this; only the reading does. */
  | BoolFact(Exp.t)
  | Other(Exp.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  bindings: list(Ctx.entry),
  /* Conditions on the rule, in source order: the `where` guards of
   * restricted binders followed by the peeled top-level `==>` antecedents
   * (`A ==> B ==> concl` contributes [A, B]). A rule with assumptions is
   * a conditional rewrite rule; applying it emits each assumption,
   * instantiated by the match, as an obligation. */
  assumptions: list(Exp.t),
  conclusion,
};

/* Peel only the outer binders (forall / forall-where) off a theorem
 * statement, collecting the binders' ctx entries and any `where` guards.
 * Returns the remaining core, which may retain `==>` antecedents. */
let rec peel_binders = (exp: Exp.t): (list(Ctx.entry), list(Exp.t), Exp.t) =>
  switch (exp |> Exp.term_of) {
  /* A `Parens` from an editor splice must not hide a binder. */
  | Parens(e) => peel_binders(e)
  | Forall(p, e) =>
    let bindings' =
      SemanticCtx.bind_pattern(p, Typ.temp(Unknown(Internal)), Ctx.empty)
      |> Option.map((x: Ctx.t) => x.entries)
      |> OptUtil.get(() => []);
    let (bindings, guards, core) = peel_binders(e);
    /* `forall p where g -> e` is `Forall(Where(p, g), e)`. */
    let guards =
      switch (Exp.pat_guard(p)) {
      | None => guards
      | Some(g) => [g, ...guards]
      };
    (bindings' @ bindings, guards, core);
  | _ => ([], [], exp)
  };

/* The rule a statement states, read as written: the boolean reading is
 * applied only where a rule is used (`BoolReading`). */
let exp_to_rule = (exp: Exp.t): t => {
  /* `A ==> B ==> concl` yields ([A, B], concl). */
  let rec peel_implications = (exp: Exp.t): (list(Exp.t), Exp.t) =>
    switch (exp |> Exp.term_of) {
    | BinOp(Bool(Implies), e1, e2) =>
      let (assumptions, core) = peel_implications(e2);
      ([e1, ...assumptions], core);
    | _ => ([], exp)
    };
  let (bindings, guards, core) = peel_binders(exp);
  let (antecedents, conclusion) = peel_implications(core);
  {
    bindings,
    assumptions: guards @ antecedents,
    conclusion:
      switch (conclusion |> Exp.term_of) {
      | BinOp(Poly(Equals), e1, e2) => Equality(e1, e2)
      | _ => Other(conclusion)
      },
  };
};

/* The rule's conclusion as written: a `BoolFact` is the bare boolean,
 * not the equation it is read as. */
let conclusion_exp = (rule: t): Exp.t =>
  switch (rule.conclusion) {
  | Equality(e1, e2) => Exp.fresh(BinOp(Poly(Equals), e1, e2))
  | BoolFact(e)
  | Other(e) => e
  };

// Instantiating a binder

/* Eliminate the quantifier over `x` in a proposition by substituting
 * `e` for it, the semantics of `revert <fact> with x = e`:
 *
 *   `forall x -> B`            ↦  `B[x := e]`
 *   `forall x where g -> B`    ↦  `g[x := e] ==> B[x := e]`
 *
 * Dropping the `where` restriction would be unsound, so it survives as
 * an antecedent. Binders quantified outside `x` stay in place. `None`
 * when no binder in the prefix is named `x`. */
let rec instantiate_binder = (x: Var.t, e: Exp.t, prop: Exp.t): option(Exp.t) => {
  let subst = (body: Exp.t) =>
    Substitution.in_exp(Environment.of_bindings([(x, e)]), body);
  switch (prop |> Exp.term_of) {
  | Parens(inner) => instantiate_binder(x, e, inner)
  | Forall(pat, body) =>
    let (base, guard) = Exp.split_pat_guard(pat);
    switch (ProofSyntax.pat_to_name(base)) {
    | Some(actual) when actual == x =>
      switch (guard) {
      | None => Some(subst(body))
      | Some(g) =>
        Some(Exp.fresh(BinOp(Bool(Implies), subst(g), subst(body))))
      }
    | Some(_)
    | None =>
      instantiate_binder(x, e, body)
      |> Option.map(b => Exp.fresh(Forall(pat, b)))
    };
  | _ => None
  };
};

/* The boolean reading.
 *
 * A rule that holds says its conclusion denotes `true`, so a boolean
 * conclusion `P` (a comparison, a connective, a `where` guard, a
 * Bool-typed application) can be read as the equation `P == true`, which
 * makes it usable as a rewrite rule. So `... ==> (a * b != 0)` needs no
 * trailing `== true`.
 *
 * The reading is applied where a rule is used (`ProofCheck`'s axiom
 * step, the stepper's rule display and activity filter), never inside
 * `exp_to_rule`: goal classification and `conclusion_exp` must see the
 * proposition as written. */
module BoolReading = {
  /* A syntactic answer to "is this expression boolean-valued?". `Unclear`
   * is the answer for a variable, an application, a `case` or an `if`,
   * whose type only the statics knows. */
  type shape =
    | Boolean
    | NotBoolean
    | Unclear;

  let rec shape = (e: Exp.t): shape => {
    /* Spelled out rather than read off `Operators.semantics_of_bin_op` so
     * that a new operator is a non-exhaustive-match warning here. */
    let bin_op_is_bool = (op: Operators.op_bin): bool =>
      switch (op) {
      | Bool(And | Or | Implies)
      | Poly(Equals | NotEquals) => true
      | Int(op)
      | SInt(op)
      | Nat(op) =>
        switch (op) {
        | LessThan
        | LessThanOrEqual
        | GreaterThan
        | GreaterThanOrEqual => true
        | Plus
        | Minus
        | Times
        | Power
        | Divide => false
        }
      | Float(op) =>
        switch (op) {
        | LessThan
        | LessThanOrEqual
        | GreaterThan
        | GreaterThanOrEqual
        | Equals
        | NotEquals => true
        | Plus
        | Minus
        | Times
        | Power
        | Divide => false
        }
      | String(Concat) => false
      };
    let un_op_is_bool = (op: Operators.op_un): bool =>
      switch (op) {
      | Bool(Not) => true
      | Int(Minus)
      | Nat(Minus)
      | SInt(Minus)
      | Float(Minus) => false
      };
    switch (e |> Exp.term_of) {
    /* Transparent wrappers. `exp_to_rule` does not peel these, so a
     * parenthesised equation still classifies `Other`; peeling here means
     * `... ==> (P)` reads the same as `... ==> P`. */
    | Parens(e)
    | Projector(_, e)
    | Asc(e, _) => shape(e)
    | Atom(Bool(_)) => Boolean
    | Atom(Int(_) | Nat(_) | SInt(_) | Float(_) | String(_)) => NotBoolean
    | UnOp(op, _) => un_op_is_bool(op) ? Boolean : NotBoolean
    | BinOp(op, _, _) => bin_op_is_bool(op) ? Boolean : NotBoolean
    | ListLit(_)
    | Tuple(_)
    | Cons(_, _)
    | ListConcat(_, _)
    | Fun(_, _, _, _)
    | TypFun(_, _, _) => NotBoolean
    | _ => Unclear
    };
  };

  /* `Other(P)` becomes `BoolFact(P)` when `P` is a boolean proposition.
   * `~info_map` is type evidence for an `Unclear` shape; without it, as
   * in the obligations panel, the reading is purely syntactic. Anything
   * it can't settle stays `Other`, which can't rewrite. */
  let apply = (~info_map: option(Statics.Map.t)=None, rule: t): t => {
    let is_bool_prop = (e: Exp.t): bool =>
      switch (shape(e), info_map) {
      | (Boolean, _) => true
      | (NotBoolean, _)
      | (Unclear, None) => false
      | (Unclear, Some(info_map)) =>
        switch (
          Option.map(
            Typ.term_of,
            Statics.Map.ty_of(Exp.rep_id(e), info_map),
          )
        ) {
        | Some(Atom(Bool)) => true
        | Some(_)
        | None => false
        }
      };
    switch (rule.conclusion) {
    | Equality(_, _)
    | BoolFact(_) => rule
    | Other(e) =>
      is_bool_prop(e)
        ? {
          ...rule,
          conclusion: BoolFact(e),
        }
        : rule
    };
  };

  /* The equation a `BoolFact` is read as, for display. */
  let equation = (rule: t): option(Exp.t) =>
    switch (rule.conclusion) {
    | BoolFact(e) =>
      Some(
        Exp.fresh(BinOp(Poly(Equals), e, Exp.fresh(Atom(Bool(true))))),
      )
    | Equality(_, _)
    | Other(_) => None
    };
};

/* Rewriting with a rule: matching one side of its conclusion against an
 * expression (MatchExp) and instantiating the other side. */
module Rewrite = {
  /* The match context with every binder of the rule unassigned. */
  let empty_bindings = (bindings: list(Ctx.entry)): MatchExp.match_ctx =>
    List.filter_map(
      (entry: Ctx.entry) =>
        switch (entry) {
        | VarEntry({name, typ, _}) => Some((name, (typ, None)))
        | ConstructorEntry(_)
        | TVarEntry(_)
        | LivelitEntry(_)
        | TheoremEntry(_) => None
        },
      bindings,
    );

  /* Pre-assign one of the rule's binders in a match context, for a
   * `with <var> = <exp>` clause. `None` when `name` is not a binder of the
   * rule, which the caller reports as `UnknownInstantiationVar`. */
  let seed =
      (name: Var.t, exp: Exp.t, bindings: MatchExp.match_ctx)
      : option(MatchExp.match_ctx) =>
    MatchExp.match_ctx_has(bindings, name)
      ? Some(
          List.map(
            ((n, (t, e))) =>
              n == name ? (n, (t, Some(exp))) : (n, (t, e)),
            bindings,
          ),
        )
      : None;

  /* A rewrite from a match that left a binder unassigned would bring the
   * rule's own variable into the goal free. */
  let has_unassigned = (mctx: MatchExp.match_ctx): bool =>
    List.exists(((_, (_, assigned))) => assigned == None, mctx);

  /* The rule's rewrites of `exp` in each direction, (Left, Right), i.e.
   * (`rewrite <-`, `rewrite`), each with the match context that produced it,
   * so callers can instantiate the rule's assumptions and detect
   * unassigned binders. `~bindings` overrides the initial (all
   * unassigned) match context, which is how a `with` clause is seeded. */
  let matches =
      (~info_map, ~env, ~bindings=?, ~reverse=false, rule: t, exp: Exp.t)
      : (
          option((Exp.t, MatchExp.match_ctx)),
          option((Exp.t, MatchExp.match_ctx)),
        ) => {
    let bindings =
      switch (bindings) {
      | Some(b) => b
      | None => empty_bindings(rule.bindings)
      };
    let via = (from, to_) =>
      MatchExp.match_exp(~info_map, ~defs=env, ~exp_r_ctx=bindings, from, exp)
      |> Option.map(mctx => (MatchExp.substitute_exp(mctx, to_), mctx));
    switch (rule.conclusion) {
    /* Right matches the LHS and rewrites to the RHS. */
    | Equality(a, b) => (via(b, a), via(a, b))
    /* Read as `P == true`. Forward (`rewrite`) rewrites `P` to `true`.
     *
     * Reverse (`rewrite <-`) rewrites a `true` in the goal to `P`. That is
     * sound, but `true` occurs everywhere, so offering it during rule
     * discovery would make every boolean rule "active" at every `true`.
     * Only an explicit citation, which names the rule, the direction and
     * the occurrence, turns it on (`~reverse`). */
    | BoolFact(p) => (
        reverse ? via(Exp.fresh(Atom(Bool(true))), p) : None,
        via(p, Exp.fresh(Atom(Bool(true)))),
      )
    | Other(_) => (None, None)
    };
  };

  /* The rewrites the checker accepts: those whose match assigned every
   * binder. */
  let apply =
      (~info_map, ~env, ~reverse=false, rule: t, exp: Exp.t)
      : (option(Exp.t), option(Exp.t)) => {
    let (l, r) = matches(~info_map, ~env, ~reverse, rule, exp);
    let determined = side =>
      switch (side) {
      | Some((w, mctx)) when !has_unassigned(mctx) => Some(w)
      | Some(_)
      | None => None
      };
    (determined(l), determined(r));
  };

  let is_active = (~info_map, ~env, rule: t, exp: Exp.t): bool =>
    switch (apply(~info_map, ~env, rule, exp)) {
    | (Some(_), _)
    | (_, Some(_)) => true
    | (None, None) => false
    };
};

// Free variables

/* Does any name in `vs` occur free in `e`, given `bound` already in scope?
 *
 * Binding structure follows `Substitution.in_exp`, except that, as in
 * the statics, `Closure(env, body)` looks only at `body`: expanding `env`
 * would inline the whole environment, the blow-up this walk exists to
 * avoid.
 *
 * Non-binding forms fall through to `map_term`'s generic traversal,
 * which also walks expressions nested inside patterns (`Asc`) under the
 * enclosing scope, as `Substitution.in_pat` does. */
let occurs_free_any =
    (~bound: list(Var.t)=[], vs: list(Var.t), e: Exp.t): bool => {
  let found = ref(false);
  let rec f_exp = (bound: list(Var.t), cont, e: Exp.t): Exp.t => {
    if (! found^) {
      let under = (bound', es) => List.iter(go(bound'), es);
      switch (e |> Exp.term_of) {
      | Var(x) =>
        if (List.mem(x, vs) && !List.mem(x, bound)) {
          found := true;
        }
      | Fun(p, body, t, _) =>
        Option.iter(go_typ(bound), t);
        go_pat(bound, p);
        under(Pat.bound_vars(p) @ bound, [body]);
      | FixF(p, body, _) =>
        go_pat(bound, p);
        under(Pat.bound_vars(p) @ bound, [body]);
      | Let(p, def, body) =>
        go_pat(bound, p);
        under(Pat.bound_vars(p) @ bound, [def, body]);
      | Theorem(p, stmt, _, body) =>
        go_pat(bound, p);
        under(Pat.bound_vars(p) @ bound, [stmt, body]);
      | Forall(p, body) =>
        go_pat(bound, p);
        /* A `where` guard on the binder pattern is an Exp under the
           binder, like the body. */
        under(
          Pat.bound_vars(p) @ bound,
          Option.to_list(Exp.pat_guard(p)) @ [body],
        );
      | Match(scrut, cases) =>
        go(bound, scrut);
        List.iter(
          ((p, body)) => {
            go_pat(bound, p);
            under(Pat.bound_vars(p) @ bound, [body]);
          },
          cases,
        );
      | _ => ignore(cont(e))
      };
    };
    e;
  }
  and go = (bound, e) => ignore(Exp.map_term(~f_exp=f_exp(bound), e))
  and go_pat = (bound, p) => ignore(Pat.map_term(~f_exp=f_exp(bound), p))
  and go_typ = (bound, t) => ignore(Typ.map_term(~f_exp=f_exp(bound), t));
  go(bound, e);
  found^;
};

/* Does the rule's core proposition (its assumptions as a `==>` chain
 * onto its conclusion) mention any of `vs` free, modulo the rule's own
 * binders? A free-variable walk, since it runs on every fact in scope at
 * every binder. */
let mentions_any = (rule: t, vs: list(Var.t)): bool => {
  let core =
    List.fold_right(
      (a, acc) => Exp.fresh(BinOp(Bool(Implies), a, acc)),
      rule.assumptions,
      conclusion_exp(rule),
    );
  let binders =
    List.filter_map(
      (entry: Ctx.entry) =>
        switch (entry) {
        | VarEntry({name, _}) => Some(name)
        | ConstructorEntry(_)
        | TVarEntry(_)
        | LivelitEntry(_)
        | TheoremEntry(_) => None
        },
      rule.bindings,
    );
  occurs_free_any(~bound=binders, vs, core);
};
