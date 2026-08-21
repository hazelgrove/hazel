open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type conclusion =
  | Equality(Exp.t, Exp.t)
  /* A BARE-BOOLEAN conclusion `P`, read as the equation `P == true`
   * (see `with_bool_fact_reading`). Kept as its own variant rather than
   * collapsed into `Equality(P, true)` for two reasons: display must be
   * able to state the reading explicitly instead of silently showing an
   * equation nobody wrote, and the reverse (`true` |-> `P`) direction is
   * gated separately from the forward one (`can_eq_inst`'s `~reverse`).
   * `classify` never produces this; only the reading does. */
  | BoolFact(Exp.t)
  | Other(Exp.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  bindings: list(Ctx.entry),
  /* Conditions on the rule, in source order: the `where` guards of
   * restricted binders followed by the peeled top-level `==>` antecedents
   * (`A ==> B ==> concl` contributes [A, B]). A rule with assumptions is
   * a conditional rewrite rule; applying it emits each assumption,
   * instantiated by the match, as an obligation
   * (docs/prover-obligations.md §2.1, §4.1). */
  assumptions: list(Exp.t),
  conclusion,
};

/* Peel only the outer binders (forall / forall-where) off a theorem
 * statement, collecting the binders' ctx entries and any `where` guards.
 * Returns the remaining core, which may retain `==>` antecedents. */
let rec peel_binders = (exp: Exp.t): (list(Ctx.entry), list(Exp.t), Exp.t) =>
  switch (exp |> Exp.term_of) {
  | Forall(p, e) =>
    let bindings' =
      ProofHacks.dhpat_extend_ctx(p, Typ.temp(Unknown(Internal)), Ctx.empty)
      |> Option.map((x: Ctx.t) => x.entries)
      |> OptUtil.get(() => []);
    let (bindings, guards, core) = peel_binders(e);
    (bindings' @ bindings, guards, core);
  | ForallWhere(p, g, e) =>
    let bindings' =
      ProofHacks.dhpat_extend_ctx(p, Typ.temp(Unknown(Internal)), Ctx.empty)
      |> Option.map((x: Ctx.t) => x.entries)
      |> OptUtil.get(() => []);
    let (bindings, guards, core) = peel_binders(e);
    (bindings' @ bindings, [g, ...guards], core);
  | _ => ([], [], exp)
  };

/* Binder identification for the Phase-4d `with <var> = ...` clause.
 *
 * The name a binder carries in an INSTALLED fact is not always the name
 * the user wrote: `Substitution.in_exp` alpha-renames a binder that
 * shadows something already in the environment by appending primes
 * (`Environment.free_name` / `Var.next_name`), and generated inductive
 * hypotheses are env-substituted before installation. That renaming is
 * invisible in the program text, so a user citing `with t0 = ...` cannot
 * know whether the fact's binder is now `t0'` or `t0'''`.
 *
 * Identification is therefore modulo trailing primes, exact matches
 * first. This is NOT slack in the discharge relation of §4.3 — no
 * proposition matching is involved, only naming which of a rule's own
 * binders is being instantiated. */
let strip_primes = (x: Var.t): Var.t => {
  let n = ref(String.length(x));
  while (n^ > 0 && x.[n^ - 1] == '\'') {
    decr(n);
  };
  String.sub(x, 0, n^);
};

let same_binder = (supplied: Var.t, actual: Var.t): bool =>
  supplied == actual || strip_primes(supplied) == strip_primes(actual);

/* Is this binder pattern exactly the variable `x` (modulo parens, a type
 * ascription, and the prime-renaming above)? */
let rec pat_binder_name = (x: Var.t, pat: Pat.t): option(Var.t) =>
  switch (pat |> Pat.term_of) {
  | Var(y) => same_binder(x, y) ? Some(y) : None
  | Parens(p1) => pat_binder_name(x, p1)
  | Asc(p1, _) => pat_binder_name(x, p1)
  | _ => None
  };

/* Eliminate the quantifier over `x` in a proposition by substituting
 * `e` for it — the Phase-4d `revert <fact> with x = e` semantics
 * (docs/prover-obligations.md, open item 3):
 *
 *   `forall x -> B`            ↦  `B[x := e]`
 *   `forall x where g -> B`    ↦  `g[x := e] ==> B[x := e]`
 *
 * (a `where` restriction is a CONDITION once its binder is gone, so it
 * survives as an antecedent rather than being dropped — dropping it
 * would be unsound). Binders quantified outside `x` are preserved in
 * place. `None` when no binder in the prefix is named `x`; substitution
 * is the same capture-avoiding pass rule instantiation uses. */
let rec instantiate_binder = (x: Var.t, e: Exp.t, prop: Exp.t): option(Exp.t) => {
  /* Substitute for the binder's ACTUAL name — which may carry renaming
   * primes the citation does not (see `same_binder`). */
  let subst = (actual: Var.t, body: Exp.t) =>
    Substitution.in_exp(Environment.of_bindings([(actual, e)]), body);
  switch (prop |> Exp.term_of) {
  | Parens(inner) => instantiate_binder(x, e, inner)
  | Forall(pat, body) =>
    switch (pat_binder_name(x, pat)) {
    | Some(actual) => Some(subst(actual, body))
    | None =>
      instantiate_binder(x, e, body)
      |> Option.map(b => Exp.fresh(Forall(pat, b)))
    }
  | ForallWhere(pat, g, body) =>
    switch (pat_binder_name(x, pat)) {
    | Some(actual) =>
      Some(
        Exp.fresh(
          BinOp(Bool(Implies), subst(actual, g), subst(actual, body)),
        ),
      )
    | None =>
      instantiate_binder(x, e, body)
      |> Option.map(b => Exp.fresh(ForallWhere(pat, g, b)))
    }
  | _ => None
  };
};

/* Peel the top-level `==>` chain off a core proposition:
 * `A ==> B ==> concl` yields ([A, B], concl). */
let rec peel_implications = (exp: Exp.t): (list(Exp.t), Exp.t) =>
  switch (exp |> Exp.term_of) {
  | BinOp(Bool(Implies), e1, e2) =>
    let (assumptions, core) = peel_implications(e2);
    ([e1, ...assumptions], core);
  | _ => ([], exp)
  };

let classify = (exp: Exp.t): conclusion =>
  switch (exp |> Exp.term_of) {
  | BinOp(Poly(Equals), e1, e2) => Equality(e1, e2)
  | _ => Other(exp)
  };

let exp_to_rule = (exp: Exp.t): t => {
  let (bindings, guards, core) = peel_binders(exp);
  let (antecedents, conclusion_core) = peel_implications(core);
  {
    bindings,
    assumptions: guards @ antecedents,
    conclusion: classify(conclusion_core),
  };
};

/* --- The bare-boolean reading (docs/prover-obligations.md §2.1) -------
 *
 * A rule/fact that is KNOWN TO HOLD says its conclusion denotes `true`.
 * So a bare-boolean conclusion `P` — a comparison, a connective, a
 * `where` guard, a Bool-typed application — admits the equality reading
 * `P == true`, which is what makes it usable as a rewrite rule at all.
 * Rule conclusions therefore do not need to be *written* as equations:
 * `... ==> (a * b != 0)` is a rule, and the `== true` that the closure
 * library used to carry existed purely to satisfy the checker.
 *
 * This is the ONE implementation of that reading. It is applied where a
 * rule is USED — `ProofCheck`'s axiom step, and the stepper's rule
 * display/activity filter — and never inside `classify`/`exp_to_rule`:
 * goal classification, `conclusion_exp` round-tripping and the
 * co-context machinery must keep seeing the proposition exactly as
 * written.
 *
 * `~info_map` is optional type evidence. Without it the gate is purely
 * syntactic, which is what a caller that has no statics available (the
 * obligations panel) gets. */

/* Does this operator produce a Bool? Spelled out rather than read off
 * `Operators.semantics_of_bin_op` so that exhaustiveness checking is the
 * guarantee: a newly added operator cannot silently acquire (or lose)
 * the reading. */
let bool_result_bin = (op: Operators.op_bin): bool =>
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

let bool_result_un = (op: Operators.op_un): bool =>
  switch (op) {
  | Bool(Not) => true
  | Int(Minus)
  | Nat(Minus)
  | SInt(Minus)
  | Float(Minus) => false
  };

/* Three-way SYNTACTIC verdict on "is this expression boolean-valued?".
 * `Unknown` is the honest answer for a variable, an application, a
 * `case`, an `if` — forms whose type only statics knows. Deliberately
 * three-way rather than two: `No` refuses the reading outright, while
 * `Unknown` defers to `~info_map` and, absent one, also refuses. */
[@deriving (show({with_path: false}), sexp, yojson)]
type bool_shape =
  | Boolean
  | NotBoolean
  | Unclear;

let rec bool_shape = (e: Exp.t): bool_shape =>
  switch (e |> Exp.term_of) {
  /* Transparent wrappers. `classify` does not peel these, so a
   * parenthesised equation still classifies `Other`; peeling here means
   * `... ==> (P)` reads the same as `... ==> P`. */
  | Parens(e)
  | Projector(_, e)
  | Asc(e, _) => bool_shape(e)
  | Atom(Bool(_)) => Boolean
  | Atom(Int(_) | Nat(_) | SInt(_) | Float(_) | String(_)) => NotBoolean
  | UnOp(op, _) => bool_result_un(op) ? Boolean : NotBoolean
  | BinOp(op, _, _) => bool_result_bin(op) ? Boolean : NotBoolean
  | ListLit(_)
  | Tuple(_)
  | Cons(_, _)
  | ListConcat(_, _)
  | Fun(_, _, _, _)
  | FunWhere(_, _, _)
  | TypFun(_, _, _) => NotBoolean
  | _ => Unclear
  };

/* Is `e` a boolean proposition — i.e. does the `e == true` reading
 * apply? Mirrors `ProofCheck.is_float_typed`'s shape: the info map when
 * it has an answer, the syntactic head otherwise. */
let is_bool_prop = (~info_map: option(Statics.Map.t)=None, e: Exp.t): bool =>
  switch (bool_shape(e)) {
  | Boolean => true
  | NotBoolean => false
  | Unclear =>
    switch (info_map) {
    | None => false
    | Some(info_map) =>
      switch (Statics.Map.ty_of(Exp.rep_id(e), info_map)) {
      | Some(ty) =>
        switch (Typ.term_of(ty)) {
        | Atom(Bool) => true
        | _ => false
        }
      | None => false
      }
    }
  };

/* Grant the reading. `Other(P)` becomes `BoolFact(P)` when `P` is a
 * boolean proposition; anything the gate cannot settle stays `Other`,
 * i.e. inert — a rule we cannot type is not silently turned into a
 * rewrite. */
let with_bool_fact_reading =
    (~info_map: option(Statics.Map.t)=None, rule: t): t =>
  switch (rule.conclusion) {
  | Equality(_, _)
  | BoolFact(_) => rule
  | Other(e) =>
    is_bool_prop(~info_map, e)
      ? {
        ...rule,
        conclusion: BoolFact(e),
      }
      : rule
  };

/* The proposition a rule is being read as `== true`, for display. */
let bool_reading = (rule: t): option(Exp.t) =>
  switch (rule.conclusion) {
  | BoolFact(e) => Some(e)
  | Equality(_, _)
  | Other(_) => None
  };

/* ... and that reading as an expression, so display can render the
 * equation the checker actually uses rather than describe it in prose. */
let bool_reading_exp = (rule: t): option(Exp.t) =>
  bool_reading(rule)
  |> Option.map(e =>
       Exp.fresh(BinOp(Poly(Equals), e, Exp.fresh(Atom(Bool(true)))))
     );

let wrap_assumptions = (assumptions: list(Exp.t), body: Exp.t): Exp.t =>
  List.fold_right(
    (a, acc) => Exp.fresh(BinOp(Bool(Implies), a, acc)),
    assumptions,
    body,
  );

/* The rule's conclusion AS WRITTEN. A `BoolFact` prints as the bare
 * boolean, not as the equation it is read as — the reading is an
 * interpretation of the statement, not part of it, so `rule_to_exp` /
 * `rule_to_typ` still round-trip to the user's own text. */
let conclusion_exp = (rule: t): Exp.t =>
  switch (rule.conclusion) {
  | Equality(e1, e2) => Exp.fresh(BinOp(Poly(Equals), e1, e2))
  | BoolFact(e)
  | Other(e) => e
  };

/* The rule's core proposition: the assumptions re-wrapped as a `==>`
 * chain onto the conclusion. This is the goal a proof of the rule works
 * on after its binders are introduced. */
let core_exp = (rule: t): Exp.t =>
  wrap_assumptions(rule.assumptions, conclusion_exp(rule));

let rule_to_exp = (rule: t): Exp.t => {
  let rec wrap_foralls = (bindings: list(Ctx.entry), body: Exp.t): Exp.t =>
    switch (bindings) {
    | [] => body
    | [VarEntry({name, typ, _}), ...rs] =>
      wrap_foralls(
        rs,
        Exp.fresh(
          Forall(Pat.fresh(Asc(Pat.fresh(Var(name)), typ)), body),
        ),
      )
    | [
        ConstructorEntry(_) | TVarEntry(_) | LivelitEntry(_) | TheoremEntry(_),
        ...rs,
      ] =>
      wrap_foralls(rs, body)
    };
  /* Note: `where` guards and `==>` antecedents are conflated in
   * `assumptions`, so this rebuilds all of them as a `==>` chain —
   * logically equivalent to the source, though not lexically identical
   * for restricted binders. */
  wrap_foralls(rule.bindings, core_exp(rule));
};

let rec get_empty_bindings = (ctx: list(Ctx.entry)) =>
  switch (ctx) {
  | [] => []
  | [VarEntry(var_entry), ...rs] => [
      (var_entry.name, (var_entry.typ, None)),
      ...get_empty_bindings(rs),
    ]
  | [_, ...rs] => get_empty_bindings(rs)
  };

/* Pre-assign one of the rule's binders in a match context: the Phase-4d
 * `with <var> = <exp>` clause, seeded BEFORE matching so that matching
 * only has to resolve what is left (docs/prover-obligations.md, open
 * item 3). `None` when `name` is not a binder of the rule — the caller
 * reports that as `UnknownInstantiationVar`. */
let seed_binding =
    (name: Var.t, exp: Exp.t, bindings: MatchExp.match_ctx)
    : option(MatchExp.match_ctx) => {
  /* Exact match first, prime-insensitive fallback second (see
   * `same_binder`); the outermost matching binder wins. */
  let target =
    if (MatchExp.match_ctx_has(bindings, name)) {
      Some(name);
    } else {
      List.find_map(
        ((n, (_, _))) => same_binder(name, n) ? Some(n) : None,
        bindings,
      );
    };
  target
  |> Option.map(target =>
       List.map(
         ((n, (t, e))) =>
           n == target ? (n, (t, Some(exp))) : (n, (t, e)),
         bindings,
       )
     );
};

/* Like `can_eq` below, but also returns the match context that produced
 * each rewrite, so callers can instantiate the rule's assumptions and
 * detect unresolved metavariables (underdetermined instantiation,
 * docs/prover-obligations.md §4.1). `~bindings` overrides the initial
 * (all-unassigned) match context, which is how an explicit `with` clause
 * is seeded. */
let can_eq_inst =
    (~info_map, ~env, ~bindings=?, ~reverse=false, rule: t, exp: Exp.t)
    : (
        option((Exp.t, MatchExp.match_ctx)),
        option((Exp.t, MatchExp.match_ctx)),
      ) => {
  let bindings =
    switch (bindings) {
    | Some(b) => b
    | None => get_empty_bindings(rule.bindings)
    };
  let via = (from, to_) =>
    MatchExp.match_exp(
      ~info_map,
      ~exp_env=env,
      ~exp_r_ctx=bindings,
      from,
      exp,
    )
    |> Option.map(mctx => (MatchExp.substitute_exp(mctx, to_), mctx));
  switch (rule.conclusion) {
  /* `Left`/`Right` are `axiomrev`/`axiom` (Conversion.re): Right matches
   * the LHS and rewrites to the RHS. */
  | Equality(a, b) => (via(b, a), via(a, b))
  /* Read as `P == true`. Forward (`axiom`) matches `P` and rewrites it
   * to `true` — the direction that does the work.
   *
   * Reverse (`axiomrev`) rewrites a `true` in the goal to `P`. It is
   * SOUND — the rule holds, so `true` and `P` denote the same value at
   * any instantiation — but `true` occurs everywhere, so offering it
   * during rule DISCOVERY would make every bare-boolean rule "active"
   * at every `true` in the goal. It is therefore gated on `~reverse`,
   * which only an explicit citation turns on: an `axiomrev` step names
   * the rule, the direction and the occurrence index, so the noise is
   * the user's own choice. Note this direction adds no new hazard
   * class: `or_true`/`impl_true`/`false_impl` are equations whose RHS
   * is literally `true`, so reverse-matching on `true` (and leaving the
   * rule's own metavariables to a Phase-4d `with` clause) is already
   * how those are used. */
  | BoolFact(p) => (
      reverse ? via(Exp.fresh(Atom(Bool(true))), p) : None,
      via(p, Exp.fresh(Atom(Bool(true)))),
    )
  | Other(_) => (None, None)
  };
};

let can_eq =
    (~info_map, ~env, ~reverse=false, rule: t, exp: Exp.t)
    : (option(Exp.t), option(Exp.t)) => {
  let (l, r) = can_eq_inst(~info_map, ~env, ~reverse, rule, exp);
  (Option.map(fst, l), Option.map(fst, r));
};

let is_active = (~info_map, ~env, rule: t, exp: Exp.t): bool =>
  switch (can_eq(~info_map, ~env, rule, exp)) {
  | (Some(_), _)
  | (_, Some(_)) => true
  | _ => false
  };

/* Names a rule's own binders bind (the entries `rule_to_exp` turns into
 * `forall`s). Free-variable questions about the core are asked modulo
 * these. */
let binding_names = (bindings: list(Ctx.entry)): list(Var.t) =>
  List.filter_map(
    (entry: Ctx.entry) =>
      switch (entry) {
      | VarEntry({name, _}) => Some(name)
      | ConstructorEntry(_)
      | TVarEntry(_)
      | LivelitEntry(_)
      | TheoremEntry(_) => None
      },
    bindings,
  );

/* Does any name in `vs` occur FREE in `e`, given `bound` already in scope?
 *
 * Binding structure mirrors `Substitution.in_exp` — the codebase's
 * authority on expression-variable scope, and the pass these same
 * expressions have already been through — with one deliberate agreement
 * with `Statics`: `Closure(env, body)` looks only at `body` and never
 * expands `env` (cf. Statics' Closure case, which passes the body's
 * co-context straight through). Expanding it would inline the whole
 * environment, which is exactly the blow-up this walk exists to avoid.
 *
 * Non-binding forms fall through to `map_term`'s generic traversal, which
 * also carries the walk into expressions nested inside patterns (`Asc`)
 * under the enclosing scope — matching `in_pat`'s
 * use of the outer environment there. */
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
        under(Pat.bound_vars(p) @ bound, [body]);
      | ForallWhere(p, g, body)
      | FunWhere(p, g, body) =>
        go_pat(bound, p);
        under(Pat.bound_vars(p) @ bound, [g, body]);
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

/* Does the rule's core proposition (assumptions ==> conclusion, so that
 * free variables of the assumptions are seen too) mention any of `vs`
 * free, modulo the rule's own binders?
 *
 * This used to run FULL statics on the core and inspect the resulting
 * co-context. That is a free-variable question wearing an expensive
 * costume, and on env-substituted proof objects — whose statements have
 * whole function definitions inlined — it was by far the deepest
 * recursion in the evaluation phase: on docs/stlc-progress-example.hazel
 * it drove ~600k redundant info-map nodes and pushed evaluation's stack
 * demand past what a browser gives a worker. */
let mentions_any = (rule: t, vs: list(Var.t)): bool =>
  occurs_free_any(~bound=binding_names(rule.bindings), vs, core_exp(rule));
