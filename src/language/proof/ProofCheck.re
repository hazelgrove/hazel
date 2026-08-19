open Util;

/* Proof checker (big-step).
 *
 * Walks a proof term, threading an "incoming" expression through each
 * sub-form and producing an "outgoing" expression (if the proof
 * propagates through). The walk also accumulates a ProofMap keyed by
 * each proof sub-term id recording the (incoming, outgoing) pair seen
 * at that node, plus any `ProofMark.t` errors emitted by the step.
 *
 * This module lives alongside the other proof-rule machinery and
 * deliberately does not import the dynamics `Transition` / `Evaluator`
 * modules. When the checker needs to take a single evaluation step (as
 * an optional normalisation helper for some rules), it does so through
 * the injected `step_fn` callback. The big-step evaluator supplies this
 * callback when it invokes the checker.
 */

type step_result = {
  auto_incoming: list((string, Exp.t)),
  auto_outgoing: list((Exp.t, string)),
  outgoing: Exp.t,
};

type step_fn = (~env: Environment.t(Exp.t), Exp.t) => option(step_result);

/* Default step callback: a no-op. Use at call sites that don't have (or
 * don't need) a real single-step function. */
let no_step: step_fn = (~env as _, _e) => None;

let entry =
    (
      ~incoming: option(Exp.t),
      ~auto_incoming: list((string, Exp.t))=[],
      ~auto_outgoing: list((Exp.t, string))=[],
      ~outgoing: option(Exp.t),
      ~marks: list(ProofMark.t)=[],
      ~obligations: list(Obligation.t)=[],
      (),
    )
    : ProofMap.entry => {
  incoming,
  auto_incoming,
  auto_outgoing,
  outgoing,
  marks,
  obligations,
};

/* Record an entry at `id`, merged into accumulated map `m`. */
let record =
    (
      ~marks: list(ProofMark.t)=[],
      ~auto_incoming: list((string, Exp.t))=[],
      ~auto_outgoing: list((Exp.t, string))=[],
      ~obligations: list(Obligation.t)=[],
      id: Id.t,
      incoming,
      outgoing,
      m: ProofMap.t,
    )
    : ProofMap.t =>
  ProofMap.add(
    id,
    entry(
      ~incoming,
      ~auto_incoming,
      ~auto_outgoing,
      ~outgoing,
      ~marks,
      ~obligations,
      (),
    ),
    m,
  );

/* --- Axiom/Algebrite step helpers -----------------------------------
 *
 * These mirror the per-step "calculate" logic in the AxiomStep / AlgebriteStep
 * UI modules. Keeping them here means the evaluator and the UI stepper can
 * share one source of truth for the semantic content of a step.
 *
 * The `_ast` variants (invoked by the proof AST) return `result(Exp.t,
 * ProofMark.t)` so callers can report precisely why a step failed. The
 * canonical helpers (taking already-extracted int/string args) are kept
 * as simple option-returning functions so the UI stepper modules can use
 * them unchanged.
 */

/* The concrete syntax of AxiomStep/AlgebriteStep stores `at_idx` as an
 * expression; we try to read an integer literal out of it. */
let exp_to_int = (e: Exp.t): option(int) =>
  switch (e |> Exp.term_of) {
  | Atom(Int(n)) => Bigint.to_int(n)
  | _ => None
  };

/* Peel parens/projectors from an expression, so we can read the "head"
 * (e.g. a variable name referring to an axiom). */
let rec unwrap_head = (e: Exp.t): Exp.t =>
  switch (e |> Exp.term_of) {
  | Parens(inner) => unwrap_head(inner)
  | Projector(_, inner) => unwrap_head(inner)
  | _ => e
  };

let exp_to_equality_name = (e: Exp.t): option(string) =>
  switch (unwrap_head(e) |> Exp.term_of) {
  | Var(name) => Some(name)
  | _ => None
  };

/* Does variable `name` occur (as a Var) anywhere in `e`? Used to detect
 * rule metavariables left unresolved by matching. Deliberately ignores
 * shadowing by binders inside `e`: rule metavariables and expression-level
 * binders sharing a name is a degenerate case, and over-reporting an
 * occurrence only makes the underdetermined check more conservative. */
let occurs_var = (name: Var.t, e: Exp.t): bool => {
  let found = ref(false);
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e: Exp.t) =>
          switch (e |> Exp.term_of) {
          | Var(x) when x == name =>
            found := true;
            e;
          | _ => continue(e)
          },
      e,
    );
  found^;
};

/* Structured-error axiom-step outgoing used by `check`. Categorises each
 * failure into a `ProofMark.t`. On success also returns the rule's
 * assumptions instantiated by the match — the conditions the application
 * incurs as obligations (empty for unconditional rules) — and the
 * metavariable instantiations the match bound (for the Phase-3a
 * definedness gates: totality check + domain scan, docs/
 * prover-obligations.md §4.1). A conditional rule whose assumptions
 * mention metavariables the match left unresolved is refused
 * (UnderdeterminedInstantiation, v1 behavior per §4.1). */
let axiom_step_outgoing_result =
    (
      ~info_map: Statics.Map.t,
      ~env: Environment.t(Exp.t),
      ~ctx: Ctx.t,
      ~at_idx: int,
      ~at_exp: Exp.t,
      ~direction: Direction.t,
      ~equality: string,
      incoming: Exp.t,
    )
    : result((Exp.t, list(Exp.t), list((Var.t, Exp.t))), ProofMark.t) => {
  let proof_ctx = ProofCtx.of_env(~builtins=Axioms.v, ~ctx, env);
  switch (ProofCtx.lookup_rule(equality, proof_ctx)) {
  | None => Error(UnknownEquality(equality))
  | Some(rule) =>
    /* Phase 4c: a cited bare-boolean fact also reads as `F == true`. */
    let rule = ProofRule.with_bool_fact_reading(rule);
    switch (ProofHacks.nth_exp_env(~env, at_exp, at_idx, incoming)) {
    | None =>
      Error(
        PatternNotFound({
          at_exp,
          at_idx,
        }),
      )
    | Some(e) =>
      let (l, r) = ProofRule.can_eq_inst(~info_map, ~env, rule, e);
      let with_exp =
        switch (direction) {
        | Direction.Left => l
        | Direction.Right => r
        };
      switch (with_exp) {
      | None =>
        Error(
          RuleDoesNotApply({
            equality,
            direction,
          }),
        )
      | Some((w, mctx)) =>
        let unresolved =
          List.filter_map(
            ((n, (_, assigned))) => assigned == None ? Some(n) : None,
            mctx,
          );
        let underdetermined =
          List.exists(
            a => List.exists(n => occurs_var(n, a), unresolved),
            rule.assumptions,
          );
        if (underdetermined) {
          Error(UnderdeterminedInstantiation({equality: equality}));
        } else {
          let instantiated =
            List.map(MatchExp.substitute_exp(mctx), rule.assumptions);
          let instantiations =
            List.filter_map(
              ((n, (_, assigned))) => Option.map(e => (n, e), assigned),
              mctx,
            );
          Ok((
            ProofHacks.replace_exp_id(Exp.rep_id(e), incoming, w),
            instantiated,
            instantiations,
          ));
        };
      };
    };
  };
};

/* Canonical axiom-step outgoing: given a resolved equality name and
 * occurrence index, produce the rewritten `incoming`. Used by the UI
 * stepper. Option-returning (no mark taxonomy, drops the incurred
 * conditions) so the UI stepper module stays unchanged. */
let axiom_step_outgoing =
    (
      ~info_map: Statics.Map.t,
      ~env: Environment.t(Exp.t),
      ~ctx: Ctx.t,
      ~at_idx: int,
      ~at_exp: Exp.t,
      ~direction: Direction.t,
      ~equality: string,
      incoming: Exp.t,
    )
    : option(Exp.t) =>
  axiom_step_outgoing_result(
    ~info_map,
    ~env,
    ~ctx,
    ~at_idx,
    ~at_exp,
    ~direction,
    ~equality,
    incoming,
  )
  |> (
    fun
    | Ok((out, _conditions, _instantiations)) => Some(out)
    | Error(_) => None
  );

/* Wrapper invoked from the Proof AST: extracts idx/name out of the
 * expression-shaped arguments before calling the structured helper. */
let axiom_step_outgoing_ast =
    (
      ~info_map: Statics.Map.t,
      ~env: Environment.t(Exp.t),
      ~ctx: Ctx.t,
      ~at_idx: Exp.t,
      ~at_exp: Exp.t,
      ~direction: Direction.t,
      ~equality: Exp.t,
      incoming: Exp.t,
    )
    : result((Exp.t, list(Exp.t), list((Var.t, Exp.t))), ProofMark.t) =>
  switch (exp_to_int(at_idx)) {
  | None => Error(MalformedIndex)
  | Some(idx) =>
    switch (exp_to_equality_name(equality)) {
    | None => Error(MalformedEqualityName)
    | Some(name) =>
      axiom_step_outgoing_result(
        ~info_map,
        ~env,
        ~ctx,
        ~at_idx=idx,
        ~at_exp,
        ~direction,
        ~equality=name,
        incoming,
      )
    }
  };

/* Is this expression Float-typed? Statics first, with a syntactic
 * fallback (float literal / float arithmetic at the head) for
 * expressions statics did not reach. Used by the Algebrite float gate
 * (docs/prover-obligations.md §1.5). */
let rec float_head = (e: Exp.t): bool =>
  switch (e |> Exp.term_of) {
  | Parens(e1)
  | Projector(_, e1) => float_head(e1)
  | Atom(Float(_)) => true
  | UnOp(Float(_), _) => true
  | BinOp(Float(Plus | Minus | Times | Power | Divide), _, _) => true
  | _ => false
  };

let is_float_typed = (~info_map: Statics.Map.t, e: Exp.t): bool =>
  switch (Statics.Map.ty_of(Exp.rep_id(e), info_map)) {
  | Some(ty) =>
    switch (Typ.term_of(ty)) {
    | Atom(Float) => true
    | Unknown(_) => float_head(e)
    | _ => false
    }
  | None => float_head(e)
  };

let algebrite_step_outgoing =
    (~at_idx: int, ~at_exp: Exp.t, ~with_exp: Exp.t, incoming: Exp.t)
    : option(Exp.t) =>
  switch (ProofHacks.nth_exp(at_exp, at_idx, incoming)) {
  | None => None
  | Some(e) =>
    Some(ProofHacks.replace_exp_id(Exp.rep_id(e), incoming, with_exp))
  };

let algebrite_step_outgoing_result =
    (~at_idx: int, ~at_exp: Exp.t, ~with_exp: Exp.t, incoming: Exp.t)
    : result(Exp.t, ProofMark.t) =>
  switch (ProofHacks.nth_exp(at_exp, at_idx, incoming)) {
  | None =>
    Error(
      PatternNotFound({
        at_exp,
        at_idx,
      }),
    )
  | Some(e) =>
    Ok(ProofHacks.replace_exp_id(Exp.rep_id(e), incoming, with_exp))
  };

let algebrite_step_outgoing_ast =
    (~at_idx: Exp.t, ~at_exp: Exp.t, ~with_exp: Exp.t, incoming: Exp.t)
    : result(Exp.t, ProofMark.t) =>
  switch (exp_to_int(at_idx)) {
  | None => Error(MalformedIndex)
  | Some(idx) =>
    algebrite_step_outgoing_result(~at_idx=idx, ~at_exp, ~with_exp, incoming)
  };

/* Canonical eval-step outgoing: locate the `at_idx`-th occurrence of
 * `at_exp` in `incoming`, apply a single dynamic evaluation step to it
 * via the injected `step_fn`, and splice the result back in. */
let eval_step_outgoing =
    (
      ~step: step_fn,
      ~env: Environment.t(Exp.t),
      ~at_idx: int,
      ~at_exp: Exp.t,
      incoming: Exp.t,
    )
    : option(Exp.t) =>
  switch (ProofHacks.nth_exp_env(~env, at_exp, at_idx, incoming)) {
  | None => None
  | Some(e) =>
    switch (step(~env, e)) {
    | None => None
    | Some({outgoing, _}) =>
      Some(ProofHacks.replace_exp_id(Exp.rep_id(e), incoming, outgoing))
    }
  };

let eval_step_outgoing_result =
    (
      ~step: step_fn,
      ~env: Environment.t(Exp.t),
      ~at_idx: int,
      ~at_exp: Exp.t,
      incoming: Exp.t,
    )
    : result(step_result, ProofMark.t) =>
  switch (ProofHacks.nth_exp_env(~env, at_exp, at_idx, incoming)) {
  | None =>
    Error(
      PatternNotFound({
        at_exp,
        at_idx,
      }),
    )
  | Some(e) =>
    switch (step(~env, e)) {
    | None => Error(NothingToStep({at_exp: e}))
    | Some({auto_incoming, auto_outgoing, outgoing}) =>
      let lift = ProofHacks.replace_exp_id(Exp.rep_id(e), incoming);
      Ok({
        auto_incoming:
          List.map(
            ((justification, output)) => (justification, lift(output)),
            auto_incoming,
          ),
        auto_outgoing:
          List.map(
            ((input, justification)) => (lift(input), justification),
            auto_outgoing,
          ),
        outgoing: lift(outgoing),
      });
    }
  };

let eval_step_outgoing_ast =
    (
      ~step: step_fn,
      ~env: Environment.t(Exp.t),
      ~at_idx: Exp.t,
      ~at_exp: Exp.t,
      incoming: Exp.t,
    )
    : result(step_result, ProofMark.t) =>
  switch (exp_to_int(at_idx)) {
  | None => Error(MalformedIndex)
  | Some(idx) =>
    eval_step_outgoing_result(~step, ~env, ~at_idx=idx, ~at_exp, incoming)
  };

/* Discharge channel 1 (binder lookup): search the facts visible in the
 * given scope — hypotheses added via `SemanticCtx.add_hypothesis`
 * (`assume`, `case_eq`, `ih`, ...) live in the ctx as var entries typed
 * `ProofOf(fact)` — for one that syntactically covers `goal`. The
 * comparison is deliberately a dumb `Exp.fast_equal`: the design mandates
 * a transparent, lookup-only discharge relation
 * (docs/prover-obligations.md §4.2–4.3). Returns the covering fact's
 * stable entry id. */
let lookup_fact = (ctx: SemanticCtx.t, goal: Exp.t): option(Id.t) =>
  SemanticCtx.get_ctx(ctx)
  |> Ctx.get_var_entries
  |> List.find_map((e: Ctx.var_entry) =>
       switch (Typ.term_of(e.typ)) {
       | ProofOf(fact) when Exp.fast_equal(fact, goal) => Some(e.id)
       | _ => None
       }
     );

/* --- Discharge channel 2: closed evaluation (§4.2) -------------------
 *
 * Ground obligations (`2 != 0`) just run: if the goal has no free
 * variables after env substitution, evaluate it by iterating the
 * injected single-step function to a fixpoint (with a generous fuel
 * bound, since `step_fn` exposes no termination signal) and check for
 * the literal `true`. Open goals are NEVER evaluated. */

let closed_eval_fuel = 1000;

/* Closedness via the co-context machinery (cf. `ProofRule.get_coctx`):
 * run statics on the goal against an empty ctx; an empty co-context
 * means no free variable occurrences. */
let is_closed = (goal: Exp.t): bool => {
  let (statics, _) =
    Statics.mk(~ana=Typ.temp(Atom(Bool)), CoreSettings.on, Ctx.empty, goal);
  switch (Statics.Map.lookup_exp(Exp.rep_id(goal), statics)) {
  | Some(info) => Info.exp_co_ctx(info) == []
  | None => false
  };
};

let rec eval_via_step = (~step: step_fn, ~env, ~fuel: int, e: Exp.t): Exp.t =>
  if (fuel <= 0) {
    e;
  } else {
    switch (step(~env, e)) {
    | Some({outgoing, _}) =>
      eval_via_step(~step, ~env, ~fuel=fuel - 1, outgoing)
    | None => e
    };
  };

/* Run the discharge channels in order on an (env-substituted) obligation
 * goal: 1. binder lookup, 2. closed evaluation; otherwise Pending. */
let discharge_goal =
    (~step: step_fn, ~ctx: SemanticCtx.t, goal: Exp.t): Obligation.discharge =>
  switch (lookup_fact(ctx, goal)) {
  | Some(fact_id) => Obligation.Remote(fact_id)
  | None =>
    if (is_closed(goal)) {
      let result =
        eval_via_step(
          ~step,
          ~env=SemanticCtx.get_env(ctx),
          ~fuel=closed_eval_fuel,
          goal,
        );
      Exp.fast_equal(result, Exp.temp(Atom(Bool(true))))
        ? Obligation.Evaluated : Obligation.Pending;
    } else {
      Obligation.Pending;
    }
  };

/* Build the obligation record for one incurred condition at step `id`,
 * running the discharge channels. */
let incur_obligation =
    (~step: step_fn, ~ctx: SemanticCtx.t, ~origin: Id.t, goal: Exp.t)
    : Obligation.t => {
  let goal = goal |> Substitution.in_exp(SemanticCtx.get_env(ctx));
  Obligation.{
    origin,
    bindings: SemanticCtx.get_ctx(ctx).entries,
    goal,
    discharge: discharge_goal(~step, ~ctx, goal),
  };
};

/* --- Phase 3a: definedness gates (docs/prover-obligations.md §4.1) ----
 *
 * Two-tier treatment of partiality at the gates:
 *   - DIVERGENCE (⊥) is never a boolean obligation — a failed
 *     structural-totality check REFUSES the step (PossiblyDivergent*
 *     marks).
 *   - DOMAIN ERRORS (err) are boolean-expressible — the domain scan's
 *     conditions are incurred as ordinary obligations through the
 *     discharge channels.
 * Eval steps carry NO gates: they are denotation-preserving (§5). */

/* Deduplicate a condition list (Exp.fast_equal), preserving order. */
let dedup_conditions = (conditions: list(Exp.t)): list(Exp.t) =>
  List.fold_left(
    (acc, c) => List.exists(Exp.fast_equal(c), acc) ? acc : acc @ [c],
    [],
    conditions,
  );

/* Instantiation gate (axiom/lemma steps): every metavariable
 * instantiation bound by the match must be structurally total —
 * `Error(var)` names the first that is not (the step is refused).
 * Otherwise `Ok(conditions)` carries the domain scan of the
 * instantiations. The common instantiations — a quantified variable, a
 * literal, compositions of them under total ops (x := y, x := 2,
 * x := a + b) — pass the totality check and scan to nothing, so they
 * emit no traffic at all. */
let instantiation_gate =
    (
      ~info_map: Statics.Map.t,
      ~ctx: SemanticCtx.t,
      instantiations: list((Var.t, Exp.t)),
    )
    : result(list(Exp.t), Var.t) => {
  let env = SemanticCtx.get_env(ctx);
  /* Substitute first: visible definitions are inlined (checkable
   * through their bodies), recursive ones surface their FixF spine,
   * quantified binders stay bare Vars. */
  let substituted =
    List.map(
      ((n, e)) => (n, e |> Substitution.in_exp(env)),
      instantiations,
    );
  let divergent =
    List.find_map(
      ((n, e)) =>
        switch (Totality.check(~info_map, ~ctx=SemanticCtx.get_ctx(ctx), e)) {
        | Ok () => None
        | Error(_) => Some(n)
        },
      substituted,
    );
  switch (divergent) {
  | Some(n) => Error(n)
  | None =>
    Ok(
      substituted
      |> List.concat_map(((_, e)) => DomainConditions.scan(e))
      |> dedup_conditions,
    )
  };
};

/* Peel the outermost binder from an incoming "for all pat, P" goal. Used
 * by the `Forall` and `Intro` proof forms to walk under the binder. The
 * third component is the binder's `where` restriction, if any — peeling a
 * restricted binder additionally installs it as a hypothesis (a free,
 * sound introduction; docs/prover-obligations.md §2.2). */
let peel_binder =
    (incoming: option(Exp.t))
    : option((Pat.t, Typ.t, option(Exp.t), Exp.t)) => {
  open OptUtil.Syntax;
  let* e = incoming;
  switch (e |> Exp.term_of) {
  | Fun(p, d1, t, _) =>
    let t = OptUtil.get(() => Typ.fresh(Unknown(Internal)), t);
    Some((p, t, None, d1));
  | Forall(p, d1) =>
    /* No annotated type at this level, use Unknown. */
    Some((p, Typ.fresh(Unknown(Internal)), None, d1))
  | ForallWhere(p, g, d1) =>
    Some((p, Typ.fresh(Unknown(Internal)), Some(g), d1))
  /* A contract function peels like Fun, with its guard installed as a
   * hypothesis — same free intro as ForallWhere (§2.2). */
  | FunWhere(p, g, d1) =>
    Some((p, Typ.fresh(Unknown(Internal)), Some(g), d1))
  | _ => None
  };
};

/* Auto-introduce a theorem statement's outer binders: extend the semantic
 * ctx with each binder's variables, install `where` restrictions as
 * hypotheses (base name "where"), and return the remaining core goal —
 * which retains any `==>` antecedents (those are introduced explicitly,
 * via `assume`). Used by the big-step evaluator's theorem hook and the
 * per-theorem UI stepper to seed the proof goal. */
let rec peel_stmt_binders =
        (ctx: SemanticCtx.t, goal: Exp.t): (SemanticCtx.t, Exp.t) =>
  switch (goal |> Exp.term_of) {
  | Forall(p, body) =>
    peel_stmt_binders(
      SemanticCtx.add_from_pattern(ctx, p, Typ.fresh(Unknown(Internal))),
      body,
    )
  | ForallWhere(p, g, body) =>
    let ctx =
      SemanticCtx.add_from_pattern(ctx, p, Typ.fresh(Unknown(Internal)));
    let g = g |> Substitution.in_exp(SemanticCtx.get_env(ctx));
    let (ctx, _) = SemanticCtx.add_hypothesis(ctx, "where", g);
    peel_stmt_binders(ctx, body);
  | _ => (ctx, goal)
  };

/* --- Phase 3b: definition-time discharge of function contracts --------
 * (docs/prover-obligations.md §2.2, "definition-time discharge")
 *
 * When a theorem is checked, every function DEFINITION its proof
 * context can see (env-bound, non-builtin, non-recursive) is scanned
 * once, at the definition's own altitude:
 *   - `fun p where g -> body`: each of the body's domain conditions is
 *     discharged if it fast_equal-matches the guard `g` or one of its
 *     `&&`-conjuncts; leftover conditions become obligations.
 *   - `fun p -> body` (no guard): every body condition is an
 *     obligation — recorded here, ONCE, instead of at every call
 *     (DomainConditions.scan deliberately does not descend into
 *     function bodies).
 *
 * Leftovers are recorded in the ProofMap keyed by the FUNCTION term's
 * id — a non-proof id, so the entry is minimal: incoming/outgoing None,
 * no marks, only obligations, each with `origin` = the function's id.
 * Recording is idempotent across theorems (same definition, same
 * entry; ProofMap.union replaces like with like). Because the
 * function's id is not inside any proof subtree, definition
 * obligations do not affect Proven/ProvenModulo status in v1 — they
 * are honest, inspectable residue at the definition, for the (!) UI.
 *
 * The body scan runs with the parameter's bindings visible
 * (SemanticCtx.add_from_pattern), so conditions may mention the
 * parameter. v1 skips (documented): builtins; recursive definitions
 * (FixF spine — tier-2 totality is Phase 4); non-function bindings. */

let rec guard_conjuncts = (g: Exp.t): list(Exp.t) =>
  switch (g |> Exp.term_of) {
  | Parens(g1) => guard_conjuncts(g1)
  | BinOp(Bool(And), a, b) => guard_conjuncts(a) @ guard_conjuncts(b)
  | _ => [g]
  };

let definition_obligations =
    (~step: step_fn=no_step, ~ctx: SemanticCtx.t, ()): ProofMap.t => {
  let env = SemanticCtx.get_env(ctx);
  let base_ctx = SemanticCtx.get_ctx(ctx);
  /* Builtin ctx entries carry Id.invalid (BuiltinsUtil), distinguishing
   * them from user definitions — cf. Totality.is_builtin. */
  let is_builtin = (name: Var.t): bool =>
    switch (Ctx.lookup_var(base_ctx, name)) {
    | Some(entry) => entry.id == Id.invalid
    | None => false
    };
  let record_leftovers = (acc, fun_term: Exp.t, p: Pat.t, guard, body) => {
    let conjuncts =
      switch (guard) {
      | Some(g) => guard_conjuncts(g)
      | None => []
      };
    let leftover =
      DomainConditions.scan(body)
      |> List.filter(c => !List.exists(Exp.fast_equal(c), conjuncts));
    switch (leftover) {
    | [] => acc
    | _ =>
      let fun_id = Exp.rep_id(fun_term);
      let ctx' =
        SemanticCtx.add_from_pattern(ctx, p, Typ.fresh(Unknown(Internal)));
      let obligations =
        List.map(
          incur_obligation(~step, ~ctx=ctx', ~origin=fun_id),
          leftover,
        );
      ProofMap.add(
        fun_id,
        entry(~incoming=None, ~outgoing=None, ~obligations, ()),
        acc,
      );
    };
  };
  Environment.to_list(env)
  |> List.fold_left(
       (acc, (name, value)) =>
         if (is_builtin(name)) {
           acc;
         } else {
           /* Resolve closures / inline visible definitions so the body
            * scan sees concrete terms; ids of the function node itself
            * are preserved by substitution. */
           let v =
             DomainConditions.unwrap(value |> Substitution.in_exp(env));
           switch (v |> Exp.term_of) {
           | FunWhere(p, g, body) =>
             record_leftovers(acc, v, p, Some(g), body)
           | Fun(p, body, _, _) => record_leftovers(acc, v, p, None, body)
           /* FixF (recursive), non-functions, self-map Vars: skipped. */
           | _ => acc
           };
         },
       ProofMap.empty,
     );
};

/* Translate an Ok/Error result from one of the *_ast helpers into an
 * (outgoing, marks) pair suitable for `record`.
 *
 * Error recovery: a failed step records its mark but passes the incoming
 * expression through unchanged, so later steps still get checked against
 * the last good expression instead of going dark. The pass-through can
 * carry `true`/`false` past a broken step, so proven/disproven status
 * additionally requires a mark-free proof subtree (see
 * `ProofMap.status_of_proof`). */
let result_to_outgoing =
    (~incoming: Exp.t, r: result(Exp.t, ProofMark.t))
    : (option(Exp.t), list(ProofMark.t)) =>
  switch (r) {
  | Ok(e) => (Some(e), [])
  | Error(m) => (Some(incoming), [m])
  };

/* Core walk: threads `incoming` through the proof tree, producing
 * the outgoing expression (if propagation holds) and a proof map
 * populated at every proof sub-term id. */
let rec check =
        (
          ~step: step_fn=no_step,
          ~info_map: Statics.Map.t,
          ~ctx: SemanticCtx.t,
          incoming: option(Exp.t),
          proof: Proof.t,
        )
        : (option(Exp.t), ProofMap.t) => {
  let id = Proof.rep_id(proof);
  switch (proof.term) {
  | EmptyHole =>
    /* Leaf: a hole stands for "the proof continues here", so it acts as
     * the identity — the incoming goal passes through untouched and
     * later steps keep working. No mark: holes reflect an
     * intentionally-incomplete proof, not an error. */
    (incoming, record(id, incoming, incoming, ProofMap.empty))
  | Invalid(_) =>
    /* Unparseable proof text is an error, unlike an EmptyHole; like any
     * other broken step it passes the goal through (see
     * `result_to_outgoing`). */
    (
      incoming,
      record(
        ~marks=[ProofMark.MalformedProofTerm],
        id,
        incoming,
        incoming,
        ProofMap.empty,
      ),
    )
  | MultiHole(_) =>
    /* We don't recurse into the any-kind children here since they are
     * not proof terms; treat the whole multi-hole as opaquely broken. */
    (
      incoming,
      record(
        ~marks=[ProofMark.MalformedProofTerm],
        id,
        incoming,
        incoming,
        ProofMap.empty,
      ),
    )
  | Seq(p1, p2) =>
    let (out1, m1) = check(~step, ~info_map, ~ctx, incoming, p1);
    let (out2, m2) = check(~step, ~info_map, ~ctx, out1, p2);
    (out2, record(id, incoming, out2, ProofMap.union(m1, m2)));
  | AxiomStep({at_idx, at_exp, direction, equality}) =>
    let (outgoing, marks, obligations) =
      switch (incoming) {
      | None => (None, [ProofMark.MissingIncoming], [])
      | Some(inc) =>
        switch (
          axiom_step_outgoing_ast(
            ~info_map,
            ~env=SemanticCtx.get_env(ctx),
            ~ctx=SemanticCtx.get_ctx(ctx),
            ~at_idx,
            ~at_exp,
            ~direction,
            ~equality,
            inc,
          )
        ) {
        | Ok((out, condition_goals, instantiations)) =>
          /* Phase-3a instantiation gate (§4.1): the matched
           * instantiations must be structurally total (divergence is
           * refused, never an obligation), and their domain scan joins
           * the rule's own instantiated assumptions as obligations on
           * this step, running the discharge channels. */
          switch (instantiation_gate(~info_map, ~ctx, instantiations)) {
          | Error(var) =>
            let equality_name =
              exp_to_equality_name(equality) |> Option.value(~default="?");
            (
              Some(inc),
              [
                ProofMark.PossiblyDivergentInstantiation({
                  equality: equality_name,
                  var,
                }),
              ],
              [],
            );
          | Ok(domain_conditions) =>
            let obligations =
              List.map(
                incur_obligation(~step, ~ctx, ~origin=id),
                dedup_conditions(condition_goals @ domain_conditions),
              );
            (Some(out), [], obligations);
          }
        /* Error recovery: pass the incoming through (see
         * `result_to_outgoing`). */
        | Error(m) => (Some(inc), [m], [])
        }
      };
    (
      outgoing,
      record(~marks, ~obligations, id, incoming, outgoing, ProofMap.empty),
    );
  | AlgebriteStep({at_idx, at_exp, with_exp}) =>
    let (outgoing, marks, obligations) =
      switch (incoming) {
      | None => (None, [ProofMark.MissingIncoming], [])
      | Some(inc) =>
        /* Float gate (§1.5): CAS field laws are false for IEEE floats
         * independent of any partiality story, so Float-typed rewrites
         * are refused outright. */
        if (is_float_typed(~info_map, at_exp)
            || is_float_typed(~info_map, with_exp)) {
          (Some(inc), [ProofMark.FloatAlgebrite], []);
        } else {
          switch (
            algebrite_step_outgoing_ast(~at_idx, ~at_exp, ~with_exp, inc)
          ) {
          | Ok(out) =>
            /* Domain scan of BOTH sides (§4.1): the CAS reasons in a
             * field; these obligations are what make that sound here.
             *
             * TODO(docs/prover-obligations.md §4.1): checker-side CAS
             * re-verification of the rewrite itself. The CAS lives in
             * the browser as window.Algebrite (the node test harness
             * has no CAS at all), so the equational content of the
             * step remains UI-trusted for now — only its domain
             * obligations are checked here. */
            let env = SemanticCtx.get_env(ctx);
            let conditions =
              dedup_conditions(
                DomainConditions.scan(at_exp |> Substitution.in_exp(env))
                @ DomainConditions.scan(with_exp |> Substitution.in_exp(env)),
              );
            (
              Some(out),
              [],
              List.map(
                incur_obligation(~step, ~ctx, ~origin=id),
                conditions,
              ),
            );
          | Error(m) => (Some(inc), [m], [])
          };
        }
      };
    (
      outgoing,
      record(~marks, ~obligations, id, incoming, outgoing, ProofMap.empty),
    );
  | EvalStep({at_idx, at_exp}) =>
    let (auto_incoming, auto_outgoing, outgoing, marks) =
      switch (incoming) {
      | None => ([], [], None, [ProofMark.MissingIncoming])
      | Some(inc) =>
        switch (
          eval_step_outgoing_ast(
            ~step,
            ~env=SemanticCtx.get_env(ctx),
            ~at_idx,
            ~at_exp,
            inc,
          )
        ) {
        | Ok({auto_incoming, auto_outgoing, outgoing}) => (
            auto_incoming,
            auto_outgoing,
            Some(outgoing),
            [],
          )
        | Error(mark) => ([], [], Some(inc), [mark])
        }
      };
    (
      outgoing,
      record(
        ~marks,
        ~auto_incoming,
        ~auto_outgoing,
        id,
        incoming,
        outgoing,
        ProofMap.empty,
      ),
    );
  | Forall(_pat, body) =>
    /* The proof's pattern nominally renames the binder; following the UI's
     * ForallStep we use the binder from the incoming goal. */
    let (body_incoming, ctx', binder_marks) =
      switch (incoming) {
      | None => (None, ctx, [ProofMark.MissingIncoming])
      | Some(_) =>
        switch (peel_binder(incoming)) {
        | Some((p, t, guard, inner)) =>
          let ctx' = SemanticCtx.add_from_pattern(ctx, p, t);
          /* A restricted binder's `where` guard becomes a hypothesis for
           * the sub-proof — free, sound intro (§2.2). */
          let ctx' =
            switch (guard) {
            | Some(g) =>
              let g = g |> Substitution.in_exp(SemanticCtx.get_env(ctx'));
              let (ctx'', _) = SemanticCtx.add_hypothesis(ctx', "where", g);
              ctx'';
            | None => ctx'
            };
          (Some(inner), ctx', []);
        /* Recovery: no binder to peel — mark it, but let the body keep
         * working on the goal as-is. */
        | None => (incoming, ctx, [ProofMark.ExpectedForallGoal])
        }
      };
    let (out_body, m) =
      check(~step, ~info_map, ~ctx=ctx', body_incoming, body);
    /* The forall is discharged when the body reduces the goal to `true`;
     * otherwise it passes the (outer, unpeeled) goal through — the
     * body's partial outgoing lives under the binder, so propagating it
     * to steps after the forall would be wrong. */
    let true_exp = Exp.temp(Atom(Bool(true)));
    let outgoing =
      switch (out_body) {
      | Some(e) when Exp.fast_equal(e, true_exp) => out_body
      | _ => incoming
      };
    (outgoing, record(~marks=binder_marks, id, incoming, outgoing, m));
  | Assume(e, body) =>
    /* Hypothesize `e` for the body's scope. Two readings, one form
     * (docs/prover-obligations.md §2.1):
     *
     * - Implication INTRO: if the incoming goal is `A ==> B` and the
     *   assumed exp equals A (alpha-equality via `Exp.fast_equal`), the
     *   antecedent is stripped — the body's incoming is B — and NO
     *   obligation is incurred: intro is unconditionally sound.
     * - Otherwise (assume-then-bake): the goal is unchanged and the step
     *   incurs an obligation to establish the assumption, run through
     *   discharge channels 1 (binder lookup) and 2 (closed evaluation).
     *
     * Assuming never changes the outgoing: it's the body's. */
    let hyp = e |> Substitution.in_exp(SemanticCtx.get_env(ctx));
    let intro_consequent =
      switch (incoming) {
      | Some(goal) =>
        switch (goal |> Exp.term_of) {
        | BinOp(Bool(Implies), a, b)
            when
              Exp.fast_equal(
                a |> Substitution.in_exp(SemanticCtx.get_env(ctx)),
                hyp,
              ) =>
          Some(b)
        | _ => None
        }
      | None => None
      };
    /* Channels run against the ENCLOSING scope's facts (before the new
     * hypothesis is added, so an assume never discharges itself). */
    let obligations =
      switch (intro_consequent) {
      | Some(_) => []
      | None => [
          Obligation.{
            origin: id,
            bindings: SemanticCtx.get_ctx(ctx).entries,
            goal: hyp,
            discharge: discharge_goal(~step, ~ctx, hyp),
          },
        ]
      };
    let body_incoming =
      switch (intro_consequent) {
      | Some(b) => Some(b)
      | None => incoming
      };
    let (ctx', _binding) = SemanticCtx.add_hypothesis(ctx, "assume", hyp);
    let (out_body, m) =
      check(~step, ~info_map, ~ctx=ctx', body_incoming, body);
    (out_body, record(~obligations, id, incoming, out_body, m));
  | Generalize(e, body) =>
    /* Re-quantify an already-peeled binder x (docs/prover-obligations.md,
     * Phase 4b): with incoming goal G, the body's incoming goal is
     * `forall x -> G` — or `forall x where g -> G` when x carries a
     * recoverable `where` restriction (below). The node's outgoing is
     * `true` ONLY when the body proves the re-quantified goal to literal
     * `true`. Soundness: `forall x -> G` denoting true entails G at the
     * ambient x; for a restricted binder, the guard's ambient instance
     * is exactly the `where` hypothesis that was installed when x was
     * peeled. (Plain-forall re-quantification of a restricted binder
     * would also be sound — it only STRENGTHENS the body's goal — just
     * needlessly unprovable, so we recover the restriction.)
     *
     * Capture soundness — the critical piece: inside the body, every
     * fact whose statement mentions x (assume-hypotheses, case_eq, IHs,
     * the where guard itself) is about the OLD x and must become
     * unavailable under the new binder. We REMOVE those entries from the
     * body's semantic ctx — both the `ProofOf` ctx entries that drive
     * discharge-channel-1 `lookup_fact`, and the `ProofObject` env
     * entries that drive `ProofCtx.of_env` rule lookup — rather than
     * relying on the env-shadowing `is_captured` machinery (which only
     * covers rule lookup, and only fires once the body re-peels the new
     * binder). Removal covers both channels in one stroke. The mention
     * test is FREE occurrence, via the same co-context machinery
     * `ProofCtx.of_env` uses for `is_captured` (`ProofRule.get_coctx`):
     * a global lemma `forall x -> ...` whose x is bound by its own
     * binder does NOT mention the generalized x and stays available.
     * (Over-removal would still be sound — it only weakens the fact
     * set — but needlessly breaks citations of such lemmas.)
     *
     * Restriction travel: `where` guards are installed as hypotheses
     * under the base name "where" (`peel_binder` / `peel_stmt_binders`,
     * freshened by appending primes). We recover x's restriction as the
     * where-based hypotheses whose fact mentions x, conjoined with `&&`
     * if several match. Re-attaching an ambient hypothesis as a binder
     * guard is sound regardless of attribution: it weakens the
     * generalized statement, and its ambient instance is discharged by
     * that same hypothesis. */
    let base_ctx = SemanticCtx.get_ctx(ctx);
    let var_name =
      switch (unwrap_head(e) |> Exp.term_of) {
      | Var(x) when Ctx.lookup_var(base_ctx, x) != None => Some(x)
      | _ => None
      };
    switch (incoming, var_name) {
    | (None, _) =>
      let (_, m) = check(~step, ~info_map, ~ctx, None, body);
      (None, record(~marks=[ProofMark.MissingIncoming], id, None, None, m));
    | (Some(_), None) =>
      /* Recovery: the argument isn't a bare in-scope variable — mark it
       * and pass the goal through unchanged (the body still gets checked
       * against the un-generalized goal). */
      let (_, m) = check(~step, ~info_map, ~ctx, incoming, body);
      (
        incoming,
        record(
          ~marks=[ProofMark.MalformedGeneralize],
          id,
          incoming,
          incoming,
          m,
        ),
      );
    | (Some(goal), Some(x)) =>
      /* Does `x` occur FREE in the fact? Peel the fact's own binders into
       * a rule and ask statics for the co-context of its core (guards
       * included) — cf. ProofCtx.of_env's capture test. */
      let mentions_x = (fact: Exp.t) => {
        let rule = ProofRule.exp_to_rule(fact);
        let coctx =
          ProofRule.get_coctx(base_ctx, Typ.temp(Atom(Bool)), rule);
        CoCtx.has_any(coctx, [x]);
      };
      /* A hypothesis name with base "where": "where", "where'", ... (see
       * SemanticCtx.add_entry_free_name / Var.next_name). */
      let is_where_name = (name: string): bool =>
        String.length(name) >= 5
        && String.sub(name, 0, 5) == "where"
        && String.for_all(
             c => c == '\'',
             String.sub(name, 5, String.length(name) - 5),
           );
      let guards =
        Ctx.get_var_entries(base_ctx)
        |> List.filter_map((ve: Ctx.var_entry) =>
             switch (Typ.term_of(ve.typ)) {
             | ProofOf(fact) when is_where_name(ve.name) && mentions_x(fact) =>
               Some(fact)
             | _ => None
             }
           );
      let binder = Pat.fresh(Var(x));
      let body_goal =
        switch (guards) {
        | [] => Exp.fresh(Forall(binder, goal))
        | [g, ...gs] =>
          let guard =
            List.fold_left(
              (acc, g') => Exp.fresh(BinOp(Bool(And), acc, g')),
              g,
              gs,
            );
          Exp.fresh(ForallWhere(binder, guard, goal));
        };
      /* Capture: strip every fact mentioning x from the body's scope.
       * x's own binder entry stays — it is inert once no fact mentions
       * it (its env binding is the identity `x ↦ Var(x)`), and the
       * body's re-peel of the new binder rebinds it anyway. */
      let ctx' =
        SemanticCtx.of_ctx_and_env(
          {
            ...base_ctx,
            entries:
              List.filter(
                (entry: Ctx.entry) =>
                  switch (entry) {
                  | VarEntry({typ, _}) =>
                    switch (Typ.term_of(typ)) {
                    | ProofOf(fact) => !mentions_x(fact)
                    | _ => true
                    }
                  | _ => true
                  },
                base_ctx.entries,
              ),
          },
          Environment.filter(
            (_, v) =>
              switch (Exp.term_of(v)) {
              | Grammar.ProofObject(fact) => !mentions_x(fact)
              | _ => true
              },
            SemanticCtx.get_env(ctx),
          ),
        );
      let (out_body, m) =
        check(~step, ~info_map, ~ctx=ctx', Some(body_goal), body);
      /* Discharged when the body reduces the re-quantified goal to
       * literal `true`; otherwise the outer goal passes through (the
       * body's partial outgoing lives under the new binder — cf. the
       * Forall case). */
      let true_exp = Exp.temp(Atom(Bool(true)));
      let outgoing =
        switch (out_body) {
        | Some(e) when Exp.fast_equal(e, true_exp) => out_body
        | _ => incoming
        };
      (outgoing, record(id, incoming, outgoing, m));
    };
  | Revert(e, body) =>
    /* Cash an in-scope fact back into the goal — the symmetric partner of
     * assume-intro (docs/prover-obligations.md, Phase 4c). With incoming
     * goal `G` and an in-scope fact `F` whose statement matches the
     * (env-substituted) argument, the body's incoming goal is `F ==> G`.
     *
     * Soundness AND completeness, hence no obligation: `F` holds in this
     * scope, so under the Kleene reading of §1.3 `(F ==> G)` denotes
     * exactly what `G` denotes. Nothing is given away and nothing is
     * assumed; the step only MOVES a fact from the context into the goal,
     * where the eval/rewrite machinery can compute with it.
     *
     * The fact is NOT removed from scope: it stays citable (both as a
     * discharge-channel-1 fact and — via the bare-boolean reading — as a
     * rewrite rule), which is what makes the ex-falso idiom work:
     * `revert` the contradictory fact, then rewrite it with the OTHER
     * facts (e.g. a `case_eq`) until the antecedent evaluates to `false`,
     * at which point `false ==> G` evaluates to `true`.
     *
     * Matching is the same dumb `Exp.fast_equal` lookup as channel 1
     * (§4.2–4.3): the written expression must name the fact as it stands.
     * No match is recovery, not refusal — mark and pass the goal
     * through. */
    let fact = e |> Substitution.in_exp(SemanticCtx.get_env(ctx));
    switch (incoming) {
    | None =>
      let (_, m) = check(~step, ~info_map, ~ctx, None, body);
      (None, record(~marks=[ProofMark.MissingIncoming], id, None, None, m));
    | Some(goal) =>
      switch (lookup_fact(ctx, fact)) {
      | None =>
        let (_, m) = check(~step, ~info_map, ~ctx, incoming, body);
        (
          incoming,
          record(
            ~marks=[ProofMark.UnknownFactReverted],
            id,
            incoming,
            incoming,
            m,
          ),
        );
      | Some(_fact_id) =>
        let body_incoming =
          Some(Exp.fresh(BinOp(Bool(Implies), fact, goal)));
        let (out_body, m) =
          check(~step, ~info_map, ~ctx, body_incoming, body);
        /* The body works on `F ==> G`, which is not a sub-expression of
         * `G`, so a partial outgoing must not leak past this node: only a
         * literal `true` discharges, anything else passes `G` through
         * (cf. the Forall / Generalize cases). */
        let true_exp = Exp.temp(Atom(Bool(true)));
        let outgoing =
          switch (out_body) {
          | Some(o) when Exp.fast_equal(o, true_exp) => out_body
          | _ => incoming
          };
        (outgoing, record(id, incoming, outgoing, m));
      }
    };
  | Induction(scrut, cases) =>
    /* Split/induction gate (§4.1). Ordinary structural induction — a
     * bare quantified-variable scrutinee — emits nothing (quantifiers
     * range over total values). A COMPUTED scrutinee is the bool-split
     * case: within a branch its `case_eq` is a genuine symmetric
     * equation only if the scrutinee is defined and terminating (§3.3),
     * so refuse possibly-divergent scrutinees and incur the domain
     * scan's conditions on this node. */
    let scrut_sub = scrut |> Substitution.in_exp(SemanticCtx.get_env(ctx));
    let is_bare_var =
      switch (unwrap_head(scrut_sub) |> Exp.term_of) {
      | Var(_) => true
      | _ => false
      };
    let (gate_marks, gate_obligations) =
      if (is_bare_var) {
        ([], []);
      } else {
        let gate_marks =
          switch (
            Totality.check(
              ~info_map,
              ~ctx=SemanticCtx.get_ctx(ctx),
              scrut_sub,
            )
          ) {
          | Ok () => []
          | Error(_) => [ProofMark.PossiblyDivergentScrutinee]
          };
        let gate_obligations =
          DomainConditions.scan(scrut_sub)
          |> List.map(incur_obligation(~step, ~ctx, ~origin=id));
        (gate_marks, gate_obligations);
      };
    let (out, marks, m) =
      check_induction(~step, ~info_map, ~ctx, ~incoming, ~scrut, ~cases);
    (
      out,
      record(
        ~marks=gate_marks @ marks,
        ~obligations=gate_obligations,
        id,
        incoming,
        out,
        m,
      ),
    );
  };
}
/* Induction on `scrut` in the incoming goal.
 *
 * For each (pat, body) case:
 *   1. Substitute `scrut` with `pat` in the goal → case-incoming.
 *   2. Extend ctx with the pat's bindings and a case-equality hypothesis
 *      `scrut == pat`, plus inductive hypotheses for any sub-patterns of
 *      the same type as scrut.
 *   3. Recurse on body with the case-incoming.
 *
 * If exhaustive (by Coverage.check) and every case proves its goal to
 * `true`, the induction's outgoing is `true`. Otherwise outgoing is None.
 * Returns a list of proof marks for the induction node itself
 * (empty-cases / not-exhaustive — per-case failures surface as children).
 */
and check_induction =
    (
      ~step: step_fn,
      ~info_map: Statics.Map.t,
      ~ctx: SemanticCtx.t,
      ~incoming: option(Exp.t),
      ~scrut: Exp.t,
      ~cases: list((Pat.t, Proof.t)),
    )
    : (option(Exp.t), list(ProofMark.t), ProofMap.t) => {
  /* Resolve scrut's type via info_map (added by statics when elaborating
   * the theorem). Fall back to Unknown if not present. */
  let scrut_id = Exp.rep_id(scrut);
  let scrut_ty =
    switch (Statics.Map.ty_of(scrut_id, info_map)) {
    | Some(ty) => ty
    | None => Typ.fresh(Unknown(Internal))
    };
  let scrut_co_ctx =
    switch (Statics.Map.lookup_exp(scrut_id, info_map)) {
    | Some({co_ctx, _}) => co_ctx
    | None => CoCtx.empty
    };
  /* Walk each case, collecting body outgoings and the accumulated
   * proof-map entries. */
  let (outgoings, pat_constraints, m_acc) =
    List.fold_left(
      ((outs, constraints, m), (pat, body)) => {
        /* 1. Replace scrut with pat in the goal (if any). */
        let case_incoming =
          switch (incoming) {
          | None => None
          | Some(goal) =>
            let added_vars = pat |> Pat.bindings |> Binding.variable_names;
            ProofHacks.replace_exp(
              info_map,
              scrut,
              scrut_co_ctx,
              pat |> ProofHacks.pat_to_exp,
              pat |> Pat.bindings |> CoCtx.of_bindings,
              added_vars,
              goal,
            );
          };
        /* 2. Extend ctx with pat bindings. */
        let ctx' = SemanticCtx.add_from_pattern(ctx, pat, scrut_ty);
        /* 2a. Add the case-equality hypothesis scrut == pat, unless it's
         *     captured by the newly-added variables. */
        let added_vars = pat |> Pat.bindings |> Binding.variable_names;
        let is_case_eq_captured = CoCtx.has_any(scrut_co_ctx, added_vars);
        let case_eq =
          is_case_eq_captured
            ? None
            : Some(
                Exp.fresh(
                  BinOp(Poly(Equals), scrut, pat |> ProofHacks.pat_to_exp),
                )
                |> Substitution.in_exp(SemanticCtx.get_env(ctx')),
              );
        let ctx' =
          switch (case_eq) {
          | Some(eq) =>
            let (ctx'', _) = SemanticCtx.add_hypothesis(ctx', "case_eq", eq);
            ctx'';
          | None => ctx'
          };
        /* 2b. Add inductive hypotheses for sub-patterns of scrut's type. */
        let ihs =
          ProofHacks.get_inductive_hypotheses(
            ~tyctx=SemanticCtx.get_ctx(ctx),
            info_map,
            scrut_ty,
            pat,
          )
          |> List.filter_map(h =>
               ProofHacks.replace_exp(
                 info_map,
                 scrut,
                 scrut_co_ctx,
                 h |> ProofHacks.pat_to_exp,
                 h |> Pat.bindings |> CoCtx.of_bindings,
                 added_vars,
                 incoming |> Option.value(~default=Exp.fresh(EmptyHole)),
               )
             );
        /* Store IHs env-substituted, like `assume` hypotheses and
         * `case_eq` above: rule exps are matched against env-substituted
         * targets (MatchExp.match_exp substitutes the exp side), so a
         * fact citing a definition by bare name (`Var ra`) could never
         * match its inlined value. */
        let ihs =
          List.map(
            ih => ih |> Substitution.in_exp(SemanticCtx.get_env(ctx')),
            ihs,
          );
        let ctx' =
          List.fold_left(
            (c, ih) => {
              let (c', _) = SemanticCtx.add_hypothesis(c, "ih", ih);
              c';
            },
            ctx',
            ihs,
          );
        /* 3. Recurse on body. A case only counts as discharged if its
         * subtree is also mark-free: with error recovery, `true` can be
         * passed through a broken step, so the outgoing alone no longer
         * proves the case. */
        let (out_body, m_body) =
          check(~step, ~info_map, ~ctx=ctx', case_incoming, body);
        let case_clean = ProofMap.error_ids(m_body) == [];
        /* Collect the pattern constraint for later coverage check. */
        let constraint_ =
          switch (Statics.Map.lookup_pat(Pat.rep_id(pat), info_map)) {
          | Some(info_pat) => Some(Info.pat_constraint(info_pat))
          | None => None
          };
        (
          outs @ [(out_body, case_clean)],
          constraints @ [constraint_],
          ProofMap.union(m, m_body),
        );
      },
      ([], [], ProofMap.empty),
      cases,
    );
  /* Exhaustiveness: check the collected constraints cover scrut's type. */
  let is_exhaustive = {
    let constraints = List.filter_map(Fun.id, pat_constraints);
    Coverage.check(
      constraints,
      Typ.normalize(SemanticCtx.get_ctx(ctx), scrut_ty),
    ).
      exhaustiveness
    == Exhaustive;
  };
  let true_exp = Exp.temp(Atom(Bool(true)));
  let all_true =
    outgoings
    |> List.for_all(
         fun
         | (Some(e), clean) => clean && Exp.fast_equal(e, true_exp)
         | (None, _) => false,
       );
  /* Structural marks on the induction node itself. Per-case failures
   * already surface as marks on their own sub-term ids. */
  let marks =
    (List.length(cases) == 0 ? [ProofMark.InductionEmptyCases] : [])
    @ (
      !is_exhaustive && List.length(cases) > 0
        ? [ProofMark.InductionNotExhaustive] : []
    );
  /* Discharged inductions produce `true`; anything else passes the
   * outer goal through (recovery — see `result_to_outgoing`). */
  let outgoing =
    is_exhaustive && all_true && List.length(cases) > 0
      ? Some(true_exp) : incoming;
  (outgoing, marks, m_acc);
};
