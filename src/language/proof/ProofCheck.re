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
      ~substitutions: list((string, Exp.t))=[],
      ~marks: list(ProofMark.t)=[],
      ~obligations: list(Obligation.t)=[],
      (),
    )
    : ProofMap.entry => {
  incoming,
  auto_incoming,
  auto_outgoing,
  outgoing,
  substitutions,
  marks,
  obligations,
};

/* Record an entry at `id`, merged into accumulated map `m`. */
let record =
    (
      ~marks: list(ProofMark.t)=[],
      ~auto_incoming: list((string, Exp.t))=[],
      ~auto_outgoing: list((Exp.t, string))=[],
      ~substitutions: list((string, Exp.t))=[],
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
      ~substitutions,
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

/* The name an `as <name>` clause asks for, or the form's fixed base name
 * when there is no clause (or the clause's pattern is not a bare
 * identifier — a hole or a compound pattern names nothing, and falling
 * back to the base name keeps the step working rather than silently
 * dropping the hypothesis). Fixed base names SHADOW; see
 * `SemanticCtx.hypothesis_name`. */
let as_name_or = (~default: Var.t, as_name: option(Pat.t)): Var.t =>
  switch (as_name) {
  /* Read the name through `Pat.bound_vars` rather than matching `Var`
     directly: the checker runs on the ELABORATED proof, where a bare
     identifier pattern can arrive wrapped (`Parens`, `Asc`), and a
     structural match on `Var` would silently fall back to the base name.
     Exactly one bound variable is required — a hole or a compound
     pattern names nothing, and falling back keeps the step working
     rather than dropping the hypothesis. */
  | Some(x) =>
    switch (Pat.bound_vars(x)) {
    | [h] => h
    | _ => default
    }
  | None => default
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
 * (UnderdeterminedInstantiation, v1 behavior per §4.1) — UNLESS the
 * citation supplies them explicitly: `~instantiation` is the Phase-4d
 * `with <var> = <exp>` clause, seeded into the match context BEFORE
 * matching, so the underdetermined check only looks at what remains
 * unresolved AFTER seeding. A `<var>` that is not a binder of the cited
 * rule is `UnknownInstantiationVar`. The seeded binding is returned in
 * the instantiation list like any matched one, so the Phase-3a gates
 * (totality refusal + domain scan) apply to it unchanged. */
let axiom_step_outgoing_result =
    (
      ~info_map: Statics.Map.t,
      ~env: Environment.t(Exp.t),
      ~ctx: Ctx.t,
      ~at_idx: int,
      ~at_exp: Exp.t,
      ~direction: Direction.t,
      ~equality: string,
      ~instantiation: option((Var.t, Exp.t))=None,
      incoming: Exp.t,
    )
    : result((Exp.t, list(Exp.t), list((Var.t, Exp.t))), ProofMark.t) => {
  let proof_ctx = ProofCtx.of_theorem_ctx(~builtins=Axioms.v, ctx);
  switch (ProofCtx.lookup_rule(equality, proof_ctx)) {
  | None => Error(UnknownEquality(equality))
  | Some(rule) =>
    /* A cited rule with a bare-boolean conclusion `P` reads as the
     * equation `P == true` (docs/prover-obligations.md §2.1). Applies to
     * every rule uniformly — hypotheses, user theorems and the built-in
     * closure library alike — which is why `Axioms.re` states those
     * lemmas as bare booleans. `~info_map` is the type evidence the gate
     * needs for a conclusion whose shape does not settle it (a `where`
     * guard that is a plain variable, a Bool-typed application). */
    let rule =
      ProofRule.with_bool_fact_reading(~info_map=Some(info_map), rule);
    /* Phase 4d: seed the explicit instantiation into the initial match
     * context. The supplied expression is env-substituted first, exactly
     * like every other expression the checker computes with. */
    let seeded =
      switch (instantiation) {
      | None => Ok(None)
      | Some((var, inst)) =>
        let inst = inst |> Substitution.in_exp(env);
        switch (
          ProofRule.seed_binding(
            var,
            inst,
            ProofRule.get_empty_bindings(rule.bindings),
          )
        ) {
        | Some(bindings) => Ok(Some(bindings))
        | None =>
          Error(
            ProofMark.UnknownInstantiationVar({
              equality,
              var,
            }),
          )
        };
      };
    switch (seeded) {
    | Error(m) => Error(m)
    | Ok(bindings) =>
      switch (
        ProofHacks.nth_exp_env_target(
          ~info_map,
          ~env,
          at_exp,
          at_idx,
          incoming,
        )
      ) {
      | None =>
        Error(
          PatternNotFound({
            at_exp,
            at_idx,
          }),
        )
      | Some(e) =>
        /* `~reverse=true`: this is an EXPLICIT citation, which names the
         * direction and the occurrence, so the bare-boolean reading's
         * `true |-> P` direction is offered here even though rule
         * discovery suppresses it (`ProofRule.can_eq_inst`). */
        let (l, r) =
          ProofRule.can_eq_inst(
            ~info_map,
            ~env,
            ~bindings?,
            ~reverse=true,
            rule,
            e,
          );
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
      }
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
      ~instantiation: option((Exp.t, Exp.t))=None,
      incoming: Exp.t,
    )
    : result((Exp.t, list(Exp.t), list((Var.t, Exp.t))), ProofMark.t) =>
  switch (exp_to_int(at_idx)) {
  | None => Error(MalformedIndex)
  | Some(idx) =>
    switch (exp_to_equality_name(equality)) {
    | None => Error(MalformedEqualityName)
    | Some(name) =>
      /* The `with <var> = ...` slot must name a binder; a non-variable
       * there is reported the same way an unknown binder name is. */
      switch (instantiation) {
      | Some((var, _)) when exp_to_equality_name(var) == None =>
        Error(
          UnknownInstantiationVar({
            equality: name,
            var: "?",
          }),
        )
      | _ =>
        let instantiation =
          Option.map(
            ((var, inst)) =>
              (
                exp_to_equality_name(var) |> Option.value(~default="?"),
                inst,
              ),
            instantiation,
          );
        axiom_step_outgoing_result(
          ~info_map,
          ~env,
          ~ctx,
          ~at_idx=idx,
          ~at_exp,
          ~direction,
          ~equality=name,
          ~instantiation,
          incoming,
        );
      }
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

/* Rewrite (`AlgebriteStep`) locates its target the SAME way eval/axiom
 * steps do: env-aware (`nth_exp_env_target`, not `nth_exp_target`).
 *
 * The `at <n>` index is written by the stepper overlay, which computes
 * it with `ProofHacks.exp_idx` over the goal AS DISPLAYED — and the
 * displayed goal is env-inlined, so recursive bindings appear as their
 * `FixF` bodies and contribute extra occurrences. Counting on the
 * un-substituted goal here would number occurrences differently from
 * the UI that produced the number, so a click on the `0` of
 * `fold_left(_, sum, 0)` (written as `at 2`, since `sum`'s inlined body
 * contributes two earlier `0`s) reported PatternNotFound.
 *
 * Counting therefore happens on the substituted tree and the match is
 * mapped back to an id in the original goal, exactly as
 * `nth_exp_env`/`nth_exp_env_pat` already do for eval. An occurrence
 * that exists only inside an inlined body has no id in the original
 * goal, cannot be rewritten there, and still reports PatternNotFound —
 * which is the honest answer, not a numbering bug. */
let algebrite_step_outgoing =
    (
      ~info_map: Statics.Map.t=Statics.Map.empty,
      ~env: Environment.t(Exp.t)=Environment.empty,
      ~at_idx: int,
      ~at_exp: Exp.t,
      ~with_exp: Exp.t,
      incoming: Exp.t,
    )
    : option(Exp.t) =>
  switch (
    ProofHacks.nth_exp_env_target(~info_map, ~env, at_exp, at_idx, incoming)
  ) {
  | None => None
  | Some(e) =>
    Some(ProofHacks.replace_exp_id(Exp.rep_id(e), incoming, with_exp))
  };

let algebrite_step_outgoing_result =
    (
      ~info_map: Statics.Map.t=Statics.Map.empty,
      ~env: Environment.t(Exp.t)=Environment.empty,
      ~at_idx: int,
      ~at_exp: Exp.t,
      ~with_exp: Exp.t,
      incoming: Exp.t,
    )
    : result(Exp.t, ProofMark.t) =>
  switch (
    ProofHacks.nth_exp_env_target(~info_map, ~env, at_exp, at_idx, incoming)
  ) {
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
    (
      ~info_map: Statics.Map.t=Statics.Map.empty,
      ~env: Environment.t(Exp.t)=Environment.empty,
      ~at_idx: Exp.t,
      ~at_exp: Exp.t,
      ~with_exp: Exp.t,
      incoming: Exp.t,
    )
    : result(Exp.t, ProofMark.t) =>
  switch (exp_to_int(at_idx)) {
  | None => Error(MalformedIndex)
  | Some(idx) =>
    algebrite_step_outgoing_result(
      ~info_map,
      ~env,
      ~at_idx=idx,
      ~at_exp,
      ~with_exp,
      incoming,
    )
  };

/* Canonical eval-step outgoing: locate the `at_idx`-th occurrence of
 * `at_exp` in `incoming`, apply a single dynamic evaluation step to it
 * via the injected `step_fn`, and splice the result back in. */
let eval_step_outgoing =
    (
      ~step: step_fn,
      ~info_map: Statics.Map.t=Statics.Map.empty,
      ~env: Environment.t(Exp.t),
      ~at_idx: int,
      ~at_exp: Exp.t,
      incoming: Exp.t,
    )
    : option(Exp.t) =>
  switch (
    ProofHacks.nth_exp_env_target(~info_map, ~env, at_exp, at_idx, incoming)
  ) {
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
      ~info_map: Statics.Map.t=Statics.Map.empty,
      ~env: Environment.t(Exp.t),
      ~at_idx: int,
      ~at_exp: Exp.t,
      incoming: Exp.t,
    )
    : result(step_result, ProofMark.t) =>
  switch (
    ProofHacks.nth_exp_env_target(~info_map, ~env, at_exp, at_idx, incoming)
  ) {
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
      ~info_map: Statics.Map.t=Statics.Map.empty,
      ~env: Environment.t(Exp.t),
      ~at_idx: Exp.t,
      ~at_exp: Exp.t,
      incoming: Exp.t,
    )
    : result(step_result, ProofMark.t) =>
  switch (exp_to_int(at_idx)) {
  | None => Error(MalformedIndex)
  | Some(idx) =>
    eval_step_outgoing_result(
      ~step,
      ~info_map,
      ~env,
      ~at_idx=idx,
      ~at_exp,
      incoming,
    )
  };

/* Discharge channel 1 (binder lookup): search the facts visible in the
 * given scope — hypotheses added via `SemanticCtx.add_hypothesis`
 * (`assume`, `case_eq`, `ih`, ...) live in the ctx's THEOREM NAMESPACE
 * (`Ctx.TheoremEntry`) — for one that syntactically covers `goal`. The
 * comparison is deliberately a dumb `Exp.fast_equal`: the design mandates
 * a transparent, lookup-only discharge relation
 * (docs/prover-obligations.md §4.2–4.3). Returns the covering fact's
 * stable entry id. */
/* The one sanctioned piece of slack in the discharge relation
 * (docs/prover-obligations.md §4.3: "a small, NAMED, documented normal
 * form ... e.g. comparison symmetry + constant folding, nothing else").
 *
 * NORMAL FORM: `P == true` and `true == P` normalise to `P`, recursively.
 *
 * It exists because §3.3's "split" exit depends on it: `induction P | true
 * => ... | false => ...` installs the case equation `P == true` as the
 * true branch's hypothesis, and the obligation the branch has to retire is
 * the bare `P`. Without this the split action emits correct text that
 * discharges nothing.
 *
 * Sound in the flat domain of §1.1: a hypothesis is a fact ASSERTED to
 * denote `true`, and `P == true` denotes `true` only when `P` does (if `P`
 * is `false` the equation is `false`; if `P` is `err` or `⊥` the equation
 * is `err`/`⊥`). So `P == true` in scope entails `P`. Note the direction:
 * the folding is applied to BOTH sides, which is fine — it is an
 * equivalence, not a weakening. Nothing else is normalised: no symmetry,
 * no arithmetic, no entailment. */
let rec strip_eq_true = (e: Exp.t): Exp.t => {
  let is_true = (x: Exp.t) =>
    Exp.fast_equal(x, Exp.temp(Atom(Bool(true))));
  switch (Exp.term_of(e)) {
  | BinOp(Poly(Equals), a, b) when is_true(b) => strip_eq_true(a)
  | BinOp(Poly(Equals), a, b) when is_true(a) => strip_eq_true(b)
  | Parens(e') => strip_eq_true(e')
  | _ => e
  };
};

let lookup_fact = (ctx: SemanticCtx.t, goal: Exp.t): option(Id.t) => {
  let goal_nf = strip_eq_true(goal);
  SemanticCtx.facts(ctx)
  |> List.find_map(((_name, id, fact)) =>
       Exp.fast_equal(fact, goal)
       || Exp.fast_equal(strip_eq_true(fact), goal_nf)
         ? Some(id) : None
     );
};

/* --- Discharge channel 2: closed evaluation (§4.2) -------------------
 *
 * Ground obligations (`2 != 0`) just run: if the goal has no free
 * variables after env substitution, evaluate it by iterating the
 * injected single-step function to a fixpoint (with a generous fuel
 * bound, since `step_fn` exposes no termination signal) and check for
 * the literal `true`. Open goals are NEVER evaluated. */

let closed_eval_fuel = 1000;

/* Closedness via the co-context machinery (cf. `ProofRule.occurs_free_any`):
 * run statics on the goal against an empty ctx; an empty co-context
 * means no free variable occurrences. */
let co_ctx_of_closed_run = (goal: Exp.t): CoCtx.t => {
  let (statics, _) =
    Statics.mk(~ana=Typ.temp(Atom(Bool)), CoreSettings.on, Ctx.empty, goal);
  switch (Statics.Map.lookup_exp(Exp.rep_id(goal), statics)) {
  | Some(info) => Info.exp_co_ctx(info)
  | None => CoCtx.empty
  };
};

let is_closed = (goal: Exp.t): bool => co_ctx_of_closed_run(goal) == [];

/* D66: automatic evaluation never runs a fixpoint. After env-inlining,
 * every recursive function a goal can call is a `FixF` node in it
 * (written in the goal, a user closure's, or a builtin's), so a `FixF`
 * anywhere in a closed goal keeps channel 2 from taking any step: the
 * goal stays Pending. Channel 1 (fact lookup) and explicit steps (`eval`,
 * `contradiction`) are unaffected. (Reused verbatim from bf2f20de.) */
let has_fixpoint = (goal: Exp.t): bool => {
  let found = ref(false);
  let _: Exp.t =
    Exp.map_term(
      ~f_exp=
        (continue, e) =>
          switch (Exp.term_of(e)) {
          | FixF(_) =>
            found := true;
            e;
          | _ => continue(e)
          },
      goal,
    );
  found^;
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
    if (is_closed(goal) && !has_fixpoint(goal)) {
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

/* --- `contradiction`: ex falso as a primitive (Phase 4e) -------------
 *
 * `contradiction F end` / `contradiction F with x = e end` closes ANY
 * goal by exhibiting that the in-scope fact `F` is false under the rest
 * of the scope's knowledge.
 *
 * SOUNDNESS. `F` being in scope means `F` denotes `true` in this
 * branch's semantics. If `F`, rewritten by an equation the branch also
 * knows, denotes `false`, then the branch's hypotheses are jointly
 * unsatisfiable and the branch is vacuous — so concluding `true` is the
 * Kleene reading of ex falso quodlibet (§1.3). This is EXACTLY what the
 * manual revert-dance already licenses (`revert F => ...rewrite...;
 * eval false ==> G at 0 end`); the primitive just does it in one step.
 *
 * WHAT IS TRUSTED. Only two things, both already trusted elsewhere:
 *   1. the step's explicit `with` rewrite, which the user WRITES and the
 *      checker only verifies against an in-scope equation (see
 *      `contradiction_substitution` below) — no search, no solver; and
 *   2. `eval_via_step`, i.e. the injected dynamics single-step run to a
 *      fixpoint under the same fuel bound as discharge channel 2. Each
 *      such step is denotation-preserving for EVERY assignment of the
 *      free variables (that is why `eval` steps are the one free step,
 *      §5), so reaching the literal `false` in finitely many steps means
 *      `F` denotes `false` here. Nothing new to gate.
 *
 * The soundness argument is UNCHANGED by the 2026-08-21 explicitness
 * rework: substituting `e` for `x` under a known `x == e` is
 * denotation-preserving here whether the checker found that equation or
 * the user named it. What changed is only WHO chooses the rewrite.
 *
 * NOTE on openness: unlike channel 2, the term is NOT required to be
 * closed. Channel 2 concludes a positive fact (`the obligation holds`)
 * and keeps closedness as its human-first boundary; here the conclusion
 * rides entirely on the eval steps' denotation-preservation, which does
 * not need closedness. The load-bearing metatheory case is open:
 * `is_value(TmVar(m0)) == true` falsifies for every `m0` because the
 * match only inspects the constructor. An open term that gets STUCK
 * instead simply fails to reach `false` and is marked (below).
 */

/* The step's ONE trusted rewrite: the user's explicit
 * `with <var> = <exp>` clause, VERIFIED against the facts in scope.
 *
 * Rework of 2026-08-21 (user decision): the step used to HARVEST every
 * in-scope variable equation and substitute to a fixpoint. That is
 * invisible search-like behavior in a human-first calculus — "automation
 * may propose; only visible steps dispose" (docs/prover-obligations.md
 * §4.3) — so the search is gone. Nothing is discovered: the user names
 * the rewrite, and the checker's only job is to confirm the branch
 * already knows it.
 *
 * The check is EXACTLY the lookup the harvest used to read equations out
 * of: `<var> == <exp>` (or `<exp> == <var>`, since `==` is symmetric)
 * must `Exp.fast_equal` an in-scope fact — an entry of the theorem
 * namespace (`Ctx.TheoremEntry`), which is what a split's `case_eq`, an
 * `assume` and an installed IH all are. Both sides are env-substituted first, to
 * bring the user's vocabulary into the same inlined form the stored
 * facts have (the spelled-out path of `cited_fact` below).
 *
 * `<var>` must be a bare variable, since that is what a substitution can
 * act on. Anything unverifiable — no such equation, or a non-variable
 * left side — is refused with `ContradictionSubstitutionUnverified`
 * rather than trusted.
 *
 * There is no fixpoint, no occurs check and no round bound: one binding
 * is applied once. Nor is the cited fact excluded from its own licensing
 * equation any more — `contradiction n == 1 with n = 1 end` may rewrite
 * the fact with itself, but that only ever lands on a tautology
 * (`1 == 1`), which fails the `false` test below. The old exclusion rule
 * existed to stop the harvest laundering an equation into `e == e`; with
 * the user naming the rewrite there is nothing to launder.
 *
 * Returns the rewritten fact and the receipt (the binding actually
 * applied, empty for the plain no-clause form). */
let contradiction_substitution =
    (
      ~env: Environment.t(Exp.t),
      ~ctx: SemanticCtx.t,
      ~instantiation: option((Exp.t, Exp.t)),
      fact: Exp.t,
    )
    : result((Exp.t, list((Var.t, Exp.t))), ProofMark.t) =>
  switch (instantiation) {
  | None => Ok((fact, []))
  | Some((var, inst)) =>
    let var_sub = var |> Substitution.in_exp(env);
    let inst_sub = inst |> Substitution.in_exp(env);
    let equation = (l, r) => Exp.temp(BinOp(Poly(Equals), l, r));
    let licensed =
      Option.is_some(lookup_fact(ctx, equation(var_sub, inst_sub)))
      || Option.is_some(lookup_fact(ctx, equation(inst_sub, var_sub)));
    switch (unwrap_head(var_sub) |> Exp.term_of) {
    | Var(x) when licensed =>
      let rewritten =
        fact
        |> Substitution.in_exp(
             Environment.extend(Environment.empty, (x, inst_sub)),
           );
      Ok((rewritten, [(x, inst_sub)]));
    | Var(x) =>
      Error(ProofMark.ContradictionSubstitutionUnverified({var: x}))
    | _ => Error(ProofMark.ContradictionSubstitutionUnverified({var: "?"}))
    };
  };

/* Build the obligation record for one incurred condition at step `id`,
 * running the discharge channels. */
let incur_obligation =
    (~step: step_fn, ~ctx: SemanticCtx.t, ~origin: Id.t, goal: Exp.t)
    : Obligation.t => {
  /* `display_goal` is the incoming term as written; `goal` is it with the
   * environment substituted in (the semantic one the channels run on).
   * See Obligation.re for why both are kept. */
  let display_goal = goal;
  let goal = goal |> Substitution.in_exp(SemanticCtx.get_env(ctx));
  Obligation.{
    origin,
    bindings: SemanticCtx.get_ctx(ctx).entries,
    goal,
    display_goal,
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

/* The `&&`-conjuncts of a guard, parens-transparently. A guard that is
 * not a conjunction is its own single conjunct. */
let rec guard_conjuncts = (g: Exp.t): list(Exp.t) =>
  switch (g |> Exp.term_of) {
  | Parens(g1) => guard_conjuncts(g1)
  | BinOp(Bool(And), a, b) => guard_conjuncts(a) @ guard_conjuncts(b)
  | _ => [g]
  };

/* Install a binder's `where` restriction as a hypothesis (a free, sound
 * intro; docs/prover-obligations.md §2.2). The ONE place this is done, so
 * the three peeling sites — `peel_stmt_binders`, the `Forall` proof arm,
 * and the UI's `ForallStep` — cannot drift apart.
 *
 * A CONJUNCTIVE guard additionally installs each conjunct as its own
 * fact. This is fact installation, not slack in the discharge relation:
 * `lookup_fact` stays a dumb `fast_equal` (+ the documented
 * `strip_eq_true` normal form) and simply finds more facts. It is sound
 * in the flat domain of §1.1 — a hypothesis asserted to denote `true`
 * whose form is `a && b` entails both `a` and `b` under the Kleene
 * reading of §1.3 — and it is what `definition_obligations` has always
 * done for a `fun p where g` contract ("the guard `g` or one of its
 * `&&`-conjuncts"). Without it the (!) panel's float exit is broken
 * exactly when the binder already carries a guard, since
 * `ObligationsPanel.float_binder_exp` AND-extends rather than replaces.
 *
 * All entries go under ONE name (the whole guard's), and the whole guard
 * is installed LAST so that `Ctx.lookup_theorem` — which takes the most
 * recently added match — resolves the citable `where` to the guard the
 * user wrote, exactly as `Statics.proof_ctx_of_goal` recorded it. Sharing
 * the name is also what keeps later hypotheses' auto-names in step with
 * the statics: freshening is over the set of occupied names. */
let add_where_facts = (ctx: SemanticCtx.t, g: Exp.t): SemanticCtx.t => {
  let g = g |> Substitution.in_exp(SemanticCtx.get_env(ctx));
  let name = SemanticCtx.hypothesis_name(ctx, "where");
  let install = (ctx, fact) =>
    SemanticCtx.add_hypothesis_named(ctx, name, fact) |> fst;
  let ctx =
    switch (guard_conjuncts(g)) {
    | [_] => ctx /* not a conjunction: the guard is its own conjunct */
    | conjuncts => List.fold_left(install, ctx, conjuncts)
    };
  install(ctx, g);
};

/* `add_where_facts` for the guard a binder PATTERN carries, if any. */
let add_pat_where_facts = (ctx: SemanticCtx.t, p: Pat.t): SemanticCtx.t =>
  switch (Exp.split_pat_guard(p) |> snd) {
  | None => ctx
  | Some(g) => add_where_facts(ctx, g)
  };

/* A coarse head-constructor key, used only to decide whether two case
 * arms could match the SAME value. `None` means "matches anything"
 * (variable / wildcard / hole), which overlaps everything. This is
 * deliberately syntactic and conservative: when in doubt, overlap. */
let rec pat_head_key = (p: Pat.t): option(string) =>
  switch (p |> Pat.term_of) {
  | Where(p, _)
  | StdNamed(p, _)
  | Parens(p)
  | Projector(_, p)
  | Asc(p, _)
  | TupLabel(_, p) => pat_head_key(p)
  | Constructor(c, _) => Some("ctr:" ++ c)
  | Ap(c, _) =>
    switch (Pat.ctr_name(c)) {
    | Some(c) => Some("ctr:" ++ c)
    | None => None
    }
  | Cons(_, _) => Some("cons")
  | ListLit(ps) => Some("list:" ++ string_of_int(List.length(ps)))
  | Tuple(ps) => Some("tuple:" ++ string_of_int(List.length(ps)))
  | Atom(a) => Some("atom:" ++ Atom.show(a))
  | Label(l) => Some("label:" ++ l)
  | Var(_)
  | Wild
  | EmptyHole
  | MultiHole(_)
  | Invalid(_)
  | ExplicitNonlabel => None
  };

/* Could `p1` and `p2` match the same value? Conservatively yes unless
 * their head constructors are both known and different. */
let pats_overlap = (p1: Pat.t, p2: Pat.t): bool =>
  switch (pat_head_key(p1), pat_head_key(p2)) {
  | (Some(k1), Some(k2)) => k1 == k2
  | _ => true
  };

/* Facts a case arm inherits from the arms ABOVE it: an arm is only
 * reached when every earlier arm failed, so an earlier GUARDED arm that
 * could have matched this value contributes `!g`. Constructor-disjoint
 * earlier arms contribute nothing -- `| Z =>` must not import the `S`
 * arm's negated guard.
 *
 * `!g` is installed whole (never conjunct-split: `!(a && b)` does not
 * entail `!a`), under the same `where` name as a positive guard. */
let add_previous_guard_negations =
    (ctx: SemanticCtx.t, prev_pats: list(Pat.t), pat: Pat.t): SemanticCtx.t =>
  List.fold_left(
    (ctx, prev) =>
      switch (Exp.split_pat_guard(prev)) {
      | (prev_base, Some(g)) when pats_overlap(prev_base, pat) =>
        add_where_facts(ctx, Exp.fresh(UnOp(Bool(Not), g)))
      | _ => ctx
      },
    ctx,
    prev_pats,
  );

/* --- funext: is this goal a function equality? ------------------------
 *
 * `funext x => body` applies exactly when the incoming goal is `l == r`
 * and the two sides are FUNCTIONS. The arrow requirement is load-bearing:
 * `==` between two LISTS of functions is an induction, not an
 * extensionality, so `Typ.has_fun` (which is true of `[a -> b]`) is the
 * wrong test. We want an arrow at the HEAD of the type.
 *
 * Evidence, in order:
 *   1. Statics: the recorded type of either side is an `Arrow`. This is
 *      the answer whenever statics reached the goal (the usual case in a
 *      theorem statement).
 *   2. Syntax: either side is literally a lambda (`fun` / `fun ... where`
 *      / a `Parens` around one). Covers goals statics left `Unknown`,
 *      and gives us a domain annotation when the lambda carries one.
 *   3. The semantic ctx / env: a bare variable naming a function
 *      definition. `inv == (fun x -> 1 / x)` has a lambda on the right,
 *      so this is mostly for the both-sides-named case.
 *
 * The returned type is the DOMAIN to bind `x` at — `Unknown(Internal)`
 * when we know it is a function but not at what type, which is the same
 * thing `peel_binder` does for a `Forall` binder. */
let rec arrow_domain_syntactic =
        (~env: Environment.t(Exp.t), e: Exp.t): option(Typ.t) =>
  switch (e |> Exp.term_of) {
  | Parens(inner) => arrow_domain_syntactic(~env, inner)
  /* The type field is the PARAMETER type (Statics elaborates
   * `Fun(p, e, Some(p.ty), n)`), i.e. already the domain. When it is
   * missing or unknown, the parameter's own written ascription is the
   * only other precise source; otherwise the domain is Unknown and the
   * D80 funext guard fails closed. Nothing is guessed from the body. */
  | Fun(p, _, field, _) =>
    let rec ascribed = (p: Pat.t) =>
      switch (p.term) {
      | Parens(p) => ascribed(p)
      | Asc(_, t) => Some(t)
      | _ => None
      };
    let known = (t: Typ.t) =>
      switch (Typ.term_of(t)) {
      | Unknown(_) => false
      | _ => true
      };
    switch (field, ascribed(p)) {
    | (Some(t), _) when known(t) => Some(t)
    | (_, Some(t)) => Some(t)
    | _ => Some(Typ.fresh(Unknown(Internal)))
    };
  /* A closure/fix wrapper around a lambda still denotes a function. */
  | Closure(_, inner)
  | FixF(_, inner, _) => arrow_domain_syntactic(~env, inner)
  | Var(x) =>
    switch (Environment.lookup(env, x)) {
    /* Guard against a self-referential binding looping the lookup. */
    | Some({term: Var(y), _}) when y == x => None
    | Some(d) => arrow_domain_syntactic(~env, d)
    | None => None
    }
  | _ => None
  };

let arrow_domain_statics =
    (~info_map: Statics.Map.t, e: Exp.t): option(Typ.t) =>
  switch (Statics.Map.ty_of(Exp.rep_id(e), info_map)) {
  | Some(ty) =>
    switch (Typ.term_of(ty)) {
    | Arrow(d, _) => Some(d)
    | _ => None
    }
  | None => None
  };

/* Apply a function side of a funext goal to the fresh argument.
 *
 * The goal is `f(x) == g(x)` — the same `Ap` convention the rest of the
 * language uses. When the side is a LITERAL lambda, though, the
 * application is a beta-redex whose reduction is DEFINITIONAL: a
 * call-by-value redex whose argument is a bound variable (hence a value)
 * contracts to the body with the parameter renamed, and nothing about the
 * function is discovered by doing it. Contracting here is what makes
 * `(fun x -> x + 1) == (fun x -> 1 + x)` reachable by a pointwise
 * `rewrite` rather than by a leading `eval` on each side.
 *
 * Two deliberate non-contractions:
 *   - a NAMED function (`inv(x)`, `f(x)`) stays an application. Unfolding
 *     a definition is `eval`'s job, and keeping it an `Ap` is exactly why
 *     `refl_eq` on `inv(x) == inv(x)` closes WITHOUT ever applying `inv` —
 *     a true statement, and a different one from `inv == (fun x -> 1 / x)`.
 *   - a CONTRACT lambda (`fun p where g -> body`) stays an application.
 *     Contracting it would silently drop `g`, and funext deliberately adds
 *     no `where` restriction of its own: partiality is meant to surface at
 *     the step that actually applies the function.
 *
 * A non-variable parameter pattern (a tuple, say) also stays applied —
 * there is no single name to substitute for. */
let rec funext_apply =
        (~env: Environment.t(Exp.t), ~arg: Exp.t, f: Exp.t): Exp.t => {
  let rec param_name = (p: Pat.t): option(Var.t) =>
    switch (p |> Pat.term_of) {
    | Var(x) => Some(x)
    | Parens(inner) => param_name(inner)
    | Asc(inner, _) => param_name(inner)
    | _ => None
    };
  switch (f |> Exp.term_of) {
  | Parens(inner) => funext_apply(~env, ~arg, inner)
  | Fun(p, body, _, _) =>
    switch (param_name(p)) {
    | Some(x) =>
      /* The body is substituted under the WHOLE semantic env, not just
       * `x |-> arg`. A literal lambda can mention let-bound names from
       * the surrounding proof (`let f = ... in theorem ... funext y =>`),
       * and if those stay free `Var`s here the later per-step env
       * substitution replaces them with fresh-id copies that no longer
       * match the ids in the goal — every `eval`/`rewrite` pattern lookup
       * against them then misses. `extend` puts the binder LAST so it
       * shadows any same-named outer binding. */
      body |> Substitution.in_exp(Environment.extend(env, (x, arg)))
    | None => Exp.fresh(Ap(Forward, f, arg))
    }
  | _ => Exp.fresh(Ap(Forward, f, arg))
  };
};

/* A fresh copy of a pattern: same shape, all-new ids. `Exp` has
 * `replace_all_ids` for exactly this; patterns lifted out of an existing
 * program need it for the same reason — the copy is about to be written
 * into NEW syntax, and re-using the source's ids would make two places
 * in the program claim the same tiles. */
let pat_replace_all_ids: Pat.t => Pat.t = {
  let f:
    'a.
    (IdTagged.t('a) => IdTagged.t('a), IdTagged.t('a)) => IdTagged.t('a)
   =
    (continue, x) =>
      {
        ...x,
        annotation: {
          /* Keep the lexeme (hole flavor, unknown-op tokens) so the copy
             still displays faithfully; drop the tile provenance. */
          ...IdTagged.IdTag.mk_internal([Id.mk()]),
          lexeme: x.annotation.lexeme,
        },
      }
      |> continue;
  p => Pat.map_term(~f_exp=f, ~f_pat=f, ~f_typ=f, ~f_tpat=f, ~f_rul=f, p);
};

/* The parameter pattern of a function AS WRITTEN, copied fresh.
 *
 * Same walk as `arrow_domain_syntactic`, and for the same reasons:
 * Parens/Asc/Closure/FixF are transparent wrappers around the lambda,
 * and a bare `Var` is followed through the environment so a NAMED
 * function (`inv = fun x -> 1 / x`, goal `inv == inv`) reads its binder
 * off the definition. The WHOLE pattern comes back, not a name — a
 * `fun (x, y) ->` has to be bound by `funext (x, y) =>`, since the
 * binder must bind everything the parameter binds.
 *
 * `None` when there is no lambda to read (both sides opaque at an arrow
 * type), and the caller keeps a hole. */
let rec fun_param_syntactic =
        (~env: Environment.t(Exp.t), e: Exp.t): option(Pat.t) =>
  switch (e |> Exp.term_of) {
  | Parens(inner)
  | Asc(inner, _)
  | Closure(_, inner)
  | FixF(_, inner, _) => fun_param_syntactic(~env, inner)
  | Fun(p, _, _, _) => Some(p |> pat_replace_all_ids)
  | Var(x) =>
    switch (Environment.lookup(env, x)) {
    /* Guard against a self-referential binding looping the lookup. */
    | Some({term: Var(y), _}) when y == x => None
    | Some(d) => fun_param_syntactic(~env, d)
    | None => None
    }
  | _ => None
  };

/* `Some(domain)` when `goal` is `l == r` at an arrow type; the two sides
 * come back too, so the caller can build the pointwise `l(x) == r(x)`. */
/* D80 (S′) has functional extensionality ONLY at first-order argument
 * types. A domain is first-order when, after normalization, it is built
 * from atoms, lists, tuples/labelled tuples and sums of first-order
 * payloads (recursive types through their own bound variable). Arrows,
 * unknown/hole types, polymorphism and unresolved type variables fail
 * closed. */
let first_order_domain = (tyctx: Ctx.t, ty: Typ.t): bool => {
  let rec go = (bound: list(string), t: Typ.t) =>
    switch (Typ.term_of(t)) {
    | Atom(_)
    | Label(_)
    | ExplicitNonlabel => true
    | Parens(t)
    | List(t) => go(bound, t)
    | TupLabel(_, t) => go(bound, t)
    | Prod(ts) => List.for_all(go(bound), ts)
    | Sum(variants) =>
      List.for_all(
        v =>
          switch (v) {
          | ConstructorMap.Variant(_, _, None) => true
          | ConstructorMap.Variant(_, _, Some(t)) => go(bound, t)
          | ConstructorMap.BadEntry(_) => false
          },
        variants,
      )
    | Rec({term: Var(a), _}, body) => go([a, ...bound], body)
    | Var(a) => List.mem(a, bound)
    | _ => false
    };
  go([], Typ.normalize(tyctx, ty));
};

let rec funext_goal =
        (~info_map: Statics.Map.t, ~env: Environment.t(Exp.t), goal: Exp.t)
        : option((Exp.t, Exp.t, Typ.t)) =>
  switch (goal |> Exp.term_of) {
  /* Parens are transparent here for the same reason they are in
   * `peel_binder`: an editor splice must not hide the equality. */
  | Parens(inner) => funext_goal(~info_map, ~env, inner)
  | BinOp(Poly(Equals), l, r) =>
    let domain =
      switch (arrow_domain_statics(~info_map, l)) {
      | Some(_) as d => d
      | None =>
        switch (arrow_domain_statics(~info_map, r)) {
        | Some(_) as d => d
        | None =>
          switch (arrow_domain_syntactic(~env, l)) {
          | Some(_) as d => d
          | None => arrow_domain_syntactic(~env, r)
          }
        }
      };
    domain |> Option.map(d => (l, r, d));
  | _ => None
  };

/* The binder a freshly-inserted `funext` step should start with: the
 * LEFT-hand function's parameter pattern, if the goal has one to give.
 * `(fun x -> x + 1) == (fun x -> 1 + x)` opens as `funext x => ?`
 * rather than `funext ? => ?`.
 *
 * Left-hand only, deliberately: the two sides may name their parameters
 * differently and the reading has to be predictable — "the binder you
 * would have written on the left". Insert-time only; the Funext RULE
 * reads whatever binder is actually written. */
let funext_binder =
    (~info_map: Statics.Map.t, ~env: Environment.t(Exp.t), goal: Exp.t)
    : option(Pat.t) =>
  switch (funext_goal(~info_map, ~env, goal)) {
  | Some((l, _, _)) => fun_param_syntactic(~env, l)
  | None => None
  };

/* Peel the outermost binder from an incoming "for all pat, P" goal. Used
 * by the `Forall` and `Intro` proof forms to walk under the binder. The
 * third component is the binder's `where` restriction, if any — peeling a
 * restricted binder additionally installs it as a hypothesis (a free,
 * sound introduction; docs/prover-obligations.md §2.2). */
let rec peel_binder =
        (incoming: option(Exp.t))
        : option((Pat.t, Typ.t, option(Exp.t), Exp.t)) => {
  open OptUtil.Syntax;
  let* e = incoming;
  switch (e |> Exp.term_of) {
  /* Parens are TRANSPARENT here (docs/prover-obligations.md §0.4 lists
   * them among the forms the checker quotients by). They are not
   * hypothetical: `EditorTransform` wraps a spliced sub-term in a
   * defensive `Parens` whenever it shares a segment level with sibling
   * pieces, so the (!) panel's "Add to statement" exit turns
   * `forall y where g -> forall z -> P` into
   * `forall y where g -> (forall z where c -> P)`. A binder the peeler
   * cannot see is a binder whose `where` restriction is never installed,
   * and the floated condition then stays Pending forever. */
  | Parens(inner) => peel_binder(Some(inner))
  /* The guard the BINDER PATTERN carries (`fun p where g -> _` IS
   * `Fun(Where(p, g), _)`, `forall p where g -> _` IS
   * `Forall(Where(p, g), _)`) is split back off here and installed as a
   * free `where` intro (§2.2). */
  | Fun(p, d1, t, _) =>
    let t = OptUtil.get(() => Typ.fresh(Unknown(Internal)), t);
    let (p, g) = Exp.split_pat_guard(p);
    Some((p, t, g, d1));
  | Forall(p, d1) =>
    /* No annotated type at this level, use Unknown. */
    let (p, g) = Exp.split_pat_guard(p);
    Some((p, Typ.fresh(Unknown(Internal)), g, d1));
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
  /* Transparent, for the reason spelled out on `peel_binder`. */
  | Parens(inner) => peel_stmt_binders(ctx, inner)
  | Forall(p, body) =>
    /* `forall p where g -> _` is `Forall(Where(p, g), _)`: the binder's
     * restriction rides on its pattern and is installed here. */
    let ctx =
      SemanticCtx.add_from_pattern(ctx, p, Typ.fresh(Unknown(Internal)))
      |> SemanticCtx.mark_standard(_, Pat.bound_vars(p));
    peel_stmt_binders(add_pat_where_facts(ctx, p), body);
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
           /* `fun p where g -> body` IS `Fun(Where(p, g), body)`: split
              the contract guard back off the parameter. */
           | Fun(p, body, _, _) =>
             let (p, g) = Exp.split_pat_guard(p);
             record_leftovers(acc, v, p, g, body);
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
/* Resolve the expression a `revert` / `contradiction` cites to the
 * proposition to look up.
 *
 * A bare hypothesis NAME is read out of the THEOREM NAMESPACE — so
 * `revert ih` names the same thing `axiom ih` does. This deliberately
 * does NOT go through `Substitution.in_exp`: installed facts are ALREADY
 * env-substituted (Phase 4b), and substituting one a second time
 * alpha-renames the binders inside its inlined closures
 * (`Environment.free_name` in `Substitution.in_pat`), after which it no
 * longer `fast_equal`s the fact as stored — every by-name citation would
 * miss.
 *
 * A spelled-out proposition is substituted: it is written in the user's
 * vocabulary and has to be brought into the same inlined form as the
 * stored facts. */
let cited_fact = (~ctx: SemanticCtx.t, e: Exp.t): Exp.t => {
  let env = SemanticCtx.get_env(ctx);
  let named =
    switch (exp_to_equality_name(e)) {
    | Some(name) =>
      switch (Ctx.lookup_theorem(SemanticCtx.get_ctx(ctx), name)) {
      | Some({prop: Some(prop), _}) => Some(prop)
      | _ => None
      }
    | None => None
    };
  switch (named) {
  | Some(prop) => prop
  | None => e |> Substitution.in_exp(env)
  };
};

/* --- Existentials (D75 primitive, D81 one-way) and D78 binder facts --- */

let refused = (rule, reason) => ProofMark.ProverStepRefused({rule, reason});

let rec strip_parens_exp = (e: Exp.t): Exp.t =>
  switch (e.term) {
  | Parens(e) => strip_parens_exp(e)
  | _ => e
  };

let rec strip_parens_pat = (p: Pat.t): Pat.t =>
  switch (p.term) {
  | Parens(p) => strip_parens_pat(p)
  | _ => p
  };

let rec strip_parens_proof = (p: Proof.t): Proof.t =>
  switch (p.term) {
  | Parens(p) => strip_parens_proof(p)
  | _ => p
  };

/* A single-variable binder: (name, declared type if ascribed). Pattern
   binders are an unresolved surface and give None. */
let single_binder = (p: Pat.t): option((Var.t, option(Typ.t))) =>
  switch (strip_parens_pat(p).term) {
  | Var(y) => Some((y, None))
  | Asc(x, ty) =>
    switch (strip_parens_pat(x).term) {
    | Var(y) => Some((y, Some(ty)))
    | _ => None
    }
  | _ => None
  };

/* Does `x` occur as a variable anywhere in `e`? Over-approximates free
   occurrence (a bound occurrence also counts), which is the safe side
   for every use below. */
let mentions_var = (x: Var.t, e: Exp.t): bool => {
  let found = ref(false);
  let _: Exp.t =
    Exp.map_term(
      ~f_exp=
        (continue, e: Exp.t) => {
          switch (e.term) {
          | Var(y) when y == x => found := true
          | _ => ()
          };
          continue(e);
        },
      e,
    );
  found^;
};

/* Every name bound by a pattern anywhere inside `e`. */
let bound_names = (e: Exp.t): list(Var.t) => {
  let acc = ref([]);
  let _: Exp.t =
    Exp.map_term(
      ~f_pat=
        (continue, p: Pat.t) => {
          acc := Pat.bound_vars(p) @ acc^;
          continue(p);
        },
      e,
    );
  acc^;
};

/* All variable names occurring in `e` (free or bound; conservative). */
let var_names = (e: Exp.t): list(Var.t) => {
  let acc = ref([]);
  let _: Exp.t =
    Exp.map_term(
      ~f_exp=
        (continue, e: Exp.t) => {
          switch (e.term) {
          | Var(y) => acc := [y, ...acc^]
          | _ => ()
          };
          continue(e);
        },
      e,
    );
  acc^;
};

/* Structural standardness (fail closed). `e` is env-substituted, so
   program names are already inlined: a remaining variable is a checker
   binder, and only quantifier/`obtain` binders count (D80, someStd).
   Total forms only: literals, tuples/lists of standard parts, Int
   `+ - *` and comparisons, Bool `&& || !`, and a lambda whose body is
   standard when its (typed, single) parameter is (std_fun). Builtins,
   division, `==` at unknown type, constructors, closures and everything
   else are NOT standard by this rule. */
let rec evident_first_order = (e: Exp.t): bool =>
  switch (e.term) {
  | Parens(e) => evident_first_order(e)
  | Atom(_)
  | BinOp(Int(_) | Bool(_), _, _)
  | UnOp(_, _) => true
  | _ => false
  };

/* Work bound for one structural-standardness judgement: a shared node
   budget (every node visited, and every descent certification, consumes
   it) and a nesting limit on Fun/FixF judgements. Exhausting either fails
   closed (not standard). */
let std_node_budget = 4000;
let std_certification_cost = 50;
let std_max_depth = 16;

/* Irrefutable binder for a beta/recursive application: a variable or
   wildcard (optionally ascribed), or a tuple of those whose arity is
   fixed by the actual argument (a literal tuple) or by the parameter's
   own product type. Guarded (`where`), constructor, list and literal
   patterns can fail to match, so they are not accepted. */
let rec std_irrefutable =
        (p: Pat.t, ty: option(Typ.t), arg: option(Exp.t)): bool =>
  switch (p.term) {
  | Parens(p) => std_irrefutable(p, ty, arg)
  | Var(_)
  | Wild => true
  | Asc(p, t) => std_irrefutable(p, Some(t), arg)
  | Tuple(ps) =>
    let n = List.length(ps);
    let shape_known =
      switch (Option.map(e => strip_parens_exp(e).term, arg)) {
      | Some(Tuple(args)) => List.length(args) == n
      | _ =>
        switch (Option.map(Typ.term_of, ty)) {
        | Some(Prod(ts)) => List.length(ts) == n
        | _ => false
        }
      };
    shape_known && List.for_all(q => std_irrefutable(q, None, None), ps);
  | _ => false
  };

/* Coverage of a `case`: an UNGUARDED catch-all rule, or both a `[]` rule
   and an unguarded `p :: q` rule over variables/wildcards. Guarded rows
   never count. ADT constructor sets are NOT checked (so a case over a
   user sum type is not shown covered: fail closed). */
let std_covered = (rules: list((Pat.t, Exp.t))): bool => {
  let rec bare = (p: Pat.t) =>
    switch (p.term) {
    | Parens(p)
    | Asc(p, _) => bare(p)
    | _ => p
    };
  let var_or_wild = (p: Pat.t) =>
    switch (bare(p).term) {
    | Var(_)
    | Wild => true
    | _ => false
    };
  let pats = List.map(fst, rules);
  List.exists(var_or_wild, pats)
  || List.exists(
       (p: Pat.t) =>
         switch (bare(p).term) {
         | ListLit([]) => true
         | _ => false
         },
       pats,
     )
  && List.exists(
       (p: Pat.t) =>
         switch (bare(p).term) {
         | Cons(h, t) => var_or_wild(h) && var_or_wild(t)
         | _ => false
         },
       pats,
     );
};

/* `selfs`: the recursive self-names whose descent was certified by
   StructuralRecursion.check; they are usable ONLY as the head of a direct
   application (never bare), and any rebinding removes them. */
let rec std_judge =
        (
          ~selfs: list(Var.t),
          ~depth: int,
          ~budget: ref(int),
          ctx: SemanticCtx.t,
          e: Exp.t,
        )
        : bool => {
  budget := budget^ - 1;
  if (budget^ < 0) {
    false;
  } else {
    let go = std_judge(~selfs, ~depth, ~budget, ctx);
    /* Judge `body` with `names` bound and standard (a quantifier binder,
       a case variable of a standard scrutinee, or a parameter applied to
       a standard argument); rebinding drops them from `selfs`. */
    let under = (~selfs, ~depth, names: list(Var.t), body: Exp.t) =>
      std_judge(
        ~selfs=List.filter(s => !List.mem(s, names), selfs),
        ~depth,
        ~budget,
        List.fold_left(
          (c, x) =>
            SemanticCtx.add_entry(c, Id.mk(), x, Typ.fresh(Unknown(Internal)), None),
          ctx,
          names,
        )
        |> SemanticCtx.mark_standard(_, names),
        body,
      );
    let rec head = (f: Exp.t) =>
      switch (f.term) {
      | Parens(f)
      | Closure(_, f) => head(f)
      | _ => f
      };
    switch (e.term) {
    | Parens(e) => go(e)
    | Var(x) =>
      !List.mem(x, selfs) && SemanticCtx.is_standard_var(ctx, x)
    | Atom(_) => true
    | Tuple(es)
    | ListLit(es) => List.for_all(go, es)
    | Cons(a, b)
    /* Canonical list concatenation (the `@` node, not a name): two
       standard lists give a standard list. */
    | ListConcat(a, b) => go(a) && go(b)
    | BinOp(
        Int(
          Plus | Minus | Times | LessThan | LessThanOrEqual | GreaterThan |
          GreaterThanOrEqual,
        ),
        a,
        b,
      )
    | BinOp(Bool(And | Or | Implies), a, b) => go(a) && go(b)
    /* `==` / `!=` on standard operands at an EVIDENTLY first-order type:
       one side is a literal or an Int/Bool computation, which fixes the
       type (two bare variables could be functions: fail closed). */
    | BinOp(Poly(Equals | NotEquals), a, b)
        when evident_first_order(a) || evident_first_order(b) =>
      go(a) && go(b)
    | UnOp(Int(Minus), a)
    | UnOp(Bool(Not), a) => go(a)
    | If(c, a, b) => go(c) && go(a) && go(b)
    /* Every rule of a covered case over a standard scrutinee: the rule's
       pattern variables are standard (kernel induct), its guard (if any)
       and its body must be standard. All branches are judged. */
    | Match(scrut, rules) =>
      go(scrut)
      && std_covered(rules)
      && List.for_all(
           ((p, body)) => {
             let names = Pat.bound_vars(p);
             (
               switch (Exp.pat_guard(p)) {
               | None => true
               | Some(g) => under(~selfs, ~depth, names, g)
               }
             )
             && under(~selfs, ~depth, names, body);
           },
           rules,
         )
    /* Forward std_all / std_ex (D81, one-way): `!!(forall x. P)` and
       `!!(exists x. P)` from `forall x. !!P`, derived here structurally for
       the body with the (typed, single) binder standard. Never the
       converse; no rewrite of the quantified proposition itself. */
    | Forall(p, body)
    | Exists(p, body) =>
      switch (single_binder(p)) {
      | Some((x, Some(_))) => under(~selfs, ~depth, [x], body)
      | _ => false
      }
    /* std_fun for a lambda VALUE: typed single parameter, body standard
       for a standard argument (a returned partial lambda fails here). */
    | Fun(p, body, _, _) =>
      switch (single_binder(p)) {
      | Some((x, Some(_))) => under(~selfs, ~depth, [x], body)
      | _ => false
      }
    | Ap(Forward, f, arg) =>
      switch (head(f).term) {
      /* A certified recursive call (descent verified for the enclosing
         fixpoint): standard at a standard argument, by well-founded
         induction on the certified descent. */
      | Var(s) when List.mem(s, selfs) => go(arg)
      /* Beta: an irrefutable parameter bound to a standard argument. */
      | Fun(p, body, field, _) when depth < std_max_depth =>
        go(arg)
        && std_irrefutable(p, field, Some(arg))
        && under(~selfs, ~depth=depth + 1, Pat.bound_vars(p), body)
      /* Recursion: descent certified by StructuralRecursion.check AND a
         standard body (all branches) with the parameter standard and the
         self-name usable only as a certified call head. */
      | FixF(self_p, fbody, _) when depth < std_max_depth =>
        budget := budget^ - std_certification_cost;
        switch (Pat.get_var(self_p), head(fbody).term) {
        | (Some(s), Fun(p, body, field, _))
            when
              budget^ >= 0
              && Result.is_ok(StructuralRecursion.check(head(f))) =>
          let names = Pat.bound_vars(p);
          go(arg)
          && std_irrefutable(p, field, Some(arg))
          && std_judge(
               ~selfs=
                 List.filter(
                   x => !List.mem(x, names),
                   [s, ...List.filter(x => x != s, selfs)],
                 ),
               ~depth=depth + 1,
               ~budget,
               List.fold_left(
                 (c, x) =>
                   SemanticCtx.add_entry(
                     c,
                     Id.mk(),
                     x,
                     Typ.fresh(Unknown(Internal)),
                     None,
                   ),
                 ctx,
                 names,
               )
               |> SemanticCtx.mark_standard(_, names),
               body,
             );
        | _ => false
        };
      /* Kernel std_fun application: a standard function (a quantified/
         obtained function, or a lambda by std_fun) at a standard argument.
         Builtins and anything else fail closed. */
      | _ => go(f) && go(arg)
      }
    | _ => false
    };
  };
};

let std_structural = (ctx: SemanticCtx.t, e: Exp.t): bool =>
  std_judge(~selfs=[], ~depth=0, ~budget=ref(std_node_budget), ctx, e);

/* The fact ids of the in-scope facts that are NOT captured, through the
   same capture-aware entries `axiom` uses (ProofCtx.of_theorem_ctx lists
   exactly the theorem entries with a statement, in the same
   innermost-first order as SemanticCtx.facts). */
let uncaptured_fact_id = (ctx: SemanticCtx.t, goal: Exp.t): option(Id.t) => {
  /* Grouping parentheses carry no meaning (`!!(…)` needs them in the
     written premise); they are removed on both sides before comparing. */
  let drop_parens = (e: Exp.t): Exp.t =>
    Exp.map_term(
      ~f_exp=
        (continue, e: Exp.t) =>
          switch (e.term) {
          | Parens(inner) => continue(inner)
          | _ => continue(e)
          },
      e,
    );
  let goal = drop_parens(strip_eq_true(goal));
  let facts = SemanticCtx.facts(ctx);
  let pctx =
    ProofCtx.of_theorem_ctx(~builtins=[], SemanticCtx.get_ctx(ctx));
  let rec zip = (fs, ps) =>
    switch (fs, ps) {
    | ([(_, id, fact), ...fs], [(p: ProofCtx.entry), ...ps]) =>
      !p.is_captured
      && Exp.fast_equal(drop_parens(strip_eq_true(fact)), goal)
        ? Some(id) : zip(fs, ps)
    | _ => None
    };
  zip(facts, pctx);
};

/* The shared standardness side condition `!!e` (witness exI, assume
   D47/D51 gate). Channels, in order: an in-scope fact or closed
   evaluation (`discharge_goal`, closed channel under the D66 no-FixF
   gate); for `forall`/`exists`, an uncaptured in-scope PREMISE
   `forall x. !!P` stating the body's standardness (forward std_all /
   std_ex, one-way); the structural rules. Otherwise Pending — never
   assumed. `ctx` is the context the condition is checked in (for an
   assume gate, the one BEFORE the assumption). */
let standardness_obligation =
    (
      ~step: step_fn,
      ~ctx: SemanticCtx.t,
      ~origin: Id.t,
      ~display: Exp.t,
      e: Exp.t,
    )
    : Obligation.t => {
  let goal = Exp.fresh(Valued(e));
  let premise =
    switch (strip_parens_exp(e).term) {
    | Forall(p, body)
    | Exists(p, body) =>
      uncaptured_fact_id(ctx, Exp.fresh(Forall(p, Exp.fresh(Valued(body)))))
    | _ => None
    };
  let discharge =
    switch (discharge_goal(~step, ~ctx, goal), premise) {
    | (Obligation.Pending, Some(fact_id)) => Obligation.Remote(fact_id)
    | (Obligation.Pending, None) =>
      std_structural(ctx, e) ? Obligation.Structural : Obligation.Pending
    | (d, _) => d
    };
  Obligation.{
    origin,
    bindings: SemanticCtx.get_ctx(ctx).entries,
    goal,
    display_goal: Exp.fresh(Valued(display)),
    discharge,
  };
};

/* `== true by witness v end` on an incoming `exists (y : T) -> P` (exI,
   kernel table: std v ⟹ P v ⟹ Ex P). Never establishes the existential
   by itself: the outgoing goal is the capture-safely instantiated body
   P[v/y], which later steps must still reduce to `true`; std v is
   checked by the instantiation gate (divergence refuses) plus either the
   first-order D80 rule or an explicit `!!v` obligation. Returns
   (outgoing, marks, obligations, substitutions). */
let witness_step =
    (
      ~step: step_fn,
      ~info_map: Statics.Map.t,
      ~ctx: SemanticCtx.t,
      ~origin: Id.t,
      goal: Exp.t,
      v: Exp.t,
    )
    : (Exp.t, list(ProofMark.t), list(Obligation.t), list((string, Exp.t))) => {
  let refuse = reason => (goal, [refused("witness", reason)], [], []);
  switch (strip_parens_exp(goal).term) {
  | Exists(bp, body) =>
    switch (single_binder(bp)) {
    | None =>
      refuse(
        "Only a single-variable `exists (y : T) -> P` takes a witness here.",
      )
    | Some((_, None)) =>
      refuse(
        "The existential's binder has no declared type, so the witness cannot be typed. Annotate it: `exists (y : T) -> …`.",
      )
    | Some((y, Some(ty))) =>
      let tyctx = SemanticCtx.get_ctx(ctx);
      let expected = Typ.normalize(tyctx, ty);
      let got =
        Statics.Map.ty_of(Exp.rep_id(v), info_map)
        |> Option.map(Typ.normalize(tyctx));
      let unknown = (t: Typ.t) =>
        switch (Typ.term_of(t)) {
        | Unknown(_) => true
        | _ => false
        };
      switch (got) {
      | None => refuse("The witness has no type information.")
      | Some(t) when unknown(t) || unknown(expected) =>
        refuse("The witness's type is unknown; it must have the binder's type.")
      | Some(t) when !Typ.fast_equal(t, expected) =>
        refuse("The witness does not have the binder's type.")
      | Some(_) =>
        let v_sub = v |> Substitution.in_exp(SemanticCtx.get_env(ctx));
        let captured =
          List.exists(
            n => List.mem(n, bound_names(body)),
            var_names(v_sub),
          );
        if (captured) {
          refuse(
            "Substituting this witness would capture one of its variables under a binder in the body.",
          );
        } else {
          switch (instantiation_gate(~info_map, ~ctx, [(y, v)])) {
          | Error(_) =>
            (
              goal,
              [
                ProofMark.PossiblyDivergentInstantiation({
                  equality: "witness",
                  var: y,
                }),
              ],
              [],
              [],
            )
          | Ok(conditions) =>
            let domain =
              List.map(
                c => incur_obligation(~step, ~ctx, ~origin, c),
                conditions,
              );
            /* exI side condition `!!v`, for EVERY witness, with its
             * actual discharge: an in-scope fact, closed evaluation of
             * `!!v` (dep-B valued_reduct), or the structural rules above.
             * Never inferred from the binder's type. */
            let standard = [
              standardness_obligation(
                ~step,
                ~ctx,
                ~origin,
                ~display=v,
                v_sub,
              ),
            ];
            /* A function-valued witness is spliced in parenthesized so the
               instantiated body reads back as `(fun k -> k)(1)`, not
               `fun k -> k(1)` (Parens are transparent to the checker). */
            let spliced =
              switch (strip_parens_exp(v_sub).term) {
              | Fun(_)
              | FixF(_)
              | TypFun(_)
              | Closure(_) => Exp.fresh(Parens(v_sub))
              | _ => v_sub
              };
            let instantiated =
              body
              |> Substitution.in_exp(
                   Environment.extend(Environment.empty, (y, spliced)),
                 )
              |> Exp.replace_all_ids;
            (instantiated, [], domain @ standard, [(y, v_sub)]);
          };
        };
      };
    }
  | _ => refuse("A witness needs an `exists` goal here.")
  };
};

/* A fact NAME resolved for citation (`== true by h end`, `obtain … from h`)
   through the same capture-aware theorem context `axiom` uses
   (ProofCtx.of_theorem_ctx): the innermost entry of that name, refused
   if a later binder rebound a name it mentions. */
let usable_named_fact =
    (ctx: SemanticCtx.t, name: string): result(Exp.t, ProofMark.t) => {
  let pctx =
    ProofCtx.of_theorem_ctx(~builtins=[], SemanticCtx.get_ctx(ctx));
  switch (List.find_opt((e: ProofCtx.entry) => e.name == name, pctx)) {
  | None => Error(ProofMark.UnknownEquality(name))
  | Some(e) when e.is_captured =>
    Error(
      refused(
        "captured",
        "`"
        ++ name
        ++ "` mentions a name rebound since it was stated, so it cannot be cited here.",
      ),
    )
  | Some(e) => Ok(e.exp)
  };
};

/* The builtin `refl_eq` (Axioms.v), not a user theorem that happens to
   be called `refl_eq`: a user theorem of that name is a statement-bearing
   theorem entry and shadows the builtin (whose entry carries no
   statement). Primitive reflexivity holds at ANY term (D81), so its
   instantiation needs no standardness. */
let canonical_refl = (ctx: SemanticCtx.t, equality: Exp.t): bool =>
  switch (exp_to_equality_name(equality)) {
  | Some("refl_eq") =>
    switch (Ctx.lookup_theorem(SemanticCtx.get_ctx(ctx), "refl_eq")) {
    | Some({prop: Some(_), _}) => false
    | Some({prop: None, _})
    | None => true
    }
  | _ => false
  };

/* D80: every instantiation of a quantified binder (matched or written
   `with`) must be standard. One shared `!!e` obligation per
   instantiation, with its actual discharge; unresolved stays Pending. */
let instantiation_standardness =
    (~step, ~ctx: SemanticCtx.t, ~origin: Id.t, instantiations) =>
  List.map(
    ((_, e)) => {
      let e_sub = e |> Substitution.in_exp(SemanticCtx.get_env(ctx));
      standardness_obligation(~step, ~ctx, ~origin, ~display=e, e_sub);
    },
    instantiations,
  );

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
  /* Grouping only: transparent. Same incoming, same ctx; the parens node
   * records the inner step's outgoing so the UI can key off either id. */
  | Parens(p) =>
    let (out, m) = check(~step, ~info_map, ~ctx, incoming, p);
    (out, record(id, incoming, out, m));
  | AxiomStep({at_idx, at_exp, direction, equality, instantiation}) =>
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
            ~instantiation,
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
              )
              @ (
                canonical_refl(ctx, equality)
                  ? []
                  : instantiation_standardness(
                      ~step,
                      ~ctx,
                      ~origin=id,
                      instantiations,
                    )
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
            algebrite_step_outgoing_ast(
              ~info_map,
              ~env=SemanticCtx.get_env(ctx),
              ~at_idx,
              ~at_exp,
              ~with_exp,
              inc,
            )
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
            ~info_map,
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
  | Forall(pat, body) =>
    /* The proof's pattern nominally renames the binder; following the UI's
     * ForallStep we use the binder from the incoming goal. */
    let (body_incoming, ctx', binder_marks) =
      switch (incoming) {
      | None => (None, ctx, [ProofMark.MissingIncoming])
      | Some(_) =>
        switch (peel_binder(incoming)) {
        | Some((p, t, guard, inner)) =>
          let ctx' =
            SemanticCtx.add_from_pattern(ctx, p, t)
            |> SemanticCtx.mark_standard(_, Pat.bound_vars(p));
          /* A restricted binder's `where` guard becomes a hypothesis for
           * the sub-proof — free, sound intro (§2.2). */
          let ctx' =
            switch (guard) {
            | Some(g) => add_where_facts(ctx', g)
            | None => ctx'
            };
          /* D78 `forall (x : T !! h) =>`: install `h : !!x` for the body
           * only (D80: quantified binders range over standard values).
           * Only when the written binder IS the goal's binder (same
           * name; same type if the goal declares one), else refused and
           * nothing is installed. */
          let (ctx', std_marks) =
            switch (Pat.std_binder(pat)) {
            | None => (ctx', [])
            | Some((x, ty, h)) =>
              switch (single_binder(p)) {
              | Some((y, gty)) when y == x =>
                let same_type =
                  switch (gty) {
                  | None => true
                  | Some(gty) =>
                    let tyctx = SemanticCtx.get_ctx(ctx);
                    Typ.fast_equal(
                      Typ.normalize(tyctx, gty),
                      Typ.normalize(tyctx, ty),
                    );
                  };
                same_type
                  ? (
                    SemanticCtx.add_hypothesis_named(
                      ctx',
                      h,
                      Exp.fresh(Valued(Exp.fresh(Var(y)))),
                    )
                    |> fst,
                    [],
                  )
                  : (
                    ctx',
                    [
                      refused(
                        "std-binder",
                        "The binder's type differs from the goal's binder type, so `"
                        ++ h
                        ++ "` is not installed.",
                      ),
                    ],
                  );
              | Some(_) => (
                  ctx',
                  [
                    refused(
                      "std-binder",
                      "`"
                      ++ x
                      ++ "` is not the goal's binder name, so `"
                      ++ h
                      ++ "` is not installed.",
                    ),
                  ],
                )
              | None => (
                  ctx',
                  [
                    refused(
                      "std-binder",
                      "Only a single-variable goal binder can carry a named standardness fact.",
                    ),
                  ],
                )
              }
            };
          (Some(inner), ctx', std_marks);
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
  | Funext(pat, body) =>
    /* Functional extensionality as an INTRO rule (never an axiom: an
     * axiom is a rewrite equality, and funext-as-an-equality would have
     * to quantify over functions at unknown/hole types).
     *
     * Incoming `f == g` at an ARROW type; the body's incoming is the
     * pointwise `f(x) == g(x)`, with `x` bound at the domain type. One
     * arrow per `funext` — a curried `a -> b -> c` peels to `f(x) == g(x)`
     * which may still be an arrow, and a second `funext y =>` peels again.
     *
     * NO `where` domain restriction is wrapped around the body goal.
     * Partiality surfaces where it actually bites — when a later step
     * APPLIES `f(x)`. `refl_eq` on the syntactically identical
     * `inv(x) == inv(x)` legitimately succeeds without ever applying
     * `inv`, and that is a different claim from `inv == (fun x -> 1 / x)`.
     *
     * Recovery mirrors Forall's: mark, and let the body keep working on
     * the goal as written. */
    let env = SemanticCtx.get_env(ctx);
    let (body_incoming, ctx', binder_marks) =
      switch (incoming) {
      | None => (None, ctx, [ProofMark.MissingIncoming])
      | Some(goal) =>
        switch (funext_goal(~info_map, ~env, goal)) {
        | None => (incoming, ctx, [ProofMark.ExpectedFunEqGoal])
        | Some((_, _, domain))
            when !first_order_domain(SemanticCtx.get_ctx(ctx), domain) => (
            incoming,
            ctx,
            [
              refused(
                "funext-domain",
                "Functional extensionality is only available at first-order argument types (D80); this domain is "
                ++ Typ.pretty_print(domain)
                ++ ".",
              ),
            ],
          )
        | Some((l, r, domain)) =>
          /* The binder must be something we can actually bind, else the
           * pointwise goal would mention a name nothing introduces. */
          switch (ProofHacks.dhpat_extend_ctx(pat, domain, Ctx.empty)) {
          | None => (incoming, ctx, [ProofMark.MalformedFunext])
          | Some(_) =>
            let ctx' =
              SemanticCtx.add_from_pattern(ctx, pat, domain)
              |> SemanticCtx.mark_standard(_, Pat.bound_vars(pat));
            let arg = pat |> ProofHacks.pat_to_exp;
            /* `ctx'` env, not `ctx`: it maps the funext binder to itself
             * (`y |-> Var(y)`), which Substitution leaves alone, so the
             * pointwise goal keeps mentioning `y` while every OTHER name
             * a contracted lambda body refers to is already resolved. */
            let ap = funext_apply(~env=SemanticCtx.get_env(ctx'), ~arg);
            /* Freshen the WHOLE assembled goal. This is synthesized
             * syntax spliced together out of three trees that all still
             * carry their source ids, so without it the pointwise goal
             * breaks id uniqueness twice over:
             *
             *   - the same `arg` node is written into BOTH sides (`f(x)`
             *     and `g(x)` share one `x` subtree), so one expression
             *     claims the funext binder's tiles twice. Double-clicking
             *     in the inner stepper then runs `Highlight.of_tile` over
             *     a tile whose shard count doesn't match, and it
             *     `failwith`s.
             *   - `l`/`r` are the incoming goal's own subtrees, not
             *     copies, so every token they contribute is also live in
             *     the funext row's editor and the main editor. Clicking
             *     one parks the caret in the outer occurrence.
             *
             * `replace_all_ids` is a structural traversal, so the twice-
             * written `arg` gets a DISTINCT fresh id per occurrence; it
             * also drops shard provenance, which is what the mismatched
             * `Highlight.of_tile` was reading. `StepperBase.embed_exp`
             * already does exactly this when splicing an expression into
             * proof syntax — the synthesized body incoming needs it for
             * the same reason.
             *
             * The ids must be freshened HERE, before the ProofMap records
             * the body's incoming: the inner stepper overlay and every
             * `nth_exp_env`/eval/rewrite lookup on the body resolve
             * against this tree, so it is the one that has to be
             * internally unique. Keeping the ids aligned with the outer
             * goal is not a feature to preserve — it is the bug. */
            let pointwise =
              Exp.fresh(BinOp(Poly(Equals), ap(l), ap(r)))
              |> Exp.replace_all_ids;
            (Some(pointwise), ctx', []);
          }
        }
      };
    let (out_body, m) =
      check(~step, ~info_map, ~ctx=ctx', body_incoming, body);
    /* Discharged exactly when the body reduces the pointwise goal to
     * `true`; otherwise the OUTER `f == g` passes through unchanged (the
     * body's partial outgoing lives under the binder). Same convention as
     * Forall. */
    let true_exp = Exp.temp(Atom(Bool(true)));
    let outgoing =
      switch (out_body) {
      | Some(e) when Exp.fast_equal(e, true_exp) => out_body
      | _ => incoming
      };
    (outgoing, record(~marks=binder_marks, id, incoming, outgoing, m));
  | Assume(e, as_name, body) =>
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
        /* Grouping parentheses are transparent (as for every other
         * peeling site, and as October's D47/D51 gate reads the goal):
         * `A ==> (B ==> C)` introduces B at the inner step. */
        switch (strip_parens_exp(goal) |> Exp.term_of) {
        | BinOp(Bool(Implies), a, b)
            when
              Exp.fast_equal(
                strip_parens_exp(
                  a |> Substitution.in_exp(SemanticCtx.get_env(ctx)),
                ),
                strip_parens_exp(hyp),
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
      /* D47/D51: introducing `P ==> Q` needs `!!P` (material implication
       * is only sound for a standard antecedent; Curry). Recorded as a
       * real obligation, checked in the PRE-assumption context `ctx` —
       * the new hypothesis can never discharge its own gate — and
       * accepted even when unresolved: it then stays Pending and the
       * theorem cannot be plain Proven. */
      | Some(_) => [
          standardness_obligation(~step, ~ctx, ~origin=id, ~display=e, hyp),
        ]
      | None => [
          Obligation.{
            origin: id,
            bindings: SemanticCtx.get_ctx(ctx).entries,
            goal: hyp,
            /* The assumption as written, before env-inlining (see
             * Obligation.display_goal). */
            display_goal: e,
            discharge: discharge_goal(~step, ~ctx, hyp),
          },
        ]
      };
    let body_incoming =
      switch (intro_consequent) {
      | Some(b) => Some(b)
      | None => incoming
      };
    /* `assume <e> as <h>` installs the hypothesis under `h`; without the
     * clause it is the fixed name `assume`, which SHADOWS any enclosing
     * one (`SemanticCtx.hypothesis_name`). Naming is how a nested
     * assumption keeps the outer one citable. */
    let (ctx', _binding) =
      SemanticCtx.add_hypothesis(
        ctx,
        as_name_or(~default="assume", as_name),
        hyp,
      );
    let (out_body, m) =
      check(~step, ~info_map, ~ctx=ctx', body_incoming, body);
    (out_body, record(~obligations, id, incoming, out_body, m));
  | RelEqBy(target, just) =>
    /* `== <target> by <justification> end`. Supported: target `true` with
     * `witness v` on an `exists` goal (exI, see `witness_step`), or with
     * a fact NAME whose statement is the goal itself (D76: a citation is
     * the one-step chain `== true by h end`). Anything else is refused
     * and the goal passes through. */
    let pass = marks => (
      incoming,
      record(~marks, id, incoming, incoming, ProofMap.empty),
    );
    switch (incoming) {
    | None => pass([ProofMark.MissingIncoming])
    | Some(goal) =>
      let is_true =
        Exp.fast_equal(
          strip_parens_exp(target),
          Exp.temp(Atom(Bool(true))),
        );
      if (!is_true) {
        pass([
          refused(
            "rel-eq-by",
            "Only `== true by … end` is supported here.",
          ),
        ]);
      } else {
        switch (strip_parens_proof(just).term) {
        | Witness(v) =>
          let (outgoing, marks, obligations, substitutions) =
            witness_step(~step, ~info_map, ~ctx, ~origin=id, goal, v);
          (
            Some(outgoing),
            record(
              ~marks,
              ~obligations,
              ~substitutions,
              id,
              incoming,
              Some(outgoing),
              ProofMap.empty,
            ),
          );
        | Cite(h) =>
          switch (usable_named_fact(ctx, h)) {
          | Ok(prop)
              when
                Exp.fast_equal(strip_eq_true(prop), strip_eq_true(goal)) =>
            let outgoing = Some(Exp.temp(Atom(Bool(true))));
            (outgoing, record(id, incoming, outgoing, ProofMap.empty));
          | Ok(_) =>
            pass([
              refused(
                "cite",
                "`" ++ h ++ "` does not state the current goal.",
              ),
            ])
          | Error(mark) => pass([mark])
          }
        | _ =>
          pass([
            refused(
              "rel-eq-by",
              "`== … by … end` accepts only `witness <v>` or a fact name here.",
            ),
          ])
        };
      };
    };
  | Witness(_)
  | Cite(_) =>
    /* A justification outside `== … by … end`: refused, pass-through. */
    (
      incoming,
      record(
        ~marks=[
          refused(
            "justification",
            "This is a justification: write `== true by … end` around it.",
          ),
        ],
        id,
        incoming,
        incoming,
        ProofMap.empty,
      ),
    )
  | Obtain(x, h, e, body) =>
    /* `obtain x as h from E => body` (∃-elim; kernel someEx/someStd):
     * E must be an in-scope fact `exists (y : T) -> P` — resolved by name
     * or spelled out, and FOUND among the facts, never just written. `x`
     * is a fresh proof-level variable (standard by someStd) and
     * `h : P[x/y]` a fact, both scoped to the body. No witness is
     * extracted or searched for. The body's outgoing, and any pending
     * obligation it exports, may not mention `x` (no escape). */
    let pass = marks => (
      incoming,
      record(~marks, id, incoming, incoming, ProofMap.empty),
    );
    /* A NAME goes through the capture-aware citation path; a spelled-out
     * proposition must be FOUND among the facts (`lookup_fact`). */
    let resolved: result(Exp.t, ProofMark.t) =
      switch (strip_parens_exp(e).term) {
      | Var(name) => usable_named_fact(ctx, name)
      | _ =>
        let fact = e |> Substitution.in_exp(SemanticCtx.get_env(ctx));
        switch (lookup_fact(ctx, fact)) {
        | Some(_) => Ok(fact)
        | None =>
          Error(
            refused("obtain", "No fact in scope states this existential."),
          )
        };
      };
    switch (incoming, Pat.get_var(x), Pat.get_var(h)) {
    | (None, _, _) => pass([ProofMark.MissingIncoming])
    | (_, None, _)
    | (_, _, None) =>
      pass([
        refused(
          "obtain",
          "`obtain x as h from E =>` takes a single variable and a fact name.",
        ),
      ])
    | (Some(goal), Some(xn), Some(hn)) =>
      let shape =
        switch (resolved) {
        | Ok(f) => Ok(strip_parens_exp(f).term)
        | Error(m) => Error(m)
        };
      switch (resolved, shape) {
      | (Error(mark), _) => pass([mark])
      | (Ok(_), Ok(Exists(bp, p_body))) =>
        switch (single_binder(bp)) {
        | None =>
          pass([
            refused(
              "obtain",
              "Only a single-variable `exists` can be obtained from here.",
            ),
          ])
        | Some((y, ty)) =>
          let in_ctx =
            Option.is_some(Ctx.lookup_var(SemanticCtx.get_ctx(ctx), xn));
          let in_facts =
            List.exists(
              ((_, _, f)) => mentions_var(xn, f),
              SemanticCtx.facts(ctx),
            );
          if (in_ctx || mentions_var(xn, goal) || in_facts) {
            pass([
              refused(
                "obtain",
                "`" ++ xn ++ "` is not fresh here; choose a new name.",
              ),
            ]);
          } else if (List.mem(xn, bound_names(p_body))) {
            pass([
              refused(
                "obtain",
                "`"
                ++ xn
                ++ "` would be captured by a binder in the existential's body.",
              ),
            ]);
          } else {
            let ty =
              Option.value(~default=Typ.fresh(Unknown(Internal)), ty);
            /* someStd: the obtained variable is standard. */
            let ctx' =
              SemanticCtx.add_entry(ctx, Id.mk(), xn, ty, None)
              |> SemanticCtx.mark_standard(_, [xn]);
            let hyp =
              p_body
              |> Substitution.in_exp(
                   Environment.extend(
                     Environment.empty,
                     (y, Exp.fresh(Var(xn))),
                   ),
                 );
            let (ctx', _) = SemanticCtx.add_hypothesis_named(ctx', hn, hyp);
            let (out_body, m) =
              check(~step, ~info_map, ~ctx=ctx', incoming, body);
            let escaped_obligation =
              ProofMap.obligations_of_proof(m, body)
              |> List.exists((o: Obligation.t) =>
                   Obligation.is_pending(o) && mentions_var(xn, o.goal)
                 );
            let escaped_outgoing =
              switch (out_body) {
              | Some(o) => mentions_var(xn, o)
              | None => false
              };
            if (escaped_outgoing || escaped_obligation) {
              let marks = [
                refused(
                  "obtain-escape",
                  "`"
                  ++ xn
                  ++ "` escapes its `obtain`: the result may not mention it.",
                ),
              ];
              (incoming, record(~marks, id, incoming, incoming, m));
            } else {
              (out_body, record(id, incoming, out_body, m));
            };
          };
        }
      | (Ok(_), _) =>
        pass([refused("obtain", "This fact is not an `exists`.")])
      }
    }
  | Have(e, sub, body) =>
    /* `have <exp> proof <subproof> => <body>` — the CUT rule, and the
     * target of §3.3's "prove here" exit (docs/prover-obligations.md).
     *
     * Have is an ordinary TWO-PREMISE rule, not a special shape. With
     * incoming goal `G` it incurs exactly two obligations, both recorded
     * on the have's own node, the way a two-condition axiom would:
     *
     *   1. prove `<exp>` in the ENCLOSING scope (the subproof may not
     *      cite the have's own hypothesis — otherwise `have P proof
     *      <cite P> => ...` would prove anything);
     *   2. prove `G` in the scope EXTENDED with `<exp>` under the
     *      auto-name "have".
     *
     * The two attached proofs are what retire them: obligation 1 is
     * dropped exactly when the subproof reaches literal `true` with a
     * clean subtree, obligation 2 exactly when the BODY does. An
     * unretired premise is a Pending obligation — "this premise is still
     * open" — not a broken step, so the form stays mark-free like
     * `assume`.
     *
     * Asymmetry in HOW they are retired, and it is deliberate:
     *
     * - obligation 1 also runs the ordinary discharge channels
     *   (`discharge_goal`), so an already-known `<exp>` goes Remote /
     *   Evaluated without the user writing a subproof at all;
     * - obligation 2 does NOT. Its scope contains the freshly installed
     *   `have` hypothesis, so channel-1 lookup would discharge it for
     *   free whenever `G` happens to equal `<exp>` — trivialising the
     *   second premise against the very fact the rule is introducing.
     *   It is Pending until the body's proof closes it.
     *
     * The hypothesis is installed UNCONDITIONALLY, whether or not the
     * subproof is finished: that is what lets obligations incurred by
     * STEPS INSIDE the body discharge against it through ordinary
     * channel-1 lookup (§4.2) the moment the wrapper is written. That
     * channel is for body steps, not for obligation 2 above.
     *
     * Outgoing is the body's, as before. Note that this makes obligation
     * 2 Pending exactly when the have's own outgoing is not a clean
     * `true` — i.e. only when `full_status_of_proof` already reads
     * Incomplete — so a pending second premise never masquerades as
     * ProvenModulo. The one shape it misreports is `Seq(Have(...), tail)`,
     * a have used as a mid-chain pass-through with the goal closed AFTER
     * it: under the two-premise reading that is simply the wrong place to
     * continue (the tail is outside the have's scope anyway), and the
     * stepper never writes it. */
    let hyp = e |> Substitution.in_exp(SemanticCtx.get_env(ctx));
    let (out_sub, m_sub) = check(~step, ~info_map, ~ctx, Some(hyp), sub);
    let true_exp = Exp.temp(Atom(Bool(true)));
    let proves = (out, m, p) =>
      switch (out) {
      | Some(o) =>
        Exp.fast_equal(o, true_exp) && ProofMap.proof_is_clean(m, p)
      | None => false
      };
    /* Premise 1: `<exp>` in the enclosing scope. */
    let sub_obligations =
      proves(out_sub, m_sub, sub)
        ? []
        : [
          Obligation.{
            origin: id,
            bindings: SemanticCtx.get_ctx(ctx).entries,
            goal: hyp,
            display_goal: e,
            discharge: discharge_goal(~step, ~ctx, hyp),
          },
        ];
    let (ctx', _binding) = SemanticCtx.add_hypothesis(ctx, "have", hyp);
    let (out_body, m_body) =
      check(~step, ~info_map, ~ctx=ctx', incoming, body);
    /* Premise 2: the incoming goal, in the extended scope. Its bindings
     * are ctx' — the panel's local context for this row should show the
     * `have` hypothesis the premise is allowed to use. Recorded only
     * when there IS an incoming goal to prove. */
    let body_obligations =
      switch (incoming) {
      | Some(goal) when !proves(out_body, m_body, body) => [
          Obligation.{
            origin: id,
            bindings: SemanticCtx.get_ctx(ctx').entries,
            goal,
            /* No "as written" form: unlike `<exp>`, the incoming goal
             * reaches this arm already env-inlined by the chain above. */
            display_goal: goal,
            discharge: Obligation.Pending,
          },
        ]
      | Some(_)
      | None => []
      };
    (
      out_body,
      record(
        ~obligations=sub_obligations @ body_obligations,
        id,
        incoming,
        out_body,
        ProofMap.union(m_sub, m_body),
      ),
    );
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
     * body's semantic ctx — one filter over the THEOREM NAMESPACE now
     * covers both channels (discharge-channel-1 `lookup_fact` and
     * `ProofCtx.of_theorem_ctx` rule lookup read the same entries) —
     * rather than relying on the `is_captured` machinery (which only
     * covers rule lookup, and only fires once the body re-peels the new
     * binder). The mention test is FREE occurrence, via the same
     * machinery `ProofCtx.of_theorem_ctx` uses for `is_captured`
     * (`ProofRule.mentions_any`):
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
       * a rule and ask whether its core (guards included) mentions `x` —
       * cf. ProofCtx.of_env's capture test. */
      let mentions_x = (fact: Exp.t) =>
        ProofRule.mentions_any(ProofRule.exp_to_rule(fact), [x]);
      /* A hypothesis name with base "where": "where", "where'", ... (see
       * SemanticCtx.add_hypothesis / Var.next_name). */
      let is_where_name = (name: string): bool =>
        String.length(name) >= 5
        && String.sub(name, 0, 5) == "where"
        && String.for_all(
             c => c == '\'',
             String.sub(name, 5, String.length(name) - 5),
           );
      let guards =
        SemanticCtx.facts(ctx)
        |> List.filter_map(((name, _id, fact)) =>
             is_where_name(name) && mentions_x(fact) ? Some(fact) : None
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
          Exp.fresh(Forall(Pat.fresh(Where(binder, guard)), goal));
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
                  | TheoremEntry({prop: Some(fact), _}) => !mentions_x(fact)
                  | _ => true
                  },
                base_ctx.entries,
              ),
          },
          SemanticCtx.get_env(ctx),
        );
      /* Keep the standardness of the OTHER binders; `x` is re-quantified
       * and becomes standard again only when the body re-peels it. */
      let ctx' = {
        ...ctx',
        std: List.filter(n => n != x, ctx.std),
      };
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
  | Revert(e, instantiation, body) =>
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
    let env_of_ctx = SemanticCtx.get_env(ctx);
    /* Resolve the argument to the proposition to look up (see
     * `cited_fact`, shared with the `contradiction` step). */
    let fact = cited_fact(~ctx, e);
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
        /* Phase 4d: with `with x = v`, the antecedent cashed into the
         * goal is the fact INSTANTIATED at `v` rather than the fact as
         * it stands. Sound for the same one-line reason as plain
         * revert, once `v` clears the gates: `forall x -> F(x)` holds in
         * this scope, so `F(v)` holds, so `F(v) ==> G` denotes exactly
         * what `G` denotes. The gates are the Phase-3a instantiation
         * gate applied to `v` — divergence refuses, domain conditions
         * become obligations on this step — the same treatment a
         * matched instantiation gets at an axiom step. */
        let instantiated =
          switch (instantiation) {
          | None => Ok((fact, []))
          | Some((var, inst)) =>
            switch (exp_to_equality_name(var)) {
            | None =>
              Error(([ProofMark.RevertFactNotQuantified({var: "?"})], []))
            | Some(x) =>
              let inst_sub = inst |> Substitution.in_exp(env_of_ctx);
              switch (ProofRule.instantiate_binder(x, inst_sub, fact)) {
              | None =>
                Error(([ProofMark.RevertFactNotQuantified({var: x})], []))
              | Some(f) =>
                switch (
                  instantiation_gate(~info_map, ~ctx, [(x, inst_sub)])
                ) {
                | Error(v) =>
                  Error((
                    [
                      ProofMark.PossiblyDivergentInstantiation({
                        equality: x,
                        var: v,
                      }),
                    ],
                    [],
                  ))
                | Ok(domain_conditions) =>
                  Ok((
                    f,
                    List.map(
                      incur_obligation(~step, ~ctx, ~origin=id),
                      dedup_conditions(domain_conditions),
                    )
                    @ instantiation_standardness(
                        ~step,
                        ~ctx,
                        ~origin=id,
                        [(x, inst)],
                      ),
                  ))
                }
              };
            }
          };
        switch (instantiated) {
        | Error((marks, obligations)) =>
          /* Refusal is recovery here too: check the body against the
           * unchanged goal so later steps stay meaningful. */
          let (_, m) = check(~step, ~info_map, ~ctx, incoming, body);
          (
            incoming,
            record(~marks, ~obligations, id, incoming, incoming, m),
          );
        | Ok((antecedent, obligations)) =>
          let body_incoming =
            Some(Exp.fresh(BinOp(Bool(Implies), antecedent, goal)));
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
          (outgoing, record(~obligations, id, incoming, outgoing, m));
        };
      }
    };
  | Contradiction(e, instantiation) =>
    /* `contradiction F end` / `contradiction F with x = e end` — ex falso
     * quodlibet as one terminal step (docs/prover-obligations.md,
     * Phase 4e). See the block comments above for the soundness argument
     * and for what the `with` clause is; the three steps here are:
     *
     *   1. resolve `F` against the in-scope facts EXACTLY as `revert`
     *      does (by name out of the env, or env-substituted and matched
     *      with the channel-1 `Exp.fast_equal` lookup). No match is
     *      recovery, not refusal: mark and pass the goal through.
     *   2. apply the step's own explicit substitution — nothing else, no
     *      harvesting (see `contradiction_substitution`; an unverifiable
     *      binding marks and passes through) — then run the injected
     *      `step_fn` to a fixpoint under the channel-2 fuel bound.
     *   3. literal `false` ⇒ the branch is vacuous, so the outgoing goal
     *      is the literal `true` and NO obligation is incurred. Anything
     *      else (`true`, stuck, still open, out of fuel) marks and passes
     *      the goal through.
     *
     * The applied binding is stashed in the ProofMap entry's
     * `substitutions` field: with the rework it is exactly what the user
     * wrote, which is a better receipt than a derived set. */
    let env_of_ctx = SemanticCtx.get_env(ctx);
    let fact = cited_fact(~ctx, e);
    switch (incoming) {
    | None => (
        None,
        record(
          ~marks=[ProofMark.MissingIncoming],
          id,
          None,
          None,
          ProofMap.empty,
        ),
      )
    | Some(_) =>
      switch (lookup_fact(ctx, fact)) {
      | None => (
          incoming,
          record(
            ~marks=[ProofMark.UnknownFactContradicted],
            id,
            incoming,
            incoming,
            ProofMap.empty,
          ),
        )
      | Some(_fact_id) =>
        switch (
          contradiction_substitution(
            ~env=env_of_ctx,
            ~ctx,
            ~instantiation,
            fact,
          )
        ) {
        | Error(mark) => (
            incoming,
            record(~marks=[mark], id, incoming, incoming, ProofMap.empty),
          )
        | Ok((substituted, substitutions)) =>
          let computed =
            eval_via_step(
              ~step,
              ~env=env_of_ctx,
              ~fuel=closed_eval_fuel,
              substituted,
            );
          if (Exp.fast_equal(computed, Exp.temp(Atom(Bool(false))))) {
            let outgoing = Some(Exp.fresh(Atom(Bool(true))));
            (
              outgoing,
              record(~substitutions, id, incoming, outgoing, ProofMap.empty),
            );
          } else {
            (
              incoming,
              record(
                ~substitutions,
                ~marks=[ProofMark.ContradictionNotFalse],
                id,
                incoming,
                incoming,
                ProofMap.empty,
              ),
            );
          };
        }
      }
    };
  | Alias(x, e, body) =>
    /* `alias <name> = <fact> => <body>` — RETROACTIVE naming
     * (docs/prover-obligations.md, "Hypothesis naming").
     *
     * Auto-installed facts use FIXED names and SHADOW, so a nested split
     * hides the enclosing one's `case_eq`. This form is the escape
     * hatch: it resolves `<fact>` against the in-scope facts exactly as
     * `revert` does — by bare name (`cited_fact`), or by spelling the
     * proposition out — and installs the SAME proposition again under
     * `<name>` for the body's scope.
     *
     * Sound for the shortest possible reason: nothing new is assumed.
     * The proposition was already known here, and the body proves the
     * same goal it would have proven without the wrapper. So: NO
     * obligation, and the outgoing is the body's, untouched.
     *
     * An unresolvable fact reuses `UnknownFactReverted` — the same mark
     * `revert` raises for the same mistake — and, as everywhere else in
     * this checker, refusal is RECOVERY: the body is still checked
     * against the unchanged goal so later steps stay meaningful. */
    let fact = cited_fact(~ctx, e);
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
      /* A non-identifier name slot (hole, compound pattern) names
       * nothing; the fact stays reachable under whatever it already was,
       * and the step is a no-op rather than a broken one. */
      let ctx' =
        switch (Pat.bound_vars(x)) {
        | [h] => SemanticCtx.add_hypothesis_named(ctx, h, fact) |> fst
        | _ => ctx
        };
      let (out_body, m) = check(~step, ~info_map, ~ctx=ctx', incoming, body);
      (out_body, record(id, incoming, out_body, m));
    };
  | Induction(scrut, as_name, cases) =>
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
      check_induction(
        ~step,
        ~info_map,
        ~ctx,
        ~incoming,
        ~scrut,
        ~as_name,
        ~cases,
      );
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
 * If exhaustive (by Coverage.check in STRICT mode — see below) and every
 * case proves its goal to `true`, the induction's outgoing is `true`.
 * Otherwise outgoing is None. Returns a list of proof marks for the
 * induction node itself (empty-cases / not-exhaustive / scrutinee-untyped
 * — per-case failures surface as children).
 *
 * Exhaustiveness here is STRICTER than the edit-time check in Statics.re:
 * a scrutinee whose type is `Unknown` is refused rather than treated as
 * vacuously covered, because certifying is not marking. See
 * `Coverage.check(~strict_unknown)` and docs/prover-obligations.md §1.
 */
and check_induction =
    (
      ~step: step_fn,
      ~info_map: Statics.Map.t,
      ~ctx: SemanticCtx.t,
      ~incoming: option(Exp.t),
      ~scrut: Exp.t,
      /* The split's `as <name>` clause, if any: it renames the
       * case-equality hypothesis installed in EVERY case below. */
      ~as_name: option(Pat.t),
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
  /* Kernel `induct` (kernel-table.tex:222): the constructor arguments
   * of a STANDARD scrutinee are standard. Only when the scrutinee itself
   * is shown standard (a quantifier/obtain binder, a case variable of an
   * enclosing standard split, or a structurally standard term) are the
   * case's pattern variables marked; a computed scrutinee gives them no
   * standardness, and they stay unmarked (gates on them stay Pending). */
  let scrut_standard =
    std_structural(
      ctx,
      scrut |> Substitution.in_exp(SemanticCtx.get_env(ctx)),
    );
  let (outgoings, pat_constraints, _prev_pats, m_acc) =
    List.fold_left(
      ((outs, constraints, prev_pats, m), (pat, body)) => {
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
        let ctx' =
          scrut_standard
            ? SemanticCtx.mark_standard(ctx', Pat.bound_vars(pat)) : ctx';
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
        /* 2a'. `where` facts for this arm.
         *
         * `where` on a case/induction arm is the same construct as
         * `where` on a `fun`: matching the arm means the guard HELD
         * (PatternMatch filters on it), so inside the arm the guard is a
         * citable `where` fact — and every earlier arm that could have
         * matched this value must have FAILED, so its guard's negation
         * is a fact here too. Installed after `case_eq` so the arm's own
         * positive guard is the most recent `where`. */
        let ctx' = add_previous_guard_negations(ctx', prev_pats, pat);
        let ctx' = add_pat_where_facts(ctx', pat);
        let ctx' =
          switch (case_eq) {
          | Some(eq) =>
            /* `induction <e> as <h>` puts this equation under `h` in
             * every case; the equation still differs case by case, only
             * the NAME is shared. Unnamed, it is the fixed name
             * `case_eq`, which SHADOWS any enclosing case equation
             * (`SemanticCtx.hypothesis_name`). */
            let (ctx'', _) =
              SemanticCtx.add_hypothesis(
                ctx',
                as_name_or(~default="case_eq", as_name),
                eq,
              );
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
        /* Collect the pattern constraint for later coverage check. A
         * GUARDED arm contributes NO constraint: a guard is not a case
         * split, so `| S(m) where P => .. | Z => .. end` is reported
         * not-exhaustive rather than unsoundly Proven. Same conservative
         * rule as the edit-time check in Statics.re. */
        let constraint_ =
          if (Pat.is_guarded(pat)) {
            None;
          } else {
            switch (Statics.Map.lookup_pat(Pat.rep_id(pat), info_map)) {
            | Some(info_pat) => Some(Info.pat_constraint(info_pat))
            | None => None
            };
          };
        (
          outs @ [(out_body, case_clean)],
          constraints @ [constraint_],
          prev_pats @ [pat],
          ProofMap.union(m, m_body),
        );
      },
      ([], [], [], ProofMap.empty),
      cases,
    );
  /* Exhaustiveness: check the collected constraints cover scrut's type. */
  /* Exhaustiveness, checked in STRICT mode: unlike edit-time statics, a
   * column whose type we cannot positively determine is NOT treated as
   * vacuously covered (Coverage.re, `first_col_exhaustive`). This is what
   * stops an `Unknown` scrutinee from certifying an induction with cases
   * missing — including nested `Unknown`s, e.g. an ADT whose payload type
   * is a hole. */
  let normalized_scrut_ty =
    Typ.normalize(SemanticCtx.get_ctx(ctx), scrut_ty);
  let is_exhaustive = {
    let constraints = List.filter_map(Fun.id, pat_constraints);
    Coverage.check(~strict_unknown=true, constraints, normalized_scrut_ty).
      exhaustiveness
    == Exhaustive;
  };
  /* Did we fail to determine the scrutinee's type at all? Drives the
   * sharper mark below. `ty_of` returning None already funnels into the
   * `Unknown(Internal)` fallback above, so this one test covers both. */
  let scrut_ty_undetermined =
    switch (Typ.term_of(normalized_scrut_ty)) {
    | Unknown(_) => true
    | _ => false
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
        ? [
          scrut_ty_undetermined
            ? ProofMark.InductionScrutineeUntyped
            : ProofMark.InductionNotExhaustive,
        ]
        : []
    );
  /* Discharged inductions produce `true`; anything else passes the
   * outer goal through (recovery — see `result_to_outgoing`). */
  let outgoing =
    is_exhaustive && all_true && List.length(cases) > 0
      ? Some(true_exp) : incoming;
  (outgoing, marks, m_acc);
};
