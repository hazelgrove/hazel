open Util;

/* Proof checker (big-step).
 *
 * Walks a proof term, threading an "incoming" goal through each sub-form
 * and producing an "outgoing" goal (if the proof propagates through).
 * The walk also builds a ProofMap keyed by each proof sub-term id,
 * recording the (incoming, outgoing) pair seen at that node, the
 * `ProofMark.t` errors of the step and the obligations it incurs.
 *
 * A step that fails records its mark but passes the incoming goal
 * through, so later steps are still checked against the last good goal.
 * Because that can carry `true`/`false` past a broken step, proven and
 * disproven status also require a mark-free proof subtree
 * (`ProofMap.status_of_proof`).
 *
 * The rewrite steps, binder peeling, `where` facts and funext goals are
 * shared with the UI stepper, so both compute a step the same way.
 *
 * This module doesn't depend on the dynamics (`Transition`,
 * `Evaluator`): when the checker needs to take an evaluation step, it
 * calls the injected `step_fn`, which the evaluator supplies when it
 * invokes the checker.
 */

type step_result = {
  auto_incoming: list((string, Exp.t)),
  auto_outgoing: list((Exp.t, string)),
  outgoing: Exp.t,
};

/* Where a `step_fn` takes its step: at the first redex in evaluation
 * order, or only at the root of the expression it is given. */
type redex_at =
  | Anywhere
  | Root;

type step_fn =
  (~env: Environment.t(Exp.t), ~at: redex_at, Exp.t) => option(step_result);

let record =
    (
      ~ctx: SemanticCtx.t,
      ~marks: list(ProofMark.t)=[],
      ~auto_incoming: list((string, Exp.t))=[],
      ~auto_outgoing: list((Exp.t, string))=[],
      ~substitutions: list((string, Exp.t))=[],
      ~obligations: list(Obligation.t)=[],
      id: Id.t,
      incoming: option(Exp.t),
      outgoing: option(Exp.t),
      m: ProofMap.t,
    )
    : ProofMap.t =>
  ProofMap.add(
    id,
    {
      incoming,
      auto_incoming,
      auto_outgoing,
      outgoing,
      substitutions,
      marks,
      obligations,
      ctx,
    },
    m,
  );

let is_true = (goal: option(Exp.t)): bool =>
  switch (goal) {
  | Some(e) => Exp.fast_equal(e, Exp.temp(Atom(Bool(true))))
  | None => false
  };

/* The name an `as <name>` clause asks for, or the form's fixed base name
 * when there is no clause. A hole or a compound pattern names nothing,
 * so it also gets the base name rather than dropping the hypothesis.
 * Base names shadow; see `SemanticCtx.add_hypothesis`.
 *
 * The checker runs on the elaborated proof, where a bare identifier
 * pattern can arrive wrapped (`Parens`, `Asc`), so the name is read
 * through `Pat.bound_vars` rather than by matching `Var`. */
let as_name_or = (~default: Var.t, as_name: option(Pat.t)): Var.t =>
  switch (Option.map(Pat.bound_vars, as_name)) {
  | Some([h]) => h
  | Some(_)
  | None => default
  };

/* Discharging an obligation's goal: by lookup of a fact in scope, then by
 * closed evaluation, and by nothing else, so a user can always see why
 * an obligation was discharged. */
module Discharge = {
  /* The only normalization in the discharge relation: `P == true` and
   * `true == P` normalize to `P`, recursively.
   *
   * The obligations panel's split exit needs it: `induction P | true =>
   * ... | false => ...` installs `P == true` as the true branch's
   * hypothesis, and the obligation the branch has to retire is the bare
   * `P`.
   *
   * Sound in the flat domain: `P == true` denotes `true` only when `P`
   * does (if `P` is `false` the equation is `false`; if `P` is `err` or ⊥
   * so is the equation). Being an equivalence, it can be applied to both
   * sides. */
  let rec strip_eq_true = (e: Exp.t): Exp.t =>
    switch (Exp.term_of(e)) {
    | BinOp(Poly(Equals), a, b) when is_true(Some(b)) => strip_eq_true(a)
    | BinOp(Poly(Equals), a, b) when is_true(Some(a)) => strip_eq_true(b)
    | Parens(e') => strip_eq_true(e')
    | _ => e
    };

  /* The id of a fact in scope (`SemanticCtx.facts`) that is
   * `Exp.fast_equal` to `goal`, up to `strip_eq_true`. */
  let lookup = (ctx: SemanticCtx.t, goal: Exp.t): option(Id.t) => {
    let goal_nf = strip_eq_true(goal);
    SemanticCtx.facts(ctx)
    |> List.find_map(((_name, id, fact)) =>
         Exp.fast_equal(fact, goal)
         || Exp.fast_equal(strip_eq_true(fact), goal_nf)
           ? Some(id) : None
       );
  };

  /* Runs `step` to a fixpoint. The fuel bound is generous, since
   * `step_fn` has no termination signal. */
  let evaluate = (~step: step_fn, ~env, e: Exp.t): Exp.t => {
    let rec go = (fuel, e) =>
      switch (fuel <= 0 ? None : step(~env, ~at=Anywhere, e)) {
      | Some({outgoing, _}) => go(fuel - 1, outgoing)
      | None => e
      };
    go(1000, e);
  };

  /* How `goal`, env-substituted, is discharged: by lookup, or else by
   * closed evaluation to `true`. Only a goal with no free variables
   * (`2 != 0`) is evaluated, and not one that calls a recursive
   * function: only explicit steps (`eval`, `contradiction`) run those. */
  let find = (~step: step_fn, ~ctx: SemanticCtx.t, goal: Exp.t) => {
    /* Statics against an empty ctx leave an empty co-context exactly
     * when there are no free variables. */
    let is_closed = () => {
      let (statics, _) =
        Statics.mk(
          ~ana=Typ.temp(Atom(Bool)),
          CoreSettings.on,
          Ctx.empty,
          goal,
        );
      switch (Statics.Map.lookup_exp(Exp.rep_id(goal), statics)) {
      | Some(info) => Info.exp_co_ctx(info) == []
      | None => true
      };
    };
    /* After env substitution, every recursive function a goal can call
     * (written in the goal, in a user's closure or in a builtin) is a
     * `FixF` node in it. */
    let has_fixpoint = () => {
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
    switch (lookup(ctx, goal)) {
    | Some(fact_id) => Obligation.Remote(fact_id)
    | None when is_closed() && !has_fixpoint() =>
      is_true(Some(evaluate(~step, ~env=SemanticCtx.get_env(ctx), goal)))
        ? Obligation.Evaluated : Obligation.Pending
    | None => Obligation.Pending
    };
  };

  /* The obligation to prove `goal`, incurred at step `origin` and
   * discharged if it can be. See Obligation.re for why both goals are
   * kept. */
  let obligation =
      (
        ~step: step_fn,
        ~ctx: SemanticCtx.t,
        ~origin: Id.t,
        ~kind: Obligation.kind,
        goal: Exp.t,
      )
      : Obligation.t => {
    let substituted = goal |> Substitution.in_exp(SemanticCtx.get_env(ctx));
    Obligation.{
      origin,
      kind,
      bindings: SemanticCtx.get_ctx(ctx).entries,
      goal: substituted,
      display_goal: goal,
      discharge: find(~step, ~ctx, substituted),
    };
  };
};

/* The proposition a `revert`, `contradiction` or `alias` step cites.
 *
 * A bare hypothesis name is looked up in the theorem namespace, so
 * `revert ih` names the same thing `axiom ih` does. It is not
 * substituted: installed facts are already env-substituted, and
 * substituting one again alpha-renames the binders inside its inlined
 * closures, after which it no longer `fast_equal`s the stored fact.
 *
 * A spelled-out proposition is substituted, to bring the user's
 * vocabulary into the inlined form of the stored facts. */
let cited_fact = (~ctx: SemanticCtx.t, e: Exp.t): Exp.t => {
  let named =
    Option.bind(ProofSyntax.exp_to_name(e), name =>
      Ctx.lookup_theorem(SemanticCtx.get_ctx(ctx), name)
    );
  switch (named) {
  | Some({prop: Some(prop), _}) => prop
  | Some({prop: None, _})
  | None => e |> Substitution.in_exp(SemanticCtx.get_env(ctx))
  };
};

/* Definedness gates.
 *
 * Divergence (⊥) is never a boolean obligation: a failed
 * structural-totality check refuses the step (the PossiblyDivergent*
 * marks). Domain errors (err) can be stated as booleans: the domain
 * scan's conditions are incurred as ordinary obligations. Eval steps
 * have no gates: they preserve denotation. */
module Gate = {
  /* For the instantiations an axiom or lemma step's match bound: each
   * must be structurally total; `Error(var)` names the first that is not,
   * and the step is refused. Otherwise `Ok(conditions)` carries the
   * domain scan of the instantiations. Common instantiations (x := y,
   * x := 2, x := a + b) pass and scan to nothing. */
  let instantiations =
      (~ctx: SemanticCtx.t, instantiations: list((Var.t, Exp.t)))
      : result(list(Exp.t), Var.t) => {
    /* Substitution inlines visible definitions (so Totality checks their
     * bodies), recursive ones surface their FixF spine, and quantified
     * binders stay bare Vars. */
    let substituted =
      List.map(
        ((n, e)) =>
          (n, e |> Substitution.in_exp(SemanticCtx.get_env(ctx))),
        instantiations,
      );
    let divergent =
      List.find_map(
        ((n, e)) =>
          switch (Totality.check(~ctx=SemanticCtx.get_ctx(ctx), e)) {
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
        |> DomainConditions.dedup,
      )
    };
  };

  /* For an induction's scrutinee. Structural induction on a bare
   * quantified variable needs nothing: quantifiers range over total
   * values. A computed scrutinee is a boolean split, and within a branch
   * its case equation is a symmetric equation only if the scrutinee is
   * defined and terminates. So a possibly-divergent scrutinee is marked
   * and its domain conditions become obligations. */
  let scrutinee =
      (~step: step_fn, ~ctx: SemanticCtx.t, ~origin: Id.t, scrut: Exp.t)
      : (list(ProofMark.t), list(Obligation.t)) => {
    let scrut = scrut |> Substitution.in_exp(SemanticCtx.get_env(ctx));
    switch (ProofSyntax.exp_to_name(scrut)) {
    | Some(_) => ([], [])
    | None =>
      let marks =
        switch (Totality.check(~ctx=SemanticCtx.get_ctx(ctx), scrut)) {
        | Ok () => []
        | Error(_) => [ProofMark.PossiblyDivergentScrutinee]
        };
      let obligations =
        DomainConditions.scan(scrut)
        |> List.map(
             Discharge.obligation(~step, ~ctx, ~origin, ~kind=Definedness),
           );
      (marks, obligations);
    };
  };
};

/* `where` restrictions installed as hypotheses. */
module Where = {
  /* The `&&`-conjuncts of a guard, looking through parens. */
  let rec conjuncts = (g: Exp.t): list(Exp.t) =>
    switch (g |> Exp.term_of) {
    | Parens(g1) => conjuncts(g1)
    | BinOp(Bool(And), a, b) => conjuncts(a) @ conjuncts(b)
    | _ => [g]
    };

  /* Install a binder's `where` restriction as a hypothesis. Peeling a
   * restricted binder needs no obligation. Every peeling site
   * (`peel_stmt_binders` and the `Forall` proof form) goes through here so
   * they agree.
   *
   * A conjunctive guard also installs each conjunct as its own fact, so
   * `Discharge.lookup` finds them; `a && b` holding entails both `a` and
   * `b` under the Kleene reading. `definition_obligations` treats a
   * `fun p where g` contract the same way. The obligations panel's float
   * exit depends on it when the binder already has a guard, since
   * `ObligationsPanel.float_binder_exp` extends the guard with `&&`.
   *
   * All entries go under the one name `where`, and the whole guard is
   * installed last, so that `Ctx.lookup_theorem`, which takes the most
   * recent match, resolves `where` to the guard the user wrote, as
   * `Statics.proof_ctx_of_goal` records it. */
  let add_facts = (ctx: SemanticCtx.t, g: Exp.t): SemanticCtx.t => {
    let g = g |> Substitution.in_exp(SemanticCtx.get_env(ctx));
    let install = (ctx, fact) =>
      SemanticCtx.add_hypothesis(ctx, "where", fact) |> fst;
    let ctx =
      switch (conjuncts(g)) {
      | [_] => ctx
      | conjuncts => List.fold_left(install, ctx, conjuncts)
      };
    install(ctx, g);
  };

  /* `add_facts` for the guard a binder pattern carries, if any. */
  let add_pat_facts = (ctx: SemanticCtx.t, p: Pat.t): SemanticCtx.t =>
    switch (Exp.pat_guard(p)) {
    | Some(g) => add_facts(ctx, g)
    | None => ctx
    };
};

// Binders

/* Peel the outermost binder from a "for all pat, P" goal, for the
 * `Forall` proof form: the binder, its type, its `where` restriction
 * (which the caller installs as a hypothesis) and the body. */
let rec peel_binder =
        (goal: Exp.t): option((Pat.t, Typ.t, option(Exp.t), Exp.t)) =>
  switch (goal |> Exp.term_of) {
  /* `EditorTransform` wraps a spliced sub-term in `Parens` whenever it
   * shares a segment level with sibling pieces, so the obligations
   * panel's "Add to statement" exit turns
   * `forall y where g -> forall z -> P` into
   * `forall y where g -> (forall z where c -> P)`. If the parens hid the
   * binder, its `where` restriction would never be installed and the
   * floated condition would stay Pending. */
  | Parens(inner) => peel_binder(inner)
  /* `forall p where g -> _` is `Forall(Where(p, g), _)`: split the
   * guard back off the binder pattern. */
  | Fun(p, body, t, _) =>
    let t = OptUtil.get(() => Typ.fresh(Unknown(Internal)), t);
    let (p, g) = Exp.split_pat_guard(p);
    Some((p, t, g, body));
  | Forall(p, body) =>
    let (p, g) = Exp.split_pat_guard(p);
    Some((p, Typ.fresh(Unknown(Internal)), g, body));
  | _ => None
  };

/* Introduce a theorem statement's outer binders: extend the semantic ctx
 * with each binder's variables, install `where` restrictions as
 * hypotheses, and return the remaining core goal, which keeps any `==>`
 * antecedents (those are introduced with `assume`). Used by the
 * evaluator and the UI stepper to seed the proof goal. */
let rec peel_stmt_binders =
        (ctx: SemanticCtx.t, goal: Exp.t): (SemanticCtx.t, Exp.t) =>
  switch (goal |> Exp.term_of) {
  /* Transparent, for the reason spelled out on `peel_binder`. */
  | Parens(inner) => peel_stmt_binders(ctx, inner)
  | Forall(p, body) =>
    let ctx =
      SemanticCtx.add_from_pattern(ctx, p, Typ.fresh(Unknown(Internal)));
    peel_stmt_binders(Where.add_pat_facts(ctx, p), body);
  | _ => (ctx, goal)
  };

/* `ctx` without the facts that mention any of `names`, for a scope in
 * which those names are rebound: the facts are about the old binding.
 * Unlike `ProofCtx`'s capture check, this also hides them from
 * `Discharge.lookup`. */
let forget_facts_mentioning =
    (names: list(Var.t), ctx: SemanticCtx.t): SemanticCtx.t => {
  let base_ctx = SemanticCtx.get_ctx(ctx);
  let mentions = (fact: Exp.t) =>
    ProofRule.mentions_any(ProofRule.exp_to_rule(fact), names);
  SemanticCtx.of_ctx_and_env(
    {
      ...base_ctx,
      entries:
        List.filter(
          (entry: Ctx.entry) =>
            switch (entry) {
            | TheoremEntry({prop: Some(fact), _}) => !mentions(fact)
            | TheoremEntry({prop: None, _})
            | VarEntry(_)
            | ConstructorEntry(_)
            | TVarEntry(_)
            | LivelitEntry(_) => true
            },
          base_ctx.entries,
        ),
    },
    SemanticCtx.get_env(ctx),
  );
};

// Rewrite steps

/* A rewrite step's effect, for a step that only rewrites. */
let rewritten = (outgoing: Exp.t): step_result => {
  auto_incoming: [],
  auto_outgoing: [],
  outgoing,
};

/* `axiom <rule> end`: rewrite the goal with a rule in scope. */
module Axiom = {
  /* The rewritten goal, with the rule's assumptions instantiated by the
   * match (the conditions the step incurs as obligations) and the
   * metavariable instantiations the match bound (for
   * `Gate.instantiations`).
   *
   * Every binder of the rule must be assigned, by the match or by a
   * `with <var> = <exp>` clause (`~instantiation`), or the step is
   * refused (`UnderdeterminedInstantiation`). The `with` binding is
   * seeded into the match context before matching and returned with the
   * matched ones, so the same gates apply to it. */
  let apply =
      (
        ~info_map: Statics.Map.t,
        ~env: Environment.t(Exp.t),
        ~ctx: Ctx.t,
        ~direction: Direction.t,
        ~equality: string,
        ~instantiation: option((Var.t, Exp.t))=None,
        goal: Exp.t,
      )
      : result((Exp.t, list(Exp.t), list((Var.t, Exp.t))), ProofMark.t) => {
    open Result.Syntax;
    let proof_ctx = ProofCtx.of_theorem_ctx(~builtins=Axioms.v, ctx);
    let* rule =
      ProofCtx.lookup_rule(equality, proof_ctx)
      |> Option.to_result(~none=ProofMark.UnknownEquality(equality));
    /* A cited rule with a boolean conclusion `P` reads as `P == true`.
     * `~info_map` settles conclusions whose shape doesn't (a `where`
     * guard that is a plain variable, a Bool-typed application). */
    let rule = ProofRule.BoolReading.apply(~info_map=Some(info_map), rule);
    /* The `with` instantiation is env-substituted, like every other
     * expression the checker computes with. */
    let* bindings =
      switch (instantiation) {
      | None => Ok(None)
      | Some((var, inst)) =>
        ProofRule.Rewrite.seed(
          var,
          inst |> Substitution.in_exp(env),
          ProofRule.Rewrite.empty_bindings(rule.bindings),
        )
        |> Option.map(Option.some)
        |> Option.to_result(
             ~none=
               ProofMark.UnknownInstantiationVar({
                 equality,
                 var,
               }),
           )
      };
    /* An explicit citation names the direction, so the boolean reading's
     * `true |-> P` direction is offered here although rule discovery
     * suppresses it. */
    let (l, r) =
      ProofRule.Rewrite.matches(
        ~info_map,
        ~env,
        ~bindings?,
        ~reverse=true,
        rule,
        goal,
      );
    let rewrite =
      switch (direction) {
      | Direction.Left => l
      | Direction.Right => r
      };
    switch (rewrite) {
    | None =>
      Error(
        RuleDoesNotApply({
          equality,
          direction,
        }),
      )
    | Some((_, mctx)) when ProofRule.Rewrite.has_unassigned(mctx) =>
      Error(UnderdeterminedInstantiation({equality: equality}))
    | Some((w, mctx)) =>
      let conditions =
        List.map(MatchExp.substitute_exp(mctx), rule.assumptions);
      let instantiations =
        List.filter_map(
          ((n, (_, assigned))) => Option.map(e => (n, e), assigned),
          mctx,
        );
      Ok((Exp.replace_all_ids(w), conditions, instantiations));
    };
  };

  /* The step as written: the rule's instantiated assumptions and the
   * instantiations' domain conditions are its obligations. */
  let check =
      (
        ~step: step_fn,
        ~info_map: Statics.Map.t,
        ~ctx: SemanticCtx.t,
        ~origin: Id.t,
        ~direction: Direction.t,
        ~equality: Exp.t,
        ~instantiation: option((Exp.t, Exp.t)),
        goal: Exp.t,
      )
      : result((step_result, list(Obligation.t)), ProofMark.t) => {
    open Result.Syntax;
    let* equality =
      ProofSyntax.exp_to_name(equality)
      |> Option.to_result(~none=ProofMark.MalformedEqualityName);
    /* A non-variable `with <var>` slot is reported like an unknown binder
     * name. */
    let* instantiation =
      switch (instantiation) {
      | None => Ok(None)
      | Some((var, inst)) =>
        switch (ProofSyntax.exp_to_name(var)) {
        | Some(var) => Ok(Some((var, inst)))
        | None =>
          Error(
            ProofMark.UnknownInstantiationVar({
              equality,
              var: "?",
            }),
          )
        }
      };
    let* (out, conditions, instantiations) =
      apply(
        ~info_map,
        ~env=SemanticCtx.get_env(ctx),
        ~ctx=SemanticCtx.get_ctx(ctx),
        ~direction,
        ~equality,
        ~instantiation,
        goal,
      );
    switch (Gate.instantiations(~ctx, instantiations)) {
    | Error(var) =>
      Error(
        PossiblyDivergentInstantiation({
          equality,
          var,
        }),
      )
    | Ok(domain_conditions) =>
      /* A condition that is both a premise and a domain condition is
       * shown once, as the premise the rule states. */
      let premises = DomainConditions.dedup(conditions);
      let domain_conditions =
        DomainConditions.dedup(domain_conditions)
        |> List.filter(c => !List.exists(Exp.fast_equal(c), premises));
      let obligations =
        List.map(
          Discharge.obligation(~step, ~ctx, ~origin, ~kind=Premise),
          premises,
        )
        @ List.map(
            Discharge.obligation(~step, ~ctx, ~origin, ~kind=Definedness),
            domain_conditions,
          );
      Ok((rewritten(out), obligations));
    };
  };
};

/* `rewrite <replacement> end`: replace the goal with an expression the
 * CAS found equal to it. The CAS's field laws are false for IEEE floats,
 * so a Float-typed rewrite is refused. It reasons in a field, so both
 * sides' domain conditions are its obligations.
 *
 * TODO: re-verify the rewrite itself in the checker. The CAS runs only in
 * the browser (window.Algebrite; the node test harness has none), so the
 * checker trusts the UI for the step's equation and checks only its
 * domain obligations. */
module Algebrite = {
  let check =
      (
        ~step: step_fn,
        ~info_map: Statics.Map.t,
        ~ctx: SemanticCtx.t,
        ~origin: Id.t,
        ~replacement: Exp.t,
        goal: Exp.t,
      )
      : result((step_result, list(Obligation.t)), ProofMark.t) => {
    /* The statics first, with a syntactic fallback (a float literal or
     * float arithmetic at the head) for expressions the statics didn't
     * reach. */
    let is_float = (e: Exp.t): bool => {
      let rec float_head = (e: Exp.t): bool =>
        switch (e |> Exp.term_of) {
        | Parens(e1)
        | Projector(_, e1) => float_head(e1)
        | Atom(Float(_)) => true
        | UnOp(Float(_), _) => true
        | BinOp(Float(Plus | Minus | Times | Power | Divide), _, _) => true
        | _ => false
        };
      switch (Statics.Map.ty_of(Exp.rep_id(e), info_map)) {
      | Some(ty) =>
        switch (Typ.term_of(ty)) {
        | Atom(Float) => true
        | Unknown(_) => float_head(e)
        | _ => false
        }
      | None => float_head(e)
      };
    };
    let env = SemanticCtx.get_env(ctx);
    if (is_float(goal) || is_float(replacement)) {
      Error(ProofMark.FloatAlgebrite);
    } else {
      let obligations =
        DomainConditions.dedup(
          DomainConditions.scan(goal |> Substitution.in_exp(env))
          @ DomainConditions.scan(replacement |> Substitution.in_exp(env)),
        )
        |> List.map(
             Discharge.obligation(~step, ~ctx, ~origin, ~kind=Definedness),
           );
      Ok((rewritten(Exp.replace_all_ids(replacement)), obligations));
    };
  };
};

/* `eval`: one evaluation step at the root of the goal. */
module Eval = {
  let check =
      (~step: step_fn, ~env: Environment.t(Exp.t), goal: Exp.t)
      : result((step_result, list(Obligation.t)), ProofMark.t) =>
    step(~env, ~at=Root, goal)
    |> Option.map(result => (result, []))
    |> Option.to_result(~none=ProofMark.NothingToStep);
};

/* `funext x => body`: functional extensionality as an intro rule.
 *
 * It applies exactly when the goal is `l == r` and the two sides are
 * functions: an arrow at the head of the type. `==` between two lists of
 * functions is an induction, not extensionality, so `Typ.has_fun` (true
 * of `[a -> b]`) is the wrong test.
 *
 * Evidence, in order:
 *   1. Statics: the recorded type of either side is an `Arrow`. This is
 *      the answer whenever the statics reached the goal (the usual case
 *      in a theorem statement).
 *   2. Syntax: either side is literally a lambda (`fun` / `fun ... where`
 *      / a `Parens` around one). Covers goals statics left `Unknown`,
 *      and gives a domain annotation when the lambda carries one.
 *   3. The env: a bare variable naming a function definition.
 *      `inv == (fun x -> 1 / x)` has a lambda on the right, so this is
 *      mostly for the both-sides-named case. */
module Funext = {
  /* Follows a bare variable through the env to its definition. */
  let rec syntactic_domain =
          (~env: Environment.t(Exp.t), e: Exp.t): option(Typ.t) =>
    switch (e |> Exp.term_of) {
    | Parens(inner) => syntactic_domain(~env, inner)
    | Fun(_, _, Some(t), _) =>
      switch (Typ.term_of(t)) {
      | Arrow(d, _) => Some(d)
      | _ => Some(Typ.fresh(Unknown(Internal)))
      }
    | Fun(_, _, None, _) => Some(Typ.fresh(Unknown(Internal)))
    /* A closure/fix wrapper around a lambda still denotes a function. */
    | Closure(_, inner)
    | FixF(_, inner, _) => syntactic_domain(~env, inner)
    | Var(x) =>
      switch (Environment.lookup(env, x)) {
      /* Guard against a self-referential binding looping the lookup. */
      | Some({term: Var(y), _}) when y == x => None
      | Some(d) => syntactic_domain(~env, d)
      | None => None
      }
    | _ => None
    };

  /* `Some((l, r, domain))` when `goal` is `l == r` at an arrow type. The
   * domain is the type to bind the funext binder at: `Unknown(Internal)`
   * when the sides are known to be functions but not at what type, as
   * `peel_binder` does for a `Forall` binder. */
  let rec goal =
          (~info_map: Statics.Map.t, ~env: Environment.t(Exp.t), g: Exp.t)
          : option((Exp.t, Exp.t, Typ.t)) =>
    switch (g |> Exp.term_of) {
    /* Parens are transparent here for the same reason they are in
     * `peel_binder`: an editor splice must not hide the equality. */
    | Parens(inner) => goal(~info_map, ~env, inner)
    | BinOp(Poly(Equals), l, r) =>
      let statics_domain = (e: Exp.t): option(Typ.t) =>
        switch (
          Option.map(
            Typ.term_of,
            Statics.Map.ty_of(Exp.rep_id(e), info_map),
          )
        ) {
        | Some(Arrow(d, _)) => Some(d)
        | Some(_)
        | None => None
        };
      let domain =
        switch (statics_domain(l), statics_domain(r)) {
        | (Some(d), _)
        | (None, Some(d)) => Some(d)
        | (None, None) =>
          switch (syntactic_domain(~env, l)) {
          | Some(d) => Some(d)
          | None => syntactic_domain(~env, r)
          }
        };
      domain |> Option.map(d => (l, r, d));
    | _ => None
    };

  /* The binder a newly inserted `funext` step starts with: the left-hand
   * function's parameter pattern, if it has one, so
   * `(fun x -> x + 1) == (fun x -> 1 + x)` opens as `funext x => ?`.
   * Only the left side is read, so the choice is predictable when the
   * sides name their parameters differently. The checker reads whatever
   * binder is written.
   *
   * The left side is read as `syntactic_domain` reads it, and the whole
   * pattern comes back, not a name: `fun (x, y) ->` has to be bound by
   * `funext (x, y) =>`. It is copied with fresh ids, because it is
   * written into new syntax and reusing the source's ids would make two
   * places in the program claim the same tiles. `None` when there is no
   * lambda to read (both sides opaque at an arrow type), and the caller
   * keeps a hole. */
  let binder =
      (~info_map: Statics.Map.t, ~env: Environment.t(Exp.t), g: Exp.t)
      : option(Pat.t) => {
    let fresh_copy: Pat.t => Pat.t = {
      let f:
        'a.
        (IdTagged.t('a) => IdTagged.t('a), IdTagged.t('a)) =>
        IdTagged.t('a)
       =
        (continue, x) =>
          {
            ...x,
            annotation: {
              /* Keep the lexeme (hole flavor, unknown-op tokens) so the
                 copy still displays faithfully; drop the tile
                 provenance. */
              ...IdTagged.IdTag.mk_internal([Id.mk()]),
              lexeme: x.annotation.lexeme,
            },
          }
          |> continue;
      p => Pat.map_term(~f_exp=f, ~f_pat=f, ~f_typ=f, ~f_tpat=f, ~f_rul=f, p);
    };
    let rec param = (e: Exp.t): option(Pat.t) =>
      switch (e |> Exp.term_of) {
      | Parens(inner)
      | Asc(inner, _)
      | Closure(_, inner)
      | FixF(_, inner, _) => param(inner)
      | Fun(p, _, _, _) => Some(fresh_copy(p))
      | Var(x) =>
        switch (Environment.lookup(env, x)) {
        | Some({term: Var(y), _}) when y == x => None
        | Some(d) => param(d)
        | None => None
        }
      | _ => None
      };
    switch (goal(~info_map, ~env, g)) {
    | Some((l, _, _)) => param(l)
    | None => None
    };
  };

  /* The body's goal `l(x) == r(x)`, for the funext binder read as the
   * expression `arg`. `env` maps the binder to itself (`x |-> Var(x)`),
   * which Substitution leaves alone, so the pointwise goal keeps
   * mentioning `x` while every other name a contracted lambda body
   * refers to is resolved.
   *
   * When a side is a literal lambda, the application is a beta-redex
   * whose argument is a bound variable (hence a value), so contracting
   * it is definitional: it renames the parameter and discovers nothing.
   * Contracting here lets `(fun x -> x + 1) == (fun x -> 1 + x)` be
   * proved by a pointwise `rewrite` without a leading `eval` on each
   * side. Not contracted:
   *   - a named function (`inv(x)`, `f(x)`). Unfolding a definition is
   *     `eval`'s job, and keeping the `Ap` is why `refl_eq` closes
   *     `inv(x) == inv(x)` without ever applying `inv`.
   *   - a contract lambda (`fun p where g -> body`). Contracting it would
   *     drop `g`, and funext adds no `where` restriction of its own:
   *     partiality surfaces at the step that applies the function.
   *   - a non-variable parameter pattern (a tuple, say): there is no
   *     single name to substitute for. */
  let pointwise =
      (~env: Environment.t(Exp.t), ~arg: Exp.t, l: Exp.t, r: Exp.t): Exp.t => {
    let rec apply = (f: Exp.t): Exp.t =>
      switch (f |> Exp.term_of) {
      | Parens(inner) => apply(inner)
      | Fun(p, body, _, _) =>
        switch (ProofSyntax.pat_to_name(p)) {
        /* The body is substituted under the whole env, not just
         * `x |-> arg`. A literal lambda can mention let-bound names from
         * the surrounding proof (`let f = ... in theorem ... funext y =>`),
         * and if those stay free `Var`s here, the later per-step env
         * substitution replaces them with fresh-id copies that no longer
         * match the ids in the goal, so every `eval`/`rewrite` lookup
         * misses. `extend` puts the binder last so it shadows any
         * same-named outer binding. */
        | Some(x) =>
          body |> Substitution.in_exp(Environment.extend(env, (x, arg)))
        | None => Exp.fresh(Ap(Forward, f, arg))
        }
      | _ => Exp.fresh(Ap(Forward, f, arg))
      };
    /* The goal is spliced together from trees that still carry their
     * source ids, so it is freshened whole:
     *   - the same `arg` node is written into both sides, so one
     *     expression claims the binder's tiles twice, and
     *     `Highlight.of_tile` fails on a double-click in the inner
     *     stepper;
     *   - `l`/`r` are the incoming goal's own subtrees, so clicking one of
     *     their tokens parks the caret in the outer occurrence.
     * This is what `StepperPicker.Terms.embed_exp` does when splicing an
     * expression into proof syntax. It has to happen before the ProofMap
     * records the body's incoming goal, since the inner stepper and every
     * lookup on the body resolve against this tree. */
    Exp.fresh(BinOp(Poly(Equals), apply(l), apply(r)))
    |> Exp.replace_all_ids;
  };
};

/* Definition-time discharge of function contracts.
 *
 * When a theorem is checked, every function definition its proof context
 * can see (env-bound, non-builtin, non-recursive) is scanned once:
 *   - `fun p where g -> body`: each of the body's domain conditions is
 *     discharged if it is `Exp.fast_equal` to the guard `g` or one of
 *     its `&&`-conjuncts; leftover conditions become obligations.
 *   - `fun p -> body`: every body condition is an obligation, recorded
 *     here once rather than at every call (DomainConditions.scan doesn't
 *     descend into function bodies).
 *
 * Leftovers are recorded in the ProofMap under the function term's id,
 * a non-proof id, so the entry has only obligations, each with `origin`
 * the function's id. Recording is idempotent across theorems. Because
 * the function's id is not inside any proof subtree, these obligations
 * don't affect Proven/ProvenModulo status; they are shown at the
 * definition in the obligations panel.
 *
 * The body scan runs with the parameter's bindings visible, so
 * conditions may mention the parameter. */
let definition_obligations = (~step: step_fn, ~ctx: SemanticCtx.t): ProofMap.t => {
  let env = SemanticCtx.get_env(ctx);
  let record_leftovers = (m, fun_term: Exp.t, p: Pat.t, guard, body) => {
    let conjuncts =
      guard |> Option.map(Where.conjuncts) |> Option.value(~default=[]);
    let leftover =
      DomainConditions.scan(body)
      |> List.filter(c => !List.exists(Exp.fast_equal(c), conjuncts));
    switch (leftover) {
    | [] => m
    | [_, ..._] =>
      let fun_id = Exp.rep_id(fun_term);
      let ctx =
        SemanticCtx.add_from_pattern(ctx, p, Typ.fresh(Unknown(Internal)));
      let obligations =
        List.map(
          Discharge.obligation(~step, ~ctx, ~origin=fun_id, ~kind=Contract),
          leftover,
        );
      record(~ctx, ~obligations, fun_id, None, None, m);
    };
  };
  Environment.to_list(env)
  |> List.fold_left(
       (m, (name, value)) =>
         if (Totality.is_builtin(SemanticCtx.get_ctx(ctx), name)) {
           m;
         } else {
           /* Resolve closures and inline visible definitions so the body
            * scan sees concrete terms; substitution keeps the id of the
            * function node itself. */
           let v = ProofSyntax.unwrap(value |> Substitution.in_exp(env));
           switch (v |> Exp.term_of) {
           /* `fun p where g -> body` is `Fun(Where(p, g), body)`: split
              the contract guard back off the parameter. */
           | Fun(p, body, _, _) =>
             let (p, g) = Exp.split_pat_guard(p);
             record_leftovers(m, v, p, g, body);
           /* Recursive definitions (FixF), non-functions and self-map
              Vars are skipped. */
           | _ => m
           };
         },
       ProofMap.empty,
     );
};

// The walk

/* An axiom, algebrite or eval step rewrites the goal at its root. With no
 * goal there is nothing to rewrite; a step that doesn't apply is marked
 * and passes the goal through. */
let check_rewrite =
    (
      ~ctx: SemanticCtx.t,
      ~id: Id.t,
      incoming: option(Exp.t),
      apply:
        Exp.t => result((step_result, list(Obligation.t)), ProofMark.t),
    )
    : (option(Exp.t), ProofMap.t) =>
  switch (incoming) {
  | None => (
      None,
      record(
        ~ctx,
        ~marks=[ProofMark.MissingIncoming],
        id,
        None,
        None,
        ProofMap.empty,
      ),
    )
  | Some(goal) =>
    switch (apply(goal)) {
    | Ok(({auto_incoming, auto_outgoing, outgoing}, obligations)) =>
      let outgoing = Some(outgoing);
      (
        outgoing,
        record(
          ~ctx,
          ~auto_incoming,
          ~auto_outgoing,
          ~obligations,
          id,
          incoming,
          outgoing,
          ProofMap.empty,
        ),
      );
    | Error(mark) => (
        incoming,
        record(~ctx, ~marks=[mark], id, incoming, incoming, ProofMap.empty),
      )
    }
  };

/* Threads `incoming` through the proof tree, returning the outgoing goal
 * (if the proof propagates) and a proof map with an entry at every proof
 * sub-term id. */
let rec check =
        (
          ~step: step_fn,
          ~info_map: Statics.Map.t,
          ~ctx: SemanticCtx.t,
          incoming: option(Exp.t),
          proof: Proof.t,
        )
        : (option(Exp.t), ProofMap.t) => {
  let id = Proof.rep_id(proof);
  switch (proof.term) {
  /* A hole is an unfinished proof, not an error: it passes the goal
   * through unmarked, so later steps keep working. */
  | EmptyHole => (
      incoming,
      record(~ctx, id, incoming, incoming, ProofMap.empty),
    )
  /* Unparseable proof text, or a multi-hole, whose children aren't proof
   * terms. */
  | Invalid(_)
  | MultiHole(_) => (
      incoming,
      record(
        ~ctx,
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
    (out2, record(~ctx, id, incoming, out2, ProofMap.union(m1, m2)));
  /* The parens node records the inner step's outgoing too, so the UI can
   * key off either id. */
  | Parens(p) =>
    let (out, m) = check(~step, ~info_map, ~ctx, incoming, p);
    (out, record(~ctx, id, incoming, out, m));
  | AxiomStep({direction, equality, instantiation}) =>
    check_rewrite(
      ~ctx,
      ~id,
      incoming,
      Axiom.check(
        ~step,
        ~info_map,
        ~ctx,
        ~origin=id,
        ~direction,
        ~equality,
        ~instantiation,
      ),
    )
  | AlgebriteStep(replacement) =>
    check_rewrite(
      ~ctx,
      ~id,
      incoming,
      Algebrite.check(~step, ~info_map, ~ctx, ~origin=id, ~replacement),
    )
  | EvalStep =>
    check_rewrite(
      ~ctx,
      ~id,
      incoming,
      Eval.check(~step, ~env=SemanticCtx.get_env(ctx)),
    )
  | Focus({ordinal, selector, body}) =>
    check_focus(
      ~step,
      ~info_map,
      ~ctx,
      ~id,
      incoming,
      ordinal |> Option.map(Annotated.term_of) |> Option.value(~default=1),
      selector,
      body,
    )
  | Forall(_pat, body) =>
    check_forall(~step, ~info_map, ~ctx, ~id, incoming, body)
  | Funext(pat, body) =>
    check_funext(~step, ~info_map, ~ctx, ~id, incoming, pat, body)
  | Assume(e, as_name, body) =>
    check_assume(~step, ~info_map, ~ctx, ~id, incoming, e, as_name, body)
  | Have(e, sub, body) =>
    check_have(~step, ~info_map, ~ctx, ~id, incoming, e, sub, body)
  | Generalize(e, body) =>
    check_generalize(~step, ~info_map, ~ctx, ~id, incoming, e, body)
  | Revert(e, instantiation, body) =>
    check_revert(
      ~step,
      ~info_map,
      ~ctx,
      ~id,
      incoming,
      e,
      instantiation,
      body,
    )
  | Contradiction(e, instantiation) =>
    check_contradiction(~step, ~ctx, ~id, incoming, e, instantiation)
  | Alias(x, e, body) =>
    check_alias(~step, ~info_map, ~ctx, ~id, incoming, x, e, body)
  | Induction(scrut, as_name, cases) =>
    check_induction(
      ~step,
      ~info_map,
      ~ctx,
      ~id,
      incoming,
      scrut,
      as_name,
      cases,
    )
  };
}

/* A form that can't act on the goal: mark it, pass the goal through, and
 * check the body against the unchanged goal so later steps stay
 * meaningful. */
and pass_through =
    (~step, ~info_map, ~ctx, ~id, ~marks, incoming, body)
    : (option(Exp.t), ProofMap.t) => {
  let (_, m) = check(~step, ~info_map, ~ctx, incoming, body);
  (incoming, record(~ctx, ~marks, id, incoming, incoming, m));
}

/* A form whose body proves a goal of its own instead of the incoming one
 * (a peeled binder's body, a pointwise equation, a re-quantified or
 * reverted goal). The body's partial result is about that goal, so the
 * form discharges the incoming goal only when the body reaches `true`;
 * otherwise the incoming goal passes through. */
and check_subgoal =
    (
      ~step,
      ~info_map,
      ~ctx,
      ~body_ctx=ctx,
      ~id,
      ~marks=[],
      ~obligations=[],
      incoming,
      subgoal: option(Exp.t),
      body,
    )
    : (option(Exp.t), ProofMap.t) => {
  let (out_body, m) = check(~step, ~info_map, ~ctx=body_ctx, subgoal, body);
  let outgoing = is_true(out_body) ? out_body : incoming;
  (outgoing, record(~ctx, ~marks, ~obligations, id, incoming, outgoing, m));
}

/* `from <ordinal> <selector> do <body> end`: the body is a chain of its
 * own whose incoming goal is the selected occurrence, and the focus's
 * outgoing is the goal with that occurrence replaced by the body's
 * outgoing. Every step of the body relates its goals by `==`, so by
 * congruence the replaced goal equals the incoming one. The body's
 * outgoing keeps its ids in the result; its ids are the occurrence's own
 * or fresh, since every rewrite step freshens what it writes.
 *
 * The occurrence may sit under binders of the goal (a `fun`, a
 * `forall`, a `let` body, a case arm). In the body each variable they
 * bind is fresh and arbitrary, as `forall x =>` makes it: it is bound to
 * itself in the env and the facts that mention its name are dropped,
 * since they are about an outer variable of that name. Rewriting under
 * a binder is then sound because the body's steps hold for every value
 * of the variable.
 *
 * A selector with no such occurrence is marked, the goal passes through,
 * and the body is checked with no goal. */
and check_focus =
    (~step, ~info_map, ~ctx, ~id, incoming, ordinal, selector, body)
    : (option(Exp.t), ProofMap.t) => {
  let env = SemanticCtx.get_env(ctx);
  let target =
    Option.bind(incoming, goal =>
      Occurrence.nth_env_target(~info_map, ~env, selector, ordinal - 1, goal)
      |> Option.map(target => (goal, target))
    );
  switch (incoming, target) {
  | (None, _) =>
    check_subgoal(
      ~step,
      ~info_map,
      ~ctx,
      ~id,
      ~marks=[ProofMark.MissingIncoming],
      None,
      None,
      body,
    )
  | (Some(_), None) =>
    let (_, m) = check(~step, ~info_map, ~ctx, None, body);
    (
      incoming,
      record(
        ~ctx,
        ~marks=[
          ProofMark.PatternNotFound({
            selector,
            ordinal,
          }),
        ],
        id,
        incoming,
        incoming,
        m,
      ),
    );
  | (Some(_), Some((goal, target))) =>
    let binders = Occurrence.binders(Exp.rep_id(target), goal);
    let body_ctx =
      List.fold_left(
        (ctx, p) =>
          SemanticCtx.add_from_pattern(
            forget_facts_mentioning(
              p |> Pat.bindings |> Binding.variable_names,
              ctx,
            ),
            p,
            Typ.fresh(Unknown(Internal)),
          ),
        ctx,
        binders,
      );
    let (out_body, m) =
      check(~step, ~info_map, ~ctx=body_ctx, Some(target), body);
    let outgoing =
      switch (out_body) {
      | Some(out) =>
        Some(Occurrence.replace_by_id(Exp.rep_id(target), goal, out))
      | None => incoming
      };
    (outgoing, record(~ctx, id, incoming, outgoing, m));
  };
}

/* The binder comes from the incoming goal, not the proof's pattern. */
and check_forall = (~step, ~info_map, ~ctx, ~id, incoming, body) =>
  switch (Option.map(peel_binder, incoming)) {
  | None =>
    check_subgoal(
      ~step,
      ~info_map,
      ~ctx,
      ~id,
      ~marks=[ProofMark.MissingIncoming],
      None,
      None,
      body,
    )
  | Some(Some((p, t, guard, inner))) =>
    let body_ctx = SemanticCtx.add_from_pattern(ctx, p, t);
    let body_ctx =
      Option.fold(~none=body_ctx, ~some=Where.add_facts(body_ctx), guard);
    check_subgoal(
      ~step,
      ~info_map,
      ~ctx,
      ~body_ctx,
      ~id,
      incoming,
      Some(inner),
      body,
    );
  /* No binder to peel: mark it, but let the body keep working on the goal
   * as is. */
  | Some(None) =>
    check_subgoal(
      ~step,
      ~info_map,
      ~ctx,
      ~id,
      ~marks=[ProofMark.ExpectedForallGoal],
      incoming,
      incoming,
      body,
    )
  }

/* Functional extensionality as an intro rule rather than an axiom: as a
 * rewrite equality it would have to quantify over functions at unknown
 * or hole types.
 *
 * Incoming `f == g` at an arrow type; the body's incoming is the
 * pointwise `f(x) == g(x)`, with `x` bound at the domain type. One arrow
 * per `funext`: for a curried `a -> b -> c`, `f(x) == g(x)` is still at
 * an arrow type, and a second `funext y =>` peels again. On failure, as
 * for `forall`: mark, and let the body keep working on the goal as
 * written. */
and check_funext = (~step, ~info_map, ~ctx, ~id, incoming, pat, body) => {
  let fail = mark =>
    check_subgoal(
      ~step,
      ~info_map,
      ~ctx,
      ~id,
      ~marks=[mark],
      incoming,
      incoming,
      body,
    );
  switch (incoming) {
  | None => fail(ProofMark.MissingIncoming)
  | Some(goal) =>
    switch (Funext.goal(~info_map, ~env=SemanticCtx.get_env(ctx), goal)) {
    | None => fail(ProofMark.ExpectedFunEqGoal)
    /* The binder must be something we can actually bind, else the
     * pointwise goal would mention a name nothing introduces. */
    | Some((_, _, domain))
        when Option.is_none(SemanticCtx.bind_pattern(pat, domain, Ctx.empty)) =>
      fail(ProofMark.MalformedFunext)
    | Some((l, r, domain)) =>
      let body_ctx = SemanticCtx.add_from_pattern(ctx, pat, domain);
      let pointwise =
        Funext.pointwise(
          ~env=SemanticCtx.get_env(body_ctx),
          ~arg=Exp.of_pat(pat),
          l,
          r,
        );
      check_subgoal(
        ~step,
        ~info_map,
        ~ctx,
        ~body_ctx,
        ~id,
        incoming,
        Some(pointwise),
        body,
      );
    }
  };
}

/* Hypothesize `e` for the body's scope. Two readings:
 *   - Implication intro: if the incoming goal is `A ==> B` and `e` is
 *     `Exp.fast_equal` to A, the body's incoming is B and no obligation
 *     is incurred.
 *   - Otherwise the goal is unchanged and the step incurs an obligation
 *     to establish `e`, discharged against the enclosing scope's facts so
 *     an assume never discharges itself.
 * The outgoing is the body's. */
and check_assume = (~step, ~info_map, ~ctx, ~id, incoming, e, as_name, body) => {
  let env = SemanticCtx.get_env(ctx);
  let hyp = e |> Substitution.in_exp(env);
  let consequent =
    switch (Option.map(Exp.term_of, incoming)) {
    | Some(BinOp(Bool(Implies), a, b))
        when Exp.fast_equal(a |> Substitution.in_exp(env), hyp) =>
      Some(b)
    | _ => None
    };
  let (body_incoming, obligations) =
    switch (consequent) {
    | Some(b) => (Some(b), [])
    | None => (
        incoming,
        [Discharge.obligation(~step, ~ctx, ~origin=id, ~kind=Assumption, e)],
      )
    };
  /* Without an `as <h>` clause the name is `assume`, which shadows any
   * enclosing one; naming a nested assumption keeps the outer one
   * citable. */
  let (body_ctx, _) =
    SemanticCtx.add_hypothesis(
      ctx,
      as_name_or(~default="assume", as_name),
      hyp,
    );
  let (out_body, m) =
    check(~step, ~info_map, ~ctx=body_ctx, body_incoming, body);
  (out_body, record(~ctx, ~obligations, id, incoming, out_body, m));
}

/* `have <exp> proof <subproof> => <body>`, the cut rule. With incoming
 * goal `G` it incurs two obligations, both recorded on the have's own
 * node, as a two-condition axiom would:
 *
 *   1. prove `<exp>` in the enclosing scope (if the subproof could cite
 *      the have's own hypothesis, `have P proof <cite P> => ...` would
 *      prove anything);
 *   2. prove `G` in the scope extended with `<exp>`, named `have`.
 *
 * Obligation 1 is dropped when the subproof reaches `true` with a clean
 * subtree, obligation 2 when the body does. An open premise is a Pending
 * obligation, not a broken step, so the form has no marks.
 *
 * Obligation 1 can also be discharged like any other, so an
 * already-known `<exp>` needs no subproof. Obligation 2 can't: its scope
 * contains the new `have` hypothesis, and lookup would discharge it
 * whenever `G` equals `<exp>`, trivially.
 *
 * The hypothesis is installed whether or not the subproof is finished,
 * so obligations incurred by steps inside the body can discharge against
 * it by lookup.
 *
 * The outgoing is the body's, so obligation 2 is Pending exactly when
 * the have's own outgoing is not a clean `true`, i.e. when
 * `full_status_of_proof` already reads Incomplete; a pending second
 * premise never reads as ProvenModulo. The one shape this misreports is
 * `Seq(Have(...), tail)` with the goal closed in the tail, which is
 * outside the have's scope anyway and which the stepper never writes. */
and check_have = (~step, ~info_map, ~ctx, ~id, incoming, e, sub, body) => {
  let proves = (out, m, p) => is_true(out) && ProofMap.proof_is_clean(m, p);
  let hyp = e |> Substitution.in_exp(SemanticCtx.get_env(ctx));
  let (out_sub, m_sub) = check(~step, ~info_map, ~ctx, Some(hyp), sub);
  let sub_obligations =
    proves(out_sub, m_sub, sub)
      ? []
      : [Discharge.obligation(~step, ~ctx, ~origin=id, ~kind=Premise, e)];
  let (body_ctx, _) = SemanticCtx.add_hypothesis(ctx, "have", hyp);
  let (out_body, m_body) =
    check(~step, ~info_map, ~ctx=body_ctx, incoming, body);
  /* In the extended scope, so the panel's local context for this row
   * shows the `have` hypothesis. */
  let body_obligations =
    switch (incoming) {
    | Some(goal) when !proves(out_body, m_body, body) => [
        Obligation.{
          origin: id,
          kind: Premise,
          bindings: SemanticCtx.get_ctx(body_ctx).entries,
          goal,
          /* The incoming goal is already env-inlined; there is no
           * as-written form. */
          display_goal: goal,
          discharge: Pending,
        },
      ]
    | Some(_)
    | None => []
    };
  (
    out_body,
    record(
      ~ctx,
      ~obligations=sub_obligations @ body_obligations,
      id,
      incoming,
      out_body,
      ProofMap.union(m_sub, m_body),
    ),
  );
}

/* Re-quantify an already-peeled binder x: with incoming goal G, the
 * body's incoming goal is `forall x -> G`, or `forall x where g -> G`
 * when x carries a recoverable `where` restriction (below). Soundness:
 * `forall x -> G` denoting true entails G at the ambient x; for a
 * restricted binder, the guard's ambient instance is the `where`
 * hypothesis installed when x was peeled. (Dropping the restriction
 * would also be sound, but the goal could be unprovable.)
 *
 * Capture: inside the body, every fact whose statement mentions x
 * (assume-hypotheses, case_eq, IHs, the where guard itself) is about the
 * old x, so those entries are removed from the body's scope. One filter
 * over the theorem namespace covers both `Discharge.lookup` and rule
 * lookup (`ProofCtx.of_theorem_ctx`); `is_captured` would cover only rule
 * lookup, and only once the body re-peels the binder. The test is free
 * occurrence (`ProofRule.mentions_any`), so a lemma `forall x -> ...`
 * that binds its own x stays available.
 *
 * Restriction: x's restriction is recovered as the `where` hypotheses
 * whose fact mentions x, conjoined with `&&`. Re-attaching an ambient
 * hypothesis as a binder guard is sound whichever binder it came from:
 * it weakens the generalized statement, and its ambient instance is
 * discharged by that same hypothesis. */
and check_generalize = (~step, ~info_map, ~ctx, ~id, incoming, e, body) => {
  let base_ctx = SemanticCtx.get_ctx(ctx);
  let in_scope =
    ProofSyntax.exp_to_name(e)
    |> OptUtil.filter(x => Ctx.lookup_var(base_ctx, x) != None);
  switch (incoming, in_scope) {
  | (None, _) =>
    pass_through(
      ~step,
      ~info_map,
      ~ctx,
      ~id,
      ~marks=[ProofMark.MissingIncoming],
      None,
      body,
    )
  /* The argument isn't a bare in-scope variable. */
  | (Some(_), None) =>
    pass_through(
      ~step,
      ~info_map,
      ~ctx,
      ~id,
      ~marks=[ProofMark.MalformedGeneralize],
      incoming,
      body,
    )
  | (Some(goal), Some(x)) =>
    let mentions_x = (fact: Exp.t) =>
      ProofRule.mentions_any(ProofRule.exp_to_rule(fact), [x]);
    let guards =
      SemanticCtx.facts(ctx)
      |> List.filter_map(((name, _id, fact)) =>
           name == "where" && mentions_x(fact) ? Some(fact) : None
         );
    let binder = Pat.fresh(Var(x));
    let subgoal =
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
    /* x's own binder entry stays: it is inert once no fact mentions it
     * (its env binding is the identity `x ↦ Var(x)`), and the body's
     * re-peel of the new binder rebinds it anyway. */
    let body_ctx = forget_facts_mentioning([x], ctx);
    check_subgoal(
      ~step,
      ~info_map,
      ~ctx,
      ~body_ctx,
      ~id,
      incoming,
      Some(subgoal),
      body,
    );
  };
}

/* Move an in-scope fact back into the goal, the inverse of assume's
 * implication intro. With incoming goal `G` and an in-scope fact `F`
 * matching the cited argument, the body's incoming goal is `F ==> G`.
 *
 * No obligation: `F` holds in this scope, so under the Kleene reading
 * `F ==> G` denotes exactly what `G` denotes. The step only moves a fact
 * from the context into the goal, where eval and rewrite steps can
 * compute with it.
 *
 * The fact stays in scope and citable (by lookup and, through the
 * boolean reading, as a rewrite rule). That is what makes the ex falso
 * idiom work: revert the contradictory fact, rewrite it with other facts
 * (e.g. a `case_eq`) until the antecedent evaluates to `false`, and
 * `false ==> G` evaluates to `true`.
 *
 * With `with x = v`, the antecedent is the fact instantiated at `v`.
 * Sound for the same reason, once `v` passes the instantiation gate as a
 * matched instantiation would at an axiom step: `forall x -> F(x)` holds
 * in this scope, so `F(v)` does, so `F(v) ==> G` denotes what `G`
 * denotes.
 *
 * The fact is found by `Discharge.lookup`, so the cited expression must
 * name it as it stands. A failure marks the step and passes the goal
 * through. */
and check_revert =
    (~step, ~info_map, ~ctx, ~id, incoming, e, instantiation, body) => {
  open Result.Syntax;
  let fact = cited_fact(~ctx, e);
  let instantiate = ((var, inst)) => {
    let* x =
      ProofSyntax.exp_to_name(var)
      |> Option.to_result(
           ~none=ProofMark.RevertFactNotQuantified({var: "?"}),
         );
    let inst = inst |> Substitution.in_exp(SemanticCtx.get_env(ctx));
    let* instantiated =
      ProofRule.instantiate_binder(x, inst, fact)
      |> Option.to_result(~none=ProofMark.RevertFactNotQuantified({var: x}));
    switch (Gate.instantiations(~ctx, [(x, inst)])) {
    | Error(var) =>
      Error(
        ProofMark.PossiblyDivergentInstantiation({
          equality: x,
          var,
        }),
      )
    | Ok(conditions) =>
      Ok((
        instantiated,
        List.map(
          Discharge.obligation(~step, ~ctx, ~origin=id, ~kind=Definedness),
          conditions,
        ),
      ))
    };
  };
  let fail = mark =>
    pass_through(~step, ~info_map, ~ctx, ~id, ~marks=[mark], incoming, body);
  switch (incoming, Discharge.lookup(ctx, fact)) {
  | (None, _) => fail(ProofMark.MissingIncoming)
  | (Some(_), None) => fail(ProofMark.UnknownFactReverted)
  | (Some(goal), Some(_)) =>
    let antecedent =
      switch (instantiation) {
      | None => Ok((fact, []))
      | Some(instantiation) => instantiate(instantiation)
      };
    switch (antecedent) {
    | Error(mark) => fail(mark)
    | Ok((antecedent, obligations)) =>
      check_subgoal(
        ~step,
        ~info_map,
        ~ctx,
        ~id,
        ~obligations,
        incoming,
        Some(Exp.fresh(BinOp(Bool(Implies), antecedent, goal))),
        body,
      )
    };
  };
}

/* `contradiction F end` / `contradiction F with x = e end`: ex falso as
 * one terminal step. It closes any goal by showing that the in-scope
 * fact `F` is false under the rest of the scope's knowledge.
 *
 * Soundness: `F` being in scope means `F` denotes `true` in this branch.
 * If `F`, rewritten by an equation the branch also knows, denotes
 * `false`, the branch's hypotheses are jointly unsatisfiable and the
 * branch is vacuous, so concluding `true` is the Kleene reading of ex
 * falso quodlibet. It is what `revert F => ...rewrite...; eval` proves,
 * in one step, once the antecedent is `false`.
 *
 * Only two things are trusted, both trusted elsewhere too:
 *   1. the step's `with` rewrite, which the user writes and the checker
 *      verifies against an in-scope equation (below); and
 *   2. `Discharge.evaluate`, under the same fuel bound as discharge by
 *      closed evaluation. Each step preserves the denotation for every
 *      assignment of the free variables, so reaching `false` means `F`
 *      denotes `false` here.
 *
 * Unlike discharge by closed evaluation, the term need not be closed:
 * that discharge concludes a positive fact and keeps closedness as its
 * boundary, while here the conclusion rests only on evaluation
 * preserving denotation. Open terms matter in practice:
 * `is_value(TmVar(m0)) == true` is false for every `m0` because the
 * match only inspects the constructor. An open term that gets stuck
 * fails to reach `false` and is marked, as are `true` and running out of
 * fuel. Every failure passes the goal through.
 *
 * The applied binding is recorded in the ProofMap entry's
 * `substitutions` field. */
and check_contradiction = (~step, ~ctx, ~id, incoming, e, instantiation) => {
  let env = SemanticCtx.get_env(ctx);
  let fact = cited_fact(~ctx, e);
  /* The step's one rewrite: the user's `with <var> = <exp>` clause,
   * verified against the facts in scope. The checker doesn't search for
   * equations; it only confirms that the branch already knows the one
   * the user named.
   *
   * `<var> == <exp>` (or `<exp> == <var>`) must be found by
   * `Discharge.lookup`. Both sides are env-substituted first, to bring
   * the user's vocabulary into the inlined form the stored facts have
   * (as `cited_fact` does for a spelled-out proposition). `<var>` must
   * be a bare variable.
   *
   * One binding is applied once. The cited fact may license its own
   * rewrite (`contradiction n == 1 with n = 1 end`), but that only lands
   * on a tautology (`1 == 1`), which isn't `false`. */
  let substituted =
    switch (instantiation) {
    | None => Ok((fact, []))
    | Some((var, inst)) =>
      let var = var |> Substitution.in_exp(env);
      let inst = inst |> Substitution.in_exp(env);
      let known = (l, r) =>
        Discharge.lookup(ctx, Exp.temp(BinOp(Poly(Equals), l, r))) != None;
      switch (ProofSyntax.exp_to_name(var)) {
      | Some(x) when known(var, inst) || known(inst, var) =>
        let rewritten =
          fact
          |> Substitution.in_exp(
               Environment.extend(Environment.empty, (x, inst)),
             );
        Ok((rewritten, [(x, inst)]));
      | Some(x) =>
        Error(ProofMark.ContradictionSubstitutionUnverified({var: x}))
      | None =>
        Error(ProofMark.ContradictionSubstitutionUnverified({var: "?"}))
      };
    };
  let fail = (~substitutions=[], mark) => (
    incoming,
    record(
      ~ctx,
      ~substitutions,
      ~marks=[mark],
      id,
      incoming,
      incoming,
      ProofMap.empty,
    ),
  );
  switch (incoming, Discharge.lookup(ctx, fact), substituted) {
  | (None, _, _) => fail(ProofMark.MissingIncoming)
  | (Some(_), None, _) => fail(ProofMark.UnknownFactContradicted)
  | (Some(_), Some(_), Error(mark)) => fail(mark)
  | (Some(_), Some(_), Ok((fact, substitutions))) =>
    let computed = Discharge.evaluate(~step, ~env, fact);
    if (Exp.fast_equal(computed, Exp.temp(Atom(Bool(false))))) {
      let outgoing = Some(Exp.fresh(Atom(Bool(true))));
      (
        outgoing,
        record(~ctx, ~substitutions, id, incoming, outgoing, ProofMap.empty),
      );
    } else {
      fail(~substitutions, ProofMark.ContradictionNotFalse);
    };
  };
}

/* `alias <name> = <fact> => <body>` names a fact after the fact.
 * Auto-installed facts have fixed names and shadow, so a nested split
 * hides the enclosing one's `case_eq`; this resolves `<fact>` as `revert`
 * does and installs the same proposition again under `<name>` for the
 * body's scope.
 *
 * Nothing new is assumed, so there is no obligation, and the outgoing is
 * the body's. An unresolvable fact gets `revert`'s mark,
 * `ProofMark.UnknownFactReverted`, and the body is checked against the unchanged
 * goal. A hole or compound pattern in the name slot names nothing, so
 * the step is then a no-op rather than a broken one. */
and check_alias = (~step, ~info_map, ~ctx, ~id, incoming, x, e, body) => {
  let fact = cited_fact(~ctx, e);
  switch (Discharge.lookup(ctx, fact), Pat.bound_vars(x)) {
  | (None, _) =>
    pass_through(
      ~step,
      ~info_map,
      ~ctx,
      ~id,
      ~marks=[ProofMark.UnknownFactReverted],
      incoming,
      body,
    )
  | (Some(_), names) =>
    let body_ctx =
      switch (names) {
      | [h] => SemanticCtx.add_hypothesis(ctx, h, fact) |> fst
      | _ => ctx
      };
    let (out_body, m) =
      check(~step, ~info_map, ~ctx=body_ctx, incoming, body);
    (out_body, record(~ctx, id, incoming, out_body, m));
  };
}

/* Induction on `scrut` in the incoming goal; on a computed scrutinee,
 * a boolean split.
 *
 * Each case's body is checked against the goal with `scrut` replaced by
 * the case's pattern, in a scope extended with the pattern's bindings,
 * the arm's `where` facts, the case equation `scrut == pat` and
 * inductive hypotheses for the sub-patterns of the scrutinee's type. If
 * the cases are exhaustive and every case proves its goal to `true` with
 * a mark-free subtree (a broken step passes `true` through), the
 * induction's outgoing is `true`. The induction node itself is marked
 * for empty cases, non-exhaustive cases and an untyped scrutinee;
 * per-case failures are marked on the cases.
 *
 * Exhaustiveness here is stricter than the edit-time check in the
 * statics: a column whose type is `Unknown` is refused rather than
 * treated as vacuously covered (`Coverage.check(~strict_unknown)`),
 * because certifying a proof is not the same as marking an error. */
and check_induction =
    (
      ~step,
      ~info_map,
      ~ctx,
      ~id,
      incoming,
      scrut: Exp.t,
      /* Names the case equation in every case. */
      as_name: option(Pat.t),
      cases: list((Pat.t, Proof.t)),
    )
    : (option(Exp.t), ProofMap.t) => {
  let tyctx = SemanticCtx.get_ctx(ctx);
  let (gate_marks, gate_obligations) =
    Gate.scrutinee(~step, ~ctx, ~origin=id, scrut);
  let scrut_id = Exp.rep_id(scrut);
  /* The statics record the scrutinee's type when elaborating the
   * theorem. But they analyze a `funext` binder at `Unknown`, while the
   * checker binds it at the function equality's domain, so when the
   * statics leave the type unknown it is read off the checker's ctx.
   * Only a variable or an ascription is read, never the cases, so an
   * untyped `forall b -> | true | false` stays refused. Unknown means
   * unknown at the head: `[?]` still gives coverage a list shape. */
  let scrut_ty = {
    let head_unknown = ty =>
      switch (Typ.term_of(Typ.normalize(tyctx, ty))) {
      | Unknown(_) => true
      | _ => false
      };
    let rec ctx_ty = (e: Exp.t): option(Typ.t) =>
      switch (Exp.term_of(e)) {
      | Parens(inner) => ctx_ty(inner)
      | Asc(inner, t) =>
        switch (Typ.term_of(t)) {
        | Unknown(_) => ctx_ty(inner)
        | _ => Some(t)
        }
      | Var(x) =>
        Ctx.lookup_var(tyctx, x)
        |> Option.map((entry: Ctx.var_entry) => entry.typ)
      | _ => None
      };
    let statics_ty =
      Statics.Map.ty_of(scrut_id, info_map)
      |> Option.value(~default=Typ.fresh(Unknown(Internal)));
    switch (ctx_ty(scrut)) {
    | Some(ty) when head_unknown(statics_ty) && !head_unknown(ty) => ty
    | _ => statics_ty
    };
  };
  let scrut_co_ctx =
    switch (Statics.Map.lookup_exp(scrut_id, info_map)) {
    | Some({co_ctx, _}) => co_ctx
    | None => CoCtx.empty
    };
  /* The goal and scope of the case for `pat`, below the `earlier` arms. */
  let case = (~earlier: list(Pat.t), pat: Pat.t) => {
    let added_vars = pat |> Pat.bindings |> Binding.variable_names;
    let replace_scrut = (by: Pat.t, goal: Exp.t) =>
      Induction.replace(
        info_map,
        scrut,
        scrut_co_ctx,
        Exp.of_pat(by),
        by |> Pat.bindings |> CoCtx.of_bindings,
        added_vars,
        goal,
      );
    let case_ctx = SemanticCtx.add_from_pattern(ctx, pat, scrut_ty);
    let case_env = SemanticCtx.get_env(case_ctx);
    let case_ctx =
      List.fold_left(
        Where.add_facts,
        case_ctx,
        Induction.where_facts(~earlier, pat),
      );
    /* The case equation, unless the pattern's variables capture the
     * scrutinee. `induction <e> as <h>` names it `h` in every case;
     * unnamed, it is `case_eq`, which shadows any enclosing one. */
    let case_ctx =
      CoCtx.has_any(scrut_co_ctx, added_vars)
        ? case_ctx
        : SemanticCtx.add_hypothesis(
            case_ctx,
            as_name_or(~default="case_eq", as_name),
            Exp.fresh(BinOp(Poly(Equals), scrut, Exp.of_pat(pat)))
            |> Substitution.in_exp(case_env),
          )
          |> fst;
    /* IHs are stored env-substituted, like the case equation: rules are
     * matched against env-substituted targets (MatchExp.match_exp
     * substitutes the exp side), so a fact citing a definition by bare
     * name (`Var ra`) would never match its inlined value. */
    let ihs =
      Induction.hypotheses(~tyctx, info_map, scrut_ty, pat)
      |> List.filter_map(h =>
           replace_scrut(
             h,
             incoming |> Option.value(~default=Exp.fresh(EmptyHole)),
           )
         )
      |> List.map(Substitution.in_exp(case_env));
    let case_ctx =
      List.fold_left(
        (ctx, ih) => SemanticCtx.add_hypothesis(ctx, "ih", ih) |> fst,
        case_ctx,
        ihs,
      );
    (Option.bind(incoming, replace_scrut(pat)), case_ctx);
  };
  /* `Exp.of_pat` reads a wildcard as `true`, so a wildcard case's goal
   * would substitute `true` for the scrutinee. The whole split is refused
   * before any case is checked. */
  let rec has_wild = (p: Pat.t): bool =>
    switch (Pat.term_of(p)) {
    | Wild => true
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | ExplicitNonlabel
    | Atom(_)
    | Constructor(_)
    | Var(_)
    | Label(_) => false
    | ListLit(ps)
    | Tuple(ps) => List.exists(has_wild, ps)
    | Cons(a, b)
    | TupLabel(a, b)
    | Ap(a, b) => has_wild(a) || has_wild(b)
    | Parens(q)
    | Projector(_, q)
    | Asc(q, _)
    | Where(q, _) => has_wild(q)
    };
  let (outgoing, marks, m) =
    if (List.exists(((p, _)) => has_wild(p), cases)) {
      (incoming, [ProofMark.InductionUnsupportedWildcard], ProofMap.empty);
    } else {
      let pats = List.map(fst, cases);
      let checked =
        List.mapi(
          (i, (pat, body)) => {
            let earlier = List.filteri((j, _) => j < i, pats);
            let (case_goal, case_ctx) = case(~earlier, pat);
            check(~step, ~info_map, ~ctx=case_ctx, case_goal, body);
          },
          cases,
        );
      /* A guarded arm contributes no coverage constraint, as in the
       * statics, so `| S(m) where P => .. | Z => .. end` is reported not
       * exhaustive. Strict mode also catches nested `Unknown`s, e.g. an
       * ADT whose payload type is a hole. */
      let constraints =
        List.filter_map(
          pat =>
            Pat.is_guarded(pat)
              ? None
              : Statics.Map.lookup_pat(Pat.rep_id(pat), info_map)
                |> Option.map(Info.pat_constraint),
          pats,
        );
      let normalized_ty = Typ.normalize(tyctx, scrut_ty);
      let is_exhaustive =
        Coverage.check(~strict_unknown=true, constraints, normalized_ty).
          exhaustiveness
        == Exhaustive;
      let marks =
        switch (cases, is_exhaustive, Typ.term_of(normalized_ty)) {
        | ([], _, _) => [ProofMark.InductionEmptyCases]
        | (_, true, _) => []
        /* Covers a missing type too, which falls back to
         * `Unknown(Internal)` above. */
        | (_, false, Unknown(_)) => [ProofMark.InductionScrutineeUntyped]
        | (_, false, _) => [ProofMark.InductionNotExhaustive]
        };
      /* The step's conclusion is the goal itself, so it steps to `true`
       * whether or not its cases are finished; an open case keeps the
       * proof incomplete (`ProofMap.status_of_proof`). */
      let outgoing =
        cases != [] && is_exhaustive
          ? Some(Exp.temp(Atom(Bool(true)))) : incoming;
      let m =
        List.fold_left(
          (acc, (_, m)) => ProofMap.union(acc, m),
          ProofMap.empty,
          checked,
        );
      (outgoing, marks, m);
    };
  (
    outgoing,
    record(
      ~ctx,
      ~marks=gate_marks @ marks,
      ~obligations=gate_obligations,
      id,
      incoming,
      outgoing,
      m,
    ),
  );
};
