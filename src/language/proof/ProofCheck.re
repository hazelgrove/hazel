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
 * The rewrite steps, binder peeling and funext goals are
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

/* The name a naming form (`assume h = P`) asks for, or the form's fixed
 * base name when it names nothing. A hole or a compound pattern names nothing,
 * so it also gets the base name rather than dropping the hypothesis.
 * Base names shadow; see `SemanticCtx.add_hypothesis`.
 *
 * The checker runs on the elaborated proof, where a bare identifier
 * pattern can arrive wrapped (`Parens`, `Asc`), so the name is read
 * through `Pat.bound_vars` rather than by matching `Var`. */
let name_or = (~default: Var.t, name: option(Pat.t)): Var.t =>
  switch (Option.map(~f=Pat.bound_vars, name)) {
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
   * The obligations panel's split exit needs it: `cases P | true =>
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

  /* The id of a fact in scope (`SemanticCtx.facts`) equal to `goal`, up
   * to `strip_eq_true`: as written, or else once the definitions they
   * name are unfolded where they differ. With `proven`, a theorem not
   * proven is passed over: it can be reverted, but discharges nothing. */
  let lookup = (~proven=false, ctx: SemanticCtx.t, goal: Exp.t): option(Id.t) => {
    let goal_nf = strip_eq_true(goal);
    let find = same =>
      SemanticCtx.facts(ctx)
      |> List.find_map(~f=((_name, id, fact)) =>
           switch (proven, SemanticCtx.verdict(ctx, id)) {
           | (true, Some(Unfinished | Refuted)) => None
           | _ =>
             same(fact, goal) || same(strip_eq_true(fact), goal_nf)
               ? Some(id) : None
           }
         );
    switch (find(Exp.fast_equal)) {
    | Some(_) as found => found
    | None => find(MatchExp.equal_modulo(~defs=SemanticCtx.get_env(ctx)))
    };
  };

  let has_subterm = (matches: Exp.t => bool, e: Exp.t): bool => {
    let found = ref(false);
    let _: Exp.t =
      Exp.map_term(
        ~f_exp=
          (continue, e) =>
            if (matches(e)) {
              found := true;
              e;
            } else {
              continue(e);
            },
        e,
      );
    found^;
  };

  /* Runs `step` to a fixpoint. The fuel bound is generous, since
   * `step_fn` has no termination signal.
   *
   * It stops at the first term holding a dynamic error (`1 / 0`). Each
   * `step` starts afresh, so it would redo the failed operation inside
   * the error hole and wrap a new hole around it: the term grows on every
   * step and never gets anywhere. Stopping only leaves a goal pending,
   * since a term with an error in it isn't `true`. */
  let evaluate = (~step: step_fn, ~env, e: Exp.t): Exp.t => {
    let is_error = (e: Exp.t) =>
      switch (Exp.term_of(e)) {
      | DynamicErrorHole(_) => true
      | _ => false
      };
    let rec go = (fuel, e) =>
      switch (fuel <= 0 ? None : step(~env, ~at=Anywhere, e)) {
      | Some({outgoing, _}) when has_subterm(is_error, outgoing) => outgoing
      | Some({outgoing, _}) => go(fuel - 1, outgoing)
      | None => e
      };
    go(1000, e);
  };

  /* How `goal` is discharged: by lookup, or else by closed evaluation to
   * `true`. Only a goal whose free variables are all definitions
   * (`2 != 0`, `f(2) > 0`) is evaluated, and not one that calls a
   * recursive function: only explicit steps (`step`) run those. Both are
   * judged on a copy with the definitions substituted. */
  let find = (~step: step_fn, ~ctx: SemanticCtx.t, goal: Exp.t) => {
    let substituted =
      lazy(Substitution.in_exp(SemanticCtx.get_env(ctx), goal));
    /* Statics against an empty ctx leave an empty co-context exactly
     * when there are no free variables. */
    let is_closed = () => {
      let goal = Lazy.force(substituted);
      let (statics, _) =
        Statics.mk(
          ~ana=Typ.temp(Atom(Bool)),
          CoreSettings.on,
          Ctx.empty,
          goal,
        );
      switch (Statics.Map.lookup_exp(Exp.rep_id(goal), statics)) {
      | Some(info) => List.is_empty(Info.exp_co_ctx(info))
      | None => true
      };
    };
    /* With its definitions substituted, every recursive function a goal
     * can call (written in the goal, in a user's closure or in a builtin)
     * is a `FixF` node in it. */
    let has_fixpoint = () =>
      has_subterm(
        e =>
          switch (Exp.term_of(e)) {
          | FixF(_) => true
          | _ => false
          },
        Lazy.force(substituted),
      );
    switch (lookup(~proven=true, ctx, goal)) {
    | Some(fact_id) => Obligation.Remote(fact_id)
    | None when is_closed() && !has_fixpoint() =>
      is_true(Some(evaluate(~step, ~env=SemanticCtx.get_env(ctx), goal)))
        ? Obligation.Evaluated : Obligation.Pending
    | None => Obligation.Pending
    };
  };

  /* The obligation to prove `goal`, incurred at step `origin` and
   * discharged if it can be. */
  let obligation =
      (
        ~step: step_fn,
        ~ctx: SemanticCtx.t,
        ~origin: Id.t,
        ~kind: Obligation.kind,
        goal: Exp.t,
      )
      : Obligation.t =>
    Obligation.{
      origin,
      kind,
      bindings: SemanticCtx.get_ctx(ctx).entries,
      goal,
      discharge: find(~step, ~ctx, goal),
    };
};

/* The proposition a `revert` or `alias` step cites: a
 * hypothesis or theorem by name, looked up in the theorem namespace so
 * that `revert h` names the same thing `rewrite h` does, or else the
 * proposition as spelled out. */
let cited_fact = (~ctx: SemanticCtx.t, e: Exp.t): Exp.t => {
  let named =
    Option.bind(ProofSyntax.exp_to_name(e), ~f=name =>
      Ctx.lookup_theorem(SemanticCtx.get_ctx(ctx), name)
    );
  switch (named) {
  | Some({prop: Some(prop), _}) => prop
  | Some({prop: None, _})
  | None => e
  };
};

/* A form whose argument is still a hole, as the stepper writes
 * `revert ? =>`, is unfinished rather than wrong: like a hole step, it
 * passes the goal through unmarked. */
let is_hole = (e: Exp.t): bool =>
  switch (Exp.term_of(e)) {
  | EmptyHole => true
  | _ => false
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
      (
        ~statics: ProofStatics.t,
        ~ctx: SemanticCtx.t,
        instantiations: list((Var.t, Exp.t)),
      )
      : result(list(Exp.t), Var.t) => {
    let env = SemanticCtx.get_env(ctx);
    /* Totality reads the bodies of the definitions an instantiation
     * calls, so it checks a copy with them substituted: recursive ones
     * surface their FixF spine, and quantified binders stay bare Vars. */
    let divergent =
      List.find_map(
        ~f=
          ((n, e)) =>
            switch (
              Totality.check(
                ~statics,
                ~ctx=SemanticCtx.get_ctx(ctx),
                Substitution.in_exp(env, e),
              )
            ) {
            | Ok () => None
            | Error(_) => Some(n)
            },
        instantiations,
      );
    switch (divergent) {
    | Some(n) => Error(n)
    | None =>
      Ok(
        instantiations
        |> List.concat_map(~f=((_, e)) => DomainConditions.scan(e))
        |> DomainConditions.dedup,
      )
    };
  };

  /* For a split's scrutinee. A bare quantified variable needs nothing:
   * quantifiers range over total values. A computed scrutinee has to
   * denote a value: for `cases`, so its case equation is a symmetric
   * equation, and for `induction`, so the goal is an instance of the
   * generalized one. So a possibly-divergent scrutinee is marked and its
   * domain conditions become obligations. */
  let scrutinee =
      (
        ~step: step_fn,
        ~statics: ProofStatics.t,
        ~ctx: SemanticCtx.t,
        ~origin: Id.t,
        scrut: Exp.t,
      )
      : (list(ProofMark.t), list(Obligation.t)) => {
    let env = SemanticCtx.get_env(ctx);
    switch (ProofSyntax.exp_to_name(scrut)) {
    /* A scrutinee still to fill in is unfinished, not divergent. */
    | None when is_hole(scrut) => ([], [])
    | Some(x) when !Definitions.is_definition(env, x) => ([], [])
    | Some(_)
    | None =>
      let marks =
        switch (
          Totality.check(
            ~statics,
            ~ctx=SemanticCtx.get_ctx(ctx),
            Substitution.in_exp(env, scrut),
          )
        ) {
        | Ok () => []
        | Error(_) => [ProofMark.PossiblyDivergentScrutinee]
        };
      let obligations =
        DomainConditions.scan(scrut)
        |> List.map(
             ~f=Discharge.obligation(~step, ~ctx, ~origin, ~kind=Definedness),
           );
      (marks, obligations);
    };
  };
};

// Binders

/* `ctx` without the facts that mention any of `names`, for a scope in
 * which those names are rebound: the facts are about the old binding.
 * Unlike `ProofCtx`'s capture check, this also hides them from
 * `Discharge.lookup`. */
let forget_facts_mentioning =
    (names: list(Var.t), ctx: SemanticCtx.t): SemanticCtx.t =>
  SemanticCtx.map_ctx(
    Ctx.forget_facts(fact =>
      ProofRule.mentions_any(ProofRule.exp_to_rule(fact), names)
    ),
    ctx,
  );

/* `ctx` extended with the variables `p` binds, at type `ty`. Every proof
 * binder goes through here. Names in goals and facts are resolved by
 * scope, so a fact that mentions a rebound name is about what the name
 * meant outside, and is dropped. That covers a binder that shadows a
 * definition (`forall len ->`) as well as one that shadows an earlier
 * binder. */
let bind = (ctx: SemanticCtx.t, p: Pat.t, ty: Typ.t): SemanticCtx.t =>
  SemanticCtx.add_from_pattern(
    forget_facts_mentioning(p |> Pat.bindings |> Binding.variable_names, ctx),
    p,
    ty,
  );

/* Peel the outermost binder from a "for all pat, P" goal, for the
 * `Forall` proof form: the binder, its type and the body. */
let rec peel_binder = (goal: Exp.t): option((Pat.t, Typ.t, Exp.t)) =>
  switch (goal |> Exp.term_of) {
  /* `EditorTransform` wraps a spliced sub-term in `Parens` whenever it
   * shares a segment level with sibling pieces. */
  | Parens(inner) => peel_binder(inner)
  | Fun(p, body, t, _) =>
    let t = OptUtil.get(() => Typ.fresh(Unknown(Internal)), t);
    Some((p, t, body));
  | Forall(p, body) => Some((p, Typ.fresh(Unknown(Internal)), body))
  | _ => None
  };

/* Introduce a theorem statement's outer binders: extend the semantic ctx
 * with each binder's variables, and return the remaining core goal,
 * which keeps any `==>` antecedents (those are introduced with
 * `assume`). Used by the
 * evaluator and the UI stepper to seed the proof goal. */
let rec peel_stmt_binders =
        (ctx: SemanticCtx.t, goal: Exp.t): (SemanticCtx.t, Exp.t) =>
  switch (goal |> Exp.term_of) {
  /* Transparent, for the reason spelled out on `peel_binder`. */
  | Parens(inner) => peel_stmt_binders(ctx, inner)
  | Forall(p, body) =>
    let ctx = bind(ctx, p, Typ.fresh(Unknown(Internal)));
    peel_stmt_binders(ctx, body);
  | _ => (ctx, goal)
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
        ~statics: ProofStatics.t,
        ~ctx: SemanticCtx.t,
        ~direction: Direction.t,
        ~equality: string,
        ~instantiation: option((Var.t, Exp.t))=None,
        goal: Exp.t,
      )
      : result((Exp.t, list(Exp.t), list((Var.t, Exp.t))), ProofMark.t) => {
    open Result.Syntax;
    let env = SemanticCtx.get_env(ctx);
    let proof_ctx =
      ProofCtx.of_theorem_ctx(
        ~builtins=Axioms.v,
        ~captured=ctx.captured,
        SemanticCtx.get_ctx(ctx),
      );
    let* rule =
      ProofCtx.lookup_rule(equality, proof_ctx)
      |> Result.of_option(~error=ProofMark.UnknownEquality(equality));
    /* A cited rule with a boolean conclusion `P` reads as `P == true`.
     * `~statics` settles conclusions whose shape doesn't (a plain
     * variable, a Bool-typed application). */
    let rule = ProofRule.BoolReading.apply(~statics=Some(statics), rule);
    let* bindings =
      switch (instantiation) {
      | None => Ok(None)
      | Some((var, inst)) =>
        ProofRule.Rewrite.seed(
          var,
          inst,
          ProofRule.Rewrite.empty_bindings(rule.bindings),
        )
        |> Option.map(~f=Option.some)
        |> Result.of_option(
             ~error=
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
        ~statics,
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
        List.map(~f=MatchExp.substitute_exp(mctx), rule.assumptions);
      let instantiations =
        List.filter_map(
          ~f=((n, (_, assigned))) => Option.map(~f=e => (n, e), assigned),
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
        ~statics: ProofStatics.t,
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
      |> Result.of_option(~error=ProofMark.MalformedEqualityName);
    let* () =
      switch (SemanticCtx.cited_verdict(ctx, equality)) {
      | Some(Refuted) => Error(ProofMark.CitesRefuted(equality))
      | Some(Proven | Unfinished)
      | None => Ok()
      };
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
      apply(~statics, ~ctx, ~direction, ~equality, ~instantiation, goal);
    switch (Gate.instantiations(~statics, ~ctx, instantiations)) {
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
        |> List.filter(~f=c => !List.exists(~f=Exp.fast_equal(c), premises));
      let obligations =
        List.map(
          ~f=Discharge.obligation(~step, ~ctx, ~origin, ~kind=Premise),
          premises,
        )
        @ List.map(
            ~f=Discharge.obligation(~step, ~ctx, ~origin, ~kind=Definedness),
            domain_conditions,
          );
      let citation =
        switch (Ctx.lookup_theorem(SemanticCtx.get_ctx(ctx), equality)) {
        | Some({id, prop: Some(statement), _})
            when Poly.equal(SemanticCtx.verdict(ctx, id), Some(Unfinished)) => [
            Discharge.obligation(
              ~step,
              ~ctx,
              ~origin,
              ~kind=Citation,
              statement,
            ),
          ]
        | Some(_)
        | None => []
        };
      Ok((rewritten(out), obligations @ citation));
    };
  };
};

/* `algebra <replacement> end`: replace the goal with an expression the
 * CAS finds equal to it. The CAS's field laws are false for IEEE floats,
 * so a Float-typed rewrite is refused. It reasons in a field, so both
 * sides' domain conditions are its obligations.
 *
 * The CAS verifies the equation only for Int arithmetic it is right
 * about: Int literals, +, −, ×, unary −, and ** with a literal exponent
 * ≥ 0, over variables known to be Int. It verifies Nat literals, +, × and
 * ** over variables known to be Nat the same way: such an expression has
 * the value of the same polynomial over Int, and polynomials equal on
 * every Nat are equal. Nat subtraction truncates, so it stays out.
 * Definitions are substituted first,
 * so one whose value is a literal counts as that literal, and one whose
 * value is an error or a function leaves the fragment. A step the CAS
 * finds wrong is refused. Any other step incurs its equation as an
 * obligation. */
module Algebrite = {
  type verdict =
    | Equal
    | Differ
    | Unverified;

  let verify =
      (~statics: ProofStatics.t, ~ctx: SemanticCtx.t, goal, replacement)
      : verdict => {
    /* By the statics at this occurrence, or else by the checker's binder
     * for it: the statics type a proof's `forall x =>` binder `Unknown`,
     * where the checker binds it at the goal's type. */
    let has_cls = (cls: Atom.cls, x: Exp.t, name: Var.t): bool => {
      let binder_typ = () =>
        Ctx.lookup_var(SemanticCtx.get_ctx(ctx), name)
        |> Option.map(~f=(entry: Ctx.var_entry) =>
             Typ.term_of(Typ.normalize(Ctx.empty, entry.typ))
           );
      switch (Option.map(~f=Typ.term_of, statics.typ(Exp.rep_id(x)))) {
      | Some(Unknown(_))
      | None => Poly.equal(binder_typ(), Some(Atom(cls)))
      | Some(ty) => Poly.equal(ty, Atom(cls))
      };
    };
    /* Variables are renamed, since Algebrite reads `i` as the imaginary
     * unit and `e` as Euler's number. */
    let names = ref([]);
    let rename = name =>
      switch (List.Assoc.find(names^, name, ~equal=String.equal)) {
      | Some(renamed) => renamed
      | None =>
        let renamed = "hzv" ++ string_of_int(List.length(names^));
        names := [(name, renamed), ...names^];
        renamed;
      };
    /* `cls` is Int or Nat. Every operator and operand has to be of it: a
     * Nat variable under an Int `+` is ill-typed and doesn't evaluate. */
    let rec print = (cls: Atom.cls, e: Exp.t): option(string) => {
      open OptUtil.Syntax;
      let binary = (a, op, b) => {
        let* a = print(cls, a);
        let+ b = print(cls, b);
        "(" ++ a ++ ")" ++ op ++ "(" ++ b ++ ")";
      };
      switch (cls, Exp.term_of(e)) {
      | (Int, Atom(Int(n)))
      | (Nat, Atom(Nat(n))) => Some("(" ++ Bigint.to_string(n) ++ ")")
      | (_, Var(x)) when has_cls(cls, e, x) => Some(rename(x))
      | (_, Parens(e) | Asc(e, _)) => print(cls, e)
      | (Int, UnOp(Int(Minus), e)) =>
        print(cls, e) |> Option.map(~f=e => "-(" ++ e ++ ")")
      | (Int, BinOp(Int(Plus), a, b))
      | (Nat, BinOp(Nat(Plus), a, b)) => binary(a, "+", b)
      | (Int, BinOp(Int(Minus), a, b)) => binary(a, "-", b)
      | (Int, BinOp(Int(Times), a, b))
      | (Nat, BinOp(Nat(Times), a, b)) => binary(a, "*", b)
      | (Int, BinOp(Int(Power), a, {term: Atom(Int(n)), _} as b))
          when Bigint.(>=)(n, Bigint.zero) =>
        binary(a, "^", b)
      | (Nat, BinOp(Nat(Power), a, {term: Atom(Nat(_)), _} as b)) =>
        binary(a, "^", b)
      | _ => None
      };
    };
    let substituted = Substitution.in_exp(SemanticCtx.get_env(ctx));
    let printed = (cls: Atom.cls) =>
      OptUtil.zip(
        print(cls, substituted(goal)),
        print(cls, substituted(replacement)),
      );
    let sides =
      switch (printed(Int)) {
      | Some(_) as sides => sides
      | None => printed(Nat)
      };
    switch (sides) {
    | Some((l, r)) =>
      switch (Cas.run^("simplify((" ++ l ++ ")-(" ++ r ++ "))")) {
      | Some("0") => Equal
      | Some(_) => Differ
      | None => Unverified
      }
    | _ => Unverified
    };
  };

  let check =
      (
        ~step: step_fn,
        ~statics: ProofStatics.t,
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
      switch (statics.typ(Exp.rep_id(e))) {
      | Some(ty) =>
        switch (Typ.term_of(ty)) {
        | Atom(Float) => true
        | Unknown(_) => float_head(e)
        | _ => false
        }
      | None => float_head(e)
      };
    };
    let obligation = Discharge.obligation(~step, ~ctx, ~origin);
    let domain_conditions = () =>
      DomainConditions.dedup(
        DomainConditions.scan(goal) @ DomainConditions.scan(replacement),
      )
      |> List.map(~f=obligation(~kind=Definedness));
    let rewritten = rewritten(Exp.replace_all_ids(replacement));
    if (is_float(goal) || is_float(replacement)) {
      Error(ProofMark.FloatAlgebrite);
    } else {
      switch (verify(~statics, ~ctx, goal, replacement)) {
      | Equal => Ok((rewritten, domain_conditions()))
      | Differ => Error(ProofMark.AlgebraDiffers)
      | Unverified =>
        let equation =
          Exp.fresh(BinOp(Poly(Equals), goal, replacement))
          |> obligation(~kind=Algebra);
        Ok((rewritten, domain_conditions() @ [equation]));
      };
    };
  };
};

/* A redex contracts whatever its argument is: a lambda with a
 * one-variable parameter, or a named function that isn't recursive,
 * applied to anything, or a `let` binding one variable. A destructuring
 * parameter needs its argument's shape, and a recursive function unfolds
 * by evaluation. */
module Beta = {
  /* The redex at the root of `e`: its variable, body and argument. */
  let redex =
      (~env: Environment.t(Exp.t), e: Exp.t): option((Var.t, Exp.t, Exp.t)) => {
    open OptUtil.Syntax;
    /* The statics name a recursive function `f+` (`Definitions.fold`). */
    let is_recursive = (name: option(Var.t)) =>
      switch (name) {
      | Some(f) =>
        String.length(f) > 0 && Char.equal(f.[String.length(f) - 1], '+')
      | None => false
      };
    /* Not through a closure: its body's names mean what its environment
     * says, not what the goal's scope does. */
    let rec lambda = (f: Exp.t) =>
      switch (Exp.term_of(f)) {
      | Parens(f)
      | Asc(f, _) => lambda(f)
      | Fun(p, body, _, name) when !is_recursive(name) =>
        let+ x = ProofSyntax.pat_to_name(p);
        (x, body);
      | _ => None
      };
    switch (Exp.term_of(ProofSyntax.unwrap(e))) {
    | Ap(Forward, f, arg) =>
      /* A definition is unfolded, closed, only once its value shows it is
       * such a lambda. */
      let* head =
        switch (ProofSyntax.exp_to_name(f)) {
        | Some(name) =>
          let* value = Environment.lookup(env, name);
          let* _ = lambda(ProofSyntax.unwrap(value));
          Definitions.unfold(env, name);
        | None => Some(f)
        };
      let+ (x, body) = lambda(head);
      (x, body, arg);
    | Let(p, def, body) =>
      switch (Exp.term_of(ProofSyntax.unwrap(def))) {
      | FixF(_) => None
      | _ =>
        let+ x = ProofSyntax.pat_to_name(p);
        (x, body, def);
      }
    | _ => None
    };
  };

  /* `body` with `arg` for `x`, without capturing a variable of `arg`. */
  let contract = ((x, body, arg): (Var.t, Exp.t, Exp.t)): Exp.t =>
    Substitution.in_exp(Substitution.env_for([(x, arg)], ~into=body), body)
    |> Exp.replace_all_ids;
};

/* `step`: one evaluation step at the root of the goal, or, where
 * evaluation takes none, the redex there contracted (`Beta`). Evaluation
 * contracts a lambda or `let` itself whatever the argument, as a variable
 * pattern matches anything, but not a named function whose argument isn't
 * a value yet. The evaluator inlines the definitions it looks up, and the
 * goals it leaves name them again (`Definitions.fold`). */
module Eval = {
  let check =
      (~step: step_fn, ~env: Environment.t(Exp.t), goal: Exp.t)
      : result((step_result, list(Obligation.t)), ProofMark.t) => {
    let fold = Definitions.fold(env);
    switch (step(~env, ~at=Root, goal)) {
    | Some({auto_incoming, auto_outgoing, outgoing}) =>
      Ok((
        {
          auto_incoming:
            List.map(~f=((j, e)) => (j, fold(e)), auto_incoming),
          auto_outgoing:
            List.map(~f=((e, j)) => (fold(e), j), auto_outgoing),
          outgoing: fold(outgoing),
        },
        [],
      ))
    | None =>
      switch (Beta.redex(~env, goal)) {
      | Some(redex) => Ok((rewritten(fold(Beta.contract(redex))), []))
      | None => Error(ProofMark.NothingToStep)
      }
    };
  };
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
 *   2. Syntax: either side is literally a lambda (`fun` / a `Parens`
 *      around one). Covers goals statics left `Unknown`,
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
    | Fun(p, _, t, _) =>
      let rec ascribed = (p: Pat.t) =>
        switch (Pat.term_of(p)) {
        | Parens(p) => ascribed(p)
        | Asc(_, t) => Some(t)
        | _ => None
        };
      switch (ascribed(p), Option.map(~f=Typ.term_of, t)) {
      | (Some(d), _)
      | (None, Some(Arrow(d, _))) => Some(d)
      | (None, _) => Some(Typ.fresh(Unknown(Internal)))
      };
    /* A closure/fix wrapper around a lambda still denotes a function. */
    | Closure(_, inner)
    | FixF(_, inner, _) => syntactic_domain(~env, inner)
    | Var(x) =>
      switch (Environment.lookup(env, x)) {
      /* Guard against a self-referential binding looping the lookup. */
      | Some({term: Var(y), _}) when String.equal(y, x) => None
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
          (~statics: ProofStatics.t, ~env: Environment.t(Exp.t), g: Exp.t)
          : option((Exp.t, Exp.t, Typ.t)) =>
    switch (g |> Exp.term_of) {
    /* Parens are transparent here for the same reason they are in
     * `peel_binder`: an editor splice must not hide the equality. */
    | Parens(inner) => goal(~statics, ~env, inner)
    | BinOp(Poly(Equals), l, r) =>
      let statics_domain = (e: Exp.t): option(Typ.t) =>
        switch (Option.map(~f=Typ.term_of, statics.typ(Exp.rep_id(e)))) {
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
      domain |> Option.map(~f=d => (l, r, d));
    | _ => None
    };

  /* The body's goal `l(x) == r(x)`, for the funext binder `binder` read
   * as an expression.
   *
   * When a side is a literal lambda, the application is a beta-redex
   * whose argument is a bound variable (hence a value), so contracting
   * it is definitional: it renames the parameter and discovers nothing.
   * Contracting here lets `(fun x -> x + 1) == (fun x -> 1 + x)` be
   * proved by a pointwise `algebra` without a leading `step` on each
   * side. Not contracted:
   *   - a named function (`inv(x)`, `f(x)`). Unfolding a definition is
   *     `step`'s job, and keeping the `Ap` is why `refl` closes
   *     `inv(x) == inv(x)` without ever applying `inv`.
   *   - a non-variable parameter pattern (a tuple, say): there is no
   *     single name to substitute for. */
  let pointwise = (~binder: Pat.t, l: Exp.t, r: Exp.t): Exp.t => {
    let arg = Exp.of_pat(binder);
    /* The binder's own variables map to themselves, so that substitution
     * renames a lambda's inner binder that would capture them. */
    let binder_vars =
      List.map(~f=x => (x, Exp.fresh(Var(x))), Pat.bound_vars(binder));
    let rec apply = (f: Exp.t): Exp.t =>
      switch (f |> Exp.term_of) {
      | Parens(inner) => apply(inner)
      | Fun(p, body, _, _) =>
        switch (ProofSyntax.pat_to_name(p)) {
        | Some(x) =>
          body
          |> Substitution.in_exp(
               Environment.of_bindings(binder_vars @ [(x, arg)]),
             )
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

/* What each case of a split gets besides its pattern's bindings: for
 * `cases`, the case equation `scrut == pat` as `case_eq`; for
 * `induction`, the inductive hypotheses, with the facts that mention the
 * scrutinee out of scope. */
type case_facts =
  | CaseEquation
  | InductiveHypotheses;

/* Threads `incoming` through the proof tree, returning the outgoing goal
 * (if the proof propagates) and a proof map with an entry at every proof
 * sub-term id. */
let rec check =
        (
          ~step: step_fn,
          ~statics: ProofStatics.t,
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
    let (out1, m1) = check(~step, ~statics, ~ctx, incoming, p1);
    let (out2, m2) = check(~step, ~statics, ~ctx, out1, p2);
    (out2, record(~ctx, id, incoming, out2, ProofMap.union(m1, m2)));
  /* The parens node records the inner step's outgoing too, so the UI can
   * key off either id. */
  | Parens(p) =>
    let (out, m) = check(~step, ~statics, ~ctx, incoming, p);
    (out, record(~ctx, id, incoming, out, m));
  | AxiomStep({direction, equality, instantiation}) =>
    check_rewrite(
      ~ctx,
      ~id,
      incoming,
      Axiom.check(
        ~step,
        ~statics,
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
      Algebrite.check(~step, ~statics, ~ctx, ~origin=id, ~replacement),
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
      ~statics,
      ~ctx,
      ~id,
      incoming,
      ordinal |> Option.map(~f=Annotated.term_of) |> Option.value(~default=1),
      selector,
      body,
    )
  | Forall(pat, body) =>
    check_forall(~step, ~statics, ~ctx, ~id, incoming, pat, body)
  | Funext(pat, body) =>
    check_funext(~step, ~statics, ~ctx, ~id, incoming, pat, body)
  | Assume(e, name, body) =>
    check_assume(~step, ~statics, ~ctx, ~id, incoming, e, name, body)
  /* Nothing is assumed, and the goal passes through. */
  | Have(e, _, sub, body) when is_hole(e) =>
    let (_, m_sub) = check(~step, ~statics, ~ctx, Some(e), sub);
    let (_, m_body) = check(~step, ~statics, ~ctx, incoming, body);
    (
      incoming,
      record(~ctx, id, incoming, incoming, ProofMap.union(m_sub, m_body)),
    );
  | Have(e, name, sub, body) =>
    check_have(~step, ~statics, ~ctx, ~id, incoming, e, name, sub, body)
  | Generalize(e, body) =>
    check_generalize(~step, ~statics, ~ctx, ~id, incoming, e, body)
  | Revert(e, instantiation, body) =>
    check_revert(~step, ~statics, ~ctx, ~id, incoming, e, instantiation, body)
  | Alias(x, e, body) =>
    check_alias(~step, ~statics, ~ctx, ~id, incoming, x, e, body)
  | Cases(scrut, cases) =>
    check_split(
      ~step,
      ~statics,
      ~ctx,
      ~id,
      incoming,
      scrut,
      CaseEquation,
      cases,
    )
  | Induction(scrut, cases) =>
    check_split(
      ~step,
      ~statics,
      ~ctx,
      ~id,
      incoming,
      scrut,
      InductiveHypotheses,
      cases,
    )
  };
}

/* A form that can't act on the goal: mark it, pass the goal through, and
 * check the body against the unchanged goal so later steps stay
 * meaningful. */
and pass_through =
    (~step, ~statics, ~ctx, ~id, ~marks, incoming, body)
    : (option(Exp.t), ProofMap.t) => {
  let (_, m) = check(~step, ~statics, ~ctx, incoming, body);
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
      ~statics,
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
  let (out_body, m) = check(~step, ~statics, ~ctx=body_ctx, subgoal, body);
  let outgoing = is_true(out_body) ? out_body : incoming;
  (outgoing, record(~ctx, ~marks, ~obligations, id, incoming, outgoing, m));
}

/* `from <selector> at <n> do <body> end`: the body is a chain of its
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
    (~step, ~statics, ~ctx, ~id, incoming, ordinal, selector, body)
    : (option(Exp.t), ProofMap.t) => {
  let env = SemanticCtx.get_env(ctx);
  let target =
    Option.bind(incoming, ~f=goal =>
      Occurrence.nth_env_target(~statics, ~env, selector, ordinal - 1, goal)
      |> Option.map(~f=target => (goal, target))
    );
  switch (incoming, target) {
  | (None, _) =>
    check_subgoal(
      ~step,
      ~statics,
      ~ctx,
      ~id,
      ~marks=[ProofMark.MissingIncoming],
      None,
      None,
      body,
    )
  | (Some(_), None) =>
    let (_, m) = check(~step, ~statics, ~ctx, None, body);
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
        ~f=(ctx, p) => bind(ctx, p, Typ.fresh(Unknown(Internal))),
        ~init=ctx,
        binders,
      );
    let (out_body, m) =
      check(~step, ~statics, ~ctx=body_ctx, Some(target), body);
    let outgoing =
      switch (out_body) {
      | Some(out) =>
        Some(Occurrence.replace_by_id(Exp.rep_id(target), goal, out))
      | None => incoming
      };
    (outgoing, record(~ctx, id, incoming, outgoing, m));
  };
}

/* The binder comes from the incoming goal. A different name written in
 * the proof renames it, as `funext` names its argument, unless the goal
 * already uses that name. */
and check_forall = (~step, ~statics, ~ctx, ~id, incoming, pat, body) =>
  switch (Option.map(~f=peel_binder, incoming)) {
  | None =>
    check_subgoal(
      ~step,
      ~statics,
      ~ctx,
      ~id,
      ~marks=[ProofMark.MissingIncoming],
      None,
      None,
      body,
    )
  | Some(Some((p, t, inner))) =>
    let open_body = (p, inner) =>
      check_subgoal(
        ~step,
        ~statics,
        ~ctx,
        ~body_ctx=bind(ctx, p, t),
        ~id,
        incoming,
        Some(inner),
        body,
      );
    switch (ProofSyntax.pat_to_name(p), ProofSyntax.pat_to_name(pat)) {
    | (Some(from), Some(to_)) when !String.equal(from, to_) =>
      if (Exp.occurs_free_any([to_], inner)) {
        check_subgoal(
          ~step,
          ~statics,
          ~ctx,
          ~id,
          ~marks=[ProofMark.BinderCaptures({var: to_})],
          incoming,
          incoming,
          body,
        );
      } else {
        let renamed =
          Pat.map_term(
            ~f_pat=
              (continue, q: Pat.t) =>
                switch (Pat.term_of(q)) {
                | Var(x) when String.equal(x, from) => {
                    ...q,
                    term: Var(to_),
                  }
                | _ => continue(q)
                },
            p,
          );
        /* `to_` maps to itself so that substitution renames an inner
         * binder that would capture it. */
        let inner =
          Substitution.in_exp(
            Environment.of_bindings([
              (to_, Exp.fresh(Var(to_))),
              (from, Exp.fresh(Var(to_))),
            ]),
            inner,
          );
        open_body(renamed, inner);
      }
    | _ => open_body(p, inner)
    };
  /* No binder to peel: mark it, but let the body keep working on the goal
   * as is. */
  | Some(None) =>
    check_subgoal(
      ~step,
      ~statics,
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
 * an arrow type, and a second `funext y =>` peels again. A binder free
 * in `f` or `g` would capture that use, and is marked. On failure, as
 * for `forall`: mark, and let the body keep working on the goal as
 * written. */
and check_funext = (~step, ~statics, ~ctx, ~id, incoming, pat, body) => {
  let fail = mark =>
    check_subgoal(
      ~step,
      ~statics,
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
    switch (Funext.goal(~statics, ~env=SemanticCtx.get_env(ctx), goal)) {
    | None => fail(ProofMark.ExpectedFunEqGoal)
    /* The binder must be something we can actually bind, else the
     * pointwise goal would mention a name nothing introduces. */
    | Some((_, _, domain))
        when Option.is_none(SemanticCtx.bind_pattern(pat, domain, Ctx.empty)) =>
      fail(ProofMark.MalformedFunext)
    | Some((l, r, domain)) =>
      let captured =
        List.find(
          ~f=
            x =>
              Exp.occurs_free_any([x], l) || Exp.occurs_free_any([x], r),
          Pat.bound_vars(pat),
        );
      switch (captured) {
      | Some(var) => fail(ProofMark.BinderCaptures({var: var}))
      | None =>
        check_subgoal(
          ~step,
          ~statics,
          ~ctx,
          ~body_ctx=bind(ctx, pat, domain),
          ~id,
          incoming,
          Some(Funext.pointwise(~binder=pat, l, r)),
          body,
        )
      };
    }
  };
}

/* Implication intro: with goal `A ==> B` where `e` is A (modulo the
 * definitions in scope), hypothesize `e` and prove B. On any other goal
 * the step is refused and the body keeps the goal; `have` introduces a
 * fact on any goal. A hole for `e` assumes nothing yet, so the goal
 * passes through unmarked, as for `have`. */
and check_assume = (~step, ~statics, ~ctx, ~id, incoming, e, name, body) => {
  let consequent =
    switch (Option.bind(incoming, ~f=ProofRule.implication)) {
    | Some((a, b))
        when
          Exp.fast_equal(a, e)
          || MatchExp.equal_modulo(~defs=SemanticCtx.get_env(ctx), a, e) =>
      Some(b)
    | Some(_)
    | None => None
    };
  switch (consequent) {
  | Some(b) =>
    /* Unnamed, the hypothesis is `assume`, which shadows any enclosing
     * one; naming a nested assumption keeps the outer one citable. */
    let (body_ctx, _) =
      SemanticCtx.add_hypothesis(ctx, name_or(~default="assume", name), e);
    let (_, m) = check(~step, ~statics, ~ctx=body_ctx, Some(b), body);
    /* Assuming the goal's antecedent proves the implication once the
     * body proves its consequent, so the block steps to `true` as a split
     * does, and an unfinished body is caught as an open case is
     * (`ProofMap.cases_closed`). Its body's result would not equal the
     * implication: in a focus it would replace it. */
    let outgoing = Some(Exp.temp(Atom(Bool(true))));
    (outgoing, record(~ctx, id, incoming, outgoing, m));
  | None =>
    let marks =
      is_hole(e)
        ? [] : [ProofMark.ExpectedImplicationGoal({assumption: e})];
    check_subgoal(
      ~step,
      ~statics,
      ~ctx,
      ~id,
      ~marks,
      incoming,
      incoming,
      body,
    );
  };
}

/* `have <exp> proof <subproof> => <body>`, the cut rule. With incoming
 * goal `G` it incurs two obligations, both recorded on the have's own
 * node, as a two-condition axiom would:
 *
 *   1. prove `<exp>` in the enclosing scope (if the subproof could cite
 *      the have's own hypothesis, `have P proof <cite P> => ...` would
 *      prove anything);
 *   2. prove `G` in the scope extended with `<exp>`, named `have` unless
 *      the have names it.
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
and check_have = (~step, ~statics, ~ctx, ~id, incoming, e, name, sub, body) => {
  let proves = (out, m, p) =>
    is_true(out)
    && ProofMap.proof_is_clean(m, p)
    && ProofMap.cases_closed(m, p);
  let (out_sub, m_sub) = check(~step, ~statics, ~ctx, Some(e), sub);
  let sub_obligations =
    proves(out_sub, m_sub, sub)
      ? []
      : [Discharge.obligation(~step, ~ctx, ~origin=id, ~kind=Premise, e)];
  let (body_ctx, _) =
    SemanticCtx.add_hypothesis(ctx, name_or(~default="have", name), e);
  let (out_body, m_body) =
    check(~step, ~statics, ~ctx=body_ctx, incoming, body);
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
 * body's incoming goal is `forall x -> G`. Soundness: `forall x -> G`
 * denoting true entails G at the ambient x.
 *
 * Capture: inside the body, every fact whose statement mentions x
 * (assume-hypotheses, case_eq, IHs) is about the
 * old x, so those entries are removed from the body's scope. One filter
 * over the theorem namespace covers both `Discharge.lookup` and rule
 * lookup (`ProofCtx.of_theorem_ctx`); `is_captured` would cover only rule
 * lookup, and only once the body re-peels the binder. The test is free
 * occurrence (`ProofRule.mentions_any`), so a lemma `forall x -> ...`
 * that binds its own x stays available. */
and check_generalize = (~step, ~statics, ~ctx, ~id, incoming, e, body) => {
  /* Every variable in scope, the proof's binders included, is in the env. */
  let in_scope =
    ProofSyntax.exp_to_name(e)
    |> OptUtil.filter(x =>
         Option.is_some(Environment.lookup(SemanticCtx.get_env(ctx), x))
       );
  switch (incoming, in_scope) {
  | (None, _) =>
    pass_through(
      ~step,
      ~statics,
      ~ctx,
      ~id,
      ~marks=[ProofMark.MissingIncoming],
      None,
      body,
    )
  | (Some(_), None) when is_hole(e) =>
    pass_through(~step, ~statics, ~ctx, ~id, ~marks=[], incoming, body)
  /* The argument isn't a bare in-scope variable. */
  | (Some(_), None) =>
    pass_through(
      ~step,
      ~statics,
      ~ctx,
      ~id,
      ~marks=[ProofMark.MalformedGeneralize],
      incoming,
      body,
    )
  | (Some(goal), Some(x)) =>
    /* The binder keeps x's type, so the body's re-peel binds x as it was
     * bound, and an `algebra` step can still verify arithmetic on it. */
    let binder =
      switch (Ctx.lookup_var(SemanticCtx.get_ctx(ctx), x)) {
      | Some({typ: {term: Unknown(_), _}, _})
      | None => Pat.fresh(Var(x))
      | Some({typ, _}) => Pat.fresh(Asc(Pat.fresh(Var(x)), typ))
      };
    let subgoal = Exp.fresh(Forall(binder, goal));
    /* x's own binder entry stays: it is inert once no fact mentions it
     * (its env binding is the identity `x ↦ Var(x)`), and the body's
     * re-peel of the new binder rebinds it anyway. */
    let body_ctx = forget_facts_mentioning([x], ctx);
    check_subgoal(
      ~step,
      ~statics,
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
    (~step, ~statics, ~ctx, ~id, incoming, e, instantiation, body) => {
  open Result.Syntax;
  let fact = cited_fact(~ctx, e);
  let instantiate = ((var, inst)) => {
    let* x =
      ProofSyntax.exp_to_name(var)
      |> Result.of_option(
           ~error=ProofMark.RevertFactNotQuantified({var: "?"}),
         );
    let* instantiated =
      ProofRule.instantiate_binder(x, inst, fact)
      |> Result.of_option(~error=ProofMark.RevertFactNotQuantified({var: x}));
    switch (Gate.instantiations(~statics, ~ctx, [(x, inst)])) {
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
          ~f=Discharge.obligation(~step, ~ctx, ~origin=id, ~kind=Definedness),
          conditions,
        ),
      ))
    };
  };
  let fail = mark =>
    pass_through(~step, ~statics, ~ctx, ~id, ~marks=[mark], incoming, body);
  switch (incoming, Discharge.lookup(ctx, fact)) {
  | (None, _) => fail(ProofMark.MissingIncoming)
  | (Some(_), None) when is_hole(e) =>
    pass_through(~step, ~statics, ~ctx, ~id, ~marks=[], incoming, body)
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
        ~statics,
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
and check_alias = (~step, ~statics, ~ctx, ~id, incoming, x, e, body) => {
  let fact = cited_fact(~ctx, e);
  switch (Discharge.lookup(ctx, fact), Pat.bound_vars(x)) {
  | (None, _) =>
    pass_through(
      ~step,
      ~statics,
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
      check(~step, ~statics, ~ctx=body_ctx, incoming, body);
    (out_body, record(~ctx, id, incoming, out_body, m));
  };
}

/* A split on `scrut` in the incoming goal: `cases` or `induction`.
 *
 * Each case's body is checked against the goal with `scrut` replaced by
 * the case's pattern, in a scope extended with the pattern's bindings
 * and the split's `case_facts`. If the cases are exhaustive, the split's
 * outgoing is `true`; an unfinished case keeps the proof incomplete. The
 * split node itself is marked for empty cases, non-exhaustive cases and
 * an untyped scrutinee; per-case failures are marked on the cases.
 *
 * `induction` on a computed `scrut` is induction on a fresh variable
 * standing for it: replacing `scrut` throughout the goal is the same
 * substitution, and with no case equation and no fact about `scrut` in
 * scope, nothing ties a case's pattern back to `scrut`'s actual value.
 *
 * Exhaustiveness here is stricter than the edit-time check in the
 * statics: a column whose type is `Unknown` is refused rather than
 * treated as vacuously covered (`Coverage.check(~strict_unknown)`),
 * because certifying a proof is not the same as marking an error. */
and check_split =
    (
      ~step,
      ~statics,
      ~ctx,
      ~id,
      incoming,
      scrut: Exp.t,
      case_facts: case_facts,
      cases: list((Pat.t, Proof.t)),
    )
    : (option(Exp.t), ProofMap.t) => {
  let tyctx = SemanticCtx.get_ctx(ctx);
  let (gate_marks, gate_obligations) =
    Gate.scrutinee(~step, ~statics, ~ctx, ~origin=id, scrut);
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
      switch (Typ.term_of(ty)) {
      | Unknown(_) => true
      | _ => false
      };
    let rec ctx_ty = (e: Exp.t): option(Typ.t) =>
      switch (Exp.term_of(e)) {
      | Parens(inner) => ctx_ty(inner)
      | Asc(inner, t) =>
        switch (Typ.term_of(t)) {
        | Unknown(_) => ctx_ty(inner)
        | _ => Some(statics.typ(Exp.rep_id(e)) |> Option.value(~default=t))
        }
      | Var(x) =>
        Ctx.lookup_var(tyctx, x)
        |> Option.map(~f=(entry: Ctx.var_entry) => entry.typ)
      | _ => None
      };
    let statics_ty =
      statics.typ(scrut_id)
      |> Option.value(~default=Typ.fresh(Unknown(Internal)));
    switch (ctx_ty(scrut)) {
    | Some(ty) when head_unknown(statics_ty) && !head_unknown(ty) => ty
    | _ => statics_ty
    };
  };
  let scrut_co_ctx =
    statics.co_ctx(scrut_id) |> Option.value(~default=CoCtx.empty);
  let induction_ctx =
    SemanticCtx.map_ctx(
      _ => Ctx.forget_facts(Induction.mentions(scrut), tyctx),
      ctx,
    );
  /* The goal and scope of the case for `pat`. */
  let case = (pat: Pat.t) => {
    let added_vars = pat |> Pat.bindings |> Binding.variable_names;
    let replace_scrut = (by: Pat.t, goal: Exp.t) =>
      Induction.replace(
        statics,
        scrut,
        scrut_co_ctx,
        Exp.of_pat(by),
        by |> Pat.bindings |> CoCtx.of_bindings,
        added_vars,
        goal,
      );
    let case_ctx =
      switch (case_facts) {
      /* The case equation, absent when the pattern's variables capture
       * the scrutinee. */
      | CaseEquation =>
        let case_ctx = bind(ctx, pat, scrut_ty);
        CoCtx.has_any(scrut_co_ctx, added_vars)
          ? case_ctx
          : SemanticCtx.add_hypothesis(
              case_ctx,
              "case_eq",
              Exp.fresh(BinOp(Poly(Equals), scrut, Exp.of_pat(pat))),
            )
            |> fst;
      | InductiveHypotheses =>
        let goal = incoming |> Option.value(~default=Exp.fresh(EmptyHole));
        let ihs =
          Induction.hypotheses(statics, scrut_ty, pat)
          |> List.filter_map(~f=h =>
               replace_scrut(h, goal)
               |> Option.map(~f=ih => (Induction.hypothesis_name(h), ih))
             );
        List.fold_left(
          ~f=
            (ctx, (name, ih)) =>
              SemanticCtx.add_hypothesis(ctx, name, ih) |> fst,
          ~init=bind(induction_ctx, pat, scrut_ty),
          ihs,
        );
      };
    /* `replace_scrut` gives no goal when a pattern variable would capture
     * a use in the goal away from the scrutinee. */
    let case_goal = Option.bind(incoming, ~f=replace_scrut(pat));
    let captured =
      switch (incoming, case_goal) {
      | (Some(goal), None) =>
        let free_in = (e, x) => Exp.occurs_free_any([x], e);
        Some(
          List.find(
            ~f=x => free_in(goal, x) && !free_in(scrut, x),
            added_vars,
          )
          |> Option.value(~default="?"),
        );
      | (Some(_), Some(_))
      | (None, _) => None
      };
    (case_goal, case_ctx, captured);
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
    | Tuple(ps) => List.exists(~f=has_wild, ps)
    | Cons(a, b)
    | Add(a, b)
    | TupLabel(a, b)
    | Ap(a, b) => has_wild(a) || has_wild(b)
    | Parens(q)
    | Projector(_, q)
    | Asc(q, _) => has_wild(q)
    };
  let (outgoing, marks, m) =
    if (List.exists(~f=((p, _)) => has_wild(p), cases)) {
      (incoming, [ProofMark.InductionUnsupportedWildcard], ProofMap.empty);
    } else {
      let pats = List.map(~f=fst, cases);
      let checked =
        List.map(
          ~f=
            ((pat, body)) => {
              let (case_goal, case_ctx, captured) = case(pat);
              (
                check(~step, ~statics, ~ctx=case_ctx, case_goal, body),
                captured,
              );
            },
          cases,
        );
      /* One mark per name, so each refused arm can say why. */
      let captures =
        List.filter_map(~f=snd, checked)
        |> ListUtil.dedup
        |> List.map(~f=var => ProofMark.BinderCaptures({var: var}));
      /* Strict mode also catches nested `Unknown`s, e.g. an ADT whose
       * payload type is a hole. */
      let constraints =
        List.filter_map(
          ~f=pat => statics.pat_constraint(Pat.rep_id(pat)),
          pats,
        );
      let is_exhaustive =
        Poly.equal(
          Coverage.check(~strict_unknown=true, constraints, scrut_ty).
            exhaustiveness,
          Exhaustive,
        );
      let marks =
        captures
        @ (
          switch (is_exhaustive, Typ.term_of(scrut_ty)) {
          | (true, _) => []
          /* Covers a missing type too, which falls back to
           * `Unknown(Internal)` above. */
          | (false, Unknown(_)) => [ProofMark.InductionScrutineeUntyped]
          | (false, _) => [ProofMark.InductionNotExhaustive]
          }
        );
      /* The step's conclusion is the goal itself, so it steps to `true`
       * whether or not its cases are finished; an open case keeps the
       * proof incomplete (`ProofMap.status_of_proof`). A split with no
       * cases is exhaustive only on a type with no values, where the
       * goal holds vacuously. */
      let outgoing =
        is_exhaustive ? Some(Exp.temp(Atom(Bool(true)))) : incoming;
      let m =
        List.fold_left(
          ~f=(acc, ((_, m), _)) => ProofMap.union(acc, m),
          ~init=ProofMap.empty,
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
