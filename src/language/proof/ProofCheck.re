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
      (),
    )
    : ProofMap.entry => {
  incoming,
  auto_incoming,
  auto_outgoing,
  outgoing,
  marks,
};

/* Record an entry at `id`, merged into accumulated map `m`. */
let record =
    (
      ~marks: list(ProofMark.t)=[],
      ~auto_incoming: list((string, Exp.t))=[],
      ~auto_outgoing: list((Exp.t, string))=[],
      id: Id.t,
      incoming,
      outgoing,
      m: ProofMap.t,
    )
    : ProofMap.t =>
  ProofMap.add(
    id,
    entry(~incoming, ~auto_incoming, ~auto_outgoing, ~outgoing, ~marks, ()),
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

/* Canonical axiom-step outgoing: given a resolved equality name and
 * occurrence index, produce the rewritten `incoming`. Used by both the
 * evaluator and the UI stepper. Option-returning (no mark taxonomy) so
 * the UI stepper module stays unchanged. */
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
    : option(Exp.t) => {
  let proof_ctx = ProofCtx.of_env(~builtins=Axioms.v, ~ctx, env);
  switch (ProofCtx.lookup_rule(equality, proof_ctx)) {
  | None => None
  | Some(rule) =>
    switch (ProofHacks.nth_exp_env(~env, at_exp, at_idx, incoming)) {
    | None => None
    | Some(e) =>
      let (l, r) = ProofRule.can_eq(~info_map, ~env, rule, e);
      let with_exp =
        switch (direction) {
        | Direction.Left => l
        | Direction.Right => r
        };
      switch (with_exp) {
      | None => None
      | Some(w) =>
        Some(ProofHacks.replace_exp_id(Exp.rep_id(e), incoming, w))
      };
    }
  };
};

/* Structured-error variant of `axiom_step_outgoing` used by `check`.
 * Categorises each failure into a `ProofMark.t`. */
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
    : result(Exp.t, ProofMark.t) => {
  let proof_ctx = ProofCtx.of_env(~builtins=Axioms.v, ~ctx, env);
  switch (ProofCtx.lookup_rule(equality, proof_ctx)) {
  | None => Error(UnknownEquality(equality))
  | Some(rule) =>
    switch (ProofHacks.nth_exp_env(~env, at_exp, at_idx, incoming)) {
    | None =>
      Error(
        PatternNotFound({
          at_exp,
          at_idx,
        }),
      )
    | Some(e) =>
      let (l, r) = ProofRule.can_eq(~info_map, ~env, rule, e);
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
      | Some(w) => Ok(ProofHacks.replace_exp_id(Exp.rep_id(e), incoming, w))
      };
    }
  };
};

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
    : result(Exp.t, ProofMark.t) =>
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

/* Peel the outermost binder from an incoming "for all pat, P" goal. Used
 * by the `Forall` and `Intro` proof forms to walk under the binder. */
let peel_binder = (incoming: option(Exp.t)): option((Pat.t, Typ.t, Exp.t)) => {
  open OptUtil.Syntax;
  let* e = incoming;
  switch (e |> Exp.term_of) {
  | Fun(p, d1, t, _) =>
    let t = OptUtil.get(() => Typ.fresh(Unknown(Internal)), t);
    Some((p, t, d1));
  | Forall(p, d1) =>
    /* No annotated type at this level, use Unknown. */
    Some((p, Typ.fresh(Unknown(Internal)), d1))
  | _ => None
  };
};

/* Translate an Ok/Error result from one of the *_ast helpers into an
 * (outgoing, marks) pair suitable for `record`. */
let result_to_outgoing =
    (r: result(Exp.t, ProofMark.t)): (option(Exp.t), list(ProofMark.t)) =>
  switch (r) {
  | Ok(e) => (Some(e), [])
  | Error(m) => (None, [m])
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
    /* Leaf: incoming is known but outgoing is broken. No mark: holes
     * reflect an intentionally-incomplete proof, not an error. */
    (None, record(id, incoming, None, ProofMap.empty))
  | Invalid(_) =>
    /* Unparseable proof text is an error, unlike an EmptyHole. */
    (
      None,
      record(
        ~marks=[ProofMark.MalformedProofTerm],
        id,
        incoming,
        None,
        ProofMap.empty,
      ),
    )
  | MultiHole(_) =>
    /* We don't recurse into the any-kind children here since they are
     * not proof terms; treat the whole multi-hole as opaquely broken. */
    (
      None,
      record(
        ~marks=[ProofMark.MalformedProofTerm],
        id,
        incoming,
        None,
        ProofMap.empty,
      ),
    )
  | Seq(p1, p2) =>
    let (out1, m1) = check(~step, ~info_map, ~ctx, incoming, p1);
    let (out2, m2) = check(~step, ~info_map, ~ctx, out1, p2);
    (out2, record(id, incoming, out2, ProofMap.union(m1, m2)));
  | AxiomStep({at_idx, at_exp, direction, equality}) =>
    let (outgoing, marks) =
      switch (incoming) {
      | None => (None, [ProofMark.MissingIncoming])
      | Some(inc) =>
        result_to_outgoing(
          axiom_step_outgoing_ast(
            ~info_map,
            ~env=SemanticCtx.get_env(ctx),
            ~ctx=SemanticCtx.get_ctx(ctx),
            ~at_idx,
            ~at_exp,
            ~direction,
            ~equality,
            inc,
          ),
        )
      };
    (outgoing, record(~marks, id, incoming, outgoing, ProofMap.empty));
  | AlgebriteStep({at_idx, at_exp, with_exp}) =>
    let (outgoing, marks) =
      switch (incoming) {
      | None => (None, [ProofMark.MissingIncoming])
      | Some(inc) =>
        result_to_outgoing(
          algebrite_step_outgoing_ast(~at_idx, ~at_exp, ~with_exp, inc),
        )
      };
    (outgoing, record(~marks, id, incoming, outgoing, ProofMap.empty));
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
        | Error(mark) => ([], [], None, [mark])
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
        | Some((p, t, inner)) => (
            Some(inner),
            SemanticCtx.add_from_pattern(ctx, p, t),
            [],
          )
        | None => (None, ctx, [ProofMark.ExpectedForallGoal])
        }
      };
    let (out_body, m) =
      check(~step, ~info_map, ~ctx=ctx', body_incoming, body);
    /* Outgoing mirrors the body's outgoing (the forall is discharged when
     * the body reduces the goal to `true`). */
    let outgoing = out_body;
    (outgoing, record(~marks=binder_marks, id, incoming, outgoing, m));
  | Induction(scrut, cases) =>
    let (out, marks, m) =
      check_induction(~step, ~info_map, ~ctx, ~incoming, ~scrut, ~cases);
    (out, record(~marks, id, incoming, out, m));
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
          ProofHacks.get_inductive_hypotheses(info_map, scrut_ty, pat)
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
        let ctx' =
          List.fold_left(
            (c, ih) => {
              let (c', _) = SemanticCtx.add_hypothesis(c, "ih", ih);
              c';
            },
            ctx',
            ihs,
          );
        /* 3. Recurse on body. */
        let (out_body, m_body) =
          check(~step, ~info_map, ~ctx=ctx', case_incoming, body);
        /* Collect the pattern constraint for later coverage check. */
        let constraint_ =
          switch (Statics.Map.lookup_pat(Pat.rep_id(pat), info_map)) {
          | Some(info_pat) => Some(Info.pat_constraint(info_pat))
          | None => None
          };
        (
          outs @ [out_body],
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
         | Some(e) => Exp.fast_equal(e, true_exp)
         | None => false,
       );
  /* Structural marks on the induction node itself. Per-case failures
   * already surface as marks on their own sub-term ids. */
  let marks =
    (List.length(cases) == 0 ? [ProofMark.InductionEmptyCases] : [])
    @ (
      !is_exhaustive && List.length(cases) > 0
        ? [ProofMark.InductionNotExhaustive] : []
    );
  let outgoing =
    is_exhaustive && all_true && List.length(cases) > 0
      ? Some(true_exp) : None;
  (outgoing, marks, m_acc);
};
