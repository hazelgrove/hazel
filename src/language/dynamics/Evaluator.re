open Transition;
open Trampoline.Syntax;

module EvaluatorEVMode: {
  type status =
    | Final
    | Uneval;

  include
    EV_MODE with
      type inner_result = Trampoline.t(DHExp.t) and
      type result =
        Trampoline.t(
          (status, list(EvaluatorState.effect), option(step_kind), DHExp.t),
        );
} = {
  type status =
    | Final
    | Uneval;

  type inner_result = Trampoline.t(DHExp.t);
  /* The step kind rides the result (None for non-step finals) so eval_2
     can read the step's provenance (Transition.provenance_of_kind) and
     declare delegated re-evaluations. */
  type result =
    Trampoline.t(
      (status, list(EvaluatorState.effect), option(step_kind), DHExp.t),
    );
  type requirement('a) = Trampoline.t('a);
  type requirements('a, 'b) = Trampoline.t(('a, 'b));

  let req_final = (f, _, x) => {
    let.trampoline x = Next(() => f(x));
    Trampoline.return(x);
  };

  let rec req_all_final = (f, i, xs) =>
    switch (xs) {
    | [] => Trampoline.return([])
    | [x, ...xs] =>
      let.trampoline x' = req_final(f, x => x, x);
      let.trampoline xs' = req_all_final(f, i, xs);
      Trampoline.return([x', ...xs']);
    };

  let otherwise = (_, c) => Trampoline.return(((), c));
  let (and.) = (t1, t2) => {
    let.trampoline (x1, c1) = t1;
    let.trampoline x2 = t2;
    Trampoline.return(((x1, x2), c1(x2)));
  };
  let (let.) = (t1, s) => {
    let.trampoline (x, c) = t1;
    switch (s(x)) {
    | Step({expr, side_effects, kind, is_value: true}) =>
      Trampoline.return((Final, side_effects, Some(kind), expr))
    | Step({expr, side_effects, kind, is_value: false}) =>
      Trampoline.return((Uneval, side_effects, Some(kind), expr))
    | Constructor
    | Value
    | Indet => Trampoline.return((Final, [], None, c))
    };
  };
};

module Eval = Transition(EvaluatorEVMode);

/* A declared delegation: a delegating administrative step (cast
 * distribution rebuilding a redex under its own id — see
 * Transition.provenance_of_kind) pushes the redex's span key here, and a
 * nested evaluation matching it CONTINUES the enclosing observation span
 * instead of opening its own (the delegation law; consumed in
 * eval_3_record_probe_sample). Keys are (syntax id, call-stack instance);
 * genuine re-entry (recursion) always differs in stack. The stack is
 * kept by reference, not projected to ids: projecting is O(depth) per
 * delegating step, which made an ascription inside deep recursion
 * quadratic (#2524). Only probe targets are declared, since only a
 * target's key is ever looked up. */
type delegation = (Id.t, CallStack.t);

let continues_delegation =
    (expr_id: Id.t, call_stack: CallStack.t, delegations: list(delegation))
    : bool =>
  List.exists(
    ((id, stack): delegation) =>
      Id.equal(id, expr_id)
      /* the continuation runs on the very stack value that was declared */
      && (stack === call_stack || CallStack.equal(stack, call_stack)),
    delegations,
  );

/* The flag a binding made by a step of kind `kind` carries: the flag its
 * right-hand side actually finished with, at the callstack it ran at -- the
 * formalism's letP, App and case premises, where the bound entry's flag is
 * the one Eval1 returned for e1, e2 or the scrutinee. Those three steps
 * evaluate that expression themselves, just before binding, so its flag in
 * `flags` is the one from this very evaluation (see IncrEval's `flags`).
 * A fixpoint's self-binding and a theorem's are made before their
 * right-hand side runs at all; the formalism's fix rule is Dirty, and so are
 * they. */
let binding_flag =
    (
      ~flags: IncrEval.flags,
      ~call_stack_ids: list(Id.t),
      ~kind: option(step_kind),
      rhs: DHExp.t,
    )
    : IncrEval.flag =>
  switch (kind) {
  | Some(LetBind(_) | FunAp | CaseApply) =>
    IncrEval.flag_at(flags, ~call_stack_ids, ~id=DHExp.rep_id(rhs))
  | _ => Dirty
  };

/* How many environment nodes a step's bindings mint: Environment.extend
 * adds one per variable a pattern binds. */
let nodes_bound = (effects: list(EvaluatorState.effect)): int =>
  List.fold_left(
    (n, effect) =>
      switch (effect) {
      | EvaluatorState.RecordPatMatch({pat: {term: Var(_), _}, _}) => n + 1
      | EvaluatorState.RecordPatMatch({pat, _}) =>
        n + List.length(Pat.bound_vars(pat))
      | _ => n
      },
    0,
    effects,
  );

let called_name = (effects: list(EvaluatorState.effect)): option(string) =>
  List.find_map(
    fun
    | EvaluatorState.RecordStackFrame(name, _, _) => name
    | _ => None,
    effects,
  );

let rec evaluate =
        // Constants
        (
          ~prev: EvaluatorState.incr_eval=IncrEval.empty,
          ~track_reuse: bool,
          ~tuple_flags: bool,
          /* a2's guard: the deepest callstack an entry may be keyed at. 0 is
           * aPL — only the empty callstack — and is what every mode without
           * the callstack_keys capability passes. */
          ~depth_limit: int,
          /* Each node's flag, as it finished, for the whole evaluation.
           * Shared like `closure_maps`. */
          ~flags: IncrEval.flags,
          ~eval_info: EvalInfo.t,
          // Call Stack
          ~in_closure=?,
          ~call_stack: CallStack.t,
          ~delegations: list(delegation),
          // Inputs
          ~reuse_map: IncrEval.reuse_map,
          /* The re-use map each closure captured, so a call reads the
           * closure's own map as the formalism's App rule does. Shared for
           * the whole evaluation, like `outbox`; None unless callstack keys
           * are on, since only then is a map read inside a call. */
          ~closure_maps: option(IncrEval.closure_maps),
          env,
          exp: DHExp.t,
          // Outputs
          ~parent_state: ref(EvaluatorState.t),
          ~outbox: option(ref(IncrEval.outbox(EvaluatorState.t))),
          ~current_top_id: option(Id.t),
        )
        : Trampoline.t(DHExp.t) => {
  /* NOTE: This trampoline looks like it only returns an expression, but
   * it also mutates the parent_state and outbox references while it's
   * running. */

  let evaluate =
    evaluate(
      ~prev,
      ~track_reuse,
      ~tuple_flags,
      ~depth_limit,
      ~flags,
      ~eval_info,
      ~outbox,
      ~closure_maps,
    );
  let expr_id = DHExp.rep_id(exp);
  /* A `match` or `if` takes its flag from the arm it continues into, which
   * its one step chooses (see IncrEval.case_continuation); the step leaves
   * the arm's id here for the node to read as it finishes. Only those two
   * forms allocate it. */
  let continued_into =
    switch (DHExp.term_of(exp)) {
    | Match(_)
    | If(_) when track_reuse && tuple_flags => Some(ref(None))
    | _ => None
    };
  /* A node evaluated as the DIRECT CHILD of a Closure reports back, through
   * `in_closure`, that its result still depends on that closure's
   * environment: Transition.wrap_closure_when_done takes the
   * `(Some(f), Constructor | Indet | Value)` branch, calls `f()`, and hands
   * the value back BARE, leaving the enclosing Closure to supply the
   * environment. With `in_closure = None` the same rule instead wraps the
   * value itself. So one program node has two possible final values, and
   * which one it has is decided by a signal that travels OUT OF BAND --
   * beside the value, not in it.
   *
   * That is what a cache entry cannot carry. A hit returns `entry.value`
   * without running the transition, so `f()` is never called; the enclosing
   * Closure sees `needs_closure` unset, takes its CompleteClosure step and
   * strips the environment the bare value needs. The residual then mentions
   * variables with no binder in it.
   *
   * Observe the signal here so that `record_value` below can store a value
   * that means the same thing in any context. */
  let (needed_closure, in_closure) =
    switch (in_closure) {
    /* `evaluate` runs once per node, so the common case -- not the direct
     * child of a Closure -- must not allocate. */
    | None => (None, None)
    | Some(f) =>
      let raised = ref(false);
      (
        Some(raised),
        Some(
          () => {
            raised := true;
            f();
          },
        ),
      );
    };
  /* Does the guard admit entries at this callstack? With depth_limit = 0
   * this is exactly `call_stack == []`, the condition it replaces. */
  let cacheable = (call_stack: CallStack.t): bool =>
    IncrEval.cacheable(~depth_limit, call_stack);
  /* The cache key for this evaluation, or None when the guard refuses it. */
  let cache_key = (call_stack: CallStack.t): option(list(Id.t)) =>
    cacheable(call_stack) ? Some(CallStack.ids_of_stack(call_stack)) : None;
  /* Whatever is evaluated inside a Closure runs under that closure's
   * environment, so under that environment's map. A step that binds names
   * has already left exactly that map to its continuation; this is for the
   * Closures no binding step built -- a type function's body, applied
   * without a call frame, runs under the type function's environment, not
   * the caller's. */
  let reuse_map =
    switch (closure_maps, exp.term) {
    | (Some(maps), Closure(closure_env, _))
        when track_reuse && cacheable(call_stack) =>
      IncrEval.closure_map(maps, ~closure=None, closure_env)
    | _ => reuse_map
    };
  /* Outbox publication keys only on proper program nodes
   * (EvalInfo.is_program_node).
   * Administrative/stepped intermediates never publish: doing so either
   * collides with StreamCollector's own Exp.temp nodes (truncating the
   * walk so streamed results appear to go backwards) or never matches
   * the walk; and carrying a prior program id across intermediates
   * publishes eval_5's fresh empty inner_state under it, making the
   * collector short-circuit. current_top_id = None ⇒ no outbox write,
   * so the last program-node publish stands until the next one. */
  let current_top_id =
    if (call_stack == [] && EvalInfo.is_program_node(expr_id, eval_info)) {
      Some(expr_id);
    } else if (call_stack == []) {
      None;
    } else if
      /* a2 collects a separate state slice for each cacheable expression
       * inside a call (eval_5), so publishing from inside one would put a
       * fragment of an enclosing top-level node's state under its id. The
       * last top-level publish stands instead. */
      (depth_limit > 0) {
      None;
    } else {
      current_top_id;
    };
  let replay_state = (state: EvaluatorState.t): EvaluatorState.t =>
    EvaluatorState.clear_transient({
      ...state,
      incr_eval: IncrEval.empty,
    });
  let update_outbox_current = (state: EvaluatorState.t) =>
    switch (outbox, current_top_id) {
    | (Some(outbox), Some(id)) =>
      outbox :=
        IncrEval.set_outbox_current(~id, ~state=replay_state(state), outbox^)
    | (None, _)
    | (_, None) => ()
    };

  // Fully evaluate all children and take this expression one step forward
  let eval_0_main =
      (
        ~reuse_map,
        ~in_closure=?,
        ~call_stack,
        ~delegations,
        ~state,
        env,
        exp: DHExp.t,
      )
      : EvaluatorEVMode.result => {
    Eval.transition(
      (~in_closure=?, env, child) =>
        evaluate(
          ~reuse_map,
          ~in_closure?,
          ~call_stack,
          ~delegations,
          ~parent_state=state,
          ~current_top_id,
          env,
          child,
        ),
      ~mode=`Environment,
      ~targets=eval_info.targets,
      ~in_closure?,
      env,
      exp,
    );
  };

  // Do the above but also run side effects on state and stack
  let eval_1_effects =
      (
        ~reuse_map,
        ~in_closure=?,
        ~call_stack,
        ~delegations,
        ~state,
        env,
        exp: DHExp.t,
      ) => {
    let.trampoline (is_finished, effects, kind, next) =
      eval_0_main(
        ~reuse_map,
        ~in_closure?,
        ~call_stack,
        ~delegations,
        ~state,
        env,
        exp,
      );

    /* The step's children, a binding's right-hand side among them, ran at
     * the callstack the step was taken at, not the one a call pushes. */
    let step_call_stack = call_stack;
    let (call_stack, new_state) =
      EvaluatorState.update(eval_info, state^, call_stack, env, exp, effects);

    state := new_state;
    update_outbox_current(state^);

    /* Maintain the re-use map wherever the guard admits entries: under a2
     * that includes function bodies, which is what lets the re-use check
     * fire inside a call. Skip entirely when nothing downstream can consume
     * the map — beyond the guard's depth nothing is recorded or re-used, and
     * depth only grows from here. */
    let maintaining_reuse_map = track_reuse && cacheable(call_stack);
    let body_reuse_map =
      if (!maintaining_reuse_map) {
        reuse_map;
      } else {
        /* A call replaces the environment with the closure's rather than
         * extending it, so the map is switched to the closure's before the
         * parameter binding is added on top of it — a2's App rule evaluates
         * the body under rho'[x |-> <uid(e2), f>]. */
        let entered_call =
          List.exists(
            fun
            | EvaluatorState.RecordStackFrame(_) => true
            | _ => false,
            effects,
          );
        let body_env =
          entered_call
            ? switch (next.term) {
              | Closure(body_env, _) => Some(body_env)
              /* A builtin application pushes a frame without changing the
               * environment, so the caller's map still describes it. */
              | _ => None
              }
            : None;
        let base =
          switch (body_env) {
          | Some(body_env) =>
            /* The formalism's App rule reads rho straight off the closure.
             * The closure's environment sits exactly as many nodes down the
             * body's as the application's pattern bound. */
            switch (closure_maps) {
            | Some(maps) =>
              IncrEval.closure_map(
                maps,
                ~closure=called_name(effects),
                IncrEval.drop_env(nodes_bound(effects), body_env),
              )
            /* A call is inside the guard only under callstack keys, and
             * those always keep closure maps. */
            | None =>
              failwith(
                "Evaluator: call inside the guard without closure maps",
              )
            }
          /* A type function's body runs under the type function's
           * environment, without a call frame, so at the caller's callstack
           * and inside the guard. Only callstack keys keep the map of that
           * environment (the Closure case at the top of `evaluate` reads it);
           * without them the body gets no map at all, re-using nothing that
           * mentions a variable rather than the caller's map, which names
           * the caller's bindings. */
          | None when kind == Some(TypFunAp) && Option.is_none(closure_maps) => IncrEval.empty_reuse_map
          | None => reuse_map
          };
        /* The argument was evaluated out here, so its flag is read at the
         * caller's callstack even when the binding lands in the body's map. */
        let call_stack_ids = CallStack.ids_of_stack(step_call_stack);
        List.fold_left(
          (reuse_map, effect) =>
            switch (effect) {
            | EvaluatorState.RecordPatMatch({pat, rhs, _}) =>
              IncrEval.update_maps_after_binding(
                ~flag=binding_flag(~flags, ~call_stack_ids, ~kind, rhs),
                ~source_id=DHExp.rep_id(rhs),
                pat,
                ~reuse_map,
              )
            | _ => reuse_map
            },
          base,
          effects,
        );
      };

    /* The arm a `match` or `if` continues into, if its flag may be carried
     * up through it; read back in eval_4_reuse once the arm has finished. */
    switch (continued_into, kind, DHExp.term_of(exp)) {
    | (Some(cell), Some(CaseApply), Match(scrut, rules))
        when cacheable(step_call_stack) =>
      let call_stack_ids = CallStack.ids_of_stack(step_call_stack);
      let taken =
        List.find_map(
          fun
          | EvaluatorState.RecordPatMatch({pat, _}) => Some(pat)
          | _ => None,
          effects,
        );
      let rec index_of = (i, rules: list((Pat.t, DHExp.t))) =>
        switch (rules, taken) {
        | ([(p, _), ..._], Some(pat)) when p === pat => Some(i)
        | ([_, ...rules], _) => index_of(i + 1, rules)
        | ([], _) => None
        };
      cell :=
        Option.bind(index_of(0, rules), arm =>
          IncrEval.case_continuation(
            ~prev,
            ~call_stack_ids,
            ~scrut_flag=
              IncrEval.flag_at(
                flags,
                ~call_stack_ids,
                ~id=DHExp.rep_id(scrut),
              ),
            ~arm,
            exp,
          )
        );
    | (Some(cell), Some(Conditional(took_then)), If(cond, _, _))
        when cacheable(step_call_stack) =>
      let call_stack_ids = CallStack.ids_of_stack(step_call_stack);
      cell :=
        IncrEval.if_continuation(
          ~prev,
          ~call_stack_ids,
          ~cond_flag=
            IncrEval.flag_at(flags, ~call_stack_ids, ~id=DHExp.rep_id(cond)),
          ~took_then,
          exp,
        );
    | _ => ()
    };

    /* Record the map of each environment node this step minted. A binding
     * step extends the environment and the map in the same breath -- the
     * formalism's `sigma_c.set x v` beside its `rho_c.set x (...)` -- so this
     * is the map a closure over the new node captures. Inside the guard that
     * map is `body_reuse_map`. Beyond it no map is maintained, so the step
     * records only what it bound, in O(1), and the map is built from that if
     * a closure over the node is ever called back inside the guard. Every
     * flag there is Dirty, except a parameter whose argument the guard's
     * last cacheable frame evaluated. A step that mints no node presents an
     * environment an earlier step already described. */
    switch (closure_maps, next.term) {
    | (Some(maps), Closure(produced_env, _)) =>
      let added = nodes_bound(effects);
      if (added == 0) {
        ();
      } else if (maintaining_reuse_map) {
        IncrEval.record_captured(maps, ~env=produced_env, body_reuse_map);
      } else {
        let call_stack_ids =
          cacheable(step_call_stack)
            ? Some(CallStack.ids_of_stack(step_call_stack)) : None;
        IncrEval.record_bindings(
          maps,
          ~env=produced_env,
          ~added,
          List.filter_map(
            fun
            | EvaluatorState.RecordPatMatch({pat, rhs, _}) =>
              Some(
                IncrEval.{
                  pat,
                  source_id: DHExp.rep_id(rhs),
                  flag:
                    switch (call_stack_ids) {
                    | Some(call_stack_ids) =>
                      binding_flag(~flags, ~call_stack_ids, ~kind, rhs)
                    | None => Dirty
                    },
                },
              )
            | _ => None,
            effects,
          ),
        );
      };
    | _ => ()
    };

    Trampoline.return((is_finished, call_stack, body_reuse_map, kind, next));
  };

  // Do the above but until the expression is final
  let eval_2_until_final =
      (
        ~reuse_map,
        ~in_closure=?,
        ~call_stack,
        ~delegations,
        ~state,
        env,
        exp: DHExp.t,
      ) => {
    let.trampoline (is_finished, call_stack, body_reuse_map, kind, next) =
      eval_1_effects(
        ~reuse_map,
        ~in_closure?,
        ~call_stack,
        ~delegations,
        ~state,
        env,
        exp,
      );

    switch (is_finished) {
    | Final => Trampoline.return(next)
    | Uneval =>
      /* A delegating step re-evaluates this redex's id inside `next`;
       * declare the continuation so the nested eval_3 doesn't open a
       * second span. Populated by the dynamics' own provenance — not
       * inferred — so an undeclared same-id re-evaluation now surfaces
       * as a visible duplicate sample instead of a silent suppression. */
      let delegations =
        switch (Option.map(provenance_of_kind, kind)) {
        | Some(Administrative({may_delegate: true}))
            when Id.Map.mem(DHExp.rep_id(exp), eval_info.targets) => [
            (DHExp.rep_id(exp), call_stack),
            ...delegations,
          ]
        | _ => delegations
        };
      Trampoline.Next(
        () =>
          evaluate(
            ~reuse_map=body_reuse_map,
            ~in_closure?,
            ~call_stack,
            ~delegations,
            ~parent_state=state,
            ~current_top_id,
            env,
            next,
          ),
      );
    };
  };

  // Do the above but also record probe samples if required
  let eval_3_record_probe_sample =
      (
        ~call_stack: CallStack.t,
        ~delegations,
        ~state: ref(EvaluatorState.t),
        ~expr_id,
        env,
        exp: DHExp.t,
      ) => {
    let current_step_count = state^.step_count;

    /* Save original call_stack before update. For probed compound expressions
     * (Uneval case), we need this because:
     * - The updated call_stack (after RecordStackFrame) should be passed to
     *   recursive evaluation so inner expressions see the app_id
     * - But the probe sample for THIS expression should use the original
     *   call_stack (what it was before entering the function) */
    let original_call_stack = call_stack;

    /* Delegation law: at most one observation span per (source id,
     * call-stack instance). A delegating administrative step declares the
     * continuation (see eval_2); an evaluation matching a declared key
     * CONTINUES that span — no second SpanOpen, no second sample. The
     * span-opening evaluation mints on close: full step range, post-cast
     * value, enter-data attached to the open span by the trace fold.
     * (The different-stack flavor of this smear is still handled
     * heuristically by ascription dominance in Sample.Map.dominated,
     * applied by the trace fold — unifying it needs delegation markers
     * that scope over subtree re-evaluation; deferred.) */
    let is_target = Id.Map.find_opt(expr_id, eval_info.targets);
    let continues_delegated_span =
      switch (is_target) {
      | None => false
      | Some(_) => continues_delegation(expr_id, call_stack, delegations)
      };
    switch (is_target) {
    | Some(_) when !continues_delegated_span =>
      state :=
        EvaluatorState.record_event(
          state^,
          ObsTrace.SpanOpen({
            syntax_id: expr_id,
            stack: original_call_stack,
            step: current_step_count,
          }),
        )
    | _ => ()
    };

    let.trampoline final_value =
      eval_2_until_final(
        ~reuse_map,
        ~in_closure?,
        ~call_stack,
        ~delegations,
        ~state,
        env,
        exp,
      );

    // Record probe sample when the span this evaluation opened closes
    switch (is_target) {
    | Some(probe) when !continues_delegated_span =>
      /* Closing the span mints the sample: record_event runs the trace
       * fold, which pops the open span (start step, stack, enter-data)
       * and lands the sample in state.probes. */
      state :=
        EvaluatorState.record_event(
          state^,
          ObsTrace.SpanClose({
            syntax_id: expr_id,
            value: final_value,
            env,
            spec: probe,
            step: state^.step_count - 1,
          }),
        );
      update_outbox_current(state^);
    | _ => ()
    };

    Trampoline.return(final_value);
  };

  // Do the above but also reuse the previous result if possible
  let eval_4_reuse =
      (
        ~call_stack: CallStack.t,
        ~delegations,
        ~state: ref(EvaluatorState.t),
        ~expr_id,
        env,
        exp: DHExp.t,
      )
      : Trampoline.t(DHExp.t) => {
    let key = cache_key(call_stack);
    /* The value as a LATER run has to read it. That run will not re-run this
     * node's transition, so it cannot re-raise `in_closure` (see
     * `needed_closure` above); if this evaluation leaned on the enclosing
     * closure for its environment, the entry has to carry that environment
     * itself or the replayed value is an open term.
     *
     * Only the ENTRY is rewritten -- this run still returns `final_value`
     * unchanged, so the closure the caller already holds is not duplicated.
     * A value that is already a Closure carries its environment and is left
     * alone. The recorded environment is the right one to carry: reuse_check
     * only grants a hit when every name in this expression's co-context is
     * bound as it was when the entry was recorded, which is the same premise
     * that makes `entry.value` applicable at all. */
    let record_value = (v: DHExp.t): DHExp.t =>
      switch (needed_closure) {
      | Some(raised) when raised^ =>
        switch (v.term) {
        | Grammar.Closure(_, _) => v
        /* Mirror Transition's `generated`: a minted node gets a real id only
         * when something (probe targets) can address it. */
        | _ =>
          let term: Exp.term = Grammar.Closure(env, v);
          Id.Map.is_empty(eval_info.targets)
            ? Exp.temp(term) : Exp.fresh(term);
        }
      | Some(_)
      | None => v
      };
    /* Record under the full (callstack, id) key. */
    let record = (state: EvaluatorState.t, entry) =>
      switch (key) {
      | None => state
      | Some(call_stack_ids) => {
          ...state,
          incr_eval:
            IncrEval.add_entry_at(
              ~call_stack_ids,
              ~id=expr_id,
              entry,
              state.incr_eval,
            ),
        }
      };
    switch (
      key,
      IncrEval.reuse_check(
        ~call_stack_ids=key,
        ~prev,
        ~reuse_map,
        ~eval_info,
        ~id=expr_id,
      ),
    ) {
    | (Some(call_stack_ids), Some(entry)) =>
      // Evaluation cache hit: reuse previous result
      IncrEval.record_flag(flags, ~call_stack_ids, ~id=expr_id, Clean);
      state := EvaluatorState.append(state^, entry.state);
      update_outbox_current(state^);
      // Add the entry to the next incremental evaluation cache
      state := record(state^, entry);
      // Copy cache entries for the reused subtree from prev
      state :=
        {
          ...state^,
          incr_eval:
            IncrEval.copy_descendant_entries(
              ~call_stack_ids,
              ~root_id=expr_id,
              ~root=entry.prev_elab,
              ~prev,
              state^.incr_eval,
            ),
        };
      Trampoline.return(entry.value);
    | (None, _)
    | (_, None) =>
      // Evaluation cache miss: evaluate the expression from scratch
      let.trampoline final_value =
        eval_3_record_probe_sample(
          ~call_stack,
          ~delegations,
          ~state,
          ~expr_id,
          env,
          exp,
        );

      /* Record the flag this node finished with before anything reads it:
       * its parent as the parent finishes, or the binding its value lands
       * in. */
      switch (key) {
      | Some(call_stack_ids) when track_reuse =>
        IncrEval.record_flag(
          flags,
          ~call_stack_ids,
          ~id=expr_id,
          tuple_flags
            ? IncrEval.node_flag(
                ~prev,
                ~flags,
                ~call_stack_ids,
                ~reuse_map,
                ~continued_into=Option.bind(continued_into, cell => cell^),
                exp,
              )
            : Dirty,
        )
      | Some(_)
      | None => ()
      };

      // Record incremental entry if required
      let info_snapshot =
        if (key == None || !track_reuse) {
          None;
        } else {
          EvalInfo.find_opt(expr_id, eval_info);
        };
      switch (info_snapshot) {
      | None => Trampoline.return(final_value)
      | Some({
          elab_term: prev_elab,
          co_ctx,
          probe_targets: prev_probe_targets,
          _,
        }) =>
        let entry: IncrEval.entry(EvaluatorState.t) = {
          prev_elab,
          prev_reuse_map:
            IncrEval.make_clean(
              IncrEval.restrict_to_co_ctx(reuse_map, co_ctx),
            ),
          prev_probe_targets,
          value: record_value(final_value),
          state: replay_state(state^),
        };

        /* The outbox is keyed by id alone — it is what the streaming
         * collector replays a top-level node from — so only top-level
         * entries are published. A nested entry reaches the next run
         * through the evaluation state instead. */
        switch (outbox) {
        | Some(outbox) when call_stack == [] =>
          outbox := IncrEval.add_outbox_entry(expr_id, entry, outbox^);
          switch (closure_maps) {
          | Some(maps) =>
            outbox := IncrEval.carry_streamed(maps, entry.value, outbox^)
          | None => ()
          };
        | Some(_)
        | None => ()
        };
        state := record(state^, entry);
        Trampoline.return(final_value);
      };
    };
  };

  /* [PERF] We collect separate states for cacheable expressions so we can
   * replay those states. Under a2 that includes expressions inside a call:
   * an entry's state slice has to be the slice that expression produced,
   * not the enclosing call's. */
  let eval_5_state_merge =
      (~call_stack: CallStack.t, ~delegations, ~state, ~expr_id, env, exp) =>
    if (cacheable(call_stack)) {
      let inner_state =
        ref(EvaluatorState.empty_at(parent_state^.step_count));
      let.trampoline final_value =
        eval_4_reuse(
          ~call_stack,
          ~delegations,
          ~state=inner_state,
          ~expr_id,
          env,
          exp,
        );
      let new_state = EvaluatorState.append(state^, inner_state^);
      state :=
        {
          ...new_state,
          incr_eval:
            IncrEval.add_stream(inner_state^.incr_eval, new_state.incr_eval),
        };
      update_outbox_current(inner_state^);
      Trampoline.return(final_value);
    } else {
      eval_4_reuse(~call_stack, ~delegations, ~state, ~expr_id, env, exp);
    };

  eval_5_state_merge(
    ~call_stack,
    ~delegations,
    ~state=parent_state,
    ~expr_id,
    env,
    exp,
  );
};

[@deriving (show({with_path: false}), sexp, yojson)]
type limited_result =
  | LimitedCompleted((Exp.t, EvaluatorState.t))
  | StepLimitExceeded;

let finish = (~env, e: DHExp.t): Exp.t =>
  e |> Substitution.in_exp(env) |> Exp.replace_all_ids;

/* Shared setup for all evaluation entry points: build the (unstarted)
 * evaluation trampoline. No re-use pass runs first: every flag the
 * evaluator reads is one a node actually finished with (`binding_flag`). */
let prepare_evaluation =
    (
      ~calculus: Calculus.t,
      ~prev,
      ~eval_info: EvalInfo.t,
      ~env,
      ~reuse_map: option(IncrEval.reuse_map),
      ~outbox,
      d: DHExp.t,
    )
    : (ref(EvaluatorState.t), Trampoline.t(DHExp.t)) => {
  /* A0 is the non-incremental control: it must not read a cache even if one
   * is handed to it, so that the mode alone fixes the scheme. Dropping prev
   * here also drives track_reuse to false, which switches off the re-use map
   * and entry recording — the control should not pay for bookkeeping it
   * never reads. */
  let caps = Calculus.capabilities(calculus);
  let reuse = caps.reuse;
  let tuple_flags = caps.tuple_flags;
  /* The guard is one constant, read once here: 0 keys entries by id alone
   * (aPL), and anything larger is a2 keying them by callstack and id. */
  let depth_limit = caps.callstack_keys ? Calculus.callstack_depth_limit : 0;
  let prev = reuse ? prev : IncrEval.empty;
  /* The reuse map is only ever consumed by reuse_check or by incr-entry
   * snapshots, both of which need statics in eval_info (reuse_check also
   * needs a non-empty prev). When neither can fire — e.g. `hazel run`,
   * MVU app dispatch — skip maintaining it: the per-binder
   * remove_pat_bindings walk dominates evaluation otherwise. */
  let track_reuse =
    reuse && (!IncrEval.is_empty(prev) || EvalInfo.has_statics(eval_info));
  /* Only a call reads a closure's map, and only callstack keys cache
   * anything inside a call, so no other calculus keeps them. */
  let closure_maps =
    caps.callstack_keys && track_reuse
      ? Some(IncrEval.new_closure_maps(~prev, ~env, ~top_map=reuse_map))
      : None;
  let reuse_map =
    switch (reuse_map) {
    | Some(m) => m
    | None =>
      track_reuse
        ? IncrEval.clean_reuse_map_of_env(env) : IncrEval.empty_reuse_map
    };
  let state = ref(EvaluatorState.empty);
  let result =
    evaluate(
      ~prev,
      ~track_reuse,
      ~tuple_flags,
      ~depth_limit,
      ~eval_info,
      ~call_stack=CallStack.empty,
      ~delegations=[],
      ~reuse_map,
      ~flags=IncrEval.new_flags(),
      ~parent_state=state,
      ~outbox,
      ~closure_maps,
      ~current_top_id=None,
      env,
      d,
    );
  /* Once the run is over, hand the next one the maps of the closures its
   * cache holds. */
  let result =
    switch (closure_maps) {
    | None => result
    | Some(maps) =>
      let.trampoline value = result;
      state :=
        {
          ...state^,
          incr_eval: IncrEval.carry(maps, state^.incr_eval),
        };
      Trampoline.return(value);
    };
  (state, result);
};

let evaluate_and_limit =
    (
      ~calculus: Calculus.t=Calculus.default,
      ~step_limit: int,
      ~prev: EvaluatorState.incr_eval=IncrEval.empty,
      ~eval_info: EvalInfo.t=EvalInfo.empty,
      ~env,
      ~reuse_map: option(IncrEval.reuse_map)=?,
      ~outbox: option(ref(IncrEval.outbox(EvaluatorState.t)))=?,
      d: DHExp.t,
    )
    : limited_result => {
  let (state, result) =
    prepare_evaluation(
      ~calculus,
      ~prev,
      ~eval_info,
      ~env,
      ~reuse_map,
      ~outbox,
      d,
    );
  switch (
    Trampoline.Yielding.run_slice(
      ~step_budget=step_limit,
      result |> Trampoline.Yielding.start,
    )
  ) {
  | SliceDone(x) => LimitedCompleted((finish(~env, x), state^))
  | SliceYielded(_) => StepLimitExceeded
  };
};

type yielding_evaluation = {
  env: Environment.t(Exp.t),
  state: ref(EvaluatorState.t),
  outbox: ref(IncrEval.outbox(EvaluatorState.t)),
  continuation: Trampoline.Yielding.continuation(DHExp.t),
};

type yielding_result =
  | EvaluationCompleted((Exp.t, EvaluatorState.t))
  | EvaluationYielded(yielding_evaluation);

let start_yielding_evaluation =
    (
      ~calculus: Calculus.t=Calculus.default,
      ~prev: EvaluatorState.incr_eval=IncrEval.empty,
      ~eval_info: EvalInfo.t=EvalInfo.empty,
      ~env,
      ~reuse_map: option(IncrEval.reuse_map)=?,
      d: DHExp.t,
    )
    : yielding_evaluation => {
  let outbox = ref(IncrEval.empty_outbox);
  let (state, result) =
    prepare_evaluation(
      ~calculus,
      ~prev,
      ~eval_info,
      ~env,
      ~reuse_map,
      ~outbox=Some(outbox),
      d,
    );
  {
    env,
    state,
    outbox,
    continuation: Trampoline.Yielding.start(result),
  };
};

let drain_streaming_outbox =
    (evaluation: yielding_evaluation): IncrEval.outbox(EvaluatorState.t) => {
  let outbox = evaluation.outbox^;
  evaluation.outbox := IncrEval.empty_outbox;
  outbox;
};

let run_yielding_slice =
    (~step_budget: int, evaluation: yielding_evaluation): yielding_result =>
  switch (
    Trampoline.Yielding.run_slice(~step_budget, evaluation.continuation)
  ) {
  | SliceDone(x) =>
    EvaluationCompleted((finish(~env=evaluation.env, x), evaluation.state^))
  | SliceYielded(continuation) =>
    EvaluationYielded({
      ...evaluation,
      continuation,
    })
  };

let yielding_step_count = (evaluation: yielding_evaluation): int => {
  let Trampoline.Yielding.Continuation(_, _, step_counter) =
    evaluation.continuation;
  step_counter;
};

let evaluate =
    (
      ~calculus: Calculus.t=Calculus.default,
      ~prev: EvaluatorState.incr_eval=IncrEval.empty,
      ~eval_info: EvalInfo.t=EvalInfo.empty,
      ~env,
      d: DHExp.t,
    )
    : (Exp.t, EvaluatorState.t) => {
  let (state, result) =
    prepare_evaluation(
      ~calculus,
      ~prev,
      ~eval_info,
      ~env,
      ~reuse_map=None,
      ~outbox=None,
      d,
    );
  /* Must be sequenced before reading `state`: running the trampoline is what
     populates it, and tuple components are evaluated right-to-left. */
  let value = finish(~env, Trampoline.run(result));
  (value, state^);
};
