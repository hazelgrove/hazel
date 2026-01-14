open Util;
open OptUtil.Syntax;
open Language;

module FocusEffect = {
  /* Scheduled focus for probe elements after step-into.
   * This ref is set when step-into resolves and cleared when focus is executed.
   * We use a ref (not model state) because DOM focus must happen AFTER render,
   * and we can't dispatch actions from after_display without causing loops. */
  let scheduled: ref(option(Id.t)) = ref(None);

  /* Schedule DOM focus on a probe element (called from resolve_pending_focus) */
  let schedule = (probe_id: Id.t): unit => {
    scheduled := Some(probe_id);
  };

  /* Execute any scheduled focus (called from Main.re after_display).
   * Returns whether focus was executed. */
  let execute = (): bool =>
    switch (scheduled^) {
    | Some(probe_id) =>
      scheduled := None;
      let elem_id = Id.cls(probe_id);
      switch (JsUtil.get_elem_by_id_opt(elem_id)) {
      | Some(elem) =>
        elem##focus;
        true;
      | None => false
      };
    | None => false
    };
};

let rec target_subterm_ids = (id: Id.t, info_map: Statics.Map.t) =>
  switch (Statics.Map.lookup(id, info_map)) {
  /* If we're trying to probe a function literal,
     put probes on parameters and body instead */
  | Some(InfoExp({term: {term: Fun(pat, body, _, _), _}, _})) => [
      IdTagged.rep_id(body),
      IdTagged.rep_id(pat),
    ]
  | Some(InfoExp({term: {term: Let(_pat, def, _), _}, _})) =>
    /* If trying to probe a let, probe the definition instead.
       Recurse so that if def is a fun literal, the above case will get it */
    target_subterm_ids(IdTagged.rep_id(def), info_map)

  | Some(InfoExp({term: {term: Var(_), _} as v, _})) =>
    /* If we're trying to probe variable in function position for an
       application, probe the whole application instead */
    switch (Statics.Map.parent_term_of(info_map, IdTagged.rep_id(v))) {
    | Some(Exp({term: Ap(_, f_expr, _), _} as ap)) when f_expr == v => [
        IdTagged.rep_id(ap),
      ]
    | Some(Exp({term: DeferredAp(f_expr, _), _} as dap)) when f_expr == v =>
      /* If we're trying to probe a variable in function position in a partially
         applied function, itself in function position of an application,
         in particular but not limited to a reverse application chain,
         probe the whole application instead */
      switch (Statics.Map.parent_term_of(info_map, IdTagged.rep_id(dap))) {
      | Some(Exp({term: Ap(_, f_expr, _), _} as ap)) when f_expr == dap => [
          IdTagged.rep_id(ap),
        ]
      | _ => [id]
      }
    | _ => [id]
    }
  | Some(InfoExp({term: {term: DeferredAp(_), _} as v, _})) =>
    /* If we're trying to probe a partially applied function in function
       position of an application, in particular but not limited to a reverse
       application chain, probe the whole application instead */
    switch (Statics.Map.parent_term_of(info_map, IdTagged.rep_id(v))) {
    | Some(Exp({term: Ap(_, f_expr, _), _} as ap)) when f_expr == v => [
        IdTagged.rep_id(ap),
      ]
    | _ => [id]
    }
  /* Disallow probing deferrals and labels for now as it's not useful
     and it also breaks parsing */
  | Some(InfoExp({term: {term: Deferral(_) | Label(_), _}, _})) => []
  | Some(InfoExp({term: {term: TyAlias(_), _}, _})) => []
  /* Disallow probing types pending alexander's stuff */
  | Some(InfoTyp(_) | InfoTPat(_)) => []
  /* Default: use rep_id for expressions and patterns to handle multi-tile forms
     (tuples, list literals, case expressions) where non-representative tile IDs
     would otherwise cause probe_map/evaluator ID mismatch */
  | Some(InfoExp({term, _})) => [IdTagged.rep_id(term)]
  | Some(InfoPat({term, _})) => [Pat.rep_id(term)]
  | _ => [id]
  };

type probe_status =
  | Manual(list(Id.t)) /* If a function literal, ids are pat and body ids */
  | REPL
  | Non;

let probe_status =
    (id: Id.t, info_map: Statics.Map.t, refractors: Zipper.Refractor.t)
    : probe_status => {
  let target_ids = target_subterm_ids(id, info_map);
  /* For manual: check if ALL target IDs have manual probes */
  List.for_all(id => Id.Map.mem(id, refractors.manuals), target_ids)
    ? Manual(target_ids)
    /* For REPL: check if ANY target ID is an auto probe anchor */
    : List.exists(id => Id.Map.mem(id, refractors.autos.ids), target_ids)
        ? REPL : Non;
};

let ids_from_term =
    (~syntax: CachedSyntax.t, ~info_map, id: Id.t): list(Id.t) =>
  AutoProbe.ids_to_autoprobe(
    id,
    syntax.term_data,
    syntax.terms,
    syntax.measured,
    info_map,
  )
  |> Option.to_list
  |> List.flatten
  |> List.filter_map(Fun.id);

let maybe_rm_pin = (ids: list(Id.t)): (Zipper.t => Zipper.t) =>
  SampleCursorPerform.update_pinned_call(_, p =>
    switch (p) {
    | Some([hd, ..._] as call_stack) =>
      List.mem(hd, ids) ? None : Some(call_stack)
    | x => x
    }
  );

let rm_auto =
    (~syntax: CachedSyntax.t, ~info_map: Statics.Map.t, id: Id.t, z: Zipper.t)
    : Zipper.t => {
  /* Remove all target IDs from autos, like rm_manual does for manuals */
  let target_ids = target_subterm_ids(id, info_map);
  Zipper.update_refractors(z, refractors =>
    {
      ...refractors,
      autos: {
        ids:
          Id.Map.filter(
            (id, _) => !List.mem(id, target_ids),
            z.refractors.autos.ids,
          ),
        ephemerals:
          Id.Map.filter(
            (id', _) => !List.mem(id', target_ids),
            z.refractors.autos.ephemerals,
          ),
      },
    }
  )
  /* We need to check if any of the probed ids are pinned; if so
     we'll need to remove that pin when we remove the auto */
  |> maybe_rm_pin(
       List.concat_map(ids_from_term(~syntax, ~info_map), target_ids),
     );
};

let rm_manual = (ids: list(Id.t), z: Zipper.t): Zipper.t =>
  Zipper.update_manuals(
    map => Id.Map.filter((id, _) => !List.mem(id, ids), map),
    z,
  )
  /* If the probe has a pin we'll need to remove that too */
  |> maybe_rm_pin(ids);

let add_manual =
    (~syntax: CachedSyntax.t, id: Id.t, info_map: Statics.Map.t, z: Zipper.t)
    : Zipper.t => {
  let target_ids = target_subterm_ids(id, info_map);

  /* Get ending rows for all new probe targets */
  let target_end_rows =
    target_ids
    |> List.filter_map(id =>
         TermData.extreme_measures(id, syntax.term_data, syntax.measured)
         |> Option.map(((_, end_pt: Point.t)) => end_pt.row)
       );

  /* Find existing manual probes ending on those rows */
  let conflicting_ids =
    Id.Map.fold(
      (probe_id, _, acc) =>
        switch (
          TermData.extreme_measures(
            probe_id,
            syntax.term_data,
            syntax.measured,
          )
        ) {
        | Some((_, end_pt)) when List.mem(end_pt.row, target_end_rows) => [
            probe_id,
            ...acc,
          ]
        | _ => acc
        },
      z.refractors.manuals,
      [],
    );

  /* Remove conflicts, then add new probes */
  let z = rm_manual(conflicting_ids, z);
  List.fold_left((z, id) => MkRefractor.add_single(id, z), z, target_ids);
};

let toggle_manual =
    (~syntax: CachedSyntax.t, id: Id.t, ~info_map: Statics.Map.t, z: Zipper.t)
    : Zipper.t =>
  switch (probe_status(id, info_map, z.refractors)) {
  | REPL =>
    rm_auto(~syntax, ~info_map, id, z) |> add_manual(~syntax, id, info_map)
  | Manual(ids) => rm_manual(ids, z)
  | Non => add_manual(~syntax, id, info_map, z)
  };

let add_ids_from_auto_term =
    (~syntax: CachedSyntax.t, ~info_map: Statics.Map.t, z: Zipper.t): Zipper.t => {
  let auto_ids = Id.Map.bindings(z.refractors.autos.ids) |> List.map(fst);
  let ids = List.concat_map(ids_from_term(~syntax, ~info_map), auto_ids);
  Zipper.update_ephemerals(
    _ =>
      List.fold_left(
        (map, id) => Id.Map.add(id, MkRefractor.mk_entry(Probe), map),
        Id.Map.empty,
        ids,
      ),
    z,
  );
};

let add_auto =
    (id: Id.t, ~syntax: CachedSyntax.t, ~info_map: Statics.Map.t, z: Zipper.t)
    : Zipper.t => {
  /* Add all target IDs to autos, like add_manual does for manuals */
  let target_ids = target_subterm_ids(id, info_map);
  Zipper.update_refractors(z, refractors =>
    {
      ...refractors,
      autos: {
        ...refractors.autos,
        ids:
          List.fold_left(
            (map, id) => Id.Map.add(id, (), map),
            z.refractors.autos.ids,
            target_ids,
          ),
      },
    }
  )
  |> add_ids_from_auto_term(~syntax, ~info_map);
};

let toggle_auto =
    (~syntax: CachedSyntax.t, id: Id.t, info_map: Statics.Map.t, z: Zipper.t)
    : Zipper.t =>
  switch (probe_status(id, info_map, z.refractors)) {
  | REPL => rm_auto(~syntax, ~info_map, id, z)
  | Manual(ids) => rm_manual(ids, z) |> add_auto(id, ~syntax, ~info_map)
  | Non =>
    /* Use same gating as manual probes: if target_subterm_ids returns [],
       the term is not probeable. */
    switch (target_subterm_ids(id, info_map)) {
    | [] => z /* Can't probe this (type, type pattern, label, etc.) */
    | _ => add_auto(id, ~syntax, ~info_map, z)
    }
  };

let is_jump_target = (info_map: Statics.Map.t, z: Zipper.t): option(Id.t) => {
  let* ci = Indicated.ci_of(z, info_map);
  let* ci =
    switch (ci) {
    | InfoExp({
        term: {term: Ap(_, {term: Var(_), _} as fun_expr, _), _},
        _,
      }) =>
      Statics.Map.lookup(IdTagged.rep_id(fun_expr), info_map)
    | _ => Some(ci)
    };
  Info.get_binding_site(ci);
};

/* STEP-INTO: Sample-Level Navigation Through Execution Traces
 *
 * Step-into operates at the SAMPLE level, not the syntax level. When f(x) is
 * called 5 times during evaluation, stepping into from a specific sample takes
 * you to the function body while maintaining your position in that particular
 * execution trace - you see the body's evaluation for THAT invocation, not all
 * invocations blended together.
 *
 * This is why step-into lives in the sample context menu (environment dropdown)
 * rather than the syntax context menu - being in that dropdown means you've
 * already selected a specific sample, so step-into uses that sample's exact
 * call_stack to maintain execution context.
 *
 * WHY THIS IS COMPLEX:
 *
 * 1. CALL STACK SEMANTICS: When stepping into ap_id from a sample with
 *    call_stack=[a,b,c], the new stack is [ap_id,a,b,c]. This matches what
 *    samples inside the function body will have (the evaluator adds ap_id
 *    when RecordStackFrame is processed).
 *
 * 2. TIMING: Even when samples are available (probe_all on), the projector
 *    DOM element won't exist until after a view cycle. Both probe_all on/off
 *    cases need deferred focus - the difference is just whether we're also
 *    waiting for the worker to return samples.
 *
 * 3. TWO-PASS CALCULATION: In CellEditor.calculate, Editor.calculate runs
 *    BEFORE EvalResult.calculate. The second pass (when pending_focus is set)
 *    ensures resolve_pending_focus sees fresh dynamics after worker results.
 *
 * 4. SAMPLE ID VS JUMP TARGET: For function literals, we distinguish between:
 *    - jump_target (pattern ID): where cursor goes for UX
 *    - sample_probe_id (inner body ID): where samples are stored in dynamics
 *    target_subterm_ids(Fun) returns [inner_body, pattern], and samples are
 *    stored under inner_body. pending_focus uses sample_probe_id for lookup.
 *
 * STEP-INTO FLOW:
 * 1. User clicks "Step Into" on a sample in ProbeProj context menu
 * 2. ProbeProj dispatches Probe(StepInto(sample, ap_id))
 * 3. step_into_sample (below) sets pending_focus with probe_id and target_stack
 * 4. If probe_all enabled, an ephemeral probe is added at target, triggering eval
 * 5. CellEditor.calculate runs:
 *    a. First pass: Editor.calculate → resolve_pending_focus (may have stale dynamics)
 *    b. EvalResult.calculate processes worker results, updating dynamics
 *    c. Second pass (if pending_focus still set): resolve_pending_focus with fresh dynamics
 * 6. When resolve_pending_focus finds a matching sample:
 *    a. SampleCursorPerform.resolve_pending_focus updates sample_cursor, clears pending_focus
 *    b. FocusEffect.schedule(probe_id) schedules DOM focus
 * 7. Main.re's after_display hook calls FocusEffect.execute()
 * 8. execute() calls elem##focus, triggering CSS :focus styles on the probe
 *
 * KEY FILES:
 * - ProbeProj.re: UI, step_into_sample action dispatch
 * - ProbePerform.re: step_into_sample, resolve_pending_focus, FocusEffect
 * - SampleCursorPerform.re: cursor update operations (sample matching)
 * - CellEditor.re: Two-pass calculation for timing
 * - Sample.re: pending_focus type in Cursor.t
 * - Main.re: after_display calls FocusEffect.execute()
 */

/* Step into from a specific sample, using the sample's call_stack
   instead of the current sample_cursor's trimmed_stack. This ensures
   we maintain the exact execution context of the selected sample. */
let step_into_sample =
    (
      ~syntax: CachedSyntax.t,
      ~sample: Sample.t,
      ~ap_id: Id.t,
      info_map: Statics.Map.t,
      z: Zipper.t,
    )
    : option(Zipper.t) => {
  /* Look up the function being called from the application */
  let* ci_ap = Statics.Map.lookup(ap_id, info_map);
  let* binding_id =
    switch (ci_ap) {
    | InfoExp({
        term: {term: Ap(_, {term: Var(_), _} as fun_expr, _), _},
        _,
      }) =>
      let* ci_var = Statics.Map.lookup(IdTagged.rep_id(fun_expr), info_map);
      Info.get_binding_site(ci_var);
    | _ => None
    };
  let* body_id =
    Statics.Map.enclosing_let_of_binding(~statics=info_map, ~binding_id);
  let* ci_body = Statics.Map.lookup(body_id, info_map);

  /* Add auto probe on function body if not already probed */
  let z =
    switch (probe_status(body_id, info_map, z.refractors)) {
    | REPL => z
    | Manual(_) => z
    | Non => add_auto(body_id, ~syntax, ~info_map, z)
    };

  /* Set pin and dyn cursor using the sample's call_stack */
  let new_stack = [ap_id, ...sample.call_stack];

  /* Determine where to jump and where to look for samples.
   * For function literals:
   * - jump_target = pattern (cursor goes to parameters for UX)
   * - sample_probe_id = inner body (where samples are stored in dynamics)
   * target_subterm_ids transforms Fun to [inner_body, pattern]. */
  let (jump_target, sample_probe_id) =
    switch (ci_body) {
    | InfoExp({term: {term: Fun(pat, inner_body, _, _), _}, _}) =>
      let pat_id = IdTagged.rep_id(pat);
      let inner_body_id = IdTagged.rep_id(inner_body);
      (pat_id, inner_body_id);
    | _ => (body_id, body_id)
    };

  /* Set pending_focus using sample_probe_id (inner body), since that's where
   * the samples are stored in the dynamics map. */
  let pending_focus: Sample.Cursor.pending_focus = {
    probe_id: sample_probe_id,
    target_stack: new_stack,
  };

  let z =
    SampleCursorPerform.update(z, _ => {
      {
        ...z.refractors.sample_cursor,
        call_stack: new_stack,
        index: List.length(sample.call_stack),
        pinned_stack: Some(new_stack),
        pending_focus: Some(pending_focus),
      }
    });

  Move.jump_to_id_indicated(z, jump_target);
};

let rm_probes_in_selection =
    (~syntax: CachedSyntax.t, ~info_map: Statics.Map.t, z: Zipper.t): Zipper.t => {
  let selection_ids = Selection.selection_ids(z.selection);
  z
  |> rm_manual(selection_ids)
  |> List.fold_left(
       (z, id) => rm_auto(~syntax, ~info_map, id, z),
       _,
       selection_ids,
     );
};

let go =
    (
      ~statics as {info_map, _}: CachedStatics.t,
      ~syntax: CachedSyntax.t,
      a: Action.probe,
      z: Zipper.t,
    )
    : Zipper.t =>
  switch (a) {
  | ToggleManual =>
    switch (z.selection.content) {
    | [] =>
      switch (Indicated.index(z)) {
      | None => z
      | Some(id) => toggle_manual(~syntax, id, ~info_map, z)
      }
    | _ => rm_probes_in_selection(~syntax, ~info_map, z)
    }
  | ToggleAuto =>
    switch (Indicated.index(z)) {
    | Some(id) => toggle_auto(~syntax, id, info_map, z)
    | None => z
    }
  | StepInto(sample, ap_id) =>
    switch (step_into_sample(~syntax, ~sample, ~ap_id, info_map, z)) {
    | Some(z) => z
    | None => z
    }
  };

/* Check if id has either manual or ephermeral probe on it */
let has_probe = (id: Id.t, z: Zipper.t): bool => {
  Id.Map.mem(id, z.refractors.manuals)
  || Id.Map.mem(id, z.refractors.autos.ephemerals);
};

/* Check if probing is allowed for the given id.
   Used by ContextMenu to determine whether to show probe options. */
let can_probe = (id: Id.t, info_map: Statics.Map.t): bool =>
  target_subterm_ids(id, info_map) != [];

/* Resolve pending focus from step-into by looking up samples in dynamics
   and focusing the one that matches target_stack. Called from Editor.calculate
   after dynamics are updated. See FocusEffect module comment for full flow. */
let resolve_pending_focus = (~dynamics: Dynamics.Map.t, z: Zipper.t): Zipper.t =>
  switch (z.refractors.sample_cursor.pending_focus) {
  | None => z
  | Some({probe_id, target_stack}) =>
    switch (Dynamics.Map.lookup(probe_id, dynamics)) {
    | None => z
    | Some(samples) =>
      let z' =
        SampleCursorPerform.resolve_pending_focus(z, samples, target_stack);
      /* If pending_focus was cleared, schedule DOM focus on the probe */
      if (z'.refractors.sample_cursor.pending_focus == None) {
        FocusEffect.schedule(probe_id);
      };
      z';
    }
  };
