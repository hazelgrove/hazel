open Util;
open OptUtil.Syntax;
open Language;

module FocusEffect = {
  /* Scheduled focus for probe or editor elements after step-into.
   * This ref is set when step-into resolves and cleared when focus is executed.
   * We use a ref (not model state) because DOM focus must happen AFTER render,
   * and we can't dispatch actions from after_display without causing loops. */
  type target =
    | Editor
    | Probe(Id.t);

  let scheduled: ref(option(target)) = ref(None);

  /* Schedule DOM focus on a probe element (called from resolve_pending_focus) */
  let schedule = (probe_id: Id.t): unit => {
    scheduled := Some(Probe(probe_id));
  };

  /* Schedule DOM focus on the main editor (called from step_into_sample) */
  let schedule_editor = (): unit => {
    scheduled := Some(Editor);
  };

  /* Execute any scheduled focus (called from Main.re after_display).
   * Returns whether focus was executed. */
  let execute = (): bool =>
    switch (scheduled^) {
    | Some(Editor) =>
      scheduled := None;
      JsUtil.focus_clipboard_shim();
      true;
    | Some(Probe(probe_id)) =>
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
  | Some(InfoExp({term: {term: Let(_pat, def, _), _} as let_term, _})) =>
    /* If trying to probe a let, probe the definition instead.
       Exception: if the let is the body of a test, probe the let itself
       (so we see the test result, not just the definition value).
       Recurse so that if def is a fun literal, the above case will get it */
    let is_test_body =
      switch (
        Statics.Map.parent_term_of(info_map, IdTagged.rep_id(let_term))
      ) {
      | Some(Exp({term: Test(_) | HintedTest(_, _), _})) => true
      | _ => false
      };
    is_test_body
      ? [IdTagged.rep_id(let_term)]
      : target_subterm_ids(IdTagged.rep_id(def), info_map);

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
  /* Filter out terms that can't meaningfully be probed */
  | info when !Info.is_typable_term(info) => []
  /* Default: use rep_id for expressions and patterns to handle multi-tile forms
     (tuples, list literals, case expressions) where non-representative tile IDs
     would otherwise cause probe_map/evaluator ID mismatch */
  | Some(InfoExp({term, _})) => [IdTagged.rep_id(term)]
  | Some(InfoPat({term, _})) => [Pat.rep_id(term)]
  | _ => [id]
  };

type probe_status =
  | Manual(list(Id.t)) /* manual probe; ids are target IDs (for fun literals: pat and body) */
  | Statics(list(Id.t)) /* statics annotation; ids are target IDs */
  | Auto
  | Non;

let probe_status =
    (id: Id.t, info_map: Statics.Map.t, refractors: Zipper.Refractor.t)
    : probe_status => {
  let target_ids = target_subterm_ids(id, info_map);
  /* For manual/statics: check if ALL target IDs have manual entries */
  if (List.for_all(id => Id.Map.mem(id, refractors.manuals), target_ids)
      && target_ids != []) {
    /* Distinguish between probe and statics by checking kind */
    let all_statics =
      List.for_all(
        id =>
          switch (Id.Map.find_opt(id, refractors.manuals)) {
          | Some(entry: Refractors.entry) => entry.kind == Statics
          | None => false
          },
        target_ids,
      );
    all_statics ? Statics(target_ids) : Manual(target_ids);
  } else if
    /* For Auto: check if ANY target ID is an auto probe anchor */
    (List.exists(id => Id.Map.mem(id, refractors.autos.ids), target_ids)) {
    Auto;
  } else {
    Non;
  };
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

/* Sort IDs by lexical position (earliest first).
 * Uses the start position of each term to determine order. */
let sort_ids_lexically =
    (~syntax: CachedSyntax.t, ids: list(Id.t)): list(Id.t) => {
  let with_positions =
    List.filter_map(
      id =>
        switch (
          TermData.extreme_measures(id, syntax.term_data, syntax.measured)
        ) {
        | Some((start_pt, _)) => Some((id, start_pt.row, start_pt.col))
        | None => None
        },
      ids,
    );
  let sorted =
    List.sort(
      ((_, r1, c1), (_, r2, c2)) =>
        switch (Int.compare(r1, r2)) {
        | 0 => Int.compare(c1, c2)
        | n => n
        },
      with_positions,
    );
  List.map(((id, _, _)) => id, sorted);
};

let maybe_rm_pin = (ids: list(Id.t)): (Zipper.t => Zipper.t) =>
  SampleCursorPerform.update_pinned_call(_, p =>
    switch (p) {
    | Some([hd, ..._] as call_stack) =>
      List.mem(hd, ids) ? None : Some(call_stack)
    | x => x
    }
  );

/* Check if there are no probes (manual or auto) remaining */
let has_no_probes = (z: Zipper.t): bool =>
  Id.Map.is_empty(z.refractors.manuals)
  && Id.Map.is_empty(z.refractors.autos.ids);

/* Reset the sample cursor if no probes remain.
 * This prevents stale dynamic cursor state from showing in the sidebar
 * when all probes have been removed. */
let maybe_reset_cursor = (z: Zipper.t): Zipper.t =>
  has_no_probes(z) ? SampleCursorPerform.reset(z) : z;

let rm_auto =
    (
      ~drill: bool=true,
      ~syntax: CachedSyntax.t,
      ~info_map: Statics.Map.t,
      id: Id.t,
      z: Zipper.t,
    )
    : Zipper.t => {
  /* Remove all target IDs from autos, like rm_manual does for manuals.
     When drill=false, remove just the ID directly (must match how it was added). */
  let target_ids = drill ? target_subterm_ids(id, info_map) : [id];
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
     )
  /* Reset sample cursor if no probes remain */
  |> maybe_reset_cursor;
};

let rm_manual = (ids: list(Id.t), z: Zipper.t): Zipper.t =>
  Zipper.update_manuals(
    map => Id.Map.filter((id, _) => !List.mem(id, ids), map),
    z,
  )
  /* If the probe has a pin we'll need to remove that too */
  |> maybe_rm_pin(ids)
  /* Reset sample cursor if no probes remain */
  |> maybe_reset_cursor;

/* Remove colliding manual probes when two end up on the same line.
 * This is called after code edits to clean up probes that were pushed
 * onto the same line due to text reflow. Keeps the rightmost probe. */
let remove_colliding_probes = (~syntax: CachedSyntax.t, z: Zipper.t): Zipper.t => {
  /* 1. Build a map: end_row -> list of (probe_id, col) */
  let row_to_probes =
    Id.Map.fold(
      (probe_id, _, acc) =>
        switch (
          TermData.extreme_measures(
            probe_id,
            syntax.term_data,
            syntax.measured,
          )
        ) {
        | Some((_, end_pt)) =>
          let existing =
            IntMap.find_opt(end_pt.row, acc) |> Option.value(~default=[]);
          IntMap.add(
            end_pt.row,
            [(probe_id, end_pt.col), ...existing],
            acc,
          );
        | None => acc
        },
      z.refractors.manuals,
      IntMap.empty,
    );

  /* 2. For rows with multiple probes, keep rightmost, remove others */
  let ids_to_remove =
    IntMap.fold(
      (_, probes, acc) =>
        switch (probes) {
        | []
        | [_] => acc /* No collision */
        | _ =>
          /* Keep rightmost probe (highest col), remove others */
          let sorted =
            List.sort(((_, a), (_, b)) => compare(b, a), probes);
          let to_remove = List.tl(sorted) |> List.map(fst);
          to_remove @ acc;
        },
      row_to_probes,
      [],
    );

  /* 3. Remove colliding probes */
  rm_manual(ids_to_remove, z);
};

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
  let z =
    List.fold_left(
      (z, id) => Zipper.add_manual(id, Probe, z),
      z,
      target_ids,
    );

  /* Set pending_probe_cursor so sample cursor updates when eval returns */
  let sorted_ids = sort_ids_lexically(~syntax, target_ids);
  Zipper.update_refractors(z, r =>
    {
      ...r,
      pending_probe_cursor: Some(sorted_ids),
    }
  );
};

let toggle_manual =
    (~syntax: CachedSyntax.t, id: Id.t, ~info_map: Statics.Map.t, z: Zipper.t)
    : Zipper.t =>
  switch (probe_status(id, info_map, z.refractors)) {
  | Auto =>
    rm_auto(~syntax, ~info_map, id, z) |> add_manual(~syntax, id, info_map)
  | Statics(ids) =>
    /* Switch from statics to manual probe */
    rm_manual(ids, z) |> add_manual(~syntax, id, info_map)
  | Manual(ids) =>
    /* Remove manual probe */
    rm_manual(ids, z)
  | Non => add_manual(~syntax, id, info_map, z)
  };

let add_ids_from_auto_term =
    (~syntax: CachedSyntax.t, ~info_map: Statics.Map.t, z: Zipper.t): Zipper.t => {
  let auto_ids = Id.Map.bindings(z.refractors.autos.ids) |> List.map(fst);
  let ids = List.concat_map(ids_from_term(~syntax, ~info_map), auto_ids);
  Zipper.update_ephemerals(
    _ =>
      List.fold_left(
        (map, id) => Id.Map.add(id, Refractors.mk_entry(Probe), map),
        Id.Map.empty,
        ids,
      ),
    z,
  );
};

/* Whether to update sample cursor when auto-def mode moves probes.
 * Set to false to disable cursor following for auto-def mode. */
let auto_def_updates_cursor = true;

let add_auto =
    (
      id: Id.t,
      ~drill: bool=true,
      ~set_pending_cursor: bool=true,
      ~syntax: CachedSyntax.t,
      ~info_map: Statics.Map.t,
      z: Zipper.t,
    )
    : Zipper.t => {
  /* Add all target IDs to autos, like add_manual does for manuals.
     When drill=false, probe the ID directly without drilling into subterms
     (used for auto-def mode to stay on top-level definition). */
  let target_ids = drill ? target_subterm_ids(id, info_map) : [id];
  let z =
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

  /* Set pending_probe_cursor so sample cursor updates when eval returns */
  if (set_pending_cursor) {
    /* Use the same target_ids that go into autos.ids, so the ephemeral IDs
       match what add_ids_from_auto_term computes for sample lookup. */
    let ephemeral_ids =
      List.concat_map(ids_from_term(~syntax, ~info_map), target_ids);
    let sorted_ids = sort_ids_lexically(~syntax, ephemeral_ids);
    Zipper.update_refractors(z, r =>
      {
        ...r,
        pending_probe_cursor: Some(sorted_ids),
      }
    );
  } else {
    z;
  };
};

let toggle_auto =
    (~syntax: CachedSyntax.t, id: Id.t, info_map: Statics.Map.t, z: Zipper.t)
    : Zipper.t =>
  switch (probe_status(id, info_map, z.refractors)) {
  | Auto => rm_auto(~syntax, ~info_map, id, z)
  | Manual(ids)
  | Statics(ids) => rm_manual(ids, z) |> add_auto(id, ~syntax, ~info_map)
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
    | Auto
    | Manual(_)
    | Statics(_) => z
    | Non => add_auto(body_id, ~syntax, ~info_map, z)
    };

  /* Set pin and dyn cursor using the sample's call_stack */
  let new_stack = [ap_id, ...sample.call_stack];

  /* Determine where to jump and where to look for samples.
   * For function literals:
   * - jump_target = pattern (cursor goes to parameters for UX)
   * - sample_probe_id = inner body (where samples are stored in dynamics)
   * target_subterm_ids transforms Fun to [inner_body, pattern]. */
  let (jump_target, _sample_probe_id) =
    switch (ci_body) {
    | InfoExp({term: {term: Fun(pat, inner_body, _, _), _}, _}) =>
      let pat_id = IdTagged.rep_id(pat);
      let inner_body_id = IdTagged.rep_id(inner_body);
      (pat_id, inner_body_id);
    | _ => (body_id, body_id)
    };

  // NOTE(andrew): disabling this for now as it doesn't work right
  /* Set pending_focus using sample_probe_id (inner body), since that's where
   * the samples are stored in the dynamics map. */
  // let pending_focus: Sample.Cursor.pending_focus = {
  //   probe_id: sample_probe_id,
  //   target_stack: new_stack,
  // };

  let z =
    SampleCursorPerform.update(z, _ => {
      {
        ...z.refractors.sample_cursor,
        call_stack: new_stack,
        index: List.length(sample.call_stack),
        pinned_stack: Some(new_stack),
        pending_focus: None //Some(pending_focus),
      }
    });

  /* Schedule focus back to the main editor after render */
  FocusEffect.schedule_editor();

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

/* Check if type annotation is allowed for the given id. */
let can_statics = (id: Id.t, info_map: Statics.Map.t): bool =>
  Info.is_typable_term(Statics.Map.lookup(id, info_map));

/* Toggle type annotation on the indicated term.
   Unlike probes, type annotations don't support auto mode or pins. */
let toggle_statics =
    (~syntax: CachedSyntax.t, id: Id.t, info_map: Statics.Map.t, z: Zipper.t)
    : Zipper.t =>
  if (!can_statics(id, info_map)) {
    z;
  } else {
    let target_ids = target_subterm_ids(id, info_map);
    let add_statics = z =>
      List.fold_left(
        (z, id) => Zipper.add_manual(id, Statics, z),
        z,
        target_ids,
      );
    switch (probe_status(id, info_map, z.refractors)) {
    | Statics(ids) =>
      /* Remove statics */
      rm_manual(ids, z)
    | Manual(ids) =>
      /* Switch from manual probe to statics */
      rm_manual(ids, z) |> add_statics
    | Auto =>
      /* Switch from auto probe to statics */
      rm_auto(~syntax, ~info_map, id, z) |> add_statics
    | Non =>
      /* Add statics */
      add_statics(z)
    };
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
  | ToggleStatics =>
    switch (Indicated.index(z)) {
    | Some(id) => toggle_statics(~syntax, id, info_map, z)
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

/* Get the kind of refractor at the given id, if any */
let refractor_kind = (id: Id.t, z: Zipper.t): option(ProjectorCore.Kind.t) => {
  switch (Id.Map.find_opt(id, z.refractors.manuals)) {
  | Some(entry: Refractors.entry) => Some(entry.kind)
  | None =>
    switch (Id.Map.find_opt(id, z.refractors.autos.ephemerals)) {
    | Some(entry: Refractors.entry) => Some(entry.kind)
    | None => None
    }
  };
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

/* Resolve pending_probe_cursor by finding the first probe ID with samples
 * and setting the sample cursor to the closest matching sample. */
let resolve_pending_probe_cursor =
    (~dynamics: Dynamics.Map.t, z: Zipper.t): Zipper.t =>
  switch (z.refractors.pending_probe_cursor) {
  | None => z
  | Some(target_ids) =>
    /* Find first ID that has samples */
    let first_with_samples =
      List.find_map(
        id =>
          switch (Dynamics.Map.lookup(id, dynamics)) {
          | Some([_, ..._] as s) => Some((id, s))
          | Some([]) => None
          | None => None
          },
        target_ids,
      );
    switch (first_with_samples) {
    | Some((_id, samples)) =>
      /* Use closest_to_cursor to pick the best sample based on current cursor,
       * rather than just the first sample. This preserves user's selection
       * when adding new probes. */
      let selected =
        Sample.Selection.closest_to_cursor(
          ~ap_id=None,
          ~cursor=z.refractors.sample_cursor,
          samples,
        );
      switch (selected) {
      | Some(sample) =>
        /* Use capture to set sample cursor (preserves deeper stack if applicable) */
        let z = SampleCursorPerform.capture(z, sample, None);
        /* Clear pending_probe_cursor */
        Zipper.update_refractors(z, r =>
          {
            ...r,
            pending_probe_cursor: None,
          }
        );
      | None =>
        /* Clear pending anyway since we had samples */
        Zipper.update_refractors(z, r =>
          {
            ...r,
            pending_probe_cursor: None,
          }
        )
      };
    | None =>
      /* No samples yet - keep pending for next eval cycle */
      z
    };
  };

/* Post-calculation probe effects: cleanup, auto-probe regeneration,
 * step-into focus resolution, pending probe cursor resolution, and cursor reset.
 * Called from Editor.calculate after syntax and statics are updated. */
let editor_effects =
    (
      ~syntax: CachedSyntax.t,
      ~info_map: Statics.Map.t,
      ~dynamics: Dynamics.Map.t,
      z: Zipper.t,
    )
    : Zipper.t =>
  z
  |> remove_colliding_probes(~syntax)
  |> add_ids_from_auto_term(~syntax, ~info_map)
  |> resolve_pending_focus(~dynamics)
  |> resolve_pending_probe_cursor(~dynamics)
  |> maybe_reset_cursor;

/* AUTO-DEF MODE: automatically place an auto-probe on the top-level
 * definition body that the cursor is currently inside. When the cursor
 * moves to a different definition, the probe follows. */

/* Determines what expression to auto-probe based on cursor position.
 *
 * Walk ancestors from outermost to innermost. For each:
 * - Test: return test body (done)
 * - Let: check if child-toward-cursor is the body
 *   - If child == body: skip (cursor is in body, this let doesn't apply)
 *   - Otherwise: return this let's def (cursor is in def/pattern/on-delimiter)
 *
 * The key insight: the ONLY way to not probe a let's def is if the cursor
 * is in its body. Being on the let delimiter, pattern, or def all qualify.
 */
let toplevel_def_body_id = (~statics: Statics.Map.t, ~id: Id.t): option(Id.t) => {
  open Language;

  /* Walk from outermost to innermost ancestor.
   * At each step, `child_id` is the next item toward the cursor.
   * ancestors is innermost-first, so we walk from the end. */
  let find_target = (starting_id: Id.t, ancestors: list(Id.t)): option(Id.t) => {
    let len = List.length(ancestors);

    let rec walk = (idx: int): option(Id.t) =>
      if (idx < 0) {
        None;
      } else {
        let anc_id = List.nth(ancestors, idx);
        /* Child is the next ancestor toward cursor, or starting_id if innermost */
        let child_id =
          if (idx == 0) {
            starting_id;
          } else {
            List.nth(ancestors, idx - 1);
          };

        switch (Statics.Map.lookup(anc_id, statics)) {
        | Some(
            InfoExp({
              term: {term: Test(body) | HintedTest(body, _), _},
              _,
            }),
          ) =>
          /* Test: return its body */
          Some(IdTagged.rep_id(body))

        | Some(InfoExp({term: {term: Let(_, def, body), _}, _})) =>
          let body_id = IdTagged.rep_id(body);
          if (Id.equal(child_id, body_id)) {
            /* Child is body → cursor is in body → skip, continue inward */
            walk(
              idx - 1,
            );
          } else {
            /* Child is def/pattern/or this is the cursor → return def */
            Some(
              IdTagged.rep_id(def),
            );
          };

        | _ =>
          /* Not a let or test, continue inward */
          walk(idx - 1)
        };
      };

    walk(len - 1);
  };

  switch (Statics.Map.lookup(id, statics)) {
  | Some(InfoExp({term: {term: Test(body) | HintedTest(body, _), _}, _})) =>
    /* Starting point IS a test → return its body */
    Some(IdTagged.rep_id(body))

  | Some(info) =>
    let ancestors = Info.ancestors_of(info);
    switch (find_target(id, ancestors)) {
    | Some(def_id) => Some(def_id)
    | None =>
      /* No outer let found where we're in def.
         Check if starting_id itself is a top-level let → return its def */
      switch (info) {
      | InfoExp({term: {term: Let(_, def, _), _}, _}) =>
        Some(IdTagged.rep_id(def))
      | _ => None
      }
    };

  | None => None
  };
};

/* Remove the auto_def probe if present */
let clear_auto_def =
    (~syntax: CachedSyntax.t, ~info_map: Statics.Map.t, z: Zipper.t): Zipper.t =>
  switch (z.refractors.auto_def) {
  | None => z
  | Some(old_id) =>
    rm_auto(~drill=false, ~syntax, ~info_map, old_id, z)
    |> Zipper.update_refractors(_, r =>
         {
           ...r,
           auto_def: None,
         }
       )
  };

/* Get the top-level definition body ID that the cursor is currently inside.
 * When the cursor is on whitespace/comments (secondaries), we fall back to
 * using the nearest ancestor tile's ID, since secondaries don't have statics. */
let current_toplevel_def =
    (info_map: Statics.Map.t, z: Zipper.t): option(Id.t) => {
  let try_id = id => toplevel_def_body_id(~statics=info_map, ~id);

  /* First try the indicated piece */
  let from_indicated =
    switch (Indicated.index(z)) {
    | None => None
    | Some(cursor_id) => try_id(cursor_id)
    };

  /* If that failed (e.g., cursor on whitespace), try the zipper ancestor */
  switch (from_indicated) {
  | Some(_) => from_indicated
  | None =>
    switch (z.relatives.ancestors) {
    | [] => None
    | [(ancestor, _), ..._] => try_id(ancestor.id)
    }
  };
};

/* Update the auto_def probe based on current cursor position.
 * Only reconstitutes the probe when the cursor moves to a different
 * top-level definition. */
let update_auto_def_probe =
    (~syntax: CachedSyntax.t, ~info_map: Statics.Map.t, z: Zipper.t): Zipper.t => {
  let current_def = current_toplevel_def(info_map, z);
  let prev_def = z.refractors.auto_def;

  /* If same definition, no change needed */
  if (Option.equal(Id.equal, current_def, prev_def)) {
    z;
  } else {
    /* Remove old auto-probe if exists.
       Use ~drill=false to match how it was added. */
    let z =
      switch (prev_def) {
      | Some(old_id) => rm_auto(~drill=false, ~syntax, ~info_map, old_id, z)
      | None => z
      };

    /* Add new auto-probe if inside a definition.
       Use ~drill=false to stay on top-level def, not drill into nested lets.
       Use auto_def_updates_cursor to control whether cursor follows. */
    let z =
      switch (current_def) {
      | Some(new_id) =>
        add_auto(
          new_id,
          ~drill=false,
          ~set_pending_cursor=auto_def_updates_cursor,
          ~syntax,
          ~info_map,
          z,
        )
      | None => z
      };

    /* Update auto_def tracking */
    Zipper.update_refractors(z, r =>
      {
        ...r,
        auto_def: current_def,
      }
    );
  };
};
