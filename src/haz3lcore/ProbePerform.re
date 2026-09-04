open Util_web;
open OptUtil.Syntax;
open Language;

module FocusEffect = {
  /* Scheduled focus for probe or editor elements after step-into.
   * This ref is set when step-into resolves and cleared when focus is executed.
   * We use a ref (not model state) because DOM focus must happen AFTER render,
   * and we can't dispatch actions from after_display without causing loops. */
  type target =
    | Editor
    | Cell
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

  /* Schedule DOM focus on the active code-editor cell (called after a
     sidebar jump, which moves the model selection to a different cell
     without moving DOM focus). */
  let schedule_cell = (): unit => {
    scheduled := Some(Cell);
  };

  /* Execute any scheduled focus (called from Main.re after_display).
   * Returns whether focus was executed. */
  let execute = (): bool =>
    switch (scheduled^) {
    | Some(Editor) =>
      scheduled := None;
      JsUtil.focus_clipboard_shim();
      true;
    | Some(Cell) =>
      scheduled := None;
      JsUtil.focus_active_cell();
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
  | Some(InfoExp({user_term: {term: Fun(pat, body, _, _), _}, _})) => [
      IdTagged.rep_id(body),
      IdTagged.rep_id(pat),
    ]
  | Some(InfoExp({user_term: {term: Let(_pat, def, _), _} as let_term, _})) =>
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
  | Some(InfoExp({user_term: {term: ModuleExp(_, def, _), _}, _})) =>
    /* If trying to probe a module expression, probe the definition.
       Recurse so fun literals get drilled into. */
    target_subterm_ids(IdTagged.rep_id(def), info_map)

  | Some(InfoExp({user_term: {term: Var(_), _} as v, _})) =>
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
  | Some(InfoExp({user_term: {term: DeferredAp(_), _} as v, _})) =>
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
  | Some(InfoExp({user_term, _})) => [IdTagged.rep_id(user_term)]
  | Some(InfoPat({user_term, _})) => [Pat.rep_id(user_term)]
  | _ => [id]
  };

type probe_status =
  | Manual(list(Id.t)) /* manual probe; ids are target IDs (for fun literals: pat and body) */
  | Statics(list(Id.t)) /* statics annotation; ids are target IDs */
  | Multi
  | Ephemeral(list(Id.t)) /* target IDs present in ephemerals map */
  | Suppressed(list(Id.t)) /* target IDs present in suppressed map */
  | Non;

let probe_status =
    (id: Id.t, info_map: Statics.Map.t, refractors: Zipper.Refractor.t)
    : probe_status => {
  let target_ids = target_subterm_ids(id, info_map);
  /* For manual/statics: check if ALL target IDs have manual entries */
  if (List.for_all(
        id => List.assoc_opt(id, refractors.manuals) != None,
        target_ids,
      )
      && target_ids != []) {
    /* Distinguish between probe and statics by checking kind */
    let all_statics =
      List.for_all(
        id =>
          switch (List.assoc_opt(id, refractors.manuals)) {
          | Some(entry: Refractors.entry) => entry.kind == Statics
          | None => false
          },
        target_ids,
      );
    all_statics ? Statics(target_ids) : Manual(target_ids);
  } else if
    /* For Multi: check if ANY target ID is a multi probe anchor */
    (List.exists(id => Id.Map.mem(id, refractors.multis.ids), target_ids)) {
    Multi;
  } else {
    let ephemeral_ids =
      List.filter(
        id => Id.Map.mem(id, refractors.multis.ephemerals),
        target_ids,
      );
    if (ephemeral_ids != []) {
      Ephemeral(ephemeral_ids);
    } else {
      let suppressed_ids =
        List.filter(
          id => Id.Map.mem(id, refractors.multis.suppressed),
          target_ids,
        );
      if (suppressed_ids != []) {
        Suppressed(suppressed_ids);
      } else {
        Non;
      };
    };
  };
};

let ids_from_term =
    (~syntax: CachedSyntax.t, ~info_map, id: Id.t): list(Id.t) =>
  MultiProbe.ids_to_multiprobe(
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

/* Set pending_probe_cursor so sample focus aligns when dynamics arrive. */
let set_pending_probe = (ids: list(Id.t), z: Zipper.t): Zipper.t => {
  Zipper.update_refractors(z, r =>
    {
      ...r,
      pending_probe_cursor: Some(ids),
    }
  );
};

/* ─────────────────────────────────────────────────────────────────────
 * Dynamic focus: auto vs manual mode
 *
 * The "dynamic focus" is the selected sample whose call stack determines
 * which samples light up across all probes. It changes via two kinds
 * of paths:
 *
 *   USER-DRIVEN (always honored):
 *     - click a sample                (ProbeProj)
 *     - toggle pin                    (ProbeProj)
 *     - step into                     (ProbeProj)
 *     - breadcrumb bar (← →, click)   (SampleFocusBar)
 *     - reset                         (SampleFocusBar)
 *
 *   AUTOMATIC (only honored in auto mode — gated on `auto_focus`):
 *     - capture on new ephemeral probe    (add_ids_from_multi_term)
 *     - spatial alignment after edit      (align_to_indicated_probe)
 *     - stale-cursor fallback             (resolve_pending_probe_cursor;
 *         after an edit invalidates the focus's call_stack frame IDs,
 *         try to find a replacement sample at the "most aligned" position.
 *         Internal recovery mechanism, invisible to the user when working.)
 *
 * Pinning a sample switches the focus into MANUAL mode: the user has
 * said "I care about this execution context, don't move me." In manual
 * mode, automatic realignment is suppressed; the focus only changes in
 * response to explicit user actions.
 *
 * Note on auto-probe cursor following: when `update_autoprobe` moves
 * auto-probe targets to track the cursor across top-level definitions,
 * its set_pending_cursor flag is also gated on auto_focus. So pinning a
 * sample in `f` and navigating to `g` will NOT jump focus to g's newly
 * added probes — consistent with the "stay here" semantics of manual mode.
 *
 * If you add a new path that can change the dynamic focus automatically,
 * gate it on `auto_focus(z)` and add it to the list above.
 * ───────────────────────────────────────────────────────────────────── */
let auto_focus = (z: Zipper.t): bool =>
  z.refractors.sample_focus.pinned_stack == None;

/* Check if id has either manual or ephemeral probe on it */
let has_probe = (id: Id.t, z: Zipper.t): bool =>
  List.assoc_opt(id, z.refractors.manuals) != None
  || Id.Map.mem(id, z.refractors.multis.ephemerals);

let maybe_rm_pin = (ids: list(Id.t)): (Zipper.t => Zipper.t) =>
  z =>
    SampleFocusPerform.update_pinned_call(z, p =>
      switch (p) {
      | Some([{id: hd_id, _}, ..._] as call_stack) =>
        List.mem(hd_id, ids) && !has_probe(hd_id, z)
          ? None : Some(call_stack)
      | x => x
      }
    );

/* Check if there are no probes (manual or auto) remaining */
let has_no_probes = (z: Zipper.t): bool =>
  List.is_empty(z.refractors.manuals)
  && Id.Map.is_empty(z.refractors.multis.ids);

/* Reset the sample focus if no probes remain.
 * This prevents stale sample focus state from showing in the sidebar
 * when all probes have been removed. */
let maybe_reset_cursor = (z: Zipper.t): Zipper.t =>
  has_no_probes(z) ? SampleFocusPerform.reset(z) : z;

let rm_multi =
    (
      ~drill: bool=true,
      ~reset: bool=true,
      ~syntax: CachedSyntax.t,
      ~info_map: Statics.Map.t,
      id: Id.t,
      z: Zipper.t,
    )
    : Zipper.t => {
  /* Remove all target IDs from multis, like rm_manual does for manuals.
     When drill=false, remove just the ID directly (must match how it was added). */
  let target_ids = drill ? target_subterm_ids(id, info_map) : [id];
  let z =
    Zipper.update_refractors(z, refractors =>
      {
        ...refractors,
        multis: {
          ids:
            Id.Map.filter(
              (id, _) => !List.mem(id, target_ids),
              z.refractors.multis.ids,
            ),
          suppressed:
            Id.Map.filter(
              (id, _) => !List.mem(id, target_ids),
              z.refractors.multis.suppressed,
            ),
          ephemerals:
            Id.Map.filter(
              (id', _) => !List.mem(id', target_ids),
              z.refractors.multis.ephemerals,
            ),
        },
      }
    )
    /* We need to check if any of the probed ids are pinned; if so
       we'll need to remove that pin when we remove the auto */
    |> maybe_rm_pin(
         List.concat_map(ids_from_term(~syntax, ~info_map), target_ids),
       );
  /* Reset sample focus if no probes remain (skipped when reset=false,
     e.g. during clear_autoprobe to avoid style flash) */
  reset ? maybe_reset_cursor(z) : z;
};

let rm_manual = (ids: list(Id.t), z: Zipper.t): Zipper.t =>
  Zipper.update_manuals(
    map => List.filter(((id, _)) => !List.mem(id, ids), map),
    z,
  )
  /* If the probe has a pin we'll need to remove that too */
  |> maybe_rm_pin(ids)
  /* Reset sample focus if no probes remain */
  |> maybe_reset_cursor;

/* Remove colliding manual probes when two end up on the same line.
 * This is called after code edits to clean up probes that were pushed
 * onto the same line due to text reflow. Keeps the rightmost probe. */
let remove_colliding_probes = (~syntax: CachedSyntax.t, z: Zipper.t): Zipper.t => {
  /* 1. Build a map: end_row -> list of (probe_id, col) */
  let row_to_probes =
    List.fold_right(
      ((probe_id, _), acc) =>
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
    List.fold_right(
      ((probe_id, _), acc) =>
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

  /* Set pending_probe_cursor so sample focus updates when eval returns */
  let sorted_ids = sort_ids_lexically(~syntax, target_ids);
  set_pending_probe(sorted_ids, z);
};

let toggle_manual =
    (~syntax: CachedSyntax.t, id: Id.t, ~info_map: Statics.Map.t, z: Zipper.t)
    : Zipper.t =>
  switch (probe_status(id, info_map, z.refractors)) {
  | Multi =>
    rm_multi(~syntax, ~info_map, id, z) |> add_manual(~syntax, id, info_map)
  | Statics(ids) =>
    /* Switch from statics to manual probe */
    rm_manual(ids, z) |> add_manual(~syntax, id, info_map)
  | Manual(ids) =>
    /* Remove manual probe */
    rm_manual(ids, z)
  | Ephemeral(_)
  | Suppressed(_)
  | Non => add_manual(~syntax, id, info_map, z)
  };

let add_suppression = (ids: list(Id.t), z: Zipper.t): Zipper.t =>
  Zipper.update_suppressed(
    suppressed =>
      List.fold_left((map, id) => Id.Map.add(id, (), map), suppressed, ids),
    z,
  );

let rm_suppression = (ids: list(Id.t), z: Zipper.t): Zipper.t =>
  Zipper.update_suppressed(
    suppressed => Id.Map.filter((id, _) => !List.mem(id, ids), suppressed),
    z,
  );

let add_ids_from_multi_term =
    (~syntax: CachedSyntax.t, ~info_map: Statics.Map.t, z: Zipper.t): Zipper.t => {
  let auto_ids = Id.Map.bindings(z.refractors.multis.ids) |> List.map(fst);
  let all_ids = List.concat_map(ids_from_term(~syntax, ~info_map), auto_ids);
  /* Clean up suppressed: only keep IDs that are still in the would-be set */
  let z =
    Zipper.update_suppressed(
      suppressed =>
        Id.Map.filter((id, _) => List.mem(id, all_ids), suppressed),
      z,
    );
  /* Filter out IDs that have manual probes or are suppressed */
  let manual_ids = List.map(fst, z.refractors.manuals);
  let ids =
    List.filter(
      id =>
        !List.mem(id, manual_ids)
        && !Id.Map.mem(id, z.refractors.multis.suppressed),
      all_ids,
    );
  /* Filter out ephemerals that would render on the same line as a manual */
  let manual_end_rows =
    List.filter_map(
      ((id, _)) =>
        switch (
          TermData.extreme_measures(id, syntax.term_data, syntax.measured)
        ) {
        | Some((_, end_loc)) => Some(end_loc.row)
        | None => None
        },
      z.refractors.manuals,
    );
  let ids =
    List.filter(
      id =>
        switch (
          TermData.extreme_measures(id, syntax.term_data, syntax.measured)
        ) {
        | Some((_, end_loc)) => !List.mem(end_loc.row, manual_end_rows)
        | None => true
        },
      ids,
    );
  let old_ephemerals = z.refractors.multis.ephemerals;
  let new_ephemeral_map =
    List.fold_left(
      (map, id) => Id.Map.add(id, Refractors.mk_entry(Probe), map),
      Id.Map.empty,
      ids,
    );
  let z = Zipper.update_ephemerals(_ => new_ephemeral_map, z);
  /* If there are genuinely new ephemeral IDs, set pending_probe_cursor
     so the sample focus aligns when evaluation results arrive.
     Gated on auto_focus: in manual focus mode, don't auto-capture. */
  let new_ids = List.filter(id => !Id.Map.mem(id, old_ephemerals), ids);
  switch (new_ids) {
  | [] => z
  | _ when !auto_focus(z) => z
  | _ =>
    let sorted = sort_ids_lexically(~syntax, new_ids);
    set_pending_probe(sorted, z);
  };
};

/* Whether to update sample focus when auto probe moves probes.
 * Set to false to disable cursor following for auto probe. */
let autoprobe_updates_cursor = true;

let add_multi =
    (
      id: Id.t,
      ~drill: bool=true,
      ~set_pending_cursor: bool=true,
      ~syntax: CachedSyntax.t,
      ~info_map: Statics.Map.t,
      z: Zipper.t,
    )
    : Zipper.t => {
  /* Add all target IDs to multis, like add_manual does for manuals.
     When drill=false, probe the ID directly without drilling into subterms
     (used for auto probe to stay on top-level definition). */
  let target_ids = drill ? target_subterm_ids(id, info_map) : [id];
  let z =
    Zipper.update_refractors(z, refractors =>
      {
        ...refractors,
        multis: {
          ...refractors.multis,
          ids:
            List.fold_left(
              (map, id) => Id.Map.add(id, (), map),
              z.refractors.multis.ids,
              target_ids,
            ),
        },
      }
    )
    |> add_ids_from_multi_term(~syntax, ~info_map);

  /* Set pending_probe_cursor so sample focus updates when eval returns */
  if (set_pending_cursor) {
    /* Use the same target_ids that go into multis.ids, so the ephemeral IDs
       match what add_ids_from_multi_term computes for sample lookup. */
    let ephemeral_ids =
      List.concat_map(ids_from_term(~syntax, ~info_map), target_ids);
    let sorted_ids = sort_ids_lexically(~syntax, ephemeral_ids);
    set_pending_probe(sorted_ids, z);
  } else {
    z;
  };
};

let toggle_multi =
    (~syntax: CachedSyntax.t, id: Id.t, info_map: Statics.Map.t, z: Zipper.t)
    : Zipper.t =>
  switch (probe_status(id, info_map, z.refractors)) {
  | Multi => rm_multi(~syntax, ~info_map, id, z)
  | Manual(ids)
  | Statics(ids) => rm_manual(ids, z) |> add_multi(id, ~syntax, ~info_map)
  | Ephemeral(_)
  | Suppressed(_)
  | Non =>
    /* Use same gating as manual probes: if target_subterm_ids returns [],
       the term is not probeable. */
    switch (target_subterm_ids(id, info_map)) {
    | [] => z /* Can't probe this (type, type pattern, label, etc.) */
    | _ => add_multi(id, ~syntax, ~info_map, z)
    }
  };

/* Check if the indicated term is a definition form (Let or Test/HintedTest).
   When true, the unified probe action adds a multi probe instead of manual.
   This is because definition bodies benefit from multi probe's per-line
   expansion, while specific expressions are better served by manual probes. */
let is_definition_form = (id: Id.t, info_map: Statics.Map.t): bool =>
  switch (Statics.Map.lookup(id, info_map)) {
  | Some(InfoExp({user_term: {term: Let(_, _, _), _}, _})) => true
  | Some(InfoExp({user_term: {term: Test(_) | HintedTest(_, _), _}, _})) =>
    true
  | _ => false
  };

/* Unified probe toggle: on definition forms (Let, Test), adds/removes multi
   probes; on other terms, adds/removes manual probes. This merges the
   previously separate ToggleManual/ToggleMulti actions into a single
   context-sensitive action behind one keyboard shortcut (Cmd+E). */
let toggle_probe =
    (~syntax: CachedSyntax.t, id: Id.t, ~info_map: Statics.Map.t, z: Zipper.t)
    : Zipper.t =>
  if (is_definition_form(id, info_map)) {
    /* Definition form: use multi probe */
    switch (probe_status(id, info_map, z.refractors)) {
    | Multi => rm_multi(~syntax, ~info_map, id, z)
    | Manual(ids) => rm_manual(ids, z)
    | Statics(ids) => rm_manual(ids, z) |> add_multi(id, ~syntax, ~info_map)
    | Ephemeral(ids) => add_suppression(ids, z)
    | Suppressed(ids) => rm_suppression(ids, z)
    | Non =>
      switch (target_subterm_ids(id, info_map)) {
      | [] => z
      | _ => add_multi(id, ~syntax, ~info_map, z)
      }
    };
  } else {
    /* Non-definition: use manual probe */
    switch (probe_status(id, info_map, z.refractors)) {
    | Manual(ids) => rm_manual(ids, z) |> add_suppression(ids)
    | Multi => rm_multi(~syntax, ~info_map, id, z)
    | Statics(ids) => rm_manual(ids, z) |> add_manual(~syntax, id, info_map)
    | Ephemeral(ids) => add_suppression(ids, z)
    | Suppressed(ids) => rm_suppression(ids, z)
    | Non => add_manual(~syntax, id, info_map, z)
    };
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
 *    a. SampleFocusPerform.resolve_pending_focus updates sample_focus, clears pending_focus
 *    b. FocusEffect.schedule(probe_id) schedules DOM focus
 * 7. Main.re's after_display hook calls FocusEffect.execute()
 * 8. execute() calls elem##focus, triggering CSS :focus styles on the probe
 *
 * KEY FILES:
 * - ProbeProj.re: UI, step_into_sample action dispatch
 * - ProbePerform.re: step_into_sample, resolve_pending_focus, FocusEffect
 * - SampleFocusPerform.re: cursor update operations (sample matching)
 * - CellEditor.re: Two-pass calculation for timing
 * - Sample.re: pending_focus type in Cursor.t
 * - Main.re: after_display calls FocusEffect.execute()
 */

/* Step into from a specific sample, using the sample's call_stack
   instead of the current sample_focus's effective_stack. This ensures
   we maintain the exact execution context of the selected sample. */
let step_into_call_stack =
    (
      ~syntax: CachedSyntax.t,
      ~call_stack: CallStack.t,
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
        user_term: {term: Ap(_, {term: Var(_), _} as fun_expr, _), _},
        _,
      }) =>
      let* ci_var = Statics.Map.lookup(IdTagged.rep_id(fun_expr), info_map);
      Info.get_binding_site(ci_var);
    | _ => None
    };
  let* body_id =
    Statics.Map.enclosing_let_of_binding(~statics=info_map, ~binding_id);
  let* ci_body = Statics.Map.lookup(body_id, info_map);

  /* Ensure a manual probe on the source expression (ap_id) before jumping.
     If there's only a multi probe, promote it to manual so it persists. */
  let z =
    switch (probe_status(ap_id, info_map, z.refractors)) {
    | Manual(_)
    | Statics(_) => z
    | Multi
    | Ephemeral(_)
    | Suppressed(_)
    | Non => Zipper.add_manual(ap_id, Probe, z)
    };

  /* Add multi probe on function body if not already probed */
  let z =
    switch (probe_status(body_id, info_map, z.refractors)) {
    | Multi
    | Manual(_)
    | Statics(_)
    | Ephemeral(_) => z
    | Suppressed(_)
    | Non => add_multi(body_id, ~syntax, ~info_map, z)
    };

  /* Set pin and dyn cursor using the call_stack */
  let new_stack = CallStack.extend(ap_id, call_stack);

  /* Determine where to jump and where to look for samples.
   * For function literals:
   * - jump_target = pattern (cursor goes to parameters for UX)
   * - sample_probe_id = inner body (where samples are stored in dynamics)
   * target_subterm_ids transforms Fun to [inner_body, pattern]. */
  let (jump_target, _sample_probe_id) =
    switch (ci_body) {
    | InfoExp({user_term: {term: Fun(pat, inner_body, _, _), _}, _}) =>
      let pat_id = IdTagged.rep_id(pat);
      let inner_body_id = IdTagged.rep_id(inner_body);
      (pat_id, inner_body_id);
    | _ => (body_id, body_id)
    };

  // NOTE(andrew): disabling this for now as it doesn't work right
  /* Set pending_focus using sample_probe_id (inner body), since that's where
   * the samples are stored in the dynamics map. */
  // let pending_focus: Sample.Focus.pending_focus = {
  //   probe_id: sample_probe_id,
  //   target_stack: new_stack,
  // };

  let z =
    SampleFocusPerform.update(z, _ => {
      {
        ...z.refractors.sample_focus,
        call_stack: new_stack,
        index: List.length(call_stack),
        pinned_stack: Some(new_stack),
        pending_focus: None //Some(pending_focus),
      }
    });

  /* Schedule focus back to the main editor after render */
  FocusEffect.schedule_editor();

  Move.jump_to_id_indicated(z, jump_target);
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
    | Multi =>
      /* Switch from multi probe to statics */
      rm_multi(~syntax, ~info_map, id, z) |> add_statics
    | Ephemeral(_)
    | Suppressed(_)
    | Non =>
      /* Add statics */
      add_statics(z)
    };
  };

/** Ensure statics overlays are on for this binding; idempotent if already statics. */
let place_statics_at =
    (~syntax: CachedSyntax.t, id: Id.t, info_map: Statics.Map.t, z: Zipper.t)
    : Zipper.t =>
  if (!can_statics(id, info_map)) {
    z;
  } else {
    let target_ids = target_subterm_ids(id, info_map);
    let add_statics = z =>
      List.fold_left(
        (z, tid) => Zipper.add_manual(tid, Statics, z),
        z,
        target_ids,
      );
    switch (probe_status(id, info_map, z.refractors)) {
    | Statics(_) => z
    | Manual(ids) => rm_manual(ids, z) |> add_statics
    | Multi => rm_multi(~syntax, ~info_map, id, z) |> add_statics
    | Ephemeral(_)
    | Suppressed(_)
    | Non => add_statics(z)
    };
  };

/** Remove only statics manual entries for targets of this path; leaves probes intact. */
let remove_statics_at =
    (id: Id.t, info_map: Statics.Map.t, z: Zipper.t): Zipper.t => {
  let target_ids = target_subterm_ids(id, info_map);
  Zipper.update_manuals(
    manuals =>
      List.filter(
        ((mid, entry: Refractors.entry)) =>
          !(List.mem(mid, target_ids) && entry.kind == Statics),
        manuals,
      ),
    z,
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
      | Some(id) => toggle_probe(~syntax, id, ~info_map, z)
      }
    | _ =>
      switch (
        TermData.get_root_id_using_ranges(
          z.selection.content,
          syntax.term_data,
          syntax.measured,
        )
      ) {
      | Some(id) =>
        let z = Zipper.unselect(z);
        toggle_probe(~syntax, id, ~info_map, z);
      | None => z
      }
    }
  | ToggleAuto =>
    switch (Indicated.index(z)) {
    | Some(id) => toggle_multi(~syntax, id, info_map, z)
    | None => z
    }
  | ToggleStatics =>
    switch (Indicated.index(z)) {
    | Some(id) => toggle_statics(~syntax, id, info_map, z)
    | None => z
    }
  | StepInto(call_stack, ap_id) =>
    switch (step_into_call_stack(~syntax, ~call_stack, ~ap_id, info_map, z)) {
    | Some(z) => z
    | None => z
    }
  | Pin(call_stack, ap_id) =>
    /* Promote multi probe to manual so it persists across cursor movement,
       then toggle the pin on this call stack */
    let z =
      switch (probe_status(ap_id, info_map, z.refractors)) {
      | Manual(_)
      | Statics(_) => z
      | Multi
      | Ephemeral(_)
      | Suppressed(_)
      | Non => Zipper.add_manual(ap_id, Probe, z)
      };
    SampleFocusPerform.toggle_pin_call(z, call_stack);
  | RemoveAll =>
    z
    |> Zipper.update_manuals(_ => [])
    |> Zipper.update_refractors(_, r =>
         {
           ...r,
           multis: {
             ...r.multis,
             ids: Id.Map.empty,
             suppressed: Id.Map.empty,
           },
         }
       )
    |> SampleFocusPerform.reset
  };

/* Note: has_probe is defined earlier (above maybe_rm_pin) */

/* Get the kind of refractor at the given id, if any */
let refractor_kind = (id: Id.t, z: Zipper.t): option(ProjectorCore.Kind.t) => {
  switch (List.assoc_opt(id, z.refractors.manuals)) {
  | Some(entry: Refractors.entry) => Some(entry.kind)
  | None =>
    switch (Id.Map.find_opt(id, z.refractors.multis.ephemerals)) {
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
  switch (z.refractors.sample_focus.pending_focus) {
  | None => z
  | Some({probe_id, target_stack}) =>
    switch (Dynamics.Map.lookup(probe_id, dynamics)) {
    | None => z
    | Some(samples) =>
      let z' =
        SampleFocusPerform.resolve_pending_focus(z, samples, target_stack);
      /* If pending_focus was cleared, schedule DOM focus on the probe */
      if (z'.refractors.sample_focus.pending_focus == None) {
        FocusEffect.schedule(probe_id);
      };
      z';
    }
  };

/* Check whether the cursor is aligned with any probe's samples.
 * Returns true if the cursor has an empty call_stack (never captured)
 * or if at least one probe has a sample matching the cursor via
 * most_aligned_index. */
let cursor_is_aligned = (~dynamics: Dynamics.Map.t, z: Zipper.t): bool => {
  let cursor = z.refractors.sample_focus;
  if (cursor.call_stack == []) {
    true; /* Empty cursor is trivially aligned */
  } else {
    let all_probe_ids =
      List.map(fst, Id.Map.bindings(z.refractors.multis.ephemerals))
      @ List.map(fst, z.refractors.manuals);
    List.exists(
      id =>
        switch (Dynamics.Map.lookup(id, dynamics)) {
        | Some([_, ..._] as samples) =>
          Sample.Selection.most_aligned_index(~ap_id=None, cursor, samples)
          != None
        | _ => false
        },
      all_probe_ids,
    );
  };
};

/* Find the caret-nearest ephemeral probe ID. Uses the same strategies
 * as align_to_indicated_probe: direct match via Indicated.index,
 * then spatial proximity on the same row. */
let caret_nearest_ephemeral =
    (~syntax: CachedSyntax.t, z: Zipper.t): option(Id.t) => {
  switch (Indicated.index(z)) {
  | Some(piece_id) when Id.Map.mem(piece_id, z.refractors.multis.ephemerals) =>
    Some(piece_id)
  | _ =>
    let caret_pt = Zipper.Caret.point(syntax.measured, z);
    Id.Map.bindings(z.refractors.multis.ephemerals)
    |> List.find_map(((id, _)) =>
         switch (
           TermData.extreme_measures(id, syntax.term_data, syntax.measured)
         ) {
         | Some((start_pt, end_pt))
             when
               start_pt.row == caret_pt.row
               && caret_pt.col >= start_pt.col
               && caret_pt.col <= end_pt.col
               + 1 =>
           Some(id)
         | _ => None
         }
       );
  };
};

/* Ensure the sample focus is aligned with current dynamics.
 *
 * Handles two cases uniformly:
 * 1. pending_probe_cursor is set (probe set changed via add_ids_from_multi_term
 *    or align_to_indicated_probe): resolve by finding the first pending ID
 *    with samples and capturing from it.
 * 2. pending is None but cursor is stale (structural edit changed application
 *    site tile IDs, so the cursor's call_stack frame IDs no longer match any
 *    sample in the new dynamics): detect via cursor_is_aligned, then build
 *    a target list from all probes.
 *
 * In both cases, the caret-nearest probe is prioritized to avoid capturing
 * from a probe in a different case branch or distant expression.
 *
 * When a pin is active, skip the stale-cursor fallback (case 2) so that
 * the pinned context is preserved. Explicit pending cursors (case 1)
 * still resolve, since those represent intentional navigation. */
let resolve_pending_probe_cursor =
    (
      ~dynamics: Dynamics.Map.t,
      ~syntax: CachedSyntax.t,
      ~info_map: Statics.Map.t,
      z: Zipper.t,
    )
    : Zipper.t => {
  /* Determine which IDs to try */
  let (target_ids, is_pending) =
    switch (z.refractors.pending_probe_cursor) {
    | Some(ids) => (Some(ids), true)
    | None =>
      if (cursor_is_aligned(~dynamics, z) || !auto_focus(z)) {
        (None, false);
      } else {
        /* Cursor is stale — treat all probes as candidates */
        let all_ids =
          List.map(fst, Id.Map.bindings(z.refractors.multis.ephemerals))
          @ List.map(fst, z.refractors.manuals);
        switch (all_ids) {
        | [] => (None, false)
        | _ => (Some(all_ids), false)
        };
      }
    };

  switch (target_ids) {
  | None => z
  | Some(ids) =>
    /* Prioritize caret-nearest probe */
    let ids =
      switch (caret_nearest_ephemeral(~syntax, z)) {
      | Some(nearest) when List.mem(nearest, ids) => [
          nearest,
          ...List.filter(i => i != nearest, ids),
        ]
      | Some(nearest) => [nearest, ...ids]
      | None => ids
      };

    /* Find first ID that has samples */
    let first_with_samples =
      List.find_map(
        id =>
          switch (Dynamics.Map.lookup(id, dynamics)) {
          | Some([_, ..._] as s) => Some((id, s))
          | Some([]) => None
          | None => None
          },
        ids,
      );
    switch (first_with_samples) {
    | Some((probe_id, samples)) =>
      /* Compute ap_id from the probe's statics so indicated_call
         is set correctly for both click and keyboard navigation */
      let ap_id =
        switch (Statics.Map.lookup(probe_id, info_map)) {
        | Some(statics) => Sample.Focus.cur_var_ap(statics)
        | None => None
        };
      let selected =
        Sample.Selection.most_aligned_sample(
          ~ap_id,
          ~cursor=z.refractors.sample_focus,
          samples,
        );
      switch (selected) {
      | Some(sample) =>
        let z =
          SampleFocusPerform.capture(
            z,
            Sample.capture_of_sample(sample),
            ap_id,
          );
        Zipper.update_refractors(z, r =>
          {
            ...r,
            pending_probe_cursor: None,
          }
        );
      | None =>
        Zipper.update_refractors(z, r =>
          {
            ...r,
            pending_probe_cursor: None,
          }
        )
      };
    | None =>
      /* No samples yet — keep pending if it was pending, otherwise noop */
      if (is_pending) {z} else {z}
    };
  };
};

/* After an edit, if the new-ID diff in add_ids_from_multi_term didn't set
 * pending_probe_cursor (e.g., because grout ID preservation kept the same ID
 * despite structural changes), align the sample focus to an ephemeral probe
 * at or near the caret. This handles cases like completing `then` where the
 * hole moves from top-level sibling to then-branch without changing ID.
 *
 * Note: Indicated.index returns a piece ID, but ephemerals are keyed by term
 * IDs from MultiProbe. These usually match for simple cases (grout holes) but
 * may differ when the caret is on a delimiter or shard of a multi-piece term.
 * We try a direct match first, then fall back to spatial proximity. */
let align_to_indicated_probe =
    (~is_edited: bool, ~syntax: CachedSyntax.t, z: Zipper.t): Zipper.t =>
  if (!is_edited
      || z.refractors.pending_probe_cursor != None
      || !auto_focus(z)) {
    z;
  } else {
    /* Strategy 1: Direct match — indicated piece is an ephemeral probe */
    let direct_match =
      switch (Indicated.index(z)) {
      | None => None
      | Some(piece_id) =>
        if (Id.Map.mem(piece_id, z.refractors.multis.ephemerals)) {
          Some(piece_id);
        } else {
          None;
        }
      };
    /* Strategy 2: Spatial proximity — find ephemeral probe on same row
     * whose measured range contains the caret position */
    let spatial_match = () => {
      let caret_pt = Zipper.Caret.point(syntax.measured, z);
      let ephemerals = Id.Map.bindings(z.refractors.multis.ephemerals);
      List.find_map(
        ((id, _)) =>
          switch (
            TermData.extreme_measures(id, syntax.term_data, syntax.measured)
          ) {
          | Some((start_pt, end_pt))
              when
                start_pt.row == caret_pt.row
                && caret_pt.col >= start_pt.col
                && caret_pt.col <= end_pt.col
                + 1 =>
            Some(id)
          | _ => None
          },
        ephemerals,
      );
    };
    switch (direct_match) {
    | Some(id) => set_pending_probe([id], z)
    | None =>
      switch (spatial_match()) {
      | Some(id) => set_pending_probe([id], z)
      | None => z
      }
    };
  };

/* Post-calculation probe effects: cleanup, multi-probe regeneration,
 * step-into focus resolution, pending probe cursor resolution, and cursor reset.
 * Called from Editor.calculate after syntax and statics are updated. */
let editor_effects =
    (
      ~is_edited: bool,
      ~syntax: CachedSyntax.t,
      ~info_map: Statics.Map.t,
      ~dynamics: Dynamics.Map.t,
      z: Zipper.t,
    )
    : Zipper.t =>
  z
  |> remove_colliding_probes(~syntax)
  |> add_ids_from_multi_term(~syntax, ~info_map)
  |> align_to_indicated_probe(~is_edited, ~syntax)
  |> resolve_pending_focus(~dynamics)
  |> resolve_pending_probe_cursor(~dynamics, ~syntax, ~info_map)
  |> maybe_reset_cursor;

/* AUTO PROBE: automatically place a multi probe on the top-level
 * definition body that the cursor is currently inside. When the cursor
 * moves to a different definition, the probe follows. */

/* Determines what expression to probe based on cursor position (for auto probe).
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
              user_term: {term: Test(body) | HintedTest(body, _), _},
              _,
            }),
          ) =>
          /* Test: return its body */
          Some(IdTagged.rep_id(body))

        | Some(InfoExp({user_term: {term: Let(_, def, body), _}, _})) =>
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
  | Some(
      InfoExp({user_term: {term: Test(body) | HintedTest(body, _), _}, _}),
    ) =>
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
      | InfoExp({user_term: {term: Let(_, def, _), _}, _}) =>
        Some(IdTagged.rep_id(def))
      | _ => None
      }
    };

  | None => None
  };
};

/* Remove the auto probe's multi probe if present */
let clear_autoprobe =
    (~syntax: CachedSyntax.t, ~info_map: Statics.Map.t, z: Zipper.t): Zipper.t =>
  switch (z.refractors.autoprobe_target) {
  | None => z
  | Some(old_id) =>
    /* Skip cursor reset here: the syntax cache still has the old probes
     * (since this isn't an edit, CachedSyntax won't recalculate until
     * the next is_edited cycle). If we reset the cursor now, the stale
     * probes render one last frame with a reset cursor, causing a brief
     * color flash before they disappear. By preserving the cursor, the
     * departing probes render with their original colors. The cursor
     * will be reset on the next editor_effects call when the probes
     * are actually gone from the syntax cache. */
    rm_multi(~drill=false, ~reset=false, ~syntax, ~info_map, old_id, z)
    |> Zipper.update_refractors(_, r =>
         {
           ...r,
           autoprobe_target: None,
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

/* Update the auto probe based on current cursor position.
 * Only reconstitutes the probe when the cursor moves to a different
 * top-level definition. */
let update_autoprobe =
    (~syntax: CachedSyntax.t, ~info_map: Statics.Map.t, z: Zipper.t): Zipper.t => {
  let current_def = current_toplevel_def(info_map, z);
  let prev_def = z.refractors.autoprobe_target;
  /* If same definition, no change needed */
  if (Option.equal(Id.equal, current_def, prev_def)) {
    z;
  } else {
    /* Remove old multi probe if exists.
       Use ~drill=false to match how it was added. */
    let z =
      switch (prev_def) {
      | Some(old_id) => rm_multi(~drill=false, ~syntax, ~info_map, old_id, z)
      | None => z
      };

    /* Regenerate ephemerals from multis.ids after removal.
       rm_multi(~drill=false) only removes the auto ID itself, not the
       expanded ephemeral IDs created by add_ids_from_multi_term. When
       transitioning to None (no definition), add_multi won't run, so
       stale ephemerals would persist for one frame without this. */
    let z = add_ids_from_multi_term(~syntax, ~info_map, z);

    /* Add new multi probe if inside a definition.
       Use ~drill=false to stay on top-level def, not drill into nested lets.
       Use autoprobe_updates_cursor to control whether cursor follows.
       Gated on auto_focus: in manual focus mode, don't jump focus when
       the cursor crosses into a new top-level definition. */
    let z =
      switch (current_def) {
      | Some(new_id) =>
        add_multi(
          new_id,
          ~drill=false,
          ~set_pending_cursor=autoprobe_updates_cursor && auto_focus(z),
          ~syntax,
          ~info_map,
          z,
        )
      | None => z
      };

    /* Update auto probe tracking */
    Zipper.update_refractors(z, r =>
      {
        ...r,
        autoprobe_target: current_def,
      }
    );
  };
};
