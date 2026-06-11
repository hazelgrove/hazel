open Util;
open OptUtil.Syntax;
open Language;

/* FocusEffect lives in its own module (haz3lcore/projectors/
 * FocusEffect.re) so ProbeProj can schedule focus restorations
 * without creating a dep cycle through the projector machinery.
 * Re-exported here for callers that already use ProbePerform.
 * FocusEffect.* (Main.re's after_display hook, etc.). */
module FocusEffect = FocusEffect;

let rec target_subterm_ids = (id: Id.t, info_map: Statics.Map.t) =>
  switch (Statics.Map.lookup(id, info_map)) {
  /* If we're trying to probe a function literal,
     put probes on parameters and body instead */
  | Some(InfoExp({user_term: {term: Fun(pat, body, _, _), _}, _})) => [
      IdTagged.rep_id(body),
      IdTagged.rep_id(pat),
    ]
  | Some(InfoExp({user_term: {term: Let(pat, def, _), _} as let_term, _})) =>
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
    if (is_test_body) {
      [IdTagged.rep_id(let_term)];
    } else {
      let def_targets = target_subterm_ids(IdTagged.rep_id(def), info_map);
      /* Function-definition sugar (`let f(args) = body`) keeps the
         parameters in the surface binder `Ap(Var(f), args)`, which lives
         on the header line(s) outside the def body's row range. Anchor the
         args pattern too so the parameters get probed, mirroring the
         Fun-literal case above which returns [body, pat]. The args pattern
         is multi-probed, so the existing container logic shows the whole
         tuple on one line or each parameter when split across lines. */
      switch (FunctionSugar.detect(pat)) {
      | Some((_f_name, args, _ret_ty)) => def_targets @ [Pat.rep_id(args)]
      | None => def_targets
      };
    };
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
  /* For manual/statics: check if ANY target ID has a manual entry.
     Requiring ALL would make Manual unreachable whenever a sibling
     target was cleaned up after the fact — e.g. remove_colliding_probes
     deletes the pat probe of a single-line `fun` (same end row as the
     body), and the toggle would then re-add forever instead of removing. */
  let manual_entries =
    List.filter_map(
      id => List.assoc_opt(id, refractors.manuals),
      target_ids,
    );
  if (manual_entries != []) {
    /* Distinguish between probe and statics by checking kind */
    let all_statics =
      List.for_all(
        (entry: Refractors.entry) => entry.kind == Statics,
        manual_entries,
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

/* Memoization for the per-row multi-probe expansion.
 *
 * `MultiProbe.ids_to_multiprobe` is an O(program) per-row analysis. It is a
 * pure function of the syntax snapshots (term_data, terms, measured) +
 * info_map + the anchor id. Those snapshots are immutable and rebuilt
 * by-reference on change (CachedSyntax / CachedStatics), reused by reference
 * otherwise — so we key on PHYSICAL identity of the four refs (===, O(1)
 * pointer compares, never a structural walk) plus the small Id.t anchor.
 * When any ref changes we drop the per-anchor result table; within a
 * stable-syntax run (e.g. pure caret moves) every anchor is served from it.
 *
 * Soundness: these structures are never mutated in place, so same-ref
 * implies same-content (no stale results); a spurious ref change only costs
 * a recompute. The cache is a single global entry — multiple editors with
 * distinct syntaxes invalidate each other (correct, just less reuse); the
 * hot path (one main editor in auto-probe All) keeps it warm. */
let expansion_inputs:
  ref(option((TermData.t, TermMap.t, Measured.t, Statics.Map.t))) =
  ref(None);
let expansion_results: ref(list((Id.t, list(Id.t)))) = ref([]);

let ids_from_term =
    (~syntax: CachedSyntax.t, ~info_map, id: Id.t): list(Id.t) => {
  let inputs_stable =
    switch (expansion_inputs^) {
    | Some((term_data, terms, measured, prev_info_map)) =>
      term_data === syntax.term_data
      && terms === syntax.terms
      && measured === syntax.measured
      && prev_info_map === info_map
    | None => false
    };
  if (!inputs_stable) {
    expansion_inputs :=
      Some((syntax.term_data, syntax.terms, syntax.measured, info_map));
    expansion_results := [];
  };
  switch (List.assoc_opt(id, expansion_results^)) {
  | Some(result) => result
  | None =>
    let result =
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
    expansion_results := [(id, result), ...expansion_results^];
    result;
  };
};

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

/* Promote a multi/ephemeral probe to a manual one (Pin, Step Into),
 * carrying over the ephemeral entry's model (drawer mode, active
 * renderer, ...). A fresh default entry would visibly reset the probe
 * once the ephemeral is filtered out on the next rebuild. */
let promote_to_manual = (id: Id.t, z: Zipper.t): Zipper.t => {
  let model =
    Id.Map.find_opt(id, z.refractors.multis.ephemerals)
    |> Option.map((e: Refractors.entry) => e.model);
  Zipper.add_manual(~model?, id, Probe, z);
};

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

let add_manual_targets =
    (~syntax: CachedSyntax.t, target_ids: list(Id.t), z: Zipper.t): Zipper.t => {
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

let add_manual =
    (~syntax: CachedSyntax.t, id: Id.t, info_map: Statics.Map.t, z: Zipper.t)
    : Zipper.t =>
  switch (target_subterm_ids(id, info_map)) {
  | [] =>
    /* Not probeable (type, label, ...): add nothing. In particular don't
       set a pending_probe_cursor that could never resolve — while set it
       suppresses alignment and forces CellEditor's double-calculate pass. */
    z
  | target_ids => add_manual_targets(~syntax, target_ids, z)
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
  /* Preserve existing ephemeral entries (and their persisted models)
   * for ids that survive across rebuilds. A fresh `mk_entry(Probe)`
   * was previously used for every id, which silently wiped per-probe
   * model state on every edit — e.g. `drawer_mode=true` set via the
   * drawer-mode toggle would revert to the default within one cycle. */
  let new_ephemeral_map =
    List.fold_left(
      (map, id) =>
        switch (Id.Map.find_opt(id, old_ephemerals)) {
        | Some(existing) => Id.Map.add(id, existing, map)
        | None => Id.Map.add(id, Refractors.mk_entry(Probe), map)
        },
      Id.Map.empty,
      ids,
    );
  /* Preserve the previous ephemerals ref when the resulting set is identical,
   * rather than installing a fresh map every frame. A new ref would make
   * CachedSyntax.calculate take the refresh_shapes path and rebuild Measured
   * (O(program)) every frame — even on pure caret moves — because it gates on
   * `multis.ephemerals !==`. Keeping the ref stable lets the cheap
   * selection-only path run. (O(probes) compare, only over the probe set.) */
  let z =
    if (Id.Map.equal(
          Refractors.equal_entry,
          new_ephemeral_map,
          old_ephemerals,
        )) {
      z;
    } else {
      Zipper.update_ephemerals(_ => new_ephemeral_map, z);
    };
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

let is_jump_target = (info_map: Statics.Map.t, z: Zipper.t): option(Id.t) => {
  let* ci = Indicated.ci_of(z, info_map);
  let* ci =
    switch (ci) {
    | InfoExp({
        user_term: {term: Ap(_, {term: Var(_), _} as fun_expr, _), _},
        _,
      }) =>
      Statics.Map.lookup(IdTagged.rep_id(fun_expr), info_map)
    | _ => Some(ci)
    };
  Info.get_binding_site(ci);
};

/* When `def_id` is the definition body of a function-definition-sugar
   let (`let f(args) = body`), return the parameter pattern's rep_id so
   it can be anchored separately. The parameters live in the surface
   binder, on the header line(s) outside the body's row range, so a probe
   anchored on the body alone never shows them (mirrors the Fun/sugar
   handling in target_subterm_ids).

   We climb the body's ancestor chain to the nearest enclosing Let rather
   than using parent_term_of, because the desugaring inserts a synthesized
   Fun (and optional return-type Asc) as the body's immediate parent. */
let function_sugar_param_anchor =
    (info_map: Statics.Map.t, def_id: Id.t): option(Id.t) => {
  let* ci = Statics.Map.lookup(def_id, info_map);
  let rec climb = (ancs: list(Id.t)): option(Id.t) =>
    switch (ancs) {
    | [] => None
    | [anc_id, ...rest] =>
      switch (Statics.Map.lookup(anc_id, info_map)) {
      | Some(InfoExp({user_term: {term: Let(pat, def, _), _}, _})) =>
        /* Nearest enclosing let. Only anchor params when def_id is this
           let's def (the function body); when def_id is instead the let's
           `in` body — e.g. a call site like `f(5)` — those are someone
           else's params, so return None. Stop at the first let either way. */
        if (Id.equal(def_id, IdTagged.rep_id(def))) {
          switch (FunctionSugar.detect(pat)) {
          | Some((_, args, _)) => Some(Pat.rep_id(args))
          | None => None
          };
        } else {
          None;
        }
      | _ => climb(rest)
      }
    };
  climb(Info.ancestors_of(ci));
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
      ~call_stack: Sample.call_stack,
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
    | Non => promote_to_manual(ap_id, z)
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

  /* For function-definition sugar, the parameters live in the surface
     binder (not inside a Fun), so body_id alone doesn't cover them. Add a
     multi probe on the parameter pattern too — this mirrors how the Fun
     case probes its pattern via target_subterm_ids(Fun) = [body, pat]. */
  let param_anchor = function_sugar_param_anchor(info_map, body_id);
  let z =
    switch (param_anchor) {
    | None => z
    | Some(args_id) =>
      switch (probe_status(args_id, info_map, z.refractors)) {
      | Multi
      | Manual(_)
      | Statics(_)
      | Ephemeral(_) => z
      | Suppressed(_)
      | Non => add_multi(args_id, ~syntax, ~info_map, z)
      }
    };

  /* Set pin and dyn cursor using the call_stack */
  let new_stack: Sample.call_stack = [
    {
      id: ap_id,
      name: None,
      fn_def_id: None,
    },
    ...call_stack,
  ];

  /* Determine where to jump and where to look for samples.
   * - jump_target = parameters (cursor goes to params for UX)
   * - sample_probe_id = body (where samples are stored in dynamics)
   * For function literals the params are the Fun's pattern; for
   * function-definition sugar they're the surface binder's args
   * (function_sugar_param_anchor), and the body is body_id itself. */
  let (jump_target, _sample_probe_id) =
    switch (param_anchor, ci_body) {
    | (Some(args_id), _) => (args_id, body_id)
    | (
        None,
        InfoExp({user_term: {term: Fun(pat, inner_body, _, _), _}, _}),
      ) =>
      let pat_id = IdTagged.rep_id(pat);
      let inner_body_id = IdTagged.rep_id(inner_body);
      (pat_id, inner_body_id);
    | (None, _) => (body_id, body_id)
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
      | Non => promote_to_manual(ap_id, z)
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
let cursor_is_aligned_uncached =
    (~dynamics: Dynamics.Map.t, z: Zipper.t): bool => {
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

/* The scan above is O(probes x samples) and runs inside every
 * Editor.calculate (via resolve_pending_probe_cursor), including pure
 * caret moves. Its inputs are stable across most frames: the dynamics
 * map (by ref; rebuilt only when a worker result lands), the probe
 * stores (by ref; ephemerals are ref-preserved when unchanged, see
 * add_ids_from_multi_term), and the sample focus (small record,
 * compared structurally). Memoize the verdict on those — same single-
 * entry physical-identity pattern as the `ids_from_term` memo above. */
let cia_key:
  ref(
    option(
      (
        Dynamics.Map.t,
        Id.Map.t(Refractors.entry),
        list((Id.t, Refractors.entry)),
        Sample.Focus.t,
        bool,
      ),
    ),
  ) =
  ref(None);

let cursor_is_aligned = (~dynamics: Dynamics.Map.t, z: Zipper.t): bool => {
  let cursor = z.refractors.sample_focus;
  let ephemerals = z.refractors.multis.ephemerals;
  let manuals = z.refractors.manuals;
  switch (cia_key^) {
  | Some((d, e, m, c, verdict))
      when d === dynamics && e === ephemerals && m === manuals && c == cursor => verdict
  | _ =>
    let verdict = cursor_is_aligned_uncached(~dynamics, z);
    cia_key := Some((dynamics, ephemerals, manuals, cursor, verdict));
    verdict;
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
  /* A pending cursor whose ids no longer name any live probe (the probe
     was edited away before eval returned) can never resolve; clear it
     rather than letting it wedge — while set it suppresses alignment
     and forces CellEditor's double-calculate pass on every action. */
  let z =
    switch (z.refractors.pending_probe_cursor) {
    | Some(ids) when !List.exists(id => has_probe(id, z), ids) =>
      Zipper.update_refractors(z, r =>
        {
          ...r,
          pending_probe_cursor: None,
        }
      )
    | _ => z
    };
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

/* Drop the pinned call stack when an edit retires any of its frame ids
 * (the pinned call site was deleted or rewritten). A dead pin can never
 * match a sample again, and because automatic recovery is gated on
 * auto_focus it would otherwise leave every probe dark (⍟) until the
 * user finds the manual reset. Frame ids are call-site term ids, so
 * liveness = presence in the statics map, EXCEPT for frames from inside
 * builtin implementations (a fold/map applying a user callback): those
 * ids are never in any user statics map, but the frames are permanently
 * live, so they are exempt. Without the exemption, any pin through a
 * builtin (e.g. on an update call inside a fold_left callback) was
 * judged dead and silently dropped by the very recalculate the pin
 * action triggered. */
let drop_dead_pin =
    (~is_edited: bool, ~info_map: Statics.Map.t, z: Zipper.t): Zipper.t =>
  if (!is_edited) {
    z;
  } else {
    SampleFocusPerform.update_pinned_call(z, p =>
      switch (p) {
      | Some(stack)
          when
            List.exists(
              (frame: Sample.stack_frame) =>
                Statics.Map.lookup(frame.id, info_map) == None
                && !Builtins.is_internal_id(frame.id),
              stack,
            ) =>
        None
      | x => x
      }
    );
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
  |> drop_dead_pin(~is_edited, ~info_map)
  |> add_ids_from_multi_term(~syntax, ~info_map)
  |> align_to_indicated_probe(~is_edited, ~syntax)
  |> resolve_pending_focus(~dynamics)
  |> resolve_pending_probe_cursor(~dynamics, ~syntax, ~info_map)
  |> maybe_reset_cursor;

/* AUTO PROBE: pick one expression to probe based on cursor position.
 *
 * Walk ancestors outermost-to-innermost. At each ancestor we know which
 * child is on the path to the cursor.
 *
 *   Let(p, def, body):
 *     cursor in body          → continue inward
 *     cursor in def/pat/delim → probe def
 *   Seq(e1, e2):
 *     cursor in e1 or e2      → continue inward
 *     cursor on ;             → probe e1
 *   TyAlias(p, ty, body):
 *     cursor in body          → continue inward
 *     cursor in ty/pat/delim  → no probe (types aren't probeable)
 *   anything else (bare expr) → probe this ancestor itself
 *
 * If the walk falls through without picking anything, apply the same
 * rules to the cursor's own piece.
 *
 * Tests are not special-cased here; instead, any returned target id that
 * names a Test/HintedTest is rewritten to its body via `unwrap_test`.
 * This keeps the probe on the boolean condition (the useful value)
 * whether the cursor is in the test body, on the test, or on a Seq `;`
 * immediately after the test. */
let toplevel_def_body_id = (~statics: Statics.Map.t, ~id: Id.t): option(Id.t) => {
  open Language;

  let unwrap_test = (id: Id.t): Id.t =>
    switch (Statics.Map.lookup(id, statics)) {
    | Some(
        InfoExp({
          user_term: {term: Test(body) | HintedTest(body, _), _},
          _,
        }),
      ) =>
      IdTagged.rep_id(body)
    | _ => id
    };

  let probe_for_piece = (id: Id.t): option(Id.t) =>
    switch (Statics.Map.lookup(id, statics)) {
    | Some(InfoExp({user_term: {term: Let(_, def, _), _}, _})) =>
      Some(IdTagged.rep_id(def))
    | Some(InfoExp({user_term: {term: Seq(e1, _), _}, _})) =>
      Some(IdTagged.rep_id(e1))
    | Some(InfoExp({user_term: {term: TyAlias(_), _}, _})) => None
    | Some(InfoExp({user_term, _})) => Some(IdTagged.rep_id(user_term))
    | _ => None
    };

  let find_target = (starting_id: Id.t, ancestors: list(Id.t)): option(Id.t) => {
    let len = List.length(ancestors);
    let rec walk = (idx: int): option(Id.t) =>
      if (idx < 0) {
        None;
      } else {
        let anc_id = List.nth(ancestors, idx);
        let child_id =
          if (idx == 0) {
            starting_id;
          } else {
            List.nth(ancestors, idx - 1);
          };
        switch (Statics.Map.lookup(anc_id, statics)) {
        | Some(InfoExp({user_term: {term: Let(_, def, body), _}, _})) =>
          if (Id.equal(child_id, IdTagged.rep_id(body))) {
            walk(idx - 1);
          } else {
            Some(IdTagged.rep_id(def));
          }
        | Some(InfoExp({user_term: {term: Seq(e1, e2), _}, _})) =>
          let e1_id = IdTagged.rep_id(e1);
          let e2_id = IdTagged.rep_id(e2);
          if (Id.equal(child_id, e1_id) || Id.equal(child_id, e2_id)) {
            walk(idx - 1);
          } else {
            Some(e1_id);
          };
        | Some(InfoExp({user_term: {term: TyAlias(_, _, body), _}, _})) =>
          if (Id.equal(child_id, IdTagged.rep_id(body))) {
            walk(idx - 1);
          } else {
            None;
          }
        | _ =>
          /* Non-chain ancestor: the bare expression containing the cursor.
           * Probe it. */
          Some(anc_id)
        };
      };
    walk(len - 1);
  };

  /* WORKAROUND: collapse consecutive duplicate ids in the ancestor chain.
   * Function-definition sugar (`let f(x) = ...`) desugars to
   * `let f = fun x -> ...` while reusing the surface Let's id (see
   * FunctionSugar.rewrite + the function-sugar dispatch in Statics.re,
   * which recurses through `go` and so pushes that id onto `ancestors`
   * twice). The positional walk in `find_target` assumes each ancestor is
   * a distinct node whose child-on-path is the next entry, so a duplicated
   * id makes it read a cursor in the let body as if it were in the def and
   * probe the function body instead. Dropping adjacent duplicates makes the
   * walk see each Let once. Remove once the duplication is fixed at the
   * statics source. */
  let rec dedup_adjacent = (ids: list(Id.t)): list(Id.t) =>
    switch (ids) {
    | []
    | [_] => ids
    | [x, y, ...rest] =>
      Id.equal(x, y)
        ? dedup_adjacent([y, ...rest])
        : [x, ...dedup_adjacent([y, ...rest])]
    };

  switch (Statics.Map.lookup(id, statics)) {
  | Some(info) =>
    let ancestors = dedup_adjacent(Info.ancestors_of(info));
    let target =
      switch (find_target(id, ancestors)) {
      | Some(_) as result => result
      | None => probe_for_piece(id)
      };
    Option.map(unwrap_test, target);
  | None => None
  };
};

/* Remove the auto probe's multi probes if present */
let clear_autoprobe =
    (~syntax: CachedSyntax.t, ~info_map: Statics.Map.t, z: Zipper.t): Zipper.t =>
  switch (z.refractors.autoprobe_target) {
  | [] => z
  | old_ids =>
    /* Skip cursor reset here: the syntax cache still has the old probes
     * (since this isn't an edit, CachedSyntax won't recalculate until
     * the next is_edited cycle). If we reset the cursor now, the stale
     * probes render one last frame with a reset cursor, causing a brief
     * color flash before they disappear. By preserving the cursor, the
     * departing probes render with their original colors. The cursor
     * will be reset on the next editor_effects call when the probes
     * are actually gone from the syntax cache. */
    List.fold_left(
      (z, old_id) =>
        rm_multi(~drill=false, ~reset=false, ~syntax, ~info_map, old_id, z),
      z,
      old_ids,
    )
    |> Zipper.update_refractors(_, r =>
         {
           ...r,
           autoprobe_target: [],
         }
       )
  };

/* Pick the id to base autoprobe placement on.
 *
 * 1. `Indicated.index` for the typical cursor-on-tile case (also handles
 *    Inner caret correctly — e.g., cursor inside the `let` keyword of a
 *    later let, which must indicate THAT let, not whatever's to the
 *    left).
 * 2. Left-bias fallback: if Indicated couldn't pick anything probeable
 *    (e.g., cursor sits in trailing whitespace), skip past secondaries
 *    on the left and use the nearest meaningful piece. Keeps the probe
 *    sticky after typing a trailing space.
 * 3. Containing tile fallback: cursor surrounded only by whitespace with
 *    no left-meaningful piece reachable — probe the enclosing tile (e.g.
 *    cursor on a blank line within a let body → probe the let's def). */
let current_toplevel_def =
    (info_map: Statics.Map.t, z: Zipper.t): option(Id.t) => {
  let try_id = id => toplevel_def_body_id(~statics=info_map, ~id);

  let from_indicated = () =>
    switch (Indicated.index(z)) {
    | None => None
    | Some(cursor_id) => try_id(cursor_id)
    };

  let from_left = () => {
    let (l_sibs, _) = ZipperBase.sibs_with_sel(z);
    /* l_sibs is in source order; trim cursor-side (right-end) secondaries
     * then take the last remaining piece (the closest non-secondary to
     * the cursor on the left). */
    let trimmed = Segment.trim_secondary(Right, l_sibs);
    switch (ListUtil.split_last_opt(trimmed)) {
    | Some((_, last)) => try_id(Piece.id(last))
    | None => None
    };
  };

  let from_right = () => {
    let (_, r_sibs) = ZipperBase.sibs_with_sel(z);
    let trimmed = Segment.trim_secondary(Left, r_sibs);
    switch (trimmed) {
    | [first, ..._] => try_id(Piece.id(first))
    | [] => None
    };
  };

  let from_ancestor = () =>
    switch (z.relatives.ancestors) {
    | [] => None
    | [(ancestor, _), ..._] => try_id(ancestor.id)
    };

  switch (from_indicated()) {
  | Some(_) as r => r
  | None =>
    switch (from_right()) {
    | Some(_) as r => r
    | None =>
      switch (from_left()) {
      | Some(_) as r => r
      | None => from_ancestor()
      }
    }
  };
};

/* The program root id, used as the single anchor for `All` mode. A multi
 * probe anchored here expands (via MultiProbe.ids_to_multiprobe) to one
 * probe per source row across the whole program — every definition and
 * sequence component. Guarded against an empty/secondary-only segment
 * (e.g. a blank program), where there is nothing to anchor.
 *
 * Memoized on physical identity of `syntax.segment`: `Segment.skel` runs a
 * shunting-yard parse over the program, and update_autoprobe calls this every
 * frame in All mode (before the anchor short-circuit). The segment ref is
 * reused across non-edit frames by CachedSyntax, so a caret move hits the
 * cache; an edit rebuilds the segment and recomputes. */
let root_id_segment: ref(option(Segment.t)) = ref(None);
let root_id_result: ref(option(Id.t)) = ref(None);

let program_root_id = (syntax: CachedSyntax.t): option(Id.t) => {
  let stable =
    switch (root_id_segment^) {
    | Some(seg) => seg === syntax.segment
    | None => false
    };
  if (stable) {
    root_id_result^;
  } else {
    let result =
      switch (syntax.segment) {
      | [] => None
      | seg =>
        switch (Segment.root_id(Segment.skel(seg), seg)) {
        | id => Some(id)
        | exception _ => None
        }
      };
    root_id_segment := Some(syntax.segment);
    root_id_result := result;
    result;
  };
};

/* Update the auto probe based on the current mode.
 *
 * `Caret`: anchor on the top-level definition the cursor is in (following
 *   the cursor); only reconstitutes when the cursor crosses into a
 *   different definition.
 * `All`: anchor on the program root; the anchor is constant, so after the
 *   first placement cursor moves are no-ops here (the per-row ephemerals
 *   are still refreshed each edit by editor_effects -> add_ids_from_multi_term).
 * `Off`: never reached (Editor.calculate calls clear_autoprobe instead);
 *   handled as empty anchors for totality. */
let update_autoprobe =
    (
      ~mode: AutoProbe.t,
      ~syntax: CachedSyntax.t,
      ~info_map: Statics.Map.t,
      z: Zipper.t,
    )
    : Zipper.t => {
  /* Compute the desired anchor ids and a mode-specific "add" step that
     reconstitutes the probes for those anchors. Both `add` steps use
     ~drill=false so the anchor itself is multi-probed (and expanded by
     row), rather than drilling into subterms. */
  let (current_anchors, add_new) =
    switch (mode) {
    | Off => ([], (z => z))
    | All =>
      switch (program_root_id(syntax)) {
      | None => ([], (z => z))
      | Some(root_id) => (
          [root_id],
          (
            z =>
              add_multi(
                root_id,
                ~drill=false,
                ~set_pending_cursor=autoprobe_updates_cursor && auto_focus(z),
                ~syntax,
                ~info_map,
                z,
              )
          ),
        )
      }
    | Caret =>
      let current_def = current_toplevel_def(info_map, z);
      /* For function-definition sugar, also anchor the parameter pattern so
         params are probed on the header line(s) alongside the body. */
      let current_param =
        switch (current_def) {
        | Some(def_id) => function_sugar_param_anchor(info_map, def_id)
        | None => None
        };
      let anchors =
        Option.to_list(current_def) @ Option.to_list(current_param);
      /* Add new multi probes if inside a definition. The def body carries
         cursor following (gated on auto_focus so manual focus mode doesn't
         jump on def-crossing); the parameter anchor is added without
         re-triggering cursor following, keeping focus on the body's first
         sample rather than a parameter. */
      let add = z =>
        switch (current_def) {
        | None => z
        | Some(def_id) =>
          let z =
            add_multi(
              def_id,
              ~drill=false,
              ~set_pending_cursor=autoprobe_updates_cursor && auto_focus(z),
              ~syntax,
              ~info_map,
              z,
            );
          switch (current_param) {
          | Some(param_id) =>
            add_multi(
              param_id,
              ~drill=false,
              ~set_pending_cursor=false,
              ~syntax,
              ~info_map,
              z,
            )
          | None => z
          };
        };
      (anchors, add);
    };
  let prev_anchors = z.refractors.autoprobe_target;
  /* The anchors can be removed from multis.ids out from under us while
     autoprobe_target still lists them (RemoveAll; Cmd+E with the cursor
     on a term that happens to be an anchor, e.g. a top-level `;` in All
     mode whose Seq rep id IS the root anchor). Without this check the
     same-anchors short-circuit would then be a permanent no-op: every
     auto probe gone while the mode still reads All/Caret. Self-heal by
     re-running placement when an anchor is missing. */
  let anchors_intact =
    List.for_all(
      id => Id.Map.mem(id, z.refractors.multis.ids),
      current_anchors,
    );
  /* If same anchors, no change needed */
  if (List.equal(Id.equal, current_anchors, prev_anchors) && anchors_intact) {
    z;
  } else {
    /* Remove old multi probes.
       Use ~drill=false to match how they were added. */
    let z =
      List.fold_left(
        (z, old_id) => rm_multi(~drill=false, ~syntax, ~info_map, old_id, z),
        z,
        prev_anchors,
      );

    /* Regenerate ephemerals from multis.ids after removal.
       rm_multi(~drill=false) only removes the auto ID itself, not the
       expanded ephemeral IDs created by add_ids_from_multi_term. When
       transitioning to None (no definition), add_new won't run, so
       stale ephemerals would persist for one frame without this. */
    let z = add_ids_from_multi_term(~syntax, ~info_map, z);

    /* Add new multi probes for the current anchors. */
    let z = add_new(z);

    /* Update auto probe tracking */
    Zipper.update_refractors(z, r =>
      {
        ...r,
        autoprobe_target: current_anchors,
      }
    );
  };
};
