open Util;
open OptUtil.Syntax;
open Language;
open ProbeTargets;

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
    CachedSyntax.measured(syntax),
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
          TermData.extreme_measures(
            id,
            syntax.term_data,
            CachedSyntax.measured(syntax),
          )
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

/* The document row a manual probe's offside sample view renders on:
 * the end row of its term, remapped through the owning splice's
 * document row for in-splice terms — splice interiors are measured in
 * splice-local coordinates, so their raw rows are meaningless in (and
 * collide spuriously with) the outer frame. Mirrors RefractorView's
 * chip placement; the one-probe-per-line policy below is about chips
 * sharing a line, so rows must be compared in document coordinates. */
let chip_row = (~syntax: CachedSyntax.t, id: Id.t): option(int) => {
  let* (_, end_pt) =
    TermData.extreme_measures(
      id,
      syntax.term_data,
      CachedSyntax.measured(syntax),
    );
  switch (CachedSyntax.splice_containing_id(id, syntax)) {
  | None => Some(end_pt.row)
  | Some((sid, _)) =>
    let+ base = CachedSyntax.doc_row_of_splice(sid, syntax);
    base + end_pt.row;
  };
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

/* Automatic focus paths (ephemeral capture, post-edit alignment, stale-cursor
 * fallback) are honored only in auto mode; pinning switches to manual and
 * suppresses them. New automatic paths MUST gate on `auto_focus(z)`. */
let auto_focus = (z: Zipper.t): bool =>
  z.refractors.sample_focus.pinned_stack == None;

let has_probe = (id: Id.t, z: Zipper.t): bool =>
  List.assoc_opt(id, z.refractors.manuals) != None
  || Id.Map.mem(id, z.refractors.multis.ephemerals);

/* Carry over the ephemeral entry's model; a fresh default would visibly reset
 * the probe once the ephemeral is filtered out on the next rebuild. */
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

let has_no_probes = (z: Zipper.t): bool =>
  List.is_empty(z.refractors.manuals)
  && Id.Map.is_empty(z.refractors.multis.ids);

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
  /* drill=false removes the id directly (must match how it was added). */
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
    |> maybe_rm_pin(
         List.concat_map(ids_from_term(~syntax, ~info_map), target_ids),
       );
  /* skip reset when reset=false (clear_autoprobe), to avoid a style flash */
  reset ? maybe_reset_cursor(z) : z;
};

let rm_manual = (ids: list(Id.t), z: Zipper.t): Zipper.t =>
  Zipper.update_manuals(
    map => List.filter(((id, _)) => !List.mem(id, ids), map),
    z,
  )
  |> maybe_rm_pin(ids)
  |> maybe_reset_cursor;

/* After edits, probes can reflow onto the same line; keep the rightmost, drop the rest. */
let remove_colliding_probes = (~syntax: CachedSyntax.t, z: Zipper.t): Zipper.t => {
  let row_to_probes =
    List.fold_right(
      ((probe_id, _), acc) =>
        switch (
          chip_row(~syntax, probe_id),
          TermData.extreme_measures(
            probe_id,
            syntax.term_data,
            CachedSyntax.measured(syntax),
          ),
        ) {
        | (Some(row), Some((_, end_pt))) =>
          let existing =
            IntMap.find_opt(row, acc) |> Option.value(~default=[]);
          IntMap.add(row, [(probe_id, end_pt.col), ...existing], acc);
        | _ => acc
        },
      z.refractors.manuals,
      IntMap.empty,
    );

  let ids_to_remove =
    IntMap.fold(
      (_, probes, acc) =>
        switch (probes) {
        | []
        | [_] => acc
        | _ =>
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
    target_ids |> List.filter_map(id => chip_row(~syntax, id));

  let conflicting_ids =
    List.fold_right(
      ((probe_id, _), acc) =>
        switch (chip_row(~syntax, probe_id)) {
        | Some(row) when List.mem(row, target_end_rows) => [probe_id, ...acc]
        | _ => acc
        },
      z.refractors.manuals,
      [],
    );

  let z = rm_manual(conflicting_ids, z);
  let z =
    List.fold_left(
      (z, id) => Zipper.add_manual(id, Probe, z),
      z,
      target_ids,
    );

  let sorted_ids = sort_ids_lexically(~syntax, target_ids);
  set_pending_probe(sorted_ids, z);
};

let add_manual =
    (~syntax: CachedSyntax.t, id: Id.t, info_map: Statics.Map.t, z: Zipper.t)
    : Zipper.t =>
  switch (target_subterm_ids(id, info_map)) {
  | [] =>
    /* Not probeable: a pending_probe_cursor that never resolves would suppress alignment and force CellEditor's double-calculate pass. */
    z
  | target_ids => add_manual_targets(~syntax, target_ids, z)
  };

let toggle_manual =
    (~syntax: CachedSyntax.t, id: Id.t, ~info_map: Statics.Map.t, z: Zipper.t)
    : Zipper.t =>
  switch (probe_status(id, info_map, z.refractors)) {
  | Multi =>
    rm_multi(~syntax, ~info_map, id, z) |> add_manual(~syntax, id, info_map)
  | Statics(ids) => rm_manual(ids, z) |> add_manual(~syntax, id, info_map)
  | Manual(ids) => rm_manual(ids, z)
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
  let z =
    Zipper.update_suppressed(
      suppressed =>
        Id.Map.filter((id, _) => List.mem(id, all_ids), suppressed),
      z,
    );
  let manual_ids = List.map(fst, z.refractors.manuals);
  let ids =
    List.filter(
      id =>
        !List.mem(id, manual_ids)
        && !Id.Map.mem(id, z.refractors.multis.suppressed),
      all_ids,
    );
  let manual_end_rows =
    List.filter_map(
      ((id, _)) =>
        switch (
          TermData.extreme_measures(
            id,
            syntax.term_data,
            CachedSyntax.measured(syntax),
          )
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
          TermData.extreme_measures(
            id,
            syntax.term_data,
            CachedSyntax.measured(syntax),
          )
        ) {
        | Some((_, end_loc)) => !List.mem(end_loc.row, manual_end_rows)
        | None => true
        },
      ids,
    );
  let old_ephemerals = z.refractors.multis.ephemerals;
  /* Preserve surviving ephemeral entries; a fresh mk_entry per id would wipe per-probe state (e.g. drawer_mode). */
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
  /* Keep the previous ephemerals ref when unchanged: a fresh map makes CachedSyntax rebuild Measured (O(program)) every frame (gates on `multis.ephemerals !==`). */
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
  /* Gated on auto_focus: in manual focus mode, don't auto-capture new ephemerals. */
  let new_ids = List.filter(id => !Id.Map.mem(id, old_ephemerals), ids);
  switch (new_ids) {
  | [] => z
  | _ when !auto_focus(z) => z
  | _ =>
    let sorted = sort_ids_lexically(~syntax, new_ids);
    set_pending_probe(sorted, z);
  };
};

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
  /* drill=false probes the id directly, no subterm drilling (auto probe stays on the top-level def). */
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

  if (set_pending_cursor) {
    /* same target_ids as multis.ids, so ephemeral ids match for sample lookup */
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

/* Definition forms (Let/Test): the unified probe action uses a multi probe (per-line expansion) rather than manual. */
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

/* For function-sugar (`let f(args) = body`), params live in the surface binder
   outside the body's rows, so return their pattern id to anchor separately.
   Climb to the enclosing Let (not parent_term_of: desugaring inserts a Fun parent). */
let function_sugar_param_anchor =
    (info_map: Statics.Map.t, def_id: Id.t): option(Id.t) => {
  let* ci = Statics.Map.lookup(def_id, info_map);
  let rec climb = (ancs: list(Id.t)): option(Id.t) =>
    switch (ancs) {
    | [] => None
    | [anc_id, ...rest] =>
      switch (Statics.Map.lookup(anc_id, info_map)) {
      | Some(InfoExp({user_term: {term: Let(pat, def, _), _}, _})) =>
        /* Anchor params only when def_id is THIS let's def (the fn body); if it's the `in` body (a call site) the params are someone else's. Stop at the first let either way. */
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

/* Step-into is sample-level: a sample with call_stack [a,b,c] gives the body
 * stack [ap_id,a,b,c]. Sets pending_focus; CellEditor's second calculate pass
 * (once worker dynamics land) resolves it and FocusEffect schedules DOM focus. */
let step_into_call_stack =
    (
      ~syntax: CachedSyntax.t,
      ~call_stack: CallStack.t,
      ~frame: CallStack.frame,
      info_map: Statics.Map.t,
      z: Zipper.t,
    )
    : option(Zipper.t) => {
  let ap_id = frame.id;
  /* Tier 1 (static): resolve the fn via its name's let-binding (fn position is a let-bound var). */
  let static_body_id = {
    let* ci_ap = Statics.Map.lookup(ap_id, info_map);
    let* binding_id =
      switch (ci_ap) {
      | InfoExp({
          user_term: {term: Ap(_, {term: Var(_), _} as fun_expr, _), _},
          _,
        }) =>
        let* ci_var =
          Statics.Map.lookup(IdTagged.rep_id(fun_expr), info_map);
        Info.get_binding_site(ci_var);
      | _ => None
      };
    Statics.Map.enclosing_let_of_binding(~statics=info_map, ~binding_id);
  };
  /* Tier 2 (dynamic): fall back to the frame's recorded fn_def_id, for higher-order calls where the static binding site is only a parameter. */
  let* body_id =
    switch (static_body_id) {
    | Some(id) => Some(id)
    | None => frame.fn_def_id
    };
  let* ci_body = Statics.Map.lookup(body_id, info_map);

  /* Promote any multi probe on ap_id to manual so it persists across the jump. */
  let z =
    switch (probe_status(ap_id, info_map, z.refractors)) {
    | Manual(_)
    | Statics(_) => z
    | Multi
    | Ephemeral(_)
    | Suppressed(_)
    | Non => promote_to_manual(ap_id, z)
    };

  let z =
    switch (probe_status(body_id, info_map, z.refractors)) {
    | Multi
    | Manual(_)
    | Statics(_)
    | Ephemeral(_) => z
    | Suppressed(_)
    | Non => add_multi(body_id, ~syntax, ~info_map, z)
    };

  /* Function-sugar params live in the surface binder, not under body_id; anchor the param pattern too. */
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

  /* Use the real captured frame (name + dynamic fn_def_id), not a synthesized id-only one, so the pin/focus is precise. */
  let new_stack: CallStack.t = [frame, ...call_stack];

  /* jump_target = params (cursor for UX); samples live under body_id. */
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

  let z =
    SampleFocusPerform.update(z, _ => {
      {
        ...z.refractors.sample_focus,
        call_stack: new_stack,
        index: List.length(call_stack),
        pinned_stack: Some(new_stack),
        pending_focus: None,
        anchor: None,
        pinned_span: None,
      }
    });

  /* Schedule focus back to the main editor after render */
  FocusEffect.schedule_editor();

  Move.jump_to_id_indicated(z, jump_target);
};

let can_statics = (id: Id.t, info_map: Statics.Map.t): bool =>
  Info.is_typable_term(Statics.Map.lookup(id, info_map));

/* Type annotations don't support auto mode or pins. */
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
    | Statics(ids) => rm_manual(ids, z)
    | Manual(ids) => rm_manual(ids, z) |> add_statics
    | Multi => rm_multi(~syntax, ~info_map, id, z) |> add_statics
    | Ephemeral(_)
    | Suppressed(_)
    | Non => add_statics(z)
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
          CachedSyntax.measured(syntax),
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
  | StepInto(call_stack, frame) =>
    switch (step_into_call_stack(~syntax, ~call_stack, ~frame, info_map, z)) {
    | Some(z) => z
    | None => z
    }
  | Pin(call_stack, ap_id) =>
    /* Promote any multi probe to manual so the pin persists across cursor movement. */
    let z =
      switch (probe_status(ap_id, info_map, z.refractors)) {
      | Manual(_)
      | Statics(_) => z
      | Multi
      | Ephemeral(_)
      | Suppressed(_)
      | Non => promote_to_manual(ap_id, z)
      };
    /* Pin actions carry no sample (the pinned CALL may not itself be
     * probed); the span ref comes from stack decomposition, opened=None. */
    SampleFocusPerform.toggle_pin_call(z, call_stack, None);
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

let can_probe = (id: Id.t, info_map: Statics.Map.t): bool =>
  target_subterm_ids(id, info_map) != [];
