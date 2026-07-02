open Util;
open OptUtil.Syntax;
open Language;

let rec target_subterm_ids =
        (~drill_let=true, id: Id.t, info_map: Statics.Map.t) =>
  switch (Statics.Map.lookup(id, info_map)) {
  | Some(InfoExp({user_term: {term: Fun(pat, body, _, _), _}, _})) => [
      IdTagged.rep_id(body),
      IdTagged.rep_id(pat),
    ]
  | Some(InfoExp({user_term: {term: Let(pat, def, _), _} as let_term, _}))
      when drill_let =>
    /* Probe the def (drill_let=false: a nested let chain anchors whole); a test-body let probes itself to show the test result. */
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
      let def_targets =
        target_subterm_ids(~drill_let=false, IdTagged.rep_id(def), info_map);
      /* Function-sugar keeps params in the binder, so anchor the args pattern too. */
      switch (FunctionSugar.detect(pat)) {
      | Some((_f_name, args, _ret_ty)) => def_targets @ [Pat.rep_id(args)]
      | None => def_targets
      };
    };
  | Some(InfoExp({user_term: {term: ModuleExp(_, def, _), _}, _})) =>
    target_subterm_ids(IdTagged.rep_id(def), info_map)

  | Some(InfoExp({user_term: {term: Var(_), _} as v, _})) =>
    switch (Statics.Map.parent_term_of(info_map, IdTagged.rep_id(v))) {
    | Some(Exp({term: Ap(_, f_expr, _), _} as ap)) when f_expr == v => [
        IdTagged.rep_id(ap),
      ]
    | Some(Exp({term: DeferredAp(f_expr, _), _} as dap)) when f_expr == v =>
      switch (Statics.Map.parent_term_of(info_map, IdTagged.rep_id(dap))) {
      | Some(Exp({term: Ap(_, f_expr, _), _} as ap)) when f_expr == dap => [
          IdTagged.rep_id(ap),
        ]
      | _ => [id]
      }
    | _ => [id]
    }
  | Some(InfoExp({user_term: {term: DeferredAp(_), _} as v, _})) =>
    switch (Statics.Map.parent_term_of(info_map, IdTagged.rep_id(v))) {
    | Some(Exp({term: Ap(_, f_expr, _), _} as ap)) when f_expr == v => [
        IdTagged.rep_id(ap),
      ]
    | _ => [id]
    }
  | info when !Info.is_typable_term(info) => []
  /* rep_id: multi-tile forms (tuples, lists, case) must match between probe_map and the evaluator. */
  | Some(InfoExp({user_term, _})) => [IdTagged.rep_id(user_term)]
  | Some(InfoPat({user_term, _})) => [Pat.rep_id(user_term)]
  | _ => [id]
  };

type probe_status =
  | Manual(list(Id.t))
  | Statics(list(Id.t))
  | Multi
  | Ephemeral(list(Id.t))
  | Suppressed(list(Id.t))
  | Non;

let probe_status =
    (id: Id.t, info_map: Statics.Map.t, refractors: Zipper.Refractor.t)
    : probe_status => {
  let target_ids = target_subterm_ids(id, info_map);
  /* ANY (not ALL) target id: else a cleaned-up sibling target (remove_colliding_probes dropping a single-line fun's pat probe) makes the toggle re-add forever instead of removing. */
  let manual_entries =
    List.filter_map(
      id => List.assoc_opt(id, refractors.manuals),
      target_ids,
    );
  if (manual_entries != []) {
    let all_statics =
      List.for_all(
        (entry: Refractors.entry) => entry.kind == Statics,
        manual_entries,
      );
    all_statics ? Statics(target_ids) : Manual(target_ids);
  } else if (List.exists(
               id => Id.Map.mem(id, refractors.multis.ids),
               target_ids,
             )) {
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

/* Memoize the O(program) per-row multi-probe expansion. It's a pure function
 * of the immutable syntax/statics snapshots + anchor id, so we key on physical
 * identity of those refs (O(1)) and drop the table when any ref changes — a
 * stable-syntax run (pure caret moves) serves every anchor from cache. Single
 * global entry, so multiple editors invalidate each other (correct). */
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
    target_ids
    |> List.filter_map(id =>
         TermData.extreme_measures(id, syntax.term_data, syntax.measured)
         |> Option.map(((_, end_pt: Point.t)) => end_pt.row)
       );

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
      ~call_stack: Sample.call_stack,
      ~frame: Sample.stack_frame,
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
  let new_stack: Sample.call_stack = [frame, ...call_stack];

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

let resolve_pending_focus = (~dynamics: Dynamics.Map.t, z: Zipper.t): Zipper.t =>
  switch (z.refractors.sample_focus.pending_focus) {
  | None => z
  | Some({probe_id, target_stack}) =>
    switch (Dynamics.Map.lookup(probe_id, dynamics)) {
    | None => z
    | Some(samples) =>
      let z' =
        SampleFocusPerform.resolve_pending_focus(z, samples, target_stack);
      if (z'.refractors.sample_focus.pending_focus == None) {
        FocusEffect.schedule(probe_id);
      };
      z';
    }
  };

let cursor_is_aligned_uncached =
    (~dynamics: Dynamics.Map.t, z: Zipper.t): bool => {
  let cursor = z.refractors.sample_focus;
  if (cursor.call_stack == []) {
    true;
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

/* Memoize the O(probes x samples) verdict on physical identity (runs every Editor.calculate incl. caret moves; inputs ref-stable). */
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

/* Case 1: a pending cursor → first pending id with samples. Case 2: no pending
 * but the cursor went stale (structural edit) → caret-nearest probe. A pin skips
 * case 2 (preserve pinned context) but still resolves case 1. */
let resolve_pending_probe_cursor =
    (
      ~dynamics: Dynamics.Map.t,
      ~syntax: CachedSyntax.t,
      ~info_map: Statics.Map.t,
      z: Zipper.t,
    )
    : Zipper.t => {
  /* A pending cursor whose ids no longer name a live probe can never resolve; clear it, else it wedges (suppresses alignment, forces the double-calculate pass every action). */
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
  let (target_ids, is_pending) =
    switch (z.refractors.pending_probe_cursor) {
    | Some(ids) => (Some(ids), true)
    | None =>
      if (cursor_is_aligned(~dynamics, z) || !auto_focus(z)) {
        (None, false);
      } else {
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
    | None => if (is_pending) {z} else {z}
    };
  };
};

/* When grout ID preservation keeps the same id across a structural edit,
 * add_ids_from_multi_term won't set pending_probe_cursor; align to the
 * caret-nearest ephemeral instead. Indicated.index gives a piece id, ephemerals
 * key on term ids — try a direct match, then fall back to spatial proximity. */
let align_to_indicated_probe =
    (~is_edited: bool, ~syntax: CachedSyntax.t, z: Zipper.t): Zipper.t =>
  if (!is_edited
      || z.refractors.pending_probe_cursor != None
      || !auto_focus(z)) {
    z;
  } else {
    switch (caret_nearest_ephemeral(~syntax, z)) {
    | Some(id) => set_pending_probe([id], z)
    | None => z
    };
  };

/* Drop a pinned call stack once no sample matches it (call site deleted/
 * unreached) — a dead pin darkens every probe (⍟), since recovery is gated on
 * auto_focus. Checked against eval RESULTS, not statics: pinned stacks contain
 * builtin/worker-minted ids absent from UI statics, and samples+pins both come
 * from the worker (process-consistent). Skipped on empty dynamics. */
let drop_dead_pin = (~dynamics: Dynamics.Map.t, z: Zipper.t): Zipper.t =>
  SampleFocusPerform.update_pinned_call(z, p =>
    switch (p) {
    | Some(stack) when !Id.Map.is_empty(dynamics) =>
      let pinned_ids = Sample.ids_of_stack(stack);
      let (head_id, tail_ids) =
        switch (pinned_ids) {
        | [hd, ...tl] => (Some(hd), tl)
        | [] => (None, [])
        };
      let alive = (s: Sample.t) => {
        let s_ids = Sample.ids_of_stack(s.call_stack);
        ListUtil.is_suffix_of(pinned_ids, s_ids)
        || Some(s.syntax_id) == head_id
        && s_ids == tail_ids;
      };
      Id.Map.exists((_, samples) => List.exists(alive, samples), dynamics)
        ? Some(stack) : None;
    | x => x
    }
  );

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
  |> drop_dead_pin(~dynamics)
  |> add_ids_from_multi_term(~syntax, ~info_map)
  |> align_to_indicated_probe(~is_edited, ~syntax)
  |> resolve_pending_focus(~dynamics)
  |> resolve_pending_probe_cursor(~dynamics, ~syntax, ~info_map)
  |> maybe_reset_cursor;

/* AUTO PROBE: walk ancestors outermost-to-innermost, picking the def of the
 * enclosing Let / first component of a Seq / the bare expression at the cursor
 * (types aren't probeable); fall back to the cursor's own piece. A Test target
 * is rewritten to its body (unwrap_test) to probe the boolean condition. */
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
        | _ => Some(anc_id)
        };
      };
    walk(len - 1);
  };

  /* WORKAROUND: function-sugar reuses the surface Let's id, so it appears twice
   * in `ancestors`; the positional walk would then misread a cursor in the let
   * body as being in the def. Dedup adjacent ids. Remove once fixed in statics. */
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

let clear_autoprobe =
    (~syntax: CachedSyntax.t, ~info_map: Statics.Map.t, z: Zipper.t): Zipper.t =>
  switch (z.refractors.autoprobe_target) {
  | [] => z
  | old_ids =>
    /* Skip the cursor reset: this isn't an edit, so the stale probes render one
     * more frame; resetting now would flash them a reset color. editor_effects
     * resets once they're gone from the syntax cache. */
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

/* Pick the id to base autoprobe placement on: (1) Indicated.index for the usual
 * cursor-on-tile case; (2) left-bias fallback past secondaries (keeps the probe
 * sticky after a trailing space); (3) the enclosing tile (cursor on a blank line). */
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
    /* trim right-end secondaries, then take the last piece (nearest non-secondary on the left). */
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

  [from_indicated, from_right, from_left, from_ancestor]
  |> List.fold_left((acc, f) => acc == None ? f() : acc, None);
};

/* Program root id: the single `All`-mode anchor (expands to one probe per row).
 * Memoized on physical identity of `syntax.segment`, since Segment.skel parses
 * the whole program and update_autoprobe calls this every All-mode frame. */
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

/* Caret: anchor on the top-level def the cursor is in (reconstitutes on crossing
 * into another def). All: anchor on the program root (constant). Off: unreached
 * (Editor.calculate clears instead); empty anchors here for totality. */
let update_autoprobe =
    (
      ~mode: AutoProbe.t,
      ~syntax: CachedSyntax.t,
      ~info_map: Statics.Map.t,
      z: Zipper.t,
    )
    : Zipper.t => {
  /* drill=false so the anchor itself is multi-probed (expanded by row), not drilled into subterms. */
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
                ~set_pending_cursor=auto_focus(z),
                ~syntax,
                ~info_map,
                z,
              )
          ),
        )
      }
    | Caret =>
      let current_def = current_toplevel_def(info_map, z);
      /* Function-sugar: also anchor the param pattern so params are probed on the header line(s). */
      let current_param =
        switch (current_def) {
        | Some(def_id) => function_sugar_param_anchor(info_map, def_id)
        | None => None
        };
      let anchors =
        Option.to_list(current_def) @ Option.to_list(current_param);
      /* def body carries cursor following (gated on auto_focus); the param anchor is added without it, keeping focus on the body's first sample. */
      let add = z =>
        switch (current_def) {
        | None => z
        | Some(def_id) =>
          let z =
            add_multi(
              def_id,
              ~drill=false,
              ~set_pending_cursor=auto_focus(z),
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
  /* Anchors can be removed from multis.ids while autoprobe_target still lists
     them (RemoveAll, or Cmd+E on a term that is an anchor); without this check
     the same-anchors short-circuit would be a permanent no-op. Self-heal. */
  let anchors_intact =
    List.for_all(
      id => Id.Map.mem(id, z.refractors.multis.ids),
      current_anchors,
    );
  if (List.equal(Id.equal, current_anchors, prev_anchors) && anchors_intact) {
    z;
  } else {
    /* drill=false to match how they were added. */
    let z =
      List.fold_left(
        (z, old_id) => rm_multi(~drill=false, ~syntax, ~info_map, old_id, z),
        z,
        prev_anchors,
      );

    /* Regenerate ephemerals: rm_multi(~drill=false) drops only the anchor id, not its expanded ephemerals; without this they'd persist a frame when transitioning to no-def. */
    let z = add_ids_from_multi_term(~syntax, ~info_map, z);

    let z = add_new(z);
    Zipper.update_refractors(z, r =>
      {
        ...r,
        autoprobe_target: current_anchors,
      }
    );
  };
};
