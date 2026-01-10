open Util;
open OptUtil.Syntax;
open Language;

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
    : List.exists(id => List.mem(id, refractors.autos), target_ids)
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
  DynCursorPerform.update_pinned_call(_, p =>
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
      autos:
        List.filter(id => !List.mem(id, target_ids), z.refractors.autos),
      ephemerals:
        Id.Map.filter(
          (id', _) => !List.mem(id', target_ids),
          z.refractors.ephemerals,
        ),
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
  let ids =
    List.concat_map(ids_from_term(~syntax, ~info_map), z.refractors.autos);
  Zipper.update_ephemerals(
    _ =>
      List.fold_left(
        (map, id) => Id.Map.add(id, MkRefractor.mk(Probe, id), map),
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
      autos: target_ids @ z.refractors.autos,
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

let step_into =
    (~syntax: CachedSyntax.t, info_map: Statics.Map.t, z: Zipper.t)
    : option(Zipper.t) => {
  let* binding_id = is_jump_target(info_map, z);
  let* body_id =
    Statics.Map.enclosing_let_of_binding(~statics=info_map, ~binding_id);
  let* ci_body = Statics.Map.lookup(body_id, info_map);
  let z =
    switch (probe_status(body_id, info_map, z.refractors)) {
    /* If already probed, leave it alone */
    | REPL => z
    | Manual(_) => z
    | Non => add_auto(body_id, ~syntax, ~info_map, z)
    //toggle_manual(~syntax, body_id, ~info_map, z)
    };

  //TODO(andrew): need to first set dyn_cursor to current thing if not already set...

  // set pin and dyn cursor
  let* ap_id =
    switch (Indicated.ci_of(z, info_map)) {
    | Some(InfoExp({term: {term: Ap(_, _, _), _} as ap, _})) =>
      Some(IdTagged.rep_id(ap))
    | _ =>
      let* indicated_id = Indicated.index(z);
      switch (Statics.Map.parent_term_of(info_map, indicated_id)) {
      | Some(Exp({term: Ap(_, _, _), _} as ap)) =>
        Some(IdTagged.rep_id(ap))
      | _ => None
      };
    };
  let z =
    DynCursorPerform.update_dyn_cursor(
      z,
      _ => {
        // need to trim before adding ap
        let trimmed = DynCursor.trimmed_stack(z.refractors.dyn_cursor);
        {
          ...z.refractors.dyn_cursor,
          stack: [ap_id, ...trimmed],
          index: List.length(trimmed),
          pinned_stack: Some([ap_id, ...trimmed]),
        };
      },
    );
  switch (ci_body) {
  | InfoExp({term: {term: Fun(pat, _body, _, _), _}, _}) =>
    Move.jump_to_id_indicated(z, IdTagged.rep_id(pat))
  | _ => Move.jump_to_id_indicated(z, body_id)
  };
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

let update =
    (
      ~statics as {info_map, _}: CachedStatics.t,
      ~syntax: CachedSyntax.t,
      a: Action.refractor,
      z: Zipper.t,
    )
    : Zipper.t =>
  switch (a) {
  | ToggleProbeManual =>
    switch (z.selection.content) {
    | [] =>
      switch (Indicated.index(z)) {
      | None => z
      | Some(id) => toggle_manual(~syntax, id, ~info_map, z)
      }
    | _ => rm_probes_in_selection(~syntax, ~info_map, z)
    }
  | ToggleProbeREPL =>
    switch (Indicated.index(z)) {
    | Some(id) => toggle_auto(~syntax, id, info_map, z)
    | None => z
    }
  | ProbeJump =>
    switch (step_into(~syntax, info_map, z)) {
    | Some(z) => z
    | None => z
    }
  };

/* Check if id has either manual or ephermeral probe on it */
let has_probe = (id: Id.t, z: Zipper.t): bool => {
  Id.Map.mem(id, z.refractors.manuals)
  || Id.Map.mem(id, z.refractors.ephemerals);
};

/* Check if probing is allowed for the given id.
   Used by ContextMenu to determine whether to show probe options. */
let can_probe = (id: Id.t, info_map: Statics.Map.t): bool =>
  target_subterm_ids(id, info_map) != [];
