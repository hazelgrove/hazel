open Util;
open OptUtil.Syntax;

let target_subterm_ids = (id: Id.t, info_map: Language.Statics.Map.t) =>
  switch (Language.Statics.Map.lookup(id, info_map)) {
  | Some(InfoExp({term: {term: Fun(pat, body, _, _), _}, _})) =>
    /* Unfortunate edge behavior here; since we're inspecting the term,
       it has probes on it from the refractors; we must account for the fact
       that if a probe is already on, the id will the the probe id, not the
       underlying term id which is the id in the refractors map */
    let body_id =
      switch (body.term) {
      | Probe(_) => Id.recover_original(Language.IdTagged.rep_id(body))
      | _ => Language.IdTagged.rep_id(body)
      };
    let pat_id =
      switch (pat.term) {
      | Probe(_) => Id.recover_original(Language.IdTagged.rep_id(pat))
      | _ => Language.IdTagged.rep_id(pat)
      };
    [body_id, pat_id];
  | _ => [id]
  };

type probe_status =
  | Manual(list(Id.t)) /* If a function literal, ids are pat and body ids */
  | REPL
  | Non;

let probe_status =
    (
      id: Id.t,
      info_map: Language.Statics.Map.t,
      refractors: Zipper.Refractor.t,
    )
    : probe_status => {
  let ids = target_subterm_ids(id, info_map);
  List.for_all(id => Id.Map.mem(id, refractors.manuals), ids)
    ? Manual(target_subterm_ids(id, info_map))
    : List.mem(id, refractors.autos) ? REPL : Non;
};

let rm_repl = (id: Id.t, z: Zipper.t): Zipper.t =>
  Zipper.update_refractors(z, refractors =>
    {
      ...refractors,
      autos: List.filter((!=)(id), z.refractors.autos),
      ephemerals:
        Id.Map.filter((id', _) => id' != id, z.refractors.ephemerals),
    }
  );

let rm_manual = (ids: list(Id.t), z: Zipper.t): Zipper.t =>
  Zipper.update_manuals(
    map => Id.Map.filter((id, _) => !List.mem(id, ids), map),
    z,
  );

let add_manual =
    (id: Id.t, info_map: Language.Statics.Map.t, z: Zipper.t): Zipper.t => {
  let ids = target_subterm_ids(id, info_map);
  List.fold_left((z, id) => MkRefractor.add_single(id, z), z, ids);
};

let toggle_manual =
    (id: Id.t, info_map: Language.Statics.Map.t, z: Zipper.t): Zipper.t =>
  switch (probe_status(id, info_map, z.refractors)) {
  | REPL => rm_repl(id, z) |> add_manual(id, info_map)
  | Manual(ids) => rm_manual(ids, z)
  | Non => add_manual(id, info_map, z)
  };

let ids_from_term =
    (~term_data, ~terms, ~measured, ~info_map, id: Id.t): list(Id.t) =>
  AutoProbe.ids_to_autoprobe(id, term_data, terms, measured, info_map)
  |> Option.to_list
  |> List.flatten
  |> List.filter_map(Fun.id);

let add_ids_from_pinned_term =
    (~term_data, ~terms, ~measured, ~info_map, z: Zipper.t): Zipper.t => {
  let ids =
    switch (z.refractors.autos) {
    | [] => []
    | [hd, ..._] =>
      // This ignores other pinned terms... see below
      ids_from_term(~term_data, ~terms, ~measured, ~info_map, hd)
    };
  //TODO(andrew): should there only be one repl at a time? this is sort of what above does but not quite
  Zipper.update_ephemerals(
    _ =>
      List.fold_left(
        (map, id) =>
          Id.Map.add(
            id,
            MkRefractor.mk(Probe, Id.transform_variant(id)),
            map,
          ),
        Id.Map.empty,
        ids,
      ),
    z,
  );
};

let add_repl = (id: Id.t, syntax: CachedSyntax.t, z: Zipper.t): Zipper.t =>
  Zipper.update_refractors(z, refractors =>
    {
      ...refractors,
      autos: [id, ...z.refractors.autos],
    }
  )
  |> add_ids_from_pinned_term(
       ~term_data=syntax.term_data,
       ~terms=syntax.terms,
       ~measured=syntax.measured,
       ~info_map=Language.Statics.Map.empty /* TODO: get real info_map */
     );

let toggle_repl =
    (
      ~syntax: CachedSyntax.t,
      id: Id.t,
      info_map: Language.Statics.Map.t,
      z: Zipper.t,
    )
    : Zipper.t =>
  switch (probe_status(id, info_map, z.refractors)) {
  | REPL => rm_repl(id, z)
  | Manual(ids) => rm_manual(ids, z) |> add_repl(id, syntax)
  | Non => add_repl(id, syntax, z)
  };

let probe_jump =
    (statics: Language.Statics.Map.t, z: Zipper.t): option(Zipper.t) => {
  let* ci = Indicated.ci_of(z, statics);
  let* binding_id = Language.Info.get_binding_site(ci);
  let* body_id =
    Language.Statics.Map.enclosing_let_of_binding(~statics, ~binding_id);
  let* ci_body = Language.Statics.Map.lookup(body_id, statics);
  let z = toggle_manual(body_id, statics, z);
  switch (ci_body) {
  | InfoExp({term: {term: Fun(pat, _body, _, _), _}, _}) =>
    let jump_target_id =
      switch (pat.term) {
      | Probe(_) => Id.recover_original(Language.IdTagged.rep_id(pat))
      | _ => Language.IdTagged.rep_id(pat)
      };
    Move.jump_to_id_indicated(z, jump_target_id);
  | _ => Move.jump_to_id_indicated(z, body_id)
  };
};

let rm_probes_in_selection = (z: Zipper.t): Zipper.t => {
  //TODO: remove repls in selection too?
  let selection_ids = Selection.selection_ids(z.selection);
  let manuals =
    Id.Map.filter(
      (id, _) => !List.mem(id, selection_ids),
      z.refractors.manuals,
    );
  Zipper.update_refractors(z, refractors =>
    {
      ...refractors,
      manuals,
    }
  );
};

let update =
    (
      ~statics: CachedStatics.t,
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
      | Some(id) => toggle_manual(id, statics.info_map, z)
      }
    | _ => rm_probes_in_selection(z)
    }
  | ToggleProbeREPL =>
    switch (Indicated.index(z)) {
    | Some(id) => toggle_repl(~syntax, id, statics.info_map, z)
    | None => z
    }
  | ProbeJump =>
    switch (probe_jump(statics.info_map, z)) {
    | Some(z) => z
    | None => z
    }
  };
