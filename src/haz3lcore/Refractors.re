open Util;
open OptUtil.Syntax;

let mk_probe = (id): Base.projector => {
  let kind = ProjectorCore.Kind.Probe;
  let (module P) = ProjectorInit.to_module(kind);
  let seg: Segment.t = [Piece.mk_grout(~id=Id.invalid, Convex)];
  let piece: Base.piece = Segment.parenthesize(seg);
  let any =
    MakeTerm.for_projection(seg) |> OptUtil.get_or_fail("mk_probe: maketerm");
  let model = P.init(any) |> OptUtil.get_or_fail("mk_probe: init");
  ProjectorCore.mk(~id, kind, piece, model);
};

let update_refractors_map = (f, z: Zipper.t): Zipper.t => {
  ...z,
  refractors: {
    ...z.refractors,
    map: f(z.refractors.map),
  },
};

let update_ephemerals = (f, z: Zipper.t): Zipper.t => {
  ...z,
  refractors: {
    ...z.refractors,
    ephemerals: f(z.refractors.ephemerals),
  },
};

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
  List.for_all(id => Id.Map.mem(id, refractors.map), ids)
    ? Manual(target_subterm_ids(id, info_map))
    : List.mem(id, refractors.pinned_term_ids) ? REPL : Non;
};

let rm_repl = (id: Id.t, z: Zipper.t): Zipper.t =>
  Zipper.update_refractors(z, refractors =>
    {
      ...refractors,
      pinned_term_ids: List.filter((!=)(id), z.refractors.pinned_term_ids),
      ephemerals:
        Id.Map.filter((id', _) => id' != id, z.refractors.ephemerals),
    }
  );

let add_single = (id: Id.t, z: Zipper.t): Zipper.t => {
  let p = mk_probe(Id.transform_variant(id));
  update_refractors_map(Id.Map.add(id, p), z);
};

let rm_manual = (ids: list(Id.t), z: Zipper.t): Zipper.t =>
  update_refractors_map(
    map => Id.Map.filter((id, _) => !List.mem(id, ids), map),
    z,
  );

let add_manual =
    (id: Id.t, info_map: Language.Statics.Map.t, z: Zipper.t): Zipper.t => {
  let ids = target_subterm_ids(id, info_map);
  List.fold_left((z, id) => add_single(id, z), z, ids);
};

let toggle_manual =
    (id: Id.t, info_map: Language.Statics.Map.t, z: Zipper.t): Zipper.t =>
  switch (probe_status(id, info_map, z.refractors)) {
  | REPL => rm_repl(id, z)
  | Manual(ids) => rm_manual(ids, z)
  | Non => add_manual(id, info_map, z)
  };

let ids_from_term = (~term_data, ~measured, id: Id.t): list(Id.t) =>
  TermData.get_largest_terminal_term_ids(id, term_data, measured)
  |> Option.to_list
  |> List.flatten
  |> List.filter_map(Fun.id);

let add_ids_from_pinned_term = (~term_data, ~measured, z: Zipper.t): Zipper.t => {
  let ids =
    switch (z.refractors.pinned_term_ids) {
    | [] => []
    | [hd, ..._] =>
      // This ignores other pinned terms... see below
      ids_from_term(~term_data, ~measured, hd)
    };
  //TODO(andrew): should there only be one repl at a time? this is sort of what above does but not quite
  update_ephemerals(
    _ =>
      List.fold_left(
        (map, id) =>
          Id.Map.add(id, mk_probe(Id.transform_variant(id)), map),
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
      pinned_term_ids: [id, ...z.refractors.pinned_term_ids],
    }
  )
  |> add_ids_from_pinned_term(
       ~term_data=syntax.term_data,
       ~measured=syntax.measured,
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
  | Manual(ids) => rm_manual(ids, z)
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
  | InfoExp({term: {term: Fun(_pat, body, _, _), _}, _}) =>
    let fun_body_id =
      switch (body.term) {
      | Probe(_) => Id.recover_original(Language.IdTagged.rep_id(body))
      | _ => Language.IdTagged.rep_id(body)
      };
    Move.jump_to_id_indicated(z, fun_body_id);
  | _ => Move.jump_to_id_indicated(z, body_id)
  };
};

let rm_probes_in_selection = (z: Zipper.t): Zipper.t => {
  //TODO: remove repls in selection too?
  let selection_ids = Selection.selection_ids(z.selection);
  let map =
    Id.Map.filter(
      (id, _) => !List.mem(id, selection_ids),
      z.refractors.map,
    );
  Zipper.update_refractors(z, refractors =>
    {
      ...refractors,
      map,
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
