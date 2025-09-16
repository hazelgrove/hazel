open Util;

let mk_probe = (id): option(Base.projector) => {
  open OptUtil.Syntax;
  let kind = ProjectorCore.Kind.Probe;
  let (module P) = ProjectorInit.to_module(kind);
  let seg: Segment.t = [Piece.mk_grout(~id=Id.invalid, Convex)];
  let piece: Base.piece = Segment.parenthesize(seg);
  let* any = MakeTerm.for_projection(seg);
  let+ model = P.init(any);
  ProjectorCore.mk(~id, kind, piece, model);
};

let update_refractors = (f, z: Zipper.t): Zipper.t => {
  ...z,
  refractors: {
    ...z.refractors,
    map: f(z.refractors.map),
  },
};

let add' = (id: Id.t, z: Zipper.t): Zipper.t => {
  switch (Id.Map.find_opt(id, z.refractors.map)) {
  | Some(_) =>
    print_endline("add': removing id: " ++ Id.show(id));
    update_refractors(Id.Map.remove(id), z);
  | None =>
    switch (mk_probe(Id.transform_variant(id))) {
    | None => z
    | Some(p) =>
      print_endline("add': adding id: " ++ Id.show(id));
      update_refractors(Id.Map.add(id, p), z);
    }
  };
};

let smart_add =
    (id: Id.t, info_map: Language.Statics.Map.t, z: Zipper.t)
    : option(Zipper.t) => {
  open OptUtil.Syntax;
  let* ci_body = Language.Statics.Map.lookup(id, info_map);
  switch (ci_body) {
  | InfoExp({term: {term: Fun(pat, body, _, _), _}, _}) =>
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
    let z = add'(body_id, z);
    let z = add'(pat_id, z);
    Some(z);
  | _ =>
    let z = add'(id, z);
    Some(z);
  };
};

let add = (z: Zipper.t): Zipper.t =>
  switch (Indicated.index(z)) {
  | None => z
  | Some(id) => add'(id, z)
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
    | [hd, ..._] => ids_from_term(~term_data, ~measured, hd)
    };
  {
    ...z,
    refractors: {
      ...z.refractors,
      ephemerals:
        List.fold_left(
          (map, id) =>
            switch (mk_probe(Id.transform_variant(id))) {
            | None =>
              print_endline("no probe for " ++ Id.show(id));
              map;
            | Some(p) =>
              // print_endline(
              //   "adding ephemeral refractor "
              //   ++ Id.show(id)
              //   ++ " p.id: "
              //   ++ Id.show(p.id),
              // );
              Id.Map.add(id, p, map)
            },
          Id.Map.empty,
          ids,
        ),
    },
  };
};
