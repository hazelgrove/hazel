open Util;
open Language;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  term: Exp.t,
  elaborated: Exp.t,
  info_map: Statics.Map.t,
  error_ids: list(Id.t),
  warning_ids: list(Id.t),
  targets: Sample.targets /* Maps expr/pat IDs to capture specs for sampling */
};

let empty: t = {
  term: {
    term: Tuple([]),
    annotation: IdTagged.IdTag.temp(),
  },
  elaborated: {
    term: Tuple([]),
    annotation: IdTagged.IdTag.temp(),
  },
  info_map: Id.Map.empty,
  error_ids: [],
  warning_ids: [],
  targets: Sample.no_targets,
};

let dh_err = (error: string): DHExp.t => Var(error) |> DHExp.fresh;

let rec spine_tail = (e: Exp.t): Exp.t =>
  switch (e.term) {
  | Let(_, _, body)
  | TyAlias(_, _, body)
  | Seq(_, body)
  | ModuleExp(_, _, body) => spine_tail(body)
  | _ => e
  };

let result_ctx = (statics: t): Ctx.t => {
  let base = Builtins.ctx_init(None);
  switch (
    Statics.Map.ctx_of(
      Exp.rep_id(spine_tail(statics.term)),
      statics.info_map,
    )
  ) {
  | None => base
  | Some(ctx) =>
    let erase = t => Typ.unfold_nominals(ctx, Typ.normalize(ctx, t));
    {
      ...base,
      entries:
        List.filter_map(
          (entry: Ctx.entry) =>
            switch (entry) {
            | TVarEntry({kind: Singleton(t), _} as v) =>
              Some(
                Ctx.TVarEntry({
                  ...v,
                  kind: Singleton(erase(t)),
                }),
              )
            | TVarEntry(_) => Some(entry)
            | ConstructorEntry(c) =>
              Some(
                Ctx.ConstructorEntry({
                  ...c,
                  typ: erase(c.typ),
                }),
              )
            | _ => None
            },
          ctx.entries,
        )
        @ base.entries,
    };
  };
};

let erase_unbound_applications = (ctx: Ctx.t, ty: Typ.t): Typ.t =>
  Typ.map_term(
    ~f_typ=
      (continue, t: Typ.t) =>
        switch (t.term) {
        | TypParamAp({term: Var(head), _}, _)
            when Ctx.lookup_tvar(ctx, head) == None => {
            ...t,
            term: Unknown(Internal),
          }
        | _ => continue(t)
        },
    ty,
  );

let erase_free_in_typ = (ctx: Ctx.t, ty: Typ.t): Typ.t => {
  let ty = erase_unbound_applications(ctx, ty);
  List.fold_left(
    (ty, name) =>
      Ctx.lookup_tvar(ctx, name) == None
        ? Typ.subst(
            Unknown(Internal) |> Typ.temp,
            Var(name) |> TPat.fresh,
            ty,
          )
        : ty,
    ty,
    List.sort_uniq(compare, Typ.free_vars(ty)),
  );
};

let rec erase_free_tvars = (ctx: Ctx.t, exp: Exp.t): Exp.t =>
  Exp.map_term(
    ~f_pat=
      (continue, p: Pat.t) =>
        switch (p.term) {
        | Constructor(c, Some(Some({term: Poly(_), _}))) => {
            ...p,
            term: Constructor(c, None),
          }
        | Constructor(c, Some(Some(t))) => {
            ...p,
            term: Constructor(c, Some(Some(erase_free_in_typ(ctx, t)))),
          }
        | _ => continue(p)
        },
    ~f_typ=(_, ty) => erase_free_in_typ(ctx, ty),
    ~f_exp=
      (continue, e: Exp.t) =>
        switch (e.term) {
        | TypAbs(tp, body, name) => {
            ...e,
            term:
              TypAbs(
                tp,
                erase_free_tvars(Ctx.extend_dummy_tvar(ctx, tp), body),
                name,
              ),
          }
        | TyAlias(tp, ty, body) => {
            ...e,
            term:
              TyAlias(
                tp,
                erase_free_in_typ(ctx, ty),
                erase_free_tvars(Ctx.extend_dummy_tvar(ctx, tp), body),
              ),
          }
        | Constructor(c, Some(Some({term: Poly(_), _}))) => {
            ...e,
            term: Constructor(c, None),
          }
        | Constructor(c, Some(Some(t))) => {
            ...e,
            term: Constructor(c, Some(Some(erase_free_in_typ(ctx, t)))),
          }
        | _ => continue(e)
        },
    exp,
  );

/* Predicate for whether a term should be probed when ProbeAll is on.
 * Currently const true - probes all expressions / patterns */
let should_probe = (info: Info.t): bool =>
  switch (info) {
  | InfoExp(_)
  | InfoPat(_) => true
  | _ => false
  };

/* Collect all expression and pattern IDs from info_map that pass the should_probe predicate. */
let all_probeable_ids = (info_map: Statics.Map.t): Id.Map.t(unit) =>
  Id.Map.fold(
    (id, info, acc) => should_probe(info) ? Id.Map.add(id, (), acc) : acc,
    info_map,
    Id.Map.empty,
  );

/* Compute targets from probe_ids. For each ID, determine whether it's
 * an expression or pattern target, then look up the appropriate refs to capture.
 * When probe_all is enabled, we target everything in info_map that passes
 * should_probe, ignoring the passed probe_ids (which are a subset anyway). */
let compute_targets =
    (
      ~settings: CoreSettings.t,
      ~info_map: Statics.Map.t,
      ~probe_ids: Id.Map.t(unit),
    )
    : Sample.targets => {
  let effective_probe_ids =
    settings.probe_all ? all_probeable_ids(info_map) : probe_ids;
  Id.Map.fold(
    (id, (), acc) => {
      let refs =
        switch (Statics.Map.lookup_exp(id, info_map)) {
        | Some(_) => Statics.Map.refs_in(info_map, id)
        | None =>
          switch (Statics.Map.lookup_pat(id, info_map)) {
          | Some(_) => Statics.Map.bound_in(info_map, id)
          | None => []
          }
        };
      let spec: Sample.capture_spec = {refs: refs};
      Id.Map.add(id, spec, acc);
    },
    effective_probe_ids,
    Id.Map.empty,
  );
};

let init_from_term =
    (
      ~settings,
      ~is_dynamic_term,
      ~ctx=?,
      ~ana=?,
      ~probe_ids=Id.Map.empty,
      term,
    )
    : t => {
  let ctx_init =
    Option.value(
      ~default=Builtins.ctx_init(is_dynamic_term ? None : Some(Int)),
      ctx,
    );
  let (info_map, elaborated) = Statics.mk(~ana?, settings, ctx_init, term);
  let error_ids = Statics.Map.error_ids(info_map);
  let warning_ids = Statics.Map.warning_ids(info_map);
  let elaborated =
    switch () {
    | _ when !settings.statics => dh_err("Statics disabled")
    | _ when !settings.dynamics && !settings.elaborate =>
      dh_err("Dynamics & Elaboration disabled")
    | _ => elaborated
    };
  let targets = compute_targets(~settings, ~info_map, ~probe_ids);
  {
    term,
    elaborated,
    info_map,
    error_ids,
    warning_ids,
    targets,
  };
};

let init =
    (
      ~settings: CoreSettings.t,
      ~is_dynamic_term,
      ~stitch,
      ~ctx=?,
      ~root,
      ~ana=?,
      z: Zipper.t,
    )
    : t => {
  let make_term_result = MakeTerm.from_zip_for_sem(z, ~root);
  let term = make_term_result.term |> stitch;
  /* Extract probe IDs directly from zipper's refractors (manuals + ephemerals).
   * Map values to unit since we only need the IDs as keys. */
  let probe_ids =
    Id.Map.union(
      (_, _, _) => Some(),
      Id.Map.map(_ => (), Id.Map.of_list(z.refractors.manuals)),
      Id.Map.map(_ => (), z.refractors.multis.ephemerals),
    );

  init_from_term(~settings, ~ctx?, ~is_dynamic_term, ~ana?, ~probe_ids, term);
};

let init_typ = (~settings: CoreSettings.t, ~ctx=?, ~root, z: Zipper.t): t => {
  let typ = MakeTerm.from_zip_typ_for_sem(z, ~root);
  let ctx_init = Option.value(~default=Builtins.ctx_init(Some(Int)), ctx);
  let info_map = Statics.mk_typ(settings, ctx_init, typ);
  {
    ...empty,
    info_map,
    error_ids: Statics.Map.error_ids(info_map),
    warning_ids: Statics.Map.warning_ids(info_map),
  };
};

let init =
    (
      ~settings: CoreSettings.t,
      ~is_dynamic_term,
      ~stitch,
      ~ctx=?,
      ~root,
      ~ana=?,
      z: Zipper.t,
    ) =>
  switch (root) {
  | Sort.Typ => init_typ(~settings, ~ctx?, ~root, z)
  | _ =>
    settings.statics
      ? init(~settings, ~stitch, ~ctx?, ~is_dynamic_term, ~root, ~ana?, z)
      : empty
  };
