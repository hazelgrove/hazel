open Util;
open Language;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  term: Exp.t,
  elaborated: Exp.t,
  info_map: Statics.Map.t,
  error_ids: list(Id.t),
  warning_ids: list(Id.t),
  completion: option(MakeTerm.completion_snapshot),
  targets: Sample.targets, /* Maps expr/pat IDs to capture specs for sampling */
  /* The zipper's own probe pins these statics were computed for: what a
     probe change is judged against. Not [targets]' keys, which also hold
     ids a pin never names (dynamics-requesting projectors, a livelit use's
     model argument), so comparing pins to them read as a change on every
     frame and re-ran whole-program statics each time a result streamed. */
  pins: Id.Map.t(unit),
};

let empty: t = {
  term: {
    term: Tuple([]),
    annotation: IdTagged.IdTag.temp,
  },
  elaborated: {
    term: Tuple([]),
    annotation: IdTagged.IdTag.temp,
  },
  info_map: Id.Map.empty,
  error_ids: [],
  warning_ids: [],
  completion: None,
  targets: Sample.no_targets,
  pins: Id.Map.empty,
};

let dh_err = (error: string): DHExp.t => Var(error) |> DHExp.fresh;

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
  /* AMBIENT sites (probe_all, not an explicit probe) capture no
     environment: only the probe context menu displays it, and a
     sample's env copy of the enclosing bindings was ~80% of the
     retained memory (each of a site's samples ships its own copy of
     every bound list and view). Explicit probes keep their env. */
  let ambient = id => settings.probe_all && !Id.Map.mem(id, probe_ids);
  /* The model argument of a projected livelit use, whose value the commit
     path reads when writing a ^name.update(model, action) transition */
  let rec livelit_model = (e: Exp.t): option(Id.t) =>
    switch (e.term) {
    | Parens(e) => livelit_model(e)
    | Ap(_, {term: LivelitName(_), _}, model) => Some(Exp.rep_id(model))
    | _ => None
    };
  Id.Map.fold(
    (id, (), acc) => {
      let entries =
        switch (Statics.Map.lookup_exp(id, info_map)) {
        /* A livelit projector's samples are read only for their value;
           its refs would capture the whole livelit record per sample.
           Also target the model argument, for the commit path. */
        | Some({
            user_term: {term: Projector({kind: Livelit, _}, inner), _},
            _,
          }) =>
          let model =
            switch (livelit_model(inner)) {
            | Some(model_id) => [(model_id, {Sample.refs: []})]
            | None => []
            };
          [(id, {Sample.refs: []}), ...model];
        | Some(_) when ambient(id) => [(id, {refs: []})]
        | Some(_) => [(id, {refs: Statics.Map.refs_in(info_map, id)})]
        | None =>
          switch (Statics.Map.lookup_pat(id, info_map)) {
          | Some(_) when ambient(id) => [(id, {refs: []})]
          | Some(_) => [(id, {refs: Statics.Map.bound_in(info_map, id)})]
          | None => [(id, {refs: []})]
          }
        };
      List.fold_left(
        (acc, (id, spec)) => Id.Map.add(id, spec, acc),
        acc,
        entries,
      );
    },
    effective_probe_ids,
    Id.Map.empty,
  );
};

/* Ids of projectors which opt into dynamic information (Projector.dynamics).
 * Probing a projector's term id is what populates its `info.dynamics`, so
 * such a projector can see the live value of the syntax it replaces. */
let projector_probe_ids =
    (projectors: Id.Map.t(Base.projector)): Id.Map.t(unit) =>
  Id.Map.fold(
    (id, p: Base.projector, acc) => {
      let (module P) = ProjectorInit.to_module(p.kind);
      P.dynamics ? Id.Map.add(id, (), acc) : acc;
    },
    projectors,
    Id.Map.empty,
  );

/* Extract probe IDs directly from zipper's refractors (manuals + ephemerals),
 * plus the ids of any dynamics-requesting projectors.
 * Map values to unit since we only need the IDs as keys. */
let probe_ids_of_zipper =
    (~projectors=Id.Map.empty, z: Zipper.t): Id.Map.t(unit) =>
  Id.Map.union(
    (_, _, _) => Some(),
    Id.Map.union(
      (_, _, _) => Some(),
      Id.Map.map(_ => (), Id.Map.of_list(z.refractors.manuals)),
      Id.Map.map(_ => (), z.refractors.multis.ephemerals),
    ),
    projector_probe_ids(projectors),
  );

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
  let (info_map, elaborated) =
    Statics.mk(~ana?, ~probe_ids, settings, ctx_init, term);
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
    completion: None,
    targets,
    pins: Id.Map.empty,
  };
};

/* Recompute only `targets` from the zipper's current refractors, reusing the
 * existing info_map. Cheap: O(|probe_ids|) fold. Used at the end of
 * Editor.Update.calculate to pick up refractor changes made by probe
 * effects (collision cleanup, auto-probe regen), without redoing statics. */
let with_targets =
    (~settings: CoreSettings.t, ~projectors=Id.Map.empty, z: Zipper.t, s: t)
    : t => {
  let probe_ids = probe_ids_of_zipper(~projectors, z);
  let targets = compute_targets(~settings, ~info_map=s.info_map, ~probe_ids);
  {
    ...s,
    targets,
    /* the targets now follow the zipper's current pins */
    pins: probe_ids_of_zipper(z),
  };
};

/* Small handoff cache for top-level agent edits. Compare the actual syntax,
   settings and probe IDs: IDs alone do not establish freshness. Full syntax
   deliberately includes whitespace, which can affect incomplete terms. */
type cache_entry = {
  settings: CoreSettings.t,
  source: Segment.t,
  probes: Id.Map.t(unit),
  statics: t,
};
let last_inits: ref(list(cache_entry)) = ref([]);
let offered: ref(list(cache_entry)) = ref([]);
let entry = (~settings, z: Zipper.t, statics: t): cache_entry => {
  settings,
  source: Zipper.unselect_and_zip(~erase_buffer=true, z),
  probes: probe_ids_of_zipper(z),
  statics,
};
let matches = (~settings, z: Zipper.t, e: cache_entry): bool =>
  settings == e.settings
  && Id.Map.equal((==), probe_ids_of_zipper(z), e.probes)
  && compare(Zipper.unselect_and_zip(~erase_buffer=true, z), e.source) == 0;
let remember = e =>
  last_inits := [e, ...List.filteri((i, _) => i < 5, last_inits^)];
let offer = (~settings, z: Zipper.t, st: t): unit => {
  let e = entry(~settings, z, st);
  offered := [e, ...List.filteri((i, _) => i < 3, offered^)];
  remember(e);
};
let offered_for = (~settings, z: Zipper.t): option(t) =>
  List.find_opt(matches(~settings, z), offered^)
  |> Option.map(e => e.statics);
let for_zipper = (~settings, z: Zipper.t, st: t): option(t) =>
  List.find_opt(
    e => e.statics.info_map === st.info_map && matches(~settings, z, e),
    last_inits^,
  )
  |> Option.map(_ => st);

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
  let (make_term_result, completion) =
    MakeTerm.from_zip_for_sem_with_completion(z, ~root);
  let term = make_term_result.term |> stitch;
  let probe_ids =
    probe_ids_of_zipper(~projectors=make_term_result.projectors, z);

  let st = {
    ...
      init_from_term(
        ~settings,
        ~ctx?,
        ~is_dynamic_term,
        ~ana?,
        ~probe_ids,
        term,
      ),
    completion: Some(completion),
    pins: probe_ids_of_zipper(z),
  };
  /* The agent's handoff is only valid for the ordinary, unstitched Exp
     editor. Contextual/analysis editors compute their own statics. */
  if (!is_dynamic_term
      && root == Sort.Exp
      && ctx == None
      && ana == None
      && term === make_term_result.term) {
    remember(entry(~settings, z, st));
  };
  st;
};

let init =
    (
      ~settings: CoreSettings.t,
      ~is_dynamic_term,
      ~stitch,
      ~root,
      ~ctx=?,
      ~ana=?,
      z: Zipper.t,
    ) =>
  settings.statics
    ? init(~settings, ~stitch, ~ctx?, ~is_dynamic_term, ~root, ~ana?, z)
    : empty;
