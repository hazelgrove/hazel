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
  Id.Map.fold(
    (id, (), acc) => {
      let refs =
        switch (Statics.Map.lookup_exp(id, info_map)) {
        | Some(_) when ambient(id) => []
        | Some(_) => Statics.Map.refs_in(info_map, id)
        | None =>
          switch (Statics.Map.lookup_pat(id, info_map)) {
          | Some(_) when ambient(id) => []
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

/* Extract probe IDs directly from zipper's refractors (manuals + ephemerals).
 * Map values to unit since we only need the IDs as keys. */
let probe_ids_of_zipper = (z: Zipper.t): Id.Map.t(unit) =>
  Id.Map.union(
    (_, _, _) => Some(),
    Id.Map.map(_ => (), Id.Map.of_list(z.refractors.manuals)),
    Id.Map.map(_ => (), z.refractors.multis.ephemerals),
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
    targets,
  };
};

/* Recompute only `targets` from the zipper's current refractors, reusing the
 * existing info_map. Cheap: O(|probe_ids|) fold. Used at the end of
 * Editor.Update.calculate to pick up refractor changes made by probe
 * effects (collision cleanup, auto-probe regen), without redoing statics. */
let with_targets = (~settings: CoreSettings.t, z: Zipper.t, s: t): t => {
  let probe_ids = probe_ids_of_zipper(z);
  let targets = compute_targets(~settings, ~info_map=s.info_map, ~probe_ids);
  {
    ...s,
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
  let probe_ids = probe_ids_of_zipper(z);

  init_from_term(~settings, ~ctx?, ~is_dynamic_term, ~ana?, ~probe_ids, term);
};

/* The zipper the editor's statics were last computed for, and that
   result: a structural (agent) action on the very same zipper can start
   from this map instead of a fresh full pass (CompositionGo). Physical
   identity is the freshness test — an edited zipper is a new value. */
/* statics computed for a zipper by someone who is not the editor (the
   agent tool path checks the program it just produced): offered here so the
   editor's own recompute for that very program can take them instead.
   Keyed by a fingerprint of the program's piece ids (secondaries left out:
   normalization and re-indentation only move whitespace, and the editor
   rebuilds its zipper record on every calculate, so object identity does
   not survive the trip) */
let rec fingerprint_seg = (seg: Segment.t, acc: list(Id.t)): list(Id.t) =>
  List.fold_left(
    (acc, p: Piece.t) =>
      switch (p) {
      | Secondary(_) => acc
      | Grout(g) => [g.id, ...acc]
      | Projector(pr) => [pr.id, ...acc]
      | Tile(t) =>
        List.fold_left(
          (acc, ch) => fingerprint_seg(ch, acc),
          [t.id, ...acc],
          t.children,
        )
      },
    acc,
    seg,
  );
let fingerprint = (z: Zipper.t): list(Id.t) =>
  fingerprint_seg(Zipper.unselect_and_zip(~erase_buffer=true, z), []);
/* the last few inits (other editors and the canvas snapshot also init),
   keyed by program fingerprint */
let last_inits: ref(list((list(Id.t), t))) = ref([]);
let offered: ref(list((list(Id.t), t))) = ref([]);
let offer = (z: Zipper.t, st: t): unit => {
  let fp = fingerprint(z);
  offered := [(fp, st), ...List.filteri((i, _) => i < 3, offered^)];
  /* an editor that takes the offer holds statics that never went through
     init: enter them in the ring too, so the next tool's initial-statics
     reuse (for_zipper) recognizes them */
  last_inits := [(fp, st), ...List.filteri((i, _) => i < 5, last_inits^)];
};
let offered_for = (z: Zipper.t): option(t) =>
  switch (offered^) {
  | [] => None
  | offers =>
    let fp = fingerprint(z);
    switch (List.find_opt(((fp0, _)) => fp0 == fp, offers)) {
    | Some((_, st)) => Some(st)
    | None => None
    };
  };
/* the editor's own statics, when they were computed from the program the
   zipper holds now (the editor rebuilds its zipper record on every
   calculate, so this is a fingerprint match, not identity) */
let for_zipper = (z: Zipper.t, st: t): option(t) =>
  switch (List.find_opt(((_, st0)) => st0 === st, last_inits^)) {
  | Some((fp0, _)) when fp0 == fingerprint(z) => Some(st)
  | _ => None
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
  if (settings.statics) {
    let st =
      init(~settings, ~stitch, ~ctx?, ~is_dynamic_term, ~root, ~ana?, z);
    last_inits :=
      [
        (fingerprint(z), st),
        ...List.filteri((i, _) => i < 5, last_inits^),
      ];
    st;
  } else {
    empty;
  };
