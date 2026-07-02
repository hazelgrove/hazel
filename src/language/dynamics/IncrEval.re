open Util;

/* Cache used by the incremental evaluator to skip re-evaluating sub-expressions
 * whose elaboration and free-variable dependencies haven't changed since the
 * previous run. Function bodies are a "deferred boundary": ids inside a closure
 * body are never cached (so calling a function twice re-runs the body). */

[@deriving (show({with_path: false}), sexp, yojson)]
type projection =
  | TupleIndex(int, int)
  | ListIndex(int, int)
  | ConsHead
  | ConsTail
  | ConstructorArg(string)
  | TupleLabel(option(string))
  | Ascribed;

[@deriving (show({with_path: false}), sexp, yojson)]
type flag =
  | Clean
  | Dirty;

[@deriving (show({with_path: false}), sexp, yojson)]
type provenance = {
  source: Id.t,
  path: list(projection),
  flag,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type reuse_map = VarMap.t_(provenance);

[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  prev_elab: Exp.t,
  prev_reuse_map: reuse_map,
  prev_probe_targets: option(SubexpProbeTargets.t),
  value: DHExp.t,
  state: StateSlice.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  entries: Id.Map.t(entry),
  /* Ids evaluated from scratch on this run (cache miss). UI tint. */
  recalculated: list(Id.t),
  /* Ids short-circuited via reuse_check (cache hit). Not the complement of
   * `recalculated`: a recalculated parent can still contain reused children. */
  reused: list(Id.t),
};

let empty: t = {
  entries: Id.Map.empty,
  recalculated: [],
  reused: [],
};

let is_empty = (incr: t): bool =>
  Id.Map.is_empty(incr.entries)
  && incr.recalculated == []
  && incr.reused == [];

let add_entry = (id: Id.t, entry: entry, incr: t): t => {
  ...incr,
  entries: Id.Map.add(id, entry, incr.entries),
};

let mark_recalculated = (id: Id.t, incr: t): t => {
  ...incr,
  recalculated: [id, ...incr.recalculated],
};

let mark_reused = (id: Id.t, incr: t): t => {
  ...incr,
  reused: [id, ...incr.reused],
};

/* The set of ids the UI should paint as "frozen" this run.*/
let frozen_ids = (incr: t): list(Id.t) => {
  let acc = ref([]);
  let collect_subtree = (root: Exp.t): unit => {
    let f_exp = (continue, e: Exp.t) => {
      acc := [Exp.rep_id(e), ...acc^];
      continue(e);
    };
    let _ = TermBase.Exp.map_term(~f_exp, root);
    ();
  };
  List.iter(
    id =>
      switch (Id.Map.find_opt(id, incr.entries)) {
      | Some(entry) => collect_subtree(entry.prev_elab)
      | None => acc := [id, ...acc^]
      },
    incr.reused,
  );
  acc^;
};

let equal_provenance = (a: provenance, b: provenance): bool =>
  Id.equal(a.source, b.source) && a.path == b.path && a.flag == b.flag;

let make_clean = (reuse_map: reuse_map): reuse_map =>
  List.map(
    ((name, prov: provenance)) =>
      (
        name,
        {
          ...prov,
          flag: Clean,
        },
      ),
    reuse_map,
  );

let equal_reuse_map = (a: reuse_map, b: reuse_map): bool =>
  List.length(a) == List.length(b)
  && List.for_all(
       ((name, prov)) =>
         switch (VarMap.lookup(b, name)) {
         | Some(prov') => equal_provenance(prov, prov')
         | None => false
         },
       a,
     );

let restrict_to_co_ctx = (reuse_map: reuse_map, co_ctx: CoCtx.t): reuse_map =>
  List.fold_right(
    ((name, _), projected) =>
      switch (VarMap.lookup(reuse_map, name)) {
      | Some(prov) => [(name, prov), ...projected]
      | None => projected
      },
    VarMap.to_list(co_ctx),
    [],
  );

let reuse_map_for_co_ctx =
    (reuse_map: reuse_map, co_ctx: CoCtx.t): option(reuse_map) =>
  List.fold_right(
    ((name, _), acc) =>
      switch (acc) {
      | None => None
      | Some(projected) =>
        switch (VarMap.lookup(reuse_map, name)) {
        | Some(prov) => Some([(name, prov), ...projected])
        | None => None
        }
      },
    VarMap.to_list(co_ctx),
    Some([]),
  );

// For builtins
let clean_reuse_map_of_env = (env: Environment.t(Exp.t)): reuse_map =>
  env
  |> Environment.to_list
  |> List.map(((name, _)) =>
       (
         name,
         {
           source: Id.invalid,
           path: [],
           flag: Clean,
         },
       )
     );

let remove_pat_bindings = (pat: Pat.t, reuse_map: reuse_map): reuse_map => {
  let bound = Pat.bound_vars(pat);
  List.filter(((name, _)) => !List.mem(name, bound), reuse_map);
};

let pat_label = (pat: Pat.t): option(string) =>
  switch (pat.term) {
  | Label(name) => Some(name)
  | _ => None
  };

let pat_provenance = (~source_id: Id.t, ~flag: flag, pat: Pat.t): reuse_map => {
  let rec go = (path: list(projection), pat: Pat.t): reuse_map =>
    switch (pat.term) {
    | EmptyHole
    | MultiHole(_)
    | Wild
    | Invalid(_)
    | Atom(_)
    | Label(_)
    | ExplicitNonlabel
    | Constructor(_) => []
    | Var(name) => [
        (
          name,
          {
            source: source_id,
            path: List.rev(path),
            flag,
          },
        ),
      ]
    | Parens(p)
    | Projector(_, p) => go(path, p)
    | Asc(p, _) => go([Ascribed, ...path], p)
    | TupLabel(label, p) => go([TupleLabel(pat_label(label)), ...path], p)
    | Ap(ctr, p) =>
      switch (Pat.ctr_name(ctr)) {
      | Some(name) => go([ConstructorArg(name), ...path], p)
      | None => go(path, p)
      }
    | Tuple(ps) =>
      let arity = List.length(ps);
      ps
      |> List.mapi((i, p) => go([TupleIndex(arity, i), ...path], p))
      |> List.flatten;
    | ListLit(ps) =>
      let arity = List.length(ps);
      ps
      |> List.mapi((i, p) => go([ListIndex(arity, i), ...path], p))
      |> List.flatten;
    | Cons(hd, tl) =>
      go([ConsHead, ...path], hd) @ go([ConsTail, ...path], tl)
    };
  go([], pat);
};

let with_pat_provenance =
    (~source_id: Id.t, ~flag: flag, pat: Pat.t, reuse_map: reuse_map)
    : reuse_map =>
  pat_provenance(~source_id, ~flag, pat)
  @ remove_pat_bindings(pat, reuse_map);

let was_reused = (id: Id.t, incr: t): bool => List.mem(id, incr.reused);

let update_maps_after_binding =
    (~rhs_reused: bool, ~source_id: Id.t, pat: Pat.t, ~reuse_map: reuse_map)
    : reuse_map => {
  let flag = rhs_reused ? Clean : Dirty;
  with_pat_provenance(~source_id, ~flag, pat, reuse_map);
};

let reuse_check =
    (
      ~call_stack: Sample.call_stack,
      ~prev: t,
      ~reuse_map: reuse_map,
      ~info_map: EvalInfoMap.t,
      ~id: Id.t,
    )
    : option(entry) => {
  open OptUtil.Syntax;

  let* () = OptUtil.some_if(call_stack == [] && !is_empty(prev), ());
  let* entry = Id.Map.find_opt(id, prev.entries);
  let* info = EvalInfoMap.find_opt(id, info_map);

  let elab_same =
    Exp.fast_equal(entry.prev_elab, info.elab_term)
    /* fast_equal ignores annotations; lexeme-only edits (e.g. an unknown
       operator @@ -> @@@) change display and stuck-application semantics,
       so they must invalidate reuse too */
    && Exp.lexeme_trace(entry.prev_elab) == Exp.lexeme_trace(info.elab_term);
  let* () = OptUtil.some_if(elab_same, ());

  let* current_reuse_map = reuse_map_for_co_ctx(reuse_map, info.co_ctx);
  let* () =
    OptUtil.some_if(
      equal_reuse_map(entry.prev_reuse_map, current_reuse_map),
      (),
    );

  let* () =
    OptUtil.some_if(
      Option.equal(
        SubexpProbeTargets.equal,
        entry.prev_probe_targets,
        info.probe_targets,
      ),
      (),
    );

  Some(entry);
};
