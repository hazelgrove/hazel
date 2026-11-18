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

/* StringMap rather than VarMap: remove_pat_bindings runs at every binder
 * over a map seeded with the whole builtins env, so lookups/removals must
 * not walk an assoc list. */
[@deriving (show({with_path: false}), sexp, yojson)]
type reuse_map = Maps.StringMap.t(provenance);

let empty_reuse_map: reuse_map = Maps.StringMap.empty;

[@deriving (show({with_path: false}), sexp, yojson)]
type entry('state) = {
  prev_elab: Exp.t,
  prev_reuse_map: reuse_map,
  prev_probe_targets: EvalInfo.probe_targets,
  value: DHExp.t,
  state: 'state,
};

/* What a theorem's check depends on besides the values its proof reads:
 * the theorem, with its body dropped, the theorems it can cite and their
 * verdicts. */
[@deriving (show({with_path: false}), sexp, yojson)]
type proof_key = {
  theorem: Exp.t,
  namespace: ProofStatics.namespace,
  verdicts: list((Id.t, SemanticCtx.verdict)),
};

/* A theorem's proof as checked, by the theorem's id. Unlike the theorem's
 * own entry it doesn't depend on the theorem's body, which holds every
 * later theorem. */
[@deriving (show({with_path: false}), sexp, yojson)]
type checked_proof = {
  key: proof_key,
  /* The provenance of the variables the theorem names, when evaluation
   * last reached it rather than replaying it. */
  prev_reuse_map: option(reuse_map),
  proof_map: ProofMap.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t('state) = {
  entries: Id.Map.t(entry('state)),
  proofs: Id.Map.t(checked_proof),
  /* Steps of the evaluation that produced this cache. */
  steps: int,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type current('state) = {
  id: Id.t,
  state: 'state,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type outbox('state) = {
  completed: t('state),
  current: option(current('state)),
};

let empty: t('state) = {
  entries: Id.Map.empty,
  proofs: Id.Map.empty,
  steps: 0,
};

let empty_outbox: outbox('state) = {
  completed: empty,
  current: None,
};

let outbox_of_completed = (completed: t('state)): outbox('state) => {
  completed,
  current: None,
};

let is_empty = (incr: t('state)): bool =>
  Id.Map.is_empty(incr.entries) && Id.Map.is_empty(incr.proofs);

let outbox_is_empty = (outbox: outbox('state)): bool =>
  is_empty(outbox.completed) && Option.is_none(outbox.current);

let add_entry =
    (id: Id.t, entry: entry('state), incr: t('state)): t('state) => {
  ...incr,
  entries: Id.Map.add(id, entry, incr.entries),
};

let add_outbox_entry =
    (id: Id.t, entry: entry('state), outbox: outbox('state))
    : outbox('state) => {
  ...outbox,
  completed: add_entry(id, entry, outbox.completed),
};

let set_outbox_current =
    (~id: Id.t, ~state: 'state, outbox: outbox('state)): outbox('state) => {
  ...outbox,
  current:
    Some({
      id,
      state,
    }),
};

let add_stream = (stream: t('state), incr: t('state)): t('state) => {
  ...incr,
  entries:
    Id.Map.union(
      (_, _old, new_) => Some(new_),
      incr.entries,
      stream.entries,
    ),
  proofs:
    Id.Map.union((_, _old, new_) => Some(new_), incr.proofs, stream.proofs),
};

let merge_outbox =
    (stream: outbox('state), outbox: outbox('state)): outbox('state) => {
  completed: add_stream(stream.completed, outbox.completed),
  /* A slice that only finished completed entries (or only stepped through
   * non-program ids) may omit current. Keep the prior in-flight publish so
   * mid-stream UI state does not flicker away between slices. */
  current:
    switch (stream.current) {
    | Some(_) as current => current
    | None => outbox.current
    },
};

let copy_descendant_entries =
    (~root_id: Id.t, ~root: Exp.t, ~prev: t('state), incr: t('state))
    : t('state) => {
  let acc = ref(incr);
  let f_exp = (continue, e: Exp.t): Exp.t => {
    let sub_id = Exp.rep_id(e);
    if (!Id.equal(sub_id, root_id)) {
      switch (Id.Map.find_opt(sub_id, prev.entries)) {
      | Some(sub_entry) => acc := add_entry(sub_id, sub_entry, acc^)
      | None => ()
      };
    };
    continue(e);
  };
  let _ = TermBase.Exp.map_term(~f_exp, root);
  acc^;
};

/* Surface ids covered by cache entries: each entry short-circuits a subtree,
 * so expand via prev_elab rather than using only the map keys. Used by the
 * pending-eval worklist (to drop settled ids) and by the frozen debug tint
 * (to paint a reuse prediction). */
let visible_ids = (incr: t('state)): list(Id.t) => {
  let acc = ref([]);
  let collect_subtree = (root: Exp.t): unit => {
    let f_exp = (continue, e: Exp.t): Exp.t => {
      acc := [Exp.rep_id(e), ...acc^];
      continue(e);
    };
    let _ = TermBase.Exp.map_term(~f_exp, root);
    ();
  };
  Id.Map.iter((_, entry) => collect_subtree(entry.prev_elab), incr.entries);
  acc^;
};

/* Ids the UI should paint as "frozen" for a reuse plan / prediction. */
let frozen_ids = (~incr: t('state)): list(Id.t) => visible_ids(incr);

let equal_provenance = (a: provenance, b: provenance): bool =>
  Id.equal(a.source, b.source)
  && Poly.equal(a.path, b.path)
  && Poly.equal(a.flag, b.flag);

let make_clean = (reuse_map: reuse_map): reuse_map =>
  Maps.StringMap.map(
    (prov: provenance) =>
      {
        ...prov,
        flag: Clean,
      },
    reuse_map,
  );

let equal_reuse_map = (a: reuse_map, b: reuse_map): bool =>
  Maps.StringMap.equal(equal_provenance, a, b);

/* `$hole` is a statics-only sentinel for unused-variable warnings. It is not
 * a runtime dependency, so it should not participate in reuse provenance. */
let is_runtime_dependency = (name: string): bool =>
  !String.equal(name, "$hole");

let restrict_to_names = (reuse_map: reuse_map, names: list(Var.t)): reuse_map =>
  List.fold_left(
    ~f=
      (projected, name) =>
        if (!is_runtime_dependency(name)) {
          projected;
        } else {
          switch (Maps.StringMap.find_opt(name, reuse_map)) {
          | Some(prov) => Maps.StringMap.add(name, prov, projected)
          | None => projected
          };
        },
    ~init=empty_reuse_map,
    names,
  );

let restrict_to_co_ctx = (reuse_map: reuse_map, co_ctx: CoCtx.t): reuse_map =>
  restrict_to_names(reuse_map, List.map(~f=fst, VarMap.to_list(co_ctx)));

let reuse_map_for_co_ctx =
    (reuse_map: reuse_map, co_ctx: CoCtx.t): option(reuse_map) =>
  List.fold_left(
    ~f=
      (acc, (name, _)) =>
        if (!is_runtime_dependency(name)) {
          acc;
        } else {
          switch (acc) {
          | None => None
          | Some(projected) =>
            switch (Maps.StringMap.find_opt(name, reuse_map)) {
            | Some(prov) => Some(Maps.StringMap.add(name, prov, projected))
            | None => None
            }
          };
        },
    ~init=Some(empty_reuse_map),
    VarMap.to_list(co_ctx),
  );

// For builtins
let clean_reuse_map_of_env = (env: Environment.t(Exp.t)): reuse_map =>
  env
  |> Environment.to_list
  |> List.fold_left(
       ~f=
         (acc, (name, _)) =>
           Maps.StringMap.add(
             name,
             {
               source: Id.invalid,
               path: [],
               flag: Clean,
             },
             acc,
           ),
       ~init=empty_reuse_map,
     );

let remove_pat_bindings = (pat: Pat.t, reuse_map: reuse_map): reuse_map =>
  List.fold_left(
    ~f=(acc, name) => Maps.StringMap.remove(name, acc),
    ~init=reuse_map,
    Pat.bound_vars(pat),
  );

let pat_label = (pat: Pat.t): option(string) =>
  switch (pat.term) {
  | Label(name) => Some(name)
  | _ => None
  };

let pat_provenance = (~source_id: Id.t, ~flag: flag, pat: Pat.t): reuse_map => {
  let rec go =
          (path: list(projection), pat: Pat.t): list((string, provenance)) =>
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
      |> List.mapi(~f=(i, p) => go([TupleIndex(arity, i), ...path], p))
      |> List.concat;
    | ListLit(ps) =>
      let arity = List.length(ps);
      ps
      |> List.mapi(~f=(i, p) => go([ListIndex(arity, i), ...path], p))
      |> List.concat;
    | Cons(hd, tl) =>
      go([ConsHead, ...path], hd) @ go([ConsTail, ...path], tl)
    /* `m` in `m + 1` is a number computed from the source, not a part of
       it, so it has no provenance and isn't reused. */
    | Add(_) => []
    };
  go([], pat) |> Maps.StringMap.of_list;
};

let with_pat_provenance =
    (~source_id: Id.t, ~flag: flag, pat: Pat.t, reuse_map: reuse_map)
    : reuse_map =>
  /* Domains are disjoint: remove_pat_bindings removes every name the
   * pattern binds, a superset of those pat_provenance produces. Prefer
   * the pattern's entry regardless. */
  Maps.StringMap.union(
    (_name, from_pat, _outer) => Some(from_pat),
    pat_provenance(~source_id, ~flag, pat),
    remove_pat_bindings(pat, reuse_map),
  );

let update_maps_after_binding =
    (~rhs_reused: bool, ~source_id: Id.t, pat: Pat.t, ~reuse_map: reuse_map)
    : reuse_map => {
  let flag = rhs_reused ? Clean : Dirty;
  with_pat_provenance(~source_id, ~flag, pat, reuse_map);
};

let reuse_check =
    (
      ~call_stack: CallStack.t,
      ~prev: t('state),
      ~reuse_map: reuse_map,
      ~eval_info: EvalInfo.t,
      ~id: Id.t,
    )
    : option(entry('state)) => {
  open OptUtil.Syntax;

  let* () =
    OptUtil.some_if(List.is_empty(call_stack) && !is_empty(prev), ());
  let* entry = Id.Map.find_opt(id, prev.entries);
  let* info = EvalInfo.find_opt(id, eval_info);

  let* current_reuse_map = reuse_map_for_co_ctx(reuse_map, info.co_ctx);
  let* () =
    OptUtil.some_if(
      equal_reuse_map(entry.prev_reuse_map, current_reuse_map),
      (),
    );

  let elab_same =
    Exp.fast_equal(entry.prev_elab, info.elab_term)
    /* fast_equal ignores annotations; lexeme-only edits (e.g. an unknown
       operator @@ -> @@@) change display and stuck-application semantics,
       so they must invalidate reuse too */
    && List.equal(
         Option.equal(String.equal),
         Exp.lexeme_trace(entry.prev_elab),
         Exp.lexeme_trace(info.elab_term),
       );
  let* () = OptUtil.some_if(elab_same, ());

  let* () =
    OptUtil.some_if(
      EvalInfo.equal_probe_targets(
        entry.prev_probe_targets,
        info.probe_targets,
      ),
      (),
    );

  Some(entry);
};

/* Reusing a theorem's checked proof. It is reused while its key is the
 * same, the theorem's ids included, since the proof map is keyed by them,
 * and the values the theorem names are: where evaluation reached it, they
 * have the same provenance. A theorem replayed from a reused node has
 * none to compare, but the node was reused only because they are the
 * same. */
module CheckedProofs = {
  /* What evaluation found where it reached a theorem: the provenance of
   * the variables it names, or, inside a function call, nothing, since
   * the theorem is checked afresh each time. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type input =
    | Reached(reuse_map)
    | InCall;

  /* Every variable name in the theorem, bound or free, as names it might
   * read from the env. */
  let reached = (reuse_map: reuse_map, theorem: Exp.t): input => {
    let names = ref([]);
    let _: Exp.t =
      Exp.map_term(
        ~f_exp=
          (continue, e) => {
            switch (Exp.term_of(e)) {
            | Var(x) => names := [x, ...names^]
            | _ => ()
            };
            continue(e);
          },
        theorem,
      );
    Reached(restrict_to_names(reuse_map, names^));
  };

  let key = (~namespace, ~verdicts, theorem: Exp.t): proof_key => {
    theorem:
      switch (theorem.term) {
      | Theorem(p, goal, proof, _) => {
          ...theorem,
          term: Theorem(p, goal, proof, Exp.temp(EmptyHole)),
        }
      | _ => theorem
      },
    namespace,
    verdicts,
  };

  let same_key = (a: proof_key, b: proof_key): bool => {
    let ids = (e: Exp.t): list(Id.t) => {
      let acc = ref([]);
      let f = (continue, t: IdTagged.t(_)) => {
        acc := [IdTagged.rep_id(t), ...acc^];
        continue(t);
      };
      let _: Exp.t =
        Exp.map_term(
          ~f_exp=f,
          ~f_pat=f,
          ~f_typ=f,
          ~f_tpat=f,
          ~f_rul=f,
          ~f_proof=f,
          e,
        );
      acc^;
    };
    Exp.fast_equal_with_lexemes(a.theorem, b.theorem)
    && List.equal(Id.equal, ids(a.theorem), ids(b.theorem))
    && TheoremNamespace.same(a.namespace, b.namespace)
    && Poly.equal(a.verdicts, b.verdicts);
  };

  let find =
      (~prev: t('state), ~input: option(input), id: Id.t, key: proof_key)
      : option(ProofMap.t) =>
    switch (Id.Map.find_opt(id, prev.proofs), input) {
    | (Some(checked), None) when same_key(checked.key, key) =>
      Some(checked.proof_map)
    | (
        Some({prev_reuse_map: Some(prev_reuse_map), _} as checked),
        Some(Reached(reuse_map)),
      )
        when
          same_key(checked.key, key)
          && equal_reuse_map(prev_reuse_map, reuse_map) =>
      Some(checked.proof_map)
    | (Some(_), _)
    | (None, _) => None
    };

  /* The entry to cache for a theorem checked as `proof_map`. */
  let entry =
      (
        ~prev: t('state),
        ~input: option(input),
        id: Id.t,
        key: proof_key,
        proof_map: ProofMap.t,
      )
      : option(checked_proof) =>
    switch (input) {
    | Some(InCall) => None
    | Some(Reached(reuse_map)) =>
      Some({
        key,
        prev_reuse_map: Some(make_clean(reuse_map)),
        proof_map,
      })
    | None =>
      Some({
        key,
        prev_reuse_map:
          Option.bind(Id.Map.find_opt(id, prev.proofs), ~f=checked =>
            checked.prev_reuse_map
          ),
        proof_map,
      })
    };
};
