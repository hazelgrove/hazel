open Util;

/* Witness for incremental-eval cache invalidation when probing changes.
 * `ProbeAll` means every probeable subexpression is targeted (settings.probe_all). */
[@deriving (show({with_path: false}), sexp, yojson)]
type probe_targets =
  | ProbeAll
  | ProbeTargets(SubexpProbeTargets.t);

let equal_probe_targets = (a: probe_targets, b: probe_targets): bool =>
  switch (a, b) {
  | (ProbeAll, ProbeAll) => true
  | (ProbeTargets(a), ProbeTargets(b)) => SubexpProbeTargets.equal(a, b)
  | (ProbeAll, ProbeTargets(_))
  | (ProbeTargets(_), ProbeAll) => false
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  elab_term: Exp.t,
  co_ctx: CoCtx.t,
  probe_targets,
};

/* What the proof checker reads, for a program with theorems. Types are
 * normalized: there is no typing context to resolve an alias against
 * where the evaluator runs. */
[@deriving (show({with_path: false}), sexp, yojson)]
type proofs = {
  /* Of each expression and pattern. */
  types: Id.Map.t(Typ.t),
  /* What each pattern covers. */
  covers: Id.Map.t(Coverage.Constraint.t),
  /* The theorems each theorem can cite, by its node's id. */
  namespaces: Id.Map.t(ProofStatics.namespace),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  statics: Id.Map.t(entry),
  proofs: option(proofs),
  targets: Sample.targets /* IDs of expressions/patterns to sample */
};

let empty: t = {
  statics: Id.Map.empty,
  proofs: None,
  targets: Sample.no_targets,
};

let find_opt = (id: Id.t, map: t): option(entry) =>
  Id.Map.find_opt(id, map.statics);

/* Observation and stream publication key only on PROPER program
 * structure — ids of the elaborated program the user wrote. Stepped
 * intermediates (administrative structure: Id.invalid temps, fresh ids
 * minted mid-evaluation) are never program nodes. */
let is_program_node = (id: Id.t, map: t): bool =>
  Id.Map.mem(id, map.statics);

/* Statics presence (not targets) is what enables incr-entry snapshots
 * and reuse_check hits. */
let has_statics = (map: t): bool => !Id.Map.is_empty(map.statics);

/* For callers (CLI, tests) that evaluate without statics. */
let of_targets = (targets: Sample.targets): t => {
  ...empty,
  targets,
};

let of_info_map =
    (~probe_all: bool, ~targets: Sample.targets, info_map: StaticsBase.Map.t)
    : t => {
  let has_theorem =
    Id.Map.exists(
      (_, info) =>
        switch (info) {
        | Info.InfoExp({elab_term: {term: Theorem(_), _}, _}) => true
        | _ => false
        },
      info_map,
    );
  let proofs = () => {
    /* Normalizing copies every alias it expands, so equal types are made
     * one value again, which the worker message then sends once. The
     * copies keep the alias's ids, so they are equal as values. */
    let seen = Stdlib.Hashtbl.create(64);
    let normalize = (ctx, ty) => {
      let ty = Typ.normalize(ctx, ty);
      switch (Stdlib.Hashtbl.find_opt(seen, ty)) {
      | Some(same) => same
      | None =>
        Stdlib.Hashtbl.add(seen, ty, ty);
        ty;
      };
    };
    {
      types:
        Id.Map.filter_map(
          (_, info) =>
            switch (info) {
            | Info.InfoExp({ctx, ty, _})
            | Info.InfoPat({ctx, ty, _}) => Some(normalize(ctx, ty))
            | _ => None
            },
          info_map,
        ),
      covers:
        Id.Map.filter_map(
          (_, info) =>
            switch (info) {
            | Info.InfoPat({constraint_, _}) => Some(constraint_)
            | _ => None
            },
          info_map,
        ),
      namespaces:
        Id.Map.filter_map(
          (_, info) =>
            switch (info) {
            | Info.InfoExp({elab_term: {term: Theorem(_), _}, ctx, _}) =>
              Some(ProofStatics.namespace_of_ctx(ctx))
            | _ => None
            },
          info_map,
        ),
    };
  };
  {
    statics:
      Id.Map.filter_map(
        (_id, info) =>
          switch (info) {
          | Info.InfoExp({elab_term, co_ctx, probe_targets, _}) =>
            Some({
              elab_term,
              co_ctx,
              probe_targets:
                probe_all ? ProbeAll : ProbeTargets(probe_targets),
            })
          | _ => None
          },
        info_map,
      ),
    proofs: has_theorem ? Some(proofs()) : None,
    targets,
  };
};

/* The checker's view of the statics. None for a program without theorems,
 * where there is nothing to check. */
let proof_statics = (map: t): option(ProofStatics.t) =>
  Option.map(
    ~f=
      ({types, covers, namespaces}) =>
        ProofStatics.{
          typ: id => Id.Map.find_opt(id, types),
          namespace: id => Id.Map.find_opt(id, namespaces),
          co_ctx: id =>
            Option.map(
              ~f=(entry: entry) => entry.co_ctx,
              find_opt(id, map),
            ),
          pat_constraint: id => Id.Map.find_opt(id, covers),
        },
    map.proofs,
  );
