open Util;

/* A ProofMap records, for each id of a proof sub-term encountered during
 * evaluation, the expression going in to the step and the expression
 * coming out.
 *
 * Either expression side is optional: `None` means proof-propagation was broken at
 * this sub-term (e.g. the incoming expression could not be computed
 * because an earlier step failed, or the outgoing expression could not
 * be produced because this step's rule didn't apply).
 *
 * Entries also carry structured proof-checking marks used by the editor
 * and inspector to surface failures at the corresponding proof node.
 *
 * The map is keyed by `Proof.rep_id` of the sub-term. */

[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  incoming: option(Exp.t),
  auto_incoming: list((string, Exp.t)),
  auto_outgoing: list((Exp.t, string)),
  outgoing: option(Exp.t),
  marks: list(ProofMark.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Id.Map.t(entry);

let empty: t = Id.Map.empty;

let add = (id: Id.t, entry: entry, pm: t): t => Id.Map.add(id, entry, pm);

let lookup = (id: Id.t, pm: t): option(entry) => Id.Map.find_opt(id, pm);

let marks_of = (id: Id.t, pm: t): list(ProofMark.t) =>
  switch (lookup(id, pm)) {
  | Some({marks, _}) => marks
  | None => []
  };

let error_ids = (pm: t): list(Id.t) =>
  Id.Map.fold(
    (id, entry, acc) => entry.marks == [] ? acc : [id, ...acc],
    pm,
    [],
  );

let add_marks = (id: Id.t, new_marks: list(ProofMark.t), pm: t): t =>
  switch (lookup(id, pm)) {
  | Some(e) =>
    Id.Map.add(
      id,
      {
        ...e,
        marks: e.marks @ new_marks,
      },
      pm,
    )
  | None =>
    Id.Map.add(
      id,
      {
        incoming: None,
        auto_incoming: [],
        auto_outgoing: [],
        outgoing: None,
        marks: new_marks,
      },
      pm,
    )
  };

/* Union prefers the right-hand side on overlap; used to merge a freshly-
 * produced proof map into the accumulated state. */
let union = (a: t, b: t): t => Id.Map.union((_id, _l, r) => Some(r), a, b);

/* Entries in `after` whose key is absent or bound to a different entry in
 * `before`. Used by StateSlice to capture just the new additions. */
let diff = (~before: t, ~after: t): t =>
  Id.Map.filter(
    (id, entry) =>
      switch (Id.Map.find_opt(id, before)) {
      | None => true
      | Some(prev) => prev != entry
      },
    after,
  );

/* No entry in the proof's subtree carries a mark. The checker recovers
 * from failed steps by passing the goal through unchanged (see
 * `ProofCheck.result_to_outgoing`), so a `true`/`false` outgoing can
 * ride through a broken step — outgoing alone no longer implies the
 * proof holds. */
let rec proof_is_clean = (pm: t, proof: Proof.t): bool =>
  marks_of(Proof.rep_id(proof), pm) == []
  && (
    switch (proof.term) {
    | EmptyHole
    | Invalid(_)
    | MultiHole(_)
    | AxiomStep(_)
    | AlgebriteStep(_)
    | EvalStep(_) => true
    | Seq(p1, p2) => proof_is_clean(pm, p1) && proof_is_clean(pm, p2)
    | Forall(_, body) => proof_is_clean(pm, body)
    | Induction(_, cases) =>
      List.for_all(((_, body)) => proof_is_clean(pm, body), cases)
    }
  );

/* UI status derived from a proof map for a given proof term:
 * - `Some(true)` (proven) iff the proof's outgoing expression is the
 *   literal `true` and no step in the proof is broken.
 * - `Some(false)` (disproven) iff the proof's outgoing expression is the
 *   literal `false` and no step in the proof is broken.
 * - `None` otherwise — incomplete proofs (holes), broken steps (marks in
 *   the subtree), or any other non-boolean outgoing. A
 *   concrete-but-incorrect proof is not a disproof.
 *
 * Mirrors the stepper's expression-validity convention
 * (`true` / `false` / unknown). */
let status_of_proof = (pm: t, proof: Proof.t): option(bool) =>
  switch (lookup(Proof.rep_id(proof), pm)) {
  | Some({outgoing: Some(e), _})
      when
        Exp.fast_equal(e, Exp.temp(Atom(Bool(true))))
        && proof_is_clean(pm, proof) =>
    Some(true)
  | Some({outgoing: Some(e), _})
      when
        Exp.fast_equal(e, Exp.temp(Atom(Bool(false))))
        && proof_is_clean(pm, proof) =>
    Some(false)
  | _ => None
  };
