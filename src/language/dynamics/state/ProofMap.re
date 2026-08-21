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
  /* Receipt for a `contradiction` step: the variable equations the
   * checker substituted into the cited fact before evaluating it, in the
   * order applied. Empty for every other step. */
  substitutions: list((string, Exp.t)),
  marks: list(ProofMark.t),
  /* Obligations incurred by this step (e.g. an `assume`'s hypothesis),
   * with their discharge provenance. Empty for most steps. */
  obligations: list(Obligation.t),
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
        substitutions: [],
        marks: new_marks,
        obligations: [],
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
    | Forall(_, body)
    | Assume(_, _, body)
    | Generalize(_, body)
    | Alias(_, _, body)
    | Revert(_, _, body) => proof_is_clean(pm, body)
    | Contradiction(_) => true
    /* A `have` is clean only if its attached subproof is: an unfinished
     * subproof leaves the have's obligation pending, but a BROKEN one
     * must not read as a clean proof (docs/prover-obligations.md §3.3). */
    | Have(_, sub, body) =>
      proof_is_clean(pm, sub) && proof_is_clean(pm, body)
    | Induction(_, _, cases) =>
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

/* All obligations recorded in the proof subtree, in proof-term order. */
let rec obligations_of_proof = (pm: t, proof: Proof.t): list(Obligation.t) => {
  let here =
    switch (lookup(Proof.rep_id(proof), pm)) {
    | Some({obligations, _}) => obligations
    | None => []
    };
  here
  @ (
    switch (proof.term) {
    | EmptyHole
    | Invalid(_)
    | MultiHole(_)
    | AxiomStep(_)
    | AlgebriteStep(_)
    | EvalStep(_) => []
    | Seq(p1, p2) =>
      obligations_of_proof(pm, p1) @ obligations_of_proof(pm, p2)
    | Forall(_, body)
    | Assume(_, _, body)
    | Generalize(_, body)
    | Alias(_, _, body)
    | Revert(_, _, body) => obligations_of_proof(pm, body)
    | Contradiction(_) => []
    | Have(_, sub, body) =>
      obligations_of_proof(pm, sub) @ obligations_of_proof(pm, body)
    | Induction(_, _, cases) =>
      List.concat_map(((_, body)) => obligations_of_proof(pm, body), cases)
    }
  );
};

/* Every id at which a step of this proof term is recorded. Used to tell
 * proof entries from the definition-time entries below. */
let rec rep_ids_of_proof = (proof: Proof.t): list(Id.t) =>
  [Proof.rep_id(proof)]
  @ (
    switch (proof.term) {
    | EmptyHole
    | Invalid(_)
    | MultiHole(_)
    | AxiomStep(_)
    | AlgebriteStep(_)
    | EvalStep(_) => []
    | Seq(p1, p2) => rep_ids_of_proof(p1) @ rep_ids_of_proof(p2)
    | Forall(_, body)
    | Assume(_, _, body)
    | Generalize(_, body)
    | Alias(_, _, body)
    | Revert(_, _, body) => rep_ids_of_proof(body)
    | Contradiction(_) => []
    | Have(_, sub, body) => rep_ids_of_proof(sub) @ rep_ids_of_proof(body)
    | Induction(_, _, cases) =>
      List.concat_map(((_, body)) => rep_ids_of_proof(body), cases)
    }
  );

/* Definition-time obligations (docs/prover-obligations.md §2.2, produced
 * by `ProofCheck.definition_obligations`) live in this same map but are
 * keyed by a FUNCTION term's id rather than a proof step's, so they are
 * invisible to `obligations_of_proof`. Two independent criteria identify
 * them, both required:
 *
 *  1. the minimal non-proof entry SHAPE — no incoming, no outgoing, no
 *     marks, at least one obligation (this is exactly how the checker
 *     records them, and how Test_FunContracts identifies them);
 *  2. the key is not the rep_id of any step of the given proofs.
 *
 * (1) alone would also catch a proof step that happened to be recorded
 * with neither goal side and no mark; (2) alone would catch every entry
 * belonging to some OTHER cell's proof. `proofs` should be every proof
 * term whose obligations are displayed separately. */
let is_definition_entry = (entry: entry): bool =>
  switch (entry) {
  | {incoming: None, outgoing: None, marks: [], obligations: [_, ..._], _} =>
    true
  | _ => false
  };

let definition_obligations =
    (~proofs: list(Proof.t)=[], pm: t): list(Obligation.t) => {
  let proof_ids = List.concat_map(rep_ids_of_proof, proofs);
  Id.Map.fold(
    (id, entry, acc) =>
      is_definition_entry(entry) && !List.mem(id, proof_ids)
        ? acc @ entry.obligations : acc,
    pm,
    [],
  );
};

/* The still-undischarged obligations in the proof subtree. */
let pending_obligations = (pm: t, proof: Proof.t): list(Obligation.t) =>
  obligations_of_proof(pm, proof) |> List.filter(Obligation.is_pending);

/* Obligation-aware proof status. `status_of_proof` above is kept as-is
 * (its consumers predate obligations); this refines it:
 * - Proven / Refuted — literal `true`/`false`, clean subtree, and NO
 *   pending obligations anywhere in the subtree;
 * - ProvenModulo(obs) — the goal reached literal `true` with a clean
 *   subtree, but pending obligations `obs` remain (§3.1's
 *   "ProvenModulo" outcome);
 * - Incomplete — everything else. */
[@deriving (show({with_path: false}), sexp, yojson)]
type full_status =
  | Proven
  | Refuted
  | ProvenModulo(list(Obligation.t))
  | Incomplete;

let full_status_of_proof = (pm: t, proof: Proof.t): full_status =>
  switch (status_of_proof(pm, proof), pending_obligations(pm, proof)) {
  | (Some(true), []) => Proven
  | (Some(true), pending) => ProvenModulo(pending)
  | (Some(false), []) => Refuted
  | (Some(false), _)
  | (None, _) => Incomplete
  };
