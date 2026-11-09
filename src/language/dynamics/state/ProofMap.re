open Util;

/* For each proof sub-term the checker reaches, keyed by its `rep_id`: the
 * goal going into the step and the goal coming out, the marks that the
 * editor shows on that node, and the scope it was checked in.
 *
 * `None` on either side means checking broke down here: the incoming goal
 * couldn't be computed because an earlier step failed, or this step's
 * rule didn't apply. */

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
  /* The scope the checker checked this step in: the variables and facts
   * a step written here can use. A form's body is recorded under the
   * body's own id, with the scope the form opens for it. */
  ctx: SemanticCtx.t,
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

/* Union prefers the right-hand side on overlap; used to merge a freshly-
 * produced proof map into the accumulated state. */
let union = (a: t, b: t): t => Id.Map.union((_id, _l, r) => Some(r), a, b);

/* No entry in the proof's subtree carries a mark, a `have`'s subproof
 * included. The checker passes the goal through a failed step unchanged
 * (`ProofCheck.check`), so a `true` outgoing goal alone doesn't mean the
 * proof holds. */
let rec proof_is_clean = (pm: t, proof: Proof.t): bool =>
  marks_of(Proof.rep_id(proof), pm) == []
  && List.for_all(proof_is_clean(pm), Proof.children(proof));

let outgoing_is = (b: bool, pm: t, proof: Proof.t): bool =>
  switch (lookup(Proof.rep_id(proof), pm)) {
  | Some({outgoing: Some(e), _}) =>
    Exp.fast_equal(e, Exp.temp(Atom(Bool(b))))
  | Some({outgoing: None, _})
  | None => false
  };

/* Every induction case in the subtree reaches `true`. An induction steps
 * to `true` as soon as its cases cover the scrutinee, finished or not. */
let rec cases_closed = (pm: t, proof: Proof.t): bool =>
  (
    switch (proof.term) {
    | Induction(_, _, cases) =>
      List.for_all(((_, body)) => outgoing_is(true, pm, body), cases)
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | Seq(_)
    | Parens(_)
    | AxiomStep(_)
    | AlgebriteStep(_)
    | EvalStep(_)
    | Forall(_)
    | Funext(_)
    | Assume(_)
    | Alias(_)
    | Generalize(_)
    | Revert(_)
    | Contradiction(_)
    | Have(_) => true
    }
  )
  && List.for_all(cases_closed(pm), Proof.children(proof));

/* UI status derived from a proof map for a given proof term:
 * - `Some(true)` (proven) iff the proof's outgoing expression is the
 *   literal `true`, no step in the proof is broken and every induction
 *   case reaches `true`.
 * - `Some(false)` (disproven) iff the proof's outgoing expression is the
 *   literal `false` and no step in the proof is broken.
 * - `None` otherwise — incomplete proofs (holes, open induction cases),
 *   broken steps (marks in the subtree), or any other non-boolean
 *   outgoing. A
 *   concrete-but-incorrect proof is not a disproof.
 *
 * Mirrors the stepper's expression-validity convention
 * (`true` / `false` / unknown). */
let status_of_proof = (pm: t, proof: Proof.t): option(bool) =>
  if (!proof_is_clean(pm, proof) || !cases_closed(pm, proof)) {
    None;
  } else if (outgoing_is(true, pm, proof)) {
    Some(true);
  } else if (outgoing_is(false, pm, proof)) {
    Some(false);
  } else {
    None;
  };

/* All obligations recorded in the proof subtree, in proof-term order. */
let rec obligations_of_proof = (pm: t, proof: Proof.t): list(Obligation.t) => {
  let here =
    switch (lookup(Proof.rep_id(proof), pm)) {
    | Some({obligations, _}) => obligations
    | None => []
    };
  here @ List.concat_map(obligations_of_proof(pm), Proof.children(proof));
};

/* Every id at which a step of this proof term is recorded. */
let rec rep_ids_of_proof = (proof: Proof.t): list(Id.t) => [
  Proof.rep_id(proof),
  ...List.concat_map(rep_ids_of_proof, Proof.children(proof)),
];

/* Definition-time obligations (from `ProofCheck.definition_obligations`)
 * share this map but are keyed by a function's id, so
 * `obligations_of_proof` doesn't see them. An entry is one when both hold:
 *
 *  1. it has the shape the checker gives them: no incoming or outgoing
 *     goal, no marks, at least one obligation;
 *  2. its key is not the id of a step of any of `proofs`.
 *
 * (1) alone would also catch a proof step recorded with no goals and no
 * mark; (2) alone would catch the steps of other cells' proofs. `proofs`
 * should be every proof whose obligations are displayed separately. */
let definition_obligations =
    (~proofs: list(Proof.t)=[], pm: t): list(Obligation.t) => {
  let is_definition_entry = (entry: entry): bool =>
    switch (entry) {
    | {incoming: None, outgoing: None, marks: [], obligations: [_, ..._], _} =>
      true
    | _ => false
    };
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

/* `status_of_proof`, refined by obligations:
 * - Proven / Refuted: `status_of_proof` is `true`/`false` and nothing in
 *   the subtree is pending;
 * - ProvenModulo(obs): proven, but the obligations `obs` are pending;
 * - Incomplete: everything else. */
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
