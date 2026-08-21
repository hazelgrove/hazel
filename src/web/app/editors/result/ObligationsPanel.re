open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Language;

/* OBLIGATIONSPANEL.re — renders the prover's obligation system
 * (docs/prover-obligations.md §3–§4) for the theorem panels.
 *
 * Two things live here:
 *   - the theorem status chip, now driven by
 *     `ProofMap.full_status_of_proof` so that ProvenModulo — proven, but
 *     with obligations nobody has discharged — is visually its own state
 *     rather than collapsing into "proven true";
 *   - the obligations list itself: one row per obligation, showing the
 *     goal and its discharge RECEIPT (§4.2, "receipts everywhere") —
 *     the covering fact for a Remote discharge, "by evaluation" for a
 *     closed one, and a prominent mark for Pending.
 *
 * Everything above `View` is pure and unit-tested (Test_ObligationsPanel).
 */

/* --- status (§3.1's third outcome) ---------------------------------- */

let status_label = (status: ProofMap.full_status): string =>
  switch (status) {
  | Proven => "proven true"
  | Refuted => "disproven"
  /* The asterisk is the point: the goal did reach `true`, but only
   * granted the pending obligations. */
  | ProvenModulo(obs) =>
    "proven* ("
    ++ string_of_int(List.length(obs))
    ++ " pending "
    ++ (List.length(obs) == 1 ? "obligation" : "obligations")
    ++ ")"
  | Incomplete => "incomplete"
  };

/* Class suffixes are kept in sync with theorems.css. `true`/`false`/
 * `unknown` are the pre-existing three; `modulo` is new. */
let status_class = (status: ProofMap.full_status): string =>
  switch (status) {
  | Proven => "true"
  | Refuted => "false"
  | ProvenModulo(_) => "modulo"
  | Incomplete => "unknown"
  };

/* --- grouping ------------------------------------------------------- */

/* Obligations as the panel groups them: those incurred by steps of this
 * theorem's proof, and the definition-time ones (§2.2), which are keyed
 * by a function's own id and so belong to no proof tree. */
type group = {
  proof: list(Obligation.t),
  definitions: list(Obligation.t),
};

let empty_group = {
  proof: [],
  definitions: [],
};

/* Pending obligations sort first — they are the ones asking for user
 * action; discharged ones are receipts. Stable within each class, so
 * proof-term order is preserved. */
let sort_for_display = (obs: list(Obligation.t)): list(Obligation.t) => {
  let (pending, discharged) = List.partition(Obligation.is_pending, obs);
  pending @ discharged;
};

let group_of = (~pm: ProofMap.t, ~proofs: list(Proof.t)): group => {
  proof:
    proofs
    |> List.concat_map(ProofMap.obligations_of_proof(pm))
    |> sort_for_display,
  definitions:
    ProofMap.definition_obligations(~proofs, pm) |> sort_for_display,
};

let group_is_empty = (g: group): bool => g.proof == [] && g.definitions == [];

/* --- receipts (§4.2) ------------------------------------------------ */

/* Hypotheses are named by the checker after the construct that
 * introduced them (`SemanticCtx.add_hypothesis` call sites in
 * ProofCheck); these read as prose. Unknown names are printed as-is, so
 * a new hypothesis kind degrades to something correct rather than
 * wrong. */
let fact_source = (name: Var.t): string =>
  switch (name) {
  | "where" => "a `where` restriction"
  | "assume" => "an assumption"
  | "case_eq" => "a case equation"
  | "ih" => "the induction hypothesis"
  | _ => "`" ++ name ++ "`"
  };

/* The one-line "why is this discharged?" summary. The Remote case's fact
 * statement is rendered as code alongside (see `receipt`), so the text
 * only has to name the source. */
let discharge_label = (ob: Obligation.t): string =>
  switch (ob.discharge) {
  | Pending => "pending — nothing in scope covers this"
  | Evaluated => "discharged by evaluation"
  | Local(_) => "discharged by an inline subproof"
  | Remote(_) =>
    switch (Obligation.remote_fact(ob)) {
    | Some((name, _)) => "discharged by " ++ fact_source(name)
    | None => "discharged by a fact in scope"
    }
  };

let discharge_class = (ob: Obligation.t): string =>
  switch (ob.discharge) {
  | Pending => "pending"
  | Evaluated => "evaluated"
  | Local(_) => "local"
  | Remote(_) => "remote"
  };

/* --- view ----------------------------------------------------------- */

/* Obligation goals are printed with the stepper's expression settings so
 * they read like the goals in the proof rows above them. */
let view_goal = (~globals, exp: Exp.t) =>
  div(
    ~attrs=[clss(["obligation-goal", "code-box-container"])],
    [
      CodeViewable.view_exp(
        ~globals,
        ~settings=ProofMarkView.code_view_settings,
        exp,
      ),
    ],
  );

/* A covering fact stated with a BARE-BOOLEAN conclusion is used as the
 * equation `P == true` (docs/prover-obligations.md §2.1). The receipt
 * shows that reading explicitly, so a statement in the panel is never
 * quietly standing for a different proposition. `with_bool_fact_reading`
 * is called without an info map — the panel has no statics — so the gate
 * is the purely syntactic one and a fact whose shape does not settle the
 * question simply gets no note. */
let fact_reading = (fact: Exp.t): option(Exp.t) =>
  fact
  |> ProofRule.exp_to_rule
  |> ProofRule.with_bool_fact_reading
  |> ProofRule.bool_reading_exp;

let receipt = (~globals, ob: Obligation.t): list(Node.t) => {
  let label =
    div(
      ~attrs=[clss(["obligation-receipt-label"])],
      [text(discharge_label(ob))],
    );
  switch (Obligation.remote_fact(ob)) {
  /* The receipt proper: show the covering fact's statement, so "why is
   * this silent?" is answered in place rather than on a hover. */
  | Some((_, fact)) =>
    [
      label,
      div(
        ~attrs=[clss(["obligation-fact"])],
        [view_goal(~globals, fact)],
      ),
    ]
    @ (
      switch (fact_reading(fact)) {
      | None => []
      | Some(reading) => [
          div(
            ~attrs=[clss(["obligation-receipt-label", "reading"])],
            [text("reads as:")],
          ),
          div(
            ~attrs=[clss(["obligation-fact"])],
            [view_goal(~globals, reading)],
          ),
        ]
      }
    )
  | None => [label]
  };
};

let obligation_row = (~globals, ob: Obligation.t) =>
  div(
    ~attrs=[clss(["obligation-row", discharge_class(ob)])],
    [
      div(
        ~attrs=[clss(["obligation-badge"])],
        [text(Obligation.is_pending(ob) ? "!" : "✓")],
      ),
      div(
        ~attrs=[clss(["obligation-body"])],
        [
          /* `display_goal`, not `goal`: the pre-substitution term, which
           * is what the user wrote (see Obligation.display_goal). */
          view_goal(~globals, Obligation.display_goal_of(ob)),
          ...receipt(~globals, ob),
        ],
      ),
    ],
  );

let section = (~globals, ~title: string, obs: list(Obligation.t)) =>
  switch (obs) {
  | [] => []
  | _ => [
      div(
        ~attrs=[clss(["obligation-section"])],
        [
          div(
            ~attrs=[clss(["obligation-section-title"])],
            [
              text(title),
              span(
                ~attrs=[clss(["obligation-count"])],
                [text(string_of_int(List.length(obs)))],
              ),
            ],
          ),
          ...List.map(obligation_row(~globals), obs),
        ],
      ),
    ]
  };

/* The panel for one theorem: its proof's obligations. */
let view = (~globals, g: group): list(Node.t) =>
  switch (section(~globals, ~title="Obligations", g.proof)) {
  | [] => []
  | sections => [div(~attrs=[clss(["obligations-panel"])], sections)]
  };

/* The cell-level panel for definition-time obligations. These come from
 * the definitions the theorems' proof contexts can see — shared across
 * every theorem in the cell — so they are rendered ONCE, below the
 * theorem list, rather than repeated in each theorem's panel. */
let view_definitions = (~globals, g: group): list(Node.t) =>
  switch (
    section(~globals, ~title="Obligations at definitions", g.definitions)
  ) {
  | [] => []
  | sections => [
      div(~attrs=[clss(["obligations-panel", "definitions"])], sections),
    ]
  };
