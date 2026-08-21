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

/* --- actions: the three exits of the (!) menu (§3.3) ----------------- */

/* Every action is an editor EDIT: it emits an `EditorTransform.patch`
 * against the program text, after which statics/dynamics re-run and the
 * (!) states recompute. Nothing here mutates prover state — program text
 * stays the single source of truth (docs/prover-obligations.md §3.3).
 *
 * The three exits:
 *   1. FLOAT the restriction onto the theorem's binder — `forall n ->`
 *      becomes `forall n where <goal> ->`, AND-extending an existing
 *      guard. Offered ONLY for theorem-level binders: on an induction-case
 *      binder a restriction breaks exhaustiveness, so the option is shown
 *      DISABLED with that reason rather than hidden (§3.3's table).
 *   2. PROVE HERE — wrap the enclosing proof region in `have <goal> proof
 *      ? => …`. The obligation flips Pending → Remote immediately (channel
 *      1 finds the have's hypothesis) and the `?` is proved at leisure.
 *   3. SPLIT on it — wrap the same region in `induction <goal> | true =>
 *      <region> | false => ? end`. In the `true` branch the case equation
 *      covers the obligation; the `false` branch is the user's problem.
 *
 * Everything below `View` is pure and unit-tested. */

/* What an obligation's actions are computed against: the theorem's
 * STATEMENT (float rewrites a binder there) and its PROOF (the wrapping
 * actions target a region of it). Both optional so a panel with no
 * syntax to act on degrades to receipts only — which is exactly the
 * definition-time section, whose obligations belong to no proof tree. */
type action_ctx = {
  stmt: option(Exp.t),
  proof: option(Proof.t),
};

let no_action_ctx: action_ctx = {
  stmt: None,
  proof: None,
};

/* One theorem-level `forall` binder of the statement, kept whole so the
 * float patch can rebuild the node in place. */
type binder = {
  /* The Forall / ForallWhere node itself: the ExpPatch target. */
  id: Id.t,
  pat: Pat.t,
  guard: option(Exp.t),
  body: Exp.t,
  vars: list(Var.t),
};

/* The statement's theorem-level binders, OUTERMOST first. Only the
 * unbroken prefix of `forall`s counts: those are the binders whose
 * restriction is the public statement's (§2.2). */
let rec binders_of_stmt = (e: Exp.t): list(binder) =>
  switch (Exp.term_of(e)) {
  | Forall(p, body) => [
      {
        id: Exp.rep_id(e),
        pat: p,
        guard: None,
        body,
        vars: Pat.bound_vars(p),
      },
      ...binders_of_stmt(body),
    ]
  | ForallWhere(p, g, body) => [
      {
        id: Exp.rep_id(e),
        pat: p,
        guard: Some(g),
        body,
        vars: Pat.bound_vars(p),
      },
      ...binders_of_stmt(body),
    ]
  | Parens(e') => binders_of_stmt(e')
  | _ => []
  };

/* Is `origin` the id of a node in this proof subtree? */
let rec proof_contains = (~origin: Id.t, p: Proof.t): bool =>
  Proof.rep_id(p) == origin
  || (
    switch (p.term) {
    | EmptyHole
    | Invalid(_)
    | MultiHole(_)
    | AxiomStep(_)
    | AlgebriteStep(_)
    | Contradiction(_)
    | EvalStep(_) => false
    | Seq(a, b) => proof_contains(~origin, a) || proof_contains(~origin, b)
    | Forall(_, body)
    | Assume(_, body)
    | Generalize(_, body)
    | Revert(_, _, body) => proof_contains(~origin, body)
    | Have(_, sub, body) =>
      proof_contains(~origin, sub) || proof_contains(~origin, body)
    | Induction(_, cases) =>
      List.exists(((_, body)) => proof_contains(~origin, body), cases)
    }
  );

/* Variables bound by INDUCTION CASE patterns on the path from the proof
 * root down to `origin`. These are the case-local binders of §3.3: a goal
 * mentioning one of them cannot be floated (the restriction would have to
 * live at a case, which breaks exhaustiveness), and it is also why the
 * float target is COMPUTED rather than chosen.
 *
 * `forall` steps in a proof are not counted: they peel a binder the
 * theorem statement already declares, so their variables ARE
 * theorem-level. */
let rec case_vars_to = (~origin: Id.t, ~acc: list(Var.t), p: Proof.t) =>
  if (Proof.rep_id(p) == origin) {
    acc;
  } else {
    switch (p.term) {
    | EmptyHole
    | Invalid(_)
    | MultiHole(_)
    | AxiomStep(_)
    | AlgebriteStep(_)
    | Contradiction(_)
    | EvalStep(_) => acc
    | Seq(a, b) =>
      proof_contains(~origin, a)
        ? case_vars_to(~origin, ~acc, a) : case_vars_to(~origin, ~acc, b)
    | Forall(_, body)
    | Assume(_, body)
    | Generalize(_, body)
    | Revert(_, _, body) => case_vars_to(~origin, ~acc, body)
    | Have(_, sub, body) =>
      proof_contains(~origin, sub)
        ? case_vars_to(~origin, ~acc, sub)
        : case_vars_to(~origin, ~acc, body)
    | Induction(_, cases) =>
      switch (
        List.find_opt(((_, body)) => proof_contains(~origin, body), cases)
      ) {
      | Some((pat, body)) =>
        case_vars_to(~origin, ~acc=Pat.bound_vars(pat) @ acc, body)
      | None => acc
      }
    };
  };

/* Availability of the float action, computed from the goal's free
 * variables (§3.3: "the float target is computed, not chosen: the
 * innermost binder introducing any free variable of the obligation"). */
type float_target =
  /* The innermost theorem binder the goal mentions. */
  | FloatTo(binder)
  /* The goal mentions a case-local binder: a restriction there is
   * UNSOUND, so the option is greyed out with this reason. */
  | UnsoundAtCase
  /* No theorem binder is mentioned, so there is nothing to restrict
   * (ground obligations, or a goal over local definitions only). */
  | NoBinder;

let mentions = (v: Var.t, goal: Exp.t): bool =>
  ProofRule.occurs_free_any([v], goal);

let float_target_of = (~ctx: action_ctx, ob: Obligation.t): float_target => {
  let goal = Obligation.display_goal_of(ob);
  let case_vars =
    switch (ctx.proof) {
    | Some(p) when proof_contains(~origin=ob.origin, p) =>
      case_vars_to(~origin=ob.origin, ~acc=[], p)
    | _ => []
    };
  /* Case-scoping wins: even if the goal ALSO mentions a theorem binder,
   * the restriction would have to hold at the case to be usable there. */
  if (List.exists(v => mentions(v, goal), case_vars)) {
    UnsoundAtCase;
  } else {
    let binders =
      switch (ctx.stmt) {
      | Some(stmt) => binders_of_stmt(stmt)
      | None => []
      };
    /* Innermost = last mentioned in the outermost-first list. */
    switch (
      List.filter(b => List.exists(v => mentions(v, goal), b.vars), binders)
    ) {
    | [] => NoBinder
    | mentioned => FloatTo(List.nth(mentioned, List.length(mentioned) - 1))
    };
  };
};

/* The goal an action EMBEDS into the program, as opposed to the goal it
 * merely displays. An obligation's `display_goal` is a slice of the
 * CHECKED program: its exps carry the very ids the program's own tiles
 * were parsed from (and those ids' shard provenance). Splicing that
 * slice in verbatim leaves one id on two tiles, and `Measured` then
 * reports both occurrences' shards for a single tile — which is what
 * `Highlight.of_tile` reports as a shard mismatch (e.g. a one-token
 * `z` var tile arriving with `tile_Shards:2`), crashing the view on
 * every one of the three exits.
 *
 * Same hazard, same remedy as `StepperBase.embed_exp`: re-id the whole
 * subtree (which also drops the stale shard provenance), and inline any
 * closure environments first — a closure has no surface syntax, so
 * writing one out would corrupt the program text. Re-idding rather than
 * print-and-reparse keeps the term exactly as the checker produced it,
 * with no round-trip through the parser to change its shape. */
let embed_goal = (ob: Obligation.t): Exp.t =>
  Obligation.display_goal_of(ob)
  |> Substitution.in_exp(Environment.empty)
  |> Exp.replace_all_ids;

/* The rewritten binder: `forall p -> body` becomes `forall p where <goal>
 * -> body`, and an existing guard is AND-extended (`g && <goal>`) rather
 * than replaced. The goal printed is `display_goal` — what the user
 * wrote at the obligation site, not the env-inlined semantic term. */
let float_binder_exp = (~goal: Exp.t, b: binder): Exp.t =>
  switch (b.guard) {
  | None => Exp.fresh(ForallWhere(b.pat, goal, b.body))
  | Some(g) =>
    Exp.fresh(
      ForallWhere(b.pat, Exp.fresh(BinOp(Bool(And), g, goal)), b.body),
    )
  };

let float_patch =
    (~ctx: action_ctx, ob: Obligation.t)
    : option(Haz3lcore.EditorTransform.patch) =>
  switch (float_target_of(~ctx, ob)) {
  | FloatTo(b) =>
    Some(
      Haz3lcore.EditorTransform.mk_patch(
        ~target_id=b.id,
        float_binder_exp(~goal=embed_goal(ob), b),
      ),
    )
  | UnsoundAtCase
  | NoBinder => None
  };

/* The proof region a wrapping action targets: the whole step chain of the
 * INNERMOST scope containing the incurring step. Descending to the
 * innermost scope matters for soundness of the emitted text — a goal
 * mentioning a case-local variable must be wrapped INSIDE that case, or
 * the wrapper's proposition would mention an unbound variable. Taking the
 * whole chain there (rather than the single step) keeps the wrapper's
 * body a complete `Seq` spine, so no re-association is possible. */
let rec scope_body_containing = (~origin: Id.t, p: Proof.t): option(Proof.t) =>
  switch (p.term) {
  | Forall(_, body)
  | Assume(_, body)
  | Generalize(_, body)
  | Revert(_, _, body) => proof_contains(~origin, body) ? Some(body) : None
  | Have(_, sub, body) =>
    proof_contains(~origin, body)
      ? Some(body) : proof_contains(~origin, sub) ? Some(sub) : None
  | Induction(_, cases) =>
    List.find_map(
      ((_, body)) => proof_contains(~origin, body) ? Some(body) : None,
      cases,
    )
  | Seq(a, b) =>
    switch (scope_body_containing(~origin, a)) {
    | Some(_) as r => r
    | None => scope_body_containing(~origin, b)
    }
  | EmptyHole
  | Invalid(_)
  | MultiHole(_)
  | AxiomStep(_)
  | AlgebriteStep(_)
  | Contradiction(_)
  | EvalStep(_) => None
  };

let rec region_containing = (~origin: Id.t, root: Proof.t): option(Proof.t) =>
  if (!proof_contains(~origin, root)) {
    None;
  } else {
    switch (scope_body_containing(~origin, root)) {
    | Some(body) => region_containing(~origin, body)
    | None => Some(root)
    };
  };

let region_of = (~ctx: action_ctx, ob: Obligation.t): option(Proof.t) =>
  switch (ctx.proof) {
  | Some(p) => region_containing(~origin=ob.origin, p)
  | None => None
  };

/* `have <goal> proof ? => <region>`: the obligation flips Pending →
 * Remote against the have's hypothesis at once, and the `?` is the
 * subproof the user fills in later. */
let have_patch =
    (~ctx: action_ctx, ob: Obligation.t)
    : option(Haz3lcore.EditorTransform.patch) =>
  region_of(~ctx, ob)
  |> Option.map(region =>
       Haz3lcore.EditorTransform.mk_proof_patch(
         ~target_id=Proof.rep_id(region),
         Proof.fresh(
           Have(embed_goal(ob), Proof.fresh(EmptyHole), region),
         ),
       )
     );

/* `induction <goal> | true => <region> | false => ? end`: ordinary
 * bool-case analysis, so the split gate (divergence / domain, ProofCheck's
 * induction arm) applies automatically. */
let split_patch =
    (~ctx: action_ctx, ob: Obligation.t)
    : option(Haz3lcore.EditorTransform.patch) =>
  region_of(~ctx, ob)
  |> Option.map(region =>
       Haz3lcore.EditorTransform.mk_proof_patch(
         ~target_id=Proof.rep_id(region),
         Proof.fresh(
           Induction(
             embed_goal(ob),
             [
               (Pat.fresh(Atom(Bool(true))), region),
               (Pat.fresh(Atom(Bool(false))), Proof.fresh(EmptyHole)),
             ],
           ),
         ),
       )
     );

/* One row of the (!) menu. A `None` patch is a DISABLED entry: `reason`
 * says why, which is the §3.3 requirement that an unsound float be shown
 * greyed out rather than silently dropped. */
type action = {
  label: string,
  title: string,
  patch: option(Haz3lcore.EditorTransform.patch),
};

let float_action = (~ctx: action_ctx, ob: Obligation.t): action =>
  switch (float_target_of(~ctx, ob)) {
  | FloatTo(b) => {
      label: "Add to statement",
      title:
        "Restrict the theorem's `forall "
        ++ String.concat(", ", b.vars)
        ++ "` with this condition (changes the statement)",
      patch: float_patch(~ctx, ob),
    }
  | UnsoundAtCase => {
      label: "Add to statement",
      title: "Unavailable: a restriction at a case is unsound — it would break exhaustiveness",
      patch: None,
    }
  | NoBinder => {
      label: "Add to statement",
      title: "Unavailable: this goal mentions no quantified variable of the statement",
      patch: None,
    }
  };

let actions_of = (~ctx: action_ctx, ob: Obligation.t): list(action) =>
  Obligation.is_pending(ob)
    ? [
      float_action(~ctx, ob),
      {
        label: "Prove here",
        title: "Wrap this part of the proof in `have <goal> proof ? => …` and prove it at leisure",
        patch: have_patch(~ctx, ob),
      },
      {
        label: "Split on it",
        title: "Case-split on this condition: the `true` branch gets it as a fact, the `false` branch stays open",
        patch: split_patch(~ctx, ob),
      },
    ]
    : [];

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


/* Receipts click-to-jump (UI-1's deferred stretch goal): a Remote row's
 * covering fact is a term of the program — the `where` guard on a binder,
 * a `case_eq`'s scrutinee, an `assume`/`have`'s proposition — so its id is
 * usually a tile of the main editor and the receipt can move the caret
 * there. Note the hypothesis ENTRY id cannot be used: entries are minted
 * with `Id.mk()` in `SemanticCtx.add_entry_free_name` and have no syntax.
 * The fact EXPRESSION's id does survive from the source text (env
 * substitution rewrites terms, not ids, and a guard like `y != 0` is
 * substituted by the identity).
 *
 * Degrades cleanly: when the id is not a tile of the current editor (the
 * fact was synthesised, or the text has since changed), no jump is
 * offered and the receipt renders exactly as before. */
let jump_effect =
    (~main_editor: option(CodeEditable.Channel.t), fact: Exp.t)
    : option(Ui_effect.t(unit)) =>
  switch (main_editor) {
  | None => None
  | Some({model, inject}) =>
    CodeEditable.Selection.jump_to_tile(Exp.rep_id(fact), model)
    |> Option.map(inject)
  };

let receipt =
    (
      ~globals,
      ~main_editor: option(CodeEditable.Channel.t)=None,
      ob: Obligation.t,
    )
    : list(Node.t) => {
  let label =
    div(
      ~attrs=[clss(["obligation-receipt-label"])],
      [text(discharge_label(ob))],
    );
  switch (Obligation.remote_fact(ob)) {
  /* The receipt proper: show the covering fact's statement, so "why is
   * this silent?" is answered in place rather than on a hover. */
  | Some((_, fact)) =>
    let jump = jump_effect(~main_editor, fact);
    let attrs =
      switch (jump) {
      | Some(eff) => [
          clss(["obligation-fact", "jumpable"]),
          Attr.title("Jump to this fact in the editor"),
          Attr.on_click(_ => eff),
        ]
      | None => [clss(["obligation-fact"])]
      };
    [label, div(~attrs, [view_goal(~globals, fact)])]
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
    );
  | None => [label]
  };
};

/* The (!) menu itself: one button per exit, disabled ones kept visible
 * with their reason on hover (§3.3 — an unsound float is GREYED OUT, never
 * omitted). */
let action_button =
    (
      ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
      a: action,
    )
    : Node.t =>
  switch (a.patch) {
  | Some(patch) =>
    button(
      ~attrs=[
        clss(["obligation-action"]),
        Attr.title(a.title),
        Attr.on_click(_ => edit_syntax(patch)),
      ],
      [text(a.label)],
    )
  | None =>
    button(
      ~attrs=[
        clss(["obligation-action", "disabled"]),
        Attr.title(a.title),
        Attr.disabled,
      ],
      [text(a.label)],
    )
  };

let action_menu =
    (
      ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
      ~action_ctx: action_ctx,
      ob: Obligation.t,
    )
    : list(Node.t) =>
  switch (actions_of(~ctx=action_ctx, ob)) {
  | [] => []
  | actions => [
      div(
        ~attrs=[clss(["obligation-actions"])],
        List.map(action_button(~edit_syntax), actions),
      ),
    ]
  };

let obligation_row =
    (
      ~globals,
      ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit)=_ =>
                                                                    Ui_effect.Ignore,
      ~action_ctx: action_ctx=no_action_ctx,
      ~main_editor: option(CodeEditable.Channel.t)=None,
      ob: Obligation.t,
    ) =>
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
          ...receipt(~globals, ~main_editor, ob)
             @ action_menu(~edit_syntax, ~action_ctx, ob),
        ],
      ),
    ],
  );

let section =
    (
      ~globals,
      ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit)=_ =>
                                                                    Ui_effect.Ignore,
      ~action_ctx: action_ctx=no_action_ctx,
      ~main_editor: option(CodeEditable.Channel.t)=None,
      ~title: string,
      obs: list(Obligation.t),
    ) =>
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
          ...List.map(
               obligation_row(
                 ~globals,
                 ~edit_syntax,
                 ~action_ctx,
                 ~main_editor,
               ),
               obs,
             ),
        ],
      ),
    ]
  };

/* The panel for one theorem: its proof's obligations, each pending row
 * carrying the three-exit action menu. `action_ctx` is what makes the
 * actions computable — without it the rows still render as receipts. */
let view =
    (
      ~globals,
      ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit)=_ =>
                                                                    Ui_effect.Ignore,
      ~action_ctx: action_ctx=no_action_ctx,
      ~main_editor: option(CodeEditable.Channel.t)=None,
      g: group,
    )
    : list(Node.t) =>
  switch (
    section(
      ~globals,
      ~edit_syntax,
      ~action_ctx,
      ~main_editor,
      ~title="Obligations",
      g.proof,
    )
  ) {
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
