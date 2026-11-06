open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Language;

/* The theorem panels' obligations: a theorem's status chip, where
 * ProvenModulo (proven, but with obligations nobody has discharged) is a
 * state of its own, and one row per obligation, with its goal, its
 * discharge receipt, and for a pending one the actions that discharge it.
 *
 * Everything above the view section is pure, and tested in
 * Test_ObligationsPanel. */

// Status

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

/* Class suffixes are kept in sync with theorems.css. */
let status_class = (status: ProofMap.full_status): string =>
  switch (status) {
  | Proven => "true"
  | Refuted => "false"
  | ProvenModulo(_) => "modulo"
  | Incomplete => "unknown"
  };

// Grouping

/* Obligations as the panel groups them: those incurred by steps of this
 * theorem's proof, and the definition-time ones, which are keyed
 * by a function's own id and so belong to no proof tree. */
type group = {
  proof: list(Obligation.t),
  definitions: list(Obligation.t),
};

let empty_group = {
  proof: [],
  definitions: [],
};

/* Pending obligations first: they ask for action, the others are
 * receipts. Stable, so each keeps proof-term order. */
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

// Receipts

/* The checker names a hypothesis after the construct that introduced it.
 * A name not listed here prints as itself. */
let fact_source = (name: Var.t): string =>
  switch (name) {
  | "where" => "a `where` restriction"
  | "assume" => "an assumption"
  | "case_eq" => "a case equation"
  | "ih" => "the induction hypothesis"
  | _ => "`" ++ name ++ "`"
  };

/* Why the obligation is discharged. A covering fact's statement renders
 * alongside (see `receipt`), so this only names its source. */
let discharge_label = (ob: Obligation.t): string =>
  switch (ob.discharge) {
  | Pending => "pending — nothing in scope covers this"
  | Evaluated => "discharged by evaluation"
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
  | Remote(_) => "remote"
  };

// Actions

/* A pending obligation's actions, each a patch to the program text:
 *
 *   1. Add to statement: `forall n ->` becomes `forall n where <goal> ->`,
 *      extending an existing guard with `&&`. Only for the statement's
 *      binders: on an induction case's binder a restriction breaks
 *      exhaustiveness, so the action shows disabled, with that reason.
 *   2. Prove here: wrap the proof region in `have <goal> proof ? => …`.
 *      The obligation is discharged at once by the have's hypothesis, and
 *      the `?` is proved later.
 *   3. Split on it: wrap the region in `induction <goal> | true =>
 *      <region> | false => ? end`. The `true` case's equation covers the
 *      obligation; the `false` case is left open. */

/* The syntax the actions act on: the statement, whose binder (1) rewrites,
 * and the proof, a region of which (2) and (3) wrap. Without them, as for
 * the definitions' obligations, which belong to no proof, a panel shows
 * receipts only. */
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
  /* The Forall node itself: the ExpPatch target. */
  id: Id.t,
  pat: Pat.t,
  guard: option(Exp.t),
  body: Exp.t,
  vars: list(Var.t),
};

/* The statement's theorem-level binders, outermost first. Only the
 * unbroken prefix of `forall`s counts: those are the binders whose
 * restriction is the public statement's. */
let rec binders_of_stmt = (e: Exp.t): list(binder) =>
  switch (Exp.term_of(e)) {
  | Forall(p, body) =>
    /* `forall p where g -> body` is `Forall(Where(p, g), body)`: split
       the restriction back off the binder's pattern. */
    let (bare, guard) = Exp.split_pat_guard(p);
    [
      {
        id: Exp.rep_id(e),
        pat: bare,
        guard,
        body,
        vars: Pat.bound_vars(bare),
      },
      ...binders_of_stmt(body),
    ];
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
    | Parens(body)
    | Forall(_, body)
    | Funext(_, body)
    | Assume(_, _, body)
    | Generalize(_, body)
    | Alias(_, _, body)
    | Revert(_, _, body) => proof_contains(~origin, body)
    | Have(_, sub, body) =>
      proof_contains(~origin, sub) || proof_contains(~origin, body)
    | Induction(_, _, cases) =>
      List.exists(((_, body)) => proof_contains(~origin, body), cases)
    }
  );

/* Variables bound by induction cases on the path from the proof's root to
 * `origin`: a goal mentioning one can't be added to the statement. A
 * `forall` step's binder doesn't count: the statement already declares
 * it. */
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
    | Parens(body)
    | Forall(_, body)
    | Assume(_, _, body)
    | Generalize(_, body)
    | Alias(_, _, body)
    | Revert(_, _, body) => case_vars_to(~origin, ~acc, body)
    /* Unlike `forall`'s, a `funext` binder is not the statement's: the
       statement says `f == g`, and `x` exists only inside the step. */
    | Funext(pat, body) =>
      case_vars_to(~origin, ~acc=Pat.bound_vars(pat) @ acc, body)
    | Have(_, sub, body) =>
      proof_contains(~origin, sub)
        ? case_vars_to(~origin, ~acc, sub)
        : case_vars_to(~origin, ~acc, body)
    | Induction(_, _, cases) =>
      switch (
        List.find_opt(((_, body)) => proof_contains(~origin, body), cases)
      ) {
      | Some((pat, body)) =>
        case_vars_to(~origin, ~acc=Pat.bound_vars(pat) @ acc, body)
      | None => acc
      }
    };
  };

/* Where action (1) adds the goal: the innermost binder of the statement
 * that the goal mentions. */
type float_target =
  /* The innermost theorem binder the goal mentions. */
  | FloatTo(binder)
  /* The goal mentions a case-local binder: a restriction there is
   * unsound, so the option is greyed out with this reason. */
  | UnsoundAtCase
  /* No theorem binder is mentioned, so there is nothing to restrict
   * (ground obligations, or a goal over local definitions only). */
  | NoBinder;

let float_target_of = (~ctx: action_ctx, ob: Obligation.t): float_target => {
  let goal = Obligation.display_goal_of(ob);
  let mentions = (v: Var.t) => ProofRule.occurs_free_any([v], goal);
  let case_vars =
    switch (ctx.proof) {
    | Some(p) when proof_contains(~origin=ob.origin, p) =>
      case_vars_to(~origin=ob.origin, ~acc=[], p)
    | _ => []
    };
  /* Case-scoping wins: even if the goal also mentions a theorem binder,
   * the restriction would have to hold at the case to be usable there. */
  if (List.exists(mentions, case_vars)) {
    UnsoundAtCase;
  } else {
    let binders =
      switch (ctx.stmt) {
      | Some(stmt) => binders_of_stmt(stmt)
      | None => []
      };
    switch (List.filter(b => List.exists(mentions, b.vars), binders)) {
    | [] => NoBinder
    | mentioned => FloatTo(Util.ListUtil.last(mentioned))
    };
  };
};

/* The goal, fit to write into the program, as in
 * `StepperBase.Terms.embed_exp`. `display_goal` is a slice of the program
 * and carries its tiles' ids: written in verbatim, one id would land on
 * two tiles, which crashes the view. */
let embed_goal = (ob: Obligation.t): Exp.t =>
  Obligation.display_goal_of(ob)
  |> Substitution.in_exp(Environment.empty)
  |> Exp.replace_all_ids;

/* `forall p -> body` becomes `forall p where <goal> -> body`, or with an
 * existing guard `g`, `forall p where g && <goal> -> body`. */
let float_binder_exp = (~goal: Exp.t, b: binder): Exp.t => {
  let guard =
    switch (b.guard) {
    | None => goal
    | Some(g) => Exp.fresh(BinOp(Bool(And), g, goal))
    };
  Exp.fresh(Forall(Pat.fresh(Where(b.pat, guard)), b.body));
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

/* The proof region actions (2) and (3) wrap: the whole step chain of the
 * innermost scope holding the step that incurred the obligation. It has
 * to be the innermost, or a goal mentioning a case's variable would be
 * wrapped outside the case that binds it. Taking the whole chain keeps
 * the wrapper's body a complete `Seq` spine. */
let region_of = (~ctx: action_ctx, ob: Obligation.t): option(Proof.t) => {
  let origin = ob.origin;
  let rec scope_body_containing = (p: Proof.t): option(Proof.t) =>
    switch (p.term) {
    | Forall(_, body)
    | Funext(_, body)
    | Assume(_, _, body)
    | Generalize(_, body)
    | Alias(_, _, body)
    | Revert(_, _, body) =>
      proof_contains(~origin, body) ? Some(body) : None
    | Have(_, sub, body) =>
      proof_contains(~origin, body)
        ? Some(body) : proof_contains(~origin, sub) ? Some(sub) : None
    | Induction(_, _, cases) =>
      List.find_map(
        ((_, body)) => proof_contains(~origin, body) ? Some(body) : None,
        cases,
      )
    | Seq(a, b) =>
      switch (scope_body_containing(a)) {
      | Some(_) as r => r
      | None => scope_body_containing(b)
      }
    /* Parens are not a scope. */
    | Parens(inner) => scope_body_containing(inner)
    | EmptyHole
    | Invalid(_)
    | MultiHole(_)
    | AxiomStep(_)
    | AlgebriteStep(_)
    | Contradiction(_)
    | EvalStep(_) => None
    };
  let rec region_containing = (root: Proof.t): option(Proof.t) =>
    if (!proof_contains(~origin, root)) {
      None;
    } else {
      switch (scope_body_containing(root)) {
      | Some(body) => region_containing(body)
      | None => Some(root)
      };
    };
  Option.bind(ctx.proof, region_containing);
};

/* `have <goal> proof ? => <region>`. */
let have_patch =
    (~ctx: action_ctx, ob: Obligation.t)
    : option(Haz3lcore.EditorTransform.patch) =>
  region_of(~ctx, ob)
  |> Option.map(region =>
       Haz3lcore.EditorTransform.mk_proof_patch(
         ~target_id=Proof.rep_id(region),
         Proof.fresh(Have(embed_goal(ob), Proof.fresh(EmptyHole), region)),
       )
     );

/* `induction <goal> | true => <region> | false => ? end`: an ordinary
 * case split on a bool, checked like any other. */
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
             None,
             [
               (Pat.fresh(Atom(Bool(true))), region),
               (Pat.fresh(Atom(Bool(false))), Proof.fresh(EmptyHole)),
             ],
           ),
         ),
       )
     );

/* One action. With no patch it shows disabled, its title saying why. */
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

// View

/* Printed like the goals in the proof rows above. */
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

/* A covering fact with a bare boolean conclusion is used as the equation
 * `P == true`, and the receipt says so. The panel has no statics, so only
 * a fact whose syntax settles the question gets the note. */
let fact_reading = (fact: Exp.t): option(Exp.t) =>
  fact
  |> ProofRule.exp_to_rule
  |> ProofRule.BoolReading.apply
  |> ProofRule.BoolReading.equation;

/* Clicking a receipt jumps to its covering fact. The fact is usually a
 * term of the program (a `where` guard, a case's scrutinee, an `assume`'s
 * proposition) whose id survives env substitution; the hypothesis entry
 * itself has a fresh id and no syntax. No jump when the id is not a tile
 * of the editor (the fact was made up, or the text has changed). */
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
      ~main_editor: option(CodeEditable.Channel.t),
      ob: Obligation.t,
    )
    : list(Node.t) => {
  let label =
    div(
      ~attrs=[clss(["obligation-receipt-label"])],
      [text(discharge_label(ob))],
    );
  switch (Obligation.remote_fact(ob)) {
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

let action_menu =
    (
      ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
      ~action_ctx: action_ctx,
      ob: Obligation.t,
    )
    : list(Node.t) => {
  let button = (a: action) =>
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
  switch (actions_of(~ctx=action_ctx, ob)) {
  | [] => []
  | actions => [
      div(
        ~attrs=[clss(["obligation-actions"])],
        List.map(button, actions),
      ),
    ]
  };
};

let obligation_row =
    (
      ~globals,
      ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
      ~action_ctx: action_ctx,
      ~main_editor: option(CodeEditable.Channel.t),
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
          /* As written, not env-substituted. */
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

/* The panel for one theorem: its proof's obligations. */
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

/* The definitions' obligations, which every theorem in the cell shares,
 * so they render once, below the theorems. */
let view_definitions = (~globals, g: group): list(Node.t) =>
  switch (
    section(~globals, ~title="Obligations at definitions", g.definitions)
  ) {
  | [] => []
  | sections => [
      div(~attrs=[clss(["obligations-panel", "definitions"])], sections),
    ]
  };
