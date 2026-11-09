open Util;
open Language;
open Haz3lcore;

/* The step picker: what a row offers for the part of its goal the user
 * selected, as proof terms to write and boxes to open. Everything here
 * is a pure function of the row's goal, its scope and the selection, so
 * it can be tested without the UI. */

// Proof Terms

/* The proof terms the picker writes. */
module Terms = {
  /* An expression sliced from a row's goal, made fit to write into the
   * proof. Its tile ids already occur in the rendered theorem, so they
   * are freshened (a duplicate id breaks `Highlight.of_tile`). It may
   * hold closures the evaluator substituted in, which have no surface
   * syntax, so their environments are inlined. */
  let embed_exp = (e: Exp.t): Exp.t =>
    e |> Substitution.in_exp(Environment.empty) |> Exp.replace_all_ids;

  let index = (n: int): Exp.t => Exp.fresh(Atom(Int(Bigint.of_int(n))));

  let axiom_step =
      (
        ~at_idx: int,
        ~at_exp: Exp.t,
        ~direction: Direction.t,
        ~equality: string,
      )
      : TermBase.Proof.term =>
    AxiomStep({
      at_idx: index(at_idx),
      at_exp: embed_exp(at_exp),
      direction,
      equality: Exp.fresh(Var(equality)),
      instantiation: None,
    });

  let algebrite_step =
      (~at_idx: int, ~at_exp: Exp.t, ~with_exp: Exp.t): TermBase.Proof.term =>
    AlgebriteStep({
      at_idx: index(at_idx),
      at_exp: embed_exp(at_exp),
      with_exp: embed_exp(with_exp),
    });

  let eval_step = (~at_idx: int, ~at_exp: Exp.t): TermBase.Proof.term =>
    EvalStep({
      at_idx: index(at_idx),
      at_exp: embed_exp(at_exp),
    });

  /* The picker writes the plain forms; `with` clauses (`contradiction`,
   * `revert`) and `as` names (`induction`) are typed in afterwards. */
  let contradiction = (~exp: Exp.t): TermBase.Proof.term =>
    Contradiction(embed_exp(exp), None);

  /* With no cases: the user adds them in the induction row. */
  let induction = (~scrut: option(Exp.t)): TermBase.Proof.term =>
    Induction(
      scrut
      |> Option.map(embed_exp)
      |> Option.value(~default=Exp.fresh(EmptyHole)),
      None,
      [],
    );

  let forall = (): TermBase.Proof.term =>
    Forall(Pat.fresh(EmptyHole), Proof.fresh(EmptyHole));

  /* The wrapping forms start with a hole for a body, so inserting one
   * never discards proof text: the hole it replaces reappears inside the
   * new scope. Because `;` prints flat, inserting one before a step
   * reparses with that step and the rest of the chain inside the new
   * scope, which is the intended reading. */

  /* `binder` is the goal's left-hand lambda's own parameter, when it has
   * one: proving `(fun x -> x + 1) == (fun x -> 1 + x)`, the user has
   * already named the argument. */
  let funext = (~binder: option(Pat.t)): TermBase.Proof.term =>
    Funext(
      binder |> Option.value(~default=Pat.fresh(EmptyHole)),
      Proof.fresh(EmptyHole),
    );

  let assume = (~exp: Exp.t): TermBase.Proof.term =>
    Assume(embed_exp(exp), None, Proof.fresh(EmptyHole));

  let revert = (~exp: Exp.t): TermBase.Proof.term =>
    Revert(embed_exp(exp), None, Proof.fresh(EmptyHole));

  let generalize = (~exp: Exp.t): TermBase.Proof.term =>
    Generalize(embed_exp(exp), Proof.fresh(EmptyHole));

  /* `have <exp> proof ? => ?`: the subproof starts as a hole too, the
   * shape the obligations panel's "Prove here" writes. */
  let have = (~exp: Exp.t): TermBase.Proof.term =>
    Have(embed_exp(exp), Proof.fresh(EmptyHole), Proof.fresh(EmptyHole));

  /* The id of a form's argument, child 0 of its tile. `embed_exp`
   * freshens ids, so a caller can only read this back off the term. The
   * printer stamps it onto the argument's pieces, so it is also where the
   * caret lands to type into a freshly inserted form. */
  let rec arg_id = (t: TermBase.Proof.term): option(Id.t) =>
    switch (t) {
    | Parens(inner) => arg_id(inner.term)
    | Assume(e, _, _)
    | Alias(_, e, _)
    | Revert(e, _, _)
    | Generalize(e, _)
    | Have(e, _, _) => Some(Exp.rep_id(e))
    | Funext(p, _) => Some(Pat.rep_id(p))
    | EmptyHole
    | Invalid(_)
    | MultiHole(_)
    | Seq(_, _)
    | Induction(_, _, _)
    | Forall(_, _)
    | AxiomStep(_)
    | AlgebriteStep(_)
    | Contradiction(_, _)
    | EvalStep(_) => None
    };
};

/* Edits to the proof syntax, the only way the stepper changes a proof. */
module Patches = {
  type patch = EditorTransform.patch;

  /* Where a step picked on `row` goes: in place of a hole, before a
   * step, or after the chain's last step. A hole is replaced rather than
   * extended: it stands for "the proof continues here", not for a step of
   * its own. */
  let write = (row: StepperRows.t, step: Proof.t): patch =>
    switch (row.kind) {
    | Hole =>
      EditorTransform.mk_proof_patch(~target_id=Proof.rep_id(row.step), step)
    /* `Seq(new, before)` nests left inside the enclosing chain until the
     * reparse flattens it. */
    | Step(_) =>
      EditorTransform.mk_proof_patch(
        ~target_id=Proof.rep_id(row.step),
        Proof.fresh(Seq(step, row.step)),
      )
    | End =>
      EditorTransform.mk_proof_patch(
        ~target_id=Proof.rep_id(row.step),
        Proof.fresh(Seq(row.step, step)),
      )
    };

  /* Splices the step out of its chain, `;` and all, or leaves a hole
   * when it is the whole proof, so the text reads as if the step had
   * never been written. */
  let remove = (step: Proof.t): patch =>
    EditorTransform.mk_proof_remove_patch(~target_id=Proof.rep_id(step));

  /* The repair for an axiom step that doesn't apply: the same step in
   * the other direction. */
  let flip_direction = (step: Proof.t): option(patch) =>
    switch (step.term) {
    | AxiomStep({at_idx, at_exp, direction, equality, instantiation}) =>
      Some(
        EditorTransform.mk_proof_patch(
          ~target_id=Proof.rep_id(step),
          {
            ...step,
            term:
              AxiomStep({
                at_idx,
                at_exp,
                direction: Direction.toggle(direction),
                equality,
                instantiation,
              }),
          },
        ),
      )
    | _ => None
    };

  /* Adding or removing an induction's case rewrites the whole node. */
  let set_cases =
      (step: Proof.t, cases: list((Pat.t, Proof.t))): option(patch) =>
    switch (step.term) {
    | Induction(scrut, as_name, _) =>
      Some(
        EditorTransform.mk_proof_patch(
          ~target_id=Proof.rep_id(step),
          Proof.fresh(Induction(scrut, as_name, cases)),
        ),
      )
    | _ => None
    };
};

// Goals

/* The `eval` steps the picker offers. The checker's `eval` step passes
 * through `VarLookup`s silently (Evaluator.Proofs.advance_hidden), so a
 * step can't always be written at its own node: in `all_lists(xs) ==
 * true` the only step is the lookup of `all_lists`. Each step is offered
 * as the step the checker takes at the nearest node around it where it
 * takes one, here `all_lists(xs)`, and that node is the step's source. */
module EvalSteps = {
  /* Past this many silent steps nothing is offered, so a goal whose
   * hidden steps diverge can't hang the stepper. */
  let max_silent_steps = 64;

  let checker_step = (~env, e: Exp.t): option(EvaluatorStep.step) => {
    open OptUtil.Syntax;
    let* (_, e) =
      Evaluator.Proofs.advance_hidden(~max_steps=max_silent_steps, ~env, e);
    switch (EvaluatorStep.get_status(~settings=CoreSettings.on, e, env)) {
    | AvailableSteps([s, ..._]) => Some(s)
    | AvailableSteps([])
    | AutoStep(_) => None
    };
  };

  /* The `n` for which the checker's `eval <at_exp> at <n> end` finds
   * `at_exp` itself rather than an equal node before it. The checker
   * matches env-substituted terms (Occurrence.nth_env_target), so
   * `substituted` is the written goal with `env` substituted.
   *
   * None if `at_exp` holds a negative number. Evaluation produces those,
   * but source can't spell them: written out, `-5` reads back as a
   * negation, which the checker doesn't match against the number. */
  let checker_idx = (~env, ~substituted: Exp.t, at_exp: Exp.t): option(int) => {
    let rec has_negative_number = (e: Exp.t) =>
      switch (Exp.term_of(e)) {
      | Atom(Int(n)) => Bigint.compare(n, Bigint.of_int(0)) < 0
      | Atom(SInt(n)) => n < 0
      | Atom(Float(f)) => f < 0.0
      | _ => List.exists(has_negative_number, Occurrence.children(e))
      };
    let target = Substitution.in_exp(env, at_exp);
    let rec find = n =>
      switch (Occurrence.nth(target, n, substituted)) {
      | Some(found) when Exp.rep_id(found) == Exp.rep_id(at_exp) => Some(n)
      | Some(_) => find(n + 1)
      | None => None
      };
    has_negative_number(at_exp) ? None : find(0);
  };

  /* The steps to offer on the written goal, each with its source. A step
   * with no source the checker can find isn't offered, and steps with the
   * same source are offered once. */
  let offered =
      (~settings: CoreSettings.t, ~env, written: Exp.t)
      : list((EvaluatorStep.step, Id.t)) => {
    let substituted = lazy(Substitution.in_exp(env, written));
    /* The node with `id` and the nodes enclosing it, nearest first. */
    let rec self_and_ancestors = (id, e: Exp.t): option(list(Exp.t)) =>
      Exp.rep_id(e) == id
        ? Some([e])
        : Occurrence.children(e)
          |> List.find_map(self_and_ancestors(id))
          |> Option.map(path => path @ [e]);
    let offer = (s: EvaluatorStep.step) =>
      self_and_ancestors(EvaluatorStep.get_step_id(s), written)
      |> Option.value(~default=[])
      |> List.find_map(at_exp =>
           switch (checker_step(~env, at_exp)) {
           | Some(step)
               when
                 checker_idx(
                   ~env,
                   ~substituted=Lazy.force(substituted),
                   at_exp,
                 )
                 != None =>
             Some((step, Exp.rep_id(at_exp)))
           | _ => None
           }
         );
    let candidates =
      switch (EvaluatorStep.get_status(~settings, written, env)) {
      | AutoStep(s) => [s]
      | AvailableSteps(steps) => steps
      };
    candidates
    |> List.filter_map(offer)
    |> ListUtil.dedup_f(((_, id), (_, id')) => id == id');
  };
};

/* A row's goal, prepared for picking: its editor, which holds the
 * selection, and the steps on offer. Preparing one is expensive, so the
 * stepper keeps it with the goal it was made from (see `is_for`).
 *
 * The checker keeps goals as written (`sum == fold_left(_, sum, 0)`),
 * with the program's names as Vars. The row shows the goal with the env
 * substituted, which turns the names into the functions the env holds,
 * printed folded (`<sum>`). Once only: a second pass would unfold the
 * recursive calls inside the inlined bodies. Eval steps work on the
 * written form, where an inlined `FixF`, which isn't a value, doesn't
 * hide every steppable place behind its unwrap. */
module Goal = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    written: Exp.t,
    ctx: SemanticCtx.t,
    display: Exp.t,
    editor: CodeSelectable.Model.t,
    /* Each offered step, with the node of `written` that taking it
     * writes as `eval <node> at <n> end`. */
    eval_steps: list((EvaluatorStep.step, Id.t)),
    /* Equalities in `display` that reflexivity closes. */
    refls: list(Exp.t),
  };

  let env = (goal: t) => SemanticCtx.get_env(goal.ctx);

  let display_of = (~ctx: SemanticCtx.t, written: Exp.t): Exp.t =>
    Substitution.in_exp(SemanticCtx.get_env(ctx), written);

  /* Map a step's source from the written goal onto the displayed one, so
   * underlines land on `<sum>` rather than a vanished `Var`. */
  let display_id = (~written: Exp.t, ~display: Exp.t, source: Id.t): Id.t =>
    Occurrence.map_rep_id(~from=written, ~onto=display, source)
    |> Option.value(~default=source);

  let mk = (~settings: CoreSettings.t, ~ctx: SemanticCtx.t, written: Exp.t): t => {
    let env = SemanticCtx.get_env(ctx);
    let display = display_of(~ctx, written);
    let editor =
      CodeSelectable.Model.mk_from_exp(~settings, ~root=Exp, display)
      |> CodeEditable.Update.calculate(
           ~settings,
           ~is_edited=true,
           ~is_dynamic_term=true,
           ~dynamics=Dynamics.Map.empty,
           ~stitch=x => x,
           ~ctx=SemanticCtx.get_ctx(ctx),
         );
    let eval_steps = EvalSteps.offered(~settings, ~env, written);
    let step_ids =
      List.map(
        ((_, id)) => display_id(~written, ~display, id),
        eval_steps,
      );
    let refls =
      MatchExp.find_refls(
        ~info_map=CodeEditable.Model.get_statics(editor).info_map,
        ~env,
        display,
      )
      |> List.filter(e => !List.mem(Exp.rep_id(e), step_ids));
    {
      written,
      ctx,
      display,
      editor,
      eval_steps,
      refls,
    };
  };

  /* Whether `goal` shows `written` in `ctx`. Comparing the displayed
   * forms catches a changed definition the written goal mentions. */
  let is_for = (goal: t, ~ctx: SemanticCtx.t, written: Exp.t): bool =>
    Exp.fast_equal(goal.written, written)
    && Exp.fast_equal(goal.display, display_of(~ctx, written));

  let info_map = (goal: t): Statics.Map.t =>
    CodeEditable.Model.get_statics(goal.editor).info_map;

  /* The displayed ids of the offered steps, where their underlines go. */
  let step_ids = (goal: t): list(Id.t) =>
    List.map(
      ((_, id)) =>
        display_id(~written=goal.written, ~display=goal.display, id),
      goal.eval_steps,
    );

  /* The node the user selected in the goal's editor, read off the editor
   * itself so it can't lag the selection. */
  let selected = (goal: t): option(Exp.t) => {
    let editor = goal.editor.editor;
    TermData.get_root_id_using_ranges(
      editor.state.zipper.selection.content,
      editor.syntax.term_data,
      CachedSyntax.measured(editor.syntax),
    )
    |> Option.bind(_, id => Exp.find_by_id(id, goal.display));
  };
};

// Picking

/* The forms that cite an in-scope fact, picked from a list. */
[@deriving (show({with_path: false}), sexp, yojson)]
type fact_form =
  | Revert
  | Contradiction;

let keyword = (f: fact_form): string =>
  switch (f) {
  | Revert => "revert"
  | Contradiction => "contradiction"
  };

/* The boxes a picker button opens below the buttons. */
[@deriving (show({with_path: false}), sexp, yojson)]
type box =
  | Axioms
  | Rewrite
  | Facts(fact_form);

type action =
  | Write(TermBase.Proof.term)
  | Open(box);

/* What taking the `idx`-th offered step writes: `eval <at_exp> at <n>
 * end`. */
let take_step = (goal: Goal.t, idx: int): option(TermBase.Proof.term) => {
  open OptUtil.Syntax;
  let* (_, source) = List.nth_opt(goal.eval_steps, idx);
  let* at_exp = Exp.find_by_id(source, goal.written);
  let env = Goal.env(goal);
  let+ at_idx =
    EvalSteps.checker_idx(
      ~env,
      ~substituted=Substitution.in_exp(env, goal.written),
      at_exp,
    );
  Terms.eval_step(~at_idx, ~at_exp);
};

/* Reflexivity is the `refl_eq` axiom at the equality `at_exp`. */
let refl = (goal: Goal.t, at_exp: Exp.t): option(TermBase.Proof.term) =>
  Occurrence.index(at_exp, goal.display)
  |> Option.map(at_idx =>
       Terms.axiom_step(
         ~at_idx,
         ~at_exp,
         ~direction=Direction.Right,
         ~equality="refl_eq",
       )
     );

/* The buttons the picker offers for the selection `selected`, as labels
 * and actions. A button only picks, never edits: a form with an argument
 * to search (revert and contradiction match an in-scope fact) opens a
 * pick list, and one with nothing to search (assume, generalize and have
 * take any proposition) writes the step with a hole argument for the
 * user to type into. */
let offers =
    (goal: Goal.t, ~selected: option(Exp.t)): list((string, action)) => {
  let display = goal.display;
  let is_selected = (e: Exp.t) =>
    switch (selected) {
    | Some(s) => Exp.rep_id(s) == Exp.rep_id(e)
    | None => false
    };
  let step =
    switch (
      List.find_index(
        id => Option.map(Exp.rep_id, selected) == Some(id),
        Goal.step_ids(goal),
      )
      |> Option.bind(_, take_step(goal))
    ) {
    | Some(term) => [("Step", Write(term))]
    | None => []
    };
  let refl =
    switch (
      List.find_opt(is_selected, goal.refls) |> Option.bind(_, refl(goal))
    ) {
    | Some(term) => [("Reflexivity", Write(term))]
    | None => []
    };
  let function_body =
    is_selected(display) && Exp.is_fun(display)
      ? [("Function Body", Write(Terms.forall()))] : [];
  /* Exactly when the goal is `f == g` at an arrow type, by the checker's
   * own detector, so the button never offers a step that would mark.
   * The arrow type keeps it off `==` between lists of functions. */
  let info_map = Goal.info_map(goal);
  let env = Goal.env(goal);
  let funext =
    ProofCheck.Funext.goal(~info_map, ~env, display) == None
      ? []
      : [
        (
          "Funext",
          Write(
            Terms.funext(
              ~binder=ProofCheck.Funext.binder(~info_map, ~env, display),
            ),
          ),
        ),
      ];
  /* With goal `A ==> B`, assuming exactly A strips the antecedent. */
  let assume_antecedent =
    switch (Exp.term_of(display)) {
    | BinOp(Bool(Implies), a, _) => [
        ("Assume antecedent", Write(Terms.assume(~exp=a))),
      ]
    | _ => []
    };
  let hole = () => Exp.fresh(EmptyHole);
  step
  @ refl
  @ function_body
  @ funext
  @ [
    ("Algebra ▼", Open(Rewrite)),
    ("Assumptions ▼", Open(Axioms)),
    ("Assume", Write(Terms.assume(~exp=hole()))),
  ]
  @ assume_antecedent
  @ [
    ("Revert ▼", Open(Facts(Revert))),
    ("Generalize", Write(Terms.generalize(~exp=hole()))),
    ("Have", Write(Terms.have(~exp=hole()))),
    ("Contradiction ▼", Open(Facts(Contradiction))),
    ("Cases/Induction", Write(Terms.induction(~scrut=selected))),
  ];
};

/* The rules the Assumptions box lists. With no filter typed, those that
 * rewrite the selection; with one, those whose names match it. Bare
 * boolean facts read as `P == true` here, once, so the list and the box
 * agree on what each rule rewrites. The checker's axiom step makes the
 * same call, so a rule shown as applicable is one the checker accepts. */
let rules =
    (goal: Goal.t, ~filter: string, ~selected: option(Exp.t))
    : list(ProofCtx.entry) => {
  let info_map = Goal.info_map(goal);
  ProofCtx.of_theorem_ctx(~builtins=Axioms.v, SemanticCtx.get_ctx(goal.ctx))
  |> List.filter(({name, _}: ProofCtx.entry) =>
       filter == "" || StringUtil.subseq_search(name, filter)
     )
  |> List.map((entry: ProofCtx.entry) =>
       {
         ...entry,
         rule:
           ProofRule.BoolReading.apply(~info_map=Some(info_map), entry.rule),
       }
     )
  |> List.filter((entry: ProofCtx.entry) =>
       filter != ""
       || (
         switch (selected) {
         | Some(e) =>
           ProofRule.Rewrite.is_active(
             ~info_map,
             ~env=Goal.env(goal),
             entry.rule,
             DHExp.strip_ascriptions(e),
           )
         | None => false
         }
       )
     );
};

/* The in-scope facts `revert` and `contradiction` can cite: the rules of
 * the Assumptions box without the builtin axioms (rules, not facts of
 * this scope) and captured entries. Both match a fact by
 * `Exp.fast_equal` (ProofCheck.Discharge.lookup), so typing one is error
 * prone. */
let facts = (ctx: SemanticCtx.t): list(ProofCtx.entry) =>
  ProofCtx.of_theorem_ctx(~builtins=Axioms.v, SemanticCtx.get_ctx(ctx))
  |> List.filter((entry: ProofCtx.entry) =>
       !entry.is_captured
       && !List.exists((a: ProofCtx.entry) => a.name == entry.name, Axioms.v)
     );

let pick_fact = (form: fact_form, exp: Exp.t): TermBase.Proof.term =>
  switch (form) {
  | Revert => Terms.revert(~exp)
  | Contradiction => Terms.contradiction(~exp)
  };
