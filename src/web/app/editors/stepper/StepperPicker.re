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

  let axiom_step =
      (~direction: Direction.t, ~equality: string): TermBase.Proof.term =>
    AxiomStep({
      direction,
      equality: Exp.fresh(Var(equality)),
      instantiation: None,
    });

  let algebrite_step = (~replacement: Exp.t): TermBase.Proof.term =>
    AlgebriteStep(embed_exp(replacement));

  /* `from <selector> do <step> end`, selecting the match of `selector`
   * with 0-based `index`. */
  let focus =
      (~selector: Exp.t, ~index: int, step: Proof.t): TermBase.Proof.term =>
    Focus({
      ordinal: index == 0 ? None : Some(IdTagged.fresh(index + 1)),
      selector: embed_exp(selector),
      body: step,
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
    | EvalStep
    | Focus(_) => None
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

  /* Adds `step` to the end of a focus's steps, given its body. A body
   * that is still a hole is replaced. */
  let join = (body: Proof.t, step: Proof.t): patch =>
    EditorTransform.mk_proof_patch(
      ~target_id=Proof.rep_id(body),
      switch (StepperRows.unparen(body).term) {
      | EmptyHole => step
      | _ => Proof.fresh(Seq(body, step))
      },
    );

  /* The repair for an axiom step that doesn't apply: the same step in
   * the other direction. A one-step focus is drawn as its step, so it
   * offers its step's repair. */
  let rec flip_direction = (step: Proof.t): option(patch) =>
    switch (step.term, StepperRows.single_step(step)) {
    | (AxiomStep({direction, equality, instantiation}), _) =>
      Some(
        EditorTransform.mk_proof_patch(
          ~target_id=Proof.rep_id(step),
          {
            ...step,
            term:
              AxiomStep({
                direction: Direction.toggle(direction),
                equality,
                instantiation,
              }),
          },
        ),
      )
    | (_, Some(inner)) => flip_direction(inner)
    | (_, None) => None
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

  /* The `n` for which the checker's `from <n+1>th <node> do` finds
   * `node` itself rather than an equal node before it. The checker
   * matches env-substituted terms (Occurrence.nth_env_target), so
   * `substituted` is the written goal with `env` substituted.
   *
   * None if `node` holds a negative number. Evaluation produces those,
   * but source can't spell them: written out, `-5` reads back as a
   * negation, which the checker doesn't match against the number. */
  let checker_idx = (~env, ~substituted: Exp.t, node: Exp.t): option(int) => {
    let rec has_negative_number = (e: Exp.t) =>
      switch (Exp.term_of(e)) {
      | Atom(Int(n)) => Bigint.compare(n, Bigint.of_int(0)) < 0
      | Atom(SInt(n)) => n < 0
      | Atom(Float(f)) => f < 0.0
      | _ => List.exists(has_negative_number, Occurrence.children(e))
      };
    let target = Substitution.in_exp(env, node);
    let rec find = n =>
      switch (Occurrence.nth(target, n, substituted)) {
      | Some(found) when Exp.rep_id(found) == Exp.rep_id(node) => Some(n)
      | Some(_) => find(n + 1)
      | None => None
      };
    has_negative_number(node) ? None : find(0);
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
      |> List.find_map(node =>
           switch (checker_step(~env, node)) {
           | Some(step)
               when
                 checker_idx(
                   ~env,
                   ~substituted=Lazy.force(substituted),
                   node,
                 )
                 != None =>
             Some((step, Exp.rep_id(node)))
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
     * focuses on to write `eval`. */
    eval_steps: list((EvaluatorStep.step, Id.t)),
    /* Equalities in `display` that reflexivity closes. */
    refls: list(Exp.t),
    /* What the goal was built with: how it prints, and which steps it
     * offers. */
    settings: CoreSettings.t,
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
      settings,
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

  /* The displayed id of the occurrence `row`'s focus selects, where its
   * underline goes. A goal carried over from an earlier check prints the
   * same as `row.goal` but may not share its ids, so this maps by position
   * from `row.goal` rather than from `written`. */
  let focus_id = (goal: t, row: StepperRows.t): option(Id.t) =>
    switch (row.goal, row.focus_target) {
    | (Some(written), Some(target)) =>
      Occurrence.map_rep_id(~from=written, ~onto=goal.display, target)
    | (None, _)
    | (_, None) => None
    };

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

  /* `goal` built again with `settings`. The node that was selected is
   * selected again, or the caret goes back to the node it was at.
   * Substitution gives what it inlines fresh ids each time, so the node is
   * found by its position in the display. */
  let rebuild = (~settings: CoreSettings.t, goal: t): t => {
    let rebuilt = mk(~settings, ~ctx=goal.ctx, goal.written);
    let zipper = goal.editor.editor.state.zipper;
    let (node, restore) =
      switch (selected(goal)) {
      | Some(node) => (
          Some(Exp.rep_id(node)),
          (id => Action.Select(Term(Id(id, zipper.selection.focus)))),
        )
      | None => (
          Indicated.index(zipper),
          (id => Action.Move(Goal(TileId(id)))),
        )
      };
    let moved =
      Option.bind(
        node,
        id => {
          let id =
            Occurrence.map_rep_id(
              ~from=goal.display,
              ~onto=rebuilt.display,
              id,
            )
            |> Option.value(~default=id);
          Editor.Update.update(
            ~settings,
            restore(id),
            rebuilt.editor.statics,
            rebuilt.editor.dynamics,
            rebuilt.editor.editor,
          )
          |> Stdlib.Result.to_option;
        },
      );
    switch (moved) {
    | Some(editor) => {
        ...rebuilt,
        editor: {
          ...rebuilt.editor,
          editor,
        },
      }
    | None => rebuilt
    };
  };
};

// Picking

/* The boxes a picker button opens below the buttons. */
[@deriving (show({with_path: false}), sexp, yojson)]
type box =
  | Rewrite
  | Strategies;

/* A step the picker writes, and where it acts: on the row's whole goal,
 * or through a focus on one occurrence in it. */
type pick = {
  at: option(target),
  step: Proof.t,
}
and target = {
  /* The occurrence, a node of the goal's display. */
  node: Exp.t,
  /* What the focus writes, and which of its matches the occurrence is,
   * counting from 0. */
  selector: Exp.t,
  index: int,
};

/* Where a step picked on `node` acts: through a focus, unless `node` is
 * the whole goal. */
let target = (goal: Goal.t, node: Exp.t): option(target) =>
  Exp.rep_id(node) == Exp.rep_id(goal.display)
    ? None
    : Some({
        node,
        selector: node,
        index:
          Occurrence.index(node, goal.display) |> Option.value(~default=0),
      });

/* The proof `pick` writes on its own: its step, focused on its target. */
let focused = (pick: pick): Proof.t =>
  switch (pick.at) {
  | None => pick.step
  | Some({selector, index, _}) =>
    Proof.fresh(Terms.focus(~selector, ~index, pick.step))
  };

/* The patch writing `pick` at `row`. A step picked inside what the focus
 * just before the row rewrote joins that focus, focusing again within
 * that part if it is picked on less than all of it. So stepping in one
 * place grows one focus. */
let write = (row: StepperRows.t, goal: Goal.t, pick: pick): Patches.patch => {
  let rewrote =
    Option.bind(row.after_focus, ((body, result)) =>
      Exp.find_by_id(
        Goal.display_id(~written=goal.written, ~display=goal.display, result),
        goal.display,
      )
      |> Option.map(result => (body, result))
    );
  switch (rewrote, pick.at) {
  | (Some((body, result)), Some({node, _} as at))
      when Exp.find_by_id(Exp.rep_id(node), result) != None =>
    let within =
      Exp.rep_id(node) == Exp.rep_id(result)
        ? None
        : Some({
            ...at,
            index:
              Occurrence.index(node, result) |> Option.value(~default=0),
          });
    Patches.join(
      body,
      focused({
        ...pick,
        at: within,
      }),
    );
  | _ => Patches.write(row, focused(pick))
  };
};

/* A button: its label, a tooltip saying what it writes, and its action. */
type offer = {
  label: string,
  tooltip: string,
  action,
}
and action =
  | Write(pick)
  | Open(box);

/* What taking the `idx`-th offered step writes: `eval`, focused on the
 * step's source. */
let take_step = (goal: Goal.t, idx: int): option(pick) => {
  open OptUtil.Syntax;
  let* (_, source) = List.nth_opt(goal.eval_steps, idx);
  let* written = Exp.find_by_id(source, goal.written);
  let env = Goal.env(goal);
  let+ index =
    EvalSteps.checker_idx(
      ~env,
      ~substituted=Substitution.in_exp(env, goal.written),
      written,
    );
  let node =
    Exp.find_by_id(
      Goal.display_id(~written=goal.written, ~display=goal.display, source),
      goal.display,
    )
    |> Option.value(~default=written);
  {
    at:
      source == Exp.rep_id(goal.written)
        ? None
        : Some({
            node,
            selector: written,
            index,
          }),
    step: Proof.fresh(EvalStep),
  };
};

/* Reflexivity is the `refl_eq` axiom at the equality `node`. */
let refl = (goal: Goal.t, node: Exp.t): pick => {
  at: target(goal, node),
  step:
    Proof.fresh(
      Terms.axiom_step(~direction=Direction.Right, ~equality="refl_eq"),
    ),
};

/* The buttons the picker offers for the selection `selected`. A button
 * only picks, never edits: a form with an argument to search (a rule to
 * rewrite with, a fact to cite) opens a box to pick it from, and one with
 * nothing to search (have, generalize) writes the step with a hole
 * argument for the user to type into. */
let offers = (goal: Goal.t, ~selected: option(Exp.t)): list(offer) => {
  let display = goal.display;
  let whole = term => {
    at: None,
    step: Proof.fresh(term),
  };
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
    | Some(pick) => [
        {
          label: "Step",
          tooltip: "Writes `eval`: one evaluation step at the selection",
          action: Write(pick),
        },
      ]
    | None => []
    };
  let refl =
    switch (List.find_opt(is_selected, goal.refls)) {
    | Some(e) => [
        {
          label: "Reflexivity",
          tooltip: "Writes `axiom refl_eq`: closes an equation of equals",
          action: Write(refl(goal, e)),
        },
      ]
    | None => []
    };
  let function_body =
    is_selected(display) && Exp.is_fun(display)
      ? [
        {
          label: "Function Body",
          tooltip: "Writes `forall ? =>`: names the argument, to prove the body",
          action: Write(whole(Terms.forall())),
        },
      ]
      : [];
  /* Exactly when the goal is `f == g` at an arrow type, by the checker's
   * own detector, so the button never offers a step that would mark.
   * The arrow type keeps it off `==` between lists of functions. */
  let info_map = Goal.info_map(goal);
  let env = Goal.env(goal);
  let funext =
    ProofCheck.Funext.goal(~info_map, ~env, display) == None
      ? []
      : [
        {
          label: "Funext",
          tooltip: "Writes `funext x =>`: proves the functions equal at any x",
          action:
            Write(
              whole(
                Terms.funext(
                  ~binder=ProofCheck.Funext.binder(~info_map, ~env, display),
                ),
              ),
            ),
        },
      ];
  /* With goal `A ==> B`, assuming exactly A strips the antecedent. */
  let assume =
    switch (Exp.term_of(display)) {
    | BinOp(Bool(Implies), a, _) => [
        {
          label: "Assume",
          tooltip: "Writes `assume A =>`: takes the left side of `A ==> B` as a fact",
          action: Write(whole(Terms.assume(~exp=a))),
        },
      ]
    | _ => []
    };
  let rewrite =
    selected == None
      ? []
      : [
        {
          label: "Rewrite ▼",
          tooltip: "Rewrites the selection, by algebra or with a fact, theorem or axiom",
          action: Open(Rewrite),
        },
      ];
  step
  @ refl
  @ function_body
  @ funext
  @ assume
  @ rewrite
  @ [
    {
      label: "Strategies ▼",
      tooltip: "Have, contradiction, cases: steps that shape the rest of the proof",
      action: Open(Strategies),
    },
  ];
};

/* A scope's citable rules: its own facts and theorems, then the builtin
 * axioms, which `ProofCtx.of_theorem_ctx` lists last. */
let scope_and_axioms =
    (ctx: SemanticCtx.t): (list(ProofCtx.entry), list(ProofCtx.entry)) => {
  let all =
    ProofCtx.of_theorem_ctx(~builtins=Axioms.v, SemanticCtx.get_ctx(ctx));
  ListUtil.split_n(List.length(all) - List.length(Axioms.v), all);
};

type section = {
  heading: string,
  rules: list(ProofCtx.entry),
};

/* The rules the Rewrite box lists, by source, leaving out empty
 * sections. With no filter typed, those that rewrite the selection; with
 * one, those whose names match it. Bare boolean facts read as
 * `P == true` here, once, so the list and the box agree on what each rule
 * rewrites. The checker's axiom step makes the same call, so a rule shown
 * as applicable is one the checker accepts. */
let rules =
    (goal: Goal.t, ~filter: string, ~selected: option(Exp.t)): list(section) => {
  let info_map = Goal.info_map(goal);
  let shown = entries =>
    entries
    |> List.filter(({name, _}: ProofCtx.entry) =>
         filter == "" || StringUtil.subseq_search(name, filter)
       )
    |> List.map((entry: ProofCtx.entry) =>
         {
           ...entry,
           rule:
             ProofRule.BoolReading.apply(
               ~info_map=Some(info_map),
               entry.rule,
             ),
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
  let (scope, axioms) = scope_and_axioms(goal.ctx);
  [
    {
      heading: "Facts and theorems",
      rules: shown(scope),
    },
    {
      heading: "Axioms",
      rules: shown(axioms),
    },
  ]
  |> List.filter(section => section.rules != []);
};

/* A row of the Strategies box. */
type strategy = {
  label: string,
  /* One line on what it is for. */
  explanation: string,
  tooltip: string,
  picks,
}
and picks =
  | Writes(TermBase.Proof.term)
  /* One step per in-scope fact, by the fact's name. The forms that cite
   * a fact match it by `Exp.fast_equal` (ProofCheck.Discharge.lookup),
   * so typing one in is error prone. */
  | Cites(list((string, TermBase.Proof.term)));

/* The Strategies box, as the strategies most proofs use and those
 * rarely needed. */
let strategies =
    (goal: Goal.t, ~selected: option(Exp.t))
    : (list(strategy), list(strategy)) => {
  let hole = () => Exp.fresh(EmptyHole);
  let cites = term =>
    Cites(
      fst(scope_and_axioms(goal.ctx))
      |> List.filter((entry: ProofCtx.entry) => !entry.is_captured)
      |> List.map((entry: ProofCtx.entry) =>
           (entry.name, term(~exp=entry.exp))
         ),
    );
  (
    [
      {
        label: "Have",
        explanation: "Prove a stepping-stone fact first, then use it here.",
        tooltip: "Writes `have ? proof ? =>`",
        picks: Writes(Terms.have(~exp=hole())),
      },
      {
        label: "Contradiction",
        explanation: "Close an impossible case: pick a fact here that evaluates to false.",
        tooltip: "Writes `contradiction <fact> end`",
        picks: cites(Terms.contradiction),
      },
      {
        label: "Cases/Induction",
        explanation: "Split the proof into one case per form of the selected value.",
        tooltip: "Writes `induction <selection> end`; add the cases in its row",
        picks: Writes(Terms.induction(~scrut=selected)),
      },
    ],
    [
      {
        label: "Revert",
        explanation: "Move a fact back into the goal as fact ==> goal, e.g. before induction.",
        tooltip: "Writes `revert <fact> =>`",
        picks: cites(Terms.revert),
      },
      {
        label: "Generalize",
        explanation: "Turn a fixed variable back into a forall, e.g. before induction.",
        tooltip: "Writes `generalize ? =>`",
        picks: Writes(Terms.generalize(~exp=hole())),
      },
    ],
  );
};
