open Util;
open Language;
open Haz3lcore;

/* What the write box offers and writes: the steps a row's goal can take,
 * what Tab takes, the strategies, and the proof terms and patches they
 * write. Everything here is a pure function of the row's goal, its scope
 * and the selection, so it can be tested without the UI. */

// Proof Terms

/* The proof terms the stepper writes. */
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

  /* The stepper writes the plain forms; `with` clauses (`contradiction`,
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

/* The node with `id` and the nodes enclosing it, nearest first. */
let rec self_and_ancestors = (id, e: Exp.t): option(list(Exp.t)) =>
  Exp.rep_id(e) == id
    ? Some([e])
    : Occurrence.children(e)
      |> List.find_map(self_and_ancestors(id))
      |> Option.map(path => path @ [e]);

/* The `eval` steps the stepper offers. The checker's `eval` step passes
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

/* A row's goal, prepared for stepping: its editor, which holds the
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

  /* Where the underline marking what `row`'s step acts on goes: the
   * displayed id of that part. A goal carried over from an earlier check
   * prints the same as `row.goal` but may not share its ids, so this maps
   * by position from `row.goal` rather than from `written`.
   *
   * The editor may not draw that node. Inside a folded function it is in
   * the editor's term but not drawn, so the underline goes on the nearest
   * drawn node around it there, the fold. A hidden ascription isn't in the
   * editor's term at all, so the underline goes on the first drawn node
   * inside it. */
  let acts_on = (goal: t, row: StepperRows.t): option(Id.t) => {
    let syntax = goal.editor.editor.syntax;
    let drawn = (e: Exp.t) =>
      TermData.extreme_measures(
        Exp.rep_id(e),
        syntax.term_data,
        CachedSyntax.measured(syntax),
      )
      != None;
    let rec inside = (e: Exp.t) =>
      drawn(e)
        ? Some(e)
        : (
          switch (Occurrence.children(e)) {
          | [child] => inside(child)
          | _ => None
          }
        );
    let editor_term = CodeEditable.Model.get_statics(goal.editor).term;
    switch (row.goal, row.acts_on) {
    | (Some(written), Some(part)) =>
      Occurrence.map_rep_id(~from=written, ~onto=goal.display, part)
      |> Option.bind(_, id =>
           switch (self_and_ancestors(id, editor_term)) {
           | Some(path) => List.find_opt(drawn, path)
           | None => Option.bind(Exp.find_by_id(id, goal.display), inside)
           }
         )
      |> Option.map(Exp.rep_id)
    | (None, _)
    | (_, None) => None
    };
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

/* A step to write, and where it acts: on the row's whole goal, or through
 * a focus on one occurrence in it. */
[@deriving (show({with_path: false}), sexp, yojson)]
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

/* The patch writing `pick` as a row of its own after `row`'s goal. */
let write = (row: StepperRows.t, pick: pick): Patches.patch =>
  Patches.write(row, focused(pick));

/* The patch writing `steps` after `row`'s goal, one row each, or None
 * for no steps. */
let write_steps =
    (row: StepperRows.t, steps: list(Proof.t)): option(Patches.patch) =>
  switch (List.rev(steps)) {
  | [] => None
  | [last, ...earlier] =>
    Some(
      Patches.write(
        row,
        List.fold_left(
          (rest, step) => Proof.fresh(Seq(step, rest)),
          last,
          earlier,
        ),
      ),
    )
  };

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

/* The goal `proof` leaves, checked from `goal` alone rather than as part
 * of its theorem, or None if the checker marks any step of it. */
let outgoing = (goal: Goal.t, written: Exp.t, proof: Proof.t): option(Exp.t) => {
  let checked =
    Evaluator.Proofs.check_proof(
      ~statics=Goal.info_map(goal),
      ~ctx=goal.ctx,
      ~goal=written,
      proof,
    );
  ProofMap.error_ids(checked) == []
    ? Option.bind(ProofMap.lookup(Proof.rep_id(proof), checked), e =>
        e.outgoing
      )
    : None;
};

/* A scope's citable rules: its own facts and theorems, then the builtin
 * axioms, which `ProofCtx.of_theorem_ctx` lists last. */
let scope_and_axioms =
    (ctx: SemanticCtx.t): (list(ProofCtx.entry), list(ProofCtx.entry)) => {
  let all =
    ProofCtx.of_theorem_ctx(~builtins=Axioms.v, SemanticCtx.get_ctx(ctx));
  ListUtil.split_n(List.length(all) - List.length(Axioms.v), all);
};

// The write box

/* What Tab takes in an empty write box: reflexivity when it closes the
 * goal, or else the goal's next eval step. With part of the goal selected,
 * the step is the selection's own, and the ghost is what the selection
 * becomes. */
module Ghost = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    /* The rule's name, shown beside the Tab key. */
    rule: string,
    pick,
    /* As the row would show it. */
    result: Exp.t,
  };

  let mk = (goal: Goal.t, ~selected: option(Exp.t)): option(t) => {
    open OptUtil.Syntax;
    /* A step on `(1 + 1)` has the parens as its source, so a selection of
     * `1 + 1` takes in the parens around it. */
    let rec widen = (node, ancestors) =>
      switch (ancestors) {
      | [parens, ...rest] =>
        switch (Exp.term_of(parens)) {
        | Parens(_) => widen(parens, rest)
        | _ => node
        }
      | [] => node
      };
    let node =
      switch (selected) {
      | Some(s) =>
        switch (self_and_ancestors(Exp.rep_id(s), goal.display)) {
        | Some([_, ...ancestors]) => widen(s, ancestors)
        | Some([])
        | None => s
        }
      | None => goal.display
      };
    let is_node = (e: Exp.t) => Exp.rep_id(e) == Exp.rep_id(node);
    let inside = id => Exp.find_by_id(id, node) != None;
    let candidate =
      List.exists(is_node, goal.refls)
        ? Some(("refl", refl(goal, node)))
        : List.find_index(inside, Goal.step_ids(goal))
          |> Option.bind(_, take_step(goal))
          |> Option.map(pick => ("eval", pick));
    let* (rule, pick) = candidate;
    let+ written = outgoing(goal, goal.written, focused(pick));
    let display = Goal.display_of(~ctx=goal.ctx, written);
    {
      rule,
      pick,
      result:
        Occurrence.map_rep_id(
          ~from=goal.display,
          ~onto=display,
          Exp.rep_id(node),
        )
        |> Option.bind(_, id => Exp.find_by_id(id, display))
        |> Option.value(~default=display),
    };
  };
};

/* The Proof strategy tab: the goal's own introduction, then the
 * strategies any goal can use. One with something to choose, a variable
 * or a fact, offers the choices in a menu, ending with a hole to type
 * into. */
module Strategies = {
  type choice = {
    label: Exp.t,
    term: TermBase.Proof.term,
  };

  type strategy = {
    label: string,
    /* What it writes and what for. */
    tooltip: string,
    /* Shown beside the label, as Assume shows the antecedent it assumes. */
    argument: option(Exp.t),
    writes,
  }
  and writes =
    | Writes(TermBase.Proof.term)
    | Chooses(list(choice));

  let intros = (goal: Goal.t): list(strategy) => {
    let display = goal.display;
    let info_map = Goal.info_map(goal);
    let env = Goal.env(goal);
    let forall =
      Exp.is_fun(display)
        ? [
          {
            label: "Forall",
            tooltip: "Writes `forall x =>`: names the argument, to prove the body for any x",
            argument: None,
            writes: Writes(Terms.forall()),
          },
        ]
        : [];
    /* Exactly when the goal is `f == g` at an arrow type, by the checker's
     * own detector, so it never writes a step that would mark. */
    let funext =
      ProofCheck.Funext.goal(~info_map, ~env, display) == None
        ? []
        : [
          {
            label: "Funext",
            tooltip: "Writes `funext x =>`: proves the functions equal at any argument x",
            argument: None,
            writes:
              Writes(
                Terms.funext(
                  ~binder=ProofCheck.Funext.binder(~info_map, ~env, display),
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
            tooltip: "Writes `assume A =>`: adds A as a fact and proves the rest",
            argument: Some(a),
            writes: Writes(Terms.assume(~exp=a)),
          },
        ]
      | _ => []
      };
    forall @ funext @ assume;
  };

  let general = (goal: Goal.t): list(strategy) => {
    let ctx = SemanticCtx.get_ctx(goal.ctx);
    let hole = Exp.fresh(EmptyHole);
    let choose = (term, labels) =>
      Chooses(
        List.map(
          label =>
            {
              label,
              term: term(~exp=label),
            },
          labels,
        )
        @ [
          {
            label: hole,
            term: term(~exp=hole),
          },
        ],
      );
    let rec nodes = (e: Exp.t) => [
      e,
      ...List.concat_map(nodes, Occurrence.children(e)),
    ];
    let distinct = ListUtil.dedup_f(Exp.fast_equal);
    /* The goal's variables that name values in scope, not functions. */
    let variables =
      nodes(goal.written)
      |> List.filter_map((e: Exp.t) =>
           switch (Exp.term_of(e)) {
           | Var(x) =>
             Ctx.lookup_var(ctx, x)
             |> Option.map((entry: Ctx.var_entry) =>
                  (e, Typ.weak_head_normalize(ctx, entry.typ))
                )
           | _ => None
           }
         )
      |> List.filter(((_, ty)) =>
           switch (Typ.term_of(ty)) {
           | Arrow(_) => false
           | _ => true
           }
         );
    let inductive =
      variables
      |> List.filter(((_, ty)) =>
           switch (Typ.term_of(ty)) {
           | List(_)
           | Sum(_)
           | Rec(_) => true
           | _ => false
           }
         )
      |> List.map(fst)
      |> distinct;
    /* What a case split can split on: a variable, or a call. */
    let splittable =
      nodes(goal.written)
      |> List.filter((e: Exp.t) =>
           switch (Exp.term_of(e)) {
           | Ap(_) => true
           | _ => false
           }
         )
      |> (calls => List.map(fst, variables) @ calls)
      |> distinct;
    let facts =
      fst(scope_and_axioms(goal.ctx))
      |> List.filter((entry: ProofCtx.entry) => !entry.is_captured)
      |> List.map((entry: ProofCtx.entry) => entry.exp)
      |> distinct;
    let induction = (~exp) => Terms.induction(~scrut=Some(exp));
    [
      {
        label: "Induction",
        tooltip: "Writes `induction x end`: one case per form of x, each with its inductive hypotheses",
        argument: None,
        writes: choose(induction, inductive),
      },
      {
        label: "Cases",
        tooltip: "Writes `induction e end` on any expression: one case per form of its value",
        argument: None,
        writes: choose(induction, splittable),
      },
      {
        label: "Have",
        tooltip: "Writes `have ? proof ? =>`: proves a stepping-stone fact first, then uses it here",
        argument: None,
        writes: Writes(Terms.have(~exp=hole)),
      },
      {
        label: "Contradiction",
        tooltip: "Writes `contradiction <fact> end`: closes an impossible case with a fact that evaluates to false",
        argument: None,
        writes: choose(Terms.contradiction, facts),
      },
      {
        label: "Generalize",
        tooltip: "Writes `generalize x =>`: makes x arbitrary again, a forall in the goal, e.g. before induction",
        argument: None,
        writes:
          choose(Terms.generalize, List.map(fst, variables) |> distinct),
      },
      {
        label: "Revert",
        tooltip: "Writes `revert <fact> =>`: moves a fact back into the goal as fact ==> goal, e.g. before induction",
        argument: None,
        writes: choose(Terms.revert, facts),
      },
    ];
  };
};
