open Util;
open Language;
open Haz3lcore;
open Poly;

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

  /* The splits start with no cases: the user adds them in the split's
   * row. */
  let cases = (~exp: Exp.t): TermBase.Proof.term =>
    Cases(embed_exp(exp), []);

  let induction = (~exp: Exp.t): TermBase.Proof.term =>
    Induction(embed_exp(exp), []);

  let forall = (): TermBase.Proof.term =>
    Forall(Pat.fresh(EmptyHole), Proof.fresh(EmptyHole));

  /* The wrapping forms start with a hole for a body, so inserting one
   * never discards proof text: the hole it replaces reappears inside the
   * new scope. Because `;` prints flat, inserting one before a step
   * reparses with that step and the rest of the chain inside the new
   * scope, which is the intended reading. */
  let funext = (): TermBase.Proof.term =>
    Funext(Pat.fresh(EmptyHole), Proof.fresh(EmptyHole));

  let assume = (~exp: Exp.t): TermBase.Proof.term =>
    Assume(embed_exp(exp), None, Proof.fresh(EmptyHole));

  let revert = (~exp: Exp.t): TermBase.Proof.term =>
    Revert(embed_exp(exp), None, Proof.fresh(EmptyHole));

  let generalize = (~exp: Exp.t): TermBase.Proof.term =>
    Generalize(embed_exp(exp), Proof.fresh(EmptyHole));

  /* `have <exp> proof ? => ?`: the subproof starts as a hole too, the
   * shape the obligations panel's "Prove here" writes. */
  let have = (~exp: Exp.t): TermBase.Proof.term =>
    Have(
      embed_exp(exp),
      None,
      Proof.fresh(EmptyHole),
      Proof.fresh(EmptyHole),
    );

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
    | Have(e, _, _, _) => Some(Exp.rep_id(e))
    | Forall(p, _)
    | Funext(p, _) => Some(Pat.rep_id(p))
    | EmptyHole
    | Invalid(_)
    | MultiHole(_)
    | Seq(_, _)
    | Cases(_, _)
    | Induction(_, _)
    | AxiomStep(_)
    | AlgebriteStep(_)
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
   * the other direction, when that checks from the step's goal. A
   * one-step focus is drawn as its step, so it offers its step's repair. */
  let rec flip_direction =
          (~info_map: Statics.Map.t, ~proof_map: ProofMap.t, step: Proof.t)
          : option(patch) =>
    switch (step.term, StepperRows.single_step(step)) {
    | (AxiomStep({direction, equality, instantiation}), _) =>
      let flipped: Proof.t = {
        ...step,
        term:
          AxiomStep({
            direction: Direction.toggle(direction),
            equality,
            instantiation,
          }),
      };
      switch (ProofMap.lookup(Proof.rep_id(step), proof_map)) {
      | Some({incoming: Some(goal), ctx, _})
          when
            Evaluator.Proofs.check_proof(
              ~statics=ProofStatics.of_info_map(info_map),
              ~ctx,
              ~goal,
              flipped,
            )
            |> ProofMap.error_ids == [] =>
        Some(
          EditorTransform.mk_proof_patch(
            ~target_id=Proof.rep_id(step),
            flipped,
          ),
        )
      | _ => None
      };
    | (_, Some(inner)) => flip_direction(~info_map, ~proof_map, inner)
    | (_, None) => None
    };

  /* Adding or removing a split's case rewrites the whole node. */
  let set_cases =
      (step: Proof.t, cases: list((Pat.t, Proof.t))): option(patch) => {
    let rewrite = term =>
      Some(
        EditorTransform.mk_proof_patch(
          ~target_id=Proof.rep_id(step),
          Proof.fresh(term),
        ),
      );
    switch (step.term) {
    | Cases(scrut, _) => rewrite(Cases(scrut, cases))
    | Induction(scrut, _) => rewrite(Induction(scrut, cases))
    | _ => None
    };
  };

  /* A new case for a split whose cases don't yet cover its scrutinee, as
   * the checker's `marks` on it say. */
  let add_case = (step: Proof.t, ~marks: list(ProofMark.t)): option(patch) => {
    let uncovered =
      List.mem(marks, ProofMark.InductionNotExhaustive, ~equal=Poly.equal);
    let new_case = (Pat.fresh(EmptyHole), Proof.fresh(EmptyHole));
    switch (step.term) {
    | Cases(_, cases)
    | Induction(_, cases) when uncovered =>
      set_cases(step, cases @ [new_case])
    | _ => None
    };
  };
};

// Goals

/* The node with `id` and the nodes enclosing it, nearest first. */
let rec self_and_ancestors = (id, e: Exp.t): option(list(Exp.t)) =>
  Id.equal(Exp.rep_id(e), id)
    ? Some([e])
    : Occurrence.children(e)
      |> List.find_map(~f=self_and_ancestors(id))
      |> Option.map(~f=path => path @ [e]);

/* The `step`s the stepper offers. The checker's `step` passes
 * through `VarLookup`s silently (Evaluator.Proofs.advance_hidden), and
 * then takes a step only at the root of its goal, so a step can't always
 * be written at its own node: in `all_lists(xs) == true` the only step is
 * the lookup of `all_lists`. Each step is offered as the step the checker
 * takes at the nearest node around it where it takes one, here
 * `all_lists(xs)`, and that node is the step's source. */
module EvalSteps = {
  /* Past this many silent steps nothing is offered, so a goal whose
   * hidden steps diverge can't hang the stepper. */
  let max_silent_steps = 64;

  let checker_step = (~env, e: Exp.t): option(EvaluatorStep.step) => {
    open OptUtil.Syntax;
    let* (_, e) =
      Evaluator.Proofs.advance_hidden(~max_steps=max_silent_steps, ~env, e);
    switch (EvaluatorStep.get_status(~settings=CoreSettings.on, e, env)) {
    | AvailableSteps(steps) => List.find(~f=EvaluatorStep.is_at_root, steps)
    | AutoStep(_) => None
    };
  };

  /* The `n` for which the checker's `from <n+1>th <node> do` finds
   * `node` itself in `goal` rather than an equal node before it.
   *
   * None if `node` holds a negative number. Evaluation produces those,
   * but source can't spell them: written out, `-5` reads back as a
   * negation, which the checker doesn't match against the number. */
  let checker_idx = (goal: Exp.t, node: Exp.t): option(int) => {
    let rec has_negative_number = (e: Exp.t) =>
      switch (Exp.term_of(e)) {
      | Atom(Int(n)) => Bigint.compare(n, Bigint.of_int(0)) < 0
      | Atom(SInt(n)) => n < 0
      | Atom(Float(f)) => f < 0.0
      | _ => List.exists(~f=has_negative_number, Occurrence.children(e))
      };
    has_negative_number(node) ? None : Occurrence.index(node, goal);
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type kind =
    /* Evaluation's own step at the source, which the checker takes. */
    | Evaluation(EvaluatorStep.step)
    /* A redex evaluation takes no step at, such as a named function
     * applied to something that isn't a value yet, which `step`
     * contracts (`ProofCheck.Beta`). */
    | Contraction;

  /* A step on offer, by its source: the node that taking it focuses on
   * to write `step`. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type offer = {
    source: Id.t,
    kind,
  };

  /* The steps to offer on `goal`. A step with no source the checker can
   * find isn't offered, and steps with the same source are offered once.
   * A step at the whole goal needs no selector, so it is offered even
   * where `checker_idx` refuses one. */
  let offered = (~settings: CoreSettings.t, ~env, goal: Exp.t): list(offer) => {
    let has_selector = (node: Exp.t) =>
      Id.equal(Exp.rep_id(node), Exp.rep_id(goal))
      || checker_idx(goal, node) != None;
    let evaluation = (s: EvaluatorStep.step) =>
      self_and_ancestors(EvaluatorStep.get_step_id(s), goal)
      |> Option.value(~default=[])
      |> List.find_map(~f=node =>
           switch (checker_step(~env, node)) {
           | Some(step) when has_selector(node) =>
             Some({
               source: Exp.rep_id(node),
               kind: Evaluation(step),
             })
           | _ => None
           }
         );
    /* Every step, not the one the evaluator would take next: a hidden
     * step it would take first, such as dropping an ascription, would hide
     * the steps on every other operand. */
    let evaluations =
      EvaluatorStep.all_steps(~settings, goal, env)
      |> List.filter_map(~f=evaluation)
      |> ListUtil.dedup_f((a, b) => Id.equal(a.source, b.source));
    let contractions =
      Occurrence.in_source_order(~stop=_ => false, goal)
      |> List.filter(~f=node =>
           ProofCheck.Beta.redex(~env, node) != None
           && checker_step(~env, node) == None
           && has_selector(node)
         )
      |> List.map(~f=node =>
           {
             source: Exp.rep_id(node),
             kind: Contraction,
           }
         );
    evaluations @ contractions;
  };
};

/* A row's goal, prepared for stepping: its editor, which holds the
 * selection, and the steps on offer. Preparing one is expensive, so the
 * stepper keeps it with the goal it was made from (see `is_for`). The
 * goal is shown as the checker keeps it, naming the program's
 * definitions. */
module Goal = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    written: Exp.t,
    ctx: SemanticCtx.t,
    editor: CodeSelectable.Model.t,
    eval_steps: list(EvalSteps.offer),
    /* Equalities in `written` that reflexivity closes. */
    refls: list(Exp.t),
    /* What the goal was built with: how it prints, and which steps it
     * offers. */
    settings: CoreSettings.t,
  };

  let env = (goal: t) => SemanticCtx.get_env(goal.ctx);

  let mk = (~settings: CoreSettings.t, ~ctx: SemanticCtx.t, written: Exp.t): t => {
    let env = SemanticCtx.get_env(ctx);
    let editor =
      CodeSelectable.Model.mk_from_exp(~settings, ~root=Exp, written)
      |> CodeEditable.Update.calculate(
           ~settings,
           ~is_edited=true,
           ~is_dynamic_term=true,
           ~dynamics=Dynamics.Map.empty,
           ~stitch=x => x,
           ~ctx=SemanticCtx.get_ctx(ctx),
         );
    let eval_steps = EvalSteps.offered(~settings, ~env, written);
    let step_ids = List.map(~f=(o: EvalSteps.offer) => o.source, eval_steps);
    let refls =
      MatchExp.find_refls(
        ~statics=
          ProofStatics.of_info_map(
            CodeEditable.Model.get_statics(editor).info_map,
          ),
        ~defs=env,
        written,
      )
      |> List.filter(~f=e =>
           !List.mem(step_ids, Exp.rep_id(e), ~equal=Id.equal)
         );
    {
      written,
      ctx,
      editor,
      eval_steps,
      refls,
      settings,
    };
  };

  /* Whether `name` cites a refuted theorem in the goal's scope, which
   * the checker refuses. */
  let cites_refuted = (goal: t, name: string): bool =>
    SemanticCtx.cited_verdict(goal.ctx, name) == Some(Refuted);

  /* Whether `goal` is for `written` in `ctx`: the same goal, whose names
   * are defined by the same text, as are the names those definitions use,
   * and so on. A step on the goal can unfold any of them. */
  let is_for = (goal: t, ~ctx: SemanticCtx.t, written: Exp.t): bool => {
    let rec text = (e: Exp.t): Exp.t =>
      switch (Exp.term_of(e)) {
      | Closure(_, inner) => text(inner)
      | FixF(p, body, Some(_)) => Exp.fresh(FixF(p, body, None))
      | _ => e
      };
    let names = (e: Exp.t): list(Var.t) => {
      let found = ref([]);
      let _: Exp.t =
        Exp.map_term(
          ~f_exp=
            (continue, e) =>
              switch (Exp.term_of(e)) {
              | Var(x) =>
                found := [x, ...found^];
                e;
              | _ => continue(e)
              },
          e,
        );
      found^;
    };
    let before = env(goal);
    let after = SemanticCtx.get_env(ctx);
    let rec same_definitions = (seen, todo) =>
      switch (todo) {
      | [] => true
      | [x, ...todo] when List.mem(seen, x, ~equal=String.equal) =>
        same_definitions(seen, todo)
      | [x, ...todo] =>
        switch (Environment.lookup(before, x), Environment.lookup(after, x)) {
        | (None, None) => same_definitions([x, ...seen], todo)
        | (Some(a), Some(b)) =>
          (phys_equal(a, b) || Exp.fast_equal(text(a), text(b)))
          && same_definitions([x, ...seen], names(text(b)) @ todo)
        | (Some(_), None)
        | (None, Some(_)) => false
        }
      };
    Exp.fast_equal(goal.written, written)
    && (phys_equal(before, after) || same_definitions([], names(written)));
  };

  let info_map = (goal: t): Statics.Map.t =>
    CodeEditable.Model.get_statics(goal.editor).info_map;

  /* The ids of the offered steps' sources, where their underlines go. */
  let step_ids = (goal: t): list(Id.t) =>
    List.map(~f=(o: EvalSteps.offer) => o.source, goal.eval_steps);

  /* Where the underline marking what `row`'s step acts on goes. A goal
   * carried over from an earlier check prints the same as `row.goal` but
   * may not share its ids, so the part is found by its position in
   * `row.goal`.
   *
   * The editor may not draw that node. Inside a folded lambda it is in
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
    | (Some(row_goal), Some(part)) =>
      Occurrence.at_same_position(~from=row_goal, ~onto=goal.written, part)
      |> Option.bind(_, ~f=(node: Exp.t) =>
           switch (self_and_ancestors(Exp.rep_id(node), editor_term)) {
           | Some(path) => List.find(~f=drawn, path)
           | None => inside(node)
           }
         )
      |> Option.map(~f=Exp.rep_id)
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
    |> Option.bind(_, ~f=id => Exp.find_by_id(id, goal.written));
  };

  /* `goal` built again with `settings`. The node that was selected is
   * selected again, or the caret goes back to the node it was at. */
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
      Option.bind(node, ~f=id =>
        Editor.Update.update(
          ~settings,
          restore(id),
          rebuilt.editor.statics,
          rebuilt.editor.dynamics,
          rebuilt.editor.editor,
        )
        |> Stdlib.Result.to_option
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
  /* The occurrence, a node of the goal. */
  node: Exp.t,
  /* What the focus writes, and which of its matches the occurrence is,
   * counting from 0. */
  selector: Exp.t,
  index: int,
};

/* Where a step picked on `node` acts: through a focus, unless `node` is
 * the whole goal. The focus writes `node` with as much of it replaced by
 * `$e` as still selects `node`, so the step survives edits to the parts
 * of the goal it doesn't need. */
let target = (goal: Goal.t, node: Exp.t): option(target) =>
  Id.equal(Exp.rep_id(node), Exp.rep_id(goal.written))
    ? None
    : {
      let (selector, index) =
        Occurrence.generalize(
          ~statics=ProofStatics.of_info_map(Goal.info_map(goal)),
          ~env=Goal.env(goal),
          ~index=
            Occurrence.index(node, goal.written) |> Option.value(~default=0),
          node,
          Node(node),
          goal.written,
        );
      Some({
        node,
        selector,
        index,
      });
    };

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
          ~f=(rest, step) => Proof.fresh(Seq(step, rest)),
          ~init=last,
          earlier,
        ),
      ),
    )
  };

/* What taking the `idx`-th offered step writes: `step`, focused on the
 * step's source. */
let take_step = (goal: Goal.t, idx: int): option(pick) => {
  open OptUtil.Syntax;
  let* {source, _}: EvalSteps.offer = List.nth(goal.eval_steps, idx);
  let* node = Exp.find_by_id(source, goal.written);
  /* A step at the root needs no selector. */
  let+ _ =
    Id.equal(source, Exp.rep_id(goal.written))
      ? Some(0) : EvalSteps.checker_idx(goal.written, node);
  {
    at: target(goal, node),
    step: Proof.fresh(EvalStep),
  };
};

/* Reflexivity is the `refl` axiom at the equality `node`. */
let refl = (goal: Goal.t, node: Exp.t): pick => {
  at: target(goal, node),
  step:
    Proof.fresh(
      Terms.axiom_step(~direction=Direction.Right, ~equality="refl"),
    ),
};

/* `proof` checked from `written` in `goal`'s scope, rather than as part
 * of its theorem, or None if the checker marks any step of it. */
let check =
    (goal: Goal.t, written: Exp.t, proof: Proof.t): option(ProofMap.t) => {
  let checked =
    Evaluator.Proofs.check_proof(
      ~statics=ProofStatics.of_info_map(Goal.info_map(goal)),
      ~ctx=goal.ctx,
      ~goal=written,
      proof,
    );
  ProofMap.error_ids(checked) == [] ? Some(checked) : None;
};

/* The goal a step of a checked proof leaves. */
let outgoing_of = (checked: ProofMap.t, step: Proof.t): option(Exp.t) =>
  Option.bind(ProofMap.lookup(Proof.rep_id(step), checked), ~f=e =>
    e.outgoing
  );

/* The goal `proof` leaves, checked as `check` does. */
let outgoing = (goal: Goal.t, written: Exp.t, proof: Proof.t): option(Exp.t) =>
  Option.bind(check(goal, written, proof), ~f=checked =>
    outgoing_of(checked, proof)
  );

/* A scope's citable rules: its own facts and theorems, then the builtin
 * axioms, which `ProofCtx.of_theorem_ctx` lists last. */
let scope_and_axioms =
    (ctx: SemanticCtx.t): (list(ProofCtx.entry), list(ProofCtx.entry)) => {
  let all =
    ProofCtx.of_theorem_ctx(
      ~builtins=Axioms.v,
      ~captured=ctx.captured,
      SemanticCtx.get_ctx(ctx),
    );
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
        switch (self_and_ancestors(Exp.rep_id(s), goal.written)) {
        | Some([_, ...ancestors]) => widen(s, ancestors)
        | Some([])
        | None => s
        }
      | None => goal.written
      };
    let is_node = (e: Exp.t) => Id.equal(Exp.rep_id(e), Exp.rep_id(node));
    let inside = id => Exp.find_by_id(id, node) != None;
    /* The step to take in `node`. On an equation, a part that also occurs
     * on the other side is left alone, as stepping it only makes the sides
     * differ, and the left side goes first; then the outermost part. With
     * nothing selected, only evaluation's own steps count, so a redex it
     * won't contract yet doesn't displace the step it takes. */
    let preferred_step = {
      let sides =
        switch (Exp.term_of(ProofSyntax.unwrap(node))) {
        | BinOp(Poly(Equals), l, r) => Some((l, r))
        | _ => None
        };
      let rank = (source: Exp.t) => {
        let (shared, right) =
          switch (sides) {
          | Some((l, r)) =>
            let on_left = Exp.find_by_id(Exp.rep_id(source), l) != None;
            (Occurrence.index(source, on_left ? r : l) != None, !on_left);
          | None => (false, false)
          };
        let depth =
          self_and_ancestors(Exp.rep_id(source), node)
          |> Option.value_map(~default=0, ~f=List.length);
        (shared, right, depth);
      };
      goal.eval_steps
      |> List.mapi(~f=(idx, offer) => (idx, offer))
      |> List.filter(~f=((_, offer: EvalSteps.offer)) =>
           inside(offer.source)
           && (
             switch (selected, offer.kind) {
             | (None, Contraction) => false
             | (Some(_), _)
             | (None, Evaluation(_)) => true
             }
           )
         )
      |> List.filter_map(~f=((idx, offer: EvalSteps.offer)) =>
           Exp.find_by_id(offer.source, goal.written)
           |> Option.map(~f=source => (rank(source), idx))
         )
      |> List.stable_sort(~compare=((a, _), (b, _)) => compare(a, b))
      |> List.map(~f=snd)
      |> ListUtil.hd_opt;
    };
    /* Reflexivity inside the part, as in `n == 1 ==> n == n`, only when
     * no eval step is left. */
    let candidate =
      switch (List.exists(~f=is_node, goal.refls), preferred_step) {
      | (true, _) => Some(("refl", refl(goal, node)))
      | (false, Some(idx)) =>
        take_step(goal, idx) |> Option.map(~f=pick => ("step", pick))
      | (false, None) =>
        List.find(~f=(e: Exp.t) => inside(Exp.rep_id(e)), goal.refls)
        |> Option.map(~f=e => ("refl", refl(goal, e)))
      };
    let* (rule, pick) = candidate;
    let proof = focused(pick);
    let* checked = check(goal, goal.written, proof);
    /* What `node` becomes. A focus keeps the ids around the occurrence it
     * replaces, so a selection wider than the step's own part is found by
     * its id. */
    let+ result =
      switch (pick.at) {
      | Some({node: part, _})
          when !Id.equal(Exp.rep_id(part), Exp.rep_id(node)) =>
        outgoing_of(checked, proof)
        |> Option.bind(_, ~f=Exp.find_by_id(Exp.rep_id(node)))
      | Some(_)
      | None => outgoing_of(checked, pick.step)
      };
    {
      rule,
      pick,
      result,
    };
  };
};

/* The Proof strategy tab: the goal's own introduction, then the
 * strategies any goal can use. Each writes its form with holes for the
 * user to fill in. */
module Strategies = {
  type strategy = {
    label: string,
    /* What it writes and what for. */
    tooltip: string,
    writes: TermBase.Proof.term,
  };

  let intros = (goal: Goal.t): list(strategy) => {
    let written = goal.written;
    let hole = Exp.fresh(EmptyHole);
    let forall =
      ProofCheck.peel_binder(written) != None
        ? [
          {
            label: "Forall",
            tooltip: "Writes `forall x =>`: names the argument, to prove the body for any x",
            writes: Terms.forall(),
          },
        ]
        : [];
    /* Exactly when the goal is `f == g` at an arrow type, by the checker's
     * own detector, so it never writes a step that would mark. */
    let funext =
      ProofCheck.Funext.goal(
        ~statics=ProofStatics.of_info_map(Goal.info_map(goal)),
        ~env=Goal.env(goal),
        written,
      )
      == None
        ? []
        : [
          {
            label: "Funext",
            tooltip: "Writes `funext x =>`: proves the functions equal at any argument x",
            writes: Terms.funext(),
          },
        ];
    let assume =
      ProofRule.implication(written) == None
        ? []
        : [
          {
            label: "Assume",
            tooltip: "Writes `assume A =>`: for a goal A ==> B, adds A as a fact and proves B",
            writes: Terms.assume(~exp=hole),
          },
        ];
    forall @ funext @ assume;
  };

  let general = (): list(strategy) => {
    let hole = Exp.fresh(EmptyHole);
    [
      {
        label: "Induction",
        tooltip: "Writes `induction x end`: one case per form of x, each with its inductive hypotheses",
        writes: Terms.induction(~exp=hole),
      },
      {
        label: "Cases",
        tooltip: "Writes `cases e end` on any expression: one case per form of its value, each with its case equation `case_eq`",
        writes: Terms.cases(~exp=hole),
      },
      {
        label: "Have",
        tooltip: "Writes `have ? proof ? =>`: proves a stepping-stone fact first, then uses it here",
        writes: Terms.have(~exp=hole),
      },
      {
        label: "Generalize",
        tooltip: "Writes `generalize x =>`: makes x arbitrary again, a forall in the goal, e.g. before induction",
        writes: Terms.generalize(~exp=hole),
      },
      {
        label: "Revert",
        tooltip: "Writes `revert <fact> =>`: moves a fact back into the goal as fact ==> goal, e.g. before induction",
        writes: Terms.revert(~exp=hole),
      },
    ];
  };
};
