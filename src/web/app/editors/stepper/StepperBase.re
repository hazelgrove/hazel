open Util;
open Language;
open StepInterface;
open OptUtil.Syntax;

/* The stepper: a proof drawn as a chain of rows, one per step, each
 * showing the goal the step starts from and the step's justification.
 *
 * The proof syntax is the source of truth. A row never changes its own
 * step: picking, inserting or deleting a step writes a patch into the
 * proof (see Patches), and the next `calculate` pass reads the proof
 * again and reshapes the chain to match. Goals are the checker's, read
 * off its ProofMap by each step's proof id.
 *
 * The types come before the modules because the recursive modules'
 * signatures name them. */

[@deriving (show({with_path: false}), sexp, yojson)]
type next_step =
  /* A row for a hole in the proof, offering the step picker. A hole is
   * the identity (the goal passes through), so it carries the rest of
   * the chain: every piece of proof syntax gets a row, junk included.
   * The trailing picker row carries `Finished`. */
  | MissingStep(MissingStep.Model.t, next_step)
  | NextStep(step_model)
  | Finished

and step_kind_model =
  | InductionStep(InductionStep.model'(next_step))
  | ForallStep(ForallStep.model'(next_step))
  | FunextStep(WrappingStep.model'(next_step))
  | AxiomStep(AxiomStep.model'(next_step))
  | AlgebriteStep(LabelStep.model'(next_step))
  | EvalStep(LabelStep.model'(next_step))
  | ContradictionStep(LabelStep.model'(next_step))
  | AssumeStep(WrappingStep.model'(next_step))
  | RevertStep(WrappingStep.model'(next_step))
  | GeneralizeStep(WrappingStep.model'(next_step))
  | HaveStep(HaveStep.model'(next_step))

and step_model = {
  /* The checker's ProofMap entry for `proof`, if it has one yet. */
  entry: Calc.saved(option(ProofMap.entry)),
  /* The row's goal and step picker: the same machinery a missing row
   * uses, except that a step picked here is inserted before this one. */
  picker: MissingStep.Model.t,
  step_kind: step_kind_model,
  next_step,
  /* The step this row renders: the head of its `;` chain. */
  proof: Calc.saved(Proof.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type step_kind_action =
  | InductionStep(InductionStep.action'(step_action))
  | ForallStep(ForallStep.action'(step_action))
  | FunextStep(WrappingStep.action'(step_action))
  | AxiomStep(AxiomStep.action'(step_action))
  | AlgebriteStep(LabelStep.action'(step_action))
  | EvalStep(LabelStep.action'(step_action))
  | ContradictionStep(LabelStep.action'(step_action))
  | AssumeStep(WrappingStep.action'(step_action))
  | RevertStep(WrappingStep.action'(step_action))
  | GeneralizeStep(WrappingStep.action'(step_action))
  | HaveStep(HaveStep.action'(step_action))

and step_action =
  | StepKindAction(step_kind_action)
  | MissingStepAction(MissingStep.Update.t)
  | EditorAction(CodeSelectable.Update.t)
  | NextStep(step_action);

[@deriving (show({with_path: false}), sexp, yojson)]
type step_kind_focus =
  | InductionStep(InductionStep.focus'(step_focus))
  | ForallStep(ForallStep.focus'(step_focus))
  | FunextStep(WrappingStep.focus'(step_focus))
  | AxiomStep(AxiomStep.focus'(step_focus))
  | AlgebriteStep(LabelStep.focus'(step_focus))
  | EvalStep(LabelStep.focus'(step_focus))
  | ContradictionStep(LabelStep.focus'(step_focus))
  | AssumeStep(WrappingStep.focus'(step_focus))
  | RevertStep(WrappingStep.focus'(step_focus))
  | GeneralizeStep(WrappingStep.focus'(step_focus))
  | HaveStep(HaveStep.focus'(step_focus))

and step_focus =
  | StepKindFocus(step_kind_focus)
  | MissingStepFocus(MissingStep.Selection.t)
  | Here(CodeSelectable.Selection.t)
  | Next(step_focus);

let init_step = MissingStep(MissingStep.Model.init, Finished);

let is_missing_step = (ns: next_step): bool =>
  switch (ns) {
  | MissingStep(_, _) => true
  | NextStep(_)
  | Finished => false
  };

/* Which row each piece of proof syntax becomes. */
module Proofs = {
  /* The step a proof starts with, and the rest of its `;` chain. */
  let rec split = (p: Proof.t): (Proof.t, option(Proof.t)) =>
    switch (p) {
    /* Grouping parens are transparent: `( funext x => ? )` splits with a
     * Funext head. */
    | {term: Parens(inner), _} => split(inner)
    /* An inserted step lands as `Seq(Seq(new, old), tail)` where no
     * reparse flattens the chain, so re-associate it to keep the head a
     * single step. The rebuilt node reuses the inner Seq's ids, which no
     * row reads. */
    | {term: Seq({term: Seq(a, b), _} as inner, tail), _} =>
      split({
        ...p,
        term:
          Seq(
            a,
            {
              ...inner,
              term: Seq(b, tail),
            },
          ),
      })
    | {term: Seq(head, tail), _} => (head, Some(tail))
    | _ => (p, None)
    };

  /* The proofs a MissingStep row stands in for. */
  let rec is_hole = (p: Proof.t): bool =>
    switch (p.term) {
    | Parens(inner) => is_hole(inner)
    | EmptyHole
    | Invalid(_)
    | MultiHole(_) => true
    | AxiomStep(_)
    | AlgebriteStep(_)
    | EvalStep(_)
    | Contradiction(_)
    | Forall(_, _)
    | Funext(_, _)
    | Induction(_, _, _)
    | Assume(_, _, _)
    | Alias(_, _, _)
    | Generalize(_, _)
    | Revert(_, _, _)
    | Have(_, _, _)
    | Seq(_, _) => false
    };

  /* The step kind a proof calls for, as a placeholder that the kind's
   * own `calculate` fills in from the proof on the same pass. */
  let rec kind = (p: Proof.t): option(step_kind_model) =>
    switch (p.term) {
    | Parens(inner) => kind(inner)
    | AxiomStep(_) =>
      Some(
        AxiomStep({
          equality: "",
          at_exp: Exp.fresh(EmptyHole),
        }),
      )
    | AlgebriteStep(_) => Some(AlgebriteStep())
    | EvalStep(_) => Some(EvalStep())
    | Contradiction(_) => Some(ContradictionStep())
    | Forall(_, _) => Some(ForallStep(ForallStep.init(init_step)))
    | Funext(_, _) => Some(FunextStep(WrappingStep.init(init_step)))
    | Induction(scrut, _, _) =>
      /* The scrutinee editor starts from the proof's scrutinee, with its
       * own ids. Started empty, it would write back through and erase
       * the scrutinee. */
      Some(
        InductionStep(
          InductionStep.init(~exp=Exp.replace_all_ids(scrut), ()),
        ),
      )
    | Assume(_, _, _) => Some(AssumeStep(WrappingStep.init(init_step)))
    /* An alias has no row of its own: its text says it all. */
    | Alias(_, _, _) => None
    | Revert(_, _, _) => Some(RevertStep(WrappingStep.init(init_step)))
    | Generalize(_, _) =>
      Some(GeneralizeStep(WrappingStep.init(init_step)))
    | Have(_, _, _) => Some(HaveStep(HaveStep.init(init_step)))
    | EmptyHole
    | Invalid(_)
    | MultiHole(_)
    | Seq(_, _) => None
    };

  /* The step kind for `p`, keeping `sk` (and the UI state it holds) when
   * it is already the right kind. A proof edited into a hole is
   * `calculate`'s business, not this. */
  let rec adapt_kind = (sk: step_kind_model, p: Proof.t): step_kind_model =>
    switch (p.term, sk) {
    | (Parens(inner), _) => adapt_kind(sk, inner)
    | (AxiomStep(_), AxiomStep(_))
    | (AlgebriteStep(_), AlgebriteStep(_))
    | (EvalStep(_), EvalStep(_))
    | (Contradiction(_), ContradictionStep(_))
    | (Forall(_, _), ForallStep(_))
    | (Funext(_, _), FunextStep(_))
    | (Induction(_, _, _), InductionStep(_))
    | (Assume(_, _, _), AssumeStep(_))
    | (Revert(_, _, _), RevertStep(_))
    | (Generalize(_, _), GeneralizeStep(_))
    | (Have(_, _, _), HaveStep(_)) => sk
    | _ => kind(p) |> Option.value(~default=sk)
    };
};

/* A row's status, from the marks the checker recorded for its step.
 * Row.view shows it. */
module Status = {
  type t =
    /* Checked, with no marks. Says nothing about whether the goal is
     * proven. */
    | Clean
    /* Clean, but the step's own arguments hold a hole: an induction
     * case's pattern or scrutinee, a forall binder, an axiom target.
     * Holes in nested proofs don't count: each renders its own
     * continuation row already. */
    | Incomplete
    /* At fault: the highest-priority mark. */
    | Broken(ProofMark.t)
    /* An earlier row is at fault, so this one has no incoming goal. */
    | Blocked
    /* Not checked yet. */
    | Pending;

  let of_step = (entry: option(ProofMap.entry), proof: Proof.t): t =>
    switch (entry) {
    | None => Pending
    | Some({marks, _}) =>
      let blocked =
        List.exists(
          fun
          | ProofMark.MissingIncoming => true
          | _ => false,
          marks,
        );
      switch (blocked, ProofMark.highest(marks), proof) {
      | (true, _, _) => Blocked
      | (false, Some(m), _) => Broken(m)
      | (false, None, _) when Proof.args_have_hole(proof) => Incomplete
      | (false, None, _) => Clean
      };
    };

  /* Garbage syntax in a hole is broken. A genuine `?`, the trailing
   * picker's included, carries no indicator: the picker already says the
   * proof continues here. */
  let of_hole = (proof: option(Proof.t)): t =>
    switch (proof) {
    | Some({term: Invalid(_) | MultiHole(_), _}) =>
      Broken(ProofMark.MalformedProofTerm)
    | Some(_)
    | None => Clean
    };

  let broken_mark = (status: t): option(ProofMark.t) =>
    switch (status) {
    | Broken(mark) => Some(mark)
    | Clean
    | Incomplete
    | Blocked
    | Pending => None
    };
};

/* The proof terms the step picker writes. */
module Terms = {
  /* An expression sliced from the row's goal, made fit to write into the
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

/* Edits to the proof syntax, the only way a row changes the proof. */
module Patches = {
  type patch = Haz3lcore.EditorTransform.patch;

  /* Where a picked step lands. Every row is backed either by the proof
   * node it replaces (a written hole), or by the leaf before it, which a
   * trailing row past the end of the chain extends. */
  type target =
    | Replace(Proof.t)
    | Extend(Proof.t);

  /* A hole is replaced rather than extended: it stands for "the proof
   * continues here", not for a step of its own. No `; ?` is appended
   * after the new step: `calculate` adds the trailing row regardless. */
  let add_step = (~target: target, term: TermBase.Proof.term): patch => {
    let step = Proof.fresh(term);
    switch (target) {
    | Replace(p) =>
      Haz3lcore.EditorTransform.mk_proof_patch(
        ~target_id=Proof.rep_id(p),
        step,
      )
    | Extend(prev) =>
      Haz3lcore.EditorTransform.mk_proof_patch(
        ~target_id=Proof.rep_id(prev),
        switch (prev.term) {
        | EmptyHole
        | Invalid(_)
        | MultiHole(_) => step
        | _ => Proof.fresh(Seq(prev, step))
        },
      )
    };
  };

  /* A step picked on a step row acts on that row's goal, so it goes in
   * before the row's step. `Seq(new, before)` nests left inside the
   * enclosing chain; the reparse after the patch flattens it. */
  let insert_step = (~before: Proof.t, term: TermBase.Proof.term): patch =>
    Haz3lcore.EditorTransform.mk_proof_patch(
      ~target_id=Proof.rep_id(before),
      Proof.fresh(Seq(Proof.fresh(term), before)),
    );

  /* Splices the step out of its chain, `;` and all, or leaves a hole
   * when it is the whole proof, so the text reads as if the step had
   * never been written. */
  let remove_step = (p: Proof.t): patch =>
    Haz3lcore.EditorTransform.mk_proof_remove_patch(
      ~target_id=Proof.rep_id(p),
    );

  /* The repair for an axiom step that doesn't apply: the same step in
   * the other direction. */
  let flip_direction = (p: Proof.t): option(patch) =>
    switch (p.term) {
    | AxiomStep({at_idx, at_exp, direction, equality, instantiation}) =>
      Some(
        Haz3lcore.EditorTransform.mk_proof_patch(
          ~target_id=Proof.rep_id(p),
          {
            ...p,
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
};

/* One row of the chain: the goal it starts from, then the transition to
 * the next row, which holds the justification and a delete button.
 *
 * The same syntax always takes the same space, so status never moves the
 * proof (no scroll jumps). It shows only as a class on the row, which
 * draws an indicator along the row's right edge (red broken, tan
 * incomplete, dotted light red blocked), and as a popup above the
 * justification, hidden until the justification is clicked, holding the
 * message, any detail and the repair buttons. Both are out of the flow. */
module Row = {
  let view =
      (
        ~globals: Globals.t,
        ~status: Status.t,
        /* Off for a row that shows the error of the step above instead of
         * a goal: hidden rather than removed, so the text stays aligned. */
        ~show_equiv: bool,
        /* None for a row with no step of its own. */
        ~delete: option(Ui_effect.t(unit)),
        /* Detail for the popup, e.g. an induction's missing pattern. */
        ~popup_extra: list(WebUtil.Node.t),
        ~repair: list(WebUtil.Node.t),
        ~editor_view: WebUtil.Node.t,
        ~justification: WebUtil.Node.t,
        ~content: list(WebUtil.Node.t),
      ) => {
    let (row_classes, popup_body) =
      switch (status) {
      | Clean => (["ok"], [])
      | Pending => (["pending"], [])
      | Incomplete => (["incomplete"], [])
      | Broken(mark) => (
          ["broken"],
          ProofMarkView.message(~globals, mark) @ popup_extra @ repair,
        )
      | Blocked => (
          ["blocked"],
          [WebUtil.Node.text("Waiting on the step above.")]
          @ popup_extra
          @ repair,
        )
      };
    /* The popup sits inside the focusable wrapper, so a click on a repair
     * button keeps focus within it (`:focus-within` holds) and lands
     * before the popup hides. */
    let justification =
      WebUtil.(
        switch (popup_body) {
        | [] => div_c("step-justification-wrap", [justification])
        | _ =>
          Node.div(
            ~attrs=[
              Attr.classes(["step-justification-wrap", "has-popup"]),
              Attr.create("tabindex", "0"),
            ],
            [div_c("step-error-popup", popup_body), justification],
          )
        }
      );
    let delete_button =
      switch (delete) {
      | Some(d) => [
          Widgets.button(
            ~clss=["step-delete"], ~tooltip="Delete this step", Icons.trash, _ =>
            d
          ),
        ]
      | None => []
      };
    let transition =
      WebUtil.(div_c("step-transition", [justification] @ delete_button));
    /* A row with block content (induction cases, a forall's body) puts
     * its transition after the whole block. */
    let row_classes = row_classes @ (content == [] ? [] : ["has-content"]);
    WebUtil.[
      Node.div(
        ~attrs=[Attr.classes(["step-border"] @ row_classes)],
        [
          div_c(
            "step-display",
            [
              Node.div(
                ~attrs=[
                  Attr.classes(
                    ["equiv"] @ (show_equiv ? [] : ["equiv-hidden"]),
                  ),
                ],
                [Node.text("≡")],
              ),
              div_c("step-output", [editor_view]),
              transition,
            ],
          ),
        ]
        @ content,
      ),
    ];
  };

  /* In place of the goal, when the step above broke: the checker passed
   * its goal through, so printing it again would be noise. */
  let view_error_above = (~globals: Globals.t, mark: ProofMark.t) =>
    WebUtil.div_c(
      "step-error-inline",
      ProofMarkView.message(~globals, mark),
    );

  /* The repairs a broken step offers: removing it, and, for an axiom that
   * doesn't apply, trying the other direction. */
  let repairs =
      (
        ~edit_syntax: Patches.patch => Ui_effect.t(unit),
        status: Status.t,
        proof: Proof.t,
      )
      : list(WebUtil.Node.t) => {
    let button = (~tooltip, label, patch) =>
      Widgets.button(
        ~clss=["proof-button"], ~tooltip, WebUtil.Node.text(label), _ =>
        edit_syntax(patch)
      );
    switch (status) {
    | Broken(mark) =>
      let flip =
        switch (mark, Patches.flip_direction(proof)) {
        | (RuleDoesNotApply(_), Some(patch)) => [
            button(
              ~tooltip="Apply the equality in the other direction",
              "Try other direction",
              patch,
            ),
          ]
        | _ => []
        };
      flip
      @ [
        button(
          ~tooltip="Replace this step with a hole",
          "Remove step",
          Patches.remove_step(proof),
        ),
      ];
    | Clean
    | Incomplete
    | Blocked
    | Pending => []
    };
  };
};

/* What the step picker's events do. Missing rows and step rows offer the
 * same picker and differ only in `emit`, which writes the picked step
 * into the proof. */
module Picker = {
  let signal =
      (
        ~take_focus: step_focus => Ui_effect.t(unit),
        ~hide_stepper: Ui_effect.t(unit),
        ~emit: TermBase.Proof.term => Ui_effect.t(unit),
        ~main_editor: option(CodeEditable.Channel.t),
        m: MissingStep.Model.t,
        event: MissingStep.View.event,
      )
      : Ui_effect.t(unit) => {
    /* Insert a wrapping form and leave the user typing its argument. The
     * argument is a SubEditor window onto the main editor (see
     * ProofFormView), so that takes three things:
     *
     *  1. Move the main editor's caret into the new argument, after the
     *     patch has made it: a sub-editor rejects edits, and draws no
     *     caret, while the caret is outside its splice.
     *  2. Focus the new step's `Arg`, which renders the splice as the
     *     active editor. The new step takes this row's place in the
     *     chain, so its focus path is this row's with `MissingStepFocus`
     *     swapped for the form's `Arg`.
     *  3. Schedule DOM focus for after the render: the click left it on
     *     the menu button. */
    let emit_wrapping_form =
        (~arg_focus: step_kind_focus, term: TermBase.Proof.term) => {
      let jump_to_arg =
        switch (main_editor, Terms.arg_id(term)) {
        | (Some(channel: CodeEditable.Channel.t), Some(arg_id)) =>
          channel.inject(Perform(Move(Goal(TileId(arg_id)))))
        | (None, _)
        | (_, None) => Ui_effect.Ignore
        };
      Ui_effect.Many([
        emit(term),
        jump_to_arg,
        take_focus(StepKindFocus(arg_focus)),
        Ui_effect.of_sync_fun(
          () => Haz3lcore.FocusEffect.schedule_cell(),
          (),
        ),
      ]);
    };
    switch (event) {
    | MakeActive(selection) => take_focus(MissingStepFocus(selection))
    | HideStepper => hide_stepper
    | AddForall => emit(Terms.forall())
    | AddFunext(binder) =>
      emit_wrapping_form(
        ~arg_focus=FunextStep(Arg()),
        Terms.funext(~binder),
      )
    | AddInduction(scrut) => emit(Terms.induction(~scrut))
    | AddAxiomStep(_name, at_idx, at_exp, direction, equality) =>
      emit(Terms.axiom_step(~at_idx, ~at_exp, ~direction, ~equality))
    | AddAlgebriteStep(at_idx, at_exp, with_exp) =>
      emit(Terms.algebrite_step(~at_idx, ~at_exp, ~with_exp))
    | AddAssume(exp) =>
      emit_wrapping_form(~arg_focus=AssumeStep(Arg()), Terms.assume(~exp))
    | AddRevert(exp) =>
      emit_wrapping_form(~arg_focus=RevertStep(Arg()), Terms.revert(~exp))
    | AddGeneralize(exp) =>
      emit_wrapping_form(
        ~arg_focus=GeneralizeStep(Arg()),
        Terms.generalize(~exp),
      )
    | AddHave(exp) =>
      emit_wrapping_form(~arg_focus=HaveStep(Arg()), Terms.have(~exp))
    | AddContradiction(exp) => emit(Terms.contradiction(~exp))
    | TakeStep(idx) =>
      let steps =
        switch (m.next_steps |> Calc.get_saved_opt) {
        | Some(AvailableSteps(steps)) => steps
        | Some(AutoStep(_))
        | None => []
        };
      switch (
        Option.bind(
          List.nth_opt(steps, idx),
          MissingStep.Update.written_target(m),
        )
      ) {
      | Some((at_exp, at_idx)) => emit(Terms.eval_step(~at_idx, ~at_exp))
      | None => Ui_effect.Ignore
      };
    /* Reflexivity is the `refl_eq` axiom at the picked equality. */
    | Refl(idx) =>
      switch (
        List.nth_opt(Calc.get_saved([], m.refls), idx),
        m.full_exp |> Calc.get_saved_opt,
      ) {
      | (Some(at_exp), Some(full_exp)) =>
        switch (Occurrence.index(at_exp, full_exp)) {
        | Some(at_idx) =>
          emit(
            Terms.axiom_step(
              ~at_idx,
              ~at_exp,
              ~direction=Direction.Right,
              ~equality="refl_eq",
            ),
          )
        | None => Ui_effect.Ignore
        }
      | (None, _)
      | (_, None) => Ui_effect.Ignore
      }
    };
  };
};
module rec StepKind:
  STEP with
    type model = step_kind_model and
    type action = step_kind_action and
    type focus = step_kind_focus = {
  /* Dispatch to the step kinds' own modules. */

  module InductionStep = InductionStep.F(Stepper);
  module ForallStep = ForallStep.F(Stepper);
  module FunextStep = WrappingStep.F(WrappingStep.Funext, Stepper);
  module AxiomStep = AxiomStep.F(Stepper);
  module AlgebriteStep =
    LabelStep.F(
      {
        let justification = "algebra";
      },
      Stepper,
    );
  module EvalStep =
    LabelStep.F(
      {
        let justification = "eval";
      },
      Stepper,
    );
  module ContradictionStep =
    LabelStep.F(
      {
        let justification = "contradiction";
      },
      Stepper,
    );
  module AssumeStep = WrappingStep.F(WrappingStep.Assume, Stepper);
  module HaveStep = HaveStep.F(Stepper);
  module RevertStep = WrappingStep.F(WrappingStep.Revert, Stepper);
  module GeneralizeStep = WrappingStep.F(WrappingStep.Generalize, Stepper);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = step_kind_model;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = step_kind_action;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus = step_kind_focus;

  let update = (~settings, action: action, model: model) => {
    Updated.(
      switch (action, model) {
      | (InductionStep(a), InductionStep(m)) =>
        let* s = InductionStep.update(~settings, a, m);
        (InductionStep(s): model);
      | (ForallStep(a), ForallStep(m)) =>
        let* s = ForallStep.update(~settings, a, m);
        (ForallStep(s): model);
      | (FunextStep(a), FunextStep(m)) =>
        let* s = FunextStep.update(~settings, a, m);
        (FunextStep(s): model);
      | (AxiomStep(a), AxiomStep(m)) =>
        let* s = AxiomStep.update(~settings, a, m);
        (AxiomStep(s): model);
      | (AlgebriteStep(a), AlgebriteStep(m)) =>
        let* s = AlgebriteStep.update(~settings, a, m);
        (AlgebriteStep(s): model);
      | (EvalStep(a), EvalStep(m)) =>
        let* s = EvalStep.update(~settings, a, m);
        (EvalStep(s): model);
      | (ContradictionStep(a), ContradictionStep(m)) =>
        let* s = ContradictionStep.update(~settings, a, m);
        (ContradictionStep(s): model);
      | (AssumeStep(a), AssumeStep(m)) =>
        let* s = AssumeStep.update(~settings, a, m);
        (AssumeStep(s): model);
      | (HaveStep(a), HaveStep(m)) =>
        let* s = HaveStep.update(~settings, a, m);
        (HaveStep(s): model);
      | (RevertStep(a), RevertStep(m)) =>
        let* s = RevertStep.update(~settings, a, m);
        (RevertStep(s): model);
      | (GeneralizeStep(a), GeneralizeStep(m)) =>
        let* s = GeneralizeStep.update(~settings, a, m);
        (GeneralizeStep(s): model);
      | (
          InductionStep(_) | ForallStep(_) | FunextStep(_) | AxiomStep(_) |
          AlgebriteStep(_) |
          EvalStep(_) |
          ContradictionStep(_) |
          AssumeStep(_) |
          RevertStep(_) |
          GeneralizeStep(_) |
          HaveStep(_),
          _,
        ) =>
        model |> Updated.raise_invalid_action
      }
    );
  };

  let calculate =
      (
        ~settings: Calc.t(CoreSettings.t),
        ~exp: Calc.t(Exp.t),
        ~ctx: Calc.t(SemanticCtx.t),
        ~info_map: Calc.t(Statics.Map.t),
        ~proof_info_map: Calc.t(Statics.Map.t),
        ~ana,
        ~proof: Calc.t(Proof.t),
        ~proof_map: Calc.t(ProofMap.t),
        model: model,
      ) => {
    /* Every kind takes the same calculate arguments; only the model
       type differs. */
    let calculate_with = (calculate, m) =>
      calculate(
        ~settings,
        ~exp,
        ~ctx,
        ~info_map,
        ~proof_info_map,
        ~ana,
        ~proof,
        ~proof_map,
        m,
      );
    switch (model) {
    | InductionStep(m) =>
      let+ m = calculate_with(InductionStep.calculate, m);
      (InductionStep(m): model);
    | ForallStep(m) =>
      let+ m = calculate_with(ForallStep.calculate, m);
      (ForallStep(m): model);
    | FunextStep(m) =>
      let+ m = calculate_with(FunextStep.calculate, m);
      (FunextStep(m): model);
    | AxiomStep(m) =>
      let+ m = calculate_with(AxiomStep.calculate, m);
      (AxiomStep(m): model);
    | AlgebriteStep(m) =>
      let+ m = calculate_with(AlgebriteStep.calculate, m);
      (AlgebriteStep(m): model);
    | EvalStep(m) =>
      let+ m = calculate_with(EvalStep.calculate, m);
      (EvalStep(m): model);
    | ContradictionStep(m) =>
      let+ m = calculate_with(ContradictionStep.calculate, m);
      (ContradictionStep(m): model);
    | AssumeStep(m) =>
      let+ m = calculate_with(AssumeStep.calculate, m);
      (AssumeStep(m): model);
    | HaveStep(m) =>
      let+ m = calculate_with(HaveStep.calculate, m);
      (HaveStep(m): model);
    | RevertStep(m) =>
      let+ m = calculate_with(RevertStep.calculate, m);
      (RevertStep(m): model);
    | GeneralizeStep(m) =>
      let+ m = calculate_with(GeneralizeStep.calculate, m);
      (GeneralizeStep(m): model);
    };
  };

  let get_cursor_info = (~inject, ~focus: focus, model: model) =>
    Cursor.(
      switch (focus, model) {
      | (InductionStep(focus), InductionStep(m)) =>
        let+ focus_info =
          InductionStep.get_cursor_info(
            ~inject=x => inject(InductionStep(x): action),
            ~focus,
            m,
          );
        (InductionStep(focus_info): action);
      | (FunextStep(focus), FunextStep(m)) =>
        let+ focus_info =
          FunextStep.get_cursor_info(
            ~inject=x => inject(FunextStep(x): action),
            ~focus,
            m,
          );
        (FunextStep(focus_info): action);
      | (ForallStep(focus), ForallStep(m)) =>
        let+ focus_info =
          ForallStep.get_cursor_info(
            ~inject=x => inject(ForallStep(x): action),
            ~focus,
            m,
          );
        (ForallStep(focus_info): action);
      | (AxiomStep(focus), AxiomStep(m)) =>
        let+ focus_info =
          AxiomStep.get_cursor_info(
            ~inject=x => inject(AxiomStep(x): action),
            ~focus,
            m,
          );
        (AxiomStep(focus_info): action);
      | (AlgebriteStep(focus), AlgebriteStep(m)) =>
        let+ focus_info =
          AlgebriteStep.get_cursor_info(
            ~inject=x => inject(AlgebriteStep(x): action),
            ~focus,
            m,
          );
        (AlgebriteStep(focus_info): action);
      | (EvalStep(focus), EvalStep(m)) =>
        let+ focus_info =
          EvalStep.get_cursor_info(
            ~inject=x => inject(EvalStep(x): action),
            ~focus,
            m,
          );
        (EvalStep(focus_info): action);
      | (ContradictionStep(focus), ContradictionStep(m)) =>
        let+ focus_info =
          ContradictionStep.get_cursor_info(
            ~inject=x => inject(ContradictionStep(x): action),
            ~focus,
            m,
          );
        (ContradictionStep(focus_info): action);
      | (AssumeStep(focus), AssumeStep(m)) =>
        let+ focus_info =
          AssumeStep.get_cursor_info(
            ~inject=x => inject(AssumeStep(x): action),
            ~focus,
            m,
          );
        (AssumeStep(focus_info): action);
      | (HaveStep(focus), HaveStep(m)) =>
        let+ focus_info =
          HaveStep.get_cursor_info(
            ~inject=x => inject(HaveStep(x): action),
            ~focus,
            m,
          );
        (HaveStep(focus_info): action);
      | (RevertStep(focus), RevertStep(m)) =>
        let+ focus_info =
          RevertStep.get_cursor_info(
            ~inject=x => inject(RevertStep(x): action),
            ~focus,
            m,
          );
        (RevertStep(focus_info): action);
      | (GeneralizeStep(focus), GeneralizeStep(m)) =>
        let+ focus_info =
          GeneralizeStep.get_cursor_info(
            ~inject=x => inject(GeneralizeStep(x): action),
            ~focus,
            m,
          );
        (GeneralizeStep(focus_info): action);
      | (
          InductionStep(_) | ForallStep(_) | FunextStep(_) | AxiomStep(_) |
          AlgebriteStep(_) |
          EvalStep(_) |
          ContradictionStep(_) |
          AssumeStep(_) |
          RevertStep(_) |
          GeneralizeStep(_) |
          HaveStep(_),
          _,
        ) => Cursor.empty
      }
    );

  let view_content =
      (
        ~globals: Globals.t,
        ~focus: option(focus),
        ~inject: action => Ui_effect.t(unit),
        ~take_focus: focus => Ui_effect.t(unit),
        ~hide_stepper: Ui_effect.t(unit),
        ~is_toplevel: bool,
        ~proof: option(Proof.t),
        ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
        ~main_editor: option(CodeEditable.Channel.t),
        model: model,
      ) =>
    switch (model) {
    | InductionStep(m) =>
      InductionStep.view_content(
        ~globals,
        ~hide_stepper,
        ~is_toplevel,
        ~proof,
        ~edit_syntax,
        ~main_editor,
        ~focus=
          switch (focus) {
          | Some(InductionStep(f)) => Some(f)
          | _ => None
          },
        ~inject=x => inject(InductionStep(x)),
        ~take_focus=x => take_focus(InductionStep(x)),
        m,
      )
    | FunextStep(m) =>
      FunextStep.view_content(
        ~globals,
        ~focus=
          switch (focus) {
          | Some(FunextStep(f)) => Some(f)
          | _ => None
          },
        ~inject=x => inject(FunextStep(x)),
        ~take_focus=x => take_focus(FunextStep(x)),
        ~hide_stepper,
        ~is_toplevel,
        ~proof,
        ~edit_syntax,
        ~main_editor,
        m,
      )
    | ForallStep(m) =>
      ForallStep.view_content(
        ~globals,
        ~hide_stepper,
        ~is_toplevel,
        ~proof,
        ~edit_syntax,
        ~main_editor,
        ~focus=
          switch (focus) {
          | Some(ForallStep(f)) => Some(f)
          | _ => None
          },
        ~inject=x => inject(ForallStep(x)),
        ~take_focus=x => take_focus(ForallStep(x)),
        m,
      )
    | AxiomStep(m) =>
      AxiomStep.view_content(
        ~globals,
        ~hide_stepper,
        ~is_toplevel,
        ~proof,
        ~edit_syntax,
        ~main_editor,
        ~focus=
          switch (focus) {
          | Some(AxiomStep(f)) => Some(f)
          | _ => None
          },
        ~inject=x => inject(AxiomStep(x)),
        ~take_focus=x => take_focus(AxiomStep(x)),
        m,
      )
    | AlgebriteStep(m) =>
      AlgebriteStep.view_content(
        ~globals,
        ~hide_stepper,
        ~is_toplevel,
        ~proof,
        ~edit_syntax,
        ~main_editor,
        ~focus=
          switch (focus) {
          | Some(AlgebriteStep(f)) => Some(f)
          | _ => None
          },
        ~inject=x => inject(AlgebriteStep(x)),
        ~take_focus=x => take_focus(AlgebriteStep(x)),
        m,
      )
    | EvalStep(m) =>
      EvalStep.view_content(
        ~globals,
        ~hide_stepper,
        ~is_toplevel,
        ~proof,
        ~edit_syntax,
        ~main_editor,
        ~focus=
          switch (focus) {
          | Some(EvalStep(f)) => Some(f)
          | _ => None
          },
        ~inject=x => inject(EvalStep(x)),
        ~take_focus=x => take_focus(EvalStep(x)),
        m,
      )
    | ContradictionStep(m) =>
      ContradictionStep.view_content(
        ~globals,
        ~hide_stepper,
        ~is_toplevel,
        ~proof,
        ~edit_syntax,
        ~main_editor,
        ~focus=
          switch (focus) {
          | Some(ContradictionStep(f)) => Some(f)
          | _ => None
          },
        ~inject=x => inject(ContradictionStep(x)),
        ~take_focus=x => take_focus(ContradictionStep(x)),
        m,
      )
    | AssumeStep(m) =>
      AssumeStep.view_content(
        ~globals,
        ~hide_stepper,
        ~is_toplevel,
        ~proof,
        ~edit_syntax,
        ~main_editor,
        ~focus=
          switch (focus) {
          | Some(AssumeStep(f)) => Some(f)
          | _ => None
          },
        ~inject=x => inject(AssumeStep(x)),
        ~take_focus=x => take_focus(AssumeStep(x)),
        m,
      )
    | HaveStep(m) =>
      HaveStep.view_content(
        ~globals,
        ~hide_stepper,
        ~is_toplevel,
        ~proof,
        ~edit_syntax,
        ~main_editor,
        ~focus=
          switch (focus) {
          | Some(HaveStep(f)) => Some(f)
          | _ => None
          },
        ~inject=x => inject(HaveStep(x)),
        ~take_focus=x => take_focus(HaveStep(x)),
        m,
      )
    | RevertStep(m) =>
      RevertStep.view_content(
        ~globals,
        ~hide_stepper,
        ~is_toplevel,
        ~proof,
        ~edit_syntax,
        ~main_editor,
        ~focus=
          switch (focus) {
          | Some(RevertStep(f)) => Some(f)
          | _ => None
          },
        ~inject=x => inject(RevertStep(x)),
        ~take_focus=x => take_focus(RevertStep(x)),
        m,
      )
    | GeneralizeStep(m) =>
      GeneralizeStep.view_content(
        ~globals,
        ~hide_stepper,
        ~is_toplevel,
        ~proof,
        ~edit_syntax,
        ~main_editor,
        ~focus=
          switch (focus) {
          | Some(GeneralizeStep(f)) => Some(f)
          | _ => None
          },
        ~inject=x => inject(GeneralizeStep(x)),
        ~take_focus=x => take_focus(GeneralizeStep(x)),
        m,
      )
    };

  let justification = (model: model): string =>
    switch (model) {
    | InductionStep(m) => InductionStep.justification(m)
    | ForallStep(m) => ForallStep.justification(m)
    | FunextStep(m) => FunextStep.justification(m)
    | AxiomStep(m) => AxiomStep.justification(m)
    | AlgebriteStep(m) => AlgebriteStep.justification(m)
    | EvalStep(m) => EvalStep.justification(m)
    | ContradictionStep(m) => ContradictionStep.justification(m)
    | AssumeStep(m) => AssumeStep.justification(m)
    | HaveStep(m) => HaveStep.justification(m)
    | RevertStep(m) => RevertStep.justification(m)
    | GeneralizeStep(m) => GeneralizeStep.justification(m)
    };
}

and Stepper:
  STEPPER with
    type model = next_step and
    type action = step_action and
    type focus = step_focus = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = next_step;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = step_action;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus = step_focus;

  let init = init_step;

  let rec update =
          (~settings, action: step_action, model: next_step)
          : Updated.t(next_step) =>
    Updated.(
      switch (model, action) {
      | (Finished, _) => Finished |> raise_invalid_action
      | (MissingStep(m, rest), MissingStepAction(a)) =>
        let* m = MissingStep.Update.update(~settings, a, m);
        (MissingStep(m, rest): next_step);
      | (MissingStep(m, rest), EditorAction(a)) =>
        let* m = MissingStep.Update.update_editor(~settings, a, m);
        (MissingStep(m, rest): next_step);
      | (MissingStep(m, rest), NextStep(a)) =>
        let* rest = update(~settings, a, rest);
        (MissingStep(m, rest): next_step);
      | (MissingStep(_, _), StepKindAction(_)) =>
        model |> raise_invalid_action
      | (NextStep(sm), MissingStepAction(a)) =>
        let* picker = MissingStep.Update.update(~settings, a, sm.picker);
        (
          NextStep({
            ...sm,
            picker,
          }): next_step
        );
      | (NextStep(sm), EditorAction(a)) =>
        let* picker =
          MissingStep.Update.update_editor(~settings, a, sm.picker);
        (
          NextStep({
            ...sm,
            picker,
          }): next_step
        );
      | (NextStep(sm), NextStep(a)) =>
        let* next_step = update(~settings, a, sm.next_step);
        (
          NextStep({
            ...sm,
            next_step,
          }): next_step
        );
      | (NextStep(sm), StepKindAction(a)) =>
        let* step_kind = StepKind.update(~settings, a, sm.step_kind);
        (
          NextStep({
            ...sm,
            step_kind,
          }): next_step
        );
      }
    );

  /* The rows for the chain `proof`, starting from the goal `exp` until
   * the checker has goals for them. */
  let rec calculate =
          (
            ~settings: Calc.t(CoreSettings.t),
            ~exp: Calc.t(Exp.t),
            ~ctx: Calc.t(SemanticCtx.t),
            ~ana: Calc.t(Typ.t),
            ~proof: Calc.t(Proof.t),
            ~proof_map: Calc.t(ProofMap.t),
            ~proof_info_map: Calc.t(Statics.Map.t)=Calc.OldValue(
                                                      Id.Map.empty,
                                                    ),
            model: next_step,
          )
          : next_step => {
    let (head, tail) = Proofs.split(Calc.get_value(proof));
    let entry =
      ProofMap.lookup(Proof.rep_id(head), Calc.get_value(proof_map));
    /* The checker keeps goals as written (`sum == fold_left(_, sum, 0)`),
     * the program's names as Vars. Rows show them with the env
     * substituted, which turns the names into the functions the env
     * holds, printed folded (`<sum>`). Once only: a second pass would
     * unfold the recursive calls inside the inlined bodies.
     *
     * The picker works on the written form: an inlined `FixF` is not a
     * value, and its hidden unwrap would hide every steppable place. */
    let display = (e: Exp.t): Exp.t =>
      Substitution.in_exp(SemanticCtx.get_env(Calc.get_value(ctx)), e);
    /* The goal this row starts from, displayed and as written: the
     * checker's incoming, after any automatic steps it took first. */
    let (goal, written_goal) =
      switch (entry) {
      | Some({incoming: Some(incoming), auto_incoming, _}) =>
        let current =
          List.fold_left((_, (_, e)) => e, incoming, auto_incoming);
        (Calc.NewValue(display(current)), Calc.NewValue(current));
      | Some({incoming: None, _})
      | None => (exp, exp)
      };
    /* The picker rekeys the goal against the one it last showed, so its
     * editor, and the caret it holds, survives a pass that didn't change
     * the goal. */
    let calculate_picker =
        (
          ~hole: option(Proof.t),
          ~goal,
          ~written_goal,
          m: MissingStep.Model.t,
        ) =>
      MissingStep.Update.calculate(
        ~settings=Calc.get_value(settings),
        ~exp=goal,
        ~eval_exp=written_goal,
        ~ctx,
        {
          ...m,
          proof: Calc.Calculated(hole),
        },
      );
    /* The rows for `tail`, threading the last pass's rows through so they
     * keep their UI state. A stale `Finished` restarts as a missing row,
     * since `calculate` treats it as the end. */
    let calculate_rest = (~exp, tail: Proof.t, prev: next_step) =>
      calculate(
        ~settings,
        ~exp,
        ~ctx,
        ~ana,
        ~proof_info_map,
        ~proof=Calc.map(_ => tail, proof),
        ~proof_map,
        switch (prev) {
        | Finished => init_step
        | MissingStep(_, _)
        | NextStep(_) => prev
        },
      );
    let missing_row = (m: MissingStep.Model.t, prev_rest: next_step) =>
      MissingStep(
        calculate_picker(~hole=Some(head), ~goal, ~written_goal, m),
        /* A hole is the identity, so the rows after it start from the
         * same goal. */
        switch (tail) {
        | Some(tail) => calculate_rest(~exp=goal, tail, prev_rest)
        | None => Finished
        },
      );
    let step_row = (sm: step_model): next_step => {
      let step_kind = {
        let adapted = Proofs.adapt_kind(sm.step_kind, head);
        StepKind.calculate(
          ~settings,
          ~exp,
          ~ctx,
          /* Rows show elaboration output with fresh ids, which no info
           * map from further up covers. */
          ~info_map=Calc.OldValue(Id.Map.empty),
          ~proof_info_map,
          ~ana,
          ~proof=Calc.map(_ => head, proof),
          ~proof_map,
          adapted,
        )
        |> Option.value(~default=adapted);
      };
      /* The next row starts from this step's outgoing goal, which can
       * only have moved if the proof or the checker's output did. */
      let (next_goal, next_written_goal) =
        switch (entry) {
        | Some({outgoing: Some(outgoing), _}) =>
          let changed = Calc.is_new(proof) || Calc.is_new(proof_map);
          let mark = e => changed ? Calc.NewValue(e) : Calc.OldValue(e);
          (mark(display(outgoing)), mark(outgoing));
        | Some({outgoing: None, _})
        | None => (exp, exp)
        };
      let next_step =
        switch (tail) {
        | Some(tail) => calculate_rest(~exp=next_goal, tail, sm.next_step)
        /* Past the end of the chain, a picker row, even when the goal is
         * proven, so the final `true` gets a row of its own. It keeps the
         * last pass's model, and with it the caret. */
        | None =>
          let prev =
            switch (sm.next_step) {
            | MissingStep(m, _) => m
            | NextStep(_)
            | Finished => MissingStep.Model.init
            };
          MissingStep(
            calculate_picker(
              ~hole=None,
              ~goal=next_goal,
              ~written_goal=next_written_goal,
              prev,
            ),
            Finished,
          );
        };
      NextStep({
        entry: Calc.Calculated(entry),
        picker: calculate_picker(~hole=None, ~goal, ~written_goal, sm.picker),
        step_kind,
        next_step,
        proof: Calc.Calculated(head),
      });
    };
    switch (model) {
    | Finished => Finished
    /* While the proof here is a hole the row offers the picker; once it
     * is a step, the row becomes that step. */
    | MissingStep(m, prev_rest) when Proofs.is_hole(head) =>
      missing_row(m, prev_rest)
    | MissingStep(m, prev_rest) =>
      switch (Proofs.kind(head)) {
      | Some(step_kind) =>
        step_row({
          entry: Calc.Pending,
          picker: MissingStep.Model.init,
          step_kind,
          next_step: init_step,
          proof: Calc.Pending,
        })
      | None => missing_row(m, prev_rest)
      }
    /* The step was deleted back to a hole; the rows after it live on. */
    | NextStep(sm) when Proofs.is_hole(head) =>
      missing_row(MissingStep.Model.init, sm.next_step)
    | NextStep(sm) => step_row(sm)
    };
  };

  let rec get_cursor_info =
          (~inject, ~focus: step_focus, model: next_step)
          : Cursor.cursor(step_action) =>
    Cursor.(
      switch (model, focus) {
      | (Finished, _) => Cursor.empty
      | (
          MissingStep(picker, _) | NextStep({picker, _}),
          MissingStepFocus(selection),
        ) =>
        let+ ci =
          MissingStep.Selection.get_cursor_info(
            ~inject=x => inject(MissingStepAction(x): step_action),
            ~selection,
            picker,
          );
        MissingStepAction(ci);
      | (MissingStep(picker, _) | NextStep({picker, _}), Here(selection)) =>
        switch (picker.editor) {
        | Calculated(editor) =>
          let+ ci =
            StepperEditor.Selection.get_cursor_info(
              ~inject=x => inject(EditorAction(x)),
              ~selection,
              editor,
            );
          EditorAction(ci);
        | Pending => Cursor.empty
        }
      | (
          MissingStep(_, rest) | NextStep({next_step: rest, _}),
          Next(focus),
        ) =>
        let+ ci =
          get_cursor_info(
            ~inject=x => inject(NextStep(x): step_action),
            ~focus,
            rest,
          );
        NextStep(ci);
      | (NextStep({step_kind, _}), StepKindFocus(focus)) =>
        let+ ci =
          StepKind.get_cursor_info(
            ~inject=x => inject(StepKindAction(x)),
            ~focus,
            step_kind,
          );
        StepKindAction(ci);
      | (MissingStep(_, _), StepKindFocus(_)) => Cursor.empty
      }
    );

  /* Every row, step or missing step, renders the same way (Row.view).
   * The row's own `take_focus`, `inject` and `focus` change from row to
   * row; the rest is the same for the whole chain. */
  let view =
      (
        ~globals: Globals.t,
        ~take_focus: step_focus => Ui_effect.t(unit),
        ~inject: step_action => Ui_effect.t(unit),
        ~hide_stepper: Ui_effect.t(unit),
        ~focus: option(step_focus),
        ~is_toplevel: bool,
        ~edit_syntax: Patches.patch => Ui_effect.t(unit)=_ =>
                                                            Ui_effect.Ignore,
        ~main_editor: option(CodeEditable.Channel.t)=None,
        root: next_step,
      ) => {
    /* The row's goal in the picker's editor, with the picker's buttons
     * over the selection they act on. `emit` writes a picked step. */
    let view_goal =
        (
          ~take_focus,
          ~inject,
          ~focus,
          ~emit,
          picker: MissingStep.Model.t,
          model: StepperEditor.Model.t,
        ) => {
      let signal =
        Picker.signal(
          ~take_focus,
          ~hide_stepper,
          ~emit,
          ~main_editor,
          picker,
        );
      StepperEditor.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive => take_focus(Here())
          | TakeStep(idx) => signal(TakeStep(idx))
          | Refl(idx) => signal(Refl(idx)),
        ~inject=x => inject(EditorAction(x)),
        ~selected=
          switch (focus) {
          | Some(Here(_)) => true
          | _ => false
          },
        ~selected_id=Calc.get_saved(None, picker.selected_id),
        ~overlays=
          MissingStep.View.view_overlay(
            ~globals,
            ~signal,
            ~inject=x => inject(MissingStepAction(x)),
            ~selected=
              switch (focus) {
              | Some(MissingStepFocus(s)) => Some(s)
              | _ => None
              },
            picker,
          ),
        model,
      );
    };
    /* `prev_broken`: the mark of the step above, when it broke. */
    let rec view_step = (~take_focus, ~inject, ~focus, ~prev_broken, sm) => {
      let proof =
        sm.proof |> Calc.get_saved_exc(~print="step row before calculate");
      let status = Status.of_step(Calc.get_saved(None, sm.entry), proof);
      /* With history off, only the picker row at the end, where the user
       * acts, is shown. */
      let row =
        if (!globals.settings.core.evaluation.stepper_history) {
          [];
        } else {
          let step_kind_focus =
            switch (focus) {
            | Some(StepKindFocus(sk)) => Some(sk)
            | _ => None
            };
          let editor_view =
            switch (prev_broken, sm.picker.editor |> Calc.get_saved_opt) {
            | (Some(mark), _) => Row.view_error_above(~globals, mark)
            | (None, None) =>
              WebUtil.(div_c("step-placeholder", [Node.text("—")]))
            | (None, Some(editor)) =>
              view_goal(
                ~take_focus,
                ~inject,
                ~focus,
                ~emit=
                  term =>
                    edit_syntax(Patches.insert_step(~before=proof, term)),
                sm.picker,
                {
                  /* A step row draws no passive hints (step underlines,
                   * refl markers): those belong to the picker at the end of
                   * the chain. It acts through its selection. */

                  editor,
                  taken_steps:
                    switch (sm.step_kind) {
                    | AxiomStep(m) => [Exp.rep_id(m.at_exp)]
                    | _ => []
                    },
                  next_steps: [],
                  refls: [],
                },
              )
            };
          let popup_extra =
            switch (sm.step_kind) {
            | InductionStep(m) =>
              switch (m.inexhaustive |> Calc.get_saved_opt |> Option.join) {
              | Some(example) =>
                ProofMarkView.inexhaustive_message(~globals, example)
              | None => []
              }
            | _ => []
            };
          Row.view(
            ~globals,
            ~status,
            ~show_equiv=Option.is_none(prev_broken),
            ~delete=Some(edit_syntax(Patches.remove_step(proof))),
            ~popup_extra,
            ~repair=Row.repairs(~edit_syntax, status, proof),
            ~editor_view,
            ~justification=
              WebUtil.Node.text(StepKind.justification(sm.step_kind)),
            ~content=
              StepKind.view_content(
                ~globals,
                ~take_focus=f => take_focus(StepKindFocus(f)),
                ~hide_stepper,
                ~inject=a => inject(StepKindAction(a)),
                ~focus=step_kind_focus,
                ~is_toplevel,
                ~proof=Some(proof),
                ~edit_syntax,
                ~main_editor,
                sm.step_kind,
              ),
          );
        };
      row
      @ view_rest(
          ~take_focus,
          ~inject,
          ~focus,
          ~prev_leaf=proof,
          ~prev_broken=Status.broken_mark(status),
          sm.next_step,
        );
    }
    /* The rows after a row; `take_focus`, `inject` and `focus` are that
     * row's. A missing row with no hole of its own extends `prev_leaf`. */
    and view_rest =
        (~take_focus, ~inject, ~focus, ~prev_leaf, ~prev_broken, rest) => {
      let take_focus = f => take_focus(Next(f));
      let inject = x => inject(NextStep(x));
      let focus =
        switch (focus) {
        | Some(Next(f)) => Some(f)
        | _ => None
        };
      switch (rest) {
      | Finished => []
      | MissingStep(m, rest) =>
        view_missing_step(
          ~take_focus,
          ~inject,
          ~focus,
          ~target=
            switch (m.proof |> Calc.get_saved_opt |> Option.join) {
            | Some(hole) => Replace(hole)
            | None => Extend(prev_leaf)
            },
          /* A hole after a step is inside a Seq, so deleting it removes
           * something. */
          ~deletable=true,
          ~prev_broken,
          m,
          rest,
        )
      | NextStep(sm) =>
        view_step(~take_focus, ~inject, ~focus, ~prev_broken, sm)
      };
    }
    and view_missing_step =
        (
          ~take_focus,
          ~inject,
          ~focus,
          ~target: Patches.target,
          ~deletable,
          ~prev_broken,
          m: MissingStep.Model.t,
          rest,
        ) => {
      let hole = m.proof |> Calc.get_saved_opt |> Option.join;
      let status = Status.of_hole(hole);
      /* A `?` marks a hole written mid-proof, a step to come back for.
       * A hole at the end needs no mark: the picker already says the
       * proof continues here. */
      let mid_chain =
        switch (hole, rest) {
        | (Some(_), MissingStep(_, _) | NextStep(_)) => true
        | (None, _)
        | (Some(_), Finished) => false
        };
      let justification =
        WebUtil.(
          div_c(
            "missing-step-justification",
            (mid_chain ? [div_c("step-unknown", [Node.text("?")])] : [])
            @ [
              MissingStep.View.view_justification(
                ~globals,
                ~is_toplevel,
                ~hide_stepper,
                m,
              ),
            ],
          )
        );
      /* Splices the written `?`, and its `;`, out of the proof. */
      let delete =
        deletable
          ? Option.map(p => edit_syntax(Patches.remove_step(p)), hole)
          : None;
      let row =
        switch (prev_broken, m.editor |> Calc.get_saved_opt) {
        /* The justification still renders, so the broken step above is
         * easy to delete. */
        | (Some(mark), _) =>
          Row.view(
            ~globals,
            ~status,
            ~show_equiv=false,
            ~delete,
            ~popup_extra=[],
            ~repair=[],
            ~editor_view=Row.view_error_above(~globals, mark),
            ~justification,
            ~content=[],
          )
        | (None, None) => [justification]
        | (None, Some(editor)) =>
          let steps =
            switch (m.next_steps |> Calc.get_saved_opt) {
            | Some(AvailableSteps(steps)) => steps
            | Some(AutoStep(_))
            | None => []
            };
          Row.view(
            ~globals,
            ~status,
            ~show_equiv=true,
            ~delete,
            ~popup_extra=[],
            /* Picking a step replaces a malformed hole, so it needs no
             * other repair. */
            ~repair=[],
            ~editor_view=
              view_goal(
                ~take_focus,
                ~inject,
                ~focus,
                ~emit=term => edit_syntax(Patches.add_step(~target, term)),
                m,
                {
                  editor,
                  taken_steps: [],
                  next_steps:
                    List.map(MissingStep.Update.display_step_id(m), steps),
                  refls: List.map(Exp.rep_id, Calc.get_saved([], m.refls)),
                },
              ),
            ~justification,
            ~content=[],
          );
        };
      row
      @ view_rest(
          ~take_focus,
          ~inject,
          ~focus,
          ~prev_leaf=
            switch (target) {
            | Replace(leaf)
            | Extend(leaf) => leaf
            },
          ~prev_broken=Status.broken_mark(status),
          rest,
        );
    };
    let rows =
      switch (root) {
      | Finished => []
      | MissingStep(m, rest) =>
        switch (m.proof |> Calc.get_saved_opt |> Option.join) {
        | Some(hole) =>
          view_missing_step(
            ~take_focus,
            ~inject,
            ~focus,
            ~target=Replace(hole),
            /* A root hole with nothing after it is the whole proof, so
             * deleting it would change nothing. */
            ~deletable=
              switch (rest) {
              | Finished => false
              | MissingStep(_, _)
              | NextStep(_) => true
              },
            ~prev_broken=None,
            m,
            rest,
          )
        /* The root row has no leaf before it to extend. */
        | None => failwith("Stepper.view: the root missing row has no hole")
        }
      | NextStep(sm) =>
        view_step(~take_focus, ~inject, ~focus, ~prev_broken=None, sm)
      };
    WebUtil.[
      Node.div(~attrs=[Attr.classes(["stepper", "cell-result"])], rows),
    ];
  };
};
