open Util;
open Language;
open StepInterface;
open OptUtil.Syntax;

/* Note[Matt]: I've defined the types outside the modules here,
   this is in case we ever want to parameterize the types in
   the future, it'll be easier to please the OCaml compiler. */

[@deriving (show({with_path: false}), sexp, yojson)]
type cached_proof_map_entry =
  | EntryNotFound
  | EntryFound(ProofMap.entry);

[@deriving (show({with_path: false}), sexp, yojson)]
type next_step =
  | MissingStep(MissingStep.Model.t)
  | NextStep(step_model)
  | Finished

and step_kind_model =
  /* Nested steppers use `next_step` because `Stepper.model = next_step`. */
  | InductionStep(InductionStep.model'(next_step))
  | ForallStep(ForallStep.model'(next_step))
  | AxiomStep(AxiomStep.model'(next_step))
  | AlgebriteStep(AlgebriteStep.model'(next_step))
  | EvalStep(EvalStep.model'(next_step))

and step_model = {
  // Cache
  cached_proof_map_entry: Calc.saved(cached_proof_map_entry),
  // Editors
  pre_editors: Calc.saved(list(CodeSelectable.Model.t)), // only if auto steps are shown
  current_editor: Calc.saved(option(CodeSelectable.Model.t)),
  post_editors: Calc.saved(list(CodeSelectable.Model.t)), // only if auto steps are shown
  // Updated
  step_kind: step_kind_model,
  next_step,
  /* Seq-split head for this full step (required; MissingStep uses option). */
  proof: Calc.saved(Proof.t),
};

let init_step = MissingStep(MissingStep.Model.init);

let is_missing_step = (ns: next_step): bool =>
  switch (ns) {
  | MissingStep(_) => true
  | NextStep(_)
  | Finished => false
  };

let editor_exps_of_entry =
    (entry: ProofMap.entry): option((list(Exp.t), Exp.t, list(Exp.t))) =>
  switch (entry.incoming) {
  | None => None
  | Some(incoming) =>
    let (pre_rev, current) =
      List.fold_left(
        ((pre_rev, current), (_justification, resulting_exp)) =>
          ([current, ...pre_rev], resulting_exp),
        ([], incoming),
        entry.auto_incoming,
      );
    Some((
      List.rev(pre_rev),
      current,
      List.map(
        ((input_exp, _justification)) => input_exp,
        entry.auto_outgoing,
      ),
    ));
  };

/* Mirrors ProofMap.status_of_proof, reading the entry already cached on
 * a step instead of looking it up again. */
let status_of_entry = (entry: cached_proof_map_entry): option(bool) => {
  let literal = (b: bool, e: Exp.t) =>
    Exp.fast_equal(e, Exp.temp(Atom(Bool(b))));
  switch (entry) {
  | EntryFound({outgoing: Some(e), _}) =>
    literal(true, e) ? Some(true) : literal(false, e) ? Some(false) : None
  | EntryFound(_)
  | EntryNotFound => None
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type step_kind_action =
  | InductionStep(InductionStep.action'(step_action))
  | ForallStep(ForallStep.action'(step_action))
  | AxiomStep(AxiomStep.action'(step_action))
  | AlgebriteStep(AlgebriteStep.action'(step_action))
  | EvalStep(EvalStep.action'(step_action))

and step_action =
  | StepKindAction(step_kind_action)
  | MissingStepAction(MissingStep.Update.t)
  | EditorAction(CodeSelectable.Update.t)
  | NextStep(step_action);

[@deriving (show({with_path: false}), sexp, yojson)]
type step_kind_focus =
  | InductionStep(InductionStep.focus'(step_focus))
  | ForallStep(ForallStep.focus'(step_focus))
  | AxiomStep(AxiomStep.focus'(step_focus))
  | AlgebriteStep(AlgebriteStep.focus'(step_focus))
  | EvalStep(EvalStep.focus'(step_focus))

and step_focus =
  | StepKindFocus(step_kind_focus)
  | MissingStepFocus(MissingStep.Selection.t)
  | Here(CodeSelectable.Selection.t)
  | Next(step_focus);

/* Every rendered step row is backed either by the proof node it replaces,
 * or by the preceding leaf that a derived trailing row extends. */
type proof_target =
  | ReplaceProof(Proof.t)
  | ExtendProof(Proof.t);

module rec StepKind:
  STEP with
    type model = step_kind_model and
    type action = step_kind_action and
    type focus = step_kind_focus = {
  /* The StepKind code here is almost all dispatch to the
     individual step modules. */

  module InductionStep = InductionStep.F(Stepper);
  module ForallStep = ForallStep.F(Stepper);
  module AxiomStep = AxiomStep.F(Stepper);
  module AlgebriteStep = AlgebriteStep.F(Stepper);
  module EvalStep = EvalStep.F(Stepper);

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
      | (AxiomStep(a), AxiomStep(m)) =>
        let* s = AxiomStep.update(~settings, a, m);
        (AxiomStep(s): model);
      | (AlgebriteStep(a), AlgebriteStep(m)) =>
        let* s = AlgebriteStep.update(~settings, a, m);
        (AlgebriteStep(s): model);
      | (EvalStep(a), EvalStep(m)) =>
        let* s = EvalStep.update(~settings, a, m);
        (EvalStep(s): model);
      | (
          InductionStep(_) | ForallStep(_) | AxiomStep(_) | AlgebriteStep(_) |
          EvalStep(_),
          _,
        ) =>
        model |> Updated.raise_invalid_action
      }
    );
  };

  let can_undo = (a: action): bool => {
    switch (a) {
    | InductionStep(action) => InductionStep.can_undo(action)
    | ForallStep(action) => ForallStep.can_undo(action)
    | AxiomStep(action) => AxiomStep.can_undo(action)
    | AlgebriteStep(action) => AlgebriteStep.can_undo(action)
    | EvalStep(action) => EvalStep.can_undo(action)
    };
  };

  let calculate =
      (
        ~settings: Calc.t(CoreSettings.t),
        ~hidden: Calc.saved(bool),
        ~exp: Calc.t(Exp.t),
        ~ctx: Calc.t(SemanticCtx.t),
        ~editor: Calc.t(CodeSelectable.Model.t),
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
        ~hidden,
        ~exp,
        ~ctx,
        ~editor,
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
    | AxiomStep(m) =>
      let+ m = calculate_with(AxiomStep.calculate, m);
      (AxiomStep(m): model);
    | AlgebriteStep(m) =>
      let+ m = calculate_with(AlgebriteStep.calculate, m);
      (AlgebriteStep(m): model);
    | EvalStep(m) =>
      let+ m = calculate_with(EvalStep.calculate, m);
      (EvalStep(m): model);
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
      | (
          InductionStep(_) | ForallStep(_) | AxiomStep(_) | AlgebriteStep(_) |
          EvalStep(_),
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
        ~undo: option(Ui_effect.t(unit)),
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
        ~undo,
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
    | ForallStep(m) =>
      ForallStep.view_content(
        ~globals,
        ~hide_stepper,
        ~undo,
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
        ~undo,
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
        ~undo,
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
        ~undo,
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
    };

  let view_justification =
      (
        ~globals: Globals.t,
        ~focus: option(focus),
        ~inject: action => Ui_effect.t(unit),
        ~take_focus: focus => Ui_effect.t(unit),
        ~hide_stepper: Ui_effect.t(unit),
        ~undo: option(Ui_effect.t(unit)),
        ~is_toplevel: bool,
        ~proof: option(Proof.t),
        ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
        model: model,
      ) =>
    switch (model) {
    | InductionStep(m) =>
      InductionStep.view_justification(
        ~globals,
        ~focus=
          switch (focus) {
          | Some(InductionStep(f)) => Some(f)
          | Some(_)
          | None => None
          },
        ~inject=x => inject(InductionStep(x)),
        ~take_focus=x => take_focus(InductionStep(x)),
        ~hide_stepper,
        ~undo,
        ~is_toplevel,
        ~proof,
        ~edit_syntax,
        m,
      )
    | ForallStep(m) =>
      ForallStep.view_justification(
        ~globals,
        ~focus=
          switch (focus) {
          | Some(ForallStep(f)) => Some(f)
          | Some(_)
          | None => None
          },
        ~inject=x => inject(ForallStep(x)),
        ~take_focus=x => take_focus(ForallStep(x)),
        ~hide_stepper,
        ~undo,
        ~is_toplevel,
        ~proof,
        ~edit_syntax,
        m,
      )
    | AxiomStep(m) =>
      AxiomStep.view_justification(
        ~globals,
        ~focus=
          switch (focus) {
          | Some(AxiomStep(f)) => Some(f)
          | Some(_)
          | None => None
          },
        ~inject=x => inject(AxiomStep(x)),
        ~take_focus=x => take_focus(AxiomStep(x)),
        ~hide_stepper,
        ~undo,
        ~is_toplevel,
        ~proof,
        ~edit_syntax,
        m,
      )
    | AlgebriteStep(m) =>
      AlgebriteStep.view_justification(
        ~globals,
        ~focus=
          switch (focus) {
          | Some(AlgebriteStep(f)) => Some(f)
          | Some(_)
          | None => None
          },
        ~inject=x => inject(AlgebriteStep(x)),
        ~take_focus=x => take_focus(AlgebriteStep(x)),
        ~hide_stepper,
        ~undo,
        ~is_toplevel,
        ~proof,
        ~edit_syntax,
        m,
      )
    | EvalStep(m) =>
      EvalStep.view_justification(
        ~globals,
        ~focus=
          switch (focus) {
          | Some(EvalStep(f)) => Some(f)
          | Some(_)
          | None => None
          },
        ~inject=x => inject(EvalStep(x)),
        ~take_focus=x => take_focus(EvalStep(x)),
        ~hide_stepper,
        ~undo,
        ~is_toplevel,
        ~proof,
        ~edit_syntax,
        m,
      )
    };
}
and Stepper: {
  include
    STEPPER with
      type model = next_step and
      type action = step_action and
      type focus = step_focus;
  let get_validity: next_step => option(bool);
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = next_step;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = step_action;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus = step_focus;

  let init = init_step;

  let get_validity = (model: model): option(bool) =>
    switch (model) {
    | NextStep({cached_proof_map_entry, _}) =>
      cached_proof_map_entry
      |> Calc.get_saved_opt
      |> Option.map(status_of_entry)
      |> Option.join
    | MissingStep(_)
    | Finished => None
    };

  let rec update_full_step =
          (~settings, action: step_action, model: step_model)
          : Updated.t(step_model) => {
    Updated.(
      switch (action) {
      | EditorAction(ea) =>
        switch (model.current_editor) {
        | Calc.Calculated(Some(editor)) =>
          let* new_editor =
            CodeSelectable.Update.update(~settings, ea, editor);
          {
            ...model,
            current_editor: Calc.Calculated(Some(new_editor)),
          };
        | Calc.Pending
        | Calc.Calculated(None) => model |> raise_invalid_action
        }
      | NextStep(a) =>
        let* next_step = update(~settings, a, model.next_step);
        {
          ...model,
          next_step,
        };
      | StepKindAction(sk_action) =>
        let* step_kind =
          StepKind.update(~settings, sk_action, model.step_kind);
        {
          ...model,
          step_kind,
        };
      /* Missing-step actions belong to a MissingStep row, which is only
         ever reached through `next_step`. */
      | MissingStepAction(_) => model |> raise_invalid_action
      }
    );
  }
  and update =
      (~settings, action: step_action, model: next_step)
      : Updated.t(next_step) => {
    Updated.(
      switch (model, action) {
      | (Finished, _) => Finished |> raise_invalid_action
      | (MissingStep(m), MissingStepAction(a)) =>
        let* s = MissingStep.Update.update(~settings, a, m);
        (MissingStep(s): next_step);
      | (MissingStep(m), EditorAction(a)) =>
        let* s = MissingStep.Update.update_editor(~settings, a, m);
        (MissingStep(s): next_step);
      | (MissingStep(_), _) => model |> raise_invalid_action
      | (NextStep(sm), a) =>
        let* sm' = update_full_step(~settings, a, sm);
        (NextStep(sm'): next_step);
      }
    );
  };

  let rec can_undo = (a: step_action): bool => {
    switch (a) {
    | EditorAction(action) => CodeSelectable.Update.can_undo(action)
    | NextStep(next) => can_undo(next)
    | StepKindAction(action) => StepKind.can_undo(action)
    | MissingStepAction(action) => MissingStep.Update.can_undo(action)
    };
  };

  let make_editor =
      (~settings: Calc.t(CoreSettings.t), exp: Exp.t): CodeSelectable.Model.t =>
    exp
    |> CodeSelectable.Model.mk_from_exp(
         ~settings=Calc.get_value(settings),
         ~root=Exp,
       );

  /* Decompose the proof sub-term passed to a step. A `Seq(head, tail)`
   * represents "step then more steps", so the head describes the current
   * step kind and the tail is recursed into `next_step`. A leaf-shaped
   * term (AxiomStep, EvalStep, Forall, Induction, …) describes the
   * current step and has no further chain. */
  let split_proof = (p: Proof.t): (Proof.t, option(Proof.t)) =>
    switch (p) {
    | {term: Seq(head, tail), _} => (head, Some(tail))
    | _ => (p, None)
    };

  /* The hole-shaped proof terms that a MissingStep row stands in for. */
  let is_hole_proof = (p: Proof.t): bool =>
    switch (p.term) {
    | EmptyHole
    | Invalid(_)
    | MultiHole(_) => true
    | AxiomStep(_)
    | AlgebriteStep(_)
    | EvalStep(_)
    | Forall(_, _)
    | Induction(_, _)
    | Seq(_, _) => false
    };

  /* The step kind a proof sub-term calls for, as a placeholder model whose
   * fields the kind's own `calculate` fills in from the proof on the same
   * pass (see `AxiomStep.calculate`). */
  let kind_of_proof = (proof_head: Proof.t): option(step_kind_model) =>
    switch (proof_head.term) {
    | AxiomStep(_) =>
      Some(
        AxiomStep({
          name: "",
          at_idx: 0,
          at_exp: Exp.fresh(EmptyHole),
          direction: Direction.Right,
          equality: "",
          next_exp: Calc.Pending,
        }),
      )
    | AlgebriteStep(_) =>
      Some(
        AlgebriteStep({
          at_idx: 0,
          at_exp: Exp.fresh(EmptyHole),
          with_exp: Exp.fresh(EmptyHole),
          next_exp: Calc.Pending,
        }),
      )
    | EvalStep(_) => Some(EvalStep())
    | Forall(_, _) => Some(ForallStep(ForallStep.init(init_step)))
    | Induction(scrut, _) =>
      /* Seed the scrutinee editor from the proof's scrutinee (freshening
       * ids so the step's editor is independent of the syntax copy);
       * otherwise the scrutinee shows only in the syntax and the empty
       * editor writes back through, erasing it. */
      Some(
        InductionStep(
          InductionStep.init(~exp=Exp.replace_all_ids(scrut), ()),
        ),
      )
    | EmptyHole
    | Invalid(_)
    | MultiHole(_)
    | Seq(_, _) => None
    };

  /* When the proof sub-term contradicts the current step kind (e.g. the
   * proof became `AxiomStep(_)` after a syntax-side patch but the stepper
   * model still carries a different kind), swap in the proof-implied kind.
   *
   * Hole collapse (EmptyHole / Invalid / MultiHole → MissingStep) is
   * handled by `calculate`, not here. */
  let adapt_step_kind =
      (sk: step_kind_model, proof_head: Proof.t): step_kind_model =>
    switch (proof_head.term, sk) {
    | (AxiomStep(_), AxiomStep(_))
    | (AlgebriteStep(_), AlgebriteStep(_))
    | (EvalStep(_), EvalStep(_))
    | (Forall(_, _), ForallStep(_))
    | (Induction(_, _), InductionStep(_)) => sk
    | _ => kind_of_proof(proof_head) |> Option.value(~default=sk)
    };

  let empty_step_model = (step_kind: step_kind_model): step_model => {
    cached_proof_map_entry: Calc.Pending,
    pre_editors: Calc.Pending,
    current_editor: Calc.Pending,
    post_editors: Calc.Pending,
    step_kind,
    next_step: init_step,
    proof: Calc.Pending,
  };

  /* A MissingStep row owns the editor whose selection its overlay acts on,
   * so it needs calculating like any other row. `proof` is the hole this
   * row stands in for, or None when the row was synthesized past the end
   * of the chain. */
  let calculate_missing_step =
      (
        ~settings: Calc.t(CoreSettings.t),
        ~exp: Calc.t(Exp.t),
        ~ctx: Calc.t(SemanticCtx.t),
        ~proof: option(Proof.t),
        m: MissingStep.Model.t,
      )
      : MissingStep.Model.t =>
    MissingStep.Update.calculate(
      ~settings=Calc.get_value(settings),
      ~exp,
      ~ctx,
      {
        ...m,
        proof: Calc.Calculated(proof),
      },
    );

  let rec calculate_full_step =
          (
            ~settings: Calc.t(CoreSettings.t),
            ~exp: Calc.t(Exp.t),
            ~ctx: Calc.t(SemanticCtx.t),
            ~ana: Calc.t(Typ.t),
            ~proof: Calc.t(Proof.t),
            ~proof_map: Calc.t(ProofMap.t),
            ~proof_info_map: Calc.t(Statics.Map.t),
            model: step_model,
          )
          : step_model => {
    let make_editor = make_editor(~settings);
    let editor_groups = (entry: cached_proof_map_entry) =>
      switch (entry) {
      | EntryFound(entry) =>
        switch (editor_exps_of_entry(entry)) {
        | Some((pre, current, post)) => (
            List.map(make_editor, pre),
            Some(make_editor(current)),
            List.map(make_editor, post),
          )
        | None => ([], None, [])
        }
      | EntryNotFound => ([], None, [])
      };
    let (proof_head, proof_tail) = split_proof(Calc.get_value(proof));
    let proof_head =
      Calc.is_new(proof)
        ? Calc.NewValue(proof_head) : Calc.OldValue(proof_head);
    let cached_proof_map_entry =
      switch (
        ProofMap.lookup(
          Proof.rep_id(Calc.get_value(proof_head)),
          Calc.get_value(proof_map),
        )
      ) {
      | Some(entry) => EntryFound(entry)
      | None => EntryNotFound
      };
    let (pre_editors, current_editor, post_editors) =
      editor_groups(cached_proof_map_entry);
    let step_kind = {
      let adapted =
        adapt_step_kind(model.step_kind, Calc.get_value(proof_head));
      StepKind.calculate(
        ~settings,
        /* Auto steps are not rendered yet (see pre_editors / post_editors),
         * so no row is hidden. */
        ~hidden=Calc.Calculated(false),
        ~exp,
        ~ctx,
        ~editor=
          Calc.NewValue(
            switch (current_editor) {
            | Some(editor) => editor
            | None => make_editor(Calc.get_value(exp))
            },
          ),
        /* Rows render elaboration output with freshened ids, so no info map
         * from further up covers them; proof-backed kinds read what they
         * need out of ~proof / ~proof_map / ~proof_info_map instead. */
        ~info_map=Calc.OldValue(Id.Map.empty),
        ~proof_info_map,
        ~ana,
        ~proof=proof_head,
        ~proof_map,
        adapted,
      )
      |> Option.value(~default=adapted);
    };
    /* Steps chain off each other's results, so whatever follows this step
     * starts from its outgoing expression — which can only have moved if
     * the proof or the checker's output did. */
    let next_exp =
      switch (cached_proof_map_entry) {
      | EntryFound({outgoing: Some(outgoing), _}) =>
        Calc.is_new(proof) || Calc.is_new(proof_map)
          ? Calc.NewValue(outgoing) : Calc.OldValue(outgoing)
      | EntryFound(_)
      | EntryNotFound => exp
      };
    let next_step =
      switch (proof_tail) {
      | Some(tail) =>
        let tail =
          Calc.is_new(proof) ? Calc.NewValue(tail) : Calc.OldValue(tail);
        calculate(
          ~settings,
          ~exp=next_exp,
          ~ctx,
          ~ana,
          ~proof_info_map,
          ~proof=tail,
          ~proof_map,
          init_step,
        );
      | None when status_of_entry(cached_proof_map_entry) != Some(true) =>
        /* Synthesized trailing row — no backing proof leaf. */
        MissingStep(
          calculate_missing_step(
            ~settings,
            ~exp=next_exp,
            ~ctx,
            ~proof=None,
            MissingStep.Model.init,
          ),
        )
      | None => Finished
      };
    {
      cached_proof_map_entry: Calc.Calculated(cached_proof_map_entry),
      pre_editors: Calc.Calculated(pre_editors),
      current_editor: Calc.Calculated(current_editor),
      post_editors: Calc.Calculated(post_editors),
      step_kind,
      next_step,
      proof: proof_head |> Calc.save,
    };
  }
  and calculate =
      (
        ~settings: Calc.t(CoreSettings.t),
        ~exp: Calc.t(Exp.t),
        ~ctx: Calc.t(SemanticCtx.t),
        ~ana: Calc.t(Typ.t),
        ~proof: Calc.t(Proof.t),
        ~proof_map: Calc.t(ProofMap.t),
        ~proof_info_map: Calc.t(Statics.Map.t)=Calc.OldValue(Id.Map.empty),
        model: next_step,
      )
      : next_step => {
    let full_step = (model: step_model): next_step =>
      NextStep(
        calculate_full_step(
          ~settings,
          ~exp,
          ~ctx,
          ~ana,
          ~proof,
          ~proof_map,
          ~proof_info_map,
          model,
        ),
      );
    let missing_step =
        (~proof_head: Proof.t, m: MissingStep.Model.t): next_step =>
      MissingStep(
        calculate_missing_step(
          ~settings,
          ~exp,
          ~ctx,
          ~proof=Some(proof_head),
          m,
        ),
      );
    let (proof_head, _) = split_proof(Calc.get_value(proof));
    switch (model) {
    | Finished => Finished
    /* Promote-or-stay: once the proof here describes a real step, this row
     * becomes that step; while it is still a hole, it stays the row that
     * offers the step-picking UI. */
    | MissingStep(m) when is_hole_proof(proof_head) =>
      missing_step(~proof_head, m)
    | MissingStep(m) =>
      switch (kind_of_proof(proof_head)) {
      | Some(step_kind) => full_step(empty_step_model(step_kind))
      | None => missing_step(~proof_head, m)
      }
    /* The step this row rendered has been deleted back to a hole. */
    | NextStep(_) when is_hole_proof(proof_head) =>
      missing_step(~proof_head, MissingStep.Model.init)
    | NextStep(sm) => full_step(sm)
    };
  };

  let rec get_cursor_info_full_step =
          (~inject, ~focus: step_focus, model: step_model)
          : Cursor.cursor(step_action) => {
    Cursor.(
      switch (focus) {
      | StepKindFocus(sk) =>
        let+ ci =
          StepKind.get_cursor_info(
            ~inject=x => inject(StepKindAction(x)),
            ~focus=sk,
            model.step_kind,
          );
        StepKindAction(ci);
      | Here(a) =>
        switch (model.current_editor) {
        | Calc.Calculated(Some(editor)) =>
          let+ ci =
            StepperEditor.Selection.get_cursor_info(
              ~inject=x => inject(EditorAction(x)),
              ~selection=a,
              editor,
            );
          EditorAction(ci);
        | Calc.Pending
        | Calc.Calculated(None) => Cursor.empty
        }
      | Next(a) =>
        let+ ci =
          get_cursor_info(
            ~inject=x => inject(NextStep(x): action),
            ~focus=a,
            model.next_step,
          );
        NextStep(ci);
      /* A step row has no missing-step controls of its own. */
      | MissingStepFocus(_) => Cursor.empty
      }
    );
  }
  and get_cursor_info =
      (~inject, ~focus: step_focus, model: next_step)
      : Cursor.cursor(step_action) => {
    Cursor.(
      switch (model, focus) {
      | (Finished, _) => Cursor.empty
      | (MissingStep(m), MissingStepFocus(selection)) =>
        let+ focus_info =
          MissingStep.Selection.get_cursor_info(
            ~inject=x => inject(MissingStepAction(x): action),
            ~selection,
            m,
          );
        MissingStepAction(focus_info);
      | (MissingStep(m), Here(selection)) =>
        switch (m.editor) {
        | Calc.Calculated(editor) =>
          let+ ci =
            StepperEditor.Selection.get_cursor_info(
              ~inject=x => inject(EditorAction(x)),
              ~selection,
              editor,
            );
          EditorAction(ci);
        | Calc.Pending => Cursor.empty
        }
      | (MissingStep(_), _) => Cursor.empty
      | (NextStep(sm), focus) =>
        get_cursor_info_full_step(~inject, ~focus, sm)
      }
    );
  };

  /* Expressions embedded into a freshly-inserted proof step (at_exp /
   * with_exp / scrut) are sliced from the current step's expression, so:
   *  - their tile ids already occur elsewhere in the rendered theorem —
   *    freshen them, otherwise a tile id is duplicated and
   *    Highlight.of_tile fails with a shard mismatch; and
   *  - they may contain closures the evaluator substituted in, which
   *    have no surface syntax — writing them verbatim corrupts the
   *    program and crashes rendering. Substitution removes them by
   *    inlining each closure's environment into its body. */
  let embed_exp = (e: Exp.t): Exp.t =>
    e |> Substitution.in_exp(Environment.empty) |> Exp.replace_all_ids;

  let axiom_step_term =
      (
        ~at_idx: int,
        ~at_exp: Exp.t,
        ~direction: Direction.t,
        ~equality: string,
      )
      : TermBase.Proof.term =>
    AxiomStep({
      at_idx: Exp.fresh(Atom(Int(Bigint.of_int(at_idx)))),
      at_exp: at_exp |> embed_exp,
      direction,
      equality: Exp.fresh(Var(equality)),
    });

  let algebrite_step_term =
      (~at_idx: int, ~at_exp: Exp.t, ~with_exp: Exp.t): TermBase.Proof.term =>
    AlgebriteStep({
      at_idx: Exp.fresh(Atom(Int(Bigint.of_int(at_idx)))),
      at_exp: at_exp |> embed_exp,
      with_exp: with_exp |> embed_exp,
    });

  let eval_step_term = (~at_idx: int, ~at_exp: Exp.t): TermBase.Proof.term =>
    EvalStep({
      at_idx: Exp.fresh(Atom(Int(Bigint.of_int(at_idx)))),
      at_exp: at_exp |> embed_exp,
    });

  let induction_term = (~scrut: option(Exp.t)): TermBase.Proof.term =>
    Induction(
      scrut
      |> Option.map(embed_exp)
      |> Option.value(~default=Exp.fresh(EmptyHole)),
      /* Start with no cases; the user adds them via the InductionStep UI.
       * `MakeTerm.prul` accepts a bare scrutinee with no
       * `| <pat> => <body>` tiles. */
      [],
    );

  let forall_term = (): TermBase.Proof.term =>
    Forall(Pat.fresh(EmptyHole), Proof.fresh(EmptyHole));

  /* Patch that lands a new step in the proof syntax. Two shapes:
   *   - this row is backed by a written hole (`?`): replace the hole with
   *     the bare step — no trailing `; ?` is appended, the next-step UI is
   *     synthesized by `calculate` regardless;
   *   - this row was synthesized past the end of the chain (no backing
   *     proof): extend the chain by replacing the previous step's leaf
   *     with `Seq(leaf, new_step)`.
   * Compound kinds (Forall, Induction) own their body proof rather than
   * chaining, but they land in the syntax the same way. */
  let add_step_patch =
      (~proof_target: proof_target, proof_term: TermBase.Proof.term)
      : Haz3lcore.EditorTransform.patch => {
    let head = Proof.fresh(proof_term);
    /* A hole "extends" by being replaced — it stands for "the proof
     * continues here", not for a step of its own. */
    let extend = (prev: Proof.t) =>
      switch (prev.term) {
      | EmptyHole
      | Invalid(_)
      | MultiHole(_) => head
      | _ => Proof.fresh(Seq(prev, head))
      };
    switch (proof_target) {
    | ReplaceProof(p) =>
      Haz3lcore.EditorTransform.mk_proof_patch(
        ~target_id=Proof.rep_id(p),
        head,
      )
    | ExtendProof(prev) =>
      Haz3lcore.EditorTransform.mk_proof_patch(
        ~target_id=Proof.rep_id(prev),
        extend(prev),
      )
    };
  };

  /* Removing a row rewrites its proof subtree back to a hole. */
  let remove_step_patch = (p: Proof.t): Haz3lcore.EditorTransform.patch =>
    Haz3lcore.EditorTransform.mk_proof_patch(
      ~target_id=Proof.rep_id(p),
      Proof.fresh(EmptyHole),
    );

  /* Every row in the chain — step or missing step — renders the same way:
   * the expression it starts from, then its justification. */
  let step_row =
      (
        ~editor_view: WebUtil.Node.t,
        ~justification: WebUtil.Node.t,
        ~content: list(WebUtil.Node.t),
      ) =>
    WebUtil.[
      Node.div(
        ~attrs=[Attr.class_("step-border")],
        [
          div_c(
            "step-display",
            [
              div_c("equiv", [Node.text("≡")]),
              div_c("step-output", [editor_view]),
              justification,
            ],
          ),
        ]
        @ content,
      ),
    ];

  let rec view_step =
          (
            ~globals: Globals.t,
            ~take_focus: step_focus => Ui_effect.t(unit),
            ~inject: step_action => Ui_effect.t(unit),
            ~hide_stepper: Ui_effect.t(unit),
            ~focus: option(step_focus),
            ~is_toplevel: bool=false,
            ~undo: option(Ui_effect.t(unit)),
            /* Forwarded write channel for structural proof edits. */
            ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit)=
                                                                    _ =>
                                                                    Ui_effect.Ignore,
            /* Forwarded main-editor capability handle for step views
             * that render slices of the surrounding syntax as
             * sub-editors (see SubEditor.re / CodeEditable.Channel). */
            ~main_editor: option(CodeEditable.Channel.t)=None,
            ~proof_target: proof_target,
            model: step_model,
          ) => {
    let current_proof: option(Proof.t) = model.proof |> Calc.get_saved_opt;
    /* Removing a child rewrites this step's proof subtree to a hole. */
    let emit_remove_step = (): Ui_effect.t(unit) =>
      switch (current_proof) {
      | Some(p) => edit_syntax(remove_step_patch(p))
      | None => Ui_effect.Ignore
      };
    /* Step rows are history: the row the user acts on is the trailing
     * MissingStep, so with history off only that row is shown. */
    let current_step =
      if (!globals.settings.core.evaluation.stepper_history) {
        [];
      } else {
        let taken_steps =
          switch (model.step_kind) {
          | AxiomStep(m) => [m.at_exp |> Exp.rep_id]
          | _ => []
          };
        let editor_opt =
          model.current_editor |> Calc.get_saved_exc(~print="current_editor");
        switch (editor_opt) {
        | None => []
        | Some(editor) =>
          let editor_view =
            StepperEditor.View.view(
              ~globals,
              ~signal=
                fun
                | MakeActive => take_focus(Here())
                | TakeStep(_) => Ui_effect.Ignore
                | Refl(_) => Ui_effect.Ignore,
              ~inject=x => inject(EditorAction(x)),
              ~selected=
                switch (focus) {
                | Some(Here(_)) => true
                | _ => false
                },
              ~selected_id=None,
              ~overlays=[],
              StepperEditor.Model.{
                editor,
                taken_steps,
                next_steps: [],
                refls: [],
              },
            );
          let step_kind_focus =
            switch (focus) {
            | Some(StepKindFocus(sk)) => Some(sk)
            | Some(_)
            | None => None
            };
          let justification =
            StepKind.view_justification(
              ~globals,
              ~take_focus=f => take_focus(StepKindFocus(f)),
              ~hide_stepper,
              ~inject=a => inject(StepKindAction(a)),
              ~is_toplevel,
              ~focus=step_kind_focus,
              ~undo,
              ~proof=current_proof,
              ~edit_syntax,
              model.step_kind,
            );
          let content =
            StepKind.view_content(
              ~globals,
              ~take_focus=f => take_focus(StepKindFocus(f)),
              ~hide_stepper,
              ~inject=a => inject(StepKindAction(a)),
              ~focus=step_kind_focus,
              ~is_toplevel,
              ~undo,
              ~proof=current_proof,
              ~edit_syntax,
              ~main_editor,
              model.step_kind,
            );
          step_row(~editor_view, ~justification, ~content);
        };
      };
    let next_step_view =
      switch (model.next_step) {
      | Finished => []
      | MissingStep(m) =>
        view_missing_step(
          ~globals,
          ~is_toplevel,
          ~take_focus=f => take_focus(Next(f)),
          ~hide_stepper,
          ~inject=x => inject(NextStep(x)),
          ~focus=
            switch (focus) {
            | Some(Next(s)) => Some(s)
            | _ => None
            },
          ~undo=Some(emit_remove_step()),
          ~edit_syntax,
          ~proof_target=
            switch (m.proof |> Calc.get_saved_opt |> Option.join) {
            | Some(p) => ReplaceProof(p)
            | None =>
              switch (proof_target) {
              | ReplaceProof(leaf)
              | ExtendProof(leaf) => ExtendProof(leaf)
              }
            },
          m,
        )
      | NextStep(next_model) =>
        let next_target =
          switch (next_model.proof |> Calc.get_saved_opt) {
          | Some(p) => ReplaceProof(p)
          | None =>
            failwith(
              "next_model.proof Pending after calculate — unreachable",
            )
          };
        view_step(
          ~globals,
          ~is_toplevel,
          ~take_focus=f => take_focus(Next(f)),
          ~hide_stepper,
          ~inject=x => inject(NextStep(x)),
          ~focus=
            switch (focus) {
            | Some(Next(s)) => Some(s)
            | _ => None
            },
          ~undo=Some(emit_remove_step()),
          ~edit_syntax,
          ~main_editor,
          ~proof_target=next_target,
          next_model,
        );
      };
    current_step @ next_step_view;
  }
  and view_missing_step =
      (
        ~globals: Globals.t,
        ~take_focus: step_focus => Ui_effect.t(unit),
        ~inject: step_action => Ui_effect.t(unit),
        ~hide_stepper: Ui_effect.t(unit),
        ~focus: option(step_focus),
        ~is_toplevel: bool,
        ~undo: option(Ui_effect.t(unit)),
        ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
        ~proof_target: proof_target,
        m: MissingStep.Model.t,
      ) => {
    let justification =
      MissingStep.View.view_justification(
        ~globals,
        ~is_toplevel,
        ~hide_stepper,
        ~undo,
        m,
      );
    let available_steps =
      switch (m.next_steps |> Calc.get_saved_opt) {
      | Some(AvailableSteps(steps)) => steps
      | Some(AutoStep(_))
      | None => []
      };
    let refls = Calc.get_saved([], m.refls);
    /* Everything the overlay offers is a step written into the proof
     * syntax; `calculate` picks the new step up on the next pass. */
    let emit = term => edit_syntax(add_step_patch(~proof_target, term));
    let signal = (event: MissingStep.View.event) =>
      switch (event) {
      | MakeActive(selection) => take_focus(MissingStepFocus(selection))
      | HideStepper => hide_stepper
      | AddForall => emit(forall_term())
      | AddInduction(scrut) => emit(induction_term(~scrut))
      | AddAxiomStep(_name, at_idx, at_exp, direction, equality) =>
        emit(axiom_step_term(~at_idx, ~at_exp, ~direction, ~equality))
      | AddAlgebriteStep(at_idx, at_exp, with_exp) =>
        emit(algebrite_step_term(~at_idx, ~at_exp, ~with_exp))
      | TakeStep(idx) =>
        switch (List.nth_opt(available_steps, idx)) {
        | Some(step) =>
          emit(
            eval_step_term(
              ~at_idx=EvaluatorStep.get_exp_idx(step),
              ~at_exp=EvaluatorStep.get_at_exp(step),
            ),
          )
        | None => Ui_effect.Ignore
        }
      /* Reflexivity is the refl_eq axiom applied to the picked equality. */
      | Refl(idx) =>
        switch (List.nth_opt(refls, idx), m.full_exp |> Calc.get_saved_opt) {
        | (Some(at_exp), Some(full_exp)) =>
          emit(
            axiom_step_term(
              ~at_idx=ProofHacks.exp_idx(at_exp, full_exp),
              ~at_exp,
              ~direction=Direction.Right,
              ~equality="refl_eq",
            ),
          )
        | (None, _)
        | (_, None) => Ui_effect.Ignore
        }
      };
    switch (m.editor |> Calc.get_saved_opt) {
    | None => [justification]
    | Some(editor) =>
      let editor_view =
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
          ~selected_id=Calc.get_saved(None, m.selected_id),
          /* The action buttons are positioned over the selection they act
           * on, so they render as an overlay of this row's editor. */
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
              m,
            ),
          StepperEditor.Model.{
            editor,
            taken_steps: [],
            next_steps: List.map(EvaluatorStep.get_step_id, available_steps),
            refls: List.map(Exp.rep_id, refls),
          },
        );
      step_row(~editor_view, ~justification, ~content=[]);
    };
  };

  let view =
      (
        ~globals: Globals.t,
        ~take_focus: step_focus => Ui_effect.t(unit),
        ~inject: step_action => Ui_effect.t(unit),
        ~hide_stepper: Ui_effect.t(unit),
        ~focus: option(step_focus),
        ~is_toplevel: bool,
        ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit)=_ =>
                                                                    Ui_effect.Ignore,
        ~main_editor: option(CodeEditable.Channel.t)=None,
        root: next_step,
      ) => {
    let body =
      switch (root) {
      | Finished => []
      | MissingStep(m) =>
        switch (m.proof |> Calc.get_saved_opt |> Option.join) {
        | Some(p) =>
          view_missing_step(
            ~globals,
            ~take_focus,
            ~hide_stepper,
            ~inject,
            ~focus,
            ~is_toplevel,
            ~undo=None,
            ~edit_syntax,
            ~proof_target=ReplaceProof(p),
            m,
          )
        | None =>
          /* Root MissingStep with no backing leaf — cannot ExtendProof. */
          failwith(
            "MissingStep root proof_target: proof is None/Pending (no leaf for ExtendProof)",
          )
        }
      | NextStep(sm) =>
        let proof_target =
          switch (sm.proof |> Calc.get_saved_opt) {
          | Some(p) => ReplaceProof(p)
          | None =>
            failwith(
              "NextStep root proof_target: proof Pending after calculate",
            )
          };
        view_step(
          ~globals,
          ~take_focus,
          ~hide_stepper,
          ~inject,
          ~focus,
          ~is_toplevel,
          ~undo=None,
          ~edit_syntax,
          ~main_editor,
          ~proof_target,
          sm,
        );
      };
    WebUtil.[
      Node.div(~attrs=[Attr.classes(["stepper", "cell-result"])], body),
    ];
  };
};
