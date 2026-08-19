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
  /* A hole-shaped proof term rendered as a step-picker row. Carries the
   * rest of the chain: a hole is the identity (the goal passes through,
   * see ProofCheck), so steps written after it still check and render —
   * every piece of proof syntax gets a row, junk included. Terminal
   * (trailing picker) missing steps carry `Finished`. */
  | MissingStep(MissingStep.Model.t, next_step)
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
  /* The row's expression display and step-picking machinery: every step
   * row is editable just like a missing-step row — selecting a sub-term
   * offers steps, and taking one INSERTS a new step before this one
   * (see `insert_step_patch`). Owns the row's editor. */
  insert: MissingStep.Model.t,
  post_editors: Calc.saved(list(CodeSelectable.Model.t)), // only if auto steps are shown
  // Updated
  step_kind: step_kind_model,
  next_step,
  /* Seq-split head for this full step (required; MissingStep uses option). */
  proof: Calc.saved(Proof.t),
};

let init_step = MissingStep(MissingStep.Model.init, Finished);

let is_missing_step = (ns: next_step): bool =>
  switch (ns) {
  | MissingStep(_, _) => true
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

/* The unified per-row error status, derived from the proof-check marks
 * the checker recorded for this row (already cached on every step).
 * Rendered as a status chip + message by `step_row`. */
[@deriving (show({with_path: false}), sexp, yojson)]
type step_status =
  /* Checked; no marks. (Says nothing about whether the goal is proven —
   * that's `status_of_entry`.) */
  | StepOk
  /* Checked and not at fault, but the step's own arguments contain a
   * hole (Proof.args_have_hole): an induction case pattern or
   * scrutinee, a forall binder, an axiom target. Deliberately narrow —
   * holes in nested sub-proofs and undischarged chains are NOT marked,
   * since every such case renders its own "…" continuation row. */
  | StepIncomplete
  /* This row is at fault; the payload is the highest-priority mark. */
  | StepBroken(ProofMark.t)
  /* An earlier row is at fault, so this row has no incoming goal. */
  | StepBlocked
  /* No proof-map entry yet (evaluation pending or unavailable). */
  | StepPending;

let status_of_step_entry = (entry: cached_proof_map_entry): step_status =>
  switch (entry) {
  | EntryNotFound => StepPending
  | EntryFound({marks, _}) =>
    let blocked =
      List.exists(
        fun
        | ProofMark.MissingIncoming => true
        | _ => false,
        marks,
      );
    if (blocked) {
      StepBlocked;
    } else {
      switch (ProofMark.highest(marks)) {
      | Some(m) => StepBroken(m)
      | None => StepOk
      };
    };
  };

/* MissingStep rows stand in for hole-shaped proof terms; garbage syntax
 * (Invalid/MultiHole) is broken. A genuine EmptyHole — including the
 * trailing step-picker row — carries no indicator: the picker itself
 * already says the proof continues here. */
let status_of_missing_step_proof = (proof: option(Proof.t)): step_status =>
  switch (proof) {
  | Some({term: Invalid(_) | MultiHole(_), _}) =>
    StepBroken(ProofMark.MalformedProofTerm)
  | Some(_)
  | None => StepOk
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
    };

  let view_justification =
      (
        ~globals: Globals.t,
        ~focus: option(focus),
        ~inject: action => Ui_effect.t(unit),
        ~take_focus: focus => Ui_effect.t(unit),
        ~hide_stepper: Ui_effect.t(unit),
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
    | MissingStep(_, _)
    | Finished => None
    };

  let rec update_full_step =
          (~settings, action: step_action, model: step_model)
          : Updated.t(step_model) => {
    Updated.(
      switch (action) {
      | EditorAction(ea) =>
        let* insert =
          MissingStep.Update.update_editor(~settings, ea, model.insert);
        {
          ...model,
          insert,
        };
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
      /* Step rows carry missing-step machinery of their own (the
         insert-a-step-here overlay), so these actions land on it. */
      | MissingStepAction(a) =>
        let* insert = MissingStep.Update.update(~settings, a, model.insert);
        {
          ...model,
          insert,
        };
      }
    );
  }
  and update =
      (~settings, action: step_action, model: next_step)
      : Updated.t(next_step) => {
    Updated.(
      switch (model, action) {
      | (Finished, _) => Finished |> raise_invalid_action
      | (MissingStep(m, ns), MissingStepAction(a)) =>
        let* s = MissingStep.Update.update(~settings, a, m);
        (MissingStep(s, ns): next_step);
      | (MissingStep(m, ns), EditorAction(a)) =>
        let* s = MissingStep.Update.update_editor(~settings, a, m);
        (MissingStep(s, ns): next_step);
      | (MissingStep(m, ns), NextStep(a)) =>
        let* ns' = update(~settings, a, ns);
        (MissingStep(m, ns'): next_step);
      | (MissingStep(_, _), _) => model |> raise_invalid_action
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
  let rec split_proof = (p: Proof.t): (Proof.t, option(Proof.t)) =>
    switch (p) {
    /* Re-associate a left-nested Seq (an inserted step lands as
     * `Seq(Seq(new, old), tail)` on the AST-owned path, where no reparse
     * flattens the `;` chain) so the head is always a leaf step. The
     * rebuilt right-nested node reuses the inner Seq's ids; Seq-node ids
     * aren't row-relevant (rows key off leaf heads). */
    | {term: Seq({term: Seq(a, b), _} as inner, tail), _} =>
      split_proof({
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
    | Assume(_, _)
    | Generalize(_, _)
    | Seq(_, _) => false
    };

  /* The step kind a proof sub-term calls for, as a placeholder model whose
   * fields the kind's own `calculate` fills in from the proof on the same
   * pass (see `AxiomStep.calculate`). */
  let kind_of_proof = (proof_head: Proof.t): option(step_kind_model) =>
    switch (proof_head.term) {
    | AxiomStep({direction, _}) =>
      Some(
        AxiomStep({
          name: "",
          at_idx: 0,
          at_exp: Exp.fresh(EmptyHole),
          direction,
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
    /* Assume/Generalize have no stepper-UI step kind yet (Phase 1: no
     * stepper-UI work; obligation rendering is a later phase). */
    | Assume(_, _)
    | Generalize(_, _)
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
    insert: MissingStep.Model.init,
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
            Some(current),
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
    /* Row editors carry live UI state (caret, selection), so rebuild
     * them only when what they display could have changed — the proof,
     * the checker's output, or settings. Rebuilding unconditionally
     * would replace the editor after every dispatch (any action
     * triggers a calculate pass), snapping the caret to a fresh
     * editor's end position and making selection impossible. */
    let editors_stale =
      Calc.is_new(settings)
      || Calc.is_new(proof)
      || Calc.is_new(proof_map)
      || model.pre_editors == Calc.Pending;
    let (pre_editors, current_exp, post_editors) =
      editors_stale
        ? editor_groups(cached_proof_map_entry)
        : (
          model.pre_editors |> Calc.get_saved([]),
          model.insert.full_exp |> Calc.saved_to_option,
          model.post_editors |> Calc.get_saved([]),
        );
    /* The row's expression display, selection, and step-picking overlay
     * are the same machinery a MissingStep row uses; the difference is
     * only what a picked step does (insert before this step rather than
     * replace a hole). `MissingStep.Update.calculate` rekeys the
     * expression internally, so its editor (and the caret/selection it
     * holds) survives passes where the expression didn't change. */
    let insert =
      calculate_missing_step(
        ~settings,
        ~exp=
          Calc.NewValue(
            switch (current_exp) {
            | Some(e) => e
            | None => Calc.get_value(exp)
            },
          ),
        ~ctx,
        ~proof=None,
        model.insert,
      );
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
            switch (insert.editor |> Calc.saved_to_option) {
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
        /* Thread the existing sub-model through so nested rows keep
         * their UI state across calculate passes; `calculate`'s
         * promote-or-stay / adapt logic reconciles it with the proof.
         * A stale `Finished` can't be threaded — `calculate` treats it
         * as terminal — so restart from a missing row in that case. */
        let prev_next =
          switch (model.next_step) {
          | Finished => init_step
          | ns => ns
          };
        calculate(
          ~settings,
          ~exp=next_exp,
          ~ctx,
          ~ana,
          ~proof_info_map,
          ~proof=tail,
          ~proof_map,
          prev_next,
        );
      | None =>
        /* Synthesized trailing row — no backing proof leaf. Rendered
         * even when the goal is discharged, so the final expression
         * (`true` in a completed proof) gets a bottom row of its own.
         * Keep the previous row model (and with it the editor's
         * caret/selection) when it was already a missing row. */
        let prev_missing =
          switch (model.next_step) {
          | MissingStep(m, _) => m
          | NextStep(_)
          | Finished => MissingStep.Model.init
          };
        MissingStep(
          calculate_missing_step(
            ~settings,
            ~exp=next_exp,
            ~ctx,
            ~proof=None,
            prev_missing,
          ),
          Finished,
        );
      };
    {
      cached_proof_map_entry: Calc.Calculated(cached_proof_map_entry),
      pre_editors: Calc.Calculated(pre_editors),
      insert,
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
    let (proof_head, proof_tail) = split_proof(Calc.get_value(proof));
    let missing_step =
        (~proof_head: Proof.t, ~prev_tail: next_step, m: MissingStep.Model.t)
        : next_step => {
      let m =
        calculate_missing_step(
          ~settings,
          ~exp,
          ~ctx,
          ~proof=Some(proof_head),
          m,
        );
      /* Steps written after the hole still render: a hole is the
       * identity, so the tail sees the same expression. */
      let tail =
        switch (proof_tail) {
        | Some(tail_proof) =>
          let tail_proof =
            Calc.is_new(proof)
              ? Calc.NewValue(tail_proof) : Calc.OldValue(tail_proof);
          let prev =
            switch (prev_tail) {
            | Finished => init_step
            | ns => ns
            };
          calculate(
            ~settings,
            ~exp,
            ~ctx,
            ~ana,
            ~proof_info_map,
            ~proof=tail_proof,
            ~proof_map,
            prev,
          );
        | None => Finished
        };
      MissingStep(m, tail);
    };
    switch (model) {
    | Finished => Finished
    /* Promote-or-stay: once the proof here describes a real step, this row
     * becomes that step; while it is still a hole, it stays the row that
     * offers the step-picking UI. */
    | MissingStep(m, prev_tail) when is_hole_proof(proof_head) =>
      missing_step(~proof_head, ~prev_tail, m)
    | MissingStep(m, prev_tail) =>
      switch (kind_of_proof(proof_head)) {
      | Some(step_kind) => full_step(empty_step_model(step_kind))
      | None => missing_step(~proof_head, ~prev_tail, m)
      }
    /* The step this row rendered has been deleted back to a hole; keep
     * the rows after it alive through the swap. */
    | NextStep(sm) when is_hole_proof(proof_head) =>
      missing_step(
        ~proof_head,
        ~prev_tail=sm.next_step,
        MissingStep.Model.init,
      )
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
        switch (model.insert.editor) {
        | Calc.Calculated(editor) =>
          let+ ci =
            StepperEditor.Selection.get_cursor_info(
              ~inject=x => inject(EditorAction(x)),
              ~selection=a,
              editor,
            );
          EditorAction(ci);
        | Calc.Pending => Cursor.empty
        }
      | Next(a) =>
        let+ ci =
          get_cursor_info(
            ~inject=x => inject(NextStep(x): action),
            ~focus=a,
            model.next_step,
          );
        NextStep(ci);
      /* Step rows carry the insert-a-step-here overlay. */
      | MissingStepFocus(selection) =>
        let+ focus_info =
          MissingStep.Selection.get_cursor_info(
            ~inject=x => inject(MissingStepAction(x): step_action),
            ~selection,
            model.insert,
          );
        MissingStepAction(focus_info);
      }
    );
  }
  and get_cursor_info =
      (~inject, ~focus: step_focus, model: next_step)
      : Cursor.cursor(step_action) => {
    Cursor.(
      switch (model, focus) {
      | (Finished, _) => Cursor.empty
      | (MissingStep(m, _), MissingStepFocus(selection)) =>
        let+ focus_info =
          MissingStep.Selection.get_cursor_info(
            ~inject=x => inject(MissingStepAction(x): action),
            ~selection,
            m,
          );
        MissingStepAction(focus_info);
      | (MissingStep(m, _), Here(selection)) =>
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
      | (MissingStep(_, ns), Next(a)) =>
        let+ ci =
          get_cursor_info(
            ~inject=x => inject(NextStep(x): action),
            ~focus=a,
            ns,
          );
        NextStep(ci);
      | (MissingStep(_, _), _) => Cursor.empty
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

  /* Patch that inserts a new step BEFORE an existing one: acting on a
   * step row's expression means "step this expression here", so the new
   * step consumes this row's incoming goal and the existing chain shifts
   * down. `Seq(new, before)` nests left inside the enclosing `Seq`, but
   * the patch reflows through ExpToSegment, where `;` prints flat, so
   * the reparse renormalises the chain. */
  let insert_step_patch =
      (~before: Proof.t, proof_term: TermBase.Proof.term)
      : Haz3lcore.EditorTransform.patch =>
    Haz3lcore.EditorTransform.mk_proof_patch(
      ~target_id=Proof.rep_id(before),
      Proof.fresh(Seq(Proof.fresh(proof_term), before)),
    );

  /* Removing a row splices its proof subtree out of the enclosing Seq
   * (the `;` goes with it), or collapses to a hole when it is the whole
   * proof — the text ends up as if the step were never written, keeping
   * the source clean iff the stepper is clean. */
  let remove_step_patch = (p: Proof.t): Haz3lcore.EditorTransform.patch =>
    Haz3lcore.EditorTransform.mk_proof_remove_patch(
      ~target_id=Proof.rep_id(p),
    );

  /* Repair for a RuleDoesNotApply axiom step: same step, other
   * direction. Rewrites the step's own proof subtree in place. */
  let flip_direction_patch =
      (p: Proof.t): option(Haz3lcore.EditorTransform.patch) =>
    switch (p.term) {
    | AxiomStep({at_idx, at_exp, direction, equality}) =>
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
              }),
          },
        ),
      )
    | _ => None
    };

  /* Shared handler for the step-picking overlay events
   * (MissingStep.View.event): missing-step rows and step rows offer the
   * same picking UI; they differ only in what `emit` does with the
   * picked step (replace/extend a hole vs. insert before an existing
   * step). */
  let missing_step_signal =
      (
        ~take_focus: step_focus => Ui_effect.t(unit),
        ~hide_stepper: Ui_effect.t(unit),
        ~emit: TermBase.Proof.term => Ui_effect.t(unit),
        m: MissingStep.Model.t,
        event: MissingStep.View.event,
      )
      : Ui_effect.t(unit) => {
    let available_steps =
      switch (m.next_steps |> Calc.get_saved_opt) {
      | Some(AvailableSteps(steps)) => steps
      | Some(AutoStep(_))
      | None => []
      };
    let refls = Calc.get_saved([], m.refls);
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
        switch (ProofHacks.exp_idx(at_exp, full_exp)) {
        | Some(at_idx) =>
          emit(
            axiom_step_term(
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

  /* Every row in the chain — step or missing step — renders the same way:
   * the expression it starts from, then its justification.
   *
   * Design principle: the same syntax always takes up the same space —
   * status must never change the proof's layout (no scroll jumps). So
   * status renders only as (a) a class on the row driving an
   * absolutely-positioned indicator line along the row's right edge
   * (red = broken, tan = incomplete, dotted light red = blocked), and
   * (b) a popup anchored above the justification, hidden until the
   * justification (button-styled when there is something to show) is
   * clicked, holding the error message, any extra detail, and the
   * repair actions. Both are out of the document flow. */
  let step_row =
      (
        ~globals: Globals.t,
        ~status: step_status,
        /* Rows whose expression slot shows an error message (the step
         * above broke) drop the ≡ marker: the content isn't an
         * equivalent expression. Hidden, not removed, so the error text
         * stays column-aligned with the expressions around it. */
        ~show_equiv: bool,
        /* Deletes this row's own step outright (splice, see
         * `remove_step_patch`); rendered as a trash button riding the
         * transition band next to the justification. None for rows with
         * no backing step (the synthesized trailing picker). */
        ~delete: option(Ui_effect.t(unit)),
        /* Extra detail appended to the popup body (e.g. an induction
         * row's missing-pattern witness). */
        ~popup_extra: list(WebUtil.Node.t),
        /* Repair affordances rendered at the end of the popup
         * (e.g. "remove step", "try other direction"). */
        ~repair: list(WebUtil.Node.t),
        ~editor_view: WebUtil.Node.t,
        ~justification: WebUtil.Node.t,
        ~content: list(WebUtil.Node.t),
      ) => {
    let (row_classes, popup_body) =
      switch (status) {
      | StepOk => (["ok"], [])
      | StepPending => (["pending"], [])
      | StepIncomplete => (["incomplete"], [])
      | StepBroken(mark) => (
          ["broken"],
          ProofMarkView.message(~globals, mark) @ popup_extra @ repair,
        )
      | StepBlocked => (
          ["blocked"],
          [WebUtil.Node.text("Waiting on the step above.")]
          @ popup_extra
          @ repair,
        )
      };
    /* The popup lives inside the focusable wrapper so clicking a repair
     * button moves focus within the wrapper (`:focus-within` holds) and
     * the click lands before the popup hides. */
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
    /* The transition band: justification plus the row's delete button,
     * offset together onto the between-rows boundary (see stepper.css). */
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
    /* Rows with block content (induction cases, forall bodies) anchor
     * their transition band at the BOTTOM of the whole block — the
     * step's transition is goal → (content) → next row. */
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

  let rec view_step =
          (
            ~globals: Globals.t,
            ~take_focus: step_focus => Ui_effect.t(unit),
            ~inject: step_action => Ui_effect.t(unit),
            ~hide_stepper: Ui_effect.t(unit),
            ~focus: option(step_focus),
            ~is_toplevel: bool=false,
            /* Forwarded write channel for structural proof edits. */
            ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit)=
                                                                    _ =>
                                                                    Ui_effect.Ignore,
            /* Forwarded main-editor capability handle for step views
             * that render slices of the surrounding syntax as
             * sub-editors (see SubEditor.re / CodeEditable.Channel). */
            ~main_editor: option(CodeEditable.Channel.t)=None,
            ~proof_target: proof_target,
            /* When the step ABOVE this row is broken, its error mark: the
             * checker recovered by passing the goal through unchanged, so
             * instead of re-printing the duplicate expression this row's
             * expression slot prints the error. */
            ~prev_broken: option(ProofMark.t),
            model: step_model,
          ) => {
    let current_proof: option(Proof.t) = model.proof |> Calc.get_saved_opt;
    /* Removing a child rewrites this step's proof subtree to a hole. */
    let emit_remove_step = (): Ui_effect.t(unit) =>
      switch (current_proof) {
      | Some(p) => edit_syntax(remove_step_patch(p))
      | None => Ui_effect.Ignore
      };
    let entry_status =
      status_of_step_entry(
        Calc.get_saved(EntryNotFound, model.cached_proof_map_entry),
      );
    /* What the row below shows in its expression slot hinges on whether
     * this step broke (see ~prev_broken). */
    let broken_here =
      switch (entry_status) {
      | StepBroken(mark) => Some(mark)
      | _ => None
      };
    /* Step rows are history: the row the user acts on is the trailing
     * MissingStep, so with history off only that row is shown. */
    let current_step =
      if (!globals.settings.core.evaluation.stepper_history) {
        [];
      } else {
        let status = entry_status;
        /* A hole in the step's OWN arguments (an induction case pattern
         * or scrutinee, a forall binder, an axiom target) marks the row
         * incomplete: nothing else indicates it. A hole in a nested
         * sub-proof deliberately does NOT — every unfinished case chain
         * renders its own "…" continuation row, so an indicator here
         * would only restate what is already visible (and paint a line
         * down the entire induction block). */
        let status =
          switch (status, current_proof) {
          | (StepOk, Some(p)) when Proof.args_have_hole(p) => StepIncomplete
          | _ => status
          };
        let taken_steps =
          switch (model.step_kind) {
          | AxiomStep(m) => [m.at_exp |> Exp.rep_id]
          | _ => []
          };
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
            ~proof=current_proof,
            ~edit_syntax,
            ~main_editor,
            model.step_kind,
          );
        /* The expression slot: the error message when the step above
         * broke (its result is just the passed-through goal — printing
         * it again would be noise); otherwise the row's editable
         * expression with the full step-picking overlay. Picking a step
         * on a step row INSERTS it before this one, so it acts on
         * exactly the expression shown here. */
        let editor_view =
          switch (prev_broken, model.insert.editor |> Calc.saved_to_option) {
          | (Some(mark), _) =>
            WebUtil.(
              div_c(
                "step-error-inline",
                ProofMarkView.message(~globals, mark),
              )
            )
          | (None, None) =>
            WebUtil.(div_c("step-placeholder", [Node.text("—")]))
          | (None, Some(editor)) =>
            let emit = term =>
              switch (current_proof) {
              | Some(p) => edit_syntax(insert_step_patch(~before=p, term))
              | None => Ui_effect.Ignore
              };
            let signal =
              missing_step_signal(
                ~take_focus,
                ~hide_stepper,
                ~emit,
                model.insert,
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
              ~selected_id=Calc.get_saved(None, model.insert.selected_id),
              /* The action buttons are positioned over the selection they
               * act on, so they render as an overlay of this row's
               * editor. */
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
                  model.insert,
                ),
              /* History rows draw no passive hints (next-step
               * underlines, refl markers) — those belong to the picker
               * rows at the frontier. Acting on a history row goes
               * through selection: select a sub-term and the overlay
               * offers the actions. */
              StepperEditor.Model.{
                editor,
                taken_steps,
                next_steps: [],
                refls: [],
              },
            );
          };
        /* Repair affordances for a broken row, alongside its message:
         * removing the step is always possible; RuleDoesNotApply also
         * offers retrying the axiom in the other direction. */
        let repair =
          switch (status) {
          | StepBroken(mark) =>
            let flip =
              switch (mark, current_proof) {
              | (RuleDoesNotApply(_), Some(p)) =>
                switch (flip_direction_patch(p)) {
                | Some(patch) => [
                    Widgets.button(
                      ~clss=["proof-button"],
                      ~tooltip="Apply the equality in the other direction",
                      WebUtil.Node.text("Try other direction"),
                      _ =>
                      edit_syntax(patch)
                    ),
                  ]
                | None => []
                }
              | _ => []
              };
            flip
            @ [
              Widgets.button(
                ~clss=["proof-button"],
                ~tooltip="Replace this step with a hole",
                WebUtil.Node.text("Remove step"),
                _ =>
                emit_remove_step()
              ),
            ];
          | StepOk
          | StepIncomplete
          | StepBlocked
          | StepPending => []
          };
        /* Kind-specific detail for the popup: an inexhaustive induction
         * includes the statics checker's missing-pattern witness. */
        let popup_extra =
          switch (model.step_kind) {
          | InductionStep(m) =>
            switch (m.inexhaustive |> Calc.get_saved_opt |> Option.join) {
            | Some(example) =>
              ProofMarkView.inexhaustive_message(~globals, example)
            | None => []
            }
          | _ => []
          };
        step_row(
          ~globals,
          ~status,
          ~show_equiv=Option.is_none(prev_broken),
          ~delete=
            Option.map(
              p => edit_syntax(remove_step_patch(p)),
              current_proof,
            ),
          ~popup_extra,
          ~repair,
          ~editor_view,
          ~justification,
          ~content,
        );
      };
    let next_step_view =
      view_tail(
        ~globals,
        ~is_toplevel,
        ~take_focus,
        ~hide_stepper,
        ~inject,
        ~focus,
        ~edit_syntax,
        ~main_editor,
        ~fallback_target=proof_target,
        ~prev_broken=broken_here,
        model.next_step,
      );
    current_step @ next_step_view;
  }
  /* The rows below a given row: shared by step rows and missing-step
   * rows (both carry a chain tail now). ~take_focus/~inject/~focus are
   * the CALLER's — the Next wrapping happens here. */
  and view_tail =
      (
        ~globals: Globals.t,
        ~take_focus: step_focus => Ui_effect.t(unit),
        ~inject: step_action => Ui_effect.t(unit),
        ~hide_stepper: Ui_effect.t(unit),
        ~focus: option(step_focus),
        ~is_toplevel: bool,
        ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
        ~main_editor: option(CodeEditable.Channel.t),
        /* Where a synthesized (proof-less) missing row lands its step. */
        ~fallback_target: proof_target,
        ~prev_broken: option(ProofMark.t),
        ns: next_step,
      ) => {
    let next_focus =
      switch (focus) {
      | Some(Next(s)) => Some(s)
      | _ => None
      };
    switch (ns) {
    | Finished => []
    | MissingStep(m, tail) =>
      view_missing_step(
        ~globals,
        ~is_toplevel,
        ~take_focus=f => take_focus(Next(f)),
        ~hide_stepper,
        ~inject=x => inject(NextStep(x)),
        ~focus=next_focus,
        ~edit_syntax,
        ~main_editor,
        ~proof_target=
          switch (m.proof |> Calc.get_saved_opt |> Option.join) {
          | Some(p) => ReplaceProof(p)
          | None =>
            switch (fallback_target) {
            | ReplaceProof(leaf)
            | ExtendProof(leaf) => ExtendProof(leaf)
            }
          },
        /* A hole reached through a tail is always inside a Seq. */
        ~deletable=true,
        ~prev_broken,
        m,
        tail,
      )
    | NextStep(next_model) =>
      let next_target =
        switch (next_model.proof |> Calc.get_saved_opt) {
        | Some(p) => ReplaceProof(p)
        | None =>
          failwith("next_model.proof Pending after calculate — unreachable")
        };
      view_step(
        ~globals,
        ~is_toplevel,
        ~take_focus=f => take_focus(Next(f)),
        ~hide_stepper,
        ~inject=x => inject(NextStep(x)),
        ~focus=next_focus,
        ~edit_syntax,
        ~main_editor,
        ~proof_target=next_target,
        ~prev_broken,
        next_model,
      );
    };
  }
  and view_missing_step =
      (
        ~globals: Globals.t,
        ~take_focus: step_focus => Ui_effect.t(unit),
        ~inject: step_action => Ui_effect.t(unit),
        ~hide_stepper: Ui_effect.t(unit),
        ~focus: option(step_focus),
        ~is_toplevel: bool,
        ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
        ~main_editor: option(CodeEditable.Channel.t),
        ~proof_target: proof_target,
        /* False when this row's hole IS the entire proof (root row of a
         * bare-`?` proof): deleting it would be a no-op splice, so no
         * trash button is offered. */
        ~deletable: bool,
        /* See `view_step`: the error mark of a broken step above, shown
         * in place of the (passed-through, duplicate) expression. */
        ~prev_broken: option(ProofMark.t),
        m: MissingStep.Model.t,
        tail: next_step,
      ) => {
    let status =
      status_of_missing_step_proof(
        m.proof |> Calc.get_saved_opt |> Option.join,
      );
    /* Rows after the hole: the hole is the identity, so they continue on
     * the same goal. */
    let tail_view =
      view_tail(
        ~globals,
        ~is_toplevel,
        ~take_focus,
        ~hide_stepper,
        ~inject,
        ~focus,
        ~edit_syntax,
        ~main_editor,
        ~fallback_target=proof_target,
        ~prev_broken=
          switch (status) {
          | StepBroken(mark) => Some(mark)
          | _ => None
          },
        tail,
      );
    let justification =
      MissingStep.View.view_justification(
        ~globals,
        ~is_toplevel,
        ~hide_stepper,
        m,
      );
    /* A "?" justification marks a hole the user actually WROTE in the
     * middle of the proof — a step to come back for. Trailing rows
     * (synthesized, or a written `?` at the end of the proof) don't
     * carry it: the picker itself already says the proof continues
     * here. */
    let written_mid_chain_hole =
      Option.is_some(m.proof |> Calc.get_saved_opt |> Option.join)
      && (
        switch (tail) {
        | Finished => false
        | MissingStep(_, _)
        | NextStep(_) => true
        }
      );
    let justification =
      WebUtil.(
        div_c(
          "missing-step-justification",
          (
            written_mid_chain_hole
              ? [div_c("step-unknown", [Node.text("?")])] : []
          )
          @ [justification],
        )
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
    let signal = missing_step_signal(~take_focus, ~hide_stepper, ~emit, m);
    /* Deleting a hole row splices the written `?` (and its `;`) out of
     * the proof. Synthesized trailing rows have nothing to delete. */
    let delete =
      deletable
        ? m.proof
          |> Calc.get_saved_opt
          |> Option.join
          |> Option.map(p => edit_syntax(remove_step_patch(p)))
        : None;
    let row =
      switch (prev_broken, m.editor |> Calc.get_saved_opt) {
      | (Some(mark), _) =>
        /* The step above broke; this row shows its error instead of the
         * recovered goal. The justification (with its back arrow) still
         * renders, so the broken step is easy to delete. */
        step_row(
          ~globals,
          ~status,
          ~show_equiv=false,
          ~delete,
          ~popup_extra=[],
          ~repair=[],
          ~editor_view=
            WebUtil.(
              div_c(
                "step-error-inline",
                ProofMarkView.message(~globals, mark),
              )
            ),
          ~justification,
          ~content=[],
        )
      | (None, None) => [justification]
      | (None, Some(editor)) =>
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
              next_steps:
                List.map(EvaluatorStep.get_step_id, available_steps),
              refls: List.map(Exp.rep_id, refls),
            },
          );
        step_row(
          ~globals,
          ~status,
          ~show_equiv=true,
          ~delete,
          ~popup_extra=[],
          /* The picker already replaces the malformed sub-term when a step
           * is chosen, so no extra repair affordance is needed here. */
          ~repair=[],
          ~editor_view,
          ~justification,
          ~content=[],
        );
      };
    row @ tail_view;
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
      | MissingStep(m, tail) =>
        switch (m.proof |> Calc.get_saved_opt |> Option.join) {
        | Some(p) =>
          view_missing_step(
            ~globals,
            ~take_focus,
            ~hide_stepper,
            ~inject,
            ~focus,
            ~is_toplevel,
            ~edit_syntax,
            ~main_editor,
            ~proof_target=ReplaceProof(p),
            /* A root hole with no tail IS the whole proof — deleting it
             * would be a no-op. */
            ~deletable=
              switch (tail) {
              | Finished => false
              | MissingStep(_, _)
              | NextStep(_) => true
              },
            ~prev_broken=None,
            m,
            tail,
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
          ~edit_syntax,
          ~main_editor,
          ~proof_target,
          ~prev_broken=None,
          sm,
        );
      };
    WebUtil.[
      Node.div(~attrs=[Attr.classes(["stepper", "cell-result"])], body),
    ];
  };
};
