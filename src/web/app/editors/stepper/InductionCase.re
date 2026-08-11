open Util;
open Language;
open Haz3lcore;
open StepInterface;

[@deriving (show({with_path: false}), sexp, yojson)]
type model'('stepper) = {
  /* In proof scope the case's surface pattern lives in the main
   * editor's syntax (the `Induction(_, cases)` term); the sub-editor
   * view renders and edits that segment directly (see SubEditor.re).
   * This local model is DERIVED from the proof's pattern (rebuilt in
   * `calculate` whenever `pattern_src` changes) and is used only for
   * statics — elaboration, inductive hypotheses — plus as the editable
   * model for legacy cell-level steppers with no backing syntax. */
  pattern: CodeEditable.Model.t,
  // Calculated
  /* Last proof-side pattern the local model was rebuilt from. */
  pattern_src: Calc.saved(Pat.t),
  elab_pattern: Calc.saved(Pat.t),
  inner_exp: Calc.saved(Exp.t),
  step: 'stepper,
  last_exp: Calc.saved(Exp.t),
  inner_ctx: Calc.saved(SemanticCtx.t),
  hypotheses: Calc.saved(list((Binding.t, Exp.t))),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type action'('stepper) =
  | PatternUpdate(CodeEditable.Update.t)
  | StepUpdate('stepper);

[@deriving (show({with_path: false}), sexp, yojson)]
type focus'('stepper) =
  | Pattern(CodeSelectable.Selection.t)
  | Stepper('stepper);

/* A `model'('stepper)` with empty pattern/proof fields, parameterised
 * on the inner stepper model so callers outside the F functor (e.g.
 * `StepperBase.adapt_step_kind`) can synthesise a default case
 * without re-applying the functor. */
let init_with = (step: 'stepper): model'('stepper) => {
  pattern: CodeEditable.Model.mk(Editor.Model.mk(Zipper.init(), ~root=Exp)),
  pattern_src: Calc.Pending,
  elab_pattern: Calc.Pending,
  inner_exp: Calc.Pending,
  step,
  last_exp: Calc.Pending,
  inner_ctx: Calc.Pending,
  hypotheses: Calc.Pending,
};

module F = (Stepper: STEPPER) => {
  type model = model'(Stepper.model);
  type action = action'(Stepper.action);
  type focus = focus'(Stepper.focus);

  let init = init_with(Stepper.init);

  let update = (~settings: Settings.t, action: action, model: model) => {
    Updated.(
      switch (action) {
      | PatternUpdate(a) =>
        let* new_pattern =
          CodeEditable.Update.update(~settings, a, model.pattern);
        {
          ...model,
          pattern: new_pattern,
        };
      | StepUpdate(a) =>
        let* new_step = Stepper.update(~settings, a, model.step);
        {
          ...model,
          step: new_step,
        };
      }
    );
  };

  let can_undo = a =>
    switch (a) {
    | PatternUpdate(action) => CodeEditable.Update.can_undo(action)
    | StepUpdate(action) => Stepper.can_undo(action)
    };

  let calculate =
      (
        ~settings: Calc.t(CoreSettings.t),
        ~elab_scrut: Calc.t(Exp.t),
        ~scrut_co_ctx: Calc.t(CoCtx.t),
        ~scrut_ty: Calc.t(Typ.t),
        ~exp: Calc.t(Exp.t),
        ~ctx: Calc.t(SemanticCtx.t),
        ~info_map: Calc.t(Statics.Map.t),
        ~ana: Calc.t(Typ.t),
        ~proof: Calc.t(option(Proof.t)),
        ~proof_map: Calc.t(ProofMap.t),
        /* The case's surface pattern from the surrounding
         * `Induction(_, cases)` proof term (None outside proof scope).
         * The local `pattern` model is rebuilt from it on change: in
         * proof scope the syntax is the source of truth and the local
         * model is only a statics vehicle, never focused, so there is
         * no caret state to preserve. */
        ~pat: Calc.t(option(Pat.t)),
        model: model,
      ) => {
    let (pattern, pattern_src) =
      switch (Calc.get_value(pat)) {
      | Some(p) =>
        let src = Calc.set(~eq=Pat.fast_equal, p, model.pattern_src);
        let pattern =
          switch (src) {
          | NewValue(p) =>
            CodeEditable.Model.mk(
              Editor.Model.mk(
                Zipper.unzip(
                  ExpToSegment.exp_to_segment(
                    ~settings=ExpToSegment.Settings.editable(~inline=true),
                    ProofHacks.pat_to_exp(p),
                  ),
                ),
                ~root=Exp,
              ),
            )
          | OldValue(_) => model.pattern
          };
        (pattern, src |> Calc.save);
      | None => (model.pattern, model.pattern_src)
      };
    let pattern =
      CodeEditable.Update.calculate(
        ~settings=Calc.get_value(settings),
        ~dynamics=Dynamics.Map.empty,
        ~is_edited=true, // This editor technically edits Exps, but we want a Pat, so we put it in a function to emulate that.
        ~stitch=
          x =>
            x
            |> ProofHacks.exp_to_pat
            |> ProofHacks.add_wrapping_function(
                 ~typ=scrut_ty |> Calc.get_value,
               ),
        ~is_dynamic_term=true,
        pattern,
      );

    let elab_pattern =
      Calc.set(
        ~eq=Pat.fast_equal,
        CodeEditable.Model.get_statics(pattern).elaborated
        |> ProofHacks.remove_wrapping_function,
        model.elab_pattern,
      );

    let (inner_ctx, inner_exp, hypotheses) =
      (model.inner_ctx, model.inner_exp, model.hypotheses)
      |> Calc.saved_3
      |> {
        open Calc.Syntax;
        let.calc elab_pattern = elab_pattern
        and.calc elab_scrut = elab_scrut
        and.calc scrut_co_ctx = scrut_co_ctx
        and.calc scrut_ty = scrut_ty
        and.calc sem_ctx = ctx
        and.calc info_map = info_map
        and.calc exp = exp;

        // 1. Find what variables the pattern adds to the scope, and
        // add them to the env and ctx.
        let added_variables =
          elab_pattern |> Pat.bindings |> Binding.variable_names;
        let sem_ctx =
          SemanticCtx.add_from_pattern(sem_ctx, elab_pattern, scrut_ty);

        // 2. Work out what the inner exp would be
        // Note: this is an option in case some capture nonsense happens.
        let inner_exp =
          ProofHacks.replace_exp(
            info_map,
            elab_scrut,
            scrut_co_ctx,
            elab_pattern |> ProofHacks.pat_to_exp,
            elab_pattern |> Pat.bindings |> CoCtx.of_bindings,
            added_variables,
            exp,
          );

        // 3. Create the case_equality assertion, and add to env and ctx if appropriate
        // Note: if the LHS of case_eq is in any way captured by the added variables, then we cannot use it.
        let is_case_eq_captured =
          CoCtx.has_any(scrut_co_ctx, added_variables);
        let case_eq =
          is_case_eq_captured
            ? None
            : Some(
                BinOp(
                  Poly(Equals),
                  elab_scrut,
                  elab_pattern |> ProofHacks.pat_to_exp,
                )
                |> Exp.fresh
                |> Substitution.in_exp(SemanticCtx.get_env(sem_ctx)),
              );
        let (sem_ctx, case_eq_name) =
          switch (case_eq) {
          | Some(case_eq) =>
            SemanticCtx.add_hypothesis(sem_ctx, "case_eq", case_eq)
            |> PairUtil.map_snd(Option.some)
          | None => (sem_ctx, None)
          };

        // 4. Find the inductive hypotheses, and add to env and ctx
        // Note: we do not add any IHs that are captured by the added variables. (This should happen iff the inner exp is captured)
        let inductive_hypotheses =
          ProofHacks.get_inductive_hypotheses(
            CodeEditable.Model.get_statics(pattern).info_map,
            scrut_ty,
            elab_pattern,
          )
          |> List.filter_map(h =>
               ProofHacks.replace_exp(
                 info_map,
                 elab_scrut,
                 scrut_co_ctx,
                 h |> ProofHacks.pat_to_exp,
                 h |> Pat.bindings |> CoCtx.of_bindings,
                 added_variables,
                 exp,
               )
             );
        let (sem_ctx, ihs) =
          List.fold_left(
            ((acc, ihs), h) =>
              SemanticCtx.add_hypothesis(acc, "ih", h)
              |> PairUtil.map_snd(x => [(x, h), ...ihs]),
            (sem_ctx, []),
            inductive_hypotheses,
          );

        let inner_exp =
          inner_exp |> Option.value(~default=Exp.fresh(EmptyHole));

        let case_eq_h =
          switch (case_eq, case_eq_name) {
          | (Some(e), Some(n)) => [(n, e)]
          | _ => []
          };
        let hypotheses = case_eq_h @ ihs;

        (sem_ctx, inner_exp, hypotheses);
      }
      |> Calc.to_3;

    /* `~proof` here is the body proof for *this* case (extracted by
     * `InductionStep.calculate` from `Induction(_, proof_cases)`).
     * Stepping inside the case therefore reads / patches the right
     * `body_i` sub-tree rather than the outer `Induction` node. */
    let (stepper, last_exp, validity) =
      Stepper.calculate(
        ~settings, // TODO: this is a little ugly
        ~ctx=inner_ctx,
        ~exp=inner_exp,
        ~ana,
        ~proof,
        ~proof_map,
        model.step,
      );

    (
      {
        pattern,
        pattern_src,
        elab_pattern: elab_pattern |> Calc.save,
        inner_exp: inner_exp |> Calc.save,
        step: stepper,
        last_exp: last_exp |> Calc.save,
        inner_ctx: inner_ctx |> Calc.save,
        hypotheses: hypotheses |> Calc.save,
      },
      validity,
    );
  };

  let get_cursor_info = (~inject, ~focus: focus, model: model) => {
    Cursor.(
      switch (focus) {
      | Pattern(a) =>
        let+ ci =
          CodeEditable.Selection.get_cursor_info(
            ~inject=a => inject(PatternUpdate(a)),
            ~selection=a,
            model.pattern,
          );
        PatternUpdate(ci);
      | Stepper(a) =>
        let+ ci =
          Stepper.get_cursor_info(
            ~inject=a => inject(StepUpdate(a)),
            ~focus=a,
            model.step,
          );
        StepUpdate(ci);
      }
    );
  };

  let view =
      (
        ~globals: Globals.t,
        ~focus: option(focus),
        ~inject: action => Ui_effect.t(unit),
        ~take_focus: focus => Ui_effect.t(unit),
        ~hide_stepper: Ui_effect.t(unit),
        ~remove_case: Ui_effect.t(unit),
        /* Syntax-edit channel, forwarded into this case's inner stepper
         * so step creation inside the case patches the proof syntax. */
        ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit),
        /* Main-editor capability handle + a structural reference to
         * this case's pattern slot (see SubEditor.Target): when both
         * resolve to a backing segment, the pattern is rendered as a
         * sub-editor over the main editor (single source of truth —
         * edits in either view are the same edit). */
        ~main_editor: option(CodeEditable.Channel.t),
        ~slot: option(SubEditor.Target.t),
        model: model,
      ) => {
    let remove_case_button =
      Widgets.button(
        Icons.trash,
        _ => remove_case,
        ~tooltip="Remove case",
        ~clss=["subtle-button"],
      );
    let pattern_focus: option(unit) =
      switch (focus) {
      | Some(Pattern ()) => Some()
      | _ => None
      };
    /* Local-model rendering, used outside proof scope (editable; the
     * legacy cell-level stepper has no backing syntax) and as a
     * read-only stand-in while the backing segment is momentarily
     * unresolvable (statics lagging a structural rewrite). */
    let local_pattern_editor = (~read_only: bool) =>
      CodeEditable.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive => take_focus(Pattern()),
        ~edit_mode=
          read_only
            ? EditMode.ReadOnly
            : EditMode.Editable({
                inject: x => inject(PatternUpdate(x)),
                escape: _ => Ui_effect.Ignore,
                take_focus: _ => Ui_effect.Ignore,
                focus: pattern_focus,
              }),
        ~dynamics=Dynamics.Map.empty,
        model.pattern,
      );
    let pattern_editor =
      switch (main_editor) {
      | Some(channel) =>
        switch (
          Option.bind(slot, target =>
            SubEditor.mk(channel.model.editor, ~target)
          )
        ) {
        | Some(sub) =>
          CodeEditable.View.view(
            ~globals,
            ~signal=
              fun
              | MakeActive => take_focus(Pattern()),
            ~edit_mode=
              EditMode.Editable({
                /* Perform actions are rewritten to PerformConfined (and
                 * TAB swallowed) inside CodeEditable.View.view when a
                 * sub-editor is given. */
                inject: channel.inject,
                escape: _ => Ui_effect.Ignore,
                take_focus: _ => Ui_effect.Ignore,
                focus: pattern_focus,
              }),
            ~dynamics=Dynamics.Map.empty,
            ~sub_editor=Some(sub),
            channel.model,
          )
        | None => local_pattern_editor(~read_only=true)
        }
      | None => local_pattern_editor(~read_only=false)
      };
    let pattern_editor =
      WebUtil.div_c("inline-editor-wrapper", [pattern_editor]);
    module StepperTargetBox = StepperTargetBox.F(Stepper);
    let stepper_view =
      StepperTargetBox.target_box(
        ~globals,
        ~take_focus=s => take_focus(Stepper(s)),
        ~hide_stepper,
        ~inject=x => inject(StepUpdate(x)),
        ~focus=
          switch (focus) {
          | Some(Stepper(f)) => Some(f)
          | _ => None
          },
        ~is_toplevel=false,
        ~edit_syntax,
        ~main_editor,
        model.step,
        Exp.fresh(Atom(Bool(true))),
        model.last_exp |> Calc.get_saved_exc(~print="last_exp not calculated"),
      );
    WebUtil.div_c(
      "induction-case",
      [
        WebUtil.div_c(
          "induction-case-header",
          [
            remove_case_button,
            WebUtil.Node.text("Case "),
            pattern_editor,
            WebUtil.Node.text(" : "),
          ],
        ),
        WebUtil.div_c(
          "induction-case-hypotheses",
          List.filter_map(
            fun
            | (Binding.{name: _, id: _}, exp) => {
                let rule = ProofRule.exp_to_rule(exp);
                let conclusion = ProofRule.conclusion_exp(rule);
                let code =
                  CodeViewable.view_any(
                    ~globals,
                    ~settings=
                      Haz3lcore.ExpToSegment.Settings.of_core(
                        ~inline=true,
                        ~fold_fn_bodies=`Text,
                        globals.settings.core,
                      ),
                    Exp(conclusion),
                  );
                Some(
                  WebUtil.div_c(
                    "induction-case-hypothesis",
                    [WebUtil.Node.text("assume "), code],
                  ),
                );
              },
            model.hypotheses
            |> Calc.get_saved_exc(~print="hypotheses not calculated"),
          ),
        ),
      ]
      @ stepper_view,
    );
  };
};
