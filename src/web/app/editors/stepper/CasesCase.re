open Util;
open Language;
open StepInterface;

[@deriving (show({with_path: false}), sexp, yojson)]
type model'('stepper) = InductionCase.model'('stepper);

[@deriving (show({with_path: false}), sexp, yojson)]
type action'('stepper) = InductionCase.action'('stepper);

[@deriving (show({with_path: false}), sexp, yojson)]
type focus'('stepper) = InductionCase.focus'('stepper);

module F = (Stepper: STEPPER) => {
  include InductionCase.F(Stepper);

  let calculate =
      (
        ~settings: Calc.t(CoreSettings.t),
        ~elab_scrut: Calc.t(Exp.t),
        ~scrut_ty: Calc.t(Typ.t),
        ~scrut_co_ctx: Calc.t(CoCtx.t),
        ~exp: Calc.t(Exp.t),
        ~ctx: Calc.t(Ctx.t),
        ~env: Calc.t(ClosureEnvironment.t),
        ~state: Calc.t(EvaluatorState.t),
        ~ana: Calc.t(Typ.t),
        model: model,
      ) => {
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
        model.pattern,
      );
    let elab_pattern =
      Calc.set(
        ~eq=Pat.fast_equal,
        CodeEditable.Model.get_statics(pattern).elaborated
        |> ProofHacks.remove_wrapping_function,
        model.elab_pattern,
      );
    let inner_exp =
      model.inner_exp
      |> {
        open Calc.Syntax;
        let.calc elab_pattern = elab_pattern
        and.calc elab_scrut = elab_scrut
        and.calc scrut_co_ctx = scrut_co_ctx
        and.calc exp = exp;
        ProofHacks.replace_exp(
          elab_scrut,
          scrut_co_ctx,
          elab_pattern |> ProofHacks.pat_to_exp,
          elab_pattern |> Pat.bindings |> CoCtx.of_bindings,
          exp,
        );
      };
    let hypo_points =
      model.hypo_points
      |> {
        open Calc.Syntax;
        let.calc elab_pattern = elab_pattern
        and.calc scrut_ty = scrut_ty;
        ProofHacks.get_inductive_hypotheses(
          CodeEditable.Model.get_statics(pattern).info_map,
          scrut_ty,
          elab_pattern,
        )
        |> List.map(v => Pat.fresh(Var(v)));
      };
    let inner_ctx =
      model.inner_ctx
      |> {
        open Calc.Syntax;
        let.calc elab_pattern = elab_pattern
        and.calc scrut_ty = scrut_ty
        and.calc ctx = ctx;
        ProofHacks.dhpat_extend_ctx(elab_pattern, scrut_ty, ctx)
        |> Option.value(~default=ctx);
      };
    let (stepper, last_exp) =
      Stepper.calculate(
        ~settings, // TODO: this is a little ugly
        ~ctx=inner_ctx,
        ~exp=inner_exp,
        ~env,
        ~state,
        ~ana,
        model.step,
      );
    (
      InductionCase.{
        pattern,
        elab_pattern: elab_pattern |> Calc.save,
        inner_exp: inner_exp |> Calc.save,
        hypo_points: hypo_points |> Calc.save,
        step: stepper,
        last_exp: last_exp |> Calc.save,
        added_ctx: Calc.Pending,
        inner_ctx: inner_ctx |> Calc.save,
        constraint_: Calc.Pending,
      },
      elab_pattern,
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
        model: model,
      ) => {
    let remove_case_button =
      Widgets.button(
        Icons.trash,
        _ => remove_case,
        ~tooltip="Remove case",
        ~clss=["subtle-button"],
      );
    let pattern_editor =
      CodeEditable.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive => take_focus(Pattern()),
        ~inject=x => inject(PatternUpdate(x)),
        ~selected=
          switch (focus) {
          | Some(Pattern ()) => true
          | _ => false
          },
        model.pattern,
      );
    let pattern_editor =
      WebUtil.div_c("inline-editor-wrapper", [pattern_editor]);
    let stepper_view =
      Stepper.view(
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
        model.step,
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
      ]
      @ stepper_view,
    );
  };
};
