open Util;
open Language;
open Haz3lcore;
open StepInterface;
open Calc.Syntax;

/* Types are defined outside the functor to make it
   easier to use them in other files. */

[@deriving (show({with_path: false}), sexp, yojson)]
type model'('stepper) = InductionStep.model'('stepper);

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent'('step) = InductionStep.persistent'('step);

[@deriving (show({with_path: false}), sexp, yojson)]
type action'('step) = InductionStep.action'('step);

[@deriving (show({with_path: false}), sexp, yojson)]
type focus'('step) = InductionStep.focus'('step);

let init = InductionStep.init;

/* The methods in this file, like the other step files, are
   parameterized by a Stepper module that implements the
   stepper interface. This allows us to use steppers inside
   steps inside steppers. The lines below can be copied as
   boilerplate to other steps.*/
module F =
       (Stepper: STEPPER)

         : (
           STEP with
             type model = model'(Stepper.model) and
             type persistent = persistent'(Stepper.persistent) and
             type action = action'(Stepper.action) and
             type focus = focus'(Stepper.focus)
       ) => {
  module InductionCase = CasesCase.F(Stepper);
  include InductionStep.F(Stepper);

  let calculate =
      (
        ~settings: Calc.t(CoreSettings.t),
        ~hidden: Calc.saved(bool),
        ~exp: Calc.t(Exp.t),
        ~ctx: Calc.t(Ctx.t),
        ~env: Calc.t(ClosureEnvironment.t),
        ~state: Calc.t(EvaluatorState.t),
        ~editor as _,
        ~ana: Calc.t(Typ.t),
        model: model,
      ) => {
    let {
      scrut,
      cases,
      elab_scrut,
      scrut_ty,
      scrut_co_ctx,
      result: _,
      result_state: _,
      join_exp,
      is_exhaustive,
      validity,
    }: model = model;
    let scrut =
      CodeEditable.Update.calculate(
        ~settings=Calc.get_value(settings),
        ~ctx=Calc.get_value(ctx),
        ~dynamics=Dynamics.Map.empty,
        ~is_edited=true,
        ~stitch=x => x,
        ~is_dynamic_term=true,
        scrut,
      );
    let elab_scrut =
      Calc.set(
        ~eq=Exp.fast_equal,
        CodeEditable.Model.get_statics(scrut).elaborated,
        elab_scrut,
      );
    let scrut_ty = {
      let self_ty =
        switch (
          Id.Map.find_opt(
            Exp.rep_id(CodeEditable.Model.get_statics(scrut).elaborated),
            CodeEditable.Model.get_statics(scrut).info_map,
          )
        ) {
        | Some(Info.InfoExp({ty, _})) => ty
        | _ => raise(Elaborator.MissingTypeInfo)
        };
      Calc.set(~eq=Typ.fast_equal, self_ty, scrut_ty);
    };
    let scrut_co_ctx = {
      let self_co_ctx =
        switch (
          Id.Map.find_opt(
            Exp.rep_id(CodeEditable.Model.get_statics(scrut).elaborated),
            CodeEditable.Model.get_statics(scrut).info_map,
          )
        ) {
        | Some(Info.InfoExp({co_ctx, _})) => co_ctx
        | _ => CoCtx.empty
        };
      Calc.set(self_co_ctx, scrut_co_ctx);
    };
    let (cases, constraints, validities) =
      List.map(
        InductionCase.calculate(
          ~settings,
          ~scrut_ty,
          ~elab_scrut,
          ~scrut_co_ctx,
          ~ctx,
          ~env,
          ~exp,
          ~state,
          ~ana,
        ),
        cases,
      )
      |> ListUtil.unzip3;

    let new_join_exp =
      List.fold_left(
        (acc, case: InductionCase.model) =>
          switch (acc, case.last_exp) {
          | (None, Calc.Pending) => None
          | (None, Calc.Calculated(last_exp)) => Some(last_exp)
          | (Some(acc), Calc.Pending) => Some(acc)
          | (Some(acc), Calc.Calculated(last_exp))
              when Exp.fast_equal(acc, last_exp) =>
            Some(acc)
          | (Some(_), Calc.Calculated(_)) => Some(Exp.fresh(EmptyHole))
          },
        None,
        cases,
      );
    let join_exp =
      Calc.set(
        ~eq=Exp.fast_equal,
        new_join_exp |> Option.value(~default=Exp.fresh(EmptyHole)),
        join_exp,
      );

    let is_exhaustive =
      is_exhaustive
      |> {
        let.calc constraints = Calc.combine_list(constraints)
        and.calc ctx = ctx
        and.calc scrut_ty = scrut_ty;
        let constraints = List.filter_map(Fun.id, constraints);
        Coverage.check(constraints, Typ.normalize(ctx, scrut_ty)).
          is_exhaustive;
      };

    let validity =
      validity
      |> {
        let.calc validities = Calc.combine_list(validities)
        and.calc is_exhaustive = is_exhaustive;
        List.fold_left(
          (v1, v2) =>
            switch (v1, v2) {
            | (Some(true), Some(true)) => Some(true)
            | (Some(false), Some(false)) => Some(false)
            | (_, _) => None
            },
          is_exhaustive ? Option.join(ListUtil.hd_opt(validities)) : None,
          validities,
        );
      };

    let result = exp |> Calc.save;
    let result_state = state |> Calc.save;

    Some((
      InductionStep.{
        scrut,
        cases,
        elab_scrut: elab_scrut |> Calc.save,
        scrut_ty: scrut_ty |> Calc.save,
        scrut_co_ctx: scrut_co_ctx |> Calc.save,
        result,
        result_state,
        join_exp: join_exp |> Calc.save,
        is_exhaustive: is_exhaustive |> Calc.save,
        validity: validity |> Calc.save,
      },
      hidden |> Calc.set(false),
      Some((join_exp, state)),
      validity,
    ));
  };

  let view_justification =
      (
        ~globals as _: Globals.t,
        ~focus as _: option(focus),
        ~inject as _: action => Ui_effect.t(unit),
        ~take_focus as _: focus => Ui_effect.t(unit),
        ~hide_stepper as _: Ui_effect.t(unit),
        ~undo as _: option(Ui_effect.t(unit)),
        ~is_toplevel as _: bool,
        _: model,
      ) =>
    WebUtil.Node.text("Case Analysis");

  let view_content =
      (
        ~globals: Globals.t,
        ~focus: option(focus),
        ~inject: action => Ui_effect.t(unit),
        ~take_focus: focus => Ui_effect.t(unit),
        ~hide_stepper: Ui_effect.t(unit),
        ~undo as _: option(Ui_effect.t(unit)),
        ~is_toplevel as _: bool,
        model: model,
      ) => {
    let scrut_editor =
      CodeEditable.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive => take_focus(Scrut()),
        ~inject=x => inject(ScrutUpdate(x)),
        ~selected=
          switch (focus) {
          | Some(Scrut(_)) => true
          | Some(_)
          | None => false
          },
        model.scrut,
      );

    let add_case_button =
      Widgets.button(
        WebUtil.Node.text("Case ..."),
        ~tooltip="Add case",
        ~clss=["subtle-button", "add-case-button"],
        _ =>
        inject(AddCase)
      );

    let cases =
      List.mapi(
        (i, case) =>
          InductionCase.view(
            ~globals,
            ~inject=x => inject(CaseUpdate(i, x)),
            ~take_focus=x => take_focus(Case(i, x)),
            ~remove_case=inject(RemoveCase(i)),
            ~hide_stepper,
            ~focus=
              switch (focus) {
              | Some(Case(j, s)) when i == j => Some(s)
              | Some(_)
              | None => None
              },
            case,
          ),
        model.cases,
      );

    [
      WebUtil.div_c(
        "induction-scrut",
        [
          WebUtil.Node.text("Cases on: "),
          WebUtil.div_c("inline-editor-wrapper", [scrut_editor]),
        ],
      ),
    ]
    @ cases
    @ [add_case_button];
  };
};
