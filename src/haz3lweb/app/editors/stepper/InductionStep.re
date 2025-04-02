open Util;
open Haz3lcore;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('step) = {
    // Updated
    scrut: CodeEditable.Model.t,
    cases: list(InductionCase.Model.t('step)),
    // Calculated
    elab_scrut: Calc.saved(Exp.t),
    scrut_ty: Calc.saved(Typ.t),
    result: Calc.saved(Exp.t),
    result_state: Calc.saved(EvaluatorState.t),
    join_exp: Calc.saved(Exp.t),
  };

  let init = {
    scrut: CodeEditable.Model.mk(Editor.Model.mk(Zipper.init())),
    cases: [],
    elab_scrut: Calc.Pending,
    scrut_ty: Calc.Pending,
    result: Calc.Pending,
    result_state: Calc.Pending,
    join_exp: Calc.Pending,
  };
};

module Update = {
  open Updated;
  // open Calc.Syntax;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('step) =
    | ScrutUpdate(CodeEditable.Update.t)
    | CaeUpdate(int, InductionCase.Update.t('step))
    | AddCase
    | RemoveCase(int);

  let update =
      (
        type step,
        type step_model,
        ~init_step,
        ~update_step,
        ~settings,
        action: t(step),
        model: Model.t(step_model),
      )
      : Updated.t(Model.t(step_model)) => {
    switch (action) {
    | ScrutUpdate(a) =>
      let* new_scrut = CodeEditable.Update.update(~settings, a, model.scrut);
      {
        ...model,
        scrut: new_scrut,
      };
    | CaeUpdate(i, a) =>
      switch (List.nth_opt(model.cases, i)) {
      | Some(case) =>
        let* new_case =
          InductionCase.Update.update(~settings, ~update_step, a, case);
        {
          ...model,
          cases: ListUtil.put_nth(i, new_case, model.cases),
        };
      | None => model |> return_quiet
      }
    | AddCase =>
      let new_case = InductionCase.Model.init(init_step);
      {
        ...model,
        cases: model.cases @ [new_case],
      }
      |> return;
    | RemoveCase(i) =>
      switch (ListUtil.remove_nth(i, model.cases)) {
      | Some(new_cases) =>
        {
          ...model,
          cases: new_cases,
        }
        |> return
      | None => model |> return_quiet
      }
    };
  };

  let calculate =
      (
        type step_model,
        ~calculate_step,
        ~settings,
        ctx,
        exp,
        state,
        model,
        hidden,
      ) => {
    let {
      scrut,
      cases,
      elab_scrut,
      scrut_ty,
      result: _,
      result_state: _,
      join_exp,
    }:
      Model.t(step_model) = model;
    let scrut =
      CodeEditable.Update.calculate(
        ~settings=Calc.get_value(settings),
        ~ctx=Calc.get_value(ctx),
        ~dynamics=Dynamics.Map.empty,
        ~is_edited=true,
        ~stitch=x => x,
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
    let cases =
      List.map(
        InductionCase.Update.calculate(
          ~calculate_step,
          ~settings,
          ~scrut_ty,
          ~elab_scrut,
          ctx,
          exp,
          state,
        ),
        cases,
      );

    let new_join_exp =
      List.fold_left(
        (acc, case: InductionCase.Model.t(step_model)) =>
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

    let result = exp |> Calc.save;
    let result_state = state |> Calc.save;

    (
      Model.{
        scrut,
        cases,
        elab_scrut: elab_scrut |> Calc.save,
        scrut_ty: scrut_ty |> Calc.save,
        result,
        result_state,
        join_exp: join_exp |> Calc.save,
      },
      hidden |> Calc.set(false),
      Some((join_exp, state)),
    );
  };
};

module Selection = {
  open Cursor;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('step) =
    | Scrut(CodeEditable.Selection.t)
    | Case(int, InductionCase.Selection.t('step));

  let get_cursor_info =
      (
        type step,
        type step_model,
        ~get_cursor_info_step,
        ~selection: t(step),
        ~model: Model.t(step_model),
      ) =>
    switch (selection) {
    | Scrut(a) =>
      let+ ci =
        CodeEditable.Selection.get_cursor_info(~selection=a, model.scrut);
      Update.ScrutUpdate(ci);
    | Case(i, a) =>
      switch (List.nth_opt(model.cases, i)) {
      | Some(case) =>
        let+ ci =
          InductionCase.Selection.get_cursor_info(
            ~selection=a,
            ~model=case,
            ~get_cursor_info_step,
          );
        Update.CaeUpdate(i, ci);
      | None => Cursor.empty
      }
    };

  let handle_key_event =
      (
        type step,
        type step_model,
        ~handle_key_event_step,
        ~selection: t(step),
        ~event,
        model: Model.t(step_model),
      ) =>
    switch (selection) {
    | Scrut(a) =>
      let editor = model.scrut;
      CodeEditable.Selection.handle_key_event(~selection=a, editor, event)
      |> Option.map(x => Update.ScrutUpdate(x));
    | Case(i, a) =>
      switch (List.nth_opt(model.cases, i)) {
      | Some(case) =>
        InductionCase.Selection.handle_key_event(
          ~handle_key_event_step,
          ~selection=a,
          ~event,
          case,
        )
        |> Option.map(x => Update.CaeUpdate(i, x))
      | None => None
      }
    };
};

module View = {
  open Web;

  type event('step_focus) =
    | MakeActive(Selection.t('step_focus))
    | HideStepper;

  let view =
      (
        type step_model,
        type step_update,
        type step_focus,
        ~view_stepper',
        ~globals: Globals.t,
        ~signal: event(step_focus) => Ui_effect.t(unit),
        ~inject: Update.t(step_update) => Ui_effect.t(unit),
        ~selected: option(Selection.t(step_focus)),
        model: Model.t(step_model),
      ) => {
    let scrut_editor =
      CodeEditable.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive => signal(MakeActive(Scrut())),
        ~inject=x => inject(ScrutUpdate(x)),
        ~selected=
          switch (selected) {
          | Some(Scrut(_)) => true
          | Some(_)
          | None => false
          },
        model.scrut,
      );

    let add_case_button = Widgets.button(Icons.star, _ => inject(AddCase));

    let cases =
      List.mapi(
        (i, case) =>
          InductionCase.View.view(
            ~globals,
            ~view_stepper',
            ~signal=
              fun
              | MakeActive(x) => signal(MakeActive(Case(i, x)))
              | HideStepper => signal(HideStepper),
            ~inject=x => inject(CaeUpdate(i, x)),
            ~selected=
              switch (selected) {
              | Some(Case(j, s)) when i == j => Some(s)
              | Some(_)
              | None => None
              },
            case,
          ),
        model.cases,
      );

    [
      Web.div_c(
        "induction-scrut",
        [
          Node.text("Cases on: "),
          Web.div_c("inline-editor-wrapper", [scrut_editor]),
        ],
      ),
    ]
    @ cases
    @ [add_case_button];
  };
};
