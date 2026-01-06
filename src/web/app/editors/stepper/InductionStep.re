open Util;
open Language;
open Haz3lcore;
open StepInterface;

/* Types are defined outside the functor to make it
   easier to use them in other files. */

[@deriving (show({with_path: false}), sexp, yojson)]
type model'('stepper) = {
  // Updated
  scrut: EditorManager.Model.t,
  cases: list(InductionCase.model'('stepper)),
  // Calculated
  elab_scrut: Calc.saved(Exp.t),
  scrut_ty: Calc.saved(Typ.t),
  scrut_co_ctx: Calc.saved(CoCtx.t),
  result: Calc.saved(Exp.t),
  result_state: Calc.saved(EvaluatorState.t),
  join_exp: Calc.saved(Exp.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent'('stepper) = {
  scrut: EditorManager.Model.persistent,
  cases: list(InductionCase.persistent'('stepper)),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type action'('step) =
  | ScrutUpdate(EditorManager.Update.t)
  | CaseUpdate(int, InductionCase.action'('step))
  | AddCase
  | RemoveCase(int);

[@deriving (show({with_path: false}), sexp, yojson)]
type focus'('step) =
  | Scrut(Editor.Focus.t)
  | Case(int, InductionCase.focus'('step));

let init = (~exp: option(Exp.t)=?, ()) => {
  let scrut =
    switch (exp) {
    | Some(e) =>
      EditorManager.Model.of_editor(
        Editor.of_zipper(
          Zipper.unzip(
            ExpToSegment.exp_to_segment(
              ~settings=ExpToSegment.Settings.editable(~inline=true),
              e,
            ),
          ),
        ),
      )
    | None => EditorManager.Model.of_editor(Editor.of_zipper(Zipper.init()))
    };
  {
    scrut,
    cases: [],
    elab_scrut: Calc.Pending,
    scrut_ty: Calc.Pending,
    scrut_co_ctx: Calc.Pending,
    result: Calc.Pending,
    result_state: Calc.Pending,
    join_exp: Calc.Pending,
  };
};

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
  module InductionCase = InductionCase.F(Stepper);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = model'(Stepper.model);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = persistent'(Stepper.persistent);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = action'(Stepper.action);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus = focus'(Stepper.focus);

  let persist = (model: model): persistent => {
    {
      scrut: model.scrut.editor |> Editor.get_z |> PersistentZipper.persist,
      cases: List.map(InductionCase.persist, model.cases),
    };
  };

  let unpersist = (p: persistent): model => {
    {
      scrut:
        p.scrut
        |> PersistentZipper.unpersist
        |> Editor.of_zipper
        |> EditorManager.Model.of_editor,
      cases: List.map(InductionCase.unpersist, p.cases),
      elab_scrut: Calc.Pending,
      scrut_ty: Calc.Pending,
      scrut_co_ctx: Calc.Pending,
      result: Calc.Pending,
      result_state: Calc.Pending,
      join_exp: Calc.Pending,
    };
  };

  let update = (~globals: Globals.t, action: action, model: model) => {
    let common: Common.global = Globals.to_common_global(globals);
    Updated.(
      switch (action) {
      | ScrutUpdate(a) =>
        let* new_scrut =
          EditorManager.Update.update(
            ~common,
            ~dynamics=Dynamics.Map.empty,
            a,
            model.scrut,
          );
        {
          ...model,
          scrut: new_scrut,
        };
      | CaseUpdate(i, a) =>
        switch (List.nth_opt(model.cases, i)) {
        | Some(case) =>
          let* new_case = InductionCase.update(~globals, a, case);
          {
            ...model,
            cases: ListUtil.put_nth(i, new_case, model.cases),
          };
        | None => model |> return_quiet
        }
      | AddCase =>
        let new_case = InductionCase.init;
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
      }
    );
  };

  let can_undo = (a: action): bool =>
    switch (a) {
    | ScrutUpdate(action) => EditorManager.Update.can_undo(action)
    | CaseUpdate(_, action) => InductionCase.can_undo(action)
    | AddCase => true
    | RemoveCase(_) => true
    };

  let calculate =
      (
        ~globals: Globals.t,
        ~settings: Calc.t(CoreSettings.t),
        ~hidden: Calc.saved(bool),
        ~exp: Calc.t(Exp.t),
        ~ctx: Calc.t(Ctx.t),
        ~state: Calc.t(EvaluatorState.t),
        ~editor as _,
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
    }: model = model;
    let scrut =
      EditorManager.Update.calculate(
        ~common=Globals.to_common_global(globals),
        ~ctx=Calc.get_value(ctx),
        ~dynamics=Dynamics.Map.empty,
        ~stitch=x => x,
        ~is_dynamic_term=true,
        scrut,
      );
    let statics = EditorManager.Model.get_statics(scrut);
    let elab_scrut =
      Calc.set(~eq=Exp.fast_equal, statics.elaborated, elab_scrut);
    let scrut_ty = {
      let self_ty =
        switch (
          Id.Map.find_opt(Exp.rep_id(statics.elaborated), statics.info_map)
        ) {
        | Some(Info.InfoExp({ty, _})) => ty
        | _ => raise(Elaborator.MissingTypeInfo)
        };
      Calc.set(~eq=Typ.fast_equal, self_ty, scrut_ty);
    };
    let scrut_co_ctx = {
      let self_co_ctx =
        switch (
          Id.Map.find_opt(Exp.rep_id(statics.elaborated), statics.info_map)
        ) {
        | Some(Info.InfoExp({co_ctx, _})) => co_ctx
        | _ => CoCtx.empty
        };
      Calc.set(self_co_ctx, scrut_co_ctx);
    };
    let cases =
      List.map(
        InductionCase.calculate(
          ~globals,
          ~settings,
          ~scrut_ty,
          ~elab_scrut,
          ~scrut_co_ctx,
          ~ctx,
          ~exp,
          ~state,
        ),
        cases,
      );

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

    let result = exp |> Calc.save;
    let result_state = state |> Calc.save;

    Some((
      {
        scrut,
        cases,
        elab_scrut: elab_scrut |> Calc.save,
        scrut_ty: scrut_ty |> Calc.save,
        scrut_co_ctx: scrut_co_ctx |> Calc.save,
        result,
        result_state,
        join_exp: join_exp |> Calc.save,
      },
      hidden |> Calc.set(false),
      Some((join_exp, state)),
    ));
  };

  let get_cursor_info =
      (
        ~globals: Globals.t,
        ~inject: action => Ui_effect.t(unit),
        ~focus: focus,
        model: model,
      )
      : Haz3lcore.Cursor.t =>
    switch (focus) {
    | Scrut(ed_focus) =>
      EditorManager.Focus.get_cursor_info(
        ~common=Globals.to_common_global(globals),
        ~dynamics=Language.Dynamics.Map.empty,
        ~inject=x => inject(ScrutUpdate(x)),
        ~read_only=false,
        model.scrut,
        ed_focus,
      )
    | Case(i, a) =>
      switch (List.nth_opt(model.cases, i)) {
      | Some(case) =>
        InductionCase.get_cursor_info(
          ~globals,
          ~inject=x => inject(CaseUpdate(i, x)),
          ~focus=a,
          case,
        )
      | None => Haz3lcore.Cursor.empty
      }
    };

  let handle_key_event =
      (~focus: focus, ~event: Key.t, model: model): option(action) =>
    switch (focus) {
    | Scrut(_) =>
      // Use standard keyboard handler for editor actions
      Keyboard.handle_key_event(event) |> Option.map(x => ScrutUpdate(x))
    | Case(i, a) =>
      switch (List.nth_opt(model.cases, i)) {
      | Some(case) =>
        InductionCase.handle_key_event(~focus=a, ~event, case)
        |> Option.map(x => CaseUpdate(i, x))
      | None => None
      }
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
    let scrut_editor = {
      let statics = EditorManager.Model.get_statics(model.scrut);
      let common: Common.t = {
        settings: globals.settings.core,
        font_metrics: globals.font_metrics,
        secondary_icons: globals.settings.secondary_icons,
        color_highlights: globals.color_highlights,
        statics,
        dynamics: Dynamics.Map.empty,
      };
      Editor.View.view(
        ~common,
        ~mode=
          Editable({
            inject: x => inject(ScrutUpdate(x)),
            take_focus: _ => take_focus(Scrut(Editor.Focus.here())),
            escape: _ => Ui_effect.Ignore,
            focus:
              switch (focus) {
              | Some(Scrut(f)) => Some(f)
              | _ => None
              },
          }),
        ~sort=Sort.Exp,
        model.scrut.editor,
      );
    };

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
