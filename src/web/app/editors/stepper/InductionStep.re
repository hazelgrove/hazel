open Util;
open Language;
open Haz3lcore;
open StepInterface;
open Calc.Syntax;

/* Types are defined outside the functor to make it
   easier to use them in other files. */

[@deriving (show({with_path: false}), sexp, yojson)]
type model'('stepper) = {
  // Updated
  scrut: CodeEditable.Model.t,
  cases: list(InductionCase.model'('stepper)),
  // Calculated
  elab_scrut_raw: Calc.saved(Exp.t),
  elab_scrut_sub: Calc.saved(Exp.t),
  scrut_ty: Calc.saved(Typ.t),
  scrut_co_ctx: Calc.saved(CoCtx.t),
  result: Calc.saved(Exp.t),
  join_exp: Calc.saved(Exp.t),
  is_exhaustive: Calc.saved(bool),
  validity: Calc.saved(option(bool)),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent'('stepper) = {
  scrut: CodeEditable.Model.persistent,
  cases: list(InductionCase.persistent'('stepper)),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type action'('step) =
  | ScrutUpdate(CodeEditable.Update.t)
  | CaseUpdate(int, InductionCase.action'('step))
  | AddCase
  | RemoveCase(int);

[@deriving (show({with_path: false}), sexp, yojson)]
type focus'('step) =
  | Scrut(CodeEditable.Selection.t)
  | Case(int, InductionCase.focus'('step));

let init = (~exp: option(Exp.t)=?, ()) => {
  let scrut =
    switch (exp) {
    | Some(e) =>
      CodeEditable.Model.mk(
        Editor.Model.mk(
          Zipper.unzip(
            ExpToSegment.exp_to_segment(
              ~settings=ExpToSegment.Settings.editable(~inline=true),
              e,
            ),
          ),
        ),
      )
    | None => CodeEditable.Model.mk(Editor.Model.mk(Zipper.init()))
    };
  {
    scrut,
    cases: [],
    elab_scrut_raw: Calc.Pending,
    elab_scrut_sub: Calc.Pending,
    scrut_ty: Calc.Pending,
    scrut_co_ctx: Calc.Pending,
    result: Calc.Pending,
    join_exp: Calc.Pending,
    is_exhaustive: Calc.Pending,
    validity: Calc.Pending,
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

  let persist = (model: model) => {
    {
      scrut: CodeEditable.Model.persist(model.scrut),
      cases: List.map(InductionCase.persist, model.cases),
    };
  };

  let unpersist = (p: persistent) => {
    {
      scrut: CodeEditable.Model.unpersist(p.scrut),
      cases: List.map(InductionCase.unpersist, p.cases),
      elab_scrut_raw: Calc.Pending,
      elab_scrut_sub: Calc.Pending,
      scrut_ty: Calc.Pending,
      scrut_co_ctx: Calc.Pending,
      result: Calc.Pending,
      join_exp: Calc.Pending,
      is_exhaustive: Calc.Pending,
      validity: Calc.Pending,
    };
  };

  let update = (~settings: Settings.t, action: action, model: model) => {
    Updated.(
      switch (action) {
      | ScrutUpdate(a) =>
        let* new_scrut =
          CodeEditable.Update.update(~settings, a, model.scrut);
        {
          ...model,
          scrut: new_scrut,
        };
      | CaseUpdate(i, a) =>
        switch (List.nth_opt(model.cases, i)) {
        | Some(case) =>
          let* new_case = InductionCase.update(~settings, a, case);
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

  let can_undo = a =>
    switch (a) {
    | ScrutUpdate(action) => CodeEditable.Update.can_undo(action)
    | CaseUpdate(_, action) => InductionCase.can_undo(action)
    | AddCase => true
    | RemoveCase(_) => true
    };

  let calculate =
      (
        ~settings: Calc.t(CoreSettings.t),
        ~hidden: Calc.saved(bool),
        ~exp: Calc.t(Exp.t),
        ~ctx: Calc.t(SemanticCtx.t),
        ~editor as _,
        ~info_map,
        ~ana: Calc.t(Typ.t),
        model: model,
      ) => {
    let {
      scrut,
      cases,
      elab_scrut_raw,
      elab_scrut_sub,
      scrut_ty,
      scrut_co_ctx,
      result: _,
      join_exp,
      is_exhaustive,
      validity,
    }: model = model;
    let scrut =
      CodeEditable.Update.calculate(
        ~settings=Calc.get_value(settings),
        ~ctx=Calc.get_value(ctx).ctx,
        ~dynamics=Calc.OldValue(Dynamics.empty),
        ~is_edited=true,
        ~stitch=x => x,
        ~is_dynamic_term=true,
        scrut,
      );
    let elab_scrut_raw =
      Calc.set(
        ~eq=Exp.fast_equal,
        CodeEditable.Model.get_statics(scrut).elaborated,
        elab_scrut_raw,
      );
    let elab_scrut_sub =
      elab_scrut_sub
      |> {
        let.calc raw = elab_scrut_raw
        and.calc sem_ctx = ctx;
        let env = SemanticCtx.get_env(sem_ctx);
        Substitution.in_exp(env, raw);
      };
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
          ~scrut_co_ctx,
          ~elab_scrut=elab_scrut_sub,
          ~ctx,
          ~info_map,
          ~exp,
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
        Coverage.check(
          constraints,
          Typ.normalize(SemanticCtx.get_ctx(ctx), scrut_ty),
        ).
          exhaustiveness
        == Exhaustive;
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
          is_exhaustive ? Some(true) : None,
          validities,
        );
      };

    let result = exp |> Calc.save;

    Some((
      {
        scrut,
        cases,
        elab_scrut_raw: elab_scrut_raw |> Calc.save,
        elab_scrut_sub: elab_scrut_sub |> Calc.save,
        scrut_ty: scrut_ty |> Calc.save,
        scrut_co_ctx: scrut_co_ctx |> Calc.save,
        result,
        join_exp: join_exp |> Calc.save,
        is_exhaustive: is_exhaustive |> Calc.save,
        validity: validity |> Calc.save,
      },
      hidden |> Calc.set(false),
      Some(Calc.OldValue(Exp.fresh(Atom(Bool(true))))),
      validity,
    ));
  };

  let get_cursor_info = (~focus: focus, model: model) =>
    Cursor.(
      switch (focus) {
      | Scrut(a) =>
        let+ ci =
          CodeEditable.Selection.get_cursor_info(~selection=a, model.scrut);
        ScrutUpdate(ci);
      | Case(i, a) =>
        switch (List.nth_opt(model.cases, i)) {
        | Some(case) =>
          let+ ci = InductionCase.get_cursor_info(~focus=a, case);
          CaseUpdate(i, ci);
        | None => Cursor.empty
        }
      }
    );

  let handle_key_event = (~focus: focus, ~event: Key.t, model: model) =>
    switch (focus) {
    | Scrut(a) =>
      let editor = model.scrut;
      CodeEditable.Selection.handle_key_event(~selection=a, editor, event)
      |> Option.map(x => ScrutUpdate(x));
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
    WebUtil.Node.text("Induction");

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
        ~dynamics=Dynamics.Map.empty,
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
          WebUtil.Node.text("Induction on: "),
          WebUtil.div_c("inline-editor-wrapper", [scrut_editor]),
        ],
      ),
    ]
    @ cases
    @ [add_case_button]
    @ [
      model.is_exhaustive |> Calc.get_saved_exc(~print="exhaustive")
        ? WebUtil.Node.text("exhaustive") : WebUtil.Node.text("inexhaustive"),
    ];
  };
};
