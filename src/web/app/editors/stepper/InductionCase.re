open Util;
open Language;
open Haz3lcore;
open StepInterface;

[@deriving (show({with_path: false}), sexp, yojson)]
type model'('stepper) = {
  // Updated
  pattern: CodeEditable.Model.t,
  // Calculated
  elab_pattern: Calc.saved(Pat.t),
  inner_exp: Calc.saved(Exp.t),
  step: 'stepper,
  last_exp: Calc.saved(Exp.t),
  inductive_hypotheses: Calc.saved(list(Exp.t)),
  case_eq: Calc.saved((Var.t, Exp.t)),
  added_ctx: Calc.saved((list(Ctx.entry), list(Var.t))),
  inner_ctx: Calc.saved(Ctx.t),
  added_env: Calc.saved(list((Var.t, Exp.t))),
  inner_env: Calc.saved(ClosureEnvironment.t),
  constraint_: Calc.saved(option(Coverage.Constraint.t)),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent'('stepper) = {
  pattern: CodeEditable.Model.persistent,
  stepper: 'stepper,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type action'('stepper) =
  | PatternUpdate(CodeEditable.Update.t)
  | StepUpdate('stepper);

[@deriving (show({with_path: false}), sexp, yojson)]
type focus'('stepper) =
  | Pattern(CodeSelectable.Selection.t)
  | Stepper('stepper);

module F = (Stepper: STEPPER) => {
  type model = model'(Stepper.model);
  type persistent = persistent'(Stepper.persistent);
  type action = action'(Stepper.action);
  type focus = focus'(Stepper.focus);

  let init = {
    pattern: CodeEditable.Model.mk(Editor.Model.mk(Zipper.init())),
    elab_pattern: Calc.Pending,
    inner_exp: Calc.Pending,
    step: Stepper.init,
    last_exp: Calc.Pending,
    inductive_hypotheses: Calc.Pending,
    case_eq: Calc.Pending,
    added_ctx: Calc.Pending,
    inner_ctx: Calc.Pending,
    added_env: Calc.Pending,
    inner_env: Calc.Pending,
    constraint_: Calc.Pending,
  };

  let persist = (model: model) => {
    {
      pattern: CodeEditable.Model.persist(model.pattern),
      stepper: Stepper.persist(model.step),
    };
  };

  let unpersist = (p: persistent) => {
    {
      pattern: CodeEditable.Model.unpersist(p.pattern),
      elab_pattern: Calc.Pending,
      inner_exp: Calc.Pending,
      step: Stepper.unpersist(p.stepper),
      last_exp: Calc.Pending,
      inductive_hypotheses: Calc.Pending,
      case_eq: Calc.Pending,
      inner_ctx: Calc.Pending,
      added_ctx: Calc.Pending,
      added_env: Calc.Pending,
      inner_env: Calc.Pending,
      constraint_: Calc.Pending,
    };
  };

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

    let case_eq =
      model.case_eq
      |> {
        open Calc.Syntax;
        let.calc elab_pattern = elab_pattern
        and.calc elab_scrut = elab_scrut
        and.calc ctx = ctx
        and.calc env = env;
        let var_name =
          Var.free_name(
            "case_eq",
            List.map(
              (e: Ctx.var_entry) => e.name,
              Ctx.get_var_entries(ctx),
            ),
          );
        (
          var_name,
          BinOp(
            Poly(Equals),
            elab_scrut,
            elab_pattern |> ProofHacks.pat_to_exp,
          )
          |> Exp.fresh
          |> Exp.substitute_closures(ClosureEnvironment.map_of(env)),
        );
      };

    let inductive_hypotheses =
      model.inductive_hypotheses
      |> {
        open Calc.Syntax;
        let.calc elab_pattern = elab_pattern
        and.calc elab_scrut = elab_scrut
        and.calc scrut_co_ctx = scrut_co_ctx
        and.calc exp = exp
        and.calc scrut_ty = scrut_ty;
        ProofHacks.get_inductive_hypotheses(
          CodeEditable.Model.get_statics(pattern).info_map,
          scrut_ty,
          elab_pattern,
        )
        |> List.map(h =>
             ProofHacks.replace_exp(
               elab_scrut,
               scrut_co_ctx,
               h |> ProofHacks.pat_to_exp,
               h |> Pat.bindings |> CoCtx.of_bindings,
               exp,
             )
           );
      };

    let added_ctx =
      model.added_ctx
      |> {
        open Calc.Syntax;
        let.calc inductive_hypotheses = inductive_hypotheses
        and.calc ctx = ctx
        and.calc env = env
        and.calc (case_eq_name, case_eq_exp) = case_eq;

        let case_eq: Ctx.entry =
          VarEntry({
            name: case_eq_name,
            id: Id.mk(),
            typ: Typ.fresh(ProofOf(case_eq_exp)),
            custom_statics: None,
          });

        let (hypo_entries: list(Ctx.entry), ih_names: list(Var.t)) =
          inductive_hypotheses
          |> List.map(e =>
               Typ.fresh(
                 ProofOf(
                   e
                   |> Exp.substitute_closures(ClosureEnvironment.map_of(env)),
                 ),
               )
             )
          |> List.fold_left_map(
               ((acc, ih_names), ty) => {
                 let name =
                   Var.free_name(
                     "ih",
                     List.map(
                       (e: Ctx.var_entry) => e.name,
                       acc @ Ctx.get_var_entries(ctx),
                     ),
                   );
                 let var_entry =
                   Ctx.{
                     name,
                     id: Id.mk(),
                     typ: ty,
                     custom_statics: None,
                   };
                 let entry = Ctx.VarEntry(var_entry);
                 (([var_entry, ...acc], [name, ...ih_names]), entry);
               },
               ([], []),
             )
          |> ((((_, ih_names), hypo_entries)) => (hypo_entries, ih_names));
        ([case_eq] @ hypo_entries, ih_names);
      };

    let inner_ctx =
      model.inner_ctx
      |> {
        open Calc.Syntax;
        let.calc ctx = ctx
        and.calc elab_pattern = elab_pattern
        and.calc scrut_ty = scrut_ty
        and.calc (added_ctx, _) = added_ctx;
        let ctx = List.fold_left(Ctx.extend, ctx, added_ctx |> List.rev);
        let ctx =
          ProofHacks.dhpat_extend_ctx(elab_pattern, scrut_ty, ctx)
          |> Option.value(~default=ctx);
        ctx;
      };

    let added_env =
      model.added_env
      |> {
        open Calc.Syntax;
        let.calc (_, ih_names) = added_ctx
        and.calc inductive_hypotheses = inductive_hypotheses
        and.calc (case_eq_name, case_eq_exp) = case_eq;
        [(case_eq_name, Exp.fresh(ProofObject(case_eq_exp)))]
        @ List.map2(
            (ih_name, ih_exp) => (ih_name, Exp.fresh(ProofObject(ih_exp))),
            ih_names,
            inductive_hypotheses,
          );
      };

    print_endline(
      "added_env_length:"
      ++ string_of_int(List.length(Calc.get_value(added_env))),
    );

    let inner_env =
      model.inner_env
      |> {
        open Calc.Syntax;
        let.calc env = env
        and.calc added_env = added_env
        and.calc elab_pattern = elab_pattern;
        let variables = Pat.bindings(elab_pattern);
        env
        |> ClosureEnvironment.update_env(
             List.fold_left(
               Environment.extend,
               _,
               added_env
               @ List.map(
                   (v: Binding.t) => (v.name, Exp.fresh(Var(v.name))),
                   variables,
                 ),
             ),
           );
      };

    let (stepper, last_exp, validity) =
      Stepper.calculate(
        ~settings, // TODO: this is a little ugly
        ~ctx=inner_ctx,
        ~exp=inner_exp,
        ~env=inner_env,
        ~state,
        ~ana,
        model.step,
      );

    let constraint_ =
      {
        open OptUtil.Syntax;
        let statics = CodeWithStatics.Model.get_statics(pattern);
        let* info =
          Statics.Map.lookup(
            elab_pattern |> Calc.get_value |> Pat.rep_id,
            statics.info_map,
          );
        let* info_pat =
          switch (info) {
          | InfoPat(info_pat) => Some(info_pat)
          | _ => None
          };
        Some(Info.pat_constraint(info_pat));
      }
      |> Calc.set(_, model.constraint_);

    (
      {
        pattern,
        elab_pattern: elab_pattern |> Calc.save,
        inner_exp: inner_exp |> Calc.save,
        inductive_hypotheses: inductive_hypotheses |> Calc.save,
        case_eq: case_eq |> Calc.save,
        step: stepper,
        last_exp: last_exp |> Calc.save,
        added_ctx: added_ctx |> Calc.save,
        inner_ctx: inner_ctx |> Calc.save,
        constraint_: constraint_ |> Calc.save,
        added_env: added_env |> Calc.save,
        inner_env: inner_env |> Calc.save,
      },
      constraint_,
      validity,
    );
  };

  let get_cursor_info = (~focus: focus, model: model) => {
    Cursor.(
      switch (focus) {
      | Pattern(a) =>
        let+ ci =
          CodeEditable.Selection.get_cursor_info(~selection=a, model.pattern);
        PatternUpdate(ci);
      | Stepper(a) =>
        let+ ci = Stepper.get_cursor_info(~focus=a, model.step);
        StepUpdate(ci);
      }
    );
  };

  let handle_key_event = (~focus: focus, ~event: Key.t, model: model) => {
    switch (focus, model) {
    | (Pattern(a), _) =>
      CodeEditable.Selection.handle_key_event(
        ~selection=a,
        model.pattern,
        event,
      )
      |> Option.map(x => PatternUpdate(x))
    | (Stepper(a), _) =>
      Stepper.handle_key_event(~focus=a, ~event, model.step)
      |> Option.map(x => StepUpdate(x))
    };
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
            | Ctx.VarEntry({name: _, id: _, typ, _}) => {
                open OptUtil.Syntax;
                let* rule = ProofRule.typ_to_rule(typ);
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
              }
            | _ => None,
            model.added_ctx
            |> Calc.get_saved_exc(~print="InductionCase not calculated")
            |> fst,
          ),
        ),
      ]
      @ stepper_view,
    );
  };
};
