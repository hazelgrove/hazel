open Util;
open Language;

/* This file follows conventions in [docs/ui-architecture.md] */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    cached_elab_subst: Calc.saved(Exp.t),
    root: StepperBase.step_model,
    rewrite_level: Axioms.rewrite_level,
    automation_stage: Axioms.automation_stage,
    automation_settings_open: bool,
  };

  let default_rewrite_level = Axioms.Trigonometry;

  let init = {
    cached_elab_subst: Calc.Pending,
    root: StepperBase.init_step,
    rewrite_level: default_rewrite_level,
    automation_stage: Axioms.MultiStepCheck,
    automation_settings_open: false,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = {root: StepperBase.persistent_step};

  let persist = (model: t): persistent => {
    root: StepperBase.Stepper.persist(model.root),
  };

  let unpersist = (p: persistent): t => {
    {
      cached_elab_subst: Calc.Pending,
      root: StepperBase.Stepper.unpersist(p.root),
      rewrite_level: default_rewrite_level,
      automation_stage: Axioms.MultiStepCheck,
      automation_settings_open: false,
    };
  };

  let get_validity = (m: t) => StepperBase.Stepper.get_validity(m.root);
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | StepperAction(StepperBase.step_action)
    | ToggleAutomationSettings
    | SelectRewriteLevel(Axioms.rewrite_level)
    | SelectAutomationStage(Axioms.automation_stage);

  let update = (~settings, action, model: Model.t) => {
    Updated.(
      switch (action) {
      | ToggleAutomationSettings =>
        {
          ...model,
          automation_settings_open: !model.automation_settings_open,
        }
        |> Updated.return_quiet(~logged=true)
      | StepperAction(action) =>
        let* root = StepperBase.Stepper.update(~settings, action, model.root);
        {
          ...model,
          root,
        };
      | SelectRewriteLevel(rewrite_level)
          when Axioms.rewrite_level_enabled(rewrite_level) =>
        {
          ...model,
          rewrite_level,
        }
        |> Updated.return_quiet(~logged=true)
      | SelectRewriteLevel(_) => model |> Updated.return_quiet
      | SelectAutomationStage(automation_stage) =>
        {
          ...model,
          automation_stage,
        }
        |> Updated.return_quiet(~logged=true)
      }
    );
  };

  let calculate =
      (
        ~settings: Calc.t(CoreSettings.t),
        ~ctx: Calc.t(SemanticCtx.t),
        elab: Calc.t(Exp.t),
        ~ana=Calc.OldValue(Typ.fresh(Unknown(SynSwitch))),
        {
          cached_elab_subst,
          root,
          rewrite_level,
          automation_stage,
          automation_settings_open,
        }: Model.t,
      )
      : Model.t => {
    let elab_subst =
      cached_elab_subst
      |> {
        open Calc.Syntax;
        let.calc elab = elab;
        elab |> Substitution.in_exp(Builtins.env_init) |> Exp.replace_all_ids;
      };
    let (root, _, _) =
      StepperBase.Stepper.calculate_with_level(
        ~rewrite_level,
        ~settings,
        ~ctx,
        ~exp=elab_subst,
        ~ana,
        root,
      );
    {
      cached_elab_subst: elab_subst |> Calc.save,
      root,
      rewrite_level,
      automation_stage,
      automation_settings_open,
    };
  };

  let can_undo =
    fun
    | StepperAction(action) => StepperBase.Stepper.can_undo(action)
    | ToggleAutomationSettings
    | SelectRewriteLevel(_)
    | SelectAutomationStage(_) => false;
};

module Focus = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = StepperBase.step_focus;

  open Cursor;

  let get_cursor_info =
      (~inject, ~focus: t, model: Model.t): cursor(Update.t) => {
    let+ ci =
      StepperBase.Stepper.get_cursor_info(
        ~inject=action => inject(Update.StepperAction(action)),
        ~focus,
        model.root,
      );
    Update.StepperAction(ci);
  };
};

module View = {
  open Virtual_dom.Vdom;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type event =
    | MakeActive(Focus.t)
    | HideStepper;

  let view =
      (
        ~globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~selected: option(Focus.t),
        ~is_toplevel=true,
        model: Model.t,
      ) => {
    let control_option =
        (~active, ~enabled, ~label, ~detail, ~callback: Ui_effect.t(unit)) =>
      Node.div(
        ~attrs=[
          Attr.classes(
            ["math-automation-option"]
            @ (active ? ["active"] : [])
            @ (enabled ? [] : ["disabled"]),
          ),
          Attr.on_click(_ =>
            enabled
              ? Ui_effect.Many([
                  callback,
                  Virtual_dom.Vdom.Effect.Stop_propagation,
                ])
              : Virtual_dom.Vdom.Effect.Stop_propagation
          ),
        ],
        [
          Node.div(
            ~attrs=[Attr.class_("math-automation-option-label")],
            [Node.text(label)],
          ),
          Node.div(
            ~attrs=[Attr.class_("math-automation-option-detail")],
            [Node.text(detail)],
          ),
        ],
      );

    let automation_choices = [
      Node.div(
        ~attrs=[Attr.class_("math-automation-control")],
        [
          Node.div(
            ~attrs=[Attr.class_("math-automation-control-label")],
            [Node.text("Math level")],
          ),
          Node.div(
            ~attrs=[Attr.class_("math-automation-options")],
            Axioms.rewrite_levels
            |> List.map(level => {
                 let enabled = Axioms.rewrite_level_enabled(level);
                 control_option(
                   ~active=model.rewrite_level == level,
                   ~enabled,
                   ~label=Axioms.rewrite_level_label(level),
                   ~detail=Axioms.rewrite_level_detail(level),
                   ~callback=inject(SelectRewriteLevel(level)),
                 );
               }),
          ),
        ],
      ),
      Node.div(
        ~attrs=[Attr.class_("math-automation-control")],
        [
          Node.div(
            ~attrs=[Attr.class_("math-automation-control-label")],
            [Node.text("Automation")],
          ),
          Node.div(
            ~attrs=[Attr.class_("math-automation-options")],
            Axioms.automation_stages
            |> List.map(stage =>
                 control_option(
                   ~active=model.automation_stage == stage,
                   ~enabled=true,
                   ~label=Axioms.automation_stage_label(stage),
                   ~detail=Axioms.automation_stage_detail(stage),
                   ~callback=inject(SelectAutomationStage(stage)),
                 )
               ),
          ),
        ],
      ),
    ];

    let automation_summary =
      Axioms.rewrite_level_label(model.rewrite_level)
      ++ " / "
      ++ Axioms.automation_stage_label(model.automation_stage);

    let automation_controls =
      Node.div(
        ~attrs=[Attr.class_("math-automation-settings")],
        [
          Node.div(
            ~attrs=[
              Attr.class_("settings-action"),
              Attr.on_pointerdown(_ =>
                Ui_effect.Many([
                  inject(ToggleAutomationSettings),
                  Virtual_dom.Vdom.Effect.Stop_propagation,
                ])
              ),
            ],
            [
              Widgets.toggle(
                ~tooltip="choose math automation",
                "∑",
                model.automation_settings_open,
                _ =>
                Ui_effect.Ignore
              ),
              Node.div([
                Node.text("math automation"),
                Node.div(
                  ~attrs=[Attr.class_("settings-action-detail")],
                  [Node.text(automation_summary)],
                ),
              ]),
            ],
          ),
        ]
        @ (
          model.automation_settings_open
            ? [
              Node.div(
                ~attrs=[Attr.class_("math-automation-popover")],
                automation_choices,
              ),
            ]
            : []
        ),
      );

    let settings_modal =
      globals.settings.core.evaluation.show_settings
        ? SettingsModal.view(
            ~inject=u => globals.inject_global(Set(u)),
            ~extra=[automation_controls],
            globals.settings.core.evaluation,
          )
        : [];
    StepperBase.Stepper.view_with_automation(
      ~globals,
      ~take_focus=f => signal(MakeActive(f)),
      ~hide_stepper=signal(HideStepper),
      ~inject=u => inject(StepperAction(u)),
      ~rewrite_level=model.rewrite_level,
      ~automation_stage=model.automation_stage,
      ~is_toplevel,
      ~focus=selected,
      model.root,
    )
    @ settings_modal;
  };
};
