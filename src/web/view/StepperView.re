open Util;
open Language;

/* This file follows conventions in [docs/ui-architecture.md] */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type math_mode_panel =
    | MathModePanelClosed
    | ProfilePanelOpen
    | MathModeBuilderPanelOpen;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    cached_elab_subst: Calc.saved(Exp.t),
    root: StepperBase.step_model,
    rewrite_level: Axioms.rewrite_level,
    automation_stage: Axioms.automation_stage,
    automation_settings_open: bool,
    profile_board: ProfileBoard.Model.t,
    math_mode_builder: MathModeBuilder.Model.t,
    math_mode_panel,
  };

  let default_rewrite_level = Axioms.Calculus;
  let default_automation_stage = Axioms.MultiStepCheck;

  let init = {
    cached_elab_subst: Calc.Pending,
    root: StepperBase.init_step,
    rewrite_level: default_rewrite_level,
    automation_stage: default_automation_stage,
    automation_settings_open: false,
    profile_board: ProfileBoard.Model.init,
    math_mode_builder: MathModeBuilder.Model.init,
    math_mode_panel: MathModePanelClosed,
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
      automation_stage: default_automation_stage,
      automation_settings_open: false,
      profile_board: ProfileBoard.Model.init,
      math_mode_builder: MathModeBuilder.Model.init,
      math_mode_panel: MathModePanelClosed,
    };
  };

  let get_validity = (m: t) => StepperBase.Stepper.get_validity(m.root);
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | StepperAction(StepperBase.step_action)
    | ToggleAutomationSettings
    | ToggleProfilePanel
    | ProfileBoardAction(ProfileBoard.Update.t)
    | ToggleMathModeBuilderPanel
    | MathModeBuilderAction(MathModeBuilder.Update.t)
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
      | ToggleProfilePanel =>
        {
          ...model,
          math_mode_panel:
            model.math_mode_panel == Model.ProfilePanelOpen
              ? MathModePanelClosed : ProfilePanelOpen,
        }
        |> Updated.return_quiet(~logged=true)
      | ProfileBoardAction(action) =>
        {
          ...model,
          profile_board:
            ProfileBoard.Update.update(action, model.profile_board),
        }
        |> Updated.return_quiet(~recalculate=true, ~logged=true)
      | ToggleMathModeBuilderPanel =>
        {
          ...model,
          math_mode_panel:
            model.math_mode_panel == Model.MathModeBuilderPanelOpen
              ? MathModePanelClosed : MathModeBuilderPanelOpen,
        }
        |> Updated.return_quiet(~logged=true)
      | MathModeBuilderAction(action) =>
        {
          ...model,
          math_mode_builder:
            MathModeBuilder.Update.update(action, model.math_mode_builder),
        }
        |> Updated.return_quiet(~recalculate=true, ~logged=true)
      | StepperAction(action) =>
        let* root = StepperBase.Stepper.update(~settings, action, model.root);
        {
          ...model,
          root,
        };
      | SelectRewriteLevel(rewrite_level)
          when
            !model.math_mode_builder.active
            && Axioms.rewrite_level_enabled(rewrite_level) =>
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
          profile_board,
          math_mode_builder,
          math_mode_panel,
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
    let active_profile =
      Axioms.math_profile(rewrite_level)
      |> (
        fallback =>
          MathModeBuilder.effective_profile(~fallback, math_mode_builder)
      )
      |> ProfileBoard.apply_model_to_profile(profile_board);
    let (root, _, _) =
      StepperBase.Stepper.calculate_with_level(
        ~rewrite_level,
        ~automation_stage,
        ~active_profile,
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
      profile_board,
      math_mode_builder,
      math_mode_panel,
    };
  };

  let can_undo =
    fun
    | StepperAction(action) => StepperBase.Stepper.can_undo(action)
    | ToggleAutomationSettings
    | ToggleProfilePanel
    | ProfileBoardAction(_)
    | ToggleMathModeBuilderPanel
    | MathModeBuilderAction(_)
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

    let custom_mode_active = model.math_mode_builder.active;
    let custom_mode_label = model.math_mode_builder.label;
    let active_profile_status =
      custom_mode_active
        ? Node.div(
            ~attrs=[Attr.class_("math-automation-active-profile custom")],
            [
              Node.div([
                Node.div(
                  ~attrs=[
                    Attr.class_("math-automation-active-profile-label"),
                  ],
                  [Node.text("Custom: " ++ custom_mode_label)],
                ),
              ]),
              Widgets.button(
                ~clss=["proof-button"],
                Node.text("Turn off"),
                ~tooltip="return to the selected built-in math level",
                _ =>
                inject(
                  MathModeBuilderAction(
                    MathModeBuilder.Update.SetActive(false),
                  ),
                )
              ),
            ],
          )
        : Node.div(
            ~attrs=[Attr.class_("math-automation-active-profile")],
            [
              Node.div(
                ~attrs=[Attr.class_("math-automation-active-profile-label")],
                [
                  Node.text(
                    "Active profile: "
                    ++ Axioms.rewrite_level_label(model.rewrite_level),
                  ),
                ],
              ),
            ],
          );

    let automation_choices = [
      active_profile_status,
      Node.div(
        ~attrs=[Attr.class_("math-automation-control")],
        [
          Node.div(
            ~attrs=[Attr.class_("math-automation-control-label")],
            [Node.text("Math level")],
          ),
          Node.div(
            ~attrs=[Attr.class_("math-automation-options")],
            Axioms.selectable_rewrite_levels
            |> List.map(level => {
                 let enabled =
                   !custom_mode_active && Axioms.rewrite_level_enabled(level);
                 control_option(
                   ~active=!custom_mode_active && model.rewrite_level == level,
                   ~enabled,
                   ~label=
                     switch (level) {
                     | Trigonometry => "Trig"
                     | _ => Axioms.rewrite_level_label(level)
                     },
                   ~detail=
                     switch (level) {
                     | Arithmetic => "constants & affine"
                     | Algebra => "distribution & factoring"
                     | Trigonometry => "identities & angles"
                     | Calculus => "derivatives"
                     | FunctionsAndLists => "functions & lists"
                     },
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
                   ~detail=
                     switch (stage) {
                     | Manual => "one visible step"
                     | MultiStepCheck => "check a result"
                     | AutoEval => "prefill the target"
                     },
                   ~callback=inject(SelectAutomationStage(stage)),
                 )
               ),
          ),
        ],
      ),
    ];

    let automation_summary =
      (
        custom_mode_active
          ? "Custom: " ++ custom_mode_label
          : Axioms.rewrite_level_label(model.rewrite_level)
      )
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

    let math_mode_button = (~callback, label) =>
      Node.div(
        ~attrs=[
          Attr.class_("proof-button"),
          Attr.on_click(_ =>
            Ui_effect.Many([
              callback,
              Virtual_dom.Vdom.Effect.Stop_propagation,
            ])
          ),
        ],
        [Node.text(label)],
      );

    let derivation_math_mode_toolbar =
      Node.div(
        ~attrs=[Attr.class_("derivation-math-mode-toolbar")],
        [
          math_mode_button(
            ~callback=inject(ToggleMathModeBuilderPanel),
            "Math Mode Builder "
            ++ (
              model.math_mode_panel == MathModeBuilderPanelOpen ? "▲" : "▼"
            ),
          ),
          math_mode_button(
            ~callback=inject(ToggleProfilePanel),
            "Profile "
            ++ (model.math_mode_panel == ProfilePanelOpen ? "▲" : "▼"),
          ),
        ],
      );

    let active_profile =
      Axioms.math_profile(model.rewrite_level)
      |> (
        fallback =>
          MathModeBuilder.effective_profile(
            ~fallback,
            model.math_mode_builder,
          )
      )
      |> ProfileBoard.apply_model_to_profile(model.profile_board);
    let math_mode_panel =
      switch (
        model.math_mode_panel,
        StepperBase.Stepper.terminal_missing_step(model.root),
      ) {
      | (MathModePanelClosed, _) => []
      | (_, None) => []
      | (ProfilePanelOpen, Some(missing_step)) =>
        switch (missing_step.cached_env |> Calc.saved_to_option) {
        | None => []
        | Some(env) =>
          let base_profile =
            MathModeBuilder.effective_profile(
              ~fallback=Axioms.math_profile(model.rewrite_level),
              model.math_mode_builder,
            );
          let summary = ProfileBoard.profile_summary(active_profile);
          let results =
            ProfileBoard.default_examples
            |> List.filter((example: ProfileBoard.example) =>
                 example.level == model.rewrite_level
               )
            |> List.map(example =>
                 ProfileBoard.run_example_with_profile(
                   ~settings=globals.settings.core,
                   ~env,
                   ~profile=active_profile,
                   example,
                 )
               );
          [
            ProfileBoard.View.editable(
              ~model=model.profile_board,
              ~inject=action => inject(ProfileBoardAction(action)),
              ~on_close=inject(ToggleProfilePanel),
              ~base_profile,
              ~effective_profile=active_profile,
              ~summary,
              ~results,
            ),
          ];
        }
      | (MathModeBuilderPanelOpen, Some(missing_step)) =>
        switch (missing_step.cached_env |> Calc.saved_to_option) {
        | None => []
        | Some(env) => [
            MathModeBuilder.View.editable(
              ~model=model.math_mode_builder,
              ~inject=action => inject(MathModeBuilderAction(action)),
              ~on_close=inject(ToggleMathModeBuilderPanel),
              ~settings=globals.settings.core,
              ~env,
            ),
          ]
        }
      };

    let settings_modal =
      globals.settings.core.evaluation.show_settings
        ? SettingsModal.view(
            ~inject=u => globals.inject_global(Set(u)),
            ~extra=[automation_controls],
            globals.settings.core.evaluation,
          )
        : [];
    let inline_automation_controls = is_toplevel ? [] : [automation_controls];
    [derivation_math_mode_toolbar]
    @ inline_automation_controls
    @ StepperBase.Stepper.view_with_automation(
        ~globals,
        ~take_focus=f => signal(MakeActive(f)),
        ~hide_stepper=signal(HideStepper),
        ~inject=u => inject(StepperAction(u)),
        ~rewrite_level=model.rewrite_level,
        ~automation_stage=model.automation_stage,
        ~active_profile,
        ~is_toplevel,
        ~focus=selected,
        model.root,
      )
    @ math_mode_panel
    @ settings_modal;
  };
};
