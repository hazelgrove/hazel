open Util;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    captions: bool,
    secondary_icons: bool,
    core: Language.CoreSettings.t,
    async_evaluation: bool,
    context_inspector: bool,
    instructor_mode: bool,
    benchmark: bool,
    explainThis: ExplainThisModel.Settings.t,
    assistant: AssistantSettings.t,
    sidebar: SidebarModel.Settings.t,
  };

  let init = {
    captions: true,
    secondary_icons: false,
    core: {
      statics: true,
      elaborate: false,
      assist: true,
      dynamics: true,
      flip_animations: true,
      evaluation: {
        show_case_clauses: true,
        show_fn_bodies: false,
        show_fixpoints: false,
        show_ascription_steps: false,
        show_lookup_steps: false,
        show_stepper_filters: false,
        stepper_history: false,
        show_settings: false,
        show_hidden_steps: false,
        enable_proof: false,
      },
    },
    async_evaluation: false,
    context_inspector: false,
    instructor_mode: false,
    benchmark: false,
    explainThis: {
      show: true,
      show_feedback: false,
      highlight: NoHighlight,
    },
    assistant: {
      mode: CodeSuggestion,
      ongoing_chat: false,
      show_history: false,
      show_api_key: false,
    },
    sidebar: {
      panel: LanguageDocumentation,
      show: true,
    },
  };

  let fix_instructor_mode = settings =>
    if (settings.instructor_mode && !ExerciseSettings.show_instructor) {
      {
        ...settings,
        instructor_mode: false,
      };
    } else {
      settings;
    };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = t;

  let persist = x => x;
  let unpersist = fix_instructor_mode;
};

module SettingsStore =
  Store.F({
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Model.persistent;
    let key = Store.Settings;
    let default = () => Model.init;
  });

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type evaluation =
    | ShowRecord
    | ForceShowRecord
    | EnableProof
    | ShowCaseClauses
    | ShowFnBodies
    | ShowAscriptionSteps
    | ShowFixpoints
    | ShowLookups
    | ShowFilters
    | ShowSettings
    | ShowHiddenSteps;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Captions
    | SecondaryIcons
    | Statics
    | Dynamics
    | Assist
    | Elaborate
    | Benchmark
    | ContextInspector
    | InstructorMode
    | Evaluation(evaluation)
    | Sidebar(SidebarModel.Settings.action)
    | ExplainThis(ExplainThisModel.Settings.action)
    | Assistant(AssistantSettings.action)
    | FlipAnimations;

  let can_undo = (action: t) => {
    switch (action) {
    | Evaluation(ShowSettings) => false
    | _ => true
    };
  };

  let update = (~action, ~settings: Model.t): Updated.t(Model.t) => {
    (
      switch (action) {
      | Statics => {
          ...settings,
          core: {
            ...settings.core,
            statics: !settings.core.statics,
            assist: !settings.core.statics,
            dynamics: !settings.core.statics && settings.core.dynamics,
          },
        }
      | Elaborate => {
          ...settings,
          core: {
            ...settings.core,
            statics: !settings.core.elaborate || settings.core.statics,
            elaborate: !settings.core.elaborate,
          },
        }
      | Dynamics => {
          ...settings,
          core: {
            ...settings.core,
            statics: !settings.core.dynamics || settings.core.statics,
            dynamics: !settings.core.dynamics,
          },
        }
      | Assist => {
          ...settings,
          core: {
            ...settings.core,
            statics: !settings.core.assist || settings.core.statics,
            assist: !settings.core.assist,
          },
        }
      | FlipAnimations => {
          ...settings,
          core: {
            ...settings.core,
            flip_animations: !settings.core.flip_animations,
          },
        }
      | Evaluation(u) =>
        let evaluation = settings.core.evaluation;
        let evaluation: Language.CoreSettings.Evaluation.t =
          switch (u) {
          | ShowRecord => {
              ...evaluation,
              stepper_history: !evaluation.stepper_history,
            }
          | ForceShowRecord => {
              ...evaluation,
              stepper_history: true,
            }
          | EnableProof => {
              ...evaluation,
              enable_proof: !evaluation.enable_proof,
            }
          | ShowCaseClauses => {
              ...evaluation,
              show_case_clauses: !evaluation.show_case_clauses,
            }
          | ShowFnBodies => {
              ...evaluation,
              show_fn_bodies: !evaluation.show_fn_bodies,
            }
          | ShowAscriptionSteps => {
              ...evaluation,
              show_ascription_steps: !evaluation.show_ascription_steps,
            }
          | ShowFixpoints => {
              ...evaluation,
              show_fixpoints: !evaluation.show_fixpoints,
            }
          | ShowLookups => {
              ...evaluation,
              show_lookup_steps: !evaluation.show_lookup_steps,
            }
          | ShowFilters => {
              ...evaluation,
              show_stepper_filters: !evaluation.show_stepper_filters,
            }
          | ShowSettings => {
              ...evaluation,
              show_settings: !evaluation.show_settings,
            }
          | ShowHiddenSteps => {
              ...evaluation,
              show_hidden_steps: !evaluation.show_hidden_steps,
            }
          };
        {
          ...settings,
          core: {
            ...settings.core,
            evaluation,
          },
        };
      | Sidebar(ToggleShow) => {
          ...settings,
          sidebar: {
            ...settings.sidebar,
            show: !settings.sidebar.show,
          },
        }
      | Sidebar(SwitchPanel(windowToSwitchTo)) => {
          ...settings,
          sidebar: {
            show:
              !settings.sidebar.show
                ? true
                : settings.sidebar.panel == windowToSwitchTo ? false : true,
            panel: windowToSwitchTo,
          },
        }
      | ExplainThis(ToggleShowFeedback) => {
          ...settings,
          explainThis: {
            ...settings.explainThis,
            show_feedback: !settings.explainThis.show_feedback,
          },
        }
      | ExplainThis(SetHighlight(a)) =>
        let highlight: ExplainThisModel.Settings.highlight =
          switch (a, settings.explainThis.highlight) {
          | (Toggle, All) => NoHighlight
          | (Toggle, _) => All
          | (Hover(_), All) => All
          | (Hover(id), _) => One(id)
          | (UnsetHover, All) => All
          | (UnsetHover, _) => NoHighlight
          };
        let explainThis = {
          ...settings.explainThis,
          highlight,
        };
        {
          ...settings,
          explainThis,
        };
      | Assistant(u) =>
        switch (u) {
        | UpdateChatStatus => {
            ...settings,
            assistant: {
              ...settings.assistant,
              ongoing_chat: !settings.assistant.ongoing_chat,
            },
          }
        | SwitchMode(mode) => {
            ...settings,
            assistant: {
              ...settings.assistant,
              mode,
            },
          }
        | ToggleHistory => {
            ...settings,
            assistant: {
              ...settings.assistant,
              show_history: !settings.assistant.show_history,
            },
          }
        | ToggleAPIKeyVisibility => {
            ...settings,
            assistant: {
              ...settings.assistant,
              show_api_key: !settings.assistant.show_api_key,
            },
          }
        }
      | Benchmark => {
          ...settings,
          benchmark: !settings.benchmark,
        }
      | Captions => {
          ...settings,
          captions: !settings.captions,
        }
      | SecondaryIcons => {
          ...settings,
          secondary_icons: !settings.secondary_icons,
        }
      | ContextInspector => {
          ...settings,
          context_inspector: !settings.context_inspector,
        }
      | InstructorMode => {
          ...settings, //TODO[Matt]: Make sure instructor mode actually makes prelude read-only
          instructor_mode: !settings.instructor_mode,
        }
      }
    )
    |> Updated.return(~scroll_active=false);
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Model.t;
