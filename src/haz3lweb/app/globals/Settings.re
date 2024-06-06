open Sexplib.Std;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    captions: bool,
    secondary_icons: bool,
    core: Haz3lcore.CoreSettings.t,
    async_evaluation: bool,
    context_inspector: bool,
    instructor_mode: bool,
    benchmark: bool,
    explainThis: ExplainThisModel.Settings.t,
  };

  let init = {
    captions: true,
    secondary_icons: false,
    core: {
      statics: true,
      elaborate: false,
      assist: true,
      dynamics: true,
      evaluation: {
        show_case_clauses: true,
        show_fn_bodies: false,
        show_fixpoints: false,
        show_casts: false,
        show_lookup_steps: false,
        show_stepper_filters: false,
        stepper_history: false,
        show_settings: false,
        show_hidden_steps: false,
      },
    },
    async_evaluation: false,
    context_inspector: false,
    instructor_mode: true,
    benchmark: false,
    explainThis: {
      show: true,
      show_feedback: false,
      highlight: NoHighlight,
    },
  };

  let fix_instructor_mode = settings =>
    if (settings.instructor_mode && !ExerciseSettings.show_instructor) {
      {...settings, instructor_mode: false};
    } else {
      settings;
    };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = t;

  let persist = x => x;
  let unpersist = fix_instructor_mode;
};

module Store =
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
    | ShowCaseClauses
    | ShowFnBodies
    | ShowCasts
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
    | ExplainThis(ExplainThisModel.Settings.action);

  let update = (action, settings: Model.t): Updated.t(Model.t) => {
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
      | Evaluation(u) =>
        let evaluation = settings.core.evaluation;
        let evaluation: Haz3lcore.CoreSettings.Evaluation.t =
          switch (u) {
          | ShowRecord => {
              ...evaluation,
              stepper_history: !evaluation.stepper_history,
            }
          | ShowCaseClauses => {
              ...evaluation,
              show_case_clauses: !evaluation.show_case_clauses,
            }
          | ShowFnBodies => {
              ...evaluation,
              show_fn_bodies: !evaluation.show_fn_bodies,
            }
          | ShowCasts => {...evaluation, show_casts: !evaluation.show_casts}
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
      | ExplainThis(ToggleShow) => {
          ...settings,
          explainThis: {
            ...settings.explainThis,
            show: !settings.explainThis.show,
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
        let explainThis = {...settings.explainThis, highlight};
        {...settings, explainThis};
      | Benchmark => {...settings, benchmark: !settings.benchmark}
      | Captions => {...settings, captions: !settings.captions}
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
