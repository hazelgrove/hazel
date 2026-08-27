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
    show_log_panel: bool,
    show_debug_panel: bool,
    explainThis: ExplainThisModel.Settings.t,
    sidebar: SidebarModel.Settings.t,
    /* Auto probe: automatically place a multi probe on the body of
       whichever top-level definition the cursor is currently inside */
    autoprobe_mode: bool,
    agent_globals: AgentGlobals.Model.t,
    line_numbers: bool,
    relative_line_numbers: bool,
    cap_undo_stack: bool,
    show_row_lines: bool,
    show_incremental_deco: bool,
  };

  let init = {
    captions: true,
    secondary_icons: false,
    core: {
      statics: true,
      elaborate: false,
      assist: true,
      dynamics: true,
      probe_all: false,
      deep_reassociate: true,
      flip_animations: true,
      display_warnings: true,
      selection_chunkiness: false,
      evaluation: {
        show_case_clauses: true,
        show_fn_bodies: false,
        show_fixpoints: false,
        show_ascription_steps: false,
        show_ascriptions: false,
        show_case_steps: false,
        show_lookup_steps: false,
        show_stepper_filters: false,
        stepper_history: false,
        show_settings: false,
        show_hidden_steps: false,
        enable_proof: false,
        project_tables: false,
        project_html: false,
      },
    },
    async_evaluation: false,
    context_inspector: false,
    instructor_mode: false,
    benchmark: false,
    show_log_panel: false,
    show_debug_panel: false,
    explainThis: {
      show: true,
      show_feedback: false,
      highlight: NoHighlight,
    },
    sidebar: {
      panel: LanguageDocumentation,
      show: true,
      problems: {
        collapsed: [],
        collapsed_editors: [],
        flat: false,
        expanded: [],
      },
      debug_show_raw: false,
      /* Start the Worker Messaging benchmark section collapsed so it doesn't
         run by default (benchmarking is gated on the section being expanded).
         Must match WorkerMessagingSection.title. */
      debug_collapsed: ["Worker Messaging"],
      /* Only the active encoding (Marshal) is benchmarked by default; Direct
         and Sexp start unchecked. */
      worker_encodings: [WorkerServer.Marshal],
    },
    autoprobe_mode: false,
    agent_globals: AgentGlobals.init(),
    line_numbers: false,
    relative_line_numbers: false,
    cap_undo_stack: false,
    show_row_lines: false,
    show_incremental_deco: false,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = t;
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
    | ForceShowRecord
    | EnableProof
    | ShowCaseClauses
    | ShowFnBodies
    | ShowAscriptionSteps
    | ShowAscriptions
    | ShowCaseSteps
    | ShowFixpoints
    | ShowLookups
    | ShowFilters
    | ShowSettings
    | ShowHiddenSteps
    | ProjectTables
    | ProjectHtml;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Captions
    | SecondaryIcons
    | Statics
    | Dynamics
    | ProbeAll
    | DeepReassociate
    | SelectionChunkiness
    | Assist
    | Elaborate
    | Benchmark
    | ContextInspector
    | InstructorMode
    | ShowLogPanel
    | ShowDebugPanel
    | Evaluation(evaluation)
    | Sidebar(SidebarModel.Settings.action)
    | ExplainThis(ExplainThisModel.Settings.action)
    | DisplayWarnings
    | FlipAnimations
    | AutoprobeMode
    | ToggleLineNumbers
    | ToggleRelativeLineNumbers
    | CapUndoStack
    | ShowRowLines
    | ShowIncrementalDeco;

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
      | ProbeAll => {
          ...settings,
          core: {
            ...settings.core,
            /* Turning on probe_all requires dynamics to be on */
            dynamics: !settings.core.probe_all || settings.core.dynamics,
            statics: !settings.core.probe_all || settings.core.statics,
            probe_all: !settings.core.probe_all,
          },
        }
      | DeepReassociate => {
          ...settings,
          core: {
            ...settings.core,
            deep_reassociate: !settings.core.deep_reassociate,
          },
        }
      | SelectionChunkiness => {
          ...settings,
          core: {
            ...settings.core,
            selection_chunkiness: !settings.core.selection_chunkiness,
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
      | DisplayWarnings => {
          ...settings,
          core: {
            ...settings.core,
            display_warnings: !settings.core.display_warnings,
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
          | ProjectTables => {
              ...evaluation,
              project_tables: !evaluation.project_tables,
            }
          | ProjectHtml => {
              ...evaluation,
              project_html: !evaluation.project_html,
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
          | ShowAscriptions => {
              ...evaluation,
              show_ascriptions: !evaluation.show_ascriptions,
            }
          | ShowCaseSteps => {
              ...evaluation,
              show_case_steps: !evaluation.show_case_steps,
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
            ...settings.sidebar,
            show:
              !settings.sidebar.show
                ? true
                : settings.sidebar.panel == windowToSwitchTo ? false : true,
            panel: windowToSwitchTo,
          },
        }
      | Sidebar(Problems(ToggleCollapsed(label, cat))) => {
          ...settings,
          sidebar: {
            ...settings.sidebar,
            problems:
              SidebarModel.Settings.toggle_collapsed(
                label,
                cat,
                settings.sidebar.problems,
              ),
          },
        }
      | Sidebar(Problems(ToggleEditorCollapsed(label))) => {
          ...settings,
          sidebar: {
            ...settings.sidebar,
            problems:
              SidebarModel.Settings.toggle_editor_collapsed(
                label,
                settings.sidebar.problems,
              ),
          },
        }
      | Sidebar(Problems(ToggleFlat)) => {
          ...settings,
          sidebar: {
            ...settings.sidebar,
            problems: {
              ...settings.sidebar.problems,
              flat: !settings.sidebar.problems.flat,
            },
          },
        }
      | Sidebar(Problems(ToggleExpanded(id))) => {
          ...settings,
          sidebar: {
            ...settings.sidebar,
            problems:
              SidebarModel.Settings.toggle_expanded(
                id,
                settings.sidebar.problems,
              ),
          },
        }
      | Sidebar(ToggleDebugRaw) => {
          ...settings,
          sidebar: {
            ...settings.sidebar,
            debug_show_raw: !settings.sidebar.debug_show_raw,
          },
        }
      | Sidebar(ToggleDebugCollapsed(key)) => {
          ...settings,
          sidebar:
            SidebarModel.Settings.toggle_debug_collapsed(
              key,
              settings.sidebar,
            ),
        }
      | Sidebar(ToggleWorkerEncoding(e)) => {
          ...settings,
          sidebar: SidebarModel.Settings.toggle_encoding(e, settings.sidebar),
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
      | ShowLogPanel => {
          ...settings,
          show_log_panel:
            !settings.show_log_panel && ExerciseSettings.show_instructor,
        }
      | ShowDebugPanel => {
          ...settings,
          show_debug_panel: !settings.show_debug_panel,
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
      | AutoprobeMode => {
          ...settings,
          autoprobe_mode: !settings.autoprobe_mode,
        }
      | ToggleLineNumbers => {
          ...settings,
          line_numbers: !settings.line_numbers,
        }
      | ToggleRelativeLineNumbers => {
          ...settings,
          relative_line_numbers: !settings.relative_line_numbers,
        }
      | CapUndoStack => {
          ...settings,
          cap_undo_stack: !settings.cap_undo_stack,
        }
      | ShowRowLines => {
          ...settings,
          show_row_lines: !settings.show_row_lines,
        }
      | ShowIncrementalDeco => {
          ...settings,
          show_incremental_deco: !settings.show_incremental_deco,
        }
      }
    )
    |> Updated.return(
         ~scroll_active=false,
         ~historic=
           switch (action) {
           | Evaluation(ShowSettings) => false
           | _ => true
           },
       );
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Model.t;
