open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Widgets;

type setting_item = {
  name: string,
  active: bool,
  setting: Settings.Update.t,
  tooltip: option(string),
};

// COMPONENTS

let item_group = (~inject as _, name: string, ts) => {
  div_c("group", [div_c("name", [text(name)]), div_c("contents", ts)]);
};

let submenu = (~tooltip, ~icon, menu) =>
  div(
    ~attrs=[clss(["top-menu-item"])],
    [
      div(
        ~attrs=[clss(["submenu-icon"]), Attr.title(tooltip)],
        [div(~attrs=[clss(["icon"])], [icon])],
      ),
      div(~attrs=[clss(["submenu"])], menu),
    ],
  );

// SETTINGS MENU

let settings_group = (~globals: Globals.t, name: string, ts) => {
  let toggle = ({name, active, setting, tooltip}) =>
    toggle_named("", ~name, ~tooltip?, active, _ =>
      globals.inject_global(Set(setting))
    );
  div_c(
    "group",
    [
      div_c("name", [text(name)]),
      div_c("contents", List.map(toggle, ts)),
    ],
  );
};

let semantics_group = (~globals) => {
  settings_group(
    ~globals,
    "Semantics",
    [
      {
        name: "Types",
        active: globals.settings.core.statics,
        setting: Statics,
        tooltip: Some("Enable static typing"),
      },
      {
        name: "Completion",
        active: globals.settings.core.assist,
        setting: Assist,
        tooltip:
          Some("Enable type-directed code completion and assistive features"),
      },
      {
        name: "Evaluation",
        active: globals.settings.core.dynamics,
        setting: Dynamics,
        tooltip: Some("Evaluate expressions and show results"),
      },
      {
        name: "Docs",
        active: globals.settings.sidebar.show,
        setting: Sidebar(ToggleShow),
        tooltip: Some("Show documentation sidebar"),
      },
    ],
  );
};

let values_group = (~globals: Globals.t) => {
  let s = globals.settings.core.evaluation;
  settings_group(
    ~globals,
    "Value Display",
    [
      {
        name: "Functions",
        active: s.show_fn_bodies,
        setting: Evaluation(ShowFnBodies),
        tooltip: Some("Show function bodies in evaluated results"),
      },
      {
        name: "Cases",
        active: s.show_case_clauses,
        setting: Evaluation(ShowCaseClauses),
        tooltip: Some("Show case clauses in evaluated results"),
      },
      {
        name: "Fixpoints",
        active: s.show_fixpoints,
        setting: Evaluation(ShowFixpoints),
        tooltip: Some("Show fixpoint expressions in evaluated results"),
      },
      {
        name: "Tables",
        active: s.project_tables,
        setting: Evaluation(ProjectTables),
        tooltip: Some("Project tables in evaluated results"),
      },
      {
        name: "HTML",
        active: s.project_html,
        setting: Evaluation(ProjectHtml),
        tooltip: Some("Render HTML values in evaluated results"),
      },
      {
        name: "Ascriptions",
        active: s.show_ascriptions,
        setting: Evaluation(ShowAscriptions),
        tooltip: Some("Show type ascriptions in evaluated results"),
      },
    ],
  );
};

let stepper_group = (~globals: Globals.t) => {
  let s = globals.settings.core.evaluation;
  settings_group(
    ~globals,
    "Stepper",
    [
      {
        name: "Show lookups",
        active: s.show_lookup_steps,
        setting: Evaluation(ShowLookups),
        tooltip: Some("Show variable lookup steps in the stepper"),
      },
      {
        name: "Show hidden",
        active: s.show_hidden_steps,
        setting: Evaluation(ShowHiddenSteps),
        tooltip: Some("Show hidden intermediate steps in the stepper"),
      },
      {
        name: "Show filters",
        active: s.show_stepper_filters,
        setting: Evaluation(ShowFilters),
        tooltip: Some("Show stepper filter controls"),
      },
      {
        name: "Show Ascription Steps",
        active: s.show_ascription_steps,
        setting: Evaluation(ShowAscriptionSteps),
        tooltip: Some("Show type ascription steps in the stepper"),
      },
      {
        name: "Show Case Steps",
        active: s.show_case_steps,
        setting: Evaluation(ShowCaseSteps),
        tooltip: Some("Show case expression steps in the stepper"),
      },
      {
        name: "Proof Steps (experimental)",
        active: s.enable_proof,
        setting: Evaluation(EnableProof),
        tooltip: Some("Enable proof-based stepping mode (experimental)"),
      },
    ],
  );
};

let dev_group = (~globals: Globals.t) => {
  settings_group(
    ~globals,
    "Developer",
    [
      {
        name: "Benchmarks",
        active: globals.settings.benchmark,
        setting: Settings.Update.Benchmark,
        tooltip: Some("Display performance benchmarks"),
      },
      {
        name: "Elaboration",
        active: globals.settings.core.elaborate,
        setting: Elaborate,
        tooltip: Some("Show elaborated (internal) expressions"),
      },
      {
        name: "Probe All",
        active: globals.settings.core.probe_all,
        setting: ProbeAll,
        tooltip: Some("Enable probes on all top-level definitions"),
      },
      {
        name: "Deep Reassociate",
        active: globals.settings.core.deep_reassociate,
        setting: DeepReassociate,
        tooltip: Some("Enable deep reassociation of syntax"),
      },
      {
        name: "Character-level mouse",
        active: globals.settings.core.selection_chunkiness,
        setting: SelectionChunkiness,
        tooltip:
          Some(
            "When on, mouse drag selects by character. When off (default), mouse drag selects by character inside a token and by whole token beyond; holding Alt (Mac) / Ctrl (PC) while dragging does the reverse. Keyboard Shift+Arrow is always character-level (hold Alt/Ctrl for whole-token).",
          ),
      },
      {
        name: "Cap Undo Stack",
        active: globals.settings.cap_undo_stack,
        setting: CapUndoStack,
        tooltip: Some("Cap the undo history stack size"),
      },
      {
        name: "Ruled Lines",
        active: globals.settings.show_row_lines,
        setting: ShowRowLines,
        tooltip: Some("Show horizontal lines between each row of code"),
      },
      {
        name: "Incremental Reuse",
        active: globals.settings.show_incremental_deco,
        setting: ShowIncrementalDeco,
        tooltip: Some("Show incremental evaluator cache hits"),
      },
      {
        name: "Debug Sidebar",
        active: globals.settings.show_debug_panel,
        setting: ShowDebugPanel,
        tooltip: Some("Show the debug info sidebar panel"),
      },
    ]
    @ (
      ExerciseSettings.show_instructor
        ? [
          {
            name: "Log Panel",
            active: globals.settings.show_log_panel,
            setting: ShowLogPanel,
            tooltip: Some("Show the debug log panel"),
          },
        ]
        : []
    ),
  );
};

let code_display_group = (~globals: Globals.t) => {
  settings_group(
    ~globals,
    "Code Display",
    [
      {
        name: "Whitespace",
        active: globals.settings.secondary_icons,
        setting: Settings.Update.SecondaryIcons,
        tooltip: Some("Show whitespace indicator icons"),
      },
      {
        name: "Animations",
        active: globals.settings.core.flip_animations,
        setting: FlipAnimations,
        tooltip: Some("Enable flip animations for code changes"),
      },
      {
        name: "Line Numbers",
        active: globals.settings.line_numbers,
        setting: ToggleLineNumbers,
        tooltip: None,
      },
    ]
    @ (
      globals.settings.line_numbers
        ? [
          {
            name: "Relative Numbers",
            active: globals.settings.relative_line_numbers,
            setting: ToggleRelativeLineNumbers,
            tooltip: Some("Show line numbers relative to cursor position"),
          },
        ]
        : []
    ),
  );
};

//("l", "Line Numbers", globals.settings.line_numbers, ToggleLineNumbers)
let settings_menu = (~globals) => {
  [
    semantics_group(~globals),
    values_group(~globals),
    stepper_group(~globals),
    code_display_group(~globals),
    dev_group(~globals),
  ];
};
