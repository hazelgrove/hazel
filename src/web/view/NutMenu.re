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
        tooltip: None,
      },
      {
        name: "Completion",
        active: globals.settings.core.assist,
        setting: Assist,
        tooltip: None,
      },
      {
        name: "Evaluation",
        active: globals.settings.core.dynamics,
        setting: Dynamics,
        tooltip: None,
      },
      {
        name: "Docs",
        active: globals.settings.sidebar.show,
        setting: Sidebar(ToggleShow),
        tooltip: None,
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
        tooltip: None,
      },
      {
        name: "Cases",
        active: s.show_case_clauses,
        setting: Evaluation(ShowCaseClauses),
        tooltip: None,
      },
      {
        name: "Fixpoints",
        active: s.show_fixpoints,
        setting: Evaluation(ShowFixpoints),
        tooltip: None,
      },
      {
        name: "Ascriptions",
        active: s.show_ascriptions,
        setting: Evaluation(ShowAscriptions),
        tooltip: None,
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
        tooltip: None,
      },
      {
        name: "Show hidden",
        active: s.show_hidden_steps,
        setting: Evaluation(ShowHiddenSteps),
        tooltip: None,
      },
      {
        name: "Show filters",
        active: s.show_stepper_filters,
        setting: Evaluation(ShowFilters),
        tooltip: None,
      },
      {
        name: "Show Ascription Steps",
        active: s.show_ascription_steps,
        setting: Evaluation(ShowAscriptionSteps),
        tooltip: None,
      },
      {
        name: "Show Case Steps",
        active: s.show_case_steps,
        setting: Evaluation(ShowCaseSteps),
        tooltip: None,
      },
      {
        name: "Proof Steps (experimental)",
        active: s.enable_proof,
        setting: Evaluation(EnableProof),
        tooltip: None,
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
        tooltip: None,
      },
      {
        name: "Elaboration",
        active: globals.settings.core.elaborate,
        setting: Elaborate,
        tooltip: None,
      },
      {
        name: "Ruled Lines",
        active: globals.settings.show_row_lines,
        setting: ShowRowLines,
        tooltip: Some("Show horizontal lines between each row of code"),
      },
    ]
    @ (
      ExerciseSettings.show_instructor
        ? [
          {
            name: "Log Panel",
            active: globals.settings.show_log_panel,
            setting: ShowLogPanel,
            tooltip: None,
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
        tooltip: None,
      },
      {
        name: "Animations",
        active: globals.settings.core.flip_animations,
        setting: FlipAnimations,
        tooltip: None,
      },
      {
        name: "Line Numbers",
        active: globals.settings.line_numbers,
        setting: ToggleLineNumbers,
        tooltip: None,
      },
      {
        name: "Relative Numbers",
        active: globals.settings.relative_line_numbers,
        setting: ToggleRelativeLineNumbers,
        tooltip: None,
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
            tooltip: None,
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
    dev_group(~globals),
    code_display_group(~globals),
  ];
};
