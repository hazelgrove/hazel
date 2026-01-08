open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Widgets;

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
  let toggle = ((_icon, tooltip, bool, setting, warning: option(string))) =>
    toggle_named("", ~tooltip, ~warning?, bool, _ =>
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
      ("τ", "Types", globals.settings.core.statics, Statics, None),
      ("⇲", "Completion", globals.settings.core.assist, Assist, None),
      ("𝛿", "Evaluation", globals.settings.core.dynamics, Dynamics, None),
      (
        "?",
        "Docs",
        globals.settings.sidebar.show,
        Sidebar(ToggleShow),
        None,
      ),
      (
        "🔄",
        "Live Typing",
        globals.settings.core.live_typing,
        LiveTyping,
        Some("May slow down editor performance"),
      ),
      // (
      //   "👍",
      //   "Feedback",
      //   settings.explainThis.show_feedback,
      //   ExplainThis(ToggleShowFeedback),
      // ),
    ],
  );
};

let values_group = (~globals: Globals.t) => {
  let s = globals.settings.core.evaluation;
  settings_group(
    ~globals,
    "Value Display",
    [
      ("λ", "Functions", s.show_fn_bodies, Evaluation(ShowFnBodies), None),
      ("|", "Cases", s.show_case_clauses, Evaluation(ShowCaseClauses), None),
      ("f", "Fixpoints", s.show_fixpoints, Evaluation(ShowFixpoints), None),
      ("☰", "Tables", s.project_tables, Evaluation(ProjectTables), None),
    ],
  );
};

let stepper_group = (~globals: Globals.t) => {
  let s = globals.settings.core.evaluation;
  settings_group(
    ~globals,
    "Stepper",
    [
      (
        "🔍",
        "Show lookups",
        s.show_lookup_steps,
        Evaluation(ShowLookups),
        None,
      ),
      (
        "🤫",
        "Show hidden",
        s.show_hidden_steps,
        Evaluation(ShowHiddenSteps),
        None,
      ),
      (
        "⏯️",
        "Show filters",
        s.show_stepper_filters,
        Evaluation(ShowFilters),
        None,
      ),
      (
        "⇨",
        "Show Ascription Steps",
        s.show_ascription_steps,
        Evaluation(ShowAscriptionSteps),
        None,
      ),
      (
        "π",
        "Proof Steps (experimental)",
        s.enable_proof,
        Evaluation(EnableProof),
        None,
      ),
    ],
  );
};

let dev_group = (~globals) => {
  settings_group(
    ~globals,
    "Developer",
    [
      ("✓", "Benchmarks", globals.settings.benchmark, Benchmark, None),
      (
        "𝑒",
        "Elaboration",
        globals.settings.core.elaborate,
        Elaborate,
        None,
      ),
      (
        "↵",
        "Whitespace",
        globals.settings.secondary_icons,
        SecondaryIcons,
        None,
      ),
      (
        "a",
        "Animations",
        globals.settings.core.flip_animations,
        FlipAnimations,
        None,
      ),
    ],
  );
};

let settings_menu = (~globals) => {
  [
    semantics_group(~globals),
    values_group(~globals),
    stepper_group(~globals),
    dev_group(~globals),
  ];
};
