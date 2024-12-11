open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Widgets;
open Haz3lcore;

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
  let toggle = ((_icon, tooltip, bool, setting)) =>
    toggle_named("", ~tooltip, bool, _ =>
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
      ("τ", "Types", globals.settings.core.statics, Statics),
      ("⇲", "Completion", globals.settings.core.assist, Assist),
      ("𝛿", "Evaluation", globals.settings.core.dynamics, Dynamics),
      (
        "?",
        "Docs",
        globals.settings.explainThis.show,
        ExplainThis(ToggleShow),
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
      ("λ", "Functions", s.show_fn_bodies, Evaluation(ShowFnBodies)),
      ("|", "Cases", s.show_case_clauses, Evaluation(ShowCaseClauses)),
      ("f", "Fixpoints", s.show_fixpoints, Evaluation(ShowFixpoints)),
      (Unicode.castArrowSym, "Casts", s.show_casts, Evaluation(ShowCasts)),
    ],
  );
};

let stepper_group = (~globals: Globals.t) => {
  let s = globals.settings.core.evaluation;
  settings_group(
    ~globals,
    "Stepper",
    [
      ("🔍", "Show lookups", s.show_lookup_steps, Evaluation(ShowLookups)),
      (
        "🤫",
        "Show hidden",
        s.show_hidden_steps,
        Evaluation(ShowHiddenSteps),
      ),
      ("⏯️", "Filters", s.show_stepper_filters, Evaluation(ShowFilters)),
    ],
  );
};

let dev_group = (~globals) => {
  settings_group(
    ~globals,
    "Developer",
    [
      ("✓", "Benchmarks", globals.settings.benchmark, Benchmark),
      ("𝑒", "Elaboration", globals.settings.core.elaborate, Elaborate),
      ("↵", "Whitespace", globals.settings.secondary_icons, SecondaryIcons),
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
