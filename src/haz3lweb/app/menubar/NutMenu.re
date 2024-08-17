open Util;
open Virtual_dom.Vdom;
open Js_of_ocaml;
open Node;
open Util.Web;
open Widgets;
open Haz3lcore;

let settings_group = (~inject, name: string, ts) => {
  let toggle = ((_icon, tooltip, bool, setting)) =>
    toggle_named("", ~tooltip, bool, _ => inject(UpdateAction.Set(setting)));
  div_c(
    "group",
    [
      div_c("name", [text(name)]),
      div_c("contents", List.map(toggle, ts)),
    ],
  );
};

let semantics_group = (~inject, ~settings: Settings.t) => {
  settings_group(
    ~inject,
    "Semantics",
    [
      ("τ", "Types", settings.core.statics, Statics),
      ("⇲", "Completion", settings.core.assist, Assist),
      ("𝛿", "Evaluation", settings.core.dynamics, Dynamics),
      ("?", "Docs", settings.explainThis.show, ExplainThis(ToggleShow)),
      // (
      //   "👍",
      //   "Feedback",
      //   settings.explainThis.show_feedback,
      //   ExplainThis(ToggleShowFeedback),
      // ),
    ],
  );
};

let values_group = (~inject, ~settings: Settings.t) => {
  let s = settings.core.evaluation;
  settings_group(
    ~inject,
    "Value Display",
    [
      ("λ", "Functions", s.show_fn_bodies, Evaluation(ShowFnBodies)),
      ("|", "Cases", s.show_case_clauses, Evaluation(ShowCaseClauses)),
      ("f", "Fixpoints", s.show_fixpoints, Evaluation(ShowFixpoints)),
      (Unicode.castArrowSym, "Casts", s.show_casts, Evaluation(ShowCasts)),
    ],
  );
};

let stepper_group = (~inject, ~settings: Settings.t) => {
  let s = settings.core.evaluation;
  settings_group(
    ~inject,
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

let dev_group = (~inject, ~settings: Settings.t) => {
  settings_group(
    ~inject,
    "Developer",
    [
      ("✓", "Benchmarks", settings.benchmark, Benchmark),
      ("𝑒", "Elaboration", settings.core.elaborate, Elaborate),
      ("↵", "Whitespace", settings.secondary_icons, SecondaryIcons),
    ],
  );
};

let settings_menu = (~inject, ~settings: Settings.t) => {
  [
    semantics_group(~inject, ~settings),
    values_group(~inject, ~settings),
    stepper_group(~inject, ~settings),
    dev_group(~inject, ~settings),
  ];
};

let export_persistent_data = (~globals: Globals.t) =>
  button_named(
    Icons.export,
    _ => inject(Export(ExportPersistentData)),
    ~tooltip="Export All Persistent Data",
  );

let reset_hazel =
  button_named(
    Icons.bomb,
    _ => {
      let confirmed =
        JsUtil.confirm(
          "Are you SURE you want to reset Hazel to its initial state? You will lose any existing code that you have written, and course staff have no way to restore it!",
        );
      if (confirmed) {
        JsUtil.clear_localstore();
        Dom_html.window##.location##reload;
      };
      Virtual_dom.Vdom.Effect.Ignore;
    },
    ~tooltip="Reset Hazel (LOSE ALL DATA)",
  );

let reparse = (~inject: Update.t => 'a) =>
  button_named(
    Icons.backpack,
    _ => inject(PerformAction(Reparse)),
    ~tooltip="Reparse Editor",
  );

let item_group = (~inject as _, name: string, ts) => {
  div_c("group", [div_c("name", [text(name)]), div_c("contents", ts)]);
};

let file_group_scratch = (~inject) =>
  item_group(
    ~inject,
    "File",
    [ScratchMode.export_button(inject), ScratchMode.import_button(inject)],
  );

let reset_group_scratch = (~inject) =>
  item_group(
    ~inject,
    "Reset",
    [ScratchMode.reset_button(inject), reparse(~inject), reset_hazel],
  );

let file_group_exercises = (~inject) =>
  item_group(
    ~inject,
    "File",
    [
      ExerciseMode.export_submission(inject),
      ExerciseMode.import_submission(~inject),
    ],
  );

let reset_group_exercises = (~inject) =>
  item_group(
    ~inject,
    "Reset",
    [ExerciseMode.reset_button(inject), reparse(~inject), reset_hazel],
  );

let dev_group_exercises = (~inject) =>
  item_group(
    ~inject,
    "Developer Export",
    [
      export_persistent_data(~inject),
      ExerciseMode.instructor_export(inject),
      ExerciseMode.instructor_transitionary_export(inject),
      ExerciseMode.instructor_grading_export(inject),
    ],
  );

let file_menu = (~inject, ~settings: Settings.t, editors: Editors.t) =>
  switch (editors) {
  | Scratch(_) => [
      file_group_scratch(~inject),
      reset_group_scratch(~inject),
    ]
  | Documentation(_) => [
      file_group_scratch(~inject),
      reset_group_scratch(~inject),
    ]
  | Exercises(_) when settings.instructor_mode => [
      file_group_exercises(~inject),
      reset_group_exercises(~inject),
      dev_group_exercises(~inject),
    ]
  | Exercises(_) => [
      file_group_exercises(~inject),
      reset_group_exercises(~inject),
    ]
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

let view =
    (
      ~globals: Globals.t,
      ~selection: option(Editors.Selection.t),
      ~inject: Editors.Update.t => 'a,
      ~editors: Editors.Model.t,
    ) =>
  div(
    ~attrs=[clss(["nut-menu"])],
    [
      submenu(~tooltip="Settings", ~icon=Icons.gear, settings_menu(~globals)),
      submenu(
        ~tooltip="File",
        ~icon=Icons.disk,
        file_menu(~inject, ~settings, editors),
      ),
      button(
        Icons.command_palette_sparkle,
        _ => {
          NinjaKeys.open_command_palette();
          Effect.Ignore;
        },
        ~tooltip=
          "Command Palette ("
          ++ Keyboard.meta(Os.is_mac^ ? Mac : PC)
          ++ " + k)",
      ),
      link(
        Icons.github,
        "https://github.com/hazelgrove/hazel",
        ~tooltip="Hazel on GitHub",
      ),
      link(Icons.info, "https://hazel.org", ~tooltip="Hazel Homepage"),
    ],
  );
