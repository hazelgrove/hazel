open Virtual_dom.Vdom;
open Js_of_ocaml;
open Node;
open Util.Web;
open Widgets;

let export_persistent_data = (~inject: Update.t => 'a) =>
  button_named(
    Icons.sprout,
    _ => inject(Export(ExportPersistentData)),
    ~tooltip="Export All Persistent Data",
  );

let reset_hazel =
  button(
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
    ~tooltip="Clear Local Storage and Reload (LOSE ALL DATA)",
  );

let reparse = (~inject: Update.t => 'a) =>
  button(
    Icons.backpack,
    _ => inject(ReparseCurrentEditor),
    ~tooltip="Reparse Current Editor",
  );

let settings_menu =
    (
      ~inject,
      ~settings as
        {
          core: {evaluation, _} as core,
          benchmark,
          secondary_icons,
          explainThis,
          _,
        }: Settings.t,
    ) => {
  let toggle = (icon, tooltip, bool, setting) =>
    toggle_named(icon, ~tooltip, bool, _ =>
      inject(UpdateAction.Set(setting))
    );
  [
    toggle("τ", "Toggle Statics", core.statics, Statics),
    toggle("⇲", "Toggle Completion", core.assist, Assist),
    toggle("↵", "Show Whitespace", secondary_icons, SecondaryIcons),
    toggle("✓", "Print Benchmarks", benchmark, Benchmark),
    toggle("𝛿", "Toggle Dynamics", core.dynamics, Dynamics),
    toggle("𝑒", "Show Elaboration", core.elaborate, Elaborate),
    toggle(
      "λ",
      "Show Function Bodies",
      evaluation.show_fn_bodies,
      Evaluation(ShowFnBodies),
    ),
    toggle(
      "|",
      "Show Case Clauses",
      evaluation.show_case_clauses,
      Evaluation(ShowCaseClauses),
    ),
    toggle(
      "f",
      "Show fixpoints",
      evaluation.show_fixpoints,
      Evaluation(ShowFixpoints),
    ),
    toggle(
      Unicode.castArrowSym,
      "Show casts",
      evaluation.show_casts,
      Evaluation(ShowCasts),
    ),
    toggle(
      "🔍",
      "Show Lookup Steps",
      evaluation.show_lookup_steps,
      Evaluation(ShowLookups),
    ),
    toggle(
      "⏯️",
      "Show Stepper Filters",
      evaluation.show_stepper_filters,
      Evaluation(ShowFilters),
    ),
    toggle(
      "🤫",
      "Show Hidden Steps",
      evaluation.show_hidden_steps,
      Evaluation(ShowHiddenSteps),
    ),
    toggle(
      "?",
      "Show Docs Sidebar",
      explainThis.show,
      ExplainThis(ToggleShow),
    ),
    toggle(
      "👍",
      "Show Docs Feedback",
      explainThis.show_feedback,
      ExplainThis(ToggleShowFeedback),
    ),
  ];
};

let export_menu = (~inject, ~settings: Settings.t, editors: Editors.t) =>
  switch (editors) {
  | Scratch(_) => [ScratchMode.export_button(inject)]
  | Documentation(_) => [ScratchMode.export_button(inject)]
  | Exercises(_, _, exercise) when settings.instructor_mode => [
      export_persistent_data(~inject),
      ExerciseMode.export_submission(~settings),
      ExerciseMode.instructor_export(exercise),
      ExerciseMode.instructor_transitionary_export(exercise),
      ExerciseMode.instructor_grading_export(exercise),
    ]
  | Exercises(_) => [ExerciseMode.export_submission(~settings)]
  };

let import_menu = (~inject, editors: Editors.t) =>
  switch (editors) {
  | Scratch(_)
  | Documentation(_) => [
      ScratchMode.import_button(inject),
      ScratchMode.reset_button(inject),
    ]
  | Exercises(_) => [
      ExerciseMode.import_submission(~inject),
      ExerciseMode.reset_button(inject),
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
    (~inject: Update.t => 'a, ~settings: Settings.t, ~editors: Editors.t) => [
  a(~attrs=[clss(["nut-icon"])], [Icons.hazelnut]),
  div(
    ~attrs=[clss(["nut-menu"])],
    [
      submenu(
        ~tooltip="Settings",
        ~icon=Icons.gear,
        settings_menu(~inject, ~settings),
      ),
      submenu(
        ~tooltip="Export",
        ~icon=Icons.export,
        export_menu(~inject, ~settings, editors),
      ),
      submenu(
        ~tooltip="Import",
        ~icon=Icons.import,
        import_menu(~inject, editors),
      ),
      reparse(~inject),
      reset_hazel,
      link(
        Icons.github,
        "https://github.com/hazelgrove/hazel",
        ~tooltip="Hazel on GitHub",
      ),
      link(Icons.info, "https://hazel.org", ~tooltip="Hazel Homepage"),
    ],
  ),
];
