open Virtual_dom.Vdom;
open Js_of_ocaml;
open Node;
open Util.Web;
open Widgets;

let export_persistent_data = (~inject: Update.t => 'a) =>
  button_named(
    Icons.sprout,
    _ => inject(ExportPersistentData),
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
        DebugAction.perform(DebugAction.ClearStore);
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
        {core: {evaluation, _} as core, benchmark, secondary_icons, _}: Settings.t,
    ) => {
  let toggle = (icon, tooltip, bool, setting) =>
    toggle_named(icon, ~tooltip, bool, _ =>
      inject(UpdateAction.Set(setting))
    );
  [
    toggle("⇲", "Toggle Completion", core.assist, Assist),
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
    toggle("↵", "Show Whitespace", secondary_icons, SecondaryIcons),
    toggle("τ", "Toggle Statics", core.statics, Statics),
    toggle("𝛿", "Toggle Dynamics", core.dynamics, Dynamics),
    toggle("✓", "Print Benchmarks", benchmark, Benchmark),
  ];
};

let export_menu = (~inject, ~settings: Settings.t, editors: Editors.t) =>
  switch (editors) {
  | DebugLoad => []
  | Scratch(slide_idx, slides) =>
    let state = List.nth(slides, slide_idx);
    [ScratchMode.export_button(state)];
  | Documentation(name, slides) =>
    let state = List.assoc(name, slides);
    [ScratchMode.export_button(state)];
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
  | DebugLoad => []
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
    ~attr=clss(["top-menu-item"]),
    [
      div(
        ~attr=Attr.many([clss(["submenu-icon"]), Attr.title(tooltip)]),
        [div(~attr=clss(["icon"]), [icon])],
      ),
      div(~attr=clss(["submenu"]), menu),
    ],
  );

let view =
    (~inject: Update.t => 'a, ~settings: Settings.t, ~editors: Editors.t) => [
  a(~attr=clss(["nut-icon"]), [Icons.hazelnut]),
  div(
    ~attr=clss(["nut-menu"]),
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
