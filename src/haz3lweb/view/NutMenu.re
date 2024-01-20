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

let settings_menu =
    (~inject, ~settings as {core, benchmark, secondary_icons, _}: Settings.t) => {
  let toggle = (icon, tooltip, bool, setting) =>
    toggle_named(icon, ~tooltip, bool, _ =>
      inject(UpdateAction.Set(setting))
    );
  [
    toggle("τ", "Toggle Statics", core.statics, Statics),
    toggle("⇲", "Toggle Completion", core.assist, Assist),
    toggle("𝛿", "Toggle Dynamics", core.dynamics, Dynamics),
    toggle("𝑒", "Show Elaboration", core.elaborate, Elaborate),
    toggle("↵", "Show Whitespace", secondary_icons, SecondaryIcons),
    toggle("✓", "Print Benchmarks", benchmark, Benchmark),
  ];
};

let export_menu = (~inject, ~settings: Settings.t, editors: Editors.t) =>
  switch (editors) {
  | DebugLoad => []
  | Scratch(slide_idx, slides) =>
    let state = List.nth(slides, slide_idx);
    [ScratchMode.export_button(state)];
  | Examples(name, slides) =>
    let state = List.assoc(name, slides);
    [ScratchMode.export_button(state)];
  | Exercise(_, _, exercise) when settings.instructor_mode => [
      export_persistent_data(~inject),
      ExerciseMode.export_submission(~settings),
      ExerciseMode.instructor_export(exercise),
      ExerciseMode.instructor_transitionary_export(exercise),
      ExerciseMode.instructor_grading_export(exercise),
    ]
  | Exercise(_) => [ExerciseMode.export_submission(~settings)]
  };

let import_menu = (~inject, editors: Editors.t) =>
  switch (editors) {
  | DebugLoad => []
  | Scratch(_)
  | Examples(_) => [
      ScratchMode.import_button(inject),
      ScratchMode.reset_button(inject),
    ]
  | Exercise(_) => [
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

let view = (~inject: Update.t => 'a, {settings, editors, _}: Model.t) => [
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
