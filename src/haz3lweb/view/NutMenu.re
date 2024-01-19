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

let reset_hazel_button =
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
        Virtual_dom.Vdom.Effect.Ignore;
      } else {
        Virtual_dom.Vdom.Effect.Ignore;
      };
    },
    ~tooltip="Clear Local Storage and Reload (LOSE ALL DATA)",
  );

let settings_menu =
    (
      ~inject: Update.t => 'a,
      {
        core: {statics, elaborate, assist, dynamics},
        benchmark,
        secondary_icons,
        _,
      }: Settings.t,
    ) => {
  let set = (icon, tooltip, bool, setting) =>
    toggle_named(icon, ~tooltip, bool, _ =>
      inject(UpdateAction.Set(setting))
    );
  div(
    ~attr=clss(["submenu", "settings"]),
    [
      set("τ", "Toggle Statics", statics, Statics),
      set("⇲", "Toggle Completion", assist, Assist),
      set("𝛿", "Toggle Dynamics", dynamics, Dynamics),
      set("𝑒", "Show Elaboration", elaborate, Elaborate),
      set("✓", "Toggle Benchmarking", benchmark, Benchmark),
      set("↵", "Show Spaces", secondary_icons, SecondaryIcons),
    ],
  );
};

let export_menu = (~inject, ~settings: Settings.t, editors: Editors.t) =>
  div(
    ~attr=clss(["submenu", "export"]),
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
    },
  );

let import_menu = (~inject, editors: Editors.t) =>
  div(
    ~attr=clss(["submenu", "export"]),
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
    },
  );

let submenu = (~tooltip, ~icon, menu) =>
  div(
    ~attr=clss(["top-menu-item"]),
    [
      div(
        ~attr=Attr.many([clss(["menu-item-iconic"]), Attr.title(tooltip)]),
        [div(~attr=Attr.many([clss(["icon"])]), [icon])],
      ),
      menu,
    ],
  );

let view = (~inject: Update.t => 'a, {settings, editors, _}: Model.t) => [
  a(~attr=clss(["menu-icon"]), [Icons.hazelnut]),
  div(
    ~attr=clss(["menu"]),
    [
      submenu(
        ~tooltip="Settings",
        ~icon=Icons.gear,
        settings_menu(~inject, settings),
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
      reset_hazel_button,
      link(
        Icons.github,
        "https://github.com/hazelgrove/hazel",
        ~tooltip="Hazel on GitHub",
      ),
      link(Icons.info, "https://hazel.org", ~tooltip="Hazel Homepage"),
    ],
  ),
];
