open Virtual_dom.Vdom;
open Js_of_ocaml;
open Node;
open Util.Web;
open Haz3lcore;
open Widgets;

let download_editor_state = (~instructor_mode) => {
  let data = Export.export_all(~instructor_mode);
  JsUtil.download_json(ExerciseSettings.filename, data);
};
let menu_icon = {
  let attr =
    Attr.many(
      Attr.[
        clss(["menu-icon"]),
        href("https://hazel.org"),
        title("Hazel"),
        create("target", "_blank"),
      ],
    );
  a(~attr, [Icons.hazelnut]);
};

let history_bar = (ed: Editor.t, ~inject: Update.t => 'a) => [
  button_d(
    Icons.undo,
    inject(Undo),
    ~disabled=!Editor.can_undo(ed),
    ~tooltip="Undo",
  ),
  button_d(
    Icons.redo,
    inject(Redo),
    ~disabled=!Editor.can_redo(ed),
    ~tooltip="Redo",
  ),
];

let nut_menu =
    (
      ~inject: Update.t => 'a,
      {statics, dynamics, benchmark, instructor_mode, _}: ModelSettings.t,
    ) => [
  menu_icon,
  div(
    ~attr=clss(["menu"]),
    [
      submenu(
        toggle("τ", statics, _ => inject(Set(Statics))),
        [submenu_label("Toggle Statics")],
      ),
      submenu(
        toggle("𝛿", ~tooltip="Toggle Dynamics", dynamics, _ =>
          inject(Set(Dynamics))
        ),
        [submenu_label("Toggle Dynamics")],
      ),
      submenu(
        toggle("b", ~tooltip="Toggle Performance Benchmark", benchmark, _ =>
          inject(Set(Benchmark))
        ),
        [submenu_label("Toggle Performance Benchmark")],
      ),
      submenu(
        button(
          Icons.export,
          _ => {
            download_editor_state(~instructor_mode);
            Virtual_dom.Vdom.Effect.Ignore;
          },
        ),
        [submenu_label("Export Submission")],
      ),
      submenu(
        file_select_button("import-submission", Icons.import, file => {
          switch (file) {
          | None => Virtual_dom.Vdom.Effect.Ignore
          | Some(file) => inject(InitImportAll(file))
          }
        }),
        [submenu_label("Import Submission")],
      ),
      submenu(
        button(Icons.eye, _ => inject(Set(SecondaryIcons))),
        [submenu_label("Toggle Visible Secondary")],
      ),
      submenu(
        link(Icons.github, "https://github.com/hazelgrove/hazel"),
        [submenu_label("Hazel on Github")],
      ),
    ],
  ),
];

let top_bar_view =
    (
      ~inject: Update.t => 'a,
      ~toolbar_buttons: list(Node.t),
      ~model as {editors, settings, _}: Model.t,
    ) =>
  div(
    ~attr=Attr.id("top-bar"),
    nut_menu(~inject, settings)
    @ [div(~attr=Attr.id("title"), [text("HAZEL")])]
    @ [EditorModeView.view(~inject, ~settings, ~editors)]
    @ history_bar(Editors.get_editor(editors), ~inject)
    @ toolbar_buttons,
  );

let exercises_view =
    (
      ~inject,
      ~exercise,
      {
        editors,
        font_metrics,
        show_backpack_targets,
        settings,
        mousedown,
        results,
        langDocMessages,
        _,
      } as model: Model.t,
    ) => {
  let exercise_mode =
    ExerciseMode.mk(
      ~settings,
      ~exercise,
      ~results=settings.dynamics ? Some(results) : None,
      ~langDocMessages,
    );
  let toolbar_buttons =
    ExerciseMode.toolbar_buttons(~inject, ~settings, editors)
    @ [
      Grading.GradingReport.view_overall_score(exercise_mode.grading_report),
    ];
  [top_bar_view(~inject, ~model, ~toolbar_buttons)]
  @ ExerciseMode.view(
      ~inject,
      ~font_metrics,
      ~mousedown,
      ~show_backpack_targets,
      exercise_mode,
    );
};

let slide_view = (~inject, ~model, slide_state) => {
  let toolbar_buttons = ScratchMode.toolbar_buttons(~inject, slide_state);
  [top_bar_view(~inject, ~toolbar_buttons, ~model)]
  @ ScratchMode.view(~inject, ~model);
};

let editors_view = (~inject, model: Model.t) => {
  switch (model.editors) {
  | DebugLoad => [DebugMode.view(~inject)]
  | Scratch(slide_idx, slides) =>
    slide_view(~inject, ~model, List.nth(slides, slide_idx))
  | Examples(name, slides) =>
    slide_view(~inject, ~model, List.assoc(name, slides))
  | Exercise(_, _, exercise) => exercises_view(~inject, ~exercise, model)
  };
};

let get_selection = (model: Model.t): string =>
  model.editors |> Editors.get_editor |> Printer.to_string_selection;

let view = (~inject, ~handlers, model: Model.t) =>
  div(
    ~attr=
      Attr.many(
        Attr.[
          id("page"),
          // safety handler in case mousedown overlay doesn't catch it
          on_mouseup(_ => inject(Update.Mouseup)),
          on_blur(_ => {
            JsUtil.focus_clipboard_shim();
            Virtual_dom.Vdom.Effect.Ignore;
          }),
          on_focus(_ => {
            JsUtil.focus_clipboard_shim();
            Virtual_dom.Vdom.Effect.Ignore;
          }),
          on_copy(_ => {
            JsUtil.copy(get_selection(model));
            Virtual_dom.Vdom.Effect.Ignore;
          }),
          on_cut(_ => {
            JsUtil.copy(get_selection(model));
            inject(UpdateAction.PerformAction(Destruct(Left)));
          }),
          on_paste(evt => {
            let pasted_text =
              Js.to_string(evt##.clipboardData##getData(Js.string("text")))
              |> Str.global_replace(Str.regexp("\n[ ]*"), "\n");
            Dom.preventDefault(evt);
            inject(UpdateAction.Paste(pasted_text));
          }),
          ...handlers(~inject, ~model),
        ],
      ),
    [
      FontSpecimen.view("font-specimen"),
      DecUtil.filters,
      JsUtil.clipboard_shim,
    ]
    @ editors_view(~inject, model),
  );
