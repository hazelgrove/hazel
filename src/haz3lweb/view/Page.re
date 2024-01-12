open Virtual_dom.Vdom;
open Js_of_ocaml;
open Node;
open Util.Web;
open Haz3lcore;
open Widgets;

let download_editor_state = (~instructor_mode) =>
  Log.get_and(log => {
    let data = Export.export_all(~instructor_mode, ~log);
    JsUtil.download_json(ExerciseSettings.filename, data);
  });

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
      {
        core: {statics, elaborate, assist, dynamics, inference},
        benchmark,
        instructor_mode,
        _,
      }: Settings.t,
    ) => [
  menu_icon,
  div(
    ~attr=clss(["menu"]),
    [
      toggle("τ", ~tooltip="Toggle Statics", statics, _ =>
        inject(Set(Statics))
      ),
      toggle("𝑐", ~tooltip="Code Completion", assist, _ =>
        inject(Set(Assist))
      ),
      toggle("𝛿", ~tooltip="Toggle Dynamics", dynamics, _ =>
        inject(Set(Dynamics))
      ),
      toggle("𝑒", ~tooltip="Show Elaboration", elaborate, _ =>
        inject(Set(Elaborate))
      ),
      toggle("∪", ~tooltip="Toggle Inference", inference, _ =>
        inject(Set(Inference))
      ),
      toggle("b", ~tooltip="Toggle Performance Benchmark", benchmark, _ =>
        inject(Set(Benchmark))
      ),
      button(
        Icons.export,
        _ => {
          download_editor_state(~instructor_mode);
          Virtual_dom.Vdom.Effect.Ignore;
        },
        ~tooltip="Export Submission",
      ),
      file_select_button(
        "import-submission",
        Icons.import,
        file => {
          switch (file) {
          | None => Virtual_dom.Vdom.Effect.Ignore
          | Some(file) => inject(InitImportAll(file))
          }
        },
        ~tooltip="Import Submission",
      ),
      button(
        Icons.eye,
        _ => inject(Set(SecondaryIcons)),
        ~tooltip="Toggle Visible Secondary",
      ),
      link(
        Icons.github,
        "https://github.com/hazelgrove/hazel",
        ~tooltip="Hazel on GitHub",
      ),
    ]
    @ (
      instructor_mode
        ? [
          button(
            Icons.sprout,
            _ => inject(ExportPersistentData),
            ~tooltip="Export Persistent Data",
          ),
        ]
        : []
    ),
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
    @ [div(~attr=Attr.id("title"), [text("hazel")])]
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
        settings,
        langDocMessages,
        meta: {
          ui_state: {font_metrics, show_backpack_targets, mousedown, _},
          results,
          _,
        },
        _,
      } as model: Model.t,
    ) => {
  let exercise_mode =
    ExerciseMode.mk(
      ~settings,
      ~exercise,
      ~results=settings.core.dynamics ? Some(results) : None,
      ~langDocMessages,
    );
  let toolbar_buttons =
    ExerciseMode.toolbar_buttons(~inject, ~settings, editors)
    @ [
      Grading.GradingReport.view_overall_score(exercise_mode.grading_report),
    ];
  let zipper = Editors.get_editor(editors).state.zipper;
  let unselected = Zipper.unselect_and_zip(zipper);
  let (term, _) = MakeTerm.go(unselected);
  let (_, suggestions) = Statics.mk_map_and_inference_solutions(term);
  let global_inference_info =
    InferenceResult.mk_global_inference_info(
      model.settings.core.inference,
      suggestions,
    );
  [top_bar_view(~inject, ~model, ~toolbar_buttons)]
  @ ExerciseMode.view(
      ~inject,
      ~font_metrics,
      ~mousedown,
      ~show_backpack_targets,
      exercise_mode,
      ~global_inference_info,
    );
};

let slide_view = (~inject, ~model, ~ctx_init, slide_state) => {
  let toolbar_buttons = ScratchMode.toolbar_buttons(~inject, slide_state);
  [top_bar_view(~inject, ~toolbar_buttons, ~model)]
  @ ScratchMode.view(~inject, ~model, ~ctx_init);
};

let editors_view = (~inject, model: Model.t) => {
  let ctx_init =
    Editors.get_ctx_init(~settings=model.settings, model.editors);
  switch (model.editors) {
  | DebugLoad => [DebugMode.view(~inject)]
  | Scratch(slide_idx, slides) =>
    slide_view(~inject, ~model, ~ctx_init, List.nth(slides, slide_idx))
  | Examples(name, slides) =>
    slide_view(~inject, ~model, ~ctx_init, List.assoc(name, slides))
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
          on_mouseup(_ => inject(Update.SetMeta(Mouseup))),
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
          ...handlers(~inject),
        ],
      ),
    [
      FontSpecimen.view("font-specimen"),
      DecUtil.filters,
      JsUtil.clipboard_shim,
    ]
    @ editors_view(~inject, model),
  );
