open Util;
open Js_of_ocaml;
open Haz3lcore;
open Virtual_dom.Vdom;
open Node;

let key_handler =
    (
      ~inject: UpdateAction.t => Ui_effect.t(unit),
      ~dir: Key.dir,
      editor: Editor.t,
      model: Model.t,
      evt: Js.t(Dom_html.keyboardEvent),
    )
    : Effect.t(unit) => {
  open Effect;
  let key = Key.mk(dir, evt);
  let get_settings = (model: Model.t): Settings.t => model.settings;
  switch (ProjectorView.key_handoff(editor, key)) {
  | Some(action) =>
    Many([Prevent_default, inject(PerformAction(Project(action)))])
  | None =>
    switch (Keyboard.handle_key_event(key)) {
    | None => Ignore
    | Some(action) =>
      get_settings(model).editing_title
        ? Many([inject(action)])
        : Many([Prevent_default, Stop_propagation, inject(action)])
    }
  };
};

let handlers =
    (
      ~inject: UpdateAction.t => Ui_effect.t(unit),
      editor: Editor.t,
      model: Model.t,
    ) => {
  let attrs = [
    Attr.on_keyup(key_handler(~inject, editor, model, ~dir=KeyUp)),
    Attr.on_keydown(key_handler(~inject, editor, model, ~dir=KeyDown)),
    /* safety handler in case mousedown overlay doesn't catch it */
    Attr.on_mouseup(_ => inject(SetMeta(Mouseup))),
    Attr.on_blur(_ => {
      JsUtil.focus_clipboard_shim();
      Effect.Ignore;
    }),
    Attr.on_focus(_ => {
      JsUtil.focus_clipboard_shim();
      Effect.Ignore;
    }),
    Attr.on_copy(_ => {
      JsUtil.copy(Printer.to_string_selection(editor));
      Effect.Ignore;
    }),
    Attr.on_cut(_ => {
      JsUtil.copy(Printer.to_string_selection(editor));
      inject(UpdateAction.PerformAction(Destruct(Left)));
    }),
    Attr.on_paste(evt => {
      let pasted_text =
        Js.to_string(evt##.clipboardData##getData(Js.string("text")))
        |> Util.StringUtil.trim_leading;
      Dom.preventDefault(evt);
      inject(PerformAction(Paste(pasted_text)));
    }),
  ];
  model.settings.editing_title
    ? attrs : attrs @ [Attr.on_keypress(_ => Effect.Prevent_default)];
};

let main_view =
    (
      ~inject: UpdateAction.t => Ui_effect.t(unit),
      {settings, editors, explainThisModel, results, ui_state, _}: Model.t,
    ) => {
  let editor = Editors.get_editor(editors);
  let cursor_info =
    Indicated.ci_of(editor.state.zipper, editor.state.meta.statics.info_map);
  let highlights =
    ExplainThis.get_color_map(~settings, ~explainThisModel, cursor_info);
  let (editors_view, cursor_info) =
    switch (editors) {
    | Scratch(idx, _) =>
      let result_key = ScratchSlide.scratch_key(string_of_int(idx));
      let view =
        ScratchMode.view(
          ~inject,
          ~ui_state,
          ~settings,
          ~highlights,
          ~results,
          ~result_key,
          editor,
        );
      (view, cursor_info);
    | Documentation(name, _) =>
      let result_key = ScratchSlide.scratch_key(name);
      let view =
        ScratchMode.view(
          ~inject,
          ~ui_state,
          ~settings,
          ~highlights,
          ~results,
          ~result_key,
          editor,
        );
      let info =
        SlideContent.get_content(editors)
        |> Option.map(i => div(~attrs=[Attr.id("slide")], [i]))
        |> Option.to_list;
      (info @ view, cursor_info);
    | Exercises(_, _, exercise) =>
      /* Note the exercises mode uses a seperate path to calculate
       * statics and dynamics via stitching together multiple editors */
      let stitched_dynamics =
        Exercise.stitch_dynamic(
          settings.core,
          exercise,
          settings.core.dynamics ? Some(results) : None,
        );
      let statics =
        Exercise.statics_of_stiched_dynamics(exercise, stitched_dynamics);
      let cursor_info =
        Indicated.ci_of(editor.state.zipper, statics.info_map);
      let highlights =
        ExplainThis.get_color_map(~settings, ~explainThisModel, cursor_info);
      let view =
        ExerciseMode.view(
          ~inject,
          ~ui_state,
          ~settings,
          ~highlights,
          ~stitched_dynamics,
          ~exercise,
        );
      (view, cursor_info);
    };
  let top_bar =
    div(
      ~attrs=[Attr.id("top-bar")],
      NutMenu.view(~inject, ~settings, ~editors)
      @ [div(~attrs=[Attr.id("title")], [text("hazel")])]
      @ [EditorModeView.view(~inject, ~settings, ~editors)],
    );
  let bottom_bar =
    CursorInspector.view(~inject, ~settings, editor, cursor_info);
  let sidebar =
    settings.explainThis.show && settings.core.statics
      ? ExplainThis.view(
          ~inject,
          ~ui_state,
          ~settings,
          ~explainThisModel,
          cursor_info,
        )
      : div([]);
  [
    top_bar,
    div(
      ~attrs=[
        Attr.id("main"),
        Attr.classes([Settings.show_mode(settings.mode)]),
      ],
      editors_view,
    ),
    sidebar,
    bottom_bar,
  ];
};

let get_selection = (model: Model.t): string =>
  model.editors |> Editors.get_editor |> Printer.to_string_selection;

let view = (~inject: UpdateAction.t => Ui_effect.t(unit), model: Model.t) =>
  div(
    ~attrs=
      Attr.[
        id("page"),
        ...handlers(~inject, Editors.get_editor(model.editors), model),
      ],
    [
      FontSpecimen.view("font-specimen"),
      DecUtil.filters,
      JsUtil.clipboard_shim,
    ]
    @ main_view(~inject, model),
  );
