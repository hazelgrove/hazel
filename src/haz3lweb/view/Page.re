open Js_of_ocaml;
open Haz3lcore;
open Virtual_dom.Vdom;
open Node;

let handlers = (~inject: UpdateAction.t => Ui_effect.t(unit), model) => {
  let get_selection = (model: Model.t): string =>
    model.editors |> Editors.get_editor |> Printer.to_string_selection;
  let key_handler =
      (~inject, ~dir: Key.dir, evt: Js.t(Dom_html.keyboardEvent))
      : Effect.t(unit) =>
    Effect.(
      switch (Keyboard.handle_key_event(Key.mk(dir, evt))) {
      | None => Ignore
      | Some(action) =>
        Many([Prevent_default, Stop_propagation, inject(action)])
      }
    );
  [
    Attr.on_keypress(_ => Effect.Prevent_default),
    Attr.on_keyup(key_handler(~inject, ~dir=KeyUp)),
    Attr.on_keydown(key_handler(~inject, ~dir=KeyDown)),
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
      JsUtil.copy(get_selection(model));
      Effect.Ignore;
    }),
    Attr.on_cut(_ => {
      JsUtil.copy(get_selection(model));
      inject(UpdateAction.PerformAction(Destruct(Left)));
    }),
    Attr.on_paste(evt => {
      let pasted_text =
        Js.to_string(evt##.clipboardData##getData(Js.string("text")))
        |> Str.global_replace(Str.regexp("\n[ ]*"), "\n");
      Dom.preventDefault(evt);
      inject(UpdateAction.Paste(pasted_text));
    }),
  ];
};

let main_view =
    (
      ~inject: UpdateAction.t => Ui_effect.t(unit),
      {settings, editors, langDocMessages, meta: {ui_state, results, _}, _}: Model.t,
    ) => {
  let statics = Editors.get_statics(~settings, editors);
  let {cursor_info, info_map, indicated_id, zipper, _}: Editor.statics = statics;
  let top_bar =
    div(
      ~attr=Attr.id("top-bar"),
      NutMenu.view(~inject, ~settings, ~editors)
      @ [div(~attr=Attr.id("title"), [text("hazel")])]
      @ [EditorModeView.view(~inject, ~settings, ~editors)],
    );
  let bottom_bar =
    CursorInspector.view(
      ~inject,
      ~settings,
      ~show_lang_doc=langDocMessages.show,
      ~cursor_info,
    );
  let sidebar =
    langDocMessages.show && settings.core.statics
      ? LangDoc.view(
          ~inject,
          ~font_metrics=ui_state.font_metrics,
          ~settings,
          ~doc=langDocMessages,
          indicated_id,
          info_map,
        )
      : div([]);
  let color_highlighting: option(ColorSteps.colorMap) =
    if (langDocMessages.highlight && langDocMessages.show) {
      Some(LangDoc.get_color_map(~settings, ~doc=langDocMessages, zipper));
    } else {
      None;
    };
  let editors_view =
    switch (editors) {
    | DebugLoad => [DebugMode.view(~inject)]
    | Scratch(_)
    | Examples(_) =>
      ScratchMode.view(
        ~inject,
        ~ui_state,
        ~settings,
        ~color_highlighting,
        ~results,
        ~statics,
      )
    | Exercise(_, _, exercise) =>
      ExerciseMode.view(
        ~inject,
        ~ui_state,
        ~settings,
        ~color_highlighting,
        ~results,
        ~exercise,
      )
    };
  [top_bar, div(~attr=Attr.id("main"), editors_view), sidebar, bottom_bar];
};

let view = (~inject: UpdateAction.t => Ui_effect.t(unit), model: Model.t) =>
  div(
    ~attr=Attr.many(Attr.[id("page"), ...handlers(~inject, model)]),
    [
      FontSpecimen.view("font-specimen"),
      DecUtil.filters,
      JsUtil.clipboard_shim,
    ]
    @ main_view(~inject, model),
  );
