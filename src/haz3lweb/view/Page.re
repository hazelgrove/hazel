open Js_of_ocaml;
open Haz3lcore;
open Virtual_dom.Vdom;
open Node;

let top_bar_view =
    (
      ~inject: Update.t => 'a,
      ~model as {editors, settings, _} as model: Model.t,
    ) =>
  div(
    ~attr=Attr.id("top-bar"),
    NutMenu.view(~inject, model)
    @ [div(~attr=Attr.id("title"), [text("hazel")])]
    @ [EditorModeView.view(~inject, ~settings, ~editors)],
  );

let slide_view = (~inject, ~model, ~ctx_init) => {
  ScratchMode.view(~inject, ~model, ~ctx_init);
};

let editors_view = (~inject, model: Model.t) => {
  let ctx_init =
    Editors.get_ctx_init(~settings=model.settings, model.editors);
  switch (model.editors) {
  | DebugLoad => [DebugMode.view(~inject)]
  | Scratch(_)
  | Examples(_) =>
    [top_bar_view(~inject, ~model)] @ slide_view(~inject, ~model, ~ctx_init)
  | Exercise(_, _, exercise) =>
    [top_bar_view(~inject, ~model)]
    @ ExerciseMode.view(~inject, ~exercise, model)
  };
};

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

let handlers = (~inject: UpdateAction.t => Ui_effect.t(unit), model) => [
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

let view = (~inject: UpdateAction.t => Ui_effect.t(unit), model: Model.t) =>
  div(
    ~attr=Attr.many(Attr.[id("page"), ...handlers(~inject, model)]),
    [
      FontSpecimen.view("font-specimen"),
      DecUtil.filters,
      JsUtil.clipboard_shim,
    ]
    @ editors_view(~inject, model),
  );
