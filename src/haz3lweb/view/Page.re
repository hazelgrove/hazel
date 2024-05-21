open Js_of_ocaml;
open Haz3lcore;
open Virtual_dom.Vdom;
open Util;
open OptUtil.Syntax;
open Node;

let handlers = (~inject: UpdateAction.t => Ui_effect.t(unit), model) => {
  let get_selection = (model: Model.t): option(string) => {
    let* selection = model.ui_state.active_editor;
    Editors.get_selected_editor(~selection, model.editors, model.results)
    |> Option.map(Printer.to_string_selection);
  };
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
    Attr.on_paste(evt => {
      let pasted_text =
        Js.to_string(evt##.clipboardData##getData(Js.string("text")))
        |> Str.global_replace(Str.regexp("\n[ ]*"), "\n");
      Dom.preventDefault(evt);
      inject(UpdateAction.PerformAction(Paste(pasted_text)));
    }),
  ]
  @ (
    switch (get_selection(model)) {
    | None => []
    | Some(selection) => [
        Attr.on_copy(_ => {
          JsUtil.copy(selection);
          Effect.Ignore;
        }),
        Attr.on_cut(_ => {
          JsUtil.copy(selection);
          inject(UpdateAction.PerformAction(Destruct(Left)));
        }),
      ]
    }
  );
};

let main_view =
    (
      ~inject: UpdateAction.t => Ui_effect.t(unit),
      {settings, editors, explainThisModel, results, statics, ui_state, _}: Model.t,
    ) => {
  let _ =
    switch (ui_state.active_editor) {
    | Some(ae) => print_endline("SELECTED: " ++ Editors.Selection.show(ae))
    | None => print_endline("NO ACTIVE EDITOR")
    };
  let cursor_info =
    Editors.get_cursor_info(
      ~selection=ui_state.active_editor,
      ~settings,
      editors,
      results,
      statics,
    );
  let top_bar =
    div(
      ~attr=Attr.id("top-bar"),
      NutMenu.view(~inject, ~settings, ~editors)
      @ [div(~attr=Attr.id("title"), [text("hazel")])]
      @ [EditorModeView.view(~inject, ~settings, ~editors)],
    );
  let bottom_bar = CursorInspector.view(~inject, ~settings, cursor_info);
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
  let highlights =
    ExplainThis.get_color_map(~settings, ~explainThisModel, cursor_info);
  let editors_view =
    switch (editors) {
    | Scratch(idx, ss) =>
      let editor = List.nth(ss, idx);
      let result_key = ScratchSlide.scratch_key(string_of_int(idx));
      let statics = CachedStatics.lookup(statics, result_key);
      let selected =
        switch (ui_state.active_editor) {
        | Some(Editors.Selection.Scratch(i)) => Some(i)
        | _ => None
        };
      ScratchMode.view(
        ~select=
          s =>
            inject(UpdateAction.MakeActive(Editors.Selection.Scratch(s))),
        ~inject,
        ~ui_state,
        ~settings,
        ~highlights,
        ~results,
        ~result_key,
        ~statics,
        ~selected,
        editor,
      );
    | Documentation(name, ss) =>
      let editor = List.assoc(name, ss);
      let result_key = ScratchSlide.scratch_key(name);
      let info =
        SlideContent.get_content(editors)
        |> Option.map(i => div(~attr=Attr.id("slide"), [i]))
        |> Option.to_list;
      let statics = CachedStatics.lookup(statics, result_key);
      let selected =
        switch (ui_state.active_editor) {
        | Some(Editors.Selection.Documentation(i)) => Some(i)
        | _ => None
        };
      info
      @ ScratchMode.view(
          ~select=
            s =>
              inject(
                UpdateAction.MakeActive(Editors.Selection.Documentation(s)),
              ),
          ~inject,
          ~ui_state,
          ~settings,
          ~highlights,
          ~results,
          ~result_key,
          ~statics,
          ~selected,
          editor,
        );
    | Exercises(_, _, exercise) =>
      let selection =
        switch (ui_state.active_editor) {
        | Some(Editors.Selection.Exercises(pos, sel)) => Some((pos, sel))
        | _ => None
        };
      ExerciseMode.view(
        ~select=
          ((pos, sel)) =>
            inject(
              UpdateAction.MakeActive(Editors.Selection.Exercises(pos, sel)),
            ),
        ~inject_global=inject,
        ~ui_state,
        ~settings,
        ~selection,
        ~highlights,
        ~results,
        ~exercise,
      );
    };
  [
    top_bar,
    div(
      ~attr=
        Attr.many([
          Attr.id("main"),
          Attr.classes([Settings.show_mode(settings.mode)]),
        ]),
      editors_view,
    ),
    sidebar,
    bottom_bar,
  ];
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
