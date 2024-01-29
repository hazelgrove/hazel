open Js_of_ocaml;
open Widgets;
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
      {settings, editors, explainThisModel, results, meta: {ui_state, _}, _}: Model.t,
    ) => {
  let statics = Editors.get_statics(~settings, editors);
  let {cursor_info, info_map, indicated_id, _}: Editor.statics = statics;
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
          ~font_metrics=ui_state.font_metrics,
          ~settings,
          ~doc=explainThisModel,
          cursor_info,
        )
      : div([]);
  let color_highlighting =
    ExplainThis.get_color_map(
      ~settings,
      ~explainThisModel,
      indicated_id,
      info_map,
    );
  let editors_view =
    switch (editors) {
    | DebugLoad => [DebugMode.view(~inject)]
    | Scratch(idx, _) =>
      let result_key = ScratchSlide.scratch_key(string_of_int(idx));
      let result = ModelResults.get(results, result_key);
      ScratchMode.view(
        ~inject,
        ~ui_state,
        ~settings,
        ~color_highlighting,
        ~result,
        ~result_key,
        ~statics,
      );
    | Documentation(name, _) =>
      let result_key = ScratchSlide.scratch_key(name);
      let result = ModelResults.get(results, result_key);
      ScratchMode.view(
        ~inject,
        ~ui_state,
        ~settings,
        ~color_highlighting,
        ~result,
        ~result_key,
        ~statics,
      );
    | Exercises(_, _, exercise) =>
      ExerciseMode.view(
        ~inject,
        ~ui_state,
        ~settings,
        ~color_highlighting,
        ~results,
        ~exercise,
      )
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

let stepper_settings_modal = (~inject, settings: Settings.t) => {
  let modal = div(~attr=Attr.many([Attr.class_("settings-modal")]));
  let setting = (icon, name, current, action: UpdateAction.settings_action) =>
    div(
      ~attr=Attr.many([Attr.class_("settings-toggle")]),
      [
        toggle(~tooltip=name, icon, current, _ => inject(Update.Set(action))),
        text(name),
      ],
    );
  [
    modal([
      div(
        ~attr=Attr.many([Attr.class_("settings-modal-top")]),
        [
          Widgets.button(Icons.x, _ =>
            inject(Update.Set(Evaluation(ShowSettings)))
          ),
        ],
      ),
      setting(
        "h",
        "show full step trace",
        settings.core.evaluation.stepper_history,
        Evaluation(ShowRecord),
      ),
      setting(
        "|",
        "show case clauses",
        settings.core.evaluation.show_case_clauses,
        Evaluation(ShowCaseClauses),
      ),
      setting(
        "λ",
        "show function bodies",
        settings.core.evaluation.show_fn_bodies,
        Evaluation(ShowFnBodies),
      ),
      setting(
        "x",
        "show fixpoints",
        settings.core.evaluation.show_fixpoints,
        Evaluation(ShowFixpoints),
      ),
      setting(
        Unicode.castArrowSym,
        "show casts",
        settings.core.evaluation.show_casts,
        Evaluation(ShowCasts),
      ),
      setting(
        "🔍",
        "show lookup steps",
        settings.core.evaluation.show_lookup_steps,
        Evaluation(ShowLookups),
      ),
      setting(
        "⏯️",
        "show stepper filters",
        settings.core.evaluation.show_stepper_filters,
        Evaluation(ShowFilters),
      ),
    ]),
    div(
      ~attr=
        Attr.many([
          Attr.class_("modal-back"),
          Attr.on_mousedown(_ =>
            inject(Update.Set(Evaluation(ShowSettings)))
          ),
        ]),
      [],
    ),
  ];
};

let get_selection = (model: Model.t): string =>
  model.editors |> Editors.get_editor |> Printer.to_string_selection;

let view = (~inject: UpdateAction.t => Ui_effect.t(unit), model: Model.t) =>
  div(
    ~attr=Attr.many(Attr.[id("page"), ...handlers(~inject, model)]),
    [
      FontSpecimen.view("font-specimen"),
      DecUtil.filters,
      JsUtil.clipboard_shim,
    ]
    @ main_view(~inject, model)
    @ (
      model.settings.core.evaluation.show_settings
        ? stepper_settings_modal(~inject, model.settings) : []
    ),
  );
