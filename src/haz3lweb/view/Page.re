open Virtual_dom.Vdom;
open Js_of_ocaml;
open Node;
open Widgets;
open Haz3lcore;

let top_bar_view =
    (~inject: Update.t => 'a, ~model as {editors, settings, _}: Model.t) =>
  div(
    ~attr=Attr.id("top-bar"),
    NutMenu.view(~inject, ~settings, ~editors)
    @ [div(~attr=Attr.id("title"), [text("hazel")])]
    @ [EditorModeView.view(~inject, ~settings, ~editors)],
  );

let exercises_view =
    (
      ~inject,
      ~exercise,
      {
        settings,
        explainThisModel,
        results,
        meta: {
          ui_state: {font_metrics, show_backpack_targets, mousedown, _},
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
      ~explainThisModel,
    );
  [top_bar_view(~inject, ~model)]
  @ ExerciseMode.view(
      ~inject,
      ~font_metrics,
      ~mousedown,
      ~show_backpack_targets,
      exercise_mode,
    );
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
          button(Icons.x, _ => inject(Update.Set(Evaluation(ShowSettings)))),
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

let slide_view = (~inject, ~model, ~ctx_init) => {
  [top_bar_view(~inject, ~model)]
  @ ScratchMode.view(~inject, ~model, ~ctx_init);
};

let editors_view = (~inject, model: Model.t) => {
  let ctx_init =
    Editors.get_ctx_init(~settings=model.settings, model.editors);
  switch (model.editors) {
  | DebugLoad => [DebugMode.view(~inject)]
  | Scratch(_)
  | Documentation(_) => slide_view(~inject, ~model, ~ctx_init)
  | Exercises(_, _, exercise) => exercises_view(~inject, ~exercise, model)
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
    @ editors_view(~inject, model)
    @ (
      model.settings.core.evaluation.show_settings
        ? stepper_settings_modal(~inject, model.settings) : []
    ),
  );
