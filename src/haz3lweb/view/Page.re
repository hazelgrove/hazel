open Js_of_ocaml;
open Virtual_dom.Vdom;
open Node;

[@deriving (show({with_path: false}), sexp, yojson)]
type selection = Editors.Selection.t;

module Model = {
  type t = {
    globals: Globals.Model.t,
    editors: Editors.Model.t,
    explain_this: ExplainThisModel.t,
    selection,
  };

  let cutoff = (===);
};

module Store = {
  let load = (): Model.t => {
    let globals = Globals.Model.load();
    let editors =
      Editors.Store.load(~instructor_mode=globals.settings.instructor_mode);
    let explain_this = ExplainThisModel.Store.load();
    {
      editors,
      globals,
      explain_this,
      selection: Editors.Selection.default_selection(editors),
    };
  };

  let save = (m: Model.t): unit => {
    Editors.Store.save(
      ~instructor_mode=m.globals.settings.instructor_mode,
      m.editors,
    );
    Globals.Model.save(m.globals);
    ExplainThisModel.Store.save(m.explain_this);
  };
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type benchmark_action =
    | Start
    | Finish;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Globals(Globals.Update.t)
    | Editors(Editors.Update.t)
    | ExplainThis(ExplainThisUpdate.update)
    | MakeActive(selection)
    | Benchmark(benchmark_action)
    | Start
    | Save;

  let update_global =
      (
        ~import_log,
        ~schedule_action,
        action: Globals.Update.t,
        model: Model.t,
      ) => {
    switch (action) {
    | SetMousedown(mousedown) =>
      {
        ...model,
        globals: {
          ...model.globals,
          mousedown,
        },
      }
      |> Updated.return_quiet
    | SetShowBackpackTargets(show) =>
      {
        ...model,
        globals: {
          ...model.globals,
          show_backpack_targets: show,
        },
      }
      |> Updated.return_quiet
    | SetFontMetrics(fm) =>
      {
        ...model,
        globals: {
          ...model.globals,
          font_metrics: fm,
        },
      }
      |> Updated.return_quiet(~scroll_active=true)
    | Set(settings) =>
      let* settings =
        Settings.Update.update(settings, model.globals.settings);
      {
        ...model,
        globals: {
          ...model.globals,
          settings,
        },
      };
    | JumpToTile(tile) =>
      let jump =
        Editors.Selection.jump_to_tile(
          ~settings=model.globals.settings,
          tile,
          model.editors,
        );
      switch (jump) {
      | None => model |> Updated.return_quiet
      | Some((action, selection)) =>
        let* editors =
          Editors.Update.update(
            ~schedule_action=a => schedule_action(Editors(a)),
            ~settings=model.globals.settings,
            action,
            model.editors,
          );
        {...model, editors, selection};
      };
    | InitImportAll(file) =>
      JsUtil.read_file(file, data =>
        schedule_action(Globals(FinishImportAll(data)))
      );
      model |> return_quiet;
    | FinishImportAll(None) => model |> return_quiet
    | FinishImportAll(Some(data)) =>
      Export.import_all(~import_log, data, ~specs=ExerciseSettings.exercises);
      Store.load() |> return;
    };
  };

  let update =
      (~import_log, ~schedule_action: t => unit, action: t, model: Model.t) => {
    switch (action) {
    | Globals(action) =>
      update_global(~import_log, ~schedule_action, action, model)
    | Editors(action) =>
      let* editors =
        Editors.Update.update(
          ~schedule_action=a => schedule_action(Editors(a)),
          ~settings=model.globals.settings,
          action,
          model.editors,
        );
      {...model, editors};
    | ExplainThis(action) =>
      let* explain_this =
        ExplainThisUpdate.set_update(model.explain_this, action);
      {...model, explain_this};
    | MakeActive(selection) => {...model, selection} |> Updated.return
    | Benchmark(Start) =>
      List.iter(a => schedule_action(Editors(a)), Benchmark.actions_1);
      schedule_action(Benchmark(Finish));
      Benchmark.start();
      model |> Updated.return_quiet;
    | Benchmark(Finish) =>
      Benchmark.finish();
      model |> Updated.return_quiet;
    | Start => model |> return // Triggers recalculation at the start
    | Save =>
      Store.save(model);
      model |> return_quiet;
    };
  };

  let calculate = (~schedule_action, model: Model.t) => {
    let editors =
      Editors.Update.calculate(
        ~settings=model.globals.settings.core,
        ~schedule_action=a => schedule_action(Editors(a)),
        model.editors,
      );
    let cursor_info =
      Editors.Selection.get_cursor_info(
        ~selection=model.selection,
        model.editors,
      );
    let color_highlights =
      ExplainThis.get_color_map(
        ~globals=model.globals,
        ~explainThisModel=model.explain_this,
        cursor_info,
      );
    let globals = Globals.Update.calculate(color_highlights, model.globals);
    {...model, globals, editors};
  };
};

module Selection = {
  type t = selection;

  let handle_key_event =
      (~selection, ~event: Key.t, model: Model.t): option(Update.t) => {
    switch (event) {
    | {key: D("Alt"), sys: Mac | PC, shift: Up, meta: Up, ctrl: Up, alt: Down} =>
      Some(Update.Globals(SetShowBackpackTargets(true)))
    | {key: U("Alt"), _} =>
      Some(Update.Globals(SetShowBackpackTargets(false)))
    | {key: D("F7"), sys: Mac | PC, shift: Down, meta: Up, ctrl: Up, alt: Up} =>
      Some(Update.Benchmark(Start))
    | _ =>
      Editors.Selection.handle_key_event(~selection, ~event, model.editors)
      |> Option.map(x => Update.Editors(x))
    };
  };

  let get_cursor_info = (~selection: t, model: Model.t) => {
    Editors.Selection.get_cursor_info(~selection, model.editors);
  };
};

module View = {
  let handlers = (~inject: Update.t => Ui_effect.t(unit), model: Model.t) => {
    let key_handler =
        (~inject, ~dir: Key.dir, evt: Js.t(Dom_html.keyboardEvent))
        : Effect.t(unit) =>
      Effect.(
        switch (
          Selection.handle_key_event(
            ~selection=Some(model.selection),
            ~event=Key.mk(dir, evt),
            model,
          )
        ) {
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
      Attr.on_mouseup(_ => inject(Globals(SetMousedown(false)))),
      Attr.on_blur(_ => {
        JsUtil.focus_clipboard_shim();
        Effect.Ignore;
      }),
      Attr.on_focus(_ => {
        JsUtil.focus_clipboard_shim();
        Effect.Ignore;
      }),
    ];
  };

  /* HACK: this is the only way I could find to be able to pass
     the polymorphic get_log_and function to the main view function */
  type log_functions = {
    get_log_and: 'a. (string => unit) => unit,
    import_log: string => unit,
  };

  let main_view =
      (
        ~log as {get_log_and, import_log}: log_functions,
        ~inject: Update.t => Ui_effect.t(unit),
        {globals, editors, explain_this: explainThisModel, selection} as model: Model.t,
      ) => {
    let globals = {
      ...globals,
      inject_global: x => inject(Globals(x)),
      get_log_and,
      export_all: Export.export_all,
      import_log,
    };
    let settings = globals.settings;
    let cursor_info = Selection.get_cursor_info(~selection, model);
    let top_bar =
      div(
        ~attr=Attr.id("top-bar"),
        NutMenu.view(
          ~globals,
          ~selection=Some(selection),
          ~inject=a => inject(Editors(a)),
          ~editors,
        )
        @ [div(~attr=Attr.id("title"), [text("hazel")])]
        @ [
          Editors.View.top_bar(
            ~globals,
            ~inject=a => inject(Editors(a)),
            ~editors,
          ),
        ],
      );
    let bottom_bar = CursorInspector.view(~globals, cursor_info);
    let sidebar =
      settings.explainThis.show && settings.core.statics
        ? ExplainThis.view(
            ~globals,
            ~inject=a => inject(ExplainThis(a)),
            ~explainThisModel,
            cursor_info,
          )
        : div([]);
    let editors_view =
      Editors.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive(selection) => inject(MakeActive(selection)),
        ~inject=a => inject(Editors(a)),
        ~selection=Some(selection),
        model.editors,
      );
    [
      top_bar,
      div(~attr=Attr.many([Attr.id("main")]), editors_view),
      sidebar,
      bottom_bar,
    ];
  };

  let view = (~log, ~inject: Update.t => Ui_effect.t(unit), model: Model.t) =>
    div(
      ~attr=Attr.many(Attr.[id("page"), ...handlers(~inject, model)]),
      [
        FontSpecimen.view("font-specimen"),
        DecUtil.filters,
        JsUtil.clipboard_shim,
      ]
      @ main_view(~log, ~inject, model),
    );
};
