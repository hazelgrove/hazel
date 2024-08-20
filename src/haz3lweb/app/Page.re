open Js_of_ocaml;
open Virtual_dom.Vdom;
open Node;
open Util;

/* The top-level UI component of Hazel */

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
      Editors.Store.load(
        ~settings=globals.settings.core,
        ~instructor_mode=globals.settings.instructor_mode,
      );
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
        ~globals: Globals.Model.t,
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
            ~globals,
            ~schedule_action=a => schedule_action(Editors(a)),
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
    | ExportPersistentData =>
      Store.save(model);
      Export.export_persistent();
      model |> return_quiet;
    | ActiveEditor(action) =>
      let cursor_info =
        Editors.Selection.get_cursor_info(
          ~selection=model.selection,
          model.editors,
        );
      switch (cursor_info.editor_action(action)) {
      | None => model |> return_quiet
      | Some(action) =>
        let* editors =
          Editors.Update.update(
            ~globals=model.globals,
            ~schedule_action=a => schedule_action(Editors(a)),
            action,
            model.editors,
          );
        {...model, editors};
      };
    | Undo =>
      let cursor_info =
        Editors.Selection.get_cursor_info(
          ~selection=model.selection,
          model.editors,
        );
      switch (cursor_info.undo_action) {
      | None => model |> return_quiet
      | Some(action) =>
        let* editors =
          Editors.Update.update(
            ~globals=model.globals,
            ~schedule_action=a => schedule_action(Editors(a)),
            action,
            model.editors,
          );
        {...model, editors};
      };
    | Redo =>
      let cursor_info =
        Editors.Selection.get_cursor_info(
          ~selection=model.selection,
          model.editors,
        );
      switch (cursor_info.redo_action) {
      | None => model |> return_quiet
      | Some(action) =>
        let* editors =
          Editors.Update.update(
            ~globals=model.globals,
            ~schedule_action=a => schedule_action(Editors(a)),
            action,
            model.editors,
          );
        {...model, editors};
      };
    };
  };

  let update =
      (~import_log, ~schedule_action: t => unit, action: t, model: Model.t) => {
    let globals = {...model.globals, export_all: Export.export_all};
    switch (action) {
    | Globals(action) =>
      update_global(~globals, ~import_log, ~schedule_action, action, model)
    | Editors(action) =>
      let* editors =
        Editors.Update.update(
          ~globals=model.globals,
          ~schedule_action=a => schedule_action(Editors(a)),
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

  let calculate = (~schedule_action, ~is_edited, model: Model.t) => {
    let editors =
      Editors.Update.calculate(
        ~settings=model.globals.settings.core,
        ~schedule_action=a => schedule_action(Editors(a)),
        ~is_edited,
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
        cursor_info.info,
      );
    let globals = Globals.Update.calculate(color_highlights, model.globals);
    {...model, globals, editors};
  };
};

module Selection = {
  open Cursor;

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

  let get_cursor_info =
      (~selection: t, model: Model.t): cursor(Editors.Update.t) => {
    Editors.Selection.get_cursor_info(~selection, model.editors);
  };
};

module View = {
  let handlers =
      (
        ~inject: Update.t => Ui_effect.t(unit),
        ~cursor: Cursor.cursor(Editors.Update.t),
        model: Model.t,
      ) => {
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
      Attr.on_copy(_ => {
        JsUtil.copy(cursor.selected_text |> Option.value(~default=""));
        Effect.Ignore;
      }),
      Attr.on_cut(_ => {
        JsUtil.copy(cursor.selected_text |> Option.value(~default=""));
        Option.map(
          inject,
          Selection.handle_key_event(
            ~selection=Some(model.selection),
            ~event=
              Key.{
                key: D("Delete"),
                sys: Os.is_mac^ ? Mac : PC,
                shift: Up,
                meta: Up,
                ctrl: Up,
                alt: Up,
              },
            model,
          ),
        )
        |> Option.value(~default=Effect.Ignore);
      }),
    ]
    @ [
      Attr.on_paste(evt => {
        let pasted_text =
          Js.to_string(evt##.clipboardData##getData(Js.string("text")))
          |> Str.global_replace(Str.regexp("\n[ ]*"), "\n");
        Dom.preventDefault(evt);
        switch (cursor.editor_action(Paste(pasted_text))) {
        | None => Effect.Ignore
        | Some(action) => inject(Editors(action))
        };
      }),
    ];
  };

  let nut_menu =
      (
        ~globals: Globals.t,
        ~inject: Editors.Update.t => 'a,
        ~editors: Editors.Model.t,
      ) => {
    NutMenu.(
      Widgets.(
        div(
          ~attrs=[Attr.class_("nut-menu")],
          [
            submenu(
              ~tooltip="Settings",
              ~icon=Icons.gear,
              NutMenu.settings_menu(~globals),
            ),
            submenu(
              ~tooltip="File",
              ~icon=Icons.disk,
              Editors.View.file_menu(~globals, ~inject, editors),
            ),
            button(
              Icons.command_palette_sparkle,
              _ => {
                NinjaKeys.open_command_palette();
                Effect.Ignore;
              },
              ~tooltip=
                "Command Palette ("
                ++ Keyboard.meta(Os.is_mac^ ? Mac : PC)
                ++ " + k)",
            ),
            link(
              Icons.github,
              "https://github.com/hazelgrove/hazel",
              ~tooltip="Hazel on GitHub",
            ),
            link(Icons.info, "https://hazel.org", ~tooltip="Hazel Homepage"),
          ],
        )
      )
    );
  };

  let top_bar = (~globals, ~inject: Update.t => Ui_effect.t(unit), ~editors) =>
    div(
      ~attrs=[Attr.id("top-bar")],
      [
        div(
          ~attrs=[Attr.class_("wrap")],
          [a(~attrs=[Attr.class_("nut-icon")], [Icons.hazelnut])],
        ),
        nut_menu(~globals, ~inject=a => inject(Editors(a)), ~editors),
        div(
          ~attrs=[Attr.class_("wrap")],
          [div(~attrs=[Attr.id("title")], [text("hazel")])],
        ),
        div(
          ~attrs=[Attr.class_("wrap")],
          [
            Editors.View.top_bar(
              ~globals,
              ~inject=a => inject(Editors(a)),
              ~editors,
            ),
          ],
        ),
      ],
    );

  let main_view =
      (
        ~get_log_and: (string => unit) => unit,
        ~inject: Update.t => Ui_effect.t(unit),
        ~cursor: Cursor.cursor(Editors.Update.t),
        {globals, editors, explain_this: explainThisModel, selection} as model: Model.t,
      ) => {
    let globals = {
      ...globals,
      inject_global: x => inject(Globals(x)),
      get_log_and,
      export_all: Export.export_all,
    };
    let bottom_bar =
      CursorInspector.view(
        ~globals,
        ~inject=a => inject(Editors(a)),
        cursor,
      );
    let sidebar =
      globals.settings.explainThis.show && globals.settings.core.statics
        ? ExplainThis.view(
            ~globals,
            ~inject=a => inject(ExplainThis(a)),
            ~explainThisModel,
            cursor.info,
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
      top_bar(~globals, ~inject, ~editors),
      div(
        ~attrs=[
          Attr.id("main"),
          Attr.class_(Editors.Model.mode_string(editors)),
        ],
        editors_view,
      ),
      sidebar,
      bottom_bar,
      CtxInspector.view(~globals, cursor.info),
    ];
  };

  let view =
      (~get_log_and, ~inject: Update.t => Ui_effect.t(unit), model: Model.t) => {
    let cursor = Selection.get_cursor_info(~selection=model.selection, model);
    div(
      ~attrs=[Attr.id("page"), ...handlers(~cursor, ~inject, model)],
      [
        FontSpecimen.view("font-specimen"),
        DecUtil.filters,
        JsUtil.clipboard_shim,
      ]
      @ main_view(~get_log_and, ~cursor, ~inject, model),
    );
  };
};
