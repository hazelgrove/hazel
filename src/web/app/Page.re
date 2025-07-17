open Virtual_dom.Vdom;
open Node;
open Util;

/* The top-level UI component of Hazel */

/* This file follows conventions in [docs/ui-architecture.md] */

[@deriving (show({with_path: false}), sexp, yojson)]
type selection = Editors.Selection.t;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    globals: Globals.Model.t,
    editors: Editors.Model.t,
    explain_this: ExplainThisModel.t,
    assistant: AssistantModel.t,
    selection,
  };

  let equal = (===);
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
    let assistant = AssistantModel.Store.load();
    {
      editors,
      globals,
      explain_this,
      assistant,
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
    AssistantModel.Store.save(m.assistant);
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
    | Assistant(AssistantUpdate.t)
    | MakeActive(selection)
    | Benchmark(benchmark_action)
    | Start
    | Save;

  let equal = (===);

  let assistant_callback =
      (
        ~schedule_action: t => unit,
        model: Model.t,
        editor: CodeEditable.Model.t,
      ) =>
    AssistantUpdate.check_req(
      ~schedule_action=a => schedule_action(Assistant(a)),
      ~schedule_setting=a => schedule_action(Globals(Set(Assistant(a)))),
      ~chat_id=model.assistant.current_chats.curr_suggestion_chat,
      ~editor,
    );

  let get_editor = (model: Model.t): CodeEditable.Model.t =>
    switch (model.editors) {
    | Scratch(m) => (List.nth(m.scratchpads, m.current) |> snd).editor
    | Documentation(m) => (List.nth(m.scratchpads, m.current) |> snd).editor
    | Exercises(m) => List.nth(m.exercises, m.current).cells.user_impl.editor
    };

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
    | Set(action) =>
      let* settings =
        Settings.Update.update(~action, ~settings=model.globals.settings);
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
            ~send_assistant_insertion_info=
              assistant_callback(~schedule_action, model),
            action,
            model.editors,
          );
        {
          ...model,
          editors,
          selection,
        };
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
    | ExportForInit =>
      let (filename, content) =
        switch (model.editors) {
        | Scratch(model)
        | Documentation(model) =>
          let current = List.nth(model.scratchpads, model.current);
          let filename =
            (current |> fst |> StringUtil.sanitize_filename) ++ ".ml";

          let content =
            [%derive.show: (string, Haz3lcore.PersistentZipper.t)]((
              current |> fst,
              current |> snd |> CellEditor.Model.persist,
            ));
          (filename, content);
        | Exercises(model) =>
          let current = List.nth(model.exercises, model.current);
          let filename = current.editors.module_name ++ ".ml";
          let content = "not supported";
          (filename, content);
        };
      JsUtil.download_string_file(
        ~filename,
        ~content_type="text/plain",
        ~contents=
          "let out : string * Haz3lcore.PersistentZipper.t = " ++ content,
      );
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
            ~send_assistant_insertion_info=
              assistant_callback(~schedule_action, model),
            action,
            model.editors,
          );
        {
          ...model,
          editors,
        };
      };
    | Undo
    | Redo => failwith("Undo/Redo are handled in the history module")
    };
  };

  let update =
      (
        ~import_log,
        ~get_log_and,
        ~schedule_action: t => unit,
        action: t,
        model: Model.t,
      ) => {
    let globals = {
      ...model.globals,
      export_all: Export.export_all,
      get_log_and,
    };
    switch (action) {
    | Globals(action) =>
      update_global(~globals, ~import_log, ~schedule_action, action, model)
    | Editors(action) =>
      let* editors =
        Editors.Update.update(
          ~globals,
          ~schedule_action=a => schedule_action(Editors(a)),
          ~send_assistant_insertion_info=
            assistant_callback(~schedule_action, model),
          action,
          model.editors,
        );
      {
        ...model,
        editors,
      };
    | ExplainThis(action) =>
      let* explain_this =
        ExplainThisUpdate.set_update(model.explain_this, action);
      {
        ...model,
        explain_this,
      };
    | Assistant(action) =>
      let* assistant =
        AssistantUpdate.update(
          ~action,
          ~settings=globals.settings,
          ~model=model.assistant,
          ~editor=get_editor(model),
          ~schedule_action=a => schedule_action(Assistant(a)),
          ~schedule_editor_action=a => schedule_action(Editors(a)),
        );
      {
        ...model,
        assistant,
      };
    | MakeActive(selection) =>
      {
        ...model,
        selection,
      }
      |> Updated.return
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
      print_endline("Saving...");
      Store.save(model);
      model |> return_quiet;
    };
  };

  let can_undo = (action: t) => {
    switch (action) {
    | Globals(action) => Globals.Update.can_undo(action)
    | Editors(action) => Editors.Update.can_undo(action)
    | ExplainThis(action) => ExplainThisUpdate.can_undo(action)
    | Assistant(action) => AssistantUpdate.can_undo(action)
    | MakeActive(_)
    | Benchmark(_) => false
    | Start => false
    | Save => false
    };
  };

  let calculate = (~schedule_action, model: Model.t) => {
    let editors =
      Editors.Update.calculate(
        ~globals=model.globals,
        ~schedule_action=a => schedule_action(Editors(a)),
        model.editors,
      );
    let cursor_info =
      Editors.Selection.get_cursor_info(
        ~globals=model.globals,
        ~inject=_ => Ui_effect.Ignore,
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
    {
      ...model,
      globals,
      editors,
    };
  };
};

module Focus = {
  type t = selection;

  let handle_key_event = (event: Key.t, ~inject): Ui_effect.t(unit) => {
    switch (event) {
    | {key: D("Alt"), sys: Mac | PC, shift: Up, meta: Up, ctrl: Up, alt: Down} =>
      inject(Update.Globals(SetShowBackpackTargets(true)))
    | {key: U("Alt"), _} =>
      inject(Update.Globals(SetShowBackpackTargets(false)))
    | {key: D("F7"), sys: Mac | PC, shift: Down, meta: Up, ctrl: Up, alt: Up} =>
      inject(Update.Benchmark(Start))
    | {
        key: D("Z" | "z"),
        sys: Mac,
        shift: Down,
        meta: Down,
        ctrl: Up,
        alt: Up,
      }
    | {
        key: D("Z" | "z"),
        sys: PC,
        shift: Down,
        meta: Up,
        ctrl: Down,
        alt: Up,
      } =>
      inject(Update.Globals(Redo))
    | {key: D("Z" | "z"), sys: Mac, shift: Up, meta: Down, ctrl: Up, alt: Up}
    | {key: D("Z" | "z"), sys: PC, shift: Up, meta: Up, ctrl: Down, alt: Up} =>
      inject(Update.Globals(Undo))
    | _ => Ui_effect.Ignore
    };
  };

  let get_cursor_info =
      (~globals, ~inject, ~selection: t, model: Model.t): Haz3lcore.Cursor.t => {
    Editors.Selection.get_cursor_info(
      ~globals,
      ~inject=a => inject(Update.Editors(a)),
      ~selection,
      model.editors,
    )
    |> Haz3lcore.Cursor.with_actions(
         Globals.contextual_actions(~inject=a => inject(Update.Globals(a))),
       )
    |> Haz3lcore.Cursor.with_actions([
         Haz3lcore.ContextualAction.mk(
           ~mdIcon="timer",
           ~section="Diagnostics",
           ~hotkey="F7",
           "Run Benchmark",
           inject(Update.Benchmark(Start)),
         ),
       ]);
  };
};

module View = {
  let is_input_field = (elId: option(string)) => {
    switch (elId) {
    | Some("title-input-box")
    | Some("module-name-input")
    | Some("prompt-input-box")
    | Some("test-required-input")
    | Some("point-max-input") => true
    | Some(id) when String.starts_with(~prefix="hint-input", id) => true
    | Some(id) when String.starts_with(~prefix="syntax-hint-input", id) =>
      true
    | Some(id) when String.starts_with(~prefix="impl-hint-input", id) => true
    | _ => false
    };
  };

  let handlers = (~inject: Update.t => Ui_effect.t(unit)) => [
    Key.handler(~f=Focus.handle_key_event(~inject)),
    /* safety handler in case mousedown overlay doesn't catch it */
    Attr.on_mouseup(_ => inject(Globals(SetMousedown(false)))),
  ];
  let nut_menu =
      (
        ~globals: Globals.t,
        ~inject: Editors.Update.t => 'a,
        ~cursor: Haz3lcore.Cursor.t,
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
              Editors.View.file_menu(~globals, ~inject, ~cursor, editors),
            ),
            button(
              Icons.command_palette_terminal,
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

  let top_bar =
      (~globals, ~inject: Update.t => Ui_effect.t(unit), ~cursor, ~editors) =>
    div(
      ~attrs=[Attr.id("top-bar")],
      [
        div(
          ~attrs=[Attr.class_("wrap")],
          [a(~attrs=[Attr.class_("nut-icon")], [Icons.hazelnut])],
        ),
        nut_menu(
          ~globals,
          ~inject=a => inject(Editors(a)),
          ~cursor,
          ~editors,
        ),
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
        ~globals,
        ~inject: Update.t => Ui_effect.t(unit),
        ~cursor,
        {
          globals: _,
          editors,
          explain_this: explainThisModel,
          assistant: assistantModel,
          selection,
        } as model: Model.t,
      ) => {
    let globals = {
      ...globals,
      inject_global: x => inject(Globals(x)),
      get_log_and,
      export_all: Export.export_all,
    };
    let bottom_bar = CursorInspector.view(~globals, cursor);
    let sidebar =
      Sidebar.view(
        ~globals,
        ~explain_this_inject=action => inject(ExplainThis(action)),
        ~assistant_inject=action => inject(Assistant(action)),
        ~signal=
          fun
          | MakeActive(s) => inject(MakeActive(Scratch(s))),
        ~explainThisModel,
        ~assistantModel,
        ~editor=Update.get_editor(model),
        cursor.info,
      );

    let editors_view =
      Editors.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive(selection) => inject(MakeActive(selection)),
        ~inject=a => inject(Editors(a)),
        ~inject_explainthis=a => inject(ExplainThis(a)),
        ~selection=Some(selection),
        model.editors,
      );
    [
      top_bar(~globals, ~inject, ~cursor, ~editors),
      div(
        ~attrs=[
          Attr.id("main"),
          Attr.class_(Editors.Model.mode_string(editors)),
        ],
        editors_view,
      ),
      sidebar,
      bottom_bar,
      ContextInspector.view(~globals, cursor.info),
    ];
  };

  let view =
      (~get_log_and, ~inject: Update.t => Ui_effect.t(unit), model: Model.t) => {
    let globals = {
      ...model.globals,
      inject_global: x => inject(Globals(x)),
      get_log_and,
      export_all: Export.export_all,
    };
    let cursor =
      Focus.get_cursor_info(
        ~globals,
        ~inject,
        ~selection=model.selection,
        model,
      );
    NinjaKeys.initialize(
      NinjaKeys.options(
        ~schedule_effect=Bonsai.Effect.Expert.handle,
        cursor.contextual_actions,
      ),
    );
    div(
      ~attrs=[Attr.id("page"), ...handlers(~inject)],
      [FontSpecimen.view] @ main_view(~globals, ~cursor, ~inject, model),
    );
  };
};
