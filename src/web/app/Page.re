open Js_of_ocaml;
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
    selection,
  };

  let equal = (===);

  let reset = (~font_metrics=?, ()) => {
    let globals = Globals.Model.init(~font_metrics?, ());
    let settings = globals.settings;
    let instructor_mode = globals.settings.instructor_mode;
    let editors =
      Editors.Store.reset(~settings=settings.core, ~instructor_mode);
    {
      globals,
      editors,
      explain_this: ExplainThisModel.init,
      selection: Editors.Selection.default_selection(editors),
    };
  };
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

  let get_editor = (model: Model.t): CodeEditable.Model.t => {
    let get_scratchpad_editor = (m: ScratchMode.Model.t) => {
      let sp = List.nth(m.scratchpads, m.current);
      switch (sp.kind) {
      | Code({editor, _}) => editor.editor
      /* For Drv scratch slides, expose the Setup editor so the sidebar's
         problem panel reflects errors from Setup only and ignores problems
         inside the derivation trees themselves. */
      | Drv(dm) => dm.cells.setup.editor
      };
    };
    switch (model.editors) {
    | Scratch(m) => get_scratchpad_editor(m)
    | Documentation(m) => get_scratchpad_editor(m)
    | Tutorial(m) => List.nth(m.exercises, m.current).cells.user_impl.editor
    | Exercises(m) => ExercisesMode.Model.get_editor(m)
    };
  };

  /* Editors feeding the Problems sidebar, paired with display labels.
     `None` labels indicate no section header. */
  let get_problem_editors =
      (model: Model.t): list((option(string), list(CodeEditable.Model.t))) => {
    let scratchpad_editors =
        (m: ScratchMode.Model.t)
        : list((option(string), list(CodeEditable.Model.t))) => {
      let sp = List.nth(m.scratchpads, m.current);
      switch (sp.kind) {
      | Code({editor, _}) =>
        /* open stack cells report their problems too (live, unlike the
           master's frozen copy of the same definitions) */
        let stack: list((option(string), list(CodeEditable.Model.t))) =
          switch (m.focus) {
          | None => []
          | Some(f) =>
            List.map(
              (e: ScratchMode.Model.stack_entry) =>
                (
                  Some(
                    Option.value(
                      ScratchMode.Model.header_name(e),
                      ~default="cell",
                    ),
                  ),
                  /* header too: binder/signature errors (TPatNotAVar,
                     shadowed type names, …) live in the header editor */
                  [e.e_header.editor, e.e_body.editor],
                ),
              f.f_entries,
            )
          };
        /* dedup: the master's copy of an OPEN definition is frozen while
           its cell is live — mask master errors/warnings covered by open
           items so each problem is listed once (under the cell's name) */
        let master_editor: CodeEditable.Model.t = editor.editor;
        let master_editor =
          switch (m.focus, Haz3lcore.DefStatics.current()) {
          | (Some(f), Some(ds)) =>
            let open_maps =
              List.filter_map(
                (e: ScratchMode.Model.stack_entry) =>
                  List.find_opt(
                    (it: Haz3lcore.DefStatics.item) =>
                      it.d_id == e.e_id
                      || Haz3lcore.Id.Map.mem(e.e_id, it.d_map),
                    ds.items,
                  )
                  |> Option.map((it: Haz3lcore.DefStatics.item) => it.d_map),
                f.f_entries,
              );
            let covered = id =>
              List.exists(map => Haz3lcore.Id.Map.mem(id, map), open_maps);
            {
              ...master_editor,
              statics: {
                ...master_editor.statics,
                error_ids:
                  List.filter(
                    id => !covered(id),
                    master_editor.statics.error_ids,
                  ),
                warning_ids:
                  List.filter(
                    id => !covered(id),
                    master_editor.statics.warning_ids,
                  ),
              },
            };
          | _ => master_editor
          };
        let master: list((option(string), list(CodeEditable.Model.t))) = [
          (None, [master_editor]),
        ];
        master @ stack;
      | Drv(dm) =>
        /* Scratch/documentation Drv slides don't render the Prelude. */
        DerivationExerciseMode.Model.get_problem_editors(
          ~scratch_mode=true,
          dm,
        )
      };
    };
    switch (model.editors) {
    | Scratch(m) => scratchpad_editors(m)
    | Documentation(m) => scratchpad_editors(m)
    | Tutorial(m) => [
        (None, [List.nth(m.exercises, m.current).cells.user_impl.editor]),
      ]
    | Exercises(m) =>
      ExercisesMode.Model.get_problem_editors(
        ~instructor_mode=model.globals.settings.instructor_mode,
        m,
      )
    };
  };

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
    | Refresh
    | Start
    | Save;

  let equal = (===);

  let update_global =
      (
        ~import_log,
        ~schedule_action: t => unit,
        ~globals: Globals.Model.t,
        action: Globals.Update.t,
        model: Model.t,
      ) => {
    switch (action) {
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
    | SetAgentGlobals(agent_globals_action) =>
      let agent_globals =
        AgentGlobals.Update.update(
          agent_globals_action, model.globals.settings.agent_globals, action =>
          schedule_action(Globals(SetAgentGlobals(action)))
        );
      {
        ...model,
        globals: {
          ...model.globals,
          settings: {
            ...model.globals.settings,
            agent_globals,
          },
        },
      }
      |> Updated.return(~scroll_active=false);
    | JumpToTile(id) =>
      let jump =
        Editors.Selection.jump_to_tile(
          ~settings=model.globals.settings,
          id,
          model.editors,
        );
      switch (jump) {
      | None => model |> Updated.raise_invalid_action
      | Some((action, selection)) =>
        let* editors =
          Editors.Update.update(
            ~globals,
            ~schedule_action=a => schedule_action(Editors(a)),
            action,
            model.editors,
          );
        /* The jump moves the model selection to the target cell but not DOM
           focus (which stays on the clicked sidebar row). Schedule a focus
           of the now-active cell after render so the editor receives
           keystrokes and the caret (gated on :focus) shows there. */
        Haz3lcore.ProbePerform.FocusEffect.schedule_cell();
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
    | SetMetaDown(meta_down) =>
      model.globals.meta_down == meta_down
        ? model |> return_quiet
        : {
            ...model,
            globals: {
              ...model.globals,
              meta_down,
            },
          }
          |> return_quiet
    | UpdateVisibleRows(visible_rows) =>
      {
        ...model,
        globals: {
          ...model.globals,
          visible_rows: Some(visible_rows),
        },
      }
      |> return_quiet
    | FinishImportAll(None) => model |> return_quiet
    | FinishImportAll(Some(data)) =>
      Export.import_all(
        ~import_log,
        data,
        ~exercise_specs=ExerciseSettings.exercises,
        ~tutorial_specs=TutorialSettings.lessons,
      );
      Store.load() |> return;
    | ExportForInit =>
      let (filename, contents) =
        switch (model.editors) {
        | Scratch(model)
        | Documentation(model) =>
          let current = List.nth(model.scratchpads, model.current);
          let (ext, contents) =
            switch (current.kind) {
            | Code({editor, _}) =>
              /* Slides are text-backed: export the committed-.hz form
                 (marker-printed content + one final newline). */
              (
                ".hz",
                Haz3lcore.PersistentZipper.persist(
                  editor.editor.editor.state.zipper,
                ).
                  backup_text,
              )
            | Drv(m) => (
                ".ml",
                DerivationExercise.export_doc_slide_module(m.editors),
              )
            };
          let filename = (current.name |> StringUtil.sanitize_filename) ++ ext;
          (filename, contents);
        | Tutorial(model) =>
          let current = TutorialsMode.Model.get_current(model);
          let filename = current.editors.module_name ++ ".ml";
          let contents =
            Tutorial.export_module(
              current.editors.module_name,
              {eds: current.editors},
            );
          (filename, contents);
        | Exercises(model) =>
          let current = List.nth(model.exercises, model.current);
          let filename =
            ExercisesMode.Model.get_exercise_module_name(current) ++ ".ml";
          let contents = ExercisesMode.Model.export_exercise_module(current);
          (filename, contents);
        };
      JsUtil.download_string_file(
        ~filename,
        ~content_type="text/plain",
        ~contents,
      );
      model |> return_quiet;
    | ActiveEditor(action) =>
      let cursor_info =
        Editors.Selection.get_cursor_info(
          ~inject=_ => Ui_effect.Ignore,
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
        {
          ...model,
          editors,
        };
      };
    | Log(_)
    | Undo
    | Redo
    | RethrowException
    | ClearException
    | RestoreLastKnownGood =>
      failwith(
        "Undo/Redo/Log import/RethrowException/ClearException/RestoreLastKnownGood are handled in higher-level modules",
      )
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
      /* Cross-cell jump-to-definition: a stack cell's jump whose binder
         lives in another definition is rewritten to (ensure the target
         is stacked, select it, then a follow-up caret jump) — mirroring
         the JumpToTile flow above. */
      let (action, selection, followup) =
        switch (Editors.Selection.stack_jump_override(action, model.editors)) {
        | Some((action', selection, followup)) => (
            action',
            selection,
            Some(followup),
          )
        | None => (action, model.selection, None)
        };
      switch (followup) {
      | Some(k) =>
        schedule_action(Editors(k));
        Haz3lcore.ProbePerform.FocusEffect.schedule_cell_top();
      | None => ()
      };
      /* outline adds move the selection (and DOM focus, which also
         scrolls the new cell into view) to the added cell */
      let selection =
        switch (followup) {
        | Some(_) => selection
        | None =>
          switch (
            Editors.Selection.stack_add_selection(action, model.editors)
          ) {
          | Some(s) =>
            Haz3lcore.ProbePerform.FocusEffect.schedule_cell_top();
            s;
          | None => selection
          }
        };
      let* editors =
        Editors.Update.update(
          ~globals,
          ~schedule_action=a => schedule_action(Editors(a)),
          action,
          model.editors,
        );
      /* Reset visible_rows when switching to modes without viewport culling,
       * otherwise stale culling bounds hide projectors incorrectly */
      let globals =
        switch (action) {
        | SwitchMode(Tutorial | Exercises) => {
            ...model.globals,
            visible_rows: None,
          }
        | _ => model.globals
        };
      {
        ...model,
        editors,
        globals,
        selection,
      };
    | ExplainThis(action) =>
      let* explain_this =
        ExplainThisUpdate.set_update(model.explain_this, action);
      {
        ...model,
        explain_this,
      };
    | MakeActive(selection) =>
      {
        ...model,
        selection,
      }
      |> Updated.return(~is_edit=false, ~scroll_active=false, ~historic=false)
    | Benchmark(Start) =>
      List.iter(a => schedule_action(Editors(a)), Benchmark.actions_1);
      schedule_action(Benchmark(Finish));
      Benchmark.start();
      model |> Updated.return_quiet;
    | Benchmark(Finish) =>
      Benchmark.finish();
      model |> Updated.return_quiet;
    | Refresh => model |> Updated.return_quiet(~recalculate=true)
    | Start => model |> return(~historic=false) // Triggers recalculation at the start
    | Save =>
      print_endline("Saving...");
      Store.save(model);
      model |> return_quiet;
    };
  };

  let calculate =
      (~schedule_action, ~is_edited, ~dynamics: bool, model: Model.t) => {
    /* Sync worker-messaging benchmark gating here (settings aren't reachable at
       the WorkerClient.request call sites); only run when the panel is open. */
    WorkerMetrics.sync(
      ~enabled=
        model.globals.settings.show_debug_panel
        && !
             SidebarModel.Settings.is_debug_collapsed(
               WorkerMessagingSection.title,
               model.globals.settings.sidebar,
             ),
      ~encodings=model.globals.settings.sidebar.worker_encodings,
    );
    let editors =
      Editors.Update.calculate(
        ~settings=
          dynamics
            ? model.globals.settings.core
            : {
              ...model.globals.settings.core,
              dynamics: false,
            },
        ~autoprobe_mode=model.globals.settings.autoprobe_mode,
        ~schedule_action=a => schedule_action(Editors(a)),
        ~is_edited,
        model.editors,
      );
    /* Compute cursor info against the POST-calculate editors: some modes
       (e.g. CodeExerciseMode, DerivationExerciseMode) only resync their
       stitched `cells` during calculate, not during update. Reading cursor
       info from `model.editors` (pre-calculate) would see stale cell state
       and yield the wrong ExplainThis highlights for a click/move-only
       action, which doesn't trigger a full statics rebuild. */
    let cursor_info =
      Editors.Selection.get_cursor_info(
        ~inject=_ => Ui_effect.Ignore,
        ~selection=model.selection,
        editors,
      );
    /* When the user's cursor is inside a derivation tree cell, the
       deduction-specific highlight map takes precedence over the generic
       ExplainThis one. We consult the live selection here (rather than
       Editors.Model.get_derivation_info, which reads the stale `model.pos`
       inside DerivationExerciseMode) so that focus on Prelude/Setup doesn't
       get misclassified as focus on the derivation.

       Only the winning map is computed. Each of these runs the whole of
       ExplainThis.decide, so computing the generic one unconditionally and then
       discarding it cost a full pass on every frame with a derivation focused. */
    let derivation_info =
      Editors.Selection.get_derivation_info(
        ~selection=model.selection,
        editors,
      );
    let color_highlights =
      switch (derivation_info) {
      | Some(_) =>
        ExplainThis.get_color_map_deduction(
          ~globals=model.globals,
          ~explainThisModel=model.explain_this,
          derivation_info,
        )
      | None =>
        ExplainThis.get_color_map(
          ~globals=model.globals,
          ~explainThisModel=model.explain_this,
          cursor_info.info,
        )
      };
    let globals = Globals.Update.calculate(color_highlights, model.globals);
    {
      ...model,
      globals,
      editors,
    };
  };
};

module Selection = {
  open Cursor;

  type t = selection;
  let get_cursor_info =
      (~inject: Update.t => Ui_effect.t(unit), ~selection: t, model: Model.t)
      : cursor(Editors.Update.t) => {
    let meta = Keyboard.meta();
    let mk = ContextualAction.mk;
    Editors.Selection.get_cursor_info(
      ~inject=a => inject(Editors(a)),
      ~selection,
      model.editors,
    )
    |> Cursor.with_actions([
         /* Undo / Redo */
         mk(
           ~mdIcon="undo",
           ~hotkey=meta ++ "+z",
           ~action=inject(Globals(Undo)),
           "Undo",
         ),
         mk(
           ~mdIcon="redo",
           ~hotkey=meta ++ "+shift+z",
           ~action=inject(Globals(Redo)),
           "Redo",
         ),
         /* Settings */
         mk(
           ~section="Settings",
           ~mdIcon="tune",
           ~action=inject(Globals(Set(Statics))),
           "Toggle Statics",
         ),
         mk(
           ~section="Settings",
           ~mdIcon="tune",
           ~action=inject(Globals(Set(Assist))),
           "Toggle Completion",
         ),
         mk(
           ~section="Settings",
           ~mdIcon="tune",
           ~action=inject(Globals(Set(SecondaryIcons))),
           "Toggle Show Whitespace",
         ),
         mk(
           ~section="Settings",
           ~mdIcon="tune",
           ~action=inject(Globals(Set(SelectionChunkiness))),
           "Toggle Character-level Mouse",
         ),
         mk(
           ~section="Settings",
           ~mdIcon="tune",
           ~action=inject(Globals(Set(Benchmark))),
           "Toggle Print Benchmarks",
         ),
         mk(
           ~section="Settings",
           ~mdIcon="tune",
           ~action=inject(Globals(Set(ShowDebugPanel))),
           "Toggle Debug Sidebar",
         ),
         mk(
           ~section="Settings",
           ~mdIcon="tune",
           ~action=inject(Globals(Set(Dynamics))),
           "Toggle Dynamics",
         ),
         mk(
           ~section="Settings",
           ~mdIcon="tune",
           ~action=inject(Globals(Set(Elaborate))),
           "Toggle Show Elaboration",
         ),
         mk(
           ~section="Settings",
           ~mdIcon="tune",
           ~action=inject(Globals(Set(Evaluation(ShowFnBodies)))),
           "Toggle Show Function Bodies",
         ),
         mk(
           ~section="Settings",
           ~mdIcon="tune",
           ~action=inject(Globals(Set(Evaluation(ShowCaseClauses)))),
           "Toggle Show Case Clauses",
         ),
         mk(
           ~section="Settings",
           ~mdIcon="tune",
           ~action=inject(Globals(Set(Evaluation(ShowFixpoints)))),
           "Toggle Show fixpoints",
         ),
         mk(
           ~section="Settings",
           ~mdIcon="tune",
           ~action=inject(Globals(Set(Evaluation(ShowAscriptionSteps)))),
           "Toggle Show Ascription Steps",
         ),
         mk(
           ~section="Settings",
           ~mdIcon="tune",
           ~action=inject(Globals(Set(Evaluation(ShowLookups)))),
           "Toggle Show Lookup Steps",
         ),
         mk(
           ~section="Settings",
           ~mdIcon="tune",
           ~action=inject(Globals(Set(Evaluation(ShowFilters)))),
           "Toggle Show Stepper Filters",
         ),
         mk(
           ~section="Settings",
           ~mdIcon="tune",
           ~action=inject(Globals(Set(Evaluation(ShowHiddenSteps)))),
           "Toggle Show Hidden Steps",
         ),
         mk(
           ~section="Settings",
           ~mdIcon="tune",
           ~action=inject(Globals(Set(Sidebar(ToggleShow)))),
           "Toggle Show Sidebar",
         ),
         mk(
           ~section="Settings",
           ~mdIcon="tune",
           ~action=inject(Globals(Set(ExplainThis(ToggleShowFeedback)))),
           "Toggle Show Docs Feedback",
         ),
         /* Export / Diagnostics */
         mk(
           ~mdIcon="download",
           ~section="Export",
           ~action=inject(Globals(ExportForInit)),
           "Export For Init",
         ),
         mk(
           ~mdIcon="timer",
           ~section="Diagnostics",
           ~hotkey="F7",
           ~action=inject(Benchmark(Start)),
           "Run Benchmark",
         ),
       ]);
  };
};

module View = {
  let handlers = (~inject: Update.t => Ui_effect.t(unit), model: Model.t) => {
    let handle_key_event = (key: Key.t): Effect.t(unit) => {
      let meta_down = key.meta == Down;
      let meta_effects =
        model.globals.meta_down == meta_down
          ? [] : [inject(Globals(SetMetaDown(meta_down)))];
      /* Page-level keys only. Editor-specific keys are handled by
       * each editor's own Key.handler and won't bubble here
       * (they call Stop_propagation). */
      let page_action =
        switch (key) {
        | {
            key: D("F7"),
            sys: Mac | PC,
            shift: Down,
            meta: Up,
            ctrl: Up,
            alt: Up,
            _,
          } =>
          Some(Update.Benchmark(Start))
        | {
            key: D("Z" | "z"),
            sys: Mac,
            shift: Down,
            meta: Down,
            ctrl: Up,
            alt: Up,
            _,
          }
        | {
            key: D("Z" | "z"),
            sys: PC,
            shift: Down,
            meta: Up,
            ctrl: Down,
            alt: Up,
            _,
          } =>
          Some(Update.Globals(Redo))
        | {
            key: D("Z" | "z"),
            sys: Mac,
            shift: Up,
            meta: Down,
            ctrl: Up,
            alt: Up,
            _,
          }
        | {
            key: D("Z" | "z"),
            sys: PC,
            shift: Up,
            meta: Up,
            ctrl: Down,
            alt: Up,
            _,
          } =>
          Some(Update.Globals(Undo))
        /* Cmd+P (Mac) / Ctrl+P (PC) toggles auto-probe mode.
           Lost in the keyboard-handling refactor; re-added at the page
           level since the toggle dispatches Globals(Set(AutoprobeMode)),
           matching the deferral comment in ProbeProj.re. */
        | {
            key: D("P" | "p"),
            sys: Mac,
            shift: Up,
            meta: Down,
            ctrl: Up,
            alt: Up,
            _,
          }
        | {
            key: D("P" | "p"),
            sys: PC,
            shift: Up,
            meta: Up,
            ctrl: Down,
            alt: Up,
            _,
          } =>
          Some(Update.Globals(Set(AutoprobeMode)))
        | _ => None
        };
      Effect.(
        switch (page_action) {
        | None => meta_effects == [] ? Ignore : Many(meta_effects)
        | Some(action) =>
          Many(
            [Prevent_default, Stop_propagation, inject(action)]
            @ meta_effects,
          )
        }
      );
    };
    [
      Key.listener(~f=handle_key_event),
      Attr.on_blur(_ => {
        JsUtil.focus_clipboard_shim();
        model.globals.meta_down
          ? Effect.Many([inject(Globals(SetMetaDown(false)))])
          : Effect.Ignore;
      }),
      Attr.on_focus(_ => {
        JsUtil.focus_clipboard_shim();
        Effect.Ignore;
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
              Icons.command_palette_terminal,
              _ => {
                NinjaKeys.open_command_palette();
                Effect.Ignore;
              },
              ~tooltip="Command Palette (" ++ Keyboard.meta() ++ " + k)",
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
        ~log_model,
        ~inject: Update.t => Ui_effect.t(unit),
        ~cursor: Cursor.cursor(Editors.Update.t),
        {globals, editors, explain_this: explainThisModel, selection} as model: Model.t,
      ) => {
    let log_count = LogCount.get();
    let globals = {
      ...globals,
      inject_global: x => inject(Globals(x)),
      get_log_and,
      get_log_count: _ =>
        failwith("get_log_count is deprecated, use Log.get_count_sync"),
      export_all: Export.export_all,
    };
    let bottom_bar = CursorInspector.view(~globals, cursor);
    let sidebar =
      Sidebar.view(
        ~globals,
        ~explain_this_inject=
          (action: ExplainThisUpdate.update) => inject(ExplainThis(action)),
        ~explainThisModel,
        ~editors_inject=(a: Editors.Update.t) => inject(Editors(a)),
        ~editors,
        ~selection=model.selection,
        ~editor=Update.get_editor(model),
        ~problem_editors=Update.get_problem_editors(model),
        ~signal=
          fun
          | MakeActive(s: Selection.t) => inject(MakeActive(s)),
        ~log_model,
        ~log_count,
        ~cursor,
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

    /* Closure cursor bar - shows call stack breadcrumbs when probes are active */
    let current_editor = Update.get_editor(model);
    /* module/definition outline (modular-editors phases 1-2) */
    let outline = {
      /* every stacked definition's id (+ live header name) */
      let focused_entries =
        switch (model.editors) {
        | Scratch(m)
        | Documentation(m) => ScratchMode.Model.focused_names(m)
        | _ => []
        };

      /* structural def ops only make sense in scratch-style modes */
      let is_scratch =
        switch (model.editors) {
        | Scratch(_)
        | Documentation(_) => true
        | _ => false
        };
      /* error attribution at OUTLINE granularity: each error badges the
         DEEPEST row containing it; ancestor rows get a roll-up badge
         that CSS shows only while collapsed (andrew: error goes on the
         deepest thing not hidden by a collapse) */
      let (error_items, error_subtree) = {
        let term = current_editor.statics.term;
        /* prefer the DefStatics slot: it stays live during stacked
           editing (the master's own statics are frozen then) */
        let (info_map, error_ids) =
          switch (Haz3lcore.DefStatics.current()) {
          | Some(ds) => (ds.merged, Haz3lcore.DefStatics.all_error_ids(ds))
          | None => (
              current_editor.statics.info_map,
              current_editor.statics.error_ids,
            )
          };
        let outline_ids = {
          let rec go = (acc, ns: list(OutlineTree.node)) =>
            List.fold_left(
              (acc, n: OutlineTree.node) =>
                go(
                  switch (n.o_id) {
                  | Some(id) => [id, ...acc]
                  | None => acc
                  },
                  n.o_children,
                ),
              acc,
              ns,
            );
          go([], OutlineTree.of_term(term));
        };
        let in_outline = id => List.mem(id, outline_ids);
        List.fold_left(
          ((direct, roll), err_id) => {
            let path =
              switch (Haz3lcore.Id.Map.find_opt(err_id, info_map)) {
              | Some(info) => [err_id, ...Language.Info.ancestors_of(info)]
              | None => [err_id]
              };
            switch (List.filter(in_outline, path)) {
            | [] => (direct, roll)
            | [deepest, ...above] => ([deepest, ...direct], above @ roll)
            };
          },
          ([], []),
          error_ids,
        );
      };
      OutlineSidebar.view(
        ~jump=id => globals.inject_global(JumpToTile(id)),
        /* plain click with a stack open ADDS (or moves to) that cell —
           never replaces the stack (andrew: replacing was a footgun) */
        ~focus=id => inject(Editors(Scratch(FocusEnsure(id)))),
        ~toggle=id => inject(Editors(Scratch(FocusToggle(id)))),
        ~toggle_run=id => inject(Editors(Scratch(FocusToggleRun(id)))),
        ~is_collapsed={
          let name =
            switch (model.editors) {
            | Scratch(m)
            | Documentation(m) =>
              switch (List.nth_opt(m.scratchpads, m.current)) {
              | Some(sp) => sp.name
              | None => ""
              }
            | _ => ""
            };
          let collapsed = ScratchMode.collapse_paths(name);
          path => List.mem(path, collapsed);
        },
        ~toggle_collapse=
          path => inject(Editors(Scratch(OutlineCollapse(path)))),
        ~error_items,
        ~error_subtree,
        ~unfocus=inject(Editors(Scratch(UnfocusDef))),
        ~focused_entries,
        ~menu=is_scratch ? ScratchMode.outline_menu^ : None,
        ~menu_open=
          (id, x, y) =>
            is_scratch
              ? inject(Editors(Scratch(OutlineMenu(Some((id, x, y))))))
              : Virtual_dom.Vdom.Effect.Ignore,
        ~menu_close=inject(Editors(Scratch(OutlineMenu(None)))),
        ~def_op=
          (op, id) => inject(Editors(Scratch(OutlineDefOp(op, id)))),
        /* live ✓/✗ for test rows, from the master's whole-program
           result (stays live while a stack is open) */
        ~test_status={
          let results =
            switch (model.editors) {
            | Scratch(m)
            | Documentation(m) =>
              switch (
                List.nth_opt(m.scratchpads, m.current)
                |> Option.map((sp: ScratchMode.Scratchpad.t) => sp.kind)
              ) {
              | Some(Code({editor, _})) =>
                EvalResult.Model.test_results(editor.CellEditor.Model.result)
              | _ => None
              }
            | _ => None
            };
          id =>
            Option.bind(results, (tr: Language.TestResults.t) =>
              Language.TestMap.lookup(id, tr.test_map)
              |> Option.map(Language.TestMap.joint_status)
            );
        },
        /* the master stays in its scratchpad slot while the stack is
           open (statics warm), so its term is always current */
        current_editor.statics.term,
      );
    };
    let indicated_id =
      Haz3lcore.Indicated.index(current_editor.editor.state.zipper);
    let closure_cursor_bar =
      SampleFocusBar.view(
        ~globals,
        ~refractors=current_editor.editor.state.zipper.refractors,
        ~info_map=current_editor.statics.info_map,
        ~indicated_id,
      );

    /* Scroll handler for viewport culling. Only enabled for Scratch and
     * Documentation modes where there's a single editor filling the
     * scrollable area. Tutorial and Exercises have multiple editors. */
    let on_scroll = (evt: Js.t(Dom_html.event)) => {
      let culling_enabled =
        switch (editors) {
        | Scratch(_)
        | Documentation(_)
        | Tutorial(_)
        | Exercises(_) => false
        };
      if (!culling_enabled) {
        Effect.Ignore;
      } else {
        let container =
          Js.Opt.to_option(evt##.currentTarget)
          |> Option.map(Js.Unsafe.coerce);
        switch (container) {
        | None => Effect.Ignore
        | Some(c) =>
          let new_visible =
            Globals.VisibleRows.compute(
              ~scroll_top=float_of_int(c##.scrollTop),
              ~client_height=float_of_int(c##.clientHeight),
              ~row_height=globals.font_metrics.row_height,
              (),
            );
          Globals.VisibleRows.changed(globals.visible_rows, new_visible)
            ? inject(Globals(UpdateVisibleRows(new_visible)))
            : Effect.Ignore;
        };
      };
    };

    [
      top_bar(~globals, ~inject, ~editors),
      closure_cursor_bar,
      div(
        ~attrs=[
          Attr.id("main"),
          Attr.classes(
            [Editors.Model.mode_string(editors)]
            @ Editors.Model.extra_main_classes(editors),
          ),
          Attr.on_scroll(on_scroll),
        ],
        editors_view,
      ),
      sidebar,
      outline,
      bottom_bar,
      ContextInspector.view(~globals, cursor.info),
      HoverRuleSpec.view(~globals),
    ];
  };

  let view =
      (
        ~log_model,
        ~get_log_and,
        ~inject: Update.t => Ui_effect.t(unit),
        model: Model.t,
      ) => {
    let cursor =
      Selection.get_cursor_info(~inject, ~selection=model.selection, model);
    NinjaKeys.initialize(cursor.contextual_actions);
    div(
      ~attrs=[Attr.id("page"), ...handlers(~inject, model)],
      [FontSpecimen.view, JsUtil.clipboard_shim]
      @ main_view(~log_model, ~get_log_and, ~cursor, ~inject, model),
    );
  };
};
