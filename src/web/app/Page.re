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
      | Code({program: Whole(editor), _}) => editor.editor
      /* divided: the cell with the caret */
      | Code({program: Divided(d), _}) => Divided.active_editor(d).editor
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
    | Config(m) => (List.nth(m.configs, m.current) |> snd).editor
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
      | Code({program: Whole(editor), _}) => [(None, [editor.editor])]
      | Code({program: Divided(d), _}) =>
        /* open cells report their own problems */
        let cells = Divided.cells(d);
        let stack: list((option(string), list(CodeEditable.Model.t))) =
          List.map(
            (e: ScratchCell.t) =>
              (
                Some(
                  Option.value(ScratchCell.header_name(e), ~default="cell"),
                ),
                /* header too: binder/signature errors (TPatNotAVar,
                   shadowed type names, …) live in the header editor */
                [e.e_header.editor, e.e_body.editor],
              ),
            cells,
          );
        /* the rest of the program: whole-program statics with the open
           items' problems masked, so each is listed once (under its
           cell) */
        let rest_editor: CodeEditable.Model.t = Divided.outside_editor(d);
        let rest_editor =
          switch (Haz3lcore.DefStatics.current()) {
          | Some(ds) =>
            let open_maps =
              List.filter_map(
                (e: ScratchCell.t) =>
                  List.find_opt(
                    (it: Haz3lcore.DefStatics.item) =>
                      it.d_id == e.e_id
                      || Haz3lcore.Id.Map.mem(e.e_id, it.d_map),
                    ds.items,
                  )
                  |> Option.map((it: Haz3lcore.DefStatics.item) => it.d_map),
                cells,
              );
            let covered = id =>
              List.exists(map => Haz3lcore.Id.Map.mem(id, map), open_maps);
            {
              ...rest_editor,
              statics: {
                ...rest_editor.statics,
                error_ids:
                  List.filter(
                    id => !covered(id),
                    rest_editor.statics.error_ids,
                  ),
                warning_ids:
                  List.filter(
                    id => !covered(id),
                    rest_editor.statics.warning_ids,
                  ),
              },
            };
          | None => rest_editor
          };
        [(None, [rest_editor]), ...stack];
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
    | Config(m) => [
        (None, [(List.nth(m.configs, m.current) |> snd).editor]),
      ]
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
            ~schedule_global=a => schedule_action(Globals(a)),
            action,
            model.editors,
          );
        /* The jump moves the model selection to the target cell but not DOM
           focus (which stays on the clicked sidebar row). Schedule a focus
           of the now-active cell after render so the editor receives
           keystrokes and the caret (gated on :focus) shows there. */
        Haz3lcore.FocusEffect.schedule_cell();
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
        | Config(model) =>
          let (config_type, cell) = List.nth(model.configs, model.current);
          let name = ConfigurationMode.Model.config_name_of_type(config_type);
          /* Config slides are text-backed like the doc slides: export the
             committed-.hz form, matching the scratch export below. */
          let persisted =
            Haz3lcore.PersistentZipper.persist(
              cell.editor.editor.state.zipper,
            );
          (
            (name |> StringUtil.sanitize_filename) ++ ".hz",
            persisted.backup_text,
          );
        | Scratch(model)
        | Documentation(model) =>
          let current = List.nth(model.scratchpads, model.current);
          let (ext, contents) =
            switch (current.kind) {
            | Code({program, _}) =>
              /* Slides are text-backed: export the committed-.hz form
                 (marker-printed content + one final newline). */
              (
                ".hz",
                Haz3lcore.PersistentZipper.persist(
                  Program.whole(program).editor.editor.state.zipper,
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
            ~schedule_global=a => schedule_action(Globals(a)),
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

  let update_model =
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
        Haz3lcore.FocusEffect.schedule_cell_top();
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
            Haz3lcore.FocusEffect.schedule_cell_top();
            s;
          | None => selection
          }
        };
      let* editors =
        Editors.Update.update(
          ~globals,
          ~schedule_action=a => schedule_action(Editors(a)),
          ~schedule_global=a => schedule_action(Globals(a)),
          action,
          model.editors,
        );
      /* A different editor (mode/slide/exercise switch) invalidates the
       * culling range: stale bounds would hide its projectors until the next
       * scroll. Main.seed_visible_rows re-seeds where culling applies. */
      let globals =
        Editors.Model.editor_key(editors)
        != Editors.Model.editor_key(model.editors)
          ? {
            ...model.globals,
            visible_rows: None,
          }
          : model.globals;
      /* an unchanged selection follows its pane (cells shift as they
         open and close); a fresh one already names its target */
      let selection =
        selection === model.selection
          ? Editors.Selection.follow(
              ~before=model.editors,
              selection,
              editors,
            )
          : selection;
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

  let update = (~import_log, ~get_log_and, ~schedule_action, action, model) => {
    let* model =
      update_model(
        ~import_log,
        ~get_log_and,
        ~schedule_action,
        action,
        model,
      );
    /* Synchronize after every update, including startup and mode changes.
       Only Probes lessons apply overrides; leaving restores user settings. */
    let lesson =
      switch (model.editors) {
      | Tutorial(t) => Some(TutorialsMode.Model.get_current(t).editors)
      | _ => None
      };
    TutorialSlideInit.maybe_apply_on_change(
      ~autoprobe=model.globals.settings.autoprobe_mode,
      ~set_autoprobe=m => schedule_action(Globals(Set(SetAutoprobe(m)))),
      lesson,
    );
    model;
  };

  let calculate =
      (~schedule_action, ~is_edited, ~dynamics: bool, model: Model.t) => {
    /* Sync debug-panel gating here (settings aren't reachable at the
       WorkerClient.request call sites nor the per-frame instrumentation sites);
       each collector only runs while its panel is open. */
    let sidebar = model.globals.settings.sidebar;
    let debug_panel_open = title =>
      model.globals.settings.show_debug_panel
      && SidebarModel.Settings.is_debug_expanded(title, sidebar);
    WorkerMetrics.sync(
      ~enabled=debug_panel_open(WorkerMessagingSection.title),
    );
    WorkerMetrics.set_encodings(sidebar.worker_encodings);
    EvalMetrics.sync(~enabled=debug_panel_open(EvaluationSection.title));
    PerfMetrics.sync(
      ~enabled=
        debug_panel_open(StaticsSection.title)
        || debug_panel_open(EditorSection.title)
        || debug_panel_open(FrameSection.title),
    );
    /* Everything below is one frame for the telemetry panels. */
    PerfMetrics.time_frame(() => {
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
        PerfMetrics.time_cursor(() =>
          Editors.Selection.get_cursor_info(
            ~inject=_ => Ui_effect.Ignore,
            ~selection=model.selection,
            editors,
          )
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
        PerfMetrics.time_colors(() =>
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
          }
        );
      let globals = Globals.Update.calculate(color_highlights, model.globals);
      {
        ...model,
        globals,
        editors,
      };
    });
  };
};

module Selection = {
  open Cursor;

  type t = selection;
  let get_cursor_info =
      (~inject: Update.t => Ui_effect.t(unit), ~selection: t, model: Model.t)
      : cursor(Editors.Update.t) => {
    let of_shortcut = ContextualAction.of_shortcut;
    Editors.Selection.get_cursor_info(
      ~inject=a => inject(Editors(a)),
      ~selection,
      model.editors,
    )
    |> Cursor.with_actions([
         /* Undo / Redo */
         of_shortcut(~action=inject(Globals(Undo)), Undo),
         of_shortcut(~action=inject(Globals(Redo)), Redo),
         /* Settings */
         of_shortcut(~action=inject(Globals(Set(Statics))), ToggleStatics),
         of_shortcut(
           ~action=inject(Globals(Set(Assist))),
           ToggleCompletion,
         ),
         of_shortcut(
           ~action=inject(Globals(Set(SecondaryIcons))),
           ToggleShowWhitespace,
         ),
         of_shortcut(
           ~action=inject(Globals(Set(SelectionChunkiness))),
           ToggleCharacterLevelMouse,
         ),
         of_shortcut(
           ~action=inject(Globals(Set(Benchmark))),
           TogglePrintBenchmarks,
         ),
         of_shortcut(
           ~action=inject(Globals(Set(ShowDebugPanel))),
           ToggleDebugSidebar,
         ),
         of_shortcut(
           ~action=inject(Globals(Set(Dynamics))),
           ToggleDynamics,
         ),
         of_shortcut(
           ~action=inject(Globals(Set(Elaborate))),
           ToggleShowElaboration,
         ),
         of_shortcut(
           ~action=inject(Globals(Set(Evaluation(ShowFnBodies)))),
           ToggleShowFunctionBodies,
         ),
         of_shortcut(
           ~action=inject(Globals(Set(Evaluation(ShowCaseClauses)))),
           ToggleShowCaseClauses,
         ),
         of_shortcut(
           ~action=inject(Globals(Set(Evaluation(ShowFixpoints)))),
           ToggleShowFixpoints,
         ),
         of_shortcut(
           ~action=inject(Globals(Set(Evaluation(ShowAscriptionSteps)))),
           ToggleShowAscriptionSteps,
         ),
         of_shortcut(
           ~action=inject(Globals(Set(Evaluation(ShowLookups)))),
           ToggleShowLookupSteps,
         ),
         of_shortcut(
           ~action=inject(Globals(Set(Evaluation(ShowFilters)))),
           ToggleShowStepperFilters,
         ),
         of_shortcut(
           ~action=inject(Globals(Set(Evaluation(ShowHiddenSteps)))),
           ToggleShowHiddenSteps,
         ),
         of_shortcut(
           ~action=inject(Globals(Set(Sidebar(ToggleShow)))),
           ToggleShowSidebar,
         ),
         of_shortcut(
           ~action=inject(Globals(Set(ExplainThis(ToggleShowFeedback)))),
           ToggleShowDocsFeedback,
         ),
         of_shortcut(
           ~action=inject(Globals(Set(CompletionDisplay(Quiver)))),
           CompletionDisplayQuiver,
         ),
         of_shortcut(
           ~action=inject(Globals(Set(CompletionDisplay(Flag)))),
           CompletionDisplayFlag,
         ),
         of_shortcut(
           ~action=inject(Globals(Set(CompletionDisplay(Hidden)))),
           CompletionDisplayNone,
         ),
         /* Export / Diagnostics */
         of_shortcut(~action=inject(Globals(ExportForInit)), ExportForInit),
         of_shortcut(~action=inject(Benchmark(Start)), RunBenchmark),
       ]);
  };
};

/* single-slot vdom memo for the outline sidebar: the roll-up walk,
   row construction and diff are O(program) per render at 4k (ledger
   §14); its inputs change on Force frames and outline interaction,
   not per keystroke. Key parts compare physically where the value is
   rebuilt-on-change (statics, the DefStatics slot, test results) and
   structurally where small. */
type outline_memo_key = {
  ok_statics: Haz3lcore.CachedStatics.t,
  ok_slot: option(Haz3lcore.DefStatics.t),
  ok_focused: list((Haz3lcore.Id.t, option(string))),
  ok_is_scratch: bool,
  ok_name: string,
  ok_collapsed: list(OutlineTree.path),
  ok_menu: option((Haz3lcore.Id.t, bool, float, float)),
  ok_results: option(Language.TestResults.t),
  ok_view: option(SlideView.t),
  ok_cursor: option(OutlineTree.path),
  ok_edit: option(OutlineEdit.t),
  ok_created: option((Haz3lcore.Id.t, string)),
};
let outline_memo: ref(option((outline_memo_key, Virtual_dom.Vdom.Node.t))) =
  ref(Option.none);
let outline_key_same = (a: outline_memo_key, b: outline_memo_key): bool =>
  a.ok_statics === b.ok_statics
  && (
    switch (a.ok_slot, b.ok_slot) {
    | (Some(x), Some(y)) => x === y
    | (None, None) => true
    | _ => false
    }
  )
  && a.ok_focused == b.ok_focused
  && a.ok_is_scratch == b.ok_is_scratch
  && a.ok_name == b.ok_name
  && a.ok_collapsed == b.ok_collapsed
  && a.ok_menu == b.ok_menu
  && a.ok_view == b.ok_view
  && a.ok_cursor == b.ok_cursor
  && a.ok_edit == b.ok_edit
  && a.ok_created == b.ok_created
  && (
    switch (a.ok_results, b.ok_results) {
    | (Some(x), Some(y)) => x === y
    | (None, None) => true
    | _ => false
    }
  );

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
        /* Leave focus alone when it is moving INTO a projector. An
           interactive projector (the keybinding recorder) needs to hold
           focus; without this guard it receives focus and has it taken back
           in the same frame, so it can never capture a key. */
        if (! JsUtil.projector_holds_focus^) {
          JsUtil.focus_clipboard_shim();
        };
        model.globals.meta_down
          ? Effect.Many([inject(Globals(SetMetaDown(false)))])
          : Effect.Ignore;
      }),
      Attr.on_focus(_ => {
        /* Focus events bubble here, so without this guard focusing a
           projector is undone immediately: an interactive projector (the
           keybinding recorder) could never hold focus or capture a key. */
        if (! JsUtil.projector_holds_focus^) {
          JsUtil.focus_clipboard_shim();
        };
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
              [
                Editors.View.config_links(~inject),
                ...NutMenu.settings_menu(~globals),
              ],
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
    let tutorial_reference =
      switch (editors) {
      | Tutorial(t) =>
        TutorialReferencePanel.of_lesson(
          TutorialsMode.Model.get_current(t).editors,
        )
      | _ => None
      };
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
        ~tutorial_reference,
      );
    /* culling bounds apply only where the mode supports them (one
       cull-scope cell); elsewhere every cell renders unculled */
    let editors_globals =
      Editors.Model.supports_viewport_culling(model.editors)
        ? globals
        : {
          ...globals,
          visible_rows: None,
        };
    let editors_view =
      Editors.View.view(
        ~globals=editors_globals,
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
    /* the outline reads the whole program, not the cell with the caret */
    let program =
      switch (model.editors) {
      | Scratch(m)
      | Documentation(m) =>
        switch (List.nth_opt(m.scratchpads, m.current)) {
        | Some({kind: Code({program, _}), _}) => Some(program)
        | _ => None
        }
      | _ => None
      };
    let outline_statics =
      switch (program) {
      | Some(p) => Program.statics(p)
      | None => current_editor.statics
      };
    /* the row holding the editor's caret, or its nearest visible
       ancestor when collapsed away (OutlineFollow) */
    let outline_mark: option(Haz3lcore.Id.t) =
      switch (program) {
      | None => Option.none
      | Some(p) =>
        let term = outline_statics.term;
        let rows = OutlineTree.row_ids(term);
        let row_of = id =>
          switch (Haz3lcore.Id.Map.find_opt(id, outline_statics.info_map)) {
          | Some(info) =>
            List.find_opt(
              x => Haz3lcore.Id.Map.mem(x, rows),
              [id, ...Language.Info.ancestors_of(info)],
            )
          | None => None
          };
        /* the indicated term, else the enclosing tiles, else the
           neighbours (a caret in a comment or between items) */
        let z = current_editor.editor.state.zipper;
        let candidates =
          Option.to_list(Haz3lcore.Indicated.index(z))
          @ List.map(
              ((a: Haz3lcore.Ancestor.t, _)) => a.id,
              z.relatives.ancestors,
            )
          @ (
            switch (Haz3lcore.Siblings.neighbors(z.relatives.siblings)) {
            | (l, r) =>
              List.filter_map(
                x => Option.map(Haz3lcore.Piece.id, x),
                [r, l],
              )
            }
          );
        let row =
          switch (List.find_map(row_of, candidates), p) {
          | (Some(r), _) => Some(r)
          | (None, Divided(d)) => Option.map(fst, Divided.active(d))
          | (None, Whole(_)) => None
          };
        let (prefix, name) =
          switch (model.editors) {
          | Scratch(m) => (
              "scratch",
              List.nth(m.scratchpads, m.current).name,
            )
          | Documentation(m) => (
              "doc",
              List.nth(m.scratchpads, m.current).name,
            )
          | _ => ("", "")
          };
        let collapsed = ScratchMode.collapse_paths(prefix, name);
        Option.map(
          r =>
            switch (OutlineTree.trail_of(r, term)) {
            | Some(trail) =>
              List.find_opt(
                id =>
                  id != r
                  && (
                    switch (OutlineTree.label_path(id, term)) {
                    | Some(path) => List.mem(path, collapsed)
                    | None => false
                    }
                  ),
                trail,
              )
              |> Option.value(~default=r)
            | None => r
            },
          row,
        );
      };
    OutlineFollow.mark := outline_mark;
    let outline_marks =
      create(
        "style",
        switch (outline_mark) {
        | Some(id) => [text(OutlineFollow.css(id))]
        | None => []
        },
      );
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
      let (slide_prefix, slide_name) =
        switch (model.editors) {
        | Scratch(m) => (
            "scratch",
            switch (List.nth_opt(m.scratchpads, m.current)) {
            | Some(sp) => sp.name
            | None => ""
            },
          )
        | Documentation(m) => (
            "doc",
            switch (List.nth_opt(m.scratchpads, m.current)) {
            | Some(sp) => sp.name
            | None => ""
            },
          )
        | _ => ("", "")
        };
      let collapsed_paths =
        is_scratch
          ? ScratchMode.collapse_paths(slide_prefix, slide_name) : [];
      let menu = is_scratch ? ScratchMode.outline_menu^ : None;
      let test_results =
        switch (model.editors) {
        | Scratch(m)
        | Documentation(m) =>
          switch (
            List.nth_opt(m.scratchpads, m.current)
            |> Option.map((sp: ScratchMode.Scratchpad.t) => sp.kind)
          ) {
          | Some(Code({program, _})) =>
            EvalResult.Model.test_results(Program.result(program))
          | _ => None
          }
        | _ => None
        };
      let slide_view =
        switch (model.editors) {
        | Scratch(m)
        | Documentation(m) => ScratchMode.Model.current_view(m)
        | _ => None
        };
      let memo_key = {
        ok_cursor: ScratchMode.outline_cursor^,
        ok_edit: ScratchMode.outline_edit^,
        ok_created: ScratchMode.outline_created^,
        ok_view: slide_view,
        ok_statics: outline_statics,
        ok_slot: Haz3lcore.DefStatics.current(),
        ok_focused: focused_entries,
        ok_is_scratch: is_scratch,
        ok_name: slide_name,
        ok_collapsed: collapsed_paths,
        ok_menu: menu,
        ok_results: test_results,
      };
      switch (outline_memo^) {
      | Some((k, node)) when outline_key_same(k, memo_key) => node
      | _ =>
        let node = {
          /* error attribution at OUTLINE granularity: each error badges the
             DEEPEST row containing it; ancestor rows get a roll-up badge
             that CSS shows only while collapsed (andrew: error goes on the
             deepest thing not hidden by a collapse) */
          /* A whole program's statics can be EMPTY right after an undo
             restores a compacted snapshot: the DefStatics slot stands in
             until they recompute. Other modes read only the current
             editor: the slot is not theirs. */
          let slot =
            is_scratch
            && !
                 List.exists(
                   (n: OutlineTree.node) => n.o_label != "",
                   OutlineTree.of_term(outline_statics.term),
                 )
              ? Haz3lcore.DefStatics.current() : None;
          let outline_term =
            switch (slot) {
            | Some(ds) => ds.Haz3lcore.DefStatics.term
            | None => outline_statics.term
            };
          let (error_items, error_subtree) = {
            let term = outline_term;
            let (info_map, error_ids) =
              switch (slot) {
              | Some(ds) => (
                  ds.merged,
                  Haz3lcore.DefStatics.all_error_ids(ds),
                )
              | None => (outline_statics.info_map, outline_statics.error_ids)
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
                  | Some(info) => [
                      err_id,
                      ...Language.Info.ancestors_of(info),
                    ]
                  | None => [err_id]
                  };
                switch (List.filter(in_outline, path)) {
                | [] => (direct, roll)
                | [deepest, ...above] => (
                    [deepest, ...direct],
                    above @ roll,
                  )
                };
              },
              ([], []),
              error_ids,
            );
          };
          OutlineSidebar.view(
            ~stack_controls=is_scratch,
            ~can_open={
              let incomplete =
                Haz3lcore.Segment.incomplete_tiles_deep(
                  switch (program) {
                  | Some(Divided(d)) => Divided.document(d)
                  | _ => current_editor.editor.syntax.segment
                  },
                )
                |> List.map((t: Haz3lcore.Tile.t) => t.id);
              id => !List.mem(id, incomplete);
            },
            ~jump=id => globals.inject_global(JumpToTile(id)),
            /* plain click with a stack open ADDS (or moves to) that cell —
               never replaces the stack (andrew: replacing was a footgun) */
            ~focus=id => inject(Editors(Scratch(FocusEnsure(id)))),
            ~toggle=id => inject(Editors(Scratch(FocusToggle(id)))),
            ~toggle_run=id => inject(Editors(Scratch(FocusToggleRun(id)))),
            ~is_collapsed=path => List.mem(path, collapsed_paths),
            ~toggle_collapse=
              path => inject(Editors(Scratch(OutlineCollapse(path)))),
            ~error_items,
            ~error_subtree,
            ~header={
              let label = id =>
                switch (OutlineTree.node_of(id, outline_term)) {
                | Some(n) => n.o_label
                | None => ""
                };
              let (h_open, h_parked) =
                switch (slide_view) {
                | Some(v) =>
                  let n =
                    List.length(SlideView.visible(~term=outline_term, v));
                  v.parked ? (0, n) : (n, 0);
                | None => (0, 0)
                };
              {
                h_program: slide_name,
                h_trail:
                  switch (slide_view) {
                  | Some(v) => List.map(id => (id, label(id)), v.zoom)
                  | None => []
                  },
                h_open,
                h_parked,
              };
            },
            ~zoom_root=Option.bind(slide_view, SlideView.zoom_root),
            ~zoom_to=m => inject(Editors(Scratch(ZoomTo(m)))),
            ~zoom_in=id => inject(Editors(Scratch(ZoomIn(id)))),
            ~show_whole=b => inject(Editors(Scratch(ShowWhole(b)))),
            ~discard=inject(Editors(Scratch(UnfocusDef))),
            ~zoom_out=inject(Editors(Scratch(ZoomOut))),
            ~cursor=ScratchMode.outline_cursor^,
            ~get_cursor=() => ScratchMode.outline_cursor^,
            /* the ref moves at the keypress; the action re-renders */
            ~set_cursor=
              c => {
                ScratchMode.outline_cursor := c;
                inject(Editors(Scratch(OutlineCursor(c))));
              },
            ~focused=inject(Editors(Scratch(OutlineFocused))),
            ~edit={
              current: ScratchMode.outline_edit^,
              get: () => ScratchMode.outline_edit^,
              set: e => {
                ScratchMode.outline_edit := e;
                inject(Editors(Scratch(OutlineEdit(e))));
              },
              commit: (ed, then_new) => {
                ScratchMode.outline_edit := None;
                inject(Editors(Scratch(OutlineCommit(ed, then_new))));
              },
            },
            ~created=ScratchMode.outline_created^,
            ~leave=Effect.of_sync_fun(() => JsUtil.focus_active_editor(), ()),
            ~focused_entries,
            ~menu,
            ~menu_open=
              (id, is_module, x, y) =>
                is_scratch
                  ? inject(
                      Editors(
                        Scratch(OutlineMenu(Some((id, is_module, x, y)))),
                      ),
                    )
                  : Virtual_dom.Vdom.Effect.Ignore,
            ~menu_close=inject(Editors(Scratch(OutlineMenu(None)))),
            ~def_op=
              (op, id) => inject(Editors(Scratch(OutlineDefOp(op, id)))),
            /* live ✓/✗ for test rows, from the master's whole-program
               result (stays live while a stack is open) */
            ~test_status=
              id =>
                Option.bind(test_results, (tr: Language.TestResults.t) =>
                  Language.TestMap.lookup(id, tr.test_map)
                  |> Option.map(Language.TestMap.joint_status)
                ),
            /* the master's statics slot when warm; the DefStatics term
               when the master was restored compacted (undo) */
            outline_term,
          );
        };
        outline_memo := Some((memo_key, node));
        node;
      };
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

    /* Cull only in auto-probe mode (hundreds of probe views) and only for
     * single-code-editor modes. Measured against the editor's own container so
     * it's correct whether the editor fills #main or sits below prompt cells. */
    let on_scroll = (_evt: Js.t(Dom_html.event)) => {
      let culling_enabled =
        Editors.Model.supports_viewport_culling(editors)
        && globals.settings.autoprobe_mode != Haz3lcore.AutoProbe.Off;
      if (!culling_enabled) {
        Effect.Ignore;
      } else {
        switch (JsUtil.code_viewport_geometry()) {
        | None => Effect.Ignore
        | Some((scroll_top, client_height)) =>
          let new_visible =
            Globals.VisibleRows.compute(
              ~scroll_top,
              ~client_height,
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
      outline_marks,
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
    /* projector views can only dispatch external_actions, so toggles that
     * update global Settings call out through these refs */
    Haz3lcore.ProbeProj.Settings.on_sticky_toggle :=
      (() => inject(Globals(Set(SampleStickyInPlace))));
    let cursor =
      Selection.get_cursor_info(~inject, ~selection=model.selection, model);
    NinjaKeys.initialize(
      ~overrides=model.globals.settings.shortcut_overrides,
      cursor.contextual_actions,
    );
    div(
      ~attrs=[Attr.id("page"), ...handlers(~inject, model)],
      [FontSpecimen.view, JsUtil.clipboard_shim]
      @ main_view(~log_model, ~get_log_and, ~cursor, ~inject, model),
    );
  };
};
