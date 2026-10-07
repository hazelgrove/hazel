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

/* undo and redo restore programs; what each slide shows stays */
let carry_views = (~from: Model.t, m: Model.t): Model.t =>
  switch (from.editors, m.editors) {
  | (Scratch(a), Scratch(b)) => {
      ...m,
      editors: Scratch(ScratchMode.Model.with_views_of(~from=a, b)),
    }
  | (Documentation(a), Documentation(b)) => {
      ...m,
      editors: Documentation(ScratchMode.Model.with_views_of(~from=a, b)),
    }
  | _ => m
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
                /* header too: binder and signature errors live there.
                   a ⇒ or `;` cell shows none, and its empty one is a hole */
                (Option.is_none(e.e_sym) ? [e.e_header.editor] : [])
                @ [e.e_body.editor],
              ),
            cells,
          );
        /* cells first: the panel dedups by id, so a problem shows in its
           cell and "elsewhere" gets the rest */
        stack @ [(Some("elsewhere"), [Divided.outside_editor(d)])];
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
          visible_rows:
            Globals.VisibleRows.tracked(settings)
              ? model.globals.visible_rows : None,
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
      switch (Editors.Selection.closed_jump(id, model.editors)) {
      | Some((ensure, selection, caret)) =>
        /* outside every open cell: open its item, then move there */
        schedule_action(Editors(caret));
        Haz3lcore.FocusEffect.schedule_cell_top();
        let* editors =
          Editors.Update.update(
            ~globals,
            ~schedule_action=a => schedule_action(Editors(a)),
            ~schedule_global=a => schedule_action(Globals(a)),
            ensure,
            model.editors,
          );
        {
          ...model,
          editors,
          selection,
        };
      | None =>
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
          /* the jump moves the selection, not DOM focus: focus the cell
             after render so it takes keys and shows the caret (gated on
             :focus), unless the jump came from the outline; a target out
             of view comes near the top */
          Haz3lcore.FocusEffect.schedule_cell_caret_top();
          {
            ...model,
            editors,
            selection,
          };
        };
      }
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
    | UpdateDrawerWidth(cols) =>
      Haz3lcore.ProbeProj.Settings.set_drawer_width(cols);
      model |> Updated.return_quiet(~recalculate=true);
    | RelayoutDrawers => model |> Updated.return_quiet(~recalculate=true)
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
      /* a stack cell's jump to a binder in another definition becomes:
         stack the target, select it, then jump the caret (as JumpToTile) */
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
      /* an outline add selects and focuses the new cell (focus also
         scrolls it into view) */
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
      /* an unchanged selection whose cell closed falls back to an open
         one; a fresh one already names its target */
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
          ~tail_probe=model.globals.settings.tail_probe,
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
    /* the slide program's dynamics, at the inspector's right end */
    let dynamics =
      switch (editors) {
      | Scratch(m)
      | Documentation(m) when globals.settings.core.dynamics =>
        ScratchMode.Model.current_program(m)
        |> Option.map(p => {
             let tail = globals.settings.tail_probe;
             /* in a stack, the value shows in the ⇒ cell: open it */
             let open_tail =
               switch (p, Program.tail_row(p)) {
               | (Program.Divided(_), Some(id)) when !tail => [
                   inject(Editors(Scratch(Workspace(FocusEnsure(id))))),
                 ]
               | _ => []
               };
             EvalResult.View.dynamics(
               ~inject=
                 a =>
                   inject(
                     Editors(
                       Scratch(Workspace(CellAction(ResultAction(a)))),
                     ),
                   ),
               ~tail,
               ~toggle_tail=
                 Effect.Many(
                   [inject(Globals(Set(TailProbe)))] @ open_tail,
                 ),
               Program.result(p),
             );
           })
      | _ => None
      };
    let bottom_bar = CursorInspector.view(~globals, ~dynamics?, cursor);
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
    let deck: option(OutlineControl.deck) =
      switch (model.editors) {
      | Scratch(m) => Some(("scratch", m))
      | Documentation(m) => Some(("doc", m))
      | _ => None
      };
    let outline_mark =
      OutlineControl.mark(~deck, ~zipper=current_editor.editor.state.zipper);
    OutlineFollow.mark := outline_mark;
    let outline_marks =
      create(
        "style",
        switch (outline_mark) {
        | Some(id) => [text(OutlineFollow.css(id))]
        | None => []
        },
      );
    let outline =
      OutlineControl.view(
        ~deck,
        ~statics=current_editor.statics,
        ~segment=current_editor.editor.syntax.segment,
        ~inject=a => inject(Editors(Scratch(Outline(a)))),
        ~inject_workspace=a => inject(Editors(Scratch(Workspace(a)))),
        ~jump=id => globals.inject_global(JumpToTile(id)),
        ~leave=Effect.of_sync_fun(() => JsUtil.focus_active_editor(), ()),
      );
    let indicated_id =
      Haz3lcore.Indicated.index(current_editor.editor.state.zipper);
    let closure_cursor_bar =
      SampleFocusBar.view(
        ~globals,
        ~refractors=current_editor.editor.state.zipper.refractors,
        ~info_map=current_editor.statics.info_map,
        ~indicated_id,
      );

    /* Track the range only while something culls by it and only for
     * single-code-editor modes. Measured against the editor's own container so
     * it's correct whether the editor fills #main or sits below prompt cells. */
    let on_scroll = (_evt: Js.t(Dom_html.event)) => {
      let culling_enabled =
        Editors.Model.supports_viewport_culling(editors)
        && Globals.VisibleRows.tracked(globals.settings);
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
