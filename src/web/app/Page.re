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
    adventure: AdventureModel.t,
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
      adventure: AdventureModel.inactive,
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
      adventure: AdventureModel.inactive,
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
      | Code({editor, _}) => [(None, [editor.editor])]
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
    | Adventure(AdventureUpdate.t)
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

  /* Create a new scratch slide for an adventure and switch to it */
  let create_adventure_slide = (editors: Editors.Model.t): Editors.Model.t => {
    let existing_scratchpads =
      switch (editors) {
      | Scratch(m) => m.scratchpads
      | Documentation(_)
      | Tutorial(_)
      | Exercises(_) => []
      };

    /* Generate unique name */
    let base_name = "Adventure";
    let existing_names =
      List.map(
        (s: ScratchMode.Scratchpad.t) => s.name,
        existing_scratchpads,
      );
    let rec find_unique_name = (suffix: int) => {
      let candidate =
        suffix == 0
          ? base_name : base_name ++ " (" ++ string_of_int(suffix) ++ ")";
      if (List.mem(candidate, existing_names)) {
        find_unique_name(suffix + 1);
      } else {
        candidate;
      };
    };
    let slide_name = find_unique_name(0);

    /* Create new blank slide and append */
    let new_scratchpads =
      existing_scratchpads @ [ScratchMode.Scratchpad.blank_code(slide_name)];

    /* Switch to Scratch mode with new slide as current */
    Editors.Model.Scratch({
      current: List.length(new_scratchpads) - 1,
      scratchpads: new_scratchpads,
    });
  };

  /* Apply adventure result side effects:
   * - Schedules editor_actions from the result
   * - Captures checkpoint if set_checkpoint is true
   * - Schedules reset actions if reset_to_checkpoint is true
   * Returns the updated adventure model. */
  let apply_adventure_result =
      (
        ~schedule_action: t => unit,
        ~zipper: Haz3lcore.Zipper.t,
        result: AdventureUpdate.update_result,
      )
      : AdventureModel.t => {
    /* Schedule any editor actions from the adventure */
    List.iter(
      a => schedule_action(Globals(ActiveEditor(a))),
      result.editor_actions,
    );

    /* Handle checkpoint capture or reset */
    if (result.set_checkpoint) {
      {
        ...result.model,
        checkpoint: Some(zipper),
      };
    } else if (result.reset_to_checkpoint) {
      switch (result.model.checkpoint) {
      | Some(checkpoint_zipper) =>
        /* Paste is text-only on dev; restore via printed checkpoint */
        let text =
          Haz3lcore.Printer.of_zipper(
            ~holes="",
            ~indent="",
            checkpoint_zipper,
          );
        List.iter(
          a => schedule_action(Globals(ActiveEditor(a))),
          [
            Haz3lcore.Action.Select(All),
            Haz3lcore.Action.Destruct(Left),
            Haz3lcore.Action.Paste(text),
          ],
        );
        result.model;
      | None => result.model
      };
    } else {
      result.model;
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
      /* Block slide/mode navigation while an adventure is active, and user
         cell edits while the tutor holds the turn. Scripted adventure
         actions bypass this via Globals(ActiveEditor). */
      let adventure_blocked =
        model.adventure.active
        && (
          switch (action) {
          | Editors.Update.SwitchMode(_)
          | Scratch(SwitchSlide(_))
          | Tutorial(SwitchExercise(_))
          | Exercises(SwitchExercise(_)) => true
          | Scratch(CellAction(MainEditor(_))) =>
            AdventureModel.is_editor_locked(model.adventure)
          | _ => false
          }
        );
      if (adventure_blocked) {
        model |> return_quiet;
      } else {
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
        };
      };
    | ExplainThis(action) =>
      let* explain_this =
        ExplainThisUpdate.set_update(model.explain_this, action);
      {
        ...model,
        explain_this,
      };
    | Adventure(action) =>
      /* Handle Start specially to create adventure slide */
      let model =
        switch (action) {
        | Start(_) =>
          let editors = create_adventure_slide(model.editors);
          {
            ...model,
            editors,
          };
        | _ => model
        };

      let result = AdventureUpdate.update(action, model.adventure);
      let zipper = get_editor(model).editor.state.zipper;
      let adventure =
        apply_adventure_result(~schedule_action, ~zipper, result);
      {
        ...model,
        adventure,
      }
      |> Updated.return_quiet;
    | MakeActive(selection) =>
      {
        ...model,
        selection,
      }
      |> Updated.return(~is_edit=false, ~scroll_active=false)
    | Benchmark(Start) =>
      List.iter(a => schedule_action(Editors(a)), Benchmark.actions_1);
      schedule_action(Benchmark(Finish));
      Benchmark.start();
      model |> Updated.return_quiet;
    | Benchmark(Finish) =>
      Benchmark.finish();
      model |> Updated.return_quiet;
    | Refresh => model |> Updated.return_quiet(~recalculate=true)
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
    | Adventure(_) => false
    | MakeActive(_)
    | Benchmark(_) => false
    | Refresh => false
    | Start => false
    | Save => false
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

    /* Check adventure gate if active and at a UserGate step.
     * This must happen after editors calculation so statics are available.
     * We always check (not just when is_edited) since the check is cheap. */
    let adventure =
      if (model.adventure.active && AdventureModel.is_at_gate(model.adventure)) {
        let editor =
          get_editor({
            ...model,
            editors,
          });
        let zipper = editor.editor.state.zipper;
        let info_map = editor.statics.info_map;
        let result =
          AdventureUpdate.check_gate(~zipper, ~info_map, model.adventure);
        apply_adventure_result(~schedule_action, ~zipper, result);
      } else {
        model.adventure;
      };

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
    let color_highlights =
      ExplainThis.get_color_map(
        ~globals=model.globals,
        ~explainThisModel=model.explain_this,
        cursor_info.info,
      );
    /* When the user's cursor is inside a derivation tree cell, the
       deduction-specific highlight map takes precedence over the generic
       ExplainThis one. We consult the live selection here (rather than
       Editors.Model.get_derivation_info, which reads the stale `model.pos`
       inside DerivationExerciseMode) so that focus on Prelude/Setup doesn't
       get misclassified as focus on the derivation. */
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
      | None => color_highlights
      };
    let globals = Globals.Update.calculate(color_highlights, model.globals);
    {
      ...model,
      globals,
      editors,
      adventure,
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
        /* Adventure mode toggle: Cmd/Ctrl+Shift+A */
        | {
            key: D("A" | "a"),
            sys: Mac,
            shift: Down,
            meta: Down,
            ctrl: Up,
            alt: Up,
            _,
          }
        | {
            key: D("A" | "a"),
            sys: PC,
            shift: Down,
            meta: Up,
            ctrl: Down,
            alt: Up,
            _,
          } =>
          model.adventure.active
            ? Some(Update.Adventure(Stop))
            : Some(Update.Adventure(Start(AdventureScripts.probes_intro)))
        /* Space advances the adventure during the tutor's turn */
        | {key: D(" "), shift: Up, meta: Up, ctrl: Up, alt: Up, _}
            when
              AdventureModel.is_editor_locked(model.adventure)
              && AdventureModel.can_advance(model.adventure) =>
          Some(Update.Adventure(Advance))
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
          /* Blocked during the tutor's turn */
          AdventureModel.is_editor_locked(model.adventure)
            ? None : Some(Update.Globals(Redo))
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
          AdventureModel.is_editor_locked(model.adventure)
            ? None : Some(Update.Globals(Undo))
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

  let autoprobe_indicator = (~globals: Globals.t, ~inject) => [
    Widgets.toggle(
      ~tooltip="Auto-probe mode active (Cmd/Ctrl+P to toggle)",
      "🔬",
      globals.settings.autoprobe_mode,
      _ =>
      inject(Update.Globals(Set(AutoprobeMode)))
    ),
  ];

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
        {
          globals,
          editors,
          explain_this: explainThisModel,
          adventure: adventureModel,
          selection,
        } as model: Model.t,
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

    /* Blocking overlay when adventure is in locked state (not at UserGate).
     * Covers everything except the adventure dialog to prevent interaction. */
    let adventure_overlay =
      if (AdventureModel.is_editor_locked(adventureModel)) {
        div(
          ~attrs=[
            Attr.id("adventure-overlay"),
            Attr.class_("adventure-overlay"),
            Attr.on_click(_ => inject(Adventure(RequestExit))),
          ],
          [],
        );
      } else {
        Node.none;
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
      bottom_bar,
      ContextInspector.view(~globals, cursor.info),
      HoverRuleSpec.view(~globals),
      adventure_overlay,
      AdventureView.view(~inject=a => inject(Adventure(a)), adventureModel),
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
    /* tutor-turn class marks the tutor's turn (editor locked) for CSS */
    let adventure_class =
      AdventureModel.is_editor_locked(model.adventure)
        ? [Attr.class_("tutor-turn")] : [];
    div(
      ~attrs=
        [Attr.id("page"), ...adventure_class] @ handlers(~inject, model),
      [FontSpecimen.view, JsUtil.clipboard_shim]
      @ main_view(~log_model, ~get_log_and, ~cursor, ~inject, model),
    );
  };
};
