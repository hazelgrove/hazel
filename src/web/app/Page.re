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
    | MakeActive(selection)
    | Benchmark(benchmark_action)
    | Refresh
    | Start
    | Save;

  let equal = (===);

  // Get full CellEditor including evaluation result (for App View sidebar)
  let get_cell_editor = (model: Model.t): option(CellEditor.Model.t) => {
    let scratchpad_editor = (sp: ScratchMode.Scratchpad.t) =>
      switch (sp.kind) {
      | Code({editor, _}) => Some(editor)
      | Drv(_) => None
      };
    switch (model.editors) {
    | Scratch(m) => scratchpad_editor(List.nth(m.scratchpads, m.current))
    | Documentation(m) =>
      scratchpad_editor(List.nth(m.scratchpads, m.current))
    | Tutorial(_)
    | Exercises(_) => None // These have different cell structures
    };
  };

  // Evaluate a Hazel expression with full elaboration (for source-level exprs)
  let evaluate_exp = (exp: Language.DHExp.t): Language.DHExp.t =>
    Language.(
      let (_info_map, elaborated) =
        Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), exp);
      fst(Evaluator.evaluate(~env=Builtins.env_init, elaborated))
    );

  // Evaluate without re-elaboration (for applying already-evaluated functions)
  let evaluate_direct = (exp: Language.DHExp.t): Language.DHExp.t =>
    Language.(fst(Evaluator.evaluate(~env=Builtins.env_init, exp)));
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
    | SetAppViewModel(new_model) =>
      switch (model.globals.app_view_state) {
      | Some(state) =>
        let html =
          evaluate_exp(
            Language.IdTagged.FreshGrammar.Exp.ap(
              Forward,
              state.view_fn,
              new_model,
            ),
          );
        let subs =
          evaluate_exp(
            Language.IdTagged.FreshGrammar.Exp.ap(
              Forward,
              state.subs_fn,
              new_model,
            ),
          );
        {
          ...model,
          globals: {
            ...model.globals,
            app_view_state:
              Some({
                ...state,
                model: new_model,
                html,
                subs,
              }),
          },
        }
        |> return_quiet;
      | None => model |> return_quiet // No app initialized yet
      }
    | AppViewMsg(msg) =>
      switch (model.globals.app_view_state) {
      | Some(state) when Option.is_some(state.update_fn) =>
        try({
          let update_fn = Option.get(state.update_fn);
          // Use evaluate_direct: sub-expressions are already elaborated+evaluated
          Js_of_ocaml.Firebug.console##log(
            Js_of_ocaml.Js.string("AppViewMsg: dispatching msg"),
          );
          let update_result =
            evaluate_direct(
              Language.IdTagged.FreshGrammar.Exp.ap(
                Forward,
                update_fn,
                Language.IdTagged.FreshGrammar.Exp.tuple([msg, state.model]),
              ),
            );
          Js_of_ocaml.Firebug.console##log(
            Js_of_ocaml.Js.string("AppViewMsg: update evaluated OK"),
          );
          // Extract (new_model, cmd) tuple — update always returns (Model, Cmd)
          let update_result =
            Haz3lcore.HazelDOM.strip_wrappers(update_result);
          let (new_model, cmd) =
            switch (update_result.term) {
            | Tuple([m, c]) =>
              Js_of_ocaml.Firebug.console##log(
                Js_of_ocaml.Js.string(
                  "AppViewMsg: extracted (model, cmd) tuple",
                ),
              );
              (m, c);
            | _ =>
              Js_of_ocaml.Firebug.console##warn(
                Js_of_ocaml.Js.string(
                  "AppViewMsg: update result is NOT a tuple, using fallback",
                ),
              );
              (
                update_result,
                Language.IdTagged.FreshGrammar.Exp.constructor(
                  "CmdNone",
                  None,
                ),
              );
            };
          let html =
            evaluate_direct(
              Language.IdTagged.FreshGrammar.Exp.ap(
                Forward,
                state.view_fn,
                new_model,
              ),
            );
          Js_of_ocaml.Firebug.console##log(
            Js_of_ocaml.Js.string("AppViewMsg: view evaluated OK"),
          );
          let subs =
            evaluate_direct(
              Language.IdTagged.FreshGrammar.Exp.ap(
                Forward,
                state.subs_fn,
                new_model,
              ),
            );
          Js_of_ocaml.Firebug.console##log(
            Js_of_ocaml.Js.string("AppViewMsg: subs evaluated OK"),
          );
          {
            // Run cmd (CmdRunner handles CmdNone as no-op)

            let cmd_ctx: Haz3lcore.CmdRunner.context = {
              model: new_model,
              inject: m => {
                schedule_action(Globals(AppViewMsg(m)));
                Virtual_dom.Vdom.Effect.Ignore;
              },
              update_fn: state.update_fn,
            };
            Bonsai.Effect.Expert.handle(
              Haz3lcore.CmdRunner.run(cmd_ctx, cmd),
            );
          };
          {
            ...model,
            globals: {
              ...model.globals,
              app_view_state:
                Some({
                  ...state,
                  model: new_model,
                  html,
                  subs,
                }),
            },
          }
          |> return_quiet;
        }) {
        | exn =>
          Js_of_ocaml.Firebug.console##error(
            Js_of_ocaml.Js.string(
              "AppViewMsg EXCEPTION: " ++ Printexc.to_string(exn),
            ),
          );
          model |> return_quiet;
        }
      | Some(_) => model |> return_quiet // Legacy app: no update_fn
      | None => model |> return_quiet
      }
    | InitAppView(source_result, init_model, update_fn, view_fn, subs_fn) =>
      let html =
        evaluate_direct(
          Language.IdTagged.FreshGrammar.Exp.ap(Forward, view_fn, init_model),
        );
      let subs =
        evaluate_direct(
          Language.IdTagged.FreshGrammar.Exp.ap(Forward, subs_fn, init_model),
        );
      let state: Globals.AppViewState.t = {
        source_result,
        model: init_model,
        update_fn,
        view_fn,
        subs_fn,
        html,
        subs,
      };
      {
        ...model,
        globals: {
          ...model.globals,
          app_view_state: Some(state),
        },
      }
      |> return_quiet;
    | RefreshAppView(source_result, init_model, update_fn, view_fn, subs_fn) =>
      // Code changed - try to preserve current model state (hot reload)
      // Only preserve model if init_model has compatible structure (same program edited)
      // Otherwise re-init (different program loaded, e.g. switching tabs)
      switch (model.globals.app_view_state) {
      | Some(state) =>
        // Check if model structures are compatible (same term kind and arity)
        let models_compatible =
          switch (
            Haz3lcore.HazelDOM.strip_wrappers(state.model).term,
            Haz3lcore.HazelDOM.strip_wrappers(init_model).term,
          ) {
          | (Atom(Int(_)), Atom(Int(_)))
          | (Atom(Float(_)), Atom(Float(_)))
          | (Atom(String(_)), Atom(String(_)))
          | (Atom(Bool(_)), Atom(Bool(_))) => true
          | (Tuple(xs), Tuple(ys)) => List.length(xs) == List.length(ys)
          | (ListLit(xs), ListLit(ys)) => List.length(xs) == List.length(ys)
          | _ => false
          };
        let model_to_use =
          if (models_compatible) {
            state.model;
          } else {
            init_model;
          };
        let new_state =
          try({
            let html =
              evaluate_direct(
                Language.IdTagged.FreshGrammar.Exp.ap(
                  Forward,
                  view_fn,
                  model_to_use,
                ),
              );
            let subs =
              evaluate_direct(
                Language.IdTagged.FreshGrammar.Exp.ap(
                  Forward,
                  subs_fn,
                  model_to_use,
                ),
              );
            Globals.AppViewState.{
              source_result,
              model: model_to_use,
              update_fn,
              view_fn,
              subs_fn,
              html,
              subs,
            };
          }) {
          | _exn =>
            // Evaluation failed - full re-init with init_model
            let html =
              evaluate_direct(
                Language.IdTagged.FreshGrammar.Exp.ap(
                  Forward,
                  view_fn,
                  init_model,
                ),
              );
            let subs =
              evaluate_direct(
                Language.IdTagged.FreshGrammar.Exp.ap(
                  Forward,
                  subs_fn,
                  init_model,
                ),
              );
            Globals.AppViewState.{
              source_result,
              model: init_model,
              update_fn,
              view_fn,
              subs_fn,
              html,
              subs,
            };
          };
        {
          ...model,
          globals: {
            ...model.globals,
            app_view_state: Some(new_state),
          },
        }
        |> return_quiet;
      | None =>
        // Shouldn't happen (refresh implies existing state), but handle as init
        let html =
          evaluate_exp(
            Language.IdTagged.FreshGrammar.Exp.ap(
              Forward,
              view_fn,
              init_model,
            ),
          );
        let subs =
          evaluate_exp(
            Language.IdTagged.FreshGrammar.Exp.ap(
              Forward,
              subs_fn,
              init_model,
            ),
          );
        let state: Globals.AppViewState.t = {
          source_result,
          model: init_model,
          update_fn,
          view_fn,
          subs_fn,
          html,
          subs,
        };
        {
          ...model,
          globals: {
            ...model.globals,
            app_view_state: Some(state),
          },
        }
        |> return_quiet;
      }
    | ResetAppView =>
      // Clean up sidebar subscriptions before resetting
      Haz3lcore.HazelDOM.cleanup_sidebar_subscriptions();
      {
        ...model,
        globals: {
          ...model.globals,
          app_view_state: None,
        },
      }
      |> return_quiet;
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
          let filename =
            (current.name |> StringUtil.sanitize_filename) ++ ".ml";
          let contents =
            switch (current.kind) {
            | Code({editor, _}) =>
              let serialized =
                Haz3lcore.(
                  [%derive.show: (string, PersistentSegment.t)]((
                    current.name,
                    editor.editor.editor.state.zipper
                    |> PersistentSegment.persist,
                  ))
                );
              "let out : string * Haz3lcore.PersistentSegment.t = "
              ++ serialized;
            | Drv(m) => DerivationExercise.export_doc_slide_module(m.editors)
            };
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
    let start = TimeUtil.now_ms();
    let globals = {
      ...model.globals,
      export_all: Export.export_all,
      get_log_and,
    };
    let result =
      switch (action) {
      | Globals(action) =>
        update_global(~globals, ~import_log, ~schedule_action, action, model)
      | Editors(action) =>
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
    TimeUtil.log_time("Page.update TOTAL", start);
    result;
  };

  let can_undo = (action: t) => {
    switch (action) {
    | Globals(action) => Globals.Update.can_undo(action)
    | Editors(action) => Editors.Update.can_undo(action)
    | ExplainThis(action) => ExplainThisUpdate.can_undo(action)
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
      /* Skip page-level shortcuts when the user is typing in a form
         element (e.g. an <input>/<textarea>/<select> rendered by a
         HazelDOM sidebar app). The clipboard shim is a textarea but
         needs page handling, so it carves out by id. */
      let target_is_input =
        switch (key.target_tag) {
        | Some("INPUT" | "TEXTAREA" | "SELECT") =>
          key.target_id != Some(JsUtil.clipboard_shim_id)
        | _ => false
        };
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
        | Some(action) when !target_is_input =>
          Many(
            [Prevent_default, Stop_propagation, inject(action)]
            @ meta_effects,
          )
        | _ => meta_effects == [] ? Ignore : Many(meta_effects)
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
        ~cell_editor=Update.get_cell_editor(model),
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
    ];
  };

  let view =
      (
        ~log_model,
        ~get_log_and,
        ~inject: Update.t => Ui_effect.t(unit),
        model: Model.t,
      ) => {
    let start = TimeUtil.now_ms();
    let cursor =
      Selection.get_cursor_info(~inject, ~selection=model.selection, model);
    NinjaKeys.initialize(cursor.contextual_actions);
    let result =
      div(
        ~attrs=[Attr.id("page"), ...handlers(~inject, model)],
        [FontSpecimen.view, JsUtil.clipboard_shim]
        @ main_view(~log_model, ~get_log_and, ~cursor, ~inject, model),
      );
    TimeUtil.log_time("Page.view TOTAL", start);
    result;
  };
};
