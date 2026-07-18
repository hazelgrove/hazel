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
    action_explorer: ActionExplorer.Model.t,
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
      action_explorer: ActionExplorer.Model.init,
    };
  };

  /* Clear transient agent state (in-flight request flags, agent cursor
     lock) from a snapshot restored via undo/redo, so restoring a model
     captured mid-agent-turn can't soft-lock the UI. */
  let sanitize_restored = (model: t): t => {
    ...model,
    editors: Editors.Model.sanitize_restored(model.editors),
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
      action_explorer: ActionExplorer.Model.init,
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

  let set_editor = (model: Model.t, code_ed: CodeEditable.Model.t): Model.t => {
    let update_pad = (s: ScratchMode.Scratchpad.t): ScratchMode.Scratchpad.t =>
      switch (s.kind) {
      | Code(code) => {
          ...s,
          kind:
            Code({
              ...code,
              editor: {
                ...code.editor,
                editor: code_ed,
              },
            }),
        }
      | Drv(_) => s
      };
    let editors =
      switch (model.editors) {
      | Scratch(m) =>
        let scratchpads =
          List.mapi(
            (i, s: ScratchMode.Scratchpad.t) =>
              i == m.current ? update_pad(s) : s,
            m.scratchpads,
          );
        Editors.Model.Scratch({
          ...m,
          scratchpads,
        });
      | Documentation(m) =>
        let scratchpads =
          List.mapi(
            (i, s: ScratchMode.Scratchpad.t) =>
              i == m.current ? update_pad(s) : s,
            m.scratchpads,
          );
        Documentation({
          ...m,
          scratchpads,
        });
      | _ => model.editors
      };
    {
      ...model,
      editors,
    };
  };

  let with_selector_session =
      (model: Model.t, selector_find: option(Haz3lcore.SelectorFind.session))
      : Model.t => {
    let code_ed = get_editor(model);
    set_editor(
      model,
      {
        ...code_ed,
        selector_find,
      },
    );
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
    | ActionExplorer(ActionExplorer.Update.t)
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

  /* Action Explorer helpers */
  let resolve_action_explorer_path =
      (path: string, model: Model.t): list(Haz3lcore.Id.t) =>
    if (String.length(String.trim(path)) == 0) {
      [];
    } else {
      let editor = get_editor(model);
      let zipper = editor.editor.state.zipper;
      let info_map =
        Language.(
          fst(
            Statics.mk(
              CoreSettings.on,
              Builtins.ctx_init(Some(Operators.default_mode)),
              Haz3lcore.MakeTerm.from_zip_for_sem(zipper, ~root=Exp).term,
            ),
          )
        );
      switch (Haz3lcore.HighLevelNodeMap.build(zipper, info_map)) {
      | None => []
      | Some(node_map) =>
        switch (Haz3lcore.HighLevelNodeMap.path_to_id_opt(node_map, path)) {
        | Some(id) => [id]
        | None => []
        }
      };
    };

  let action_explorer_model_for_session =
      (ae: ActionExplorer.Model.t, session: Haz3lcore.SelectorFind.session)
      : ActionExplorer.Model.t => {
    let ids =
      List.map(
        (m: Haz3lcore.Selector.match_result) => m.focused_id,
        session.matches,
      );
    {
      ...ae,
      highlight_ids: ids,
      active_match_index:
        Haz3lcore.SelectorFind.active_display_index(session),
      active_match_count: Haz3lcore.SelectorFind.length(session),
      active_match_id: Haz3lcore.SelectorFind.active_id(session),
      result_msg: Haz3lcore.SelectorFind.print_active(session),
    };
  };

  let action_explorer_model_clear_matches =
      (ae: ActionExplorer.Model.t, result_msg: option(string))
      : ActionExplorer.Model.t => {
    ...ae,
    highlight_ids: [],
    active_match_index: 0,
    active_match_count: 0,
    active_match_id: None,
    result_msg,
  };

  let globals_for_action_explorer =
      (globals: Globals.Model.t, ae: ActionExplorer.Model.t): Globals.Model.t => {
    ...globals,
    action_highlights: ae.highlight_ids,
    action_active_highlight: ae.active_match_id,
  };

  let start_selector_find_at =
      (~caret_point: Point.t, selector: string, model: Model.t)
      : result(Haz3lcore.SelectorFind.session, string) => {
    let editor = get_editor(model);
    let z = editor.editor.state.zipper;
    let term = Haz3lcore.MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
    Haz3lcore.SelectorFind.start(
      ~selector,
      ~root=term,
      ~caret_point,
      ~syntax=editor.editor.syntax,
    );
  };

  let start_selector_find =
      (selector: string, model: Model.t)
      : result(Haz3lcore.SelectorFind.session, string) => {
    let editor = get_editor(model);
    let z = editor.editor.state.zipper;
    start_selector_find_at(
      ~caret_point=
        Haz3lcore.Zipper.Caret.point(editor.editor.syntax.measured, z),
      selector,
      model,
    );
  };

  let canonical_selector_for_session =
      (session: Haz3lcore.SelectorFind.session, model: Model.t)
      : option(string) => {
    let editor = get_editor(model);
    let z = editor.editor.state.zipper;
    let term = Haz3lcore.MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
    Haz3lcore.SelectorFind.canonical_selector_for_active(session, term);
  };

  let schedule_jump_to_session =
      (~schedule_action: t => unit, session: Haz3lcore.SelectorFind.session) =>
    switch (Haz3lcore.SelectorFind.active_id(session)) {
    | Some(id) => schedule_action(Globals(JumpToTile(id)))
    | None => ()
    };

  let active_session_for_action_explorer =
      (model: Model.t): result(Haz3lcore.SelectorFind.session, string) => {
    let selector = String.trim(model.action_explorer.selector);
    switch (get_editor(model).selector_find) {
    | Some(session) when session.selector == selector => Ok(session)
    | _ => start_selector_find(selector, model)
    };
  };

  let selector_action_with_selector =
      (selector: string, action: Haz3lcore.Action.Structural.t)
      : Haz3lcore.Action.Structural.t =>
    switch (action) {
    | SelectorUpdate(_, code) => SelectorUpdate(selector, code)
    | SelectorDelete(_) => SelectorDelete(selector)
    | SelectorInsertBefore(_, code) => SelectorInsertBefore(selector, code)
    | SelectorInsertAfter(_, code) => SelectorInsertAfter(selector, code)
    | other => other
    };

  let read_action_with_selector =
      (selector: string, action: Haz3lcore.CompositionActions.read_action)
      : Haz3lcore.CompositionActions.read_action =>
    switch (action) {
    | Select(_) => Select(selector)
    | GetCanonical(_) => GetCanonical(selector)
    | SelectorGetStatics(_) => SelectorGetStatics(selector)
    | SelectorGetContext(_) => SelectorGetContext(selector)
    | other => other
    };

  let execute_action_explorer =
      (model: Model.t): result(Editors.Model.t, string) =>
    switch (ActionExplorer.Model.to_structural_action(model.action_explorer)) {
    | None => Error("No action configured.")
    | Some(a) =>
      let a_result =
        switch (a) {
        | SelectorUpdate(_)
        | SelectorDelete(_)
        | SelectorInsertBefore(_)
        | SelectorInsertAfter(_) =>
          switch (active_session_for_action_explorer(model)) {
          | Error(msg) => Error(msg)
          | Ok(session) =>
            switch (canonical_selector_for_session(session, model)) {
            | Some(selector) =>
              Ok(selector_action_with_selector(selector, a))
            | None => Ok(a)
            }
          }
        | _ => Ok(a)
        };
      switch (a_result) {
      | Error(msg) => Error(msg)
      | Ok(a) =>
        let code_ed: CodeEditable.Model.t = get_editor(model);
        let z = code_ed.editor.state.zipper;
        let syntax = Haz3lcore.CachedSyntax.init(z);
        let return = (err, opt_z) =>
          switch (opt_z) {
          | Some(z) => Ok(z)
          | None => Error(err)
          };
        switch (Haz3lcore.CompositionGo.Public.go(~syntax, ~z, ~a, ~return)) {
        | Ok((new_zipper, _warning)) =>
          /* Rebuild CodeEditable.Model.t with new zipper */
          let new_code_ed: CodeEditable.Model.t = {
            ...code_ed,
            selector_find: None,
            editor: {
              ...code_ed.editor,
              state: {
                ...code_ed.editor.state,
                zipper: new_zipper,
              },
            },
          };
          /* TODO: stubbed — see merge brief.
             The dev branch reshaped Scratchpad.t: `{ name; kind = Code({editor; agent}) | Drv(_) }`.
             Updating the inner CodeEditable.Model.t requires unwrapping
             through `kind = Code(code)` and `code.editor: CellEditor.Model.t`.
             Drv scratchpads are not handled here. */
          let update_pad =
              (s: ScratchMode.Scratchpad.t): ScratchMode.Scratchpad.t =>
            switch (s.kind) {
            | Code(code) => {
                ...s,
                kind:
                  Code({
                    ...code,
                    editor: {
                      ...code.editor,
                      editor: new_code_ed,
                    },
                  }),
              }
            | Drv(_) => s
            };
          switch (model.editors) {
          | Scratch(m) =>
            let scratchpads =
              List.mapi(
                (i, s: ScratchMode.Scratchpad.t) =>
                  i == m.current ? update_pad(s) : s,
                m.scratchpads,
              );
            Ok(
              Editors.Model.Scratch({
                ...m,
                scratchpads,
              }),
            );
          | Documentation(m) =>
            let scratchpads =
              List.mapi(
                (i, s: ScratchMode.Scratchpad.t) =>
                  i == m.current ? update_pad(s) : s,
                m.scratchpads,
              );
            Ok(
              Documentation({
                ...m,
                scratchpads,
              }),
            );
          | _ =>
            Error(
              "Action Explorer only supported in Scratch/Documentation modes.",
            )
          };
        | Error(failure) => Error(Haz3lcore.Action.Failure.show(failure))
        };
      };
    };

  let execute_read_action =
      (model: Model.t): result((string, list(Haz3lcore.Id.t)), string) =>
    switch (ActionExplorer.Model.to_read_action(model.action_explorer)) {
    | None => Error("No read action configured.")
    | Some(read_action) =>
      let code_ed: CodeEditable.Model.t = get_editor(model);
      let z = code_ed.editor.state.zipper;
      switch (read_action) {
      | Select(_)
      | GetCanonical(_)
      | SelectorGetStatics(_)
      | SelectorGetContext(_) =>
        switch (active_session_for_action_explorer(model)) {
        | Error(msg) => Error(msg)
        | Ok(session) =>
          let ids =
            List.map(
              (m: Haz3lcore.Selector.match_result) => m.focused_id,
              session.matches,
            );
          switch (read_action) {
          | Select(_) =>
            switch (Haz3lcore.SelectorFind.print_active(session)) {
            | Some(text) =>
              Ok((
                text ++ "\nMatch: " ++ Haz3lcore.SelectorFind.summary(session),
                ids,
              ))
            | None => Error("No active selector match.")
            }
          | _ =>
            let active_read_action =
              switch (canonical_selector_for_session(session, model)) {
              | Some(selector) =>
                read_action_with_selector(selector, read_action)
              | None => read_action
              };
            switch (
              Haz3lcore.CompositionGo.Public.read_dispatch(
                ~action=active_read_action,
                ~z,
              )
            ) {
            | Ok(result_text) => Ok((result_text, ids))
            | Error(failure) =>
              Error(Haz3lcore.Action.Failure.show(failure))
            };
          };
        }
      | _ =>
        switch (
          Haz3lcore.CompositionGo.Public.read_dispatch(
            ~action=read_action,
            ~z,
          )
        ) {
        | Ok(result_text) =>
          /* Resolve the path for highlighting via HighLevelNodeMap */
          let ids =
            resolve_action_explorer_path(model.action_explorer.path, model);
          Ok((result_text, ids));
        | Error(failure) => Error(Haz3lcore.Action.Failure.show(failure))
        }
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
    | ActionExplorer(Execute) =>
      switch (model.action_explorer.action_kind) {
      | Read
      | SelectorRead =>
        switch (execute_read_action(model)) {
        | Ok((result_text, ids)) =>
          {
            ...model,
            action_explorer: {
              ...model.action_explorer,
              result_msg: Some(result_text),
              highlight_ids: ids,
            },
            globals: {
              ...model.globals,
              action_highlights: ids,
              action_active_highlight: model.action_explorer.active_match_id,
            },
          }
          |> Updated.return(~is_edit=false, ~scroll_active=false)
        | Error(msg) =>
          {
            ...model,
            action_explorer: {
              ...model.action_explorer,
              result_msg: Some(msg),
            },
          }
          |> Updated.return(~is_edit=false, ~scroll_active=false)
        }
      | _ =>
        switch (execute_action_explorer(model)) {
        | Ok(editors) =>
          let ae = {
            ...model.action_explorer,
            result_msg: Some("Action applied successfully."),
            highlight_ids: [],
          };
          {
            ...model,
            editors,
            action_explorer: ae,
            globals: {
              ...model.globals,
              action_highlights: [],
              action_active_highlight: None,
            },
          }
          |> Updated.return(~is_edit=true);
        | Error(msg) =>
          {
            ...model,
            action_explorer: {
              ...model.action_explorer,
              result_msg: Some(msg),
            },
          }
          |> Updated.return(~is_edit=false, ~scroll_active=false)
        }
      }
    | ActionExplorer(SetPath(path) as action) =>
      let ae =
        ActionExplorer.Update.update(~action, ~model=model.action_explorer);
      let ids = resolve_action_explorer_path(path, model);
      {
        ...model,
        action_explorer: {
          ...ae,
          highlight_ids: ids,
        },
        globals: {
          ...model.globals,
          action_highlights: ids,
          action_active_highlight: None,
        },
      }
      |> Updated.return(~is_edit=false, ~scroll_active=false);
    | ActionExplorer(SetSelector(selector) as action) =>
      let ae =
        ActionExplorer.Update.update(~action, ~model=model.action_explorer);
      if (String.length(String.trim(selector)) == 0) {
        let ae = action_explorer_model_clear_matches(ae, None);
        {
          ...with_selector_session(model, None),
          action_explorer: ae,
          globals: globals_for_action_explorer(model.globals, ae),
        }
        |> Updated.return(~is_edit=false, ~scroll_active=false);
      } else {
        let anchor_point =
          switch (get_editor(model).selector_find) {
          | Some(session) => session.anchor_point
          | None =>
            let editor = get_editor(model);
            Haz3lcore.Zipper.Caret.point(
              editor.editor.syntax.measured,
              editor.editor.state.zipper,
            );
          };
        switch (
          start_selector_find_at(~caret_point=anchor_point, selector, model)
        ) {
        | Ok(session) =>
          let ae = action_explorer_model_for_session(ae, session);
          {
            ...with_selector_session(model, Some(session)),
            action_explorer: ae,
            globals: globals_for_action_explorer(model.globals, ae),
          }
          |> Updated.return(~is_edit=false, ~scroll_active=false);
        | Error(msg) =>
          let ae = action_explorer_model_clear_matches(ae, Some(msg));
          {
            ...model,
            action_explorer: ae,
            globals: globals_for_action_explorer(model.globals, ae),
          }
          |> Updated.return(~is_edit=false, ~scroll_active=false);
        };
      };
    | ActionExplorer((PrevMatch | NextMatch) as action) =>
      let current = get_editor(model).selector_find;
      let session_result =
        switch (current) {
        | Some(session)
            when
              session.selector == String.trim(model.action_explorer.selector) =>
          Ok(
            action == PrevMatch
              ? Haz3lcore.SelectorFind.prev(session)
              : Haz3lcore.SelectorFind.next(session),
          )
        | _ => start_selector_find(model.action_explorer.selector, model)
        };
      switch (session_result) {
      | Ok(session) =>
        schedule_jump_to_session(~schedule_action, session);
        let ae =
          action_explorer_model_for_session(model.action_explorer, session);
        {
          ...with_selector_session(model, Some(session)),
          action_explorer: ae,
          globals: globals_for_action_explorer(model.globals, ae),
        }
        |> Updated.return(~is_edit=false, ~scroll_active=false);
      | Error(msg) =>
        let ae =
          action_explorer_model_clear_matches(
            model.action_explorer,
            Some(msg),
          );
        {
          ...with_selector_session(model, None),
          action_explorer: ae,
          globals: globals_for_action_explorer(model.globals, ae),
        }
        |> Updated.return(~is_edit=false, ~scroll_active=false);
      };
    | ActionExplorer(action) =>
      let ae =
        ActionExplorer.Update.update(~action, ~model=model.action_explorer);
      {
        ...model,
        action_explorer: ae,
      }
      |> Updated.return(~is_edit=false, ~scroll_active=false);
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
    | ActionExplorer(Execute) => true
    | ActionExplorer(_) => false
    | MakeActive(_)
    | Benchmark(_) => false
    | Refresh => false
    | Start => false
    | Save => false
    };
  };

  let calculate =
      (~schedule_action, ~is_edited, ~dynamics: bool, model: Model.t) => {
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
           ~action=inject(Globals(Set(Benchmark))),
           "Toggle Print Benchmarks",
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
  let is_input_field = (elId: option(string)) => {
    switch (elId) {
    | Some("title-input-box")
    | Some("module-name-input")
    | Some("prompt-input-box")
    | Some("test-required-input")
    | Some("point-max-input")
    | Some("agent-api-key-input") => true
    | Some(id) when String.starts_with(~prefix="hint-input", id) => true
    | Some(id) when String.starts_with(~prefix="syntax-hint-input", id) =>
      true
    | Some(id) when String.starts_with(~prefix="impl-hint-input", id) => true
    | _ => false
    };
  };

  let selection_has_refractors =
      (
        refractors: Haz3lcore.Zipper.Refractor.t,
        selection: Haz3lcore.Segment.t,
      )
      : bool =>
    if (List.is_empty(refractors.manuals)) {
      false;
    } else {
      let ids = Haz3lcore.Segment.ids(selection);
      List.exists(
        id =>
          List.exists(((id2, _)) => Id.equal(id, id2), refractors.manuals),
        ids,
      );
    };

  let copy = (cursor: Cursor.cursor(Editors.Update.t)): unit => {
    let str = (cursor.selected_text |> Option.value(~default=() => ""))();
    let should_set =
      switch (cursor.editor, cursor.selection) {
      | (Some(editor), Some(selection)) =>
        /* If the selection contains refractors, we forgo the segment cache
         * for the sake of preserving refractors in the copy via expanding
         * their text invocation form, i.e. ^^refractor_name(<syntax>) */
        !selection_has_refractors(editor.state.zipper.refractors, selection)
      | _ => true
      };
    should_set
      ? Haz3lcore.Parser.set_segment_cache(cursor.selection, str) : ();
    JsUtil.copy(str);
  };

  let handlers =
      (
        ~inject: Update.t => Ui_effect.t(unit),
        ~cursor: Cursor.cursor(Editors.Update.t),
        model: Model.t,
      ) => {
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
      Attr.on_copy(evt => {
        let target = Js.Opt.to_option(evt##.target);
        switch (target) {
        | Some(el) =>
          let elId = Js.Opt.to_option(Js.Unsafe.coerce(el)##.id);
          if (is_input_field(elId)) {
            ();
          } else {
            let el = Js.Unsafe.coerce(el);
            if (JsUtil.has_ancestor_class(el, "system-message")
                || JsUtil.has_ancestor_class(el, "agent-message")) {
              ();
            } else {
              copy(cursor);
            };
          };
        | None => ()
        };
        Effect.Ignore;
      }),
      Attr.on_cut(evt => {
        let target = Js.Opt.to_option(evt##.target);
        switch (target) {
        | Some(el) =>
          let elId = Js.Opt.to_option(Js.Unsafe.coerce(el)##.id);
          if (is_input_field(elId)) {
            Effect.Ignore;
          } else {
            copy(cursor);
            switch (cursor.editor_action(Destruct(Right))) {
            | Some(action) => inject(Editors(action))
            | None => Effect.Ignore
            };
          };
        | None => Effect.Ignore
        };
      }),
    ]
    @ [
      Attr.on_paste(evt => {
        let target = Js.Opt.to_option(evt##.target);
        switch (target) {
        | Some(el) =>
          let elId = Js.Opt.to_option(Js.Unsafe.coerce(el)##.id);
          if (is_input_field(elId)) {
            Effect.Ignore;
          } else {
            let text =
              Js.to_string(evt##.clipboardData##getData(Js.string("text")));
            let action =
              Haz3lcore.Action.Paste(Util.StringUtil.trim_leading(text));
            Dom.preventDefault(evt);
            switch (cursor.editor_action(action)) {
            | None => Effect.Ignore
            | Some(action) => inject(Editors(action))
            };
          };
        | None => Effect.Ignore
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
        {globals, editors, explain_this: explainThisModel, selection, _} as model: Model.t,
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

    let action_explorer_bar =
      globals.settings.show_action_explorer
        ? [
          ActionExplorer.View.view(
            ~inject=a => inject(ActionExplorer(a)),
            model.action_explorer,
          ),
        ]
        : [];

    [top_bar(~globals, ~inject, ~editors)]
    @ action_explorer_bar
    @ [
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
    let cursor =
      Selection.get_cursor_info(~inject, ~selection=model.selection, model);
    NinjaKeys.initialize(cursor.contextual_actions);
    div(
      ~attrs=[Attr.id("page"), ...handlers(~cursor, ~inject, model)],
      [FontSpecimen.view, JsUtil.clipboard_shim]
      @ main_view(~log_model, ~get_log_and, ~cursor, ~inject, model),
    );
  };
};
