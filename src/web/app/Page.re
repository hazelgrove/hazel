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
    | Tutorial(m) => List.nth(m.exercises, m.current).cells.user_impl.editor
    | Exercises(m) => ExercisesMode.Model.get_editor(m)
    };

  // Get full CellEditor including evaluation result (for App View sidebar)
  let get_cell_editor = (model: Model.t): option(CellEditor.Model.t) =>
    switch (model.editors) {
    | Scratch(m) => Some(List.nth(m.scratchpads, m.current) |> snd)
    | Documentation(m) => Some(List.nth(m.scratchpads, m.current) |> snd)
    | Tutorial(_)
    | Exercises(_) => None // These have different cell structures
    };

  // Evaluate a Hazel expression with full elaboration (for source-level exprs)
  let evaluate_exp = (exp: Language.DHExp.t): Language.DHExp.t =>
    Language.(
      fst(
        Evaluator.evaluate(
          ~env=Builtins.env_init,
          fst(
            Elaborator.elaborate(
              Statics.mk(
                CoreSettings.on,
                Builtins.ctx_init(Some(Int)),
                exp,
              ),
              exp,
            ),
          ),
        ),
      )
    );

  // Evaluate without re-elaboration (for applying already-evaluated functions)
  let evaluate_direct = (exp: Language.DHExp.t): Language.DHExp.t =>
    Language.(fst(Evaluator.evaluate(~env=Builtins.env_init, exp)));

  let update_global =
      (
        ~import_log,
        ~schedule_action,
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
    | JumpToTile(id) =>
      let jump =
        Editors.Selection.jump_to_tile(
          ~settings=model.globals.settings,
          id,
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
      | Some(state) =>
        // Use evaluate_direct: sub-expressions are already elaborated+evaluated
        let update_result =
          evaluate_direct(
            Language.IdTagged.FreshGrammar.Exp.ap(
              Forward,
              state.update_fn,
              Language.IdTagged.FreshGrammar.Exp.tuple([msg, state.model]),
            ),
          );
        // Extract (new_model, cmd) or just new_model (strip wrappers)
        let update_result = Haz3lcore.HazelDOM.strip_wrappers(update_result);
        let (new_model, cmd_opt) =
          switch (update_result.term) {
          | Tuple([m, c]) when Haz3lcore.HazelDOM.is_cmd(c) => (m, Some(c))
          | _ => (update_result, None)
          };
        let html =
          evaluate_direct(
            Language.IdTagged.FreshGrammar.Exp.ap(
              Forward,
              state.view_fn,
              new_model,
            ),
          );
        let subs =
          evaluate_direct(
            Language.IdTagged.FreshGrammar.Exp.ap(
              Forward,
              state.subs_fn,
              new_model,
            ),
          );
        // Run cmd if present
        switch (cmd_opt) {
        | Some(cmd) =>
          let cmd_ctx: Haz3lcore.CmdRunner.context = {
            model: new_model,
            inject: m => {
              schedule_action(Globals(AppViewMsg(m)));
              Virtual_dom.Vdom.Effect.Ignore;
            },
            update_fn: Some(state.update_fn),
          };
          Bonsai.Effect.Expert.handle(Haz3lcore.CmdRunner.run(cmd_ctx, cmd));
        | None => ()
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
      | None => model |> return_quiet
      }
    | InitAppView(source_result, init_model, update_fn, view_fn, subs_fn) =>
      print_endline(
        "InitAppView: init_model cls = "
        ++ Language.Exp.show_cls(Language.Exp.cls_of_term(init_model.term)),
      );
      print_endline(
        "InitAppView: view_fn cls = "
        ++ Language.Exp.show_cls(Language.Exp.cls_of_term(view_fn.term)),
      );
      let html =
        evaluate_direct(
          Language.IdTagged.FreshGrammar.Exp.ap(Forward, view_fn, init_model),
        );
      print_endline(
        "InitAppView: html result cls = "
        ++ Language.Exp.show_cls(Language.Exp.cls_of_term(html.term)),
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
      let (filename, content) =
        switch (model.editors) {
        | Scratch(model)
        | Documentation(model) =>
          let current = List.nth(model.scratchpads, model.current);
          let filename =
            (current |> fst |> StringUtil.sanitize_filename) ++ ".ml";

          let content =
            Haz3lcore.(
              [%derive.show: (string, PersistentSegment.t)]((
                current |> fst,
                current
                |> snd
                |> (e => e.editor.editor.state.zipper)
                |> PersistentSegment.persist,
              ))
            );
          (filename, content);
        | Tutorial(model) =>
          let current = List.nth(model.exercises, model.current);
          let filename = current.editors.module_name ++ ".ml";
          let content = "not supported";
          (filename, content);
        | Exercises(model) =>
          let current = List.nth(model.exercises, model.current);
          let filename =
            ExercisesMode.Model.get_exercise_name(current) ++ ".ml";
          let content = "not supported";
          (filename, content);
        };
      JsUtil.download_string_file(
        ~filename,
        ~content_type="text/plain",
        ~contents=
          "let out : string * Haz3lcore.PersistentSegment.t = " ++ content,
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
    let start = TimeUtil.now_ms();
    let globals = {
      ...model.globals,
      export_all: Export.export_all,
      get_log_and,
    };
    {
      let action_tag =
        switch (action) {
        | Globals(_) => "Globals"
        | Editors(_) => "Editors"
        | ExplainThis(_) => "ExplainThis"
        | Assistant(_) => "Assistant"
        | MakeActive(_) => "MakeActive"
        | Benchmark(_) => "Benchmark"
        | Start => "Start"
        | Save => "Save"
        };
      let has_state = Option.is_some(model.globals.app_view_state);
      if (action_tag != "Editors") {
        print_endline(
          "[AppView] Page.update: "
          ++ action_tag
          ++ " | state="
          ++ (has_state ? "Some" : "None"),
        );
      };
    };
    let result =
      switch (action) {
      | Globals(action) =>
        update_global(~globals, ~import_log, ~schedule_action, action, model)
      | Editors(action) =>
        let editors_start = TimeUtil.now_ms();
        let* editors =
          Editors.Update.update(
            ~globals,
            ~schedule_action=a => schedule_action(Editors(a)),
            ~send_assistant_insertion_info=
              assistant_callback(~schedule_action, model),
            action,
            model.editors,
          );
        TimeUtil.log_time("  Editors.Update.update", editors_start);
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
        |> Updated.return(~is_edit=false, ~scroll_active=false)
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
    TimeUtil.log_time("Page.update TOTAL", start);
    result;
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

  let handle_key_event =
      (~selection, ~event: Key.t, model: Model.t): option(Update.t) => {
    switch (event) {
    | {key: D("F7"), sys: Mac | PC, shift: Down, meta: Up, ctrl: Up, alt: Up} =>
      Some(Update.Benchmark(Start))
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
      Some(Update.Globals(Redo))
    | {key: D("Z" | "z"), sys: Mac, shift: Up, meta: Down, ctrl: Up, alt: Up}
    | {key: D("Z" | "z"), sys: PC, shift: Up, meta: Up, ctrl: Down, alt: Up} =>
      Some(Update.Globals(Undo))
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
    should_set ? ClipboardCache.set(cursor.selection, str) : ();
    JsUtil.copy(str);
  };

  let handlers =
      (
        ~inject: Update.t => Ui_effect.t(unit),
        ~cursor: Cursor.cursor(Editors.Update.t),
        model: Model.t,
      ) => {
    let update_meta =
        (evt: Js.t(Dom_html.keyboardEvent)): list(Effect.t(unit)) => {
      let meta_down = Js.to_bool(evt##.metaKey);
      model.globals.meta_down == meta_down
        ? [] : [inject(Globals(SetMetaDown(meta_down)))];
    };
    let key_handler =
        (~inject, ~dir: Key.dir, evt: Js.t(Dom_html.keyboardEvent))
        : Effect.t(unit) => {
      let meta_effects = update_meta(evt);
      // Don't intercept keyboard events targeting input elements
      // (e.g. <input>/<textarea> in App View panel)
      let target_is_input = {
        let target = Js.Opt.to_option(evt##.target);
        switch (target) {
        | Some(el) =>
          let el = Js.Unsafe.coerce(el);
          let tag: string = Js.to_string(el##.tagName);
          let id: string = Js.Optdef.case(el##.id, () => "", Js.to_string);
          (tag == "INPUT" || tag == "TEXTAREA" || tag == "SELECT")
          && id != JsUtil.clipboard_shim_id;
        | None => false
        };
      };
      if (target_is_input) {
        meta_effects == [] ? Effect.Ignore : Effect.Many(meta_effects);
      } else {
        Effect.(
          switch (
            Selection.handle_key_event(
              ~selection=Some(model.selection),
              ~event=Key.mk(dir, evt),
              model,
            )
          ) {
          | None => meta_effects == [] ? Ignore : Many(meta_effects)
          | Some(action) =>
            Many(
              [Prevent_default, Stop_propagation, inject(action)]
              @ meta_effects,
            )
          }
        );
      };
    };
    [
      Attr.on_keyup(key_handler(~inject, ~dir=KeyUp)),
      Attr.on_keydown(key_handler(~inject, ~dir=KeyDown)),
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
            copy(cursor);
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
            let action =
              Js.to_string(evt##.clipboardData##getData(Js.string("text")))
              |> ClipboardCache.get;
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
        {
          globals,
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
        ~cursor,
        ~explain_this_inject=action => inject(ExplainThis(action)),
        ~assistant_inject=action => inject(Assistant(action)),
        ~signal=
          fun
          | MakeActive(s) => inject(MakeActive(Scratch(s))),
        ~explainThisModel,
        ~assistantModel,
        ~editor=Update.get_editor(model),
        ~cell_editor=Update.get_cell_editor(model),
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

    /* Scroll handler for viewport culling. Only enabled for Scratch and
     * Documentation modes where there's a single editor filling the
     * scrollable area. Tutorial and Exercises have multiple editors. */
    let on_scroll = (evt: Js.t(Dom_html.event)) => {
      let culling_enabled =
        switch (editors) {
        | Scratch(_)
        | Documentation(_) => false /* Disabled for now due to layout issues */
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
      div(
        ~attrs=[
          Attr.id("main"),
          Attr.class_(Editors.Model.mode_string(editors)),
          Attr.on_scroll(on_scroll),
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
    let start = TimeUtil.now_ms();
    let cursor = Selection.get_cursor_info(~selection=model.selection, model);
    let result =
      div(
        ~attrs=[Attr.id("page"), ...handlers(~cursor, ~inject, model)],
        [FontSpecimen.view, JsUtil.clipboard_shim]
        @ main_view(~get_log_and, ~cursor, ~inject, model),
      );
    TimeUtil.log_time("Page.view TOTAL", start);
    result;
  };
};
