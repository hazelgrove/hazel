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

  let get_editor = (model: Model.t): CodeEditable.Model.t =>
    switch (model.editors) {
    | Scratch(m) => List.nth(m.scratchpads, m.current).editor.editor
    | Documentation(m) => List.nth(m.scratchpads, m.current).editor.editor
    | Tutorial(m) => List.nth(m.exercises, m.current).cells.user_impl.editor
    | Exercises(m) => ExercisesMode.Model.get_editor(m)
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
      let (filename, content) =
        switch (model.editors) {
        | Scratch(model)
        | Documentation(model) =>
          let current = List.nth(model.scratchpads, model.current);
          let filename =
            (current.name |> StringUtil.sanitize_filename) ++ ".ml";

          let content =
            Haz3lcore.(
              [%derive.show: (string, PersistentSegment.t)]((
                current.name,
                current.editor.editor.editor.state.zipper
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
    | Start => model |> return
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
        ~autoprobe_mode=model.globals.settings.autoprobe_mode,
        ~schedule_action=a => schedule_action(Editors(a)),
        ~is_edited,
        model.editors,
      );
    let cursor_info =
      Editors.Selection.get_cursor_info(
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

module Selection = {
  open Cursor;

  type t = selection;
  let get_cursor_info =
      (~inject: Update.t => Ui_effect.t(unit), ~selection: t, model: Model.t)
      : cursor(Editors.Update.t) => {
    let sys = Util.Os.is_mac^ ? Util.Key.Mac : PC;
    let meta = Keyboard.meta(sys);
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
         /* Navigation */
         mk(
           ~hotkey="F12",
           ~mdIcon="arrow_forward",
           ~section="Navigation",
           ~action=
             inject(
               Globals(
                 ActiveEditor(Move(Goal(BindingSiteOfIndicatedVar))),
               ),
             ),
           "Go to Definition",
         ),
         mk(
           ~hotkey="shift+tab",
           ~mdIcon="arrow_upward",
           ~section="Navigation",
           ~action=
             inject(
               Globals(ActiveEditor(Move(Goal(NextProblem(Left))))),
             ),
           "Go to Previous Problem",
         ),
         mk(
           ~mdIcon="arrow_downward",
           ~section="Navigation",
           ~action=
             inject(
               Globals(ActiveEditor(Move(Goal(NextProblem(Right))))),
             ),
           "Go to Next Problem",
         ),
         /* Selection */
         mk(
           ~hotkey=meta ++ "+d",
           ~mdIcon="select_all",
           ~section="Selection",
           ~action=inject(Globals(ActiveEditor(Select(Term(Current))))),
           "Select current term",
         ),
         mk(
           ~mdIcon="select_all",
           ~hotkey=meta ++ "+a",
           ~section="Selection",
           ~action=inject(Globals(ActiveEditor(Select(All)))),
           "Select All",
         ),
         mk(
           ~mdIcon="flip_horizontal",
           ~section="Selection",
           ~action=inject(Globals(ActiveEditor(Select(ToggleFocus)))),
           "Toggle Selection Focus",
         ),
         mk(
           ~mdIcon="border_left",
           ~section="Selection",
           ~hotkey=meta ++ "+alt+shift+left",
           ~action=inject(Globals(ActiveEditor(Select(SetFocus(Left))))),
           "Set Selection Focus Left",
         ),
         mk(
           ~mdIcon="border_right",
           ~section="Selection",
           ~hotkey=meta ++ "+alt+shift+right",
           ~action=inject(Globals(ActiveEditor(Select(SetFocus(Right))))),
           "Set Selection Focus Right",
         ),
         mk(
           ~mdIcon="chevron_left",
           ~section="Selection",
           ~hotkey="alt+shift+left",
           ~action=
             inject(
               Globals(ActiveEditor(Select(Resize(Local(Left, ByToken))))),
             ),
           "Extend Selection Left by Token",
         ),
         mk(
           ~mdIcon="chevron_right",
           ~section="Selection",
           ~hotkey="alt+shift+right",
           ~action=
             inject(
               Globals(ActiveEditor(Select(Resize(Local(Right, ByToken))))),
             ),
           "Extend Selection Right by Token",
         ),
         /* Projection */
         mk(
           ~hotkey="alt+f",
           ~mdIcon="camera",
           ~section="Projection",
           ~action=
             inject(
               Globals(
                 ActiveEditor(Project(SetIndicated(Specific(Fold)))),
               ),
             ),
           "Fold",
         ),
         mk(
           ~hotkey=meta ++ "+e",
           ~mdIcon="camera",
           ~section="Projection",
           ~action=inject(Globals(ActiveEditor(Probe(ToggleManual)))),
           "Probe",
         ),
         mk(
           ~hotkey="alt+t",
           ~mdIcon="camera",
           ~section="Projection",
           ~action=inject(Globals(ActiveEditor(Probe(ToggleStatics)))),
           "Statics",
         ),
         mk(
           ~hotkey="alt+l",
           ~mdIcon="camera",
           ~section="Projection",
           ~action=
             inject(
               Globals(ActiveEditor(Project(SetIndicated(ChooseLivelit)))),
             ),
           "Livelit",
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
         /* Editor tools */
         mk(
           ~hotkey=meta ++ "+/",
           ~mdIcon="assistant",
           ~action=inject(Globals(ActiveEditor(Buffer(Set(TyDi))))),
           "TyDi Assistant",
         ),
         mk(
           ~mdIcon="download",
           ~section="Export",
           ~action=inject(Globals(ExportForInit)),
           "Export For Init",
         ),
         mk(
           ~section="Diagnostics",
           ~mdIcon="refresh",
           ~action=inject(Globals(ActiveEditor(Reparse))),
           "Reparse Current Editor",
         ),
         mk(
           ~mdIcon="timer",
           ~section="Diagnostics",
           ~hotkey="F7",
           ~action=inject(Benchmark(Start)),
           "Run Benchmark",
         ),
         mk(
           ~mdIcon="bolt",
           ~section="Refactoring",
           ~hotkey=meta ++ "+i",
           ~action=inject(Globals(ActiveEditor(Introduce))),
           "Introduce",
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
            inject(Globals(ActiveEditor(Destruct(Right))));
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

    [
      top_bar(~globals, ~inject, ~editors),
      closure_cursor_bar,
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
