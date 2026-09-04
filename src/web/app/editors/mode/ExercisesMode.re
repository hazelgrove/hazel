open Util_web;

/* This file handles the pagenation of Exercise Mode, and switching between
   exercises. CodeExerciseMode.re / DerivationExerciseMode.re /
   TheoremExerciseMode.re handle the actual per-kind exercise logic. */

/* This file follows conventions in [docs/ui-architecture.md] */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type exercise =
    | Code(CodeExerciseMode.Model.t)
    | Derivation(DerivationExerciseMode.Model.t)
    | Theorem(TheoremExerciseMode.Model.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current: int,
    exercises: list(exercise),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type exercise_spec = Exercise.t;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent_exercise =
    | PCode(CodeExerciseMode.Model.persistent)
    | PDerivation(DerivationExerciseMode.Model.persistent)
    | PTheorem(TheoremExerciseMode.Model.persistent);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = {
    cur_exercise: Haz3lcore.Id.t,
    exercise_data: list((Haz3lcore.Id.t, persistent_exercise)),
  };

  let persist_exercise =
      (~instructor_mode, exercise: exercise): persistent_exercise =>
    switch (exercise) {
    | Code(e) => PCode(CodeExerciseMode.Model.persist(~instructor_mode, e))
    | Derivation(e) =>
      PDerivation(DerivationExerciseMode.Model.persist(~instructor_mode, e))
    | Theorem(e) => PTheorem(TheoremExerciseMode.Model.persist(e))
    };

  let get_exercise_id = (exercise: exercise): Haz3lcore.Id.t =>
    switch (exercise) {
    | Code(e: CodeExerciseMode.Model.t) => e.editors.id
    | Derivation(e: DerivationExerciseMode.Model.t) => e.editors.id
    | Theorem(e: TheoremExerciseMode.Model.t) => e.id
    };

  let id_of_spec = (spec: exercise_spec): Haz3lcore.Id.t =>
    switch (spec) {
    | Code(s) => s.id
    | Derivation(s) => s.id
    | Theorem(s) => s.id
    };

  let unpersist_exercise =
      (
        ~settings,
        ~instructor_mode,
        spec: exercise_spec,
        persistent: persistent_exercise,
      )
      : exercise =>
    switch (spec, persistent) {
    | (Code(s), PCode(p)) =>
      Code(CodeExerciseMode.Model.unpersist(~instructor_mode, s, p))
    | (Code(s), _) =>
      Code(CodeExerciseMode.Model.of_spec(~settings, ~instructor_mode, s))
    | (Derivation(s), PDerivation(p)) =>
      Derivation(
        DerivationExerciseMode.Model.unpersist(
          ~settings,
          ~instructor_mode,
          p,
          s,
        ),
      )
    | (Derivation(s), _) =>
      Derivation(
        DerivationExerciseMode.Model.of_spec(~settings, ~instructor_mode, s),
      )
    | (Theorem(s), PTheorem(p)) =>
      Theorem(TheoremExerciseMode.Model.unpersist(~settings, s, p))
    | (Theorem(s), _) => Theorem(TheoremExerciseMode.Model.of_spec(s))
    };

  let unpersist = (~settings, ~instructor_mode, persistent: persistent) => {
    let exercises =
      List.map2(
        unpersist_exercise(~settings, ~instructor_mode),
        ExerciseSettings.exercises, // TODO: Move this
        persistent.exercise_data |> List.map(snd),
      );
    let current =
      ListUtil.findi_opt(
        (spec: exercise_spec) => id_of_spec(spec) == persistent.cur_exercise,
        ExerciseSettings.exercises,
      )
      |> Option.map(fst)
      |> Option.value(~default=0);
    {
      current,
      exercises,
    };
  };

  let exercise_of_spec =
      (~settings, ~instructor_mode, spec: exercise_spec): exercise =>
    switch (spec) {
    | Code(s) =>
      Code(CodeExerciseMode.Model.of_spec(~settings, ~instructor_mode, s))
    | Derivation(s) =>
      Derivation(
        DerivationExerciseMode.Model.of_spec(~settings, ~instructor_mode, s),
      )
    | Theorem(s) => Theorem(TheoremExerciseMode.Model.of_spec(s))
    };

  let get_current = (m: t) => List.nth(m.exercises, m.current);

  let get_exercise_module_name = (e: exercise): string =>
    switch (e) {
    | Code(e) => e.editors.module_name
    | Derivation(e) => e.editors.module_name
    | Theorem(e) => e.module_name
    };

  let export_exercise_module = (e: exercise): string =>
    switch (e) {
    | Code(e) => CodeExercise.export_module({eds: e.editors})
    | Derivation(e) => DerivationExercise.export_module({eds: e.editors})
    | Theorem(t) => TheoremExerciseMode.Model.export_module(t)
    };

  let export_transitionary_module = (e: exercise): string =>
    switch (e) {
    | Code(e) =>
      CodeExercise.export_transitionary_module(
        e.editors.module_name,
        {eds: e.editors},
      )
    | Derivation(e) =>
      DerivationExercise.export_transitionary_module(
        e.spec.module_name,
        {eds: e.editors},
      )
    | Theorem(_) => "(* Theorem exercises do not have an exportable transitionary module *)\n"
    };

  // Used for the assistant or something
  let get_editor = (model: t): CodeEditable.Model.t => {
    let current = List.nth(model.exercises, model.current);
    switch (current) {
    | Code(e) => e.cells.user_impl.editor
    /* Setup cell's statics are computed from the stitched `prelude + setup`
       term, so using it here surfaces problems from both editors in the
       problems sidebar (matching how `user_impl` covers prelude + your_impl
       for code exercises). Line numbers for prelude ids will show as "L?"
       since this cell's measured only covers setup content. */
    | Derivation(e) => e.cells.setup.editor
    | Theorem(e) => e.cells.theorem.editor
    };
  };

  /* Editors whose problems should appear in the Problems sidebar, each
     paired with a display label shown as a section header when multiple
     groups are present. */
  let get_problem_editors =
      (~instructor_mode: bool, model: t)
      : list((option(string), list(CodeEditable.Model.t))) => {
    let current = List.nth(model.exercises, model.current);
    switch (current) {
    | Code(e) =>
      CodeExerciseMode.Model.get_problem_editors(~instructor_mode, e)
    | Derivation(e) =>
      DerivationExerciseMode.Model.get_problem_editors(~scratch_mode=false, e)
    | Theorem(e) => TheoremExerciseMode.Model.get_problem_editors(e)
    };
  };
};

module StoreExerciseKey =
  Store.F({
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Haz3lcore.Id.t;
    let default = () =>
      List.nth(ExerciseSettings.exercises, 0) |> Model.id_of_spec;
    let key = Store.CurrentExercise;
  });

module Store = {
  let save_exercise = (exercise: Model.exercise, ~instructor_mode) => {
    let key = Model.get_exercise_id(exercise);
    let value = Model.persist_exercise(exercise, ~instructor_mode);
    module S =
      Store.F({
        [@deriving (show({with_path: false}), sexp, yojson)]
        type t = Model.persistent_exercise;
        let default = () => failwith("default should not be used in save");
        let key = Store.Exercise(key);
      });
    S.save(value);
  };

  let init_exercise = (~settings, spec, ~instructor_mode) => {
    let key = Model.id_of_spec(spec);
    let exercise = Model.exercise_of_spec(spec, ~settings, ~instructor_mode);
    save_exercise(exercise, ~instructor_mode);
    StoreExerciseKey.save(key);
    exercise;
  };

  let load_exercise =
      (~settings, key, spec, ~instructor_mode): Model.persistent_exercise => {
    module S =
      Store.F({
        [@deriving (show({with_path: false}), sexp, yojson)]
        type t = Model.persistent_exercise;
        let default = () =>
          spec
          |> Model.exercise_of_spec(~settings, ~instructor_mode)
          |> Model.persist_exercise(~instructor_mode);
        let key = Store.Exercise(key);
      });
    S.load();
  };

  let save = (model: Model.t, ~instructor_mode) => {
    let exercise = List.nth(model.exercises, model.current);
    let key = Model.get_exercise_id(exercise);
    save_exercise(exercise, ~instructor_mode);
    StoreExerciseKey.save(key);
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type exercise_export = Model.persistent;

  let load = (~settings, ~instructor_mode): Model.persistent => {
    let cur_exercise = StoreExerciseKey.load();
    let exercise_data =
      List.map(
        spec => {
          let key = Model.id_of_spec(spec);
          (key, load_exercise(~settings, key, spec, ~instructor_mode));
        },
        ExerciseSettings.exercises,
      );
    {
      cur_exercise,
      exercise_data,
    };
  };

  let export = (~settings, ~instructor_mode) =>
    {
      cur_exercise: StoreExerciseKey.load(),
      exercise_data:
        List.map(
          spec => {
            let key = Model.id_of_spec(spec);
            (key, load_exercise(~settings, key, spec, ~instructor_mode));
          },
          ExerciseSettings.exercises,
        ),
    }
    |> sexp_of_exercise_export
    |> Sexplib.Sexp.to_string;

  let import = (data, ~exercise_specs, ~settings, ~instructor_mode) => {
    let exercise_export =
      data |> Sexplib.Sexp.of_string |> exercise_export_of_sexp;
    StoreExerciseKey.save(exercise_export.cur_exercise);
    List.iter(
      ((key, value)) => {
        let n =
          ListUtil.findi_opt(
            spec => Model.id_of_spec(spec) == key,
            exercise_specs,
          )
          |> Option.get
          |> fst;
        let spec = List.nth(exercise_specs, n);
        save_exercise(
          value |> Model.unpersist_exercise(~settings, ~instructor_mode, spec),
          ~instructor_mode,
        );
      },
      exercise_export.exercise_data,
    );
  };

  let reset = (~settings, ~instructor_mode) => {
    let _ = StoreExerciseKey.reset();
    List.iter(
      spec => {
        let _ = init_exercise(~settings, spec, ~instructor_mode);
        ();
      },
      ExerciseSettings.exercises,
    );
  };
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | SwitchExercise(int)
    | Exercise(CodeExerciseMode.Update.t)
    | Derivation(DerivationExerciseMode.Update.t)
    | TheoremExercise(TheoremExerciseMode.Update.t)
    | ExportModule
    | ExportSubmission
    | ExportTransitionary;
  let export_exercise_module = (exercises: Model.t): unit => {
    let exercise = Model.get_current(exercises);
    let module_name =
      StringUtil.isEmptyOrWhitespace(
        exercise |> Model.get_exercise_module_name,
      )
        ? "UnnamedExerciseModule" : exercise |> Model.get_exercise_module_name;
    let filename = module_name ++ ".ml";
    let content_type = "text/plain";
    let contents = Model.export_exercise_module(exercise);
    JsUtil.download_string_file(~filename, ~content_type, ~contents);
  };

  let export_submission = (~globals: Globals.t) =>
    globals.get_log_and(log => {
      let data =
        globals.export_all(
          ~settings=globals.settings.core,
          ~instructor_mode=globals.settings.instructor_mode,
          ~log,
        );
      JsUtil.download_json(ExerciseSettings.filename, data);
    });

  let export_transitionary = (exercises: Model.t) => {
    let exercise = Model.get_current(exercises);
    let module_name =
      StringUtil.isEmptyOrWhitespace(
        exercise |> Model.get_exercise_module_name,
      )
        ? "UnnamedExerciseModule" : exercise |> Model.get_exercise_module_name;
    // .ml files because show uses OCaml syntax (dune handles seamlessly)
    let filename = module_name ++ ".ml";
    let content_type = "text/plain";
    let contents = Model.export_transitionary_module(exercise);
    JsUtil.download_string_file(~filename, ~content_type, ~contents);
  };

  let update =
      (~globals: Globals.t, ~schedule_action, action: t, model: Model.t) => {
    switch (Model.get_current(model), action) {
    | (Code(ex), Exercise(action)) =>
      let* new_current =
        CodeExerciseMode.Update.update(
          ~settings=globals.settings,
          ~schedule_action,
          action,
          ex,
        );
      let new_exercises =
        ListUtil.put_nth(
          model.current,
          Model.Code(new_current),
          model.exercises,
        );
      Model.{
        current: model.current,
        exercises: new_exercises,
      };
    | (_, Exercise(_)) => model |> raise_invalid_action
    | (Theorem(ex), TheoremExercise(action)) =>
      let* new_current =
        TheoremExerciseMode.Update.update(
          ~settings=globals.settings,
          action,
          ex,
        );
      let new_exercises =
        ListUtil.put_nth(
          model.current,
          Model.Theorem(new_current),
          model.exercises,
        );
      Model.{
        current: model.current,
        exercises: new_exercises,
      };
    | (_, TheoremExercise(_)) => model |> raise_invalid_action
    | (Derivation(ex), Derivation(action)) =>
      let* new_current =
        DerivationExerciseMode.Update.update(
          ~settings=globals.settings,
          ~schedule_action,
          action,
          ex,
        );
      let new_exercises =
        ListUtil.put_nth(
          model.current,
          Model.Derivation(new_current),
          model.exercises,
        );
      Model.{
        current: model.current,
        exercises: new_exercises,
      };
    | (_, Derivation(_)) => model |> raise_invalid_action
    | (_, SwitchExercise(n)) =>
      WorkerClient.cancel();
      Model.{
        current: n,
        exercises: model.exercises,
      }
      |> return(~historic=false);
    | (_, ExportModule) =>
      Store.save(~instructor_mode=globals.settings.instructor_mode, model);
      export_exercise_module(model);
      model |> return_quiet;
    | (_, ExportSubmission) =>
      Store.save(~instructor_mode=globals.settings.instructor_mode, model);
      export_submission(~globals);
      model |> return_quiet;
    | (_, ExportTransitionary) =>
      Store.save(~instructor_mode=globals.settings.instructor_mode, model);
      export_transitionary(model);
      model |> return_quiet;
    };
  };

  let calculate =
      (~settings, ~is_edited, ~schedule_action, model: Model.t): Model.t => {
    let current_exercise = Model.get_current(model);
    let current_exercise =
      switch (current_exercise) {
      | Code(ex) =>
        Model.Code(
          CodeExerciseMode.Update.calculate(
            ~settings,
            ~is_edited,
            ~schedule_action=a => schedule_action(Exercise(a)),
            ex,
          ),
        )
      | Derivation(ex) =>
        Model.Derivation(
          DerivationExerciseMode.Update.calculate(
            ~settings,
            ~is_edited,
            ~schedule_action=a => schedule_action(Derivation(a)),
            ex,
          ),
        )
      | Theorem(ex) =>
        Model.Theorem(
          TheoremExerciseMode.Update.calculate(
            ~settings,
            ~is_edited,
            ~schedule_action=a => schedule_action(TheoremExercise(a)),
            ex,
          ),
        )
      };
    Model.{
      current: model.current,
      exercises:
        ListUtil.put_nth(model.current, current_exercise, model.exercises),
    };
  };
};

module Selection = {
  open Cursor;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Code(CodeExerciseMode.Selection.t)
    | Derivation(DerivationExerciseMode.Selection.t)
    | TheoremExercise(TheoremExerciseMode.Selection.t);

  let get_cursor_info =
      (~inject: Update.t => Ui_effect.t(unit), ~selection, model: Model.t)
      : cursor(Update.t) => {
    let current = List.nth(model.exercises, model.current);
    let cursor =
      switch (current, selection) {
      | (Code(e), Code(selection)) =>
        let+ ci =
          CodeExerciseMode.Selection.get_cursor_info(
            ~inject=a => inject(Exercise(a)),
            ~selection,
            e,
          );
        Update.Exercise(ci);
      | (Code(_), _) => Cursor.empty
      | (Derivation(e), Derivation(selection)) =>
        let+ ci =
          DerivationExerciseMode.Selection.get_cursor_info(
            ~inject=a => inject(Derivation(a)),
            ~selection,
            e,
          );
        Update.Derivation(ci);
      | (Derivation(_), _) => Cursor.empty
      | (Theorem(e), TheoremExercise(selection)) =>
        let+ ci =
          TheoremExerciseMode.Selection.get_cursor_info(
            ~inject=a => inject(TheoremExercise(a)),
            ~selection,
            e,
          );
        Update.TheoremExercise(ci);
      | (Theorem(_), _) => Cursor.empty
      };
    cursor
    |> Cursor.with_actions(
         [
           ContextualAction.mk(
             ~mdIcon="download",
             ~section="Export",
             ~action=inject(ExportSubmission),
             "Export Submission",
           ),
         ]
         @ (
           if (ExerciseSettings.show_instructor) {
             [
               ContextualAction.mk(
                 ~mdIcon="download",
                 ~section="Export",
                 ~action=inject(ExportModule),
                 "Export Exercise Module",
               ),
               ContextualAction.mk(
                 ~mdIcon="download",
                 ~section="Export",
                 ~action=inject(ExportTransitionary),
                 "Export Transitionary Exercise Module",
               ),
             ];
           } else {
             [];
           }
         ),
       );
  };

  let jump_to_tile =
      (~settings, tile, model: Model.t): option((Update.t, t)) => {
    let current = List.nth(model.exercises, model.current);
    switch (current) {
    | Code(e) =>
      CodeExerciseMode.Selection.jump_to_tile(~settings, tile, e)
      |> Option.map(((x, y)) => (Update.Exercise(x), Code(y)))
    | Derivation(e) =>
      DerivationExerciseMode.Selection.jump_to_tile(~settings, tile, e)
      |> Option.map(((x, y)) => (Update.Derivation(x), Derivation(y)))
    | Theorem(e) =>
      TheoremExerciseMode.Selection.jump_to_tile(tile, e)
      |> Option.map(((x, y)) =>
           (Update.TheoremExercise(x), TheoremExercise(y))
         )
    };
  };

  let get_derivation_info = (~selection: t, model: Model.t) => {
    let current = List.nth(model.exercises, model.current);
    switch (selection, current) {
    | (Derivation(sel), Derivation(e)) =>
      DerivationExerciseMode.Selection.get_derivation_info(~selection=sel, e)
    | _ => None
    };
  };
};

module View = {
  open Widgets;
  open Js_of_ocaml;

  let view =
      (
        ~globals: Globals.t,
        ~take_focus: Selection.t => 'a,
        ~inject: Update.t => 'a,
        ~inject_explainthis,
        ~selection: option(Selection.t),
        model: Model.t,
      ) => {
    let current = List.nth(model.exercises, model.current);
    switch (current) {
    | Code(current) =>
      CodeExerciseMode.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive(s) => take_focus(Code(s)),
        ~inject=a => inject(Update.Exercise(a)),
        ~inject_explainthis,
        ~selection=
          switch (selection) {
          | Some(Code(s)) => Some(s)
          | _ => None
          },
        current,
      )
    | Derivation(current) =>
      DerivationExerciseMode.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive(s) => take_focus(Derivation(s)),
        ~inject=a => inject(Update.Derivation(a)),
        ~inject_explainthis,
        ~selection=
          switch (selection) {
          | Some(Derivation(s)) => Some(s)
          | _ => None
          },
        current,
      )
    | Theorem(current) =>
      TheoremExerciseMode.View.view(
        ~globals,
        ~take_focus=s => take_focus(TheoremExercise(s)),
        ~inject=a => inject(Update.TheoremExercise(a)),
        ~inject_explainthis,
        ~selection=
          switch (selection) {
          | Some(TheoremExercise(s)) => Some(s)
          | _ => None
          },
        current,
      )
    };
  };

  let file_menu = (~globals: Globals.t, ~inject: Update.t => 'a, _: Model.t) => {
    let reset_button =
      Widgets.button_named(
        Icons.trash,
        _ => {
          let confirmed =
            JsUtil.confirm(
              "Are you SURE you want to reset this exercise? You will lose any existing code that you have written, and course staff have no way to restore it!",
            );
          if (confirmed) {
            inject(Exercise(ResetExercise));
          } else {
            Virtual_dom.Vdom.Effect.Ignore;
          };
        },
        ~tooltip="Reset Exercise",
      );

    let instructor_export =
      Widgets.button_named(
        Icons.export,
        _ => inject(ExportModule),
        ~tooltip="Export Exercise Module",
      );

    let instructor_transitionary_export =
      Widgets.button_named(
        Icons.export,
        _ => {inject(ExportTransitionary)},
        ~tooltip="Export Transitionary Exercise Module",
      );

    let export_submission =
      Widgets.button_named(
        Icons.star,
        _ => inject(ExportSubmission),
        ~tooltip="Export Submission",
      );

    let import_submission =
      Widgets.file_select_button_named(
        "import-submission",
        Icons.import,
        file => {
          switch (file) {
          | None => Virtual_dom.Vdom.Effect.Ignore
          | Some(file) => globals.inject_global(InitImportAll(file))
          }
        },
        ~tooltip="Import Submission",
      );
    let import_logs =
      Widgets.file_select_button_named(
        "import-logs",
        Icons.import,
        file => {
          switch (file) {
          | None => Virtual_dom.Vdom.Effect.Ignore
          | Some(file) => globals.inject_global(Log(InitImport(file)))
          }
        },
        ~tooltip="Import Logs",
      );

    let reset_hazel =
      button_named(
        Icons.bomb,
        _ => {
          let confirmed =
            JsUtil.confirm(
              "Are you SURE you want to reset Hazel to its initial state? You will lose any existing code that you have written, and course staff have no way to restore it!",
            );
          if (confirmed) {
            HazelDB.clear_all();
            Dom_html.window##.location##reload;
          };
          Virtual_dom.Vdom.Effect.Ignore;
        },
        ~tooltip="Reset Hazel (LOSE ALL DATA)",
      );

    let reparse =
      button_named(
        Icons.backpack,
        _ => globals.inject_global(ActiveEditor(Reparse)),
        ~tooltip="Reparse Editor",
      );

    let file_group_exercises = () =>
      NutMenu.item_group(
        ~inject,
        "File",
        [export_submission, import_submission, import_logs],
      );

    let reset_group_exercises = () =>
      NutMenu.item_group(
        ~inject,
        "Reset",
        [reset_button, reparse, reset_hazel],
      );

    let dev_group_exercises = () =>
      NutMenu.item_group(
        ~inject,
        "Developer Export",
        [instructor_export, instructor_transitionary_export],
      );

    if (globals.settings.instructor_mode) {
      [
        file_group_exercises(),
        reset_group_exercises(),
        dev_group_exercises(),
      ];
    } else {
      [file_group_exercises(), reset_group_exercises()];
    };
  };

  let instructor_toggle = (~inject, ~instructor_mode) =>
    ExerciseSettings.show_instructor
      ? [
        Widgets.toggle(
          "🎓", ~tooltip="Toggle Instructor Mode", instructor_mode, _ =>
          inject(Globals.Update.Set(InstructorMode))
        ),
      ]
      : [];

  let top_bar = (~globals: Globals.t, ~inject: Update.t => 'a, model: Model.t) =>
    instructor_toggle(
      ~inject=globals.inject_global,
      ~instructor_mode=globals.settings.instructor_mode,
    )
    @ EditorModeView.view(
        ~edit_buttons=false,
        ~nav_buttons=true,
        ~signal=
          fun
          | Previous =>
            inject(
              Update.SwitchExercise(
                (model.current + List.length(model.exercises) - 1)
                mod List.length(model.exercises),
              ),
            )
          | Next =>
            inject(
              Update.SwitchExercise(
                (model.current + 1) mod List.length(model.exercises),
              ),
            )
          | Add
          | Rename
          | Delete => Ui_effect.Ignore,
        ~indicator=
          EditorModeView.indicator_n(
            model.current,
            List.length(model.exercises),
          ),
        (),
      );
};
