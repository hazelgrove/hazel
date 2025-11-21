open Util;

/* This file handles the pagenation of Exercise Mode, and switching between
   exercises. ExerciseMode.re handles the actual exercise. */

/* This file follows conventions in [docs/ui-architecture.md] */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type exercise =
    | Implementation(ExerciseMode.Model.t)
    | Theorem(TheoremExerciseMode.Model.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current: int,
    exercises: list(exercise),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type exercise_spec = Exercise.exercise_spec;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent_exercise =
    | PImplementation(ExerciseMode.Model.persistent)
    | PTheorem(TheoremExerciseMode.Model.persistent);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = {
    cur_exercise: Haz3lcore.Id.t,
    exercise_data: list((Haz3lcore.Id.t, persistent_exercise)),
  };

  let persist_exercise =
      (~instructor_mode, exercise: exercise): persistent_exercise =>
    switch (exercise) {
    | Implementation(e) =>
      PImplementation(ExerciseMode.Model.persist(~instructor_mode, e))
    | Theorem(e) => PTheorem(TheoremExerciseMode.Model.persist(e))
    };

  let get_exercise_id = (exercise: exercise): Haz3lcore.Id.t =>
    switch (exercise) {
    | Implementation(e: ExerciseMode.Model.t) => e.editors.id
    | Theorem(e: TheoremExerciseMode.Model.t) => e.id
    };

  let get_spec_id = (spec: exercise_spec): Haz3lcore.Id.t =>
    switch (spec) {
    | Implementation(s) => s.id
    | Theorem(s) => s.id
    };

  let persist = (~instructor_mode, model): persistent => {
    {
      cur_exercise:
        List.nth(model.exercises, model.current) |> get_exercise_id,
      exercise_data:
        List.map(
          (exercise: exercise) =>
            (
              get_exercise_id(exercise),
              persist_exercise(~instructor_mode, exercise),
            ),
          model.exercises,
        ),
    };
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
    | (Implementation(s), PImplementation(p)) =>
      Implementation(ExerciseMode.Model.unpersist(~instructor_mode, s, p))
    | (Implementation(s), _) =>
      Implementation(
        ExerciseMode.Model.of_spec(~settings, ~instructor_mode, s),
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
        (spec: exercise_spec) =>
          get_spec_id(spec) == persistent.cur_exercise,
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
    | Implementation(s) =>
      Implementation(
        ExerciseMode.Model.of_spec(~settings, ~instructor_mode, s),
      )
    | Theorem(s) => Theorem(TheoremExerciseMode.Model.of_spec(s))
    };

  let id_of_spec = (spec: exercise_spec): Haz3lcore.Id.t =>
    switch (spec) {
    | Implementation(s) => s.id
    | Theorem(s) => s.id
    };

  let get_current = (m: t) => List.nth(m.exercises, m.current);

  let get_exercise_name = (e: exercise): string =>
    switch (e) {
    | Implementation(e) => e.editors.title
    | Theorem(e) => e.title
    };

  let export_exercise_module = (e: exercise): string =>
    switch (e) {
    | Implementation(e) => Exercise.export_module({eds: e.editors})
    | Theorem(t) => TheoremExerciseMode.Model.export_module(t)
    };

  let export_transitionary_module = (e: exercise): string =>
    switch (e) {
    | Implementation(e) =>
      Exercise.export_transitionary_module(
        e.editors.module_name,
        {eds: e.editors},
      )
    | Theorem(_) => "(* Theorem exercises do not have an exportable transitionary module *)\n"
    };

  // Used for the assistant or something
  let get_editor = (model: t): CodeEditable.Model.t => {
    let current = List.nth(model.exercises, model.current);
    switch (current) {
    | Implementation(e) => e.cells.user_impl.editor
    | Theorem(e) => e.cells.theorem.editor
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
  let keystring_of_key = key => {
    key |> Haz3lcore.Id.to_string;
  };

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
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | SwitchExercise(int)
    | Exercise(ExerciseMode.Update.t)
    | TheoremExercise(TheoremExerciseMode.Update.t)
    | ExportModule
    | ExportSubmission
    | ExportTransitionary;

  let can_undo = (action: t) => {
    switch (action) {
    | SwitchExercise(_) => false
    | Exercise(action) => ExerciseMode.Update.can_undo(action)
    | TheoremExercise(action) => TheoremExerciseMode.Update.can_undo(action)
    | ExportModule => false
    | ExportSubmission => false
    | ExportTransitionary => false
    };
  };
  let export_exercise_module = (exercises: Model.t): unit => {
    let exercise = Model.get_current(exercises);
    let module_name =
      StringUtil.isEmptyOrWhitespace(exercise |> Model.get_exercise_name)
        ? "Unnamed Exercise Module" : exercise |> Model.get_exercise_name;
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
    // .ml files because show uses OCaml syntax (dune handles seamlessly)
    let filename = (exercise |> Model.get_exercise_name) ++ ".ml";
    let content_type = "text/plain";
    let contents = Model.export_transitionary_module(exercise);
    JsUtil.download_string_file(~filename, ~content_type, ~contents);
  };

  let update =
      (~globals: Globals.t, ~schedule_action, action: t, model: Model.t) => {
    switch (Model.get_current(model), action) {
    | (Implementation(ex), Exercise(action)) =>
      let* new_current =
        ExerciseMode.Update.update(
          ~settings=globals.settings,
          ~schedule_action,
          action,
          ex,
        );
      let new_exercises =
        ListUtil.put_nth(
          model.current,
          Model.Implementation(new_current),
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
    | (_, SwitchExercise(n)) =>
      Model.{
        current: n,
        exercises: model.exercises,
      }
      |> return
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
      | Implementation(ex) =>
        Model.Implementation(
          ExerciseMode.Update.calculate(
            ~settings,
            ~is_edited,
            ~schedule_action=a => schedule_action(Exercise(a)),
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
    | Implementation(ExerciseMode.Selection.t)
    | TheoremExercise(TheoremExerciseMode.Selection.t);

  let get_cursor_info = (~selection, model: Model.t): cursor(Update.t) => {
    let current = List.nth(model.exercises, model.current);
    switch (current, selection) {
    | (Implementation(e), Implementation(selection)) =>
      let+ ci = ExerciseMode.Selection.get_cursor_info(~selection, e);
      Update.Exercise(ci);
    | (Implementation(_), _) => Cursor.empty
    | (Theorem(e), TheoremExercise(selection)) =>
      let+ ci = TheoremExerciseMode.Selection.get_cursor_info(~selection, e);
      Update.TheoremExercise(ci);
    | (Theorem(_), _) => Cursor.empty
    };
  };

  let handle_key_event =
      (~selection: t, ~event, model: Model.t): option(Update.t) => {
    let current = List.nth(model.exercises, model.current);
    switch (current, selection) {
    | (Implementation(e), Implementation(selection)) =>
      ExerciseMode.Selection.handle_key_event(~selection, ~event, e)
      |> Option.map(a => Update.Exercise(a))
    | (Implementation(_), _) => None
    | (Theorem(e), TheoremExercise(selection)) =>
      TheoremExerciseMode.Selection.handle_key_event(~selection, ~event, e)
      |> Option.map(a => Update.TheoremExercise(a))
    | (Theorem(_), _) => None
    };
  };

  let jump_to_tile =
      (~settings, tile, model: Model.t): option((Update.t, t)) => {
    let current = List.nth(model.exercises, model.current);
    switch (current) {
    | Implementation(e) =>
      ExerciseMode.Selection.jump_to_tile(~settings, tile, e)
      |> Option.map(((x, y)) => (Update.Exercise(x), Implementation(y)))
    | Theorem(e) =>
      TheoremExerciseMode.Selection.jump_to_tile(tile, e)
      |> Option.map(((x, y)) =>
           (Update.TheoremExercise(x), TheoremExercise(y))
         )
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
    | Implementation(current) =>
      ExerciseMode.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive(s) => take_focus(Implementation(s)),
        ~inject=a => inject(Update.Exercise(a)),
        ~inject_explainthis,
        ~selection=
          switch (selection) {
          | Some(Implementation(s)) => Some(s)
          | _ => None
          },
        current,
      )
    | Theorem(current) =>
      TheoremExerciseMode.View.view(
        ~globals,
        ~take_focus=s => take_focus(TheoremExercise(s)),
        ~inject=a => inject(Update.TheoremExercise(a)),
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
            JsUtil.clear_localstore();
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
      );
};
