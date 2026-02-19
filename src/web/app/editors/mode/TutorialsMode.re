open Util;
/* This file handles the pagenation of Tutorial Mode, and switching between
   exercises. TutorialMode.re handles the actual exercise. */
/* This file follows conventions in [docs/ui-architecture.md] */
module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current: int,
    exercises: list(TutorialMode.Model.t),
  };
  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = {
    cur_exercise: Haz3lcore.Id.t,
    exercise_data: list((Haz3lcore.Id.t, TutorialMode.Model.persistent)),
  };
  let persist = (~instructor_mode, model): persistent => {
    {
      cur_exercise: List.nth(model.exercises, model.current).editors.id,
      exercise_data:
        List.map(
          (exercise: TutorialMode.Model.t) =>
            (
              exercise.editors.id,
              TutorialMode.Model.persist(~instructor_mode, exercise),
            ),
          model.exercises,
        ),
    };
  };
  let unpersist = (~settings, ~instructor_mode, persistent: persistent) => {
    let exercises =
      List.map2(
        TutorialMode.Model.unpersist(~settings, ~instructor_mode),
        persistent.exercise_data |> List.map(snd),
        TutorialSettings.lessons,
      );
    let current =
      ListUtil.findi_opt(
        (spec: Tutorial.spec) => spec.id == persistent.cur_exercise,
        TutorialSettings.lessons,
      )
      |> Option.map(fst)
      |> Option.value(~default=0);
    {
      current,
      exercises,
    };
  };
  let get_current = (m: t) => List.nth(m.exercises, m.current);
};
module StoreTutorialKey =
  Store.F({
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Haz3lcore.Id.t;
    let default = () =>
      List.nth(TutorialSettings.lessons, 0) |> Tutorial.id_of;
    let key = Store.CurrentTutorial;
  });
module Store = {
  let keystring_of_key = key => {
    key |> Haz3lcore.Id.to_string;
  };
  let save_exercise = (exercise: TutorialMode.Model.t, ~instructor_mode) => {
    let key = Tutorial.id_of(exercise.editors);
    let value = TutorialMode.Model.persist(exercise, ~instructor_mode);
    module S =
      Store.F({
        [@deriving (show({with_path: false}), sexp, yojson)]
        type t = TutorialMode.Model.persistent;
        let default = () => failwith("default should not be used in save");
        let key = Store.Tutorial(key);
      });
    S.save(value);
  };
  let init_exercise = (~settings, spec, ~instructor_mode) => {
    let key = Tutorial.id_of(spec);
    let exercise =
      TutorialMode.Model.of_spec(spec, ~settings, ~instructor_mode);
    save_exercise(exercise, ~instructor_mode);
    StoreTutorialKey.save(key);
    exercise;
  };
  let load_exercise =
      (~settings, key, spec, ~instructor_mode): TutorialMode.Model.persistent => {
    module S =
      Store.F({
        [@deriving (show({with_path: false}), sexp, yojson)]
        type t = TutorialMode.Model.persistent;
        let default = () =>
          spec
          |> TutorialMode.Model.of_spec(~settings, ~instructor_mode)
          |> TutorialMode.Model.persist(~instructor_mode);
        let key = Store.Tutorial(key);
      });
    S.load();
  };
  let save = (model: Model.t, ~instructor_mode) => {
    let exercise = List.nth(model.exercises, model.current);
    save_exercise(exercise, ~instructor_mode);
    let key =
      List.nth(TutorialSettings.lessons, model.current) |> Tutorial.id_of;
    StoreTutorialKey.save(key);
  };
  [@deriving (show({with_path: false}), sexp, yojson)]
  type exercise_export = Model.persistent;
  let load = (~settings, ~instructor_mode): Model.persistent => {
    let cur_exercise = StoreTutorialKey.load();
    let exercise_data =
      List.map(
        spec => {
          let key = Tutorial.id_of(spec);
          (key, load_exercise(~settings, key, spec, ~instructor_mode));
        },
        TutorialSettings.lessons,
      );
    {
      cur_exercise,
      exercise_data,
    };
  };
  let export = (~settings, ~instructor_mode) =>
    {
      cur_exercise: StoreTutorialKey.load(),
      exercise_data:
        List.map(
          spec => {
            let key = Tutorial.id_of(spec);
            (key, load_exercise(~settings, key, spec, ~instructor_mode));
          },
          TutorialSettings.lessons,
        ),
    }
    |> sexp_of_exercise_export
    |> Sexplib.Sexp.to_string;

  let import = (~settings, data, ~tutorial_specs, ~instructor_mode) => {
    let exercise_export =
      data |> Sexplib.Sexp.of_string |> exercise_export_of_sexp;
    StoreTutorialKey.save(exercise_export.cur_exercise);
    List.iter(
      ((key, value)) => {
        let n =
          ListUtil.findi_opt(
            spec => Tutorial.id_of(spec) == key,
            tutorial_specs,
          )
          |> Option.get
          |> fst;
        let spec = List.nth(tutorial_specs, n);
        save_exercise(
          value
          |> TutorialMode.Model.unpersist(
               ~settings,
               ~instructor_mode,
               _,
               spec,
             ),
          ~instructor_mode,
        );
      },
      exercise_export.exercise_data,
    );
  };

  let reset = (~settings, ~instructor_mode) => {
    let _ = StoreTutorialKey.reset();
    List.iter(
      spec => {
        let _ = init_exercise(~settings, spec, ~instructor_mode);
        ();
      },
      TutorialSettings.lessons,
    );
  };
};
module Update = {
  open Updated;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | SwitchExercise(int)
    | Tutorial(TutorialMode.Update.t)
    | ExportModule
    | ExportSubmission
    | ExportTransitionary;

  let can_undo = (action: t) => {
    switch (action) {
    | SwitchExercise(_) => false
    | Tutorial(action) => TutorialMode.Update.can_undo(action)
    | ExportModule => false
    | ExportSubmission => false
    | ExportTransitionary => false
    };
  };

  let export_exercise_module = (exercises: Model.t): unit => {
    let exercise = Model.get_current(exercises);
    let module_name = exercise.editors.module_name;
    let filename = exercise.editors.module_name ++ ".ml";
    let content_type = "text/plain";
    let contents =
      Tutorial.export_module(module_name, {eds: exercise.editors});
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
      JsUtil.download_json(TutorialSettings.filename, data);
    });
  let export_transitionary = (exercises: Model.t) => {
    let exercise = Model.get_current(exercises);
    // .ml files because show uses OCaml syntax (dune handles seamlessly)
    let module_name = exercise.editors.module_name;
    let filename = exercise.editors.module_name ++ ".ml";
    let content_type = "text/plain";
    let contents =
      Tutorial.export_transitionary_module(
        module_name,
        {eds: exercise.editors},
      );
    JsUtil.download_string_file(~filename, ~content_type, ~contents);
  };

  let update =
      (~globals: Globals.t, ~schedule_action, action: t, model: Model.t) => {
    switch (action) {
    | Tutorial(TutorialMode.Update.MoveToNextExercise) =>
      Model.{
        current:
          (model.current + 1 + List.length(model.exercises))
          mod List.length(model.exercises),
        exercises: model.exercises,
      }
      |> return
    | Tutorial(TutorialMode.Update.MoveToPrevExercise) =>
      Model.{
        current:
          (model.current - 1 + List.length(model.exercises))
          mod List.length(model.exercises),
        exercises: model.exercises,
      }
      |> return

    | Tutorial(action) =>
      let current = List.nth(model.exercises, model.current);
      let* new_current =
        TutorialMode.Update.update(
          ~settings=globals.settings,
          ~schedule_action,
          action,
          current,
        );
      let new_exercises =
        ListUtil.put_nth(model.current, new_current, model.exercises);
      Model.{
        current: model.current,
        exercises: new_exercises,
      };
    | SwitchExercise(n) =>
      Model.{
        current: n,
        exercises: model.exercises,
      }
      |> return
    | ExportModule =>
      Store.save(~instructor_mode=globals.settings.instructor_mode, model);
      export_exercise_module(model);
      model |> return_quiet;
    | ExportSubmission =>
      Store.save(~instructor_mode=globals.settings.instructor_mode, model);
      export_submission(~globals);
      model |> return_quiet;
    | ExportTransitionary =>
      Store.save(~instructor_mode=globals.settings.instructor_mode, model);
      export_transitionary(model);
      model |> return_quiet;
    };
  };
  let calculate =
      (~settings, ~is_edited, ~schedule_action, model: Model.t): Model.t => {
    let exercise =
      TutorialMode.Update.calculate(
        ~settings,
        ~is_edited,
        ~schedule_action=a => schedule_action(Tutorial(a)),
        List.nth(model.exercises, model.current),
      );
    Model.{
      current: model.current,
      exercises: ListUtil.put_nth(model.current, exercise, model.exercises),
    };
  };
};
module Selection = {
  open Cursor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = TutorialMode.Selection.t;
  let get_cursor_info = (~selection, model: Model.t): cursor(Update.t) => {
    let+ ci =
      TutorialMode.Selection.get_cursor_info(
        ~selection,
        List.nth(model.exercises, model.current),
      );
    Update.Tutorial(ci);
  };
  let handle_key_event = (~selection, ~event, model: Model.t) =>
    TutorialMode.Selection.handle_key_event(
      ~selection,
      ~event,
      List.nth(model.exercises, model.current),
    )
    |> Option.map(a => Update.Tutorial(a));
  let jump_to_tile =
      (~settings, tile, model: Model.t): option((Update.t, t)) =>
    TutorialMode.Selection.jump_to_tile(
      ~settings,
      tile,
      List.nth(model.exercises, model.current),
    )
    |> Option.map(((x, y)) => (Update.Tutorial(x), y));
};

module View = {
  open Widgets;
  open Js_of_ocaml;

  let view =
      (
        ~signal: TutorialMode.View.event => 'a,
        ~globals: Globals.t,
        ~selection: option(TutorialMode.Selection.t),
        ~inject: Update.t => 'a,
        ~inject_explainthis: ExplainThisUpdate.update => 'a,
        model: Model.t,
      ) => {
    let current = List.nth(model.exercises, model.current);
    TutorialMode.View.view(
      ~globals,
      ~signal,
      ~inject=a => inject(Update.Tutorial(a)),
      ~inject_explainthis,
      ~selection,
      current,
    );
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
            inject(Tutorial(ResetTutorial));
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
        [export_submission, import_submission],
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
    TutorialSettings.show_instructor
      ? [
        Widgets.toggle(
          "🎓", ~tooltip="Toggle Instructor Mode", instructor_mode, _ =>
          inject(Globals.Update.Set(InstructorMode))
        ),
      ]
      : [];

  let top_bar = (~globals: Globals.t, ~inject: Update.t => 'a, model: Model.t) => {
    let titles =
      List.map(
        exercise => TutorialMode.Model.return_title(exercise),
        model.exercises,
      );
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
                (model.current - 1 + List.length(model.exercises))
                mod List.length(model.exercises),
              ),
            )
          | Next =>
            inject(
              Update.SwitchExercise(
                (model.current + 1 + List.length(model.exercises))
                mod List.length(model.exercises),
              ),
            )
          | Add
          | Rename
          | Delete => Ui_effect.Ignore,
        ~indicator=
          EditorModeView.indicator_select(
            ~signal=i => inject(SwitchExercise(i)),
            model.current,
            titles,
          ),
      );
    // };
  };
};
