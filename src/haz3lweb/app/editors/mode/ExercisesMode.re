open Util;

/* This file handles the pagenation of Exercise Mode, and switching between
   exercises. ExerciseMode.re handles the actual exercise. */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current: int,
    exercises: list(ExerciseMode.Model.t),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = {
    cur_exercise: Exercise.key,
    exercise_data: list((Exercise.key, ExerciseMode.Model.persistent)),
  };

  let persist = (~instructor_mode, model): persistent => {
    cur_exercise:
      Exercise.key_of_state(
        List.nth(model.exercises, model.current).editors,
      ),
    exercise_data:
      List.map(
        (exercise: ExerciseMode.Model.t) =>
          (
            Exercise.key_of_state(exercise.editors),
            ExerciseMode.Model.persist(~instructor_mode, exercise),
          ),
        model.exercises,
      ),
  };

  let unpersist = (~settings, ~instructor_mode, persistent: persistent) => {
    let exercises =
      List.map2(
        ExerciseMode.Model.unpersist(~settings, ~instructor_mode),
        persistent.exercise_data |> List.map(snd),
        ExerciseSettings.exercises,
      );
    let current =
      ListUtil.findi_opt(
        spec => Exercise.key_of(spec) == persistent.cur_exercise,
        ExerciseSettings.exercises,
      )
      |> Option.map(fst)
      |> Option.value(~default=0);
    {current, exercises};
  };

  let get_current = (m: t) => List.nth(m.exercises, m.current);
};

module StoreExerciseKey =
  Store.F({
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Exercise.key;
    let default = () =>
      List.nth(ExerciseSettings.exercises, 0) |> Exercise.key_of;
    let key = Store.CurrentExercise;
  });

module Store = {
  let keystring_of_key = key => {
    key |> Exercise.sexp_of_key |> Sexplib.Sexp.to_string;
  };

  let save_exercise = (exercise: ExerciseMode.Model.t, ~instructor_mode) => {
    let key = Exercise.key_of_state(exercise.editors);
    let value = ExerciseMode.Model.persist(exercise, ~instructor_mode);
    module S =
      Store.F({
        [@deriving (show({with_path: false}), sexp, yojson)]
        type t = ExerciseMode.Model.persistent;
        let default = () => failwith("default should not be used in save");
        let key = Store.Exercise(key);
      });
    S.save(value);
  };

  let init_exercise = (~settings, spec, ~instructor_mode) => {
    let key = Exercise.key_of(spec);
    let exercise =
      ExerciseMode.Model.of_spec(spec, ~settings, ~instructor_mode);
    save_exercise(exercise, ~instructor_mode);
    StoreExerciseKey.save(key);
    exercise;
  };

  let load_exercise =
      (~settings, key, spec, ~instructor_mode): ExerciseMode.Model.persistent => {
    module S =
      Store.F({
        [@deriving (show({with_path: false}), sexp, yojson)]
        type t = ExerciseMode.Model.persistent;
        let default = () =>
          spec
          |> ExerciseMode.Model.of_spec(~settings, ~instructor_mode)
          |> ExerciseMode.Model.persist(~instructor_mode);
        let key = Store.Exercise(key);
      });
    S.load();
  };

  let save = (model: Model.t, ~instructor_mode) => {
    let exercise = List.nth(model.exercises, model.current);
    let key = Exercise.key_of(exercise.editors);
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
          let key = Exercise.key_of(spec);
          (key, load_exercise(~settings, key, spec, ~instructor_mode));
        },
        ExerciseSettings.exercises,
      );
    {cur_exercise, exercise_data};
  };

  let export = (~settings, ~instructor_mode) =>
    {
      cur_exercise: StoreExerciseKey.load(),
      exercise_data:
        List.map(
          spec => {
            let key = Exercise.key_of(spec);
            (key, load_exercise(~settings, key, spec, ~instructor_mode));
          },
          ExerciseSettings.exercises,
        ),
    }
    |> sexp_of_exercise_export
    |> Sexplib.Sexp.to_string;

  let import = (~settings, data, ~specs, ~instructor_mode) => {
    let exercise_export =
      data |> Sexplib.Sexp.of_string |> exercise_export_of_sexp;
    StoreExerciseKey.save(exercise_export.cur_exercise);
    List.iter(
      ((key, value)) => {
        let n =
          ListUtil.findi_opt(spec => Exercise.key_of(spec) == key, specs)
          |> Option.get
          |> fst;
        let spec = List.nth(specs, n);
        save_exercise(
          value
          |> ExerciseMode.Model.unpersist(
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
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | SwitchExercise(int)
    | Exercise(ExerciseMode.Update.t);

  let update = (~settings, ~schedule_action, action: t, model: Model.t) => {
    switch (action) {
    | Exercise(action) =>
      let current = List.nth(model.exercises, model.current);
      let* new_current =
        ExerciseMode.Update.update(
          ~settings,
          ~schedule_action,
          action,
          current,
        );
      let new_exercises =
        ListUtil.put_nth(model.current, new_current, model.exercises);
      Model.{current: model.current, exercises: new_exercises};
    | SwitchExercise(n) =>
      Model.{current: n, exercises: model.exercises} |> return
    };
  };

  let calculate = (~settings, ~schedule_action, model: Model.t): Model.t => {
    let exercise =
      ExerciseMode.Update.calculate(
        ~settings,
        ~schedule_action=a => schedule_action(Exercise(a)),
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
  type t = ExerciseMode.Selection.t;

  let get_cursor_info = (~selection, model: Model.t): cursor(Update.t) => {
    let+ ci =
      ExerciseMode.Selection.get_cursor_info(
        ~selection,
        List.nth(model.exercises, model.current),
      );
    Update.Exercise(ci);
  };

  let handle_key_event = (~selection, ~event, model: Model.t) =>
    ExerciseMode.Selection.handle_key_event(
      ~selection,
      ~event,
      List.nth(model.exercises, model.current),
    )
    |> Option.map(a => Update.Exercise(a));

  let jump_to_tile =
      (~settings, tile, model: Model.t): option((Update.t, t)) =>
    ExerciseMode.Selection.jump_to_tile(
      ~settings,
      tile,
      List.nth(model.exercises, model.current),
    )
    |> Option.map(((x, y)) => (Update.Exercise(x), y));
};

module View = {
  let view = (~globals: Globals.t, ~inject: Update.t => 'a, model: Model.t) => {
    let current = List.nth(model.exercises, model.current);
    ExerciseMode.View.view(
      ~globals,
      ~inject=a => inject(Update.Exercise(a)),
      current,
    );
  };

  let export_menu = (~globals: Globals.t, model: Model.t) => {
    let current = List.nth(model.exercises, model.current);
    ExerciseMode.View.export_menu(~globals, current);
  };

  let import_menu = (~globals: Globals.t, ~inject: Update.t => 'a) => {
    ExerciseMode.View.import_menu(~globals, ~inject=a =>
      inject(Update.Exercise(a))
    );
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
    @ SlideSelect.view(
        ~signal=
          fun
          | Previous =>
            inject(
              Update.SwitchExercise(
                model.current - 1 mod List.length(model.exercises),
              ),
            )
          | Next =>
            inject(
              Update.SwitchExercise(
                model.current + 1 mod List.length(model.exercises),
              ),
            ),
        ~indicator=
          SlideSelect.indicator_n(
            model.current,
            List.length(model.exercises),
          ),
      );
};
