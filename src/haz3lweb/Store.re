open Haz3lcore;
open Util;

// A generic key-value store for saving/loading data to/from local storage
module Generic = {
  let prefix: string = "KEY_STORE_";

  let full_key = (key: string): string => {
    prefix ++ key;
  };

  let save = (key: string, value: string): unit =>
    JsUtil.set_localstore(full_key(key), value);

  let load = (key: string): option(string) =>
    JsUtil.get_localstore(full_key(key));
};

// Settings serialization
module Settings = {
  let save_settings_key: string = "SETTINGS";

  let default = Init.startup.settings;

  let serialize = settings =>
    settings |> Settings.sexp_of_t |> Sexplib.Sexp.to_string;

  let deserialize = data =>
    try(
      data
      |> Sexplib.Sexp.of_string
      |> Settings.t_of_sexp
      |> Settings.fix_instructor_mode
    ) {
    | _ =>
      print_endline("Could not deserialize settings.");
      default;
    };

  let save = (settings: Settings.t): unit =>
    JsUtil.set_localstore(save_settings_key, serialize(settings));

  let init = () => {
    JsUtil.set_localstore(save_settings_key, serialize(default));
    default;
  };

  let load = (): Settings.t =>
    switch (JsUtil.get_localstore(save_settings_key)) {
    | None => init()
    | Some(data) => deserialize(data)
    };

  let export = () => Option.get(JsUtil.get_localstore(save_settings_key));
  let import = data => {
    let settings = deserialize(data);
    save(settings);
    settings;
  };
};

// ExplainThisModel serialization
module ExplainThisModel = {
  let save_ExplainThisModel_key: string = "ExplainThisModel";

  let serialize = explainThisModel =>
    explainThisModel |> ExplainThisModel.sexp_of_t |> Sexplib.Sexp.to_string;

  let deserialize = data =>
    try(data |> Sexplib.Sexp.of_string |> ExplainThisModel.t_of_sexp) {
    | _ =>
      print_endline("Could not deserialize ExplainThisModel.");
      ExplainThisModel.init;
    };

  let save = (explainThisModel: ExplainThisModel.t): unit =>
    JsUtil.set_localstore(
      save_ExplainThisModel_key,
      serialize(explainThisModel),
    );

  let init = () => {
    JsUtil.set_localstore(
      save_ExplainThisModel_key,
      serialize(ExplainThisModel.init),
    );
    ExplainThisModel.init;
  };

  let load = (): ExplainThisModel.t =>
    switch (JsUtil.get_localstore(save_ExplainThisModel_key)) {
    | None => init()
    | Some(data) => deserialize(data)
    };

  let rec export = () =>
    switch (JsUtil.get_localstore(save_ExplainThisModel_key)) {
    | None =>
      let _ = init();
      export();
    | Some(data) => data
    };

  let import = data => {
    let explainThisModel = deserialize(data);
    save(explainThisModel);
  };
};

// Scratch mode serialization
module Scratch = {
  let save_scratch_key: string = "SAVE_SCRATCH";

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = PersistentData.scratch;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (int, list(Editor.t), ModelResults.M.t(ModelResult.t));

  let to_persistent = ((idx, slides, results)): persistent => (
    idx,
    List.map(ScratchSlide.persist, slides),
    results
    |> ModelResults.map(ModelResult.to_persistent)
    |> ModelResults.bindings,
  );

  let of_persistent =
      (~settings: CoreSettings.t, (idx, slides, results): persistent): t => {
    (
      idx,
      List.map(ScratchSlide.unpersist(~settings), slides),
      results
      |> List.to_seq
      |> ModelResults.of_seq
      |> ModelResults.map(
           ModelResult.of_persistent(~settings=settings.evaluation),
         ),
    );
  };

  let serialize = (scratch: t): string => {
    scratch |> to_persistent |> sexp_of_persistent |> Sexplib.Sexp.to_string;
  };

  let deserialize = (data: string, ~settings: CoreSettings.t): t => {
    data
    |> Sexplib.Sexp.of_string
    |> persistent_of_sexp
    |> of_persistent(~settings);
  };

  let save = (scratch: t): unit => {
    JsUtil.set_localstore(save_scratch_key, serialize(scratch));
  };

  let init = (~settings: CoreSettings.t): t => {
    let scratch = of_persistent(~settings, Init.startup.scratch);
    save(scratch);
    scratch;
  };

  let load = (~settings: CoreSettings.t): t =>
    switch (JsUtil.get_localstore(save_scratch_key)) {
    | None => init(~settings)
    | Some(data) =>
      try(deserialize(~settings, data)) {
      | _ => init(~settings)
      }
    };

  let export = (~settings: CoreSettings.t): string =>
    serialize(load(~settings));
  let import = (~settings: CoreSettings.t, data: string): unit =>
    save(deserialize(~settings, data));
};

module Documentation = {
  let save_documentation_key: string = "SAVE_DOCUMENTATION";

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = PersistentData.documentation;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (
    string,
    list((string, Editor.t)),
    ModelResults.M.t(ModelResult.t),
  );

  let persist = ((name, editor: Editor.t)) => {
    (name, PersistentZipper.persist(editor.state.zipper));
  };

  let unpersist = ((name, zipper), ~settings: CoreSettings.t) => {
    let zipper = PersistentZipper.unpersist(zipper, ~root=Exp);
    (name, Editor.init(zipper, ~read_only=false, ~settings, ~sort=Exp));
  };

  let to_persistent = ((string, slides, results)): persistent => (
    string,
    List.map(persist, slides),
    results
    |> ModelResults.map(ModelResult.to_persistent)
    |> ModelResults.bindings,
  );

  let of_persistent =
      (~settings: CoreSettings.t, (string, slides, results): persistent): t => {
    (
      string,
      List.map(unpersist(~settings), slides),
      results
      |> List.to_seq
      |> ModelResults.of_seq
      |> ModelResults.map(
           ModelResult.of_persistent(~settings=settings.evaluation),
         ),
    );
  };

  let serialize = (slides: t): string => {
    slides |> to_persistent |> sexp_of_persistent |> Sexplib.Sexp.to_string;
  };

  let deserialize = (~settings: CoreSettings.t, data: string): t => {
    data
    |> Sexplib.Sexp.of_string
    |> persistent_of_sexp
    |> of_persistent(~settings);
  };

  let save = (slides: t): unit => {
    JsUtil.set_localstore(save_documentation_key, serialize(slides));
  };

  let init = (~settings: CoreSettings.t): t => {
    let documentation = of_persistent(~settings, Init.startup.documentation);
    save(documentation);
    documentation;
  };

  let load = (~settings: CoreSettings.t): t =>
    switch (JsUtil.get_localstore(save_documentation_key)) {
    | None => init(~settings)
    | Some(data) =>
      try(deserialize(~settings, data)) {
      | _ => init(~settings)
      }
    };

  let export = (~settings: CoreSettings.t): string =>
    serialize(load(~settings));
  let import = (~settings: CoreSettings.t, data: string): unit =>
    save(deserialize(~settings, data));
};

module Exercise = {
  open Exercise;

  let cur_exercise_key = "CUR_EXERCISE";

  let keystring_of_key = key => {
    key |> sexp_of_key |> Sexplib.Sexp.to_string;
  };

  let keystring_of = p => {
    key(p) |> keystring_of_key;
  };

  let key_of_keystring = keystring => {
    keystring |> Sexplib.Sexp.of_string |> key_of_sexp;
  };

  let save_exercise_key = key => {
    JsUtil.set_localstore(cur_exercise_key, keystring_of_key(key));
  };

  let save_exercise = (exercise, ~instructor_mode): unit => {
    let key = Exercise.key_of_state(exercise);
    let keystring = keystring_of_key(key);
    ignore(instructor_mode);
    let value = Exercise.serialize_exercise(exercise);
    JsUtil.set_localstore(keystring, value);
  };

  let init_exercise =
      (~settings: CoreSettings.t, spec, ~instructor_mode): state => {
    let key = Exercise.key_of(spec);
    let keystring = keystring_of_key(key);
    let exercise = Exercise.state_of_spec(spec, ~instructor_mode, ~settings);
    save_exercise(exercise, ~instructor_mode);
    JsUtil.set_localstore(cur_exercise_key, keystring);
    exercise;
  };

  let load_exercise =
      (~settings: CoreSettings.t, key, spec, ~instructor_mode): Exercise.state => {
    let keystring = keystring_of_key(key);
    switch (JsUtil.get_localstore(keystring)) {
    | Some(data) =>
      let exercise =
        try(
          Exercise.deserialize_exercise(
            data,
            ~instructor_mode,
            ~settings,
            ~spec,
          )
        ) {
        | _ => init_exercise(spec, ~instructor_mode, ~settings)
        };
      JsUtil.set_localstore(cur_exercise_key, keystring);
      exercise;
    | None => init_exercise(spec, ~instructor_mode, ~settings)
    };
  };

  let save = ((n, specs, exercise), ~instructor_mode): unit => {
    let key = key_of(List.nth(specs, n));
    let keystring = keystring_of_key(key);
    save_exercise(exercise, ~instructor_mode);
    JsUtil.set_localstore(cur_exercise_key, keystring);
  };

  let init =
      (~settings: CoreSettings.t, ~instructor_mode)
      : (int, list(spec), state) => {
    let exercises = {
      (
        0,
        ExerciseSettings.exercises,
        List.nth(ExerciseSettings.exercises, 0)
        |> Exercise.state_of_spec(~instructor_mode, ~settings),
      );
    };
    save(exercises, ~instructor_mode);
    exercises;
  };

  let load =
      (~settings: CoreSettings.t, ~specs, ~instructor_mode)
      : (int, list(p(ZipperBase.t)), state) => {
    switch (JsUtil.get_localstore(cur_exercise_key)) {
    | Some(keystring) =>
      let key = key_of_keystring(keystring);
      switch (Exercise.find_key_opt(key, specs)) {
      | Some((n, spec)) =>
        switch (JsUtil.get_localstore(keystring)) {
        | Some(data) =>
          let exercise =
            try(
              deserialize_exercise(data, ~spec, ~instructor_mode, ~settings)
            ) {
            // | err => raise(err)
            | _ => init_exercise(spec, ~instructor_mode, ~settings)
            };
          (n, specs, exercise);
        | None =>
          // initialize exercise from spec
          let exercise =
            Exercise.state_of_spec(spec, ~instructor_mode, ~settings);
          save_exercise(exercise, ~instructor_mode);
          (n, specs, exercise);
        }
      | None =>
        // invalid current exercise key saved, load the first exercise
        let first_spec = List.nth(specs, 0);
        let first_key = Exercise.key_of(first_spec);
        (
          0,
          specs,
          load_exercise(first_key, first_spec, ~instructor_mode, ~settings),
        );
      };
    | None => init(~instructor_mode, ~settings)
    };
  };

  let prep_exercise_export =
      (~specs, ~instructor_mode: bool, ~settings: CoreSettings.t)
      : exercise_export => {
    {
      cur_exercise:
        key_of_keystring(
          Option.get(JsUtil.get_localstore(cur_exercise_key)),
        ),
      exercise_data:
        specs
        |> List.map(spec => {
             let key = Exercise.key(spec);
             let exercise =
               load_exercise(key, spec, ~instructor_mode, ~settings)
               |> Exercise.persistent_state_of_state;
             (key, exercise);
           }),
    };
  };

  let serialize_exercise_export =
      (~specs, ~instructor_mode, ~settings: CoreSettings.t) => {
    prep_exercise_export(~specs, ~instructor_mode, ~settings)
    |> sexp_of_exercise_export
    |> Sexplib.Sexp.to_string;
  };

  let export = (~specs, ~instructor_mode) => {
    serialize_exercise_export(~specs, ~instructor_mode);
  };

  let import =
      (data, ~specs, ~instructor_mode: bool, ~settings: CoreSettings.t) => {
    let exercise_export = data |> deserialize_exercise_export;
    save_exercise_key(exercise_export.cur_exercise);
    exercise_export.exercise_data
    |> List.iter(((key, persistent_state)) => {
         let spec = Exercise.find_key_opt(key, specs);
         switch (spec) {
         | None =>
           print_endline("Warning: saved key does not correspond to exercise")
         | Some((_, spec)) =>
           save_exercise(
             Exercise.unpersist_state(
               persistent_state,
               ~spec,
               ~instructor_mode,
               ~settings,
             ),
             ~instructor_mode,
           )
         };
       });
  };
};
