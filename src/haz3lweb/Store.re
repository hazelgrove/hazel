open Haz3lcore;

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

// LangDocMessages serialization
module LangDocMessages = {
  let save_langDocMessages_key: string = "LANGDOCMESSAGES";

  let serialize = langDocMessages =>
    LangDocMessages.serialize(langDocMessages);

  let deserialize = data =>
    try(LangDocMessages.deserialize(data)) {
    | _ =>
      print_endline("Could not deserialize langDocMessages.");
      LangDocMessages.init;
    };

  let save = (langDocMessages: LangDocMessages.t): unit =>
    JsUtil.set_localstore(
      save_langDocMessages_key,
      serialize(langDocMessages),
    );

  let init = () => {
    JsUtil.set_localstore(
      save_langDocMessages_key,
      serialize(LangDocMessages.init),
    );
    LangDocMessages.init;
  };

  let load = (): LangDocMessages.t =>
    switch (JsUtil.get_localstore(save_langDocMessages_key)) {
    | None => init()
    | Some(data) => deserialize(data)
    };

  let rec export = () =>
    switch (JsUtil.get_localstore(save_langDocMessages_key)) {
    | None =>
      let _ = init();
      export();
    | Some(data) => data
    };

  let import = data => {
    let langDocMessages = deserialize(data);
    save(langDocMessages);
  };
};

// Scratch mode serialization
module Scratch = {
  let save_scratch_key: string = "SAVE_SCRATCH";

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = PersistentData.scratch;

  let to_persistent = ((idx, slides)): persistent => (
    idx,
    List.map(ScratchSlide.persist, slides),
  );

  let of_persistent = ((idx, slides): persistent, ~inference_enabled) => {
    (idx, List.map(ScratchSlide.unpersist(~inference_enabled), slides));
  };

  let serialize = scratch => {
    scratch |> to_persistent |> sexp_of_persistent |> Sexplib.Sexp.to_string;
  };

  let deserialize = data => {
    data |> Sexplib.Sexp.of_string |> persistent_of_sexp |> of_persistent;
  };

  let save = (scratch): unit => {
    JsUtil.set_localstore(save_scratch_key, serialize(scratch));
  };

  let init = (~inference_enabled) => {
    let scratch = of_persistent(Init.startup.scratch, ~inference_enabled);
    save(scratch);
    scratch;
  };

  let load = (~inference_enabled) =>
    switch (JsUtil.get_localstore(save_scratch_key)) {
    | None => init(~inference_enabled)
    | Some(data) =>
      try(deserialize(data, ~inference_enabled)) {
      | _ => init(~inference_enabled)
      }
    };

  let export = () => serialize(load(~inference_enabled=false));
  let import = (data, ~inference_enabled) =>
    save(deserialize(data, ~inference_enabled));
};

module Examples = {
  let save_examples_key: string = "SAVE_EXAMPLES";

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = PersistentData.examples;

  let persist = ((name, editor: Editor.t)) => {
    (name, PersistentZipper.persist(editor.state.zipper));
  };

  let unpersist = (~inference_enabled, (name, zipper)) => {
    let zipper = PersistentZipper.unpersist(zipper);
    (name, Editor.init(zipper, ~read_only=false, ~inference_enabled));
  };

  let to_persistent = ((string, slides)): persistent => (
    string,
    List.map(persist, slides),
  );

  let of_persistent = ((string, slides): persistent, ~inference_enabled) => {
    (string, List.map(unpersist(~inference_enabled), slides));
  };

  let serialize = examples => {
    examples |> to_persistent |> sexp_of_persistent |> Sexplib.Sexp.to_string;
  };

  let deserialize = data => {
    data |> Sexplib.Sexp.of_string |> persistent_of_sexp |> of_persistent;
  };

  let save = (examples): unit => {
    JsUtil.set_localstore(save_examples_key, serialize(examples));
  };

  let init = (~inference_enabled) => {
    let examples = of_persistent(Init.startup.examples, ~inference_enabled);
    save(examples);
    examples;
  };

  let load = (~inference_enabled) =>
    switch (JsUtil.get_localstore(save_examples_key)) {
    | None => init(~inference_enabled)
    | Some(data) =>
      try(deserialize(data, ~inference_enabled)) {
      | _ => init(~inference_enabled)
      }
    };

  let export = () => serialize(load(~inference_enabled=false));
  let import = data => save(deserialize(data, ~inference_enabled=false));
};

module Exercise = {
  open Exercise;

  let cur_exercise_key = "CUR_EXERCISE";

  let keystring_of_key = key => {
    key |> sexp_of_key |> Sexplib.Sexp.to_string;
  };

  let keystring_of = p => {
    key_of(p) |> keystring_of_key;
  };

  let key_of_keystring = keystring => {
    keystring |> Sexplib.Sexp.of_string |> key_of_sexp;
  };

  let save_exercise_key = key => {
    JsUtil.set_localstore(cur_exercise_key, keystring_of_key(key));
  };

  let save_exercise = (exercise, ~instructor_mode) => {
    let key = Exercise.key_of_state(exercise);
    let keystring = keystring_of_key(key);
    let value = Exercise.serialize_exercise(exercise, ~instructor_mode);
    JsUtil.set_localstore(keystring, value);
  };

  let init_exercise = (spec, ~instructor_mode, ~inference_enabled) => {
    let key = Exercise.key_of(spec);
    let keystring = keystring_of_key(key);
    let exercise =
      Exercise.state_of_spec(spec, ~instructor_mode, ~inference_enabled);
    save_exercise(exercise, ~instructor_mode);
    JsUtil.set_localstore(cur_exercise_key, keystring);
    exercise;
  };

  let load_exercise =
      (key, spec, ~instructor_mode, ~inference_enabled): Exercise.state => {
    let keystring = keystring_of_key(key);
    switch (JsUtil.get_localstore(keystring)) {
    | Some(data) =>
      let exercise =
        try(
          Exercise.deserialize_exercise(
            data,
            ~spec,
            ~instructor_mode,
            ~inference_enabled,
          )
        ) {
        | _ => init_exercise(spec, ~instructor_mode, ~inference_enabled)
        };
      JsUtil.set_localstore(cur_exercise_key, keystring);
      exercise;
    | None => init_exercise(spec, ~instructor_mode, ~inference_enabled)
    };
  };

  let save = ((n, specs, exercise), ~instructor_mode) => {
    let key = key_of(List.nth(specs, n));
    let keystring = keystring_of_key(key);
    save_exercise(exercise, ~instructor_mode);
    JsUtil.set_localstore(cur_exercise_key, keystring);
  };

  let init = (~instructor_mode, ~inference_enabled) => {
    let exercises = {
      (
        0,
        ExerciseSettings.exercises,
        List.nth(ExerciseSettings.exercises, 0)
        |> Exercise.state_of_spec(~instructor_mode, ~inference_enabled),
      );
    };
    save(exercises, ~instructor_mode);
    exercises;
  };

  let load = (~specs, ~instructor_mode, ~inference_enabled) => {
    switch (JsUtil.get_localstore(cur_exercise_key)) {
    | Some(keystring) =>
      let key = key_of_keystring(keystring);
      switch (Exercise.find_key_opt(key, specs)) {
      | Some((n, spec)) =>
        switch (JsUtil.get_localstore(keystring)) {
        | Some(data) =>
          let exercise =
            try(
              deserialize_exercise(
                data,
                ~spec,
                ~instructor_mode,
                ~inference_enabled,
              )
            ) {
            | _ => init_exercise(spec, ~instructor_mode, ~inference_enabled)
            };
          (n, specs, exercise);
        | None =>
          // initialize exercise from spec
          let exercise =
            Exercise.state_of_spec(
              spec,
              ~instructor_mode,
              ~inference_enabled,
            );
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
          load_exercise(
            first_key,
            first_spec,
            ~instructor_mode,
            ~inference_enabled,
          ),
        );
      };
    | None => init(~instructor_mode, ~inference_enabled)
    };
  };

  let prep_exercise_export = (~specs, ~instructor_mode) => {
    {
      cur_exercise:
        key_of_keystring(
          Option.get(JsUtil.get_localstore(cur_exercise_key)),
        ),
      exercise_data:
        specs
        |> List.map(spec => {
             let key = Exercise.key_of(spec);
             let exercise =
               load_exercise(
                 key,
                 spec,
                 ~instructor_mode,
                 ~inference_enabled=false,
               )
               |> Exercise.persistent_state_of_state(~instructor_mode);
             (key, exercise);
           }),
    };
  };

  let serialize_exercise_export = (~specs, ~instructor_mode) => {
    prep_exercise_export(~specs, ~instructor_mode)
    |> sexp_of_exercise_export
    |> Sexplib.Sexp.to_string;
  };

  let export = (~specs) => {
    serialize_exercise_export(~specs);
  };

  let import = (data, ~specs, ~instructor_mode, ~inference_enabled) => {
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
               ~inference_enabled,
             ),
             ~instructor_mode,
           )
         };
       });
  };
};
