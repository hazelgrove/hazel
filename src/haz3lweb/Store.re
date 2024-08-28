// open Haz3lcore;

// // A generic key-value store for saving/loading data to/from local storage
// module Generic = {
//   let prefix: string = "KEY_STORE_";

//   let full_key = (key: string): string => {
//     prefix ++ key;
//   };

//   let save = (key: string, value: string): unit =>
//     JsUtil.set_localstore(full_key(key), value);

//   let load = (key: string): option(string) =>
//     JsUtil.get_localstore(full_key(key));
// };

// // Settings serialization
// module Settings = {
//   let save_settings_key: string = "SETTINGS";

//   let default = Init.startup.settings;

//   let serialize = settings =>
//     settings |> Settings.sexp_of_t |> Sexplib.Sexp.to_string;

//   let deserialize = data =>
//     try(
//       data
//       |> Sexplib.Sexp.of_string
//       |> Settings.t_of_sexp
//       |> Settings.fix_instructor_mode
//     ) {
//     | _ =>
//       print_endline("Could not deserialize settings.");
//       default;
//     };

//   let save = (settings: Settings.t): unit =>
//     JsUtil.set_localstore(save_settings_key, serialize(settings));

//   let init = () => {
//     JsUtil.set_localstore(save_settings_key, serialize(default));
//     default;
//   };

//   let load = (): Settings.t =>
//     switch (JsUtil.get_localstore(save_settings_key)) {
//     | None => init()
//     | Some(data) => deserialize(data)
//     };

//   let export = () => Option.get(JsUtil.get_localstore(save_settings_key));
//   let import = data => {
//     let settings = deserialize(data);
//     save(settings);
//     settings;
//   };
// };

// // ExplainThisModel serialization
// module ExplainThisModel = {
//   let save_ExplainThisModel_key: string = "ExplainThisModel";

//   let serialize = explainThisModel =>
//     explainThisModel |> ExplainThisModel.sexp_of_t |> Sexplib.Sexp.to_string;

//   let deserialize = data =>
//     try(data |> Sexplib.Sexp.of_string |> ExplainThisModel.t_of_sexp) {
//     | _ =>
//       print_endline("Could not deserialize ExplainThisModel.");
//       ExplainThisModel.init;
//     };

//   let save = (explainThisModel: ExplainThisModel.t): unit =>
//     JsUtil.set_localstore(
//       save_ExplainThisModel_key,
//       serialize(explainThisModel),
//     );

//   let init = () => {
//     JsUtil.set_localstore(
//       save_ExplainThisModel_key,
//       serialize(ExplainThisModel.init),
//     );
//     ExplainThisModel.init;
//   };

//   let load = (): ExplainThisModel.t =>
//     switch (JsUtil.get_localstore(save_ExplainThisModel_key)) {
//     | None => init()
//     | Some(data) => deserialize(data)
//     };

//   let rec export = () =>
//     switch (JsUtil.get_localstore(save_ExplainThisModel_key)) {
//     | None =>
//       let _ = init();
//       export();
//     | Some(data) => data
//     };

//   let import = data => {
//     let explainThisModel = deserialize(data);
//     save(explainThisModel);
//   };
// };

// // Scratch mode serialization
// module Scratch = {
//   let save_scratch_key: string = "SAVE_SCRATCH";

//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type persistent = PersistentData.scratch;

//   let pzipper_to_pstate =
//       (slide: PersistentZipper.t): ScratchSlide.persistent_state => {
//     // {
//     title: "",
//     description: "",
//     hidden_tests: {
//       tests: slide,
//       hints: [],
//     },
//     // };
//   };

//   let to_persistent = ((idx, slides, results)): persistent => {
//     let slides = List.map(ScratchSlide.persist, slides);
//     let slides = List.map(pzipper_to_pstate, slides);
//     (
//       idx,
//       slides,
//       results
//       |> ModelResults.map(ModelResult.to_persistent)
//       |> ModelResults.bindings,
//     );
//   };

//   let of_persistent = (~settings, (idx, slides, results): persistent) => {
//     (
//       idx,
//       List.map(ScratchSlide.unpersist, slides),
//       results
//       |> List.to_seq
//       |> ModelResults.of_seq
//       |> ModelResults.map(ModelResult.of_persistent(~settings)),
//     );
//   };

//   let serialize = scratch => {
//     scratch |> to_persistent |> sexp_of_persistent |> Sexplib.Sexp.to_string;
//   };

//   let deserialize = data => {
//     data |> Sexplib.Sexp.of_string |> persistent_of_sexp |> of_persistent;
//   };

//   let save = (scratch): unit => {
//     JsUtil.set_localstore(save_scratch_key, serialize(scratch));
//   };

//   let init = (~settings) => {
//     let scratch = of_persistent(~settings, Init.startup.scratch);
//     save(scratch);
//     scratch;
//   };

//   let load = (~settings) =>
//     switch (JsUtil.get_localstore(save_scratch_key)) {
//     | None => init(~settings)
//     | Some(data) =>
//       try(deserialize(~settings, data)) {
//       | _ => init(~settings)
//       }
//     };

//   let export = (~settings) => serialize(load(~settings));
//   let import = (~settings, data) => save(deserialize(~settings, data));
// };

// module Documentation = {
//   let save_documentation_key: string = "SAVE_DOCUMENTATION";

//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type persistent = PersistentData.documentation;

//   let persist = ((name, editor: Editor.t)) => {
//     (name, PersistentZipper.persist(editor.state.zipper));
//   };

//   let unpersist = ((name, zipper)) => {
//     let zipper = PersistentZipper.unpersist(zipper);
//     (name, Editor.init(zipper, ~read_only=false));
//   };

//   // let fromEditor = (editor: Editor.t): ScratchSlide.persistent_state => {
//   //   title: "",
//   //   description: "",
//   //   hidden_tests: { tests: { zipper: serialize(editor), backup_text: "" }, hints: [] },
//   // };

//   let pzipper_to_pstate =
//       (slide: PersistentZipper.t): DocumentationEnv.persistent_state => {
//     let position = DocumentationEnv.YourImpl;
//     (position, [(position, slide)]);
//   };

//   let to_persistent = ((string, slides, results)): persistent => {
//     let pz_to_ps = ((str: string, zipper: PersistentZipper.t)) => {
//       (str, pzipper_to_pstate(zipper));
//     };

//     let slides = List.map(persist, slides);
//     let slides = List.map(pz_to_ps, slides);
//     (
//       string,
//       slides,
//       results
//       |> ModelResults.map(ModelResult.to_persistent)
//       |> ModelResults.bindings,
//     );
//   };

//   let of_persistent = (~settings, (string, slides, results): persistent) => {
//     let state_to_zipper = ((str: string, status: DocumentationEnv.persistent_state)) => {
//       let (initial_pos, zippers) = status;
//       let unpersisted_zippers = List.map(unpersist, zippers);  // Apply `unpersist` to each zipper in the list
//       (str, (initial_pos, unpersisted_zippers));
//     };
//     let slides = List.map(state_to_zipper, slides);
//     let slides = List.map((slide) => {
//       let (name, (_, editors)) = slide;  // Extract `name` and the list of editors
//       let first_editor = List.hd(editors);  // Get the first `(pos, editor)` pair from the list
//       let (_, editor) = first_editor;  // Extract the `Editor.t` from the pair
//       (name, editor);  // Return the simplified tuple `(name, editor)`
//     }, slides);
//     let slides = List.map(persist, slides);
//     (
//       string,
//       List.map(unpersist, slides),
//       results
//       |> List.to_seq
//       |> ModelResults.of_seq
//       |> ModelResults.map(ModelResult.of_persistent(~settings)),
//     );
//   };

//   let serialize = slides => {
//     slides |> to_persistent |> sexp_of_persistent |> Sexplib.Sexp.to_string;
//   };

//   let deserialize = data => {
//     data |> Sexplib.Sexp.of_string |> persistent_of_sexp |> of_persistent;
//   };

//   let save = (slides): unit => {
//     JsUtil.set_localstore(save_documentation_key, serialize(slides));
//   };

//   let init = (~settings) => {
//     let documentation = of_persistent(~settings, Init.startup.documentation);
//     save(documentation);
//     documentation;
//   };

//   let load = (~settings) =>
//     switch (JsUtil.get_localstore(save_documentation_key)) {
//     | None => init(~settings)
//     | Some(data) =>
//       try(deserialize(~settings, data)) {
//       | _ => init(~settings)
//       }
//     };

//   let export = (~settings) => serialize(load(~settings));
//   let import = (~settings, data) => save(deserialize(~settings, data));
// };

// module Exercise = {
//   open Exercise;

//   let cur_exercise_key = "CUR_EXERCISE";

//   let keystring_of_key = key => {
//     key |> sexp_of_key |> Sexplib.Sexp.to_string;
//   };

//   let keystring_of = p => {
//     key_of(p) |> keystring_of_key;
//   };

//   let key_of_keystring = keystring => {
//     keystring |> Sexplib.Sexp.of_string |> key_of_sexp;
//   };

//   let save_exercise_key = key => {
//     JsUtil.set_localstore(cur_exercise_key, keystring_of_key(key));
//   };

//   let save_exercise = (exercise, ~instructor_mode) => {
//     let key = Exercise.key_of_state(exercise);
//     let keystring = keystring_of_key(key);
//     let value = Exercise.serialize_exercise(exercise, ~instructor_mode);
//     JsUtil.set_localstore(keystring, value);
//   };

//   let init_exercise = (spec, ~instructor_mode) => {
//     let key = Exercise.key_of(spec);
//     let keystring = keystring_of_key(key);
//     let exercise = Exercise.state_of_spec(spec, ~instructor_mode);
//     save_exercise(exercise, ~instructor_mode);
//     JsUtil.set_localstore(cur_exercise_key, keystring);
//     exercise;
//   };

//   let load_exercise = (key, spec, ~instructor_mode): Exercise.state => {
//     let keystring = keystring_of_key(key);
//     switch (JsUtil.get_localstore(keystring)) {
//     | Some(data) =>
//       let exercise =
//         try(Exercise.deserialize_exercise(data, ~spec, ~instructor_mode)) {
//         | _ => init_exercise(spec, ~instructor_mode)
//         };
//       JsUtil.set_localstore(cur_exercise_key, keystring);
//       exercise;
//     | None => init_exercise(spec, ~instructor_mode)
//     };
//   };

//   let save = ((n, specs, exercise), ~instructor_mode) => {
//     let key = key_of(List.nth(specs, n));
//     let keystring = keystring_of_key(key);
//     save_exercise(exercise, ~instructor_mode);
//     JsUtil.set_localstore(cur_exercise_key, keystring);
//   };

//   let init = (~instructor_mode) => {
//     let exercises = {
//       (
//         0,
//         ExerciseSettings.exercises,
//         List.nth(ExerciseSettings.exercises, 0)
//         |> Exercise.state_of_spec(~instructor_mode),
//       );
//     };
//     save(exercises, ~instructor_mode);
//     exercises;
//   };

//   let load = (~specs, ~instructor_mode) => {
//     switch (JsUtil.get_localstore(cur_exercise_key)) {
//     | Some(keystring) =>
//       let key = key_of_keystring(keystring);
//       switch (Exercise.find_key_opt(key, specs)) {
//       | Some((n, spec)) =>
//         switch (JsUtil.get_localstore(keystring)) {
//         | Some(data) =>
//           let exercise =
//             try(deserialize_exercise(data, ~spec, ~instructor_mode)) {
//             | _ => init_exercise(spec, ~instructor_mode)
//             };
//           (n, specs, exercise);
//         | None =>
//           // initialize exercise from spec
//           let exercise = Exercise.state_of_spec(spec, ~instructor_mode);
//           save_exercise(exercise, ~instructor_mode);
//           (n, specs, exercise);
//         }
//       | None =>
//         // invalid current exercise key saved, load the first exercise
//         let first_spec = List.nth(specs, 0);
//         let first_key = Exercise.key_of(first_spec);
//         (0, specs, load_exercise(first_key, first_spec, ~instructor_mode));
//       };
//     | None => init(~instructor_mode)
//     };
//   };

//   let prep_exercise_export = (~specs, ~instructor_mode) => {
//     {
//       cur_exercise:
//         key_of_keystring(
//           Option.get(JsUtil.get_localstore(cur_exercise_key)),
//         ),
//       exercise_data:
//         specs
//         |> List.map(spec => {
//              let key = Exercise.key_of(spec);
//              let exercise =
//                load_exercise(key, spec, ~instructor_mode)
//                |> Exercise.persistent_state_of_state(~instructor_mode);
//              (key, exercise);
//            }),
//     };
//   };

//   let serialize_exercise_export = (~specs, ~instructor_mode) => {
//     prep_exercise_export(~specs, ~instructor_mode)
//     |> sexp_of_exercise_export
//     |> Sexplib.Sexp.to_string;
//   };

//   let export = (~specs, ~instructor_mode) => {
//     serialize_exercise_export(~specs, ~instructor_mode);
//   };

//   let import = (data, ~specs, ~instructor_mode) => {
//     let exercise_export = data |> deserialize_exercise_export;
//     save_exercise_key(exercise_export.cur_exercise);
//     exercise_export.exercise_data
//     |> List.iter(((key, persistent_state)) => {
//          let spec = Exercise.find_key_opt(key, specs);
//          switch (spec) {
//          | None =>
//            print_endline("Warning: saved key does not correspond to exercise")
//          | Some((_, spec)) =>
//            save_exercise(
//              Exercise.unpersist_state(
//                persistent_state,
//                ~spec,
//                ~instructor_mode,
//              ),
//              ~instructor_mode,
//            )
//          };
//        });
//   };
// };

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

  let pzipper_to_pstate =
      (slide: PersistentZipper.t): ScratchSlide.persistent_state => {
    // {
    title: "",
    description: "",
    hidden_tests: {
      tests: slide,
      hints: [],
    },
    // };
  };

  let to_persistent = ((idx, slides, results)): persistent => {
    let slides = List.map(ScratchSlide.persist, slides);
    let slides = List.map(pzipper_to_pstate, slides);
    (
      idx,
      slides,
      results
      |> ModelResults.map(ModelResult.to_persistent)
      |> ModelResults.bindings,
    );
  };

  let of_persistent = (~settings, (idx, slides, results): persistent) => {
    (
      idx,
      List.map(ScratchSlide.unpersist, slides),
      results
      |> List.to_seq
      |> ModelResults.of_seq
      |> ModelResults.map(ModelResult.of_persistent(~settings)),
    );
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

  let init = (~settings) => {
    let scratch = of_persistent(~settings, Init.startup.scratch);
    save(scratch);
    scratch;
  };

  let load = (~settings) =>
    switch (JsUtil.get_localstore(save_scratch_key)) {
    | None => init(~settings)
    | Some(data) =>
      try(deserialize(~settings, data)) {
      | _ => init(~settings)
      }
    };

  let export = (~settings) => serialize(load(~settings));
  let import = (~settings, data) => save(deserialize(~settings, data));
};

module Documentation = {
  let save_documentation_key: string = "SAVE_DOCUMENTATION";

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = PersistentData.documentation;

  let persist = ((name, editor: Editor.t)) => {
    (name, PersistentZipper.persist(editor.state.zipper));
  };

  let unpersist = ((name, zipper)) => {
    let zipper = PersistentZipper.unpersist(zipper);
    (name, Editor.init(zipper, ~read_only=false));
  };

  // let fromEditor = (editor: Editor.t): ScratchSlide.persistent_state => {
  //   title: "",
  //   description: "",
  //   hidden_tests: { tests: { zipper: serialize(editor), backup_text: "" }, hints: [] },
  // };

  let pzipper_to_pstate =
      (slide: PersistentZipper.t): ScratchSlide.persistent_state => {
    // {
    title: "",
    description: "",
    hidden_tests: {
      tests: slide,
      hints: [],
    },
    // };
  };

  let to_persistent = ((string, slides, results)): persistent => {
    let pz_to_ps = ((str: string, zipper: PersistentZipper.t)) => {
      (str, pzipper_to_pstate(zipper));
    };

    let slides = List.map(persist, slides);
    let slides = List.map(pz_to_ps, slides);
    (
      string,
      slides,
      results
      |> ModelResults.map(ModelResult.to_persistent)
      |> ModelResults.bindings,
    );
  };

  let of_persistent = (~settings, (string, slides, results): persistent) => {
    let state_to_zipper =
        ((str: string, status: ScratchSlide.persistent_state)) => {
      (str, ScratchSlide.unpersist(status));
    };
    let slides = List.map(state_to_zipper, slides);
    let slides = List.map(persist, slides);
    (
      string,
      List.map(unpersist, slides),
      results
      |> List.to_seq
      |> ModelResults.of_seq
      |> ModelResults.map(ModelResult.of_persistent(~settings)),
    );
  };

  let serialize = slides => {
    slides |> to_persistent |> sexp_of_persistent |> Sexplib.Sexp.to_string;
  };

  let deserialize = data => {
    data |> Sexplib.Sexp.of_string |> persistent_of_sexp |> of_persistent;
  };

  let save = (slides): unit => {
    JsUtil.set_localstore(save_documentation_key, serialize(slides));
  };

  let init = (~settings) => {
    let documentation = of_persistent(~settings, Init.startup.documentation);
    save(documentation);
    documentation;
  };

  let load = (~settings) =>
    switch (JsUtil.get_localstore(save_documentation_key)) {
    | None => init(~settings)
    | Some(data) =>
      try(deserialize(~settings, data)) {
      | _ => init(~settings)
      }
    };

  let export = (~settings) => serialize(load(~settings));
  let import = (~settings, data) => save(deserialize(~settings, data));
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

  let init_exercise = (spec, ~instructor_mode) => {
    let key = Exercise.key_of(spec);
    let keystring = keystring_of_key(key);
    let exercise = Exercise.state_of_spec(spec, ~instructor_mode);
    save_exercise(exercise, ~instructor_mode);
    JsUtil.set_localstore(cur_exercise_key, keystring);
    exercise;
  };

  let load_exercise = (key, spec, ~instructor_mode): Exercise.state => {
    let keystring = keystring_of_key(key);
    switch (JsUtil.get_localstore(keystring)) {
    | Some(data) =>
      let exercise =
        try(Exercise.deserialize_exercise(data, ~spec, ~instructor_mode)) {
        | _ => init_exercise(spec, ~instructor_mode)
        };
      JsUtil.set_localstore(cur_exercise_key, keystring);
      exercise;
    | None => init_exercise(spec, ~instructor_mode)
    };
  };

  let save = ((n, specs, exercise), ~instructor_mode) => {
    let key = key_of(List.nth(specs, n));
    let keystring = keystring_of_key(key);
    save_exercise(exercise, ~instructor_mode);
    JsUtil.set_localstore(cur_exercise_key, keystring);
  };

  let init = (~instructor_mode) => {
    let exercises = {
      (
        0,
        ExerciseSettings.exercises,
        List.nth(ExerciseSettings.exercises, 0)
        |> Exercise.state_of_spec(~instructor_mode),
      );
    };
    save(exercises, ~instructor_mode);
    exercises;
  };

  let load = (~specs, ~instructor_mode) => {
    switch (JsUtil.get_localstore(cur_exercise_key)) {
    | Some(keystring) =>
      let key = key_of_keystring(keystring);
      switch (Exercise.find_key_opt(key, specs)) {
      | Some((n, spec)) =>
        switch (JsUtil.get_localstore(keystring)) {
        | Some(data) =>
          let exercise =
            try(deserialize_exercise(data, ~spec, ~instructor_mode)) {
            | _ => init_exercise(spec, ~instructor_mode)
            };
          (n, specs, exercise);
        | None =>
          // initialize exercise from spec
          let exercise = Exercise.state_of_spec(spec, ~instructor_mode);
          save_exercise(exercise, ~instructor_mode);
          (n, specs, exercise);
        }
      | None =>
        // invalid current exercise key saved, load the first exercise
        let first_spec = List.nth(specs, 0);
        let first_key = Exercise.key_of(first_spec);
        (0, specs, load_exercise(first_key, first_spec, ~instructor_mode));
      };
    | None => init(~instructor_mode)
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
               load_exercise(key, spec, ~instructor_mode)
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

  let export = (~specs, ~instructor_mode) => {
    serialize_exercise_export(~specs, ~instructor_mode);
  };

  let import = (data, ~specs, ~instructor_mode) => {
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
             ),
             ~instructor_mode,
           )
         };
       });
  };
};
