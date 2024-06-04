// A generic key-value store for saving/loading data to/from local storage

type key =
  | Settings
  | ExplainThis
  | Mode
  | Scratch
  | Documentation
  | CurrentExercise
  | Exercise(Exercise.key);

let key_to_string =
  fun
  | Settings => "SETTINGS"
  | ExplainThis => "ExplainThisModel"
  | Mode => "MODE"
  | Scratch => "SAVE_SCRATCH"
  | Documentation => "SAVE_DOCUMENTATION"
  | CurrentExercise => "CUR_EXERCISE"
  | Exercise(key) => key |> Exercise.sexp_of_key |> Sexplib.Sexp.to_string;

module F =
       (
         STORE_KIND: {
           [@deriving (show({with_path: false}), sexp, yojson)]
           type t;
           let default: unit => t;
           let key: key;
         },
       ) => {
  include STORE_KIND;

  let serialize = (data: t) => {
    data |> sexp_of_t |> Sexplib.Sexp.to_string;
  };

  let deserialize = (data: string, default: t) =>
    try(data |> Sexplib.Sexp.of_string |> t_of_sexp) {
    | _ =>
      print_endline("Could not deserialize " ++ key_to_string(key) ++ ".");
      default;
    };

  let save = (data: t): unit =>
    JsUtil.set_localstore(key_to_string(key), serialize(data));

  let init = () => {
    JsUtil.set_localstore(key_to_string(key), serialize(default()));
    default();
  };

  let load = (): t =>
    switch (JsUtil.get_localstore(key_to_string(key))) {
    | None => init()
    | Some(data) => deserialize(data, default())
    };

  let rec export = () =>
    switch (JsUtil.get_localstore(key_to_string(key))) {
    | None =>
      let _ = init();
      export();
    | Some(data) => data
    };

  let import = data => {
    let data = deserialize(data, default());
    save(data);
  };
};

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

//   let to_persistent = ((string, slides)): persistent => (
//     string,
//     List.map(persist, slides),
//   );

//   let of_persistent = (~settings as _, (string, slides): persistent) => {
//     (string, List.map(unpersist, slides));
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
//     let value = ExerciseMode.Model.serialize_one(exercise, ~instructor_mode);
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
