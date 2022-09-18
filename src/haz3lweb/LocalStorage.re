open Js_of_ocaml;
open Sexplib.Std;

let save_settings_key: string = "SETTINGS";
let save_scratch_key: string = "SAVE_SCRATCH";
let action_log_key: string = "ACTION_LOG";
let keystoke_log_key: string = "KEYSTROKE_LOG";
let zipper_log_key: string = "ZIPPER_LOG";

let set_localstore = (k: string, v: string): unit => {
  let local_store =
    Js.Optdef.get(Dom_html.window##.localStorage, () => assert(false));
  local_store##setItem(Js.string(k), Js.string(v));
};

let get_localstore = (k: string): option(string) =>
  try({
    let local_store =
      Js.Optdef.get(Dom_html.window##.localStorage, () => assert(false));
    local_store##getItem(Js.string(k))
    |> (
      x => Js.Opt.get(x, () => assert(false)) |> Js.to_string |> Option.some
    );
  }) {
  | _ => None
  };

let save_settings = (settings: Model.settings): unit =>
  set_localstore(
    save_settings_key,
    settings |> Model.sexp_of_settings |> Sexplib.Sexp.to_string,
  );

let load_settings = (): Model.settings =>
  switch (get_localstore(save_settings_key)) {
  | None => Model.settings_init
  | Some(flag) =>
    try(
      flag
      |> Sexplib.Sexp.of_string
      |> Model.settings_of_sexp
      |> Model.fix_instructor_mode
    ) {
    | _ => Model.settings_init
    }
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type scratch_without_history = (int, list(ScratchSlide.persistent_state));

let prep_scratch_in =
    ((idx, slides): Editors.scratch): scratch_without_history => (
  idx,
  List.map(ScratchSlide.persist, slides),
);

let prep_scratch_out =
    ((idx, slides): scratch_without_history): Editors.scratch => (
  idx,
  List.map(ScratchSlide.unpersist, slides),
);

let save_scratch = (scratch: Editors.scratch): unit => {
  set_localstore(
    save_scratch_key,
    scratch
    |> prep_scratch_in
    |> sexp_of_scratch_without_history
    |> Sexplib.Sexp.to_string,
  );
};

let init_scratch = () => {
  let scratch = Scratch.init();
  save_scratch(scratch);
  scratch;
};

let load_scratch_without_history = (): scratch_without_history =>
  switch (get_localstore(save_scratch_key)) {
  | None => prep_scratch_in(init_scratch())
  | Some(flag) =>
    try(flag |> Sexplib.Sexp.of_string |> scratch_without_history_of_sexp) {
    | _ => prep_scratch_in(init_scratch())
    }
  };

let load_scratch = (): Editors.scratch =>
  load_scratch_without_history() |> prep_scratch_out;

// [@deriving (show({with_path: false}), sexp, yojson)]
// type exercise_store = list(option(SchoolExercise.persistent_state));

// [@deriving (show({with_path: false}), sexp, yojson)]
// type school_without_history = (int, exercise_store);

// let prep_school_in =
//     (
//       (n, _, exercise): Editors.school,
//       exercise_store,
//       ~instructor_mode: bool,
//     )
//     : school_without_history => (
//   n,
//   Util.ListUtil.put_nth(
//     n,
//     Some(
//       SchoolExercise.persistent_state_of_state(exercise, ~instructor_mode),
//     ),
//     exercise_store,
//   ),
// );

// let prep_school_out =
//     (
//       (n, exercise_store): school_without_history,
//       ~specs,
//       ~instructor_mode: bool,
//     )
//     : Editors.school => (
//   n,
//   specs,
//   {
//     let spec = List.nth(specs, n);
//     switch (List.nth(exercise_store, n)) {
//     | Some(exercise) =>
//       SchoolExercise.unpersist_state(exercise, spec, ~instructor_mode)
//     | None => SchoolExercise.state_of_spec(spec, ~instructor_mode)
//     };
//   },
// );

// let init_exercise_store = (~specs): exercise_store => {
//   List.init(List.length(specs), _ => None);
// };

// let load_exercise_store = () => {
//   switch (get_localstore(save_school_key)) {
//   | None => None
//   | Some(s) =>
//     try(
//       Some(
//         s |> Sexplib.Sexp.of_string |> school_without_history_of_sexp |> snd,
//       )
//     ) {
//     | _ => None
//     }
//   };
// };

// let update_exercise_store = (exercise_store, n, exercise) => {
//   Util.ListUtil.put_nth(n, Some(exercise), exercise_store);
// };

// let save_school =
//     ((_, specs, _) as school: Editors.school, ~instructor_mode: bool): unit => {
//   let exercise_store =
//     switch (load_exercise_store()) {
//     | None => init_exercise_store(~specs)
//     | Some(exercise_store) => exercise_store
//     };
//   let value =
//     prep_school_in(school, exercise_store, ~instructor_mode)
//     |> sexp_of_school_without_history
//     |> Sexplib.Sexp.to_string;
//   set_localstore(save_school_key, value);
// };

// let init_school = (~instructor_mode) => {
//   let school = School.init(~instructor_mode);
//   save_school(school, ~instructor_mode);
//   school;
// };

// let rec load_school_without_history =
//         (~instructor_mode: bool): school_without_history =>
//   switch (get_localstore(save_school_key)) {
//   | None =>
//     let school = init_school(~instructor_mode);
//     prep_school_in(school, [], ~instructor_mode);
//   | Some(s) =>
//     try(s |> Sexplib.Sexp.of_string |> school_without_history_of_sexp) {
//     | _ =>
//       let school = init_school(~instructor_mode);
//       prep_school_in(school, [], ~instructor_mode);
//     }
//   };

// let load_school = (~specs, ~instructor_mode: bool): Editors.school =>
//   load_school_without_history(~instructor_mode)
//   |> prep_school_out(~specs, ~instructor_mode);

let prep_exercise_in = (exercise, ~instructor_mode) => {
  SchoolExercise.persistent_state_of_state(exercise, ~instructor_mode)
  |> SchoolExercise.sexp_of_persistent_state
  |> Sexplib.Sexp.to_string;
};

let prep_exercise_out = (exercise_string, ~spec, ~instructor_mode) => {
  exercise_string
  |> Sexplib.Sexp.of_string
  |> SchoolExercise.persistent_state_of_sexp
  |> SchoolExercise.unpersist_state(~spec, ~instructor_mode);
};

let save_exercise = (key, exercise, ~instructor_mode) => {
  let keystring = SchoolExercise.keystring_of_key(key);
  let value = prep_exercise_in(exercise, ~instructor_mode);
  set_localstore(keystring, value);
};

let cur_exercise_key = "CUR-EXERCISE";

let init_exercise = (key, spec, ~instructor_mode) => {
  let keystring = SchoolExercise.keystring_of_key(key);

  let exercise = SchoolExercise.state_of_spec(spec, ~instructor_mode);
  save_exercise(key, exercise, ~instructor_mode);
  set_localstore(cur_exercise_key, keystring);
  exercise;
};

let load_exercise = (key, spec, ~instructor_mode): SchoolExercise.state => {
  let keystring = SchoolExercise.keystring_of_key(key);
  switch (get_localstore(keystring)) {
  | Some(exercise_string) =>
    let exercise =
      try(prep_exercise_out(exercise_string, ~spec, ~instructor_mode)) {
      | _ => init_exercise(key, spec, ~instructor_mode)
      };
    set_localstore(cur_exercise_key, keystring);
    exercise;
  | None => init_exercise(key, spec, ~instructor_mode)
  };
};

let save_school = ((n, specs, exercise), ~instructor_mode) => {
  let key = SchoolExercise.key_of(List.nth(specs, n));
  let keystring = SchoolExercise.keystring_of_key(key);
  save_exercise(key, exercise, ~instructor_mode);
  set_localstore(cur_exercise_key, keystring);
};

let init_school = (~instructor_mode) => {
  let school = School.init(~instructor_mode);
  save_school(school, ~instructor_mode);
  school;
};

let load_school = (~specs, ~instructor_mode): Editors.school => {
  switch (get_localstore(cur_exercise_key)) {
  | Some(keystring) =>
    let key = SchoolExercise.key_of_keystring(keystring);
    switch (SchoolExercise.find_key_opt(key, specs)) {
    | Some((n, spec)) =>
      switch (get_localstore(keystring)) {
      | Some(exercise_string) =>
        let exercise =
          prep_exercise_out(exercise_string, ~spec, ~instructor_mode);
        (n, specs, exercise);
      | None =>
        // initialize exercise from spec
        let exercise = SchoolExercise.state_of_spec(spec, ~instructor_mode);
        save_exercise(key, exercise, ~instructor_mode);
        (n, specs, exercise);
      }
    | None =>
      // invalid current exercise key saved, load the first exercise
      let first_spec = List.nth(specs, 0);
      let first_key = SchoolExercise.key_of(first_spec);
      (0, specs, load_exercise(first_key, first_spec, ~instructor_mode));
    };
  | None => init_school(~instructor_mode)
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type school_export = {
  cur_exercise: SchoolExercise.key,
  exercise_data: list((SchoolExercise.key, SchoolExercise.persistent_state)),
};

let prep_school_export = (specs, ~instructor_mode) => {
  {
    cur_exercise:
      SchoolExercise.key_of_keystring(
        Option.get(get_localstore(cur_exercise_key)),
      ),
    exercise_data:
      specs
      |> List.map(spec => {
           let key = SchoolExercise.key_of(spec);
           let exercise =
             load_exercise(key, spec, ~instructor_mode)
             |> SchoolExercise.persistent_state_of_state(~instructor_mode);
           (key, exercise);
         }),
  };
};

let export_school = (~specs, ~instructor_mode) => {
  prep_school_export(specs, ~instructor_mode)
  |> sexp_of_school_export
  |> Sexplib.Sexp.to_string;
};
