open Js_of_ocaml;
open Sexplib.Std;

// General API
// TODO: move to JSUtil
module Util = {
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
};

// Settings serialization
module Settings = {
  let save_settings_key: string = "SETTINGS";

  let serialize = settings =>
    settings |> Model.sexp_of_settings |> Sexplib.Sexp.to_string;

  let deserialize = data =>
    try(
      data
      |> Sexplib.Sexp.of_string
      |> Model.settings_of_sexp
      |> Model.fix_instructor_mode
    ) {
    | _ =>
      print_endline("Could not deserialize settings.");
      Model.settings_init;
    };

  let save = (settings: Model.settings): unit =>
    Util.set_localstore(save_settings_key, serialize(settings));

  let load = (): Model.settings =>
    switch (Util.get_localstore(save_settings_key)) {
    | None => Model.settings_init
    | Some(data) => deserialize(data)
    };

  let export = () => Option.get(Util.get_localstore(save_settings_key));
  let import = data => {
    let settings = deserialize(data);
    save(settings);
    settings;
  };
};

// Scratch mode serialization
module Scratch = {
  let save_scratch_key: string = "SAVE_SCRATCH";

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = (int, list(ScratchSlide.persistent_state));

  let to_persistent = ((idx, slides): Editors.scratch): persistent => (
    idx,
    List.map(ScratchSlide.persist, slides),
  );

  let of_persistent = ((idx, slides)): Editors.scratch => {
    (idx, List.map(ScratchSlide.unpersist, slides));
  };

  let serialize = scratch => {
    scratch |> to_persistent |> sexp_of_persistent |> Sexplib.Sexp.to_string;
  };

  let deserialize = data => {
    data |> Sexplib.Sexp.of_string |> persistent_of_sexp |> of_persistent;
  };

  let save = (scratch: Editors.scratch): unit => {
    Util.set_localstore(save_scratch_key, serialize(scratch));
  };

  let init = () => {
    let scratch = Scratch.init();
    save(scratch);
    scratch;
  };

  let load = () =>
    switch (Util.get_localstore(save_scratch_key)) {
    | None => init()
    | Some(data) =>
      try(deserialize(data)) {
      | _ => init()
      }
    };

  let export = () => serialize(load());
  let import = data => save(deserialize(data));
};

module School = {
  open SchoolExercise;

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
    Util.set_localstore(cur_exercise_key, keystring_of_key(key));
  };

  let serialize_exercise = (exercise, ~instructor_mode) => {
    SchoolExercise.persistent_state_of_state(exercise, ~instructor_mode)
    |> SchoolExercise.sexp_of_persistent_state
    |> Sexplib.Sexp.to_string;
  };

  let deserialize_exercise = (data, ~spec, ~instructor_mode) => {
    data
    |> Sexplib.Sexp.of_string
    |> SchoolExercise.persistent_state_of_sexp
    |> SchoolExercise.unpersist_state(~spec, ~instructor_mode);
  };

  let save_exercise = (exercise, ~instructor_mode) => {
    let key = SchoolExercise.key_of_state(exercise);
    let keystring = keystring_of_key(key);
    let value = serialize_exercise(exercise, ~instructor_mode);
    Util.set_localstore(keystring, value);
  };

  let init_exercise = (spec, ~instructor_mode) => {
    let key = SchoolExercise.key_of(spec);
    let keystring = keystring_of_key(key);
    let exercise = SchoolExercise.state_of_spec(spec, ~instructor_mode);
    save_exercise(exercise, ~instructor_mode);
    Util.set_localstore(cur_exercise_key, keystring);
    exercise;
  };

  let load_exercise = (key, spec, ~instructor_mode): SchoolExercise.state => {
    let keystring = keystring_of_key(key);
    switch (Util.get_localstore(keystring)) {
    | Some(data) =>
      let exercise =
        try(deserialize_exercise(data, ~spec, ~instructor_mode)) {
        | _ => init_exercise(spec, ~instructor_mode)
        };
      Util.set_localstore(cur_exercise_key, keystring);
      exercise;
    | None => init_exercise(spec, ~instructor_mode)
    };
  };

  let save = ((n, specs, exercise), ~instructor_mode) => {
    let key = key_of(List.nth(specs, n));
    let keystring = keystring_of_key(key);
    save_exercise(exercise, ~instructor_mode);
    Util.set_localstore(cur_exercise_key, keystring);
  };

  let init = (~instructor_mode) => {
    let school = School.init(~instructor_mode);
    save(school, ~instructor_mode);
    school;
  };

  let load = (~specs, ~instructor_mode): Editors.school => {
    switch (Util.get_localstore(cur_exercise_key)) {
    | Some(keystring) =>
      let key = key_of_keystring(keystring);
      switch (SchoolExercise.find_key_opt(key, specs)) {
      | Some((n, spec)) =>
        switch (Util.get_localstore(keystring)) {
        | Some(data) =>
          let exercise = deserialize_exercise(data, ~spec, ~instructor_mode);
          (n, specs, exercise);
        | None =>
          // initialize exercise from spec
          let exercise = SchoolExercise.state_of_spec(spec, ~instructor_mode);
          save_exercise(exercise, ~instructor_mode);
          (n, specs, exercise);
        }
      | None =>
        // invalid current exercise key saved, load the first exercise
        let first_spec = List.nth(specs, 0);
        let first_key = SchoolExercise.key_of(first_spec);
        (0, specs, load_exercise(first_key, first_spec, ~instructor_mode));
      };
    | None => init(~instructor_mode)
    };
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type school_export = {
    cur_exercise: SchoolExercise.key,
    exercise_data:
      list((SchoolExercise.key, SchoolExercise.persistent_state)),
  };

  let prep_school_export = (~specs, ~instructor_mode) => {
    {
      cur_exercise:
        key_of_keystring(Option.get(Util.get_localstore(cur_exercise_key))),
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

  let serialize_school_export = (~specs, ~instructor_mode) => {
    prep_school_export(~specs, ~instructor_mode)
    |> sexp_of_school_export
    |> Sexplib.Sexp.to_string;
  };

  let deserialize_school_export = data => {
    data |> Sexplib.Sexp.of_string |> school_export_of_sexp;
  };

  let export = (~specs, ~instructor_mode) => {
    serialize_school_export(~specs, ~instructor_mode);
  };

  let import = (data, ~specs, ~instructor_mode) => {
    let school_export = data |> deserialize_school_export;
    save_exercise_key(school_export.cur_exercise);
    school_export.exercise_data
    |> List.iter(((key, persistent_state)) => {
         let spec = SchoolExercise.find_key_opt(key, specs);
         switch (spec) {
         | None =>
           print_endline("Warning: saved key does not correspond to exercise")
         | Some((_, spec)) =>
           save_exercise(
             SchoolExercise.unpersist_state(
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
