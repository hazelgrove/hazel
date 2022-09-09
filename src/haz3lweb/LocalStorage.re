open Js_of_ocaml;
open Haz3lcore;
open Sexplib.Std;

let save_settings_key: string = "SETTINGS";
let save_simple_key: string = "SAVE_SIMPLE";
let save_study_key: string = "SAVE_STUDY";
let save_school_key: string = "SAVE_SCHOOL";
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
    try(flag |> Sexplib.Sexp.of_string |> Model.settings_of_sexp) {
    | _ => Model.settings_init
    }
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type scratch_without_history = (Id.t, int, list(Zipper.t));

[@deriving (show({with_path: false}), sexp, yojson)]
type school_without_history = (Id.t, SchoolExercise.persistent_state);

let prep_school_in =
    ((id_gen, state): Editors.school): school_without_history => (
  id_gen,
  SchoolExercise.persistent_state_of_state(state),
);

let prep_school_out =
    (
      (id_gen, persistent_state): school_without_history,
      ~instructor_mode: bool,
    )
    : Editors.school => (
  id_gen,
  SchoolExercise.unpersist_state(
    persistent_state,
    School.the_exercise,
    ~instructor_mode,
  ),
);

let prep_scratch_in =
    ((id_gen, idx, eds): Editors.scratch): scratch_without_history => (
  id_gen,
  idx,
  List.map((ed: Editor.t) => ed.state.zipper, eds),
);

let prep_scratch_out =
    ((id_gen, idx, zs): scratch_without_history): Editors.scratch => (
  id_gen,
  idx,
  List.map(Editor.init(~read_only=false), zs),
);

let save_scratch = (study: Editors.scratch): unit =>
  set_localstore(
    save_study_key,
    study
    |> prep_scratch_in
    |> sexp_of_scratch_without_history
    |> Sexplib.Sexp.to_string,
  );

let load_scratch = (): Editors.scratch =>
  switch (get_localstore(save_study_key)) {
  | None => Scratch.init
  | Some(flag) =>
    try(
      flag
      |> Sexplib.Sexp.of_string
      |> scratch_without_history_of_sexp
      |> prep_scratch_out
    ) {
    | _ => Scratch.init
    }
  };

let save_school = (school: Editors.school): unit =>
  set_localstore(
    save_school_key,
    school
    |> prep_school_in
    |> sexp_of_school_without_history
    |> Sexplib.Sexp.to_string,
  );

let load_school = (~instructor_mode: bool): Editors.school =>
  switch (get_localstore(save_school_key)) {
  | None => School.init(~instructor_mode)
  | Some(flag) =>
    try(
      flag
      |> Sexplib.Sexp.of_string
      |> school_without_history_of_sexp
      |> prep_school_out(~instructor_mode)
    ) {
    | _ => School.init(~instructor_mode)
    }
  };
