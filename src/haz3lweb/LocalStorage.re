open Js_of_ocaml;
open Sexplib.Std;

let save_settings_key: string = "SETTINGS";
let save_scratch_key: string = "SAVE_SCRATCH";
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

[@deriving (show({with_path: false}), sexp, yojson)]
type school_without_history = (int, list(SchoolExercise.persistent_state));

let prep_school_in =
    ((n, exercises): Editors.school, ~instructor_mode: bool)
    : school_without_history => (
  n,
  exercises
  |> List.map(SchoolExercise.persistent_state_of_state(~instructor_mode)),
);

let prep_school_out =
    (
      (n, persistent_exercises): school_without_history,
      ~instructor_mode: bool,
    )
    : Editors.school => (
  n,
  List.combine(persistent_exercises, School.exercises)
  |> List.map(((state, spec)) => {
       SchoolExercise.unpersist_state(state, spec, ~instructor_mode)
     }),
);

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

let load_scratch_without_history = (): scratch_without_history =>
  switch (get_localstore(save_scratch_key)) {
  | None => prep_scratch_in(Scratch.init())
  | Some(flag) =>
    try(flag |> Sexplib.Sexp.of_string |> scratch_without_history_of_sexp) {
    | _ => prep_scratch_in(Scratch.init())
    }
  };

let load_scratch = (): Editors.scratch =>
  load_scratch_without_history() |> prep_scratch_out;

let save_school = (school: Editors.school, ~instructor_mode: bool): unit => {
  let value =
    school
    |> prep_school_in(~instructor_mode)
    |> sexp_of_school_without_history
    |> Sexplib.Sexp.to_string;
  set_localstore(save_school_key, value);
};

let load_school_without_history =
    (~instructor_mode: bool): school_without_history =>
  switch (get_localstore(save_school_key)) {
  | None => prep_school_in(School.init(~instructor_mode), ~instructor_mode)
  | Some(flag) =>
    try(flag |> Sexplib.Sexp.of_string |> school_without_history_of_sexp) {
    | _ => prep_school_in(School.init(~instructor_mode), ~instructor_mode)
    }
  };

let load_school = (~instructor_mode: bool): Editors.school =>
  load_school_without_history(~instructor_mode)
  |> prep_school_out(~instructor_mode);
