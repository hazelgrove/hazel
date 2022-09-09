open Sexplib.Std;
open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Scratch(int, list(Editor.t))
  | School(SchoolExercise.state);

[@deriving (show({with_path: false}), sexp, yojson)]
type scratch = (Id.t, int, list(Editor.t));

[@deriving (show({with_path: false}), sexp, yojson)]
type school = (Id.t, SchoolExercise.state);

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | Scratch
  | School;

let rotate_mode = (editors: t) =>
  switch (editors) {
  | Scratch(_) => School
  | School(_) => Scratch
  };

let single_result_key = "single_result";

let get_editor = (editors: t): Editor.t =>
  switch (editors) {
  | Scratch(n, eds) =>
    assert(n < List.length(eds));
    List.nth(eds, n);
  | School(state) => SchoolExercise.editor_of_state(state)
  };

let put_editor = (ed: Editor.t, eds: t): t =>
  switch (eds) {
  | Scratch(n, eds) =>
    assert(n < List.length(eds));
    Scratch(n, Util.ListUtil.put_nth(n, ed, eds));
  | School(state) => School(SchoolExercise.put_editor(state, ed))
  };

let get_zipper = (editors: t): Zipper.t => get_editor(editors).state.zipper;

let get_spliced_elabs = (editors: t): list((ModelResults.key, DHExp.t)) => {
  switch (editors) {
  | Scratch(n, eds) =>
    let seg = Editor.get_seg(List.nth(eds, n));
    let (term, _) = MakeTerm.go(seg);
    let info_map = Statics.mk_map(term);
    [(single_result_key, Interface.elaborate(info_map, term))];
  | School(state) => SchoolExercise.spliced_elabs(state)
  };
};

let set_instructor_mode = (editors: t, instructor_mode: bool): t =>
  switch (editors) {
  | Scratch(_) => editors
  | School(state) =>
    School(SchoolExercise.set_instructor_mode(state, instructor_mode))
  };

let num_slides = (editors: t): int =>
  switch (editors) {
  | Scratch(_, slides) => List.length(slides)
  | School(_) => 1
  };

let cur_slide = (editors: t): int =>
  switch (editors) {
  | Scratch(n, _) => n
  | School(_) => 0
  };
