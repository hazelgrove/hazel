open Sexplib.Std;
open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Simple(Editor.t)
  | Study(int, list(Editor.t))
  | School(SchoolExercise.state);

[@deriving (show({with_path: false}), sexp, yojson)]
type simple = (Id.t, Editor.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type study = (Id.t, int, list(Editor.t));

[@deriving (show({with_path: false}), sexp, yojson)]
type school = (Id.t, SchoolExercise.state);

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | Simple
  | Study
  | School;

let single_result_key = "single_result";

let get_editor = (editors: t): Editor.t =>
  switch (editors) {
  | Simple(editor) => editor
  | Study(n, eds) =>
    assert(n < List.length(eds));
    List.nth(eds, n);
  | School(state) => SchoolExercise.editor_of_state(state)
  };

let put_editor = (ed: Editor.t, eds: t): t =>
  switch (eds) {
  | Simple(_) => Simple(ed)
  | Study(n, eds) =>
    assert(n < List.length(eds));
    Study(n, Util.ListUtil.put_nth(n, ed, eds));
  | School(state) => School(SchoolExercise.put_editor(state, ed))
  };

let get_zipper = (editors: t): Zipper.t => get_editor(editors).state.zipper;

let get_spliced_elabs = (editors: t): list((ModelResults.key, DHExp.t)) => {
  switch (editors) {
  | Simple(ed) =>
    let seg = Editor.get_seg(ed);
    let (term, _) = MakeTerm.go(seg);
    let info_map = Statics.mk_map(term);
    [(single_result_key, Interface.elaborate(info_map, term))];
  | Study(n, eds) =>
    let seg = Editor.get_seg(List.nth(eds, n));
    let (term, _) = MakeTerm.go(seg);
    let info_map = Statics.mk_map(term);
    [(single_result_key, Interface.elaborate(info_map, term))];
  | School(state) => SchoolExercise.spliced_elabs(state)
  };
};
