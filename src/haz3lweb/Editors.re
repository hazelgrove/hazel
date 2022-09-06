open Sexplib.Std;
open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Simple(Editor.t)
  | Study(int, list(Editor.t))
  | School(int, list(Editor.t));

[@deriving (show({with_path: false}), sexp, yojson)]
type simple = (Id.t, Editor.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type study = (Id.t, int, list(Editor.t));

[@deriving (show({with_path: false}), sexp, yojson)]
type school = (Id.t, int, list(Editor.t));

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | Simple
  | Study
  | School;

let simple_result_key = "simple";
let study_result_key = "study";
let school_result_key = "school";

let get_editor = (editors: t): Editor.t =>
  switch (editors) {
  | Simple(editor) => editor
  | Study(n, eds) =>
    assert(n < List.length(eds));
    List.nth(eds, n);
  | School(n, eds) =>
    assert(n < List.length(eds));
    List.nth(eds, n);
  };

let put_editor = (ed: Editor.t, eds: t): t =>
  switch (eds) {
  | Simple(_) => Simple(ed)
  | Study(n, eds) =>
    assert(n < List.length(eds));
    Study(n, Util.ListUtil.put_nth(n, ed, eds));
  | School(n, eds) =>
    assert(n < List.length(eds));
    School(n, Util.ListUtil.put_nth(n, ed, eds));
  };

let get_zipper = (editors: t): Zipper.t => get_editor(editors).state.zipper;

let get_result_key: t => ModelResults.key =
  fun
  | Simple(_) => simple_result_key
  | Study(_) => study_result_key
  | School(_) => school_result_key;

let get_spliced_segs = (editors: t): list((ModelResults.key, Segment.t)) => {
  let result_key = get_result_key(editors);
  switch (editors) {
  | Simple(ed) => [(result_key, Editor.get_seg(ed))]
  | Study(n, eds) => [(result_key, Editor.get_seg(List.nth(eds, n)))]
  | School(_, eds) =>
    // TODO(cyrus) replace placeholder
    [(result_key, Editor.get_seg(List.hd(eds)))]
  };
};

let get_spliced_elabs = eds =>
  eds
  |> get_spliced_segs
  |> List.map(((key, seg)) => {
       let (term, _) = MakeTerm.go(seg);
       let map = Statics.mk_map(term);
       (key, Interface.elaborate(map, term));
     });
