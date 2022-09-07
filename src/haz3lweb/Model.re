open Haz3lcore;
open Sexplib.Std;

[@deriving (show({with_path: false}), yojson)]
type timestamp = float;

[@deriving (show({with_path: false}), sexp, yojson)]
type settings = {
  captions: bool,
  whitespace_icons: bool,
  statics: bool,
  dynamics: bool,
  async_evaluation: bool,
  context_inspector: bool,
  student: bool,
  mode: Editors.mode,
};

let settings_init = {
  captions: true,
  whitespace_icons: false,
  statics: true,
  dynamics: true,
  async_evaluation: false,
  context_inspector: false,
  student: true,
  mode: Simple,
};

type t = {
  editors: Editors.t,
  id_gen: IdGen.state,
  results: ModelResults.t,
  settings,
  font_metrics: FontMetrics.t,
  logo_font_metrics: FontMetrics.t,
  show_backpack_targets: bool,
  double_tap: option(timestamp),
  clipboard: string,
  mousedown: bool,
};

let cutoff = (===);

let mk = editors => {
  id_gen: 1,
  editors,
  results: ModelResults.empty,
  settings: settings_init,
  // TODO: move below to 'io_state'?
  font_metrics: FontMetrics.init,
  logo_font_metrics: FontMetrics.init,
  show_backpack_targets: false,
  double_tap: None,
  clipboard: ",",
  mousedown: false,
};

let blank = mk(Editors.Study(0, []));

let get_editor = (model: t): Editor.t => Editors.get_editor(model.editors);

let put_editor = (model: t, ed: Editor.t): Editors.t =>
  Editors.put_editor(ed, model.editors);

let get_zipper = (model: t): Zipper.t => Editors.get_zipper(model.editors);

let get_history = (model: t): Editor.History.t =>
  Editors.get_editor(model.editors).history;
let get_touched = (model: t): Touched.t =>
  Editors.get_editor(model.editors).state.meta.touched;

let current_editor = (model: t): int =>
  switch (model.editors) {
  | Simple(_) => 0
  | Study(n, zs) =>
    assert(n < List.length(zs));
    n;
  | School(_) => 0
  };

let num_editors = (model: t): int =>
  switch (model.editors) {
  | Simple(_) => 1
  | Study(_, eds) => List.length(eds)
  | School(_) => 1
  };

let simple_init: Editors.simple = (1, Editor.empty(0));

// let editors_of_strings = (xs: list(string)): (Id.t, int, list(Editor.t)) => {
//   let (id_gen, zs) =
//     List.fold_left(
//       ((acc_id, acc_zs), str) => {
//         switch (Printer.zipper_of_string(acc_id, str)) {
//         | None => (acc_id, acc_zs @ [Zipper.init(0)])
//         | Some((z, new_id)) => (new_id, acc_zs @ [z])
//         }
//       },
//       (0, []),
//       xs,
//     );
//   (id_gen, 0, List.map(Editor.init, zs));
// };
