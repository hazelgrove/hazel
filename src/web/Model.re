open Core;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type editor = {
  zipper: Zipper.t,
  history: ActionHistory.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type editor_model =
  | Simple(editor)
  | Study(int, list(editor))
  | School(int, list(editor));

[@deriving (show({with_path: false}), sexp, yojson)]
type simple = (Id.t, editor);

[@deriving (show({with_path: false}), sexp, yojson)]
type study = (Id.t, int, list(editor));

[@deriving (show({with_path: false}), sexp, yojson)]
type school = (Id.t, int, list(editor));

[@deriving (show({with_path: false}), yojson)]
type timestamp = float;

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | Simple
  | Study
  | School;

[@deriving (show({with_path: false}), sexp, yojson)]
type settings = {
  captions: bool,
  whitespace_icons: bool,
  statics: bool,
  dynamics: bool,
  context_inspector: bool,
  student: bool,
  mode,
};

let settings_init = {
  captions: true,
  whitespace_icons: false,
  statics: true,
  dynamics: true,
  context_inspector: false,
  student: true,
  mode: Simple,
};

type cell_index = int;
type mousedown = option(cell_index);

type t = {
  editor_model,
  id_gen: IdGen.state,
  font_metrics: FontMetrics.t,
  logo_font_metrics: FontMetrics.t,
  show_backpack_targets: bool,
  double_tap: option(timestamp),
  mousedown,
  settings,
};

let cutoff = (===);

let mk = editor_model => {
  id_gen: 1,
  editor_model,
  settings: settings_init,
  // TODO: move below to 'internals'?
  font_metrics: FontMetrics.init,
  logo_font_metrics: FontMetrics.init,
  show_backpack_targets: false,
  double_tap: None,
  mousedown: None,
};

let blank = mk(School(0, []));

let mk_editor = (zipper): editor => {zipper, history: ActionHistory.empty};

let get_editor' = (editor_model: editor_model): editor =>
  switch (editor_model) {
  | Simple(editor) => editor
  | Study(n, eds) =>
    assert(n < List.length(eds));
    List.nth(eds, n);
  | School(n, eds) =>
    assert(n < List.length(eds));
    List.nth(eds, n);
  };

let get_editor = (model: t): editor => get_editor'(model.editor_model);

let put_editor = (model: t, ed: editor): editor_model =>
  switch (model.editor_model) {
  | Simple(_) => Simple(ed)
  | Study(n, eds) =>
    assert(n < List.length(eds));
    Study(n, Util.ListUtil.put_nth(n, ed, eds));
  | School(n, eds) =>
    assert(n < List.length(eds));
    School(n, Util.ListUtil.put_nth(n, ed, eds));
  };

let get_zipper' = (editor_model: editor_model): Zipper.t =>
  get_editor'(editor_model).zipper;
let get_zipper = (model: t): Zipper.t => get_zipper'(model.editor_model);
let get_history = (model: t): ActionHistory.t =>
  get_editor'(model.editor_model).history;

let current_editor = (model: t): int =>
  switch (model.editor_model) {
  | Simple(_) => 0
  | Study(n, zs) =>
    assert(n < List.length(zs));
    n;
  | School(n, zs) =>
    assert(n < List.length(zs));
    n;
  };

let num_editors = (model: t): int =>
  switch (model.editor_model) {
  | Simple(_) => 1
  | Study(_, zs)
  | School(_, zs) => List.length(zs)
  };

let zipper_of_string =
    (id_gen: IdGen.state, str: string): option(Zipper.state) => {
  let insert_to_zid: (Zipper.state, string) => Zipper.state =
    (z_id, c) => {
      switch (Perform.go(Insert(c == "\n" ? Whitespace.linebreak : c), z_id)) {
      | Error(err) =>
        print_endline(
          "WARNING: zipper_of_string: insert: "
          ++ Perform.Action.Failure.show(err),
        );
        z_id;
      | Ok(r) => r
      };
    };
  try(
    str
    |> Util.StringUtil.to_list
    |> List.fold_left(insert_to_zid, (Zipper.init(0), id_gen))
    |> Option.some
  ) {
  | e =>
    print_endline(
      "WARNING: zipper_of_string: exception during parse: "
      ++ Printexc.to_string(e),
    );
    None;
  };
};

let simple_init: simple = (1, mk_editor(Zipper.init(0)));

let editors_of_strings = (xs: list(string)): (Id.t, int, list(editor)) => {
  let (id_gen, zs) =
    List.fold_left(
      ((acc_id, acc_zs), str) => {
        switch (zipper_of_string(acc_id, str)) {
        | None => (acc_id, acc_zs @ [Zipper.init(0)])
        | Some((z, new_id)) => (new_id, acc_zs @ [z])
        }
      },
      (0, []),
      xs,
    );
  (id_gen, 0, List.map(mk_editor, zs));
};
