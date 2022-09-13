open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type state = (Id.t, Editor.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_state = (Id.t, Zipper.t);

let init = (code: CodeString.t) => {
  let init_id = 0;
  switch (Printer.zipper_of_string(init_id, code)) {
  | None => (0, Editor.init(Haz3lcore.Zipper.init(0), ~read_only=false))
  | Some((z, new_id)) => (new_id, Editor.init(z, ~read_only=false))
  };
};

let editor_of_state = ((_, editor): state) => editor;

let id_of_state = ((id, _): state) => id;

let put_editor_and_id = ((_, _): state, id, editor) => (id, editor);

let scratch_key = "scratch";
let spliced_elabs = ((_, editor)) => {
  let seg = Editor.get_seg(editor);
  let (term, _) = MakeTerm.go(seg);
  let info_map = Statics.mk_map(term);
  [(scratch_key, Interface.elaborate(info_map, term))];
};

let persist = ((id, editor: Editor.t)) => {
  (id, editor.state.zipper);
};

let unpersist = ((id, zipper): persistent_state) => {
  (id, Editor.init(zipper));
};
