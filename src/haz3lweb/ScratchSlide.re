open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type state = (Id.t, Editor.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_state = (Id.t, Zipper.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_state_backup = CodeString.t;

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
  (id, Editor.init(zipper, ~read_only=false));
};

let serialize = (state: state) => {
  persist(state) |> sexp_of_persistent_state |> Sexplib.Sexp.to_string;
};

let deserialize = (data: string) => {
  Sexplib.Sexp.of_string(data) |> persistent_state_of_sexp |> unpersist;
};

let deserialize_opt = (data: string) => {
  let sexp =
    try(Some(Sexplib.Sexp.of_string(data) |> persistent_state_of_sexp)) {
    | _ => None
    };
  sexp |> Option.map(sexp => sexp |> unpersist);
};

let persist_backup = (state: state) => {
  let (_, zipper) = persist(state);
  Printer.to_string_basic(zipper);
};

let unpersist_backup = (data: persistent_state_backup) => {
  let init_id = 0;
  switch (Printer.zipper_of_string(init_id, data)) {
  | None => (0, Editor.init(Haz3lcore.Zipper.init(0), ~read_only=false))
  | Some((z, new_id)) => (new_id, Editor.init(z, ~read_only=false))
  };
};

let serialize_backup = (state: state) => {
  persist_backup(state);
};

let deserialize_backup = (data: string) => {
  unpersist_backup(data);
};
