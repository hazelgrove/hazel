open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type state = Editor.t;

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_state = PersistentZipper.t;

let scratch_key = "scratch";
let spliced_elabs = editor => {
  let seg = Editor.get_seg(editor);
  let (term, _) = MakeTerm.go(seg);
  let info_map = Statics.mk_map(term);
  [(scratch_key, Interface.elaborate(info_map, term))];
};

let persist = (editor: Editor.t) => {
  PersistentZipper.persist(editor.state.zipper);
};

let unpersist = (zipper: persistent_state) => {
  let zipper = PersistentZipper.unpersist(zipper);
  Editor.init(zipper, ~read_only=false);
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

let export = (state: state) => {
  state |> persist |> yojson_of_persistent_state;
};

let import = (data: string) => {
  data |> Yojson.Safe.from_string |> persistent_state_of_yojson |> unpersist;
};

let export_init = (state: state) => {
  state |> persist |> show_persistent_state;
};
