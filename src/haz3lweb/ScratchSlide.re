open Haz3lcore;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type editor_selection =
  | MainEditor
  | Evaluation
  | Stepper(int);

[@deriving (show({with_path: false}), sexp, yojson)]
type state = (editor_selection, Editor.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_state = PersistentZipper.t;

let scratch_key = n => "scratch_" ++ n;

let persist = ((_, editor): state) => {
  PersistentZipper.persist(editor.state.zipper);
};

let unpersist = (zipper: persistent_state) => {
  let zipper = PersistentZipper.unpersist(zipper);
  (MainEditor, Editor.init(zipper, ~read_only=false));
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

let mk_statics =
    (~settings: Settings.t, editor: Editor.t, ctx_init: Ctx.t)
    : CachedStatics.statics => {
  let term = MakeTerm.from_zip_for_sem(editor.state.zipper) |> fst;
  let info_map = Interface.Statics.mk_map_ctx(settings.core, ctx_init, term);
  let error_ids =
    Statics.Map.error_ids(editor.state.meta.term_ranges, info_map);
  {term, info_map, error_ids};
};
