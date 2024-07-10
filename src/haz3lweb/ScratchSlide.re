open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type state = Editor.t;

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_state = PersistentZipper.t;

let scratch_key = n => "scratch_" ++ n;

let persist = (editor: Editor.t): persistent_state => {
  PersistentZipper.persist(editor.state.zipper);
};

let unpersist = (zipper: persistent_state, ~settings: CoreSettings.t): state => {
  let zipper = PersistentZipper.unpersist(zipper);
  Editor.init(zipper, ~read_only=false, ~settings);
};

let serialize = (state: state): string => {
  persist(state) |> sexp_of_persistent_state |> Sexplib.Sexp.to_string;
};

let deserialize = (data: string, ~settings: CoreSettings.t): state => {
  Sexplib.Sexp.of_string(data)
  |> persistent_state_of_sexp
  |> unpersist(~settings);
};

let deserialize_opt =
    (data: string, ~settings: CoreSettings.t): option(state) => {
  let sexp =
    try(Some(Sexplib.Sexp.of_string(data) |> persistent_state_of_sexp)) {
    | _ => None
    };
  sexp |> Option.map(sexp => sexp |> unpersist(~settings));
};

let export = (state: state): Yojson.Safe.t => {
  state |> persist |> yojson_of_persistent_state;
};

let import = (data: string, ~settings: CoreSettings.t): state => {
  data
  |> Yojson.Safe.from_string
  |> persistent_state_of_yojson
  |> unpersist(~settings);
};

let export_init = (state: state): string => {
  state |> persist |> show_persistent_state;
};

let mk_statics =
    (~settings: Settings.t, editor: Editor.t, ctx_init: Ctx.t)
    : CachedStatics.statics => {
  let term = MakeTerm.from_zip_for_sem(editor.state.zipper) |> fst;
  let info_map = Interface.Statics.mk_map_ctx(settings.core, ctx_init, term);
  let error_ids =
    Statics.Map.error_ids(editor.state.meta.projected.term_ranges, info_map);
  {term, info_map, error_ids};
};
