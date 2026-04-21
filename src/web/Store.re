// A generic key-value store backed by IndexedDB (via HazelDB cache)

/* Legacy localStorage read — for one-time migration of pre-existing
   data to IndexedDB. Safe to remove once all users have upgraded. */
let legacy_get = (k: string): option(string) =>
  try({
    let local_store =
      Js_of_ocaml.Dom_html.window##.localStorage
      |> Js_of_ocaml.Js.Optdef.get(_, () => assert(false));
    local_store##getItem(Js_of_ocaml.Js.string(k))
    |> (
      x =>
        Js_of_ocaml.Js.Opt.get(x, () => assert(false))
        |> Js_of_ocaml.Js.to_string
        |> Option.some
    );
  }) {
  | _ => None
  };

type key =
  | Settings
  | ExplainThis
  | Mode
  | Tutorial(Haz3lcore.Id.t)
  | CurrentTutorial
  | CurrentExercise
  | Exercise(Haz3lcore.Id.t);

let key_to_string =
  fun
  | Settings => "SETTINGS"
  | ExplainThis => "ExplainThisModel"
  | Mode => "MODE"
  | Tutorial(id) => Haz3lcore.Id.to_string(id)
  | CurrentTutorial => "CUR_TUTORIAL"
  | CurrentExercise => "CUR_EXERCISE"
  | Exercise(id) => "TUTORIAL" ++ Haz3lcore.Id.to_string(id);

module F =
       (
         STORE_KIND: {
           [@deriving (show({with_path: false}), sexp, yojson)]
           type t;
           let default: unit => t;
           let key: key;
         },
       ) => {
  include STORE_KIND;

  let key_string = key_to_string(key);

  let serialize = (data: t) => {
    data |> sexp_of_t |> Sexplib.Sexp.to_string;
  };

  let deserialize = (data: string, default: t) =>
    try(data |> Sexplib.Sexp.of_string |> t_of_sexp) {
    | _ =>
      print_endline("Could not deserialize " ++ key_to_string(key) ++ ".");
      default;
    };

  let save = (data: t): unit => {
    let serialized = serialize(data);
    HazelDB.kv_save(key_string, serialized);
  };

  let reset = () => {
    save(default());
    default();
  };

  /* Load from IndexedDB cache, falling back to legacy localStorage
     for migration of pre-existing data. */
  let load = (): t =>
    switch (HazelDB.kv_get(key_string)) {
    | Some(data) => deserialize(data, default())
    | None =>
      switch (legacy_get(key_string)) {
      | None => default()
      | Some(data) => deserialize(data, default())
      }
    };

  let export = () =>
    switch (HazelDB.kv_get(key_string)) {
    | Some(data) => data
    | None =>
      switch (legacy_get(key_string)) {
      | None => serialize(default())
      | Some(data) => data
      }
    };

  let import = data => {
    let data = deserialize(data, default());
    save(data);
  };
};
