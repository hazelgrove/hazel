open Util;

// A generic key-value store for saving/loading data to/from local storage

type key =
  | Settings
  | ExplainThis
  | Assistant
  | Mode
  | Scratch
  | Documentation
  | Tutorial(Haz3lcore.Id.t)
  | CurrentTutorial
  | CurrentExercise
  | Exercise(Haz3lcore.Id.t)
  | CurrentDerivation
  | Derivation(DerivationTree.key);

let key_to_string =
  fun
  | Settings => "SETTINGS"
  | ExplainThis => "ExplainThisModel"
  | Assistant => "AssistantModel"
  | Mode => "MODE"
  | Scratch => "SAVE_SCRATCH"
  | Documentation => "SAVE_DOCUMENTATION"
  | Tutorial(id) => Haz3lcore.Id.to_string(id)
  | CurrentTutorial => "CUR_TUTORIAL"
  | CurrentExercise => "CUR_EXERCISE"
  | CurrentDerivation => "CUR_DERIVATION"
  | Derivation(key) =>
    "DERIVATIONS"
    ++ (key |> DerivationTree.sexp_of_key |> Sexplib.Sexp.to_string)
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

  let deserialize = (data: string, default: unit => t) =>
    try(data |> Sexplib.Sexp.of_string |> t_of_sexp) {
    | _ =>
      print_endline("Could not deserialize " ++ key_to_string(key) ++ ".");
      default();
    };

  let save = (data: t): unit =>
    JsUtil.set_localstore(key_to_string(key), serialize(data));

  let init = () => {
    JsUtil.set_localstore(key_to_string(key), serialize(default()));
    default();
  };

  let load = (): t =>
    switch (JsUtil.get_localstore(key_to_string(key))) {
    | None => init()
    | Some(data) => deserialize(data, default)
    };

  let rec export = () =>
    switch (JsUtil.get_localstore(key_to_string(key))) {
    | None =>
      let _ = init();
      export();
    | Some(data) => data
    };

  let import = data => {
    let data = deserialize(data, default);
    save(data);
  };
};
