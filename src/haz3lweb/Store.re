module type Storable = {
  type t;
  let key: string;
  let default: t;
  let serialize: t => string;
  let deserialize: string => t;
};

module General = (M: Storable) => {
  let init = () => {
    JsUtil.set_localstore(M.key, M.serialize(M.default));
    M.default;
  };
  let save = (m: M.t): unit => JsUtil.set_localstore(M.key, M.serialize(m));

  let deserialize' = (s: string): M.t =>
    try(M.deserialize(s)) {
    | _ =>
      print_endline("WARNING: Could not deserialize " ++ M.key);
      M.default;
    };

  let load = (): M.t =>
    switch (JsUtil.get_localstore(M.key)) {
    | None => init()
    | Some(s) => deserialize'(s)
    };

  let import = (s: string): M.t => {
    let m = deserialize'(s);
    save(m);
    m;
  };

  let rec export = (): string =>
    switch (JsUtil.get_localstore(M.key)) {
    | None =>
      ignore(init());
      export();
    | Some(data) => data
    };
};

module Settings = General(ModelSettings);
module LangDocMessages = General(LangDocMessages);
module Editors = General(Editors);
