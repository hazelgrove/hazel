module Sexp = Sexplib.Sexp;

module Model = AssistantModel;

module Update = AssistantUpdate;

module F =
  Store.F({
    [@deriving (show({with_path: false}), yojson, sexp)]
    type t = Model.t;
    let default = () => {
      Update.init;
    };
    let key = Store.Assistant;
  });
