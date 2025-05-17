module Sexp = Sexplib.Sexp;

module Model = AssistantModel;

module F =
  Store.F({
    [@deriving (show({with_path: false}), yojson, sexp)]
    type t = Model.t;
    let default = () => {
      Model.init;
    };
    let key = Store.Assistant;
  });
