open Util;

module F =
  Store.F({
    [@deriving (show({with_path: false}), yojson, sexp)]
    type t = AssistantModel.t;
    let default = () => {
      AssistantUpdate.init;
    };
    let key = Store.Assistant;
  });
