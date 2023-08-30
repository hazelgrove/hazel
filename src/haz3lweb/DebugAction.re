[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | TurnOffDynamics
  | ClearStore;

let perform = action => {
  switch (action) {
  | TurnOffDynamics =>
    let settings = Store.Settings.load();
    Store.Settings.save({...settings, dynamics: false});
  | ClearStore => JsUtil.clear_localstore()
  };
};
