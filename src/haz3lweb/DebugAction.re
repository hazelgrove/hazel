[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | TurnOffDynamics
  | ClearStore;

let perform = action => {
  switch (action) {
  | TurnOffDynamics =>
    let settings = Store.Settings.load();
    Store.Settings.save({
      ...settings,
      core: {
        ...settings.core,
        dynamics: false,
      },
    });
  | ClearStore => JsUtil.clear_localstore()
  };
};
