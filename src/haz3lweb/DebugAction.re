[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | TurnOffDynamics
  | ClearLocalStorage;

let perform = action => {
  switch (action) {
  | TurnOffDynamics =>
    let settings = LocalStorage.Settings.load();
    LocalStorage.Settings.save({
      ...settings,
      dynamics: {
        ...settings.dynamics,
        evaluate: false,
      },
    });
  | ClearLocalStorage => JsUtil.clear_localstore()
  };
};
