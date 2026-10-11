open Alcotest;
open Web;

/* settings saved before the settings panel existed lack its fold state:
   they still load, with Stepper and Developer folded */
let old_blob = () => {
  let rec drop = (s: Sexplib.Sexp.t): Sexplib.Sexp.t =>
    switch (s) {
    | List(items) =>
      List(
        List.filter_map(
          (item: Sexplib.Sexp.t) =>
            switch (item) {
            | List([Atom("settings_folded"), ..._]) => None
            | other => Some(drop(other))
            },
          items,
        ),
      )
    | atom => atom
    };
  let old = drop(Settings.Model.sexp_of_persistent(Settings.Model.init));
  let text = Sexplib.Sexp.to_string(old);
  let rec has = (i, needle) =>
    i
    + String.length(needle) <= String.length(text)
    && (
      String.sub(text, i, String.length(needle)) == needle
      || has(i + 1, needle)
    );
  check(bool, "no fold state saved", false, has(0, "settings_folded"));
  let loaded = Settings.Model.persistent_of_sexp(old);
  check(
    list(string),
    "Stepper and Developer start folded",
    ["Stepper", "Developer"],
    loaded.sidebar.settings_folded,
  );
};

let types_toggle_carries_evaluation = () => {
  let toggle = settings =>
    Settings.Update.update(~action=Statics, ~settings).model;
  let off = toggle(Settings.Model.init);
  check(bool, "Types off turns evaluation off", false, off.core.dynamics);
  check(bool, "Types on turns it back on", true, toggle(off).core.dynamics);
};

let tests = (
  "SettingsPanel",
  [
    test_case("settings saved before the panel load", `Quick, old_blob),
    test_case(
      "Types off and on again leaves evaluation on",
      `Quick,
      types_toggle_carries_evaluation,
    ),
  ],
);
