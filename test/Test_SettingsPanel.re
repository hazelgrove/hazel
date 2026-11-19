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

/* settings saved while drag refactoring was a toggle still load, keeping
   the rest of what was saved */
let removed_drag_toggle = () => {
  let saved = {
    ...Settings.Model.init,
    core: {
      ...Settings.Model.init.core,
      flip_animations: false,
    },
  };
  let rec add = (s: Sexplib.Sexp.t): Sexplib.Sexp.t =>
    switch (s) {
    | List([Atom("core"), List(fields)]) =>
      List([
        Atom("core"),
        List([List([Atom("drag_refactor"), Atom("true")]), ...fields]),
      ])
    | List(items) => List(List.map(add, items))
    | atom => atom
    };
  let loaded =
    Settings.Model.persistent_of_sexp(
      add(Settings.Model.sexp_of_persistent(saved)),
    );
  check(bool, "flip_animations kept", false, loaded.core.flip_animations);
};

let tests = (
  "SettingsPanel",
  [
    test_case("settings saved before the panel load", `Quick, old_blob),
    test_case(
      "settings saved with the drag toggle load",
      `Quick,
      removed_drag_toggle,
    ),
  ],
);
