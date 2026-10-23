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

/* the settings modular-editors adds each get one row */
let own_rows_once = () => {
  let globals = {
    ...Globals.Model.init(),
    inject_global: _ => Virtual_dom.Vdom.Effect.Ignore,
  };
  let rows =
    SettingsPanel.sections(~globals, ~editors_inject=_ =>
      Virtual_dom.Vdom.Effect.Ignore
    )
    |> List.concat_map((s: NutMenu.section) => s.s_rows);
  let count = setting =>
    List.length(
      List.filter(
        (row: NutMenu.row) =>
          switch (row) {
          | Toggle(t) => t.setting == setting
          | _ => false
          },
        rows,
      ),
    );
  List.iter(
    ((name, setting)) => check(int, name, 1, count(setting)),
    [
      ("Program value", Settings.Update.TailProbe),
      ("Outline arrows", Settings.Update.OutlineArrows),
      ("Eval progress", Settings.Update.ShowPendingEval),
    ],
  );
};

let tests = (
  "SettingsPanel",
  [
    test_case("settings saved before the panel load", `Quick, old_blob),
    test_case(
      "modular-editors' settings get a row each",
      `Quick,
      own_rows_once,
    ),
  ],
);
