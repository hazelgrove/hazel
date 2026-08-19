open Alcotest;
open Web;

/* Export/import is the only path by which a user can lose all of their work at
 * once, and it had no coverage at all.
 *
 * The risk is not a crash -- it is `import_all`'s fallback:
 *
 *   try(data |> all_of_yojson) {
 *   | _ => let all_public = data |> all_public_of_yojson;
 *          {..., documentation: "", explainThisModel: "", ...}
 *
 * That fallback exists for saves predating the lang-doc release, and it is right
 * for those. But it catches EVERY parse failure, so if the current schema ever
 * drifts -- a renamed or retyped field -- a current export stops parsing as
 * `all`, silently takes the legacy path, and the user's documentation slides and
 * ExplainThis state are replaced with "". No error, no warning, and the next
 * export writes the loss back to disk.
 *
 * So the test that matters is not "does a round trip work" but "does a CURRENT
 * export take the full path rather than the fallback". */

let core = Settings.Model.init.core;

/* `export_all` yields a Yojson value; `import_all` takes the serialized string,
   so the round trip goes through `to_string` exactly as the app's download and
   upload do. */
let export = (~log="", ()) =>
  Export.export_all(~settings=core, ~instructor_mode=false, ~log)
  |> Yojson.Safe.to_string;

let import = data =>
  Export.import_all(
    ~import_log=_ => (),
    data,
    ~exercise_specs=ExerciseSettings.exercises,
    ~tutorial_specs=TutorialSettings.lessons,
  );

/* Read a top-level string field out of an exported payload. */
let field = (name, data) =>
  switch (Yojson.Safe.from_string(data)) {
  | `Assoc(pairs) =>
    switch (List.assoc_opt(name, pairs)) {
    | Some(`String(s)) => Some(s)
    | _ => None
    }
  | _ => None
  };

/* The legacy shape the fallback is for: the current payload minus the two
   fields that postdate it. */
let strip_to_legacy = data =>
  switch (Yojson.Safe.from_string(data)) {
  | `Assoc(pairs) =>
    `Assoc(
      List.filter(
        ((k, _)) => k != "documentation" && k != "explainThisModel",
        pairs,
      ),
    )
    |> Yojson.Safe.to_string
  | _ => failwith("export was not a JSON object")
  };

let tests = (
  "Export",
  [
    /* Fixture guard: the assertions below are only meaningful if a fresh export
       actually carries documentation. */
    test_case("an export carries documentation", `Quick, () =>
      check(
        bool,
        "documentation is non-empty",
        true,
        switch (field("documentation", export())) {
        | Some(s) => s != ""
        | None => false
        },
      )
    ),
    /* THE ONE THAT MATTERS. If the schema drifts, this fails instead of the app
       quietly discarding documentation and ExplainThis state. */
    test_case(
      "a current export imports without hitting the legacy fallback",
      `Quick,
      () => {
        let before = export();
        import(before);
        let after = export();
        check(
          bool,
          "documentation survived the round trip",
          true,
          switch (field("documentation", after)) {
          | Some(s) => s != ""
          | None => false
          },
        );
      },
    ),
    /* Round trip: importing an export and re-exporting must reproduce it. A
       field that is exported but not imported shows up here and nowhere else. */
    test_case(
      "export is a fixed point of import",
      `Quick,
      () => {
        let first = export(~log="LOG", ());
        import(first);
        let second = export(~log="LOG", ());
        check(string, "re-export matches", first, second);
      },
    ),
    /* The fallback still has to work for the saves it exists for: legacy data
       imports, and losing documentation there is the accepted cost. */
    test_case(
      "a legacy export still imports",
      `Quick,
      () => {
        let legacy = strip_to_legacy(export());
        switch (import(legacy)) {
        | () => check(bool, "imported without raising", true, true)
        | exception exn =>
          failf("legacy import raised: %s", Printexc.to_string(exn))
        };
      },
    ),
    /* The round trip above is weaker than it looks: import writes to the same
       stores export reads, so in a fresh session a field that is exported and
       never imported still round-trips -- the store simply keeps the value that
       was exported. (Verified: deleting an import line does not fail it.)
       To make "import actually restores" observable, the state has to differ
       between export and import. */
    test_case(
      "import restores state that has since changed",
      `Quick,
      () => {
        let original = Settings.Store.load();
        let snapshot = export();
        /* Move the state away from the snapshot, then import it back. */
        Settings.Store.save({
          ...original,
          captions: !original.captions,
        });
        check(
          bool,
          "the state really changed",
          false,
          Settings.Store.load().captions == original.captions,
        );
        import(snapshot);
        check(
          bool,
          "import put the snapshot's value back",
          true,
          Settings.Store.load().captions == original.captions,
        );
      },
    ),
  ],
);
