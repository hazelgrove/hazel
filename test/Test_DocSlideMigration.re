open Alcotest;
open Haz3lcore;

/* All doc slides from src/web/init/docs/ */
let web_docs_slides: list((string, string, PersistentSegment.t)) = [
  (
    "BasicReference.ml",
    fst(Web.BasicReference.out),
    snd(Web.BasicReference.out),
  ),
  ("Projectors.ml", fst(Web.Projectors.out), snd(Web.Projectors.out)),
  ("ADTs.ml", fst(Web.ADTs.out), snd(Web.ADTs.out)),
  ("Tuples.ml", fst(Web.Tuples.out), snd(Web.Tuples.out)),
  ("Tables.ml", fst(Web.Tables.out), snd(Web.Tables.out)),
  ("Polymorphism.ml", fst(Web.Polymorphism.out), snd(Web.Polymorphism.out)),
  ("Cards.ml", fst(Web.Cards.out), snd(Web.Cards.out)),
  ("Probes.ml", fst(Web.Probes.out), snd(Web.Probes.out)),
  ("Livelits.ml", fst(Web.Livelits.out), snd(Web.Livelits.out)),
];

/* All B2T2 slides from src/b2t2/slides/ (excluding Datasheet which is generated) */
let b2t2_slides: list((string, string, string, PersistentSegment.t)) = [
  /* base path, filename, title, content */
  (
    "src/b2t2/slides/",
    "B2T2ExampleTables.ml",
    fst(B2t2.B2T2ExampleTables.out),
    snd(B2t2.B2T2ExampleTables.out),
  ),
  (
    "src/b2t2/slides/table_api/",
    "B2T2TableAPIConstructorsemptyTable.ml",
    fst(B2t2.B2T2TableAPIConstructorsemptyTable.out),
    snd(B2t2.B2T2TableAPIConstructorsemptyTable.out),
  ),
  (
    "src/b2t2/slides/table_api/",
    "B2T2TableAPIConstructorsaddRows.ml",
    fst(B2t2.B2T2TableAPIConstructorsaddRows.out),
    snd(B2t2.B2T2TableAPIConstructorsaddRows.out),
  ),
  (
    "src/b2t2/slides/table_api/",
    "B2T2TableAPIConstructorsaddColumn.ml",
    fst(B2t2.B2T2TableAPIConstructorsaddColumn.out),
    snd(B2t2.B2T2TableAPIConstructorsaddColumn.out),
  ),
  (
    "src/b2t2/slides/table_api/",
    "B2T2TableAPIConstructorsbuildColumn.ml",
    fst(B2t2.B2T2TableAPIConstructorsbuildColumn.out),
    snd(B2t2.B2T2TableAPIConstructorsbuildColumn.out),
  ),
  (
    "src/b2t2/slides/table_api/",
    "B2T2TableAPIConstructorsvcat.ml",
    fst(B2t2.B2T2TableAPIConstructorsvcat.out),
    snd(B2t2.B2T2TableAPIConstructorsvcat.out),
  ),
  (
    "src/b2t2/slides/table_api/",
    "B2T2TableAPIConstructorshcat.ml",
    fst(B2t2.B2T2TableAPIConstructorshcat.out),
    snd(B2t2.B2T2TableAPIConstructorshcat.out),
  ),
  (
    "src/b2t2/slides/table_api/",
    "B2T2TableAPIConstructorsvalues.ml",
    fst(B2t2.B2T2TableAPIConstructorsvalues.out),
    snd(B2t2.B2T2TableAPIConstructorsvalues.out),
  ),
  (
    "src/b2t2/slides/table_api/",
    "B2T2TableAPIConstructorscrossJoin.ml",
    fst(B2t2.B2T2TableAPIConstructorscrossJoin.out),
    snd(B2t2.B2T2TableAPIConstructorscrossJoin.out),
  ),
  (
    "src/b2t2/slides/table_api/",
    "B2T2TableAPIConstructorsleftJoin.ml",
    fst(B2t2.B2T2TableAPIConstructorsleftJoin.out),
    snd(B2t2.B2T2TableAPIConstructorsleftJoin.out),
  ),
  (
    "src/b2t2/slides/table_api/",
    "B2T2TableAPIProperties.ml",
    fst(B2t2.B2T2TableAPIProperties.out),
    snd(B2t2.B2T2TableAPIProperties.out),
  ),
  (
    "src/b2t2/slides/table_api/",
    "B2T2TableAPIAccessSubcomponents.ml",
    fst(B2t2.B2T2TableAPIAccessSubcomponents.out),
    snd(B2t2.B2T2TableAPIAccessSubcomponents.out),
  ),
  (
    "src/b2t2/slides/table_api/",
    "B2T2TableAPISubtable.ml",
    fst(B2t2.B2T2TableAPISubtable.out),
    snd(B2t2.B2T2TableAPISubtable.out),
  ),
  (
    "src/b2t2/slides/table_api/",
    "B2T2TableAPIOrdering.ml",
    fst(B2t2.B2T2TableAPIOrdering.out),
    snd(B2t2.B2T2TableAPIOrdering.out),
  ),
  (
    "src/b2t2/slides/table_api/",
    "B2T2TableAPIAggregate.ml",
    fst(B2t2.B2T2TableAPIAggregate.out),
    snd(B2t2.B2T2TableAPIAggregate.out),
  ),
  (
    "src/b2t2/slides/table_api/",
    "B2T2TableAPIMissingValues.ml",
    fst(B2t2.B2T2TableAPIMissingValues.out),
    snd(B2t2.B2T2TableAPIMissingValues.out),
  ),
  (
    "src/b2t2/slides/table_api/",
    "B2T2TableAPIDataCleaning.ml",
    fst(B2t2.B2T2TableAPIDataCleaning.out),
    snd(B2t2.B2T2TableAPIDataCleaning.out),
  ),
  (
    "src/b2t2/slides/table_api/",
    "B2T2TableAPIUtilitiesFlatten.ml",
    fst(B2t2.B2T2TableAPIUtilitiesFlatten.out),
    snd(B2t2.B2T2TableAPIUtilitiesFlatten.out),
  ),
  (
    "src/b2t2/slides/table_api/",
    "B2T2TableAPIUtilitiestransformColumn.ml",
    fst(B2t2.B2T2TableAPIUtilitiestransformColumn.out),
    snd(B2t2.B2T2TableAPIUtilitiestransformColumn.out),
  ),
  (
    "src/b2t2/slides/table_api/",
    "B2T2TableAPIUtilitiesrenameColumns.ml",
    fst(B2t2.B2T2TableAPIUtilitiesrenameColumns.out),
    snd(B2t2.B2T2TableAPIUtilitiesrenameColumns.out),
  ),
  (
    "src/b2t2/slides/table_api/",
    "B2T2TableAPIUtilitiesfind.ml",
    fst(B2t2.B2T2TableAPIUtilitiesfind.out),
    snd(B2t2.B2T2TableAPIUtilitiesfind.out),
  ),
  (
    "src/b2t2/slides/table_api/",
    "B2T2TableAPIUtilitiesgroupByRetentive.ml",
    fst(B2t2.B2T2TableAPIUtilitiesgroupByRetentive.out),
    snd(B2t2.B2T2TableAPIUtilitiesgroupByRetentive.out),
  ),
  (
    "src/b2t2/slides/table_api/",
    "B2T2TableAPIUtilitiesgroupBySubtractive.ml",
    fst(B2t2.B2T2TableAPIUtilitiesgroupBySubtractive.out),
    snd(B2t2.B2T2TableAPIUtilitiesgroupBySubtractive.out),
  ),
  (
    "src/b2t2/slides/table_api/",
    "B2T2TableAPIUtilitiesupdate.ml",
    fst(B2t2.B2T2TableAPIUtilitiesupdate.out),
    snd(B2t2.B2T2TableAPIUtilitiesupdate.out),
  ),
  (
    "src/b2t2/slides/table_api/",
    "B2T2TableAPIUtilitiesselect.ml",
    fst(B2t2.B2T2TableAPIUtilitiesselect.out),
    snd(B2t2.B2T2TableAPIUtilitiesselect.out),
  ),
  (
    "src/b2t2/slides/table_api/",
    "B2T2TableAPIUtilitiesselectMany.ml",
    fst(B2t2.B2T2TableAPIUtilitiesselectMany.out),
    snd(B2t2.B2T2TableAPIUtilitiesselectMany.out),
  ),
  (
    "src/b2t2/slides/table_api/",
    "B2T2TableAPIUtilitiesgroupJoin.ml",
    fst(B2t2.B2T2TableAPIUtilitiesgroupJoin.out),
    snd(B2t2.B2T2TableAPIUtilitiesgroupJoin.out),
  ),
  (
    "src/b2t2/slides/table_api/",
    "B2T2TableAPIUtilitiesjoin.ml",
    fst(B2t2.B2T2TableAPIUtilitiesjoin.out),
    snd(B2t2.B2T2TableAPIUtilitiesjoin.out),
  ),
  (
    "src/b2t2/slides/example_programs/",
    "B2T2ExampleProgramsDotProduct.ml",
    fst(B2t2.B2T2ExampleProgramsDotProduct.out),
    snd(B2t2.B2T2ExampleProgramsDotProduct.out),
  ),
  (
    "src/b2t2/slides/example_programs/",
    "B2T2ExampleProgramspHackingHomogeneous.ml",
    fst(B2t2.B2T2ExampleProgramspHackingHomogeneous.out),
    snd(B2t2.B2T2ExampleProgramspHackingHomogeneous.out),
  ),
  (
    "src/b2t2/slides/example_programs/",
    "B2T2ExampleProgramspHackingHeterogeneous.ml",
    fst(B2t2.B2T2ExampleProgramspHackingHeterogeneous.out),
    snd(B2t2.B2T2ExampleProgramspHackingHeterogeneous.out),
  ),
  (
    "src/b2t2/slides/example_programs/",
    "B2T2ExampleProgramsquizScoreFilter.ml",
    fst(B2t2.B2T2ExampleProgramsquizScoreFilter.out),
    snd(B2t2.B2T2ExampleProgramsquizScoreFilter.out),
  ),
  (
    "src/b2t2/slides/example_programs/",
    "B2T2ExampleProgramsquizScoreSelect.ml",
    fst(B2t2.B2T2ExampleProgramsquizScoreSelect.out),
    snd(B2t2.B2T2ExampleProgramsquizScoreSelect.out),
  ),
  (
    "src/b2t2/slides/example_programs/",
    "B2T2ExampleProgramsgroupByRetentive.ml",
    fst(B2t2.B2T2ExampleProgramsgroupByRetentive.out),
    snd(B2t2.B2T2ExampleProgramsgroupByRetentive.out),
  ),
  (
    "src/b2t2/slides/example_programs/",
    "B2T2ExampleProgramsgroupBySubtractive.ml",
    fst(B2t2.B2T2ExampleProgramsgroupBySubtractive.out),
    snd(B2t2.B2T2ExampleProgramsgroupBySubtractive.out),
  ),
  (
    "src/b2t2/slides/errors/",
    "B2T2ErrorsMalformedTables.ml",
    fst(B2t2.B2T2ErrorsMalformedTables.out),
    snd(B2t2.B2T2ErrorsMalformedTables.out),
  ),
  (
    "src/b2t2/slides/errors/",
    "B2T2ErrorsUsingTablesPart1.ml",
    fst(B2t2.B2T2ErrorsUsingTablesPart1.out),
    snd(B2t2.B2T2ErrorsUsingTablesPart1.out),
  ),
  (
    "src/b2t2/slides/errors/",
    "B2T2ErrorsUsingTablesPart2.ml",
    fst(B2t2.B2T2ErrorsUsingTablesPart2.out),
    snd(B2t2.B2T2ErrorsUsingTablesPart2.out),
  ),
  (
    "src/b2t2/slides/errors/",
    "B2T2ErrorsUsingTablesPart3.ml",
    fst(B2t2.B2T2ErrorsUsingTablesPart3.out),
    snd(B2t2.B2T2ErrorsUsingTablesPart3.out),
  ),
];

/* Test migration on BasicReference slide */
let basic_reference_migration = () => {
  /* Get the original persistent segment */
  let (title, original) = Web.BasicReference.out;

  print_endline("Testing migration of: " ++ title);

  /* Run the migration */
  let migrated = DocSlideMigration.migrate(original);

  /* Print diagnostic info */
  DocSlideMigration.print_migration_diff(title, original, migrated);

  /* Verify round-trip: reparsing migrated backup_text should match migrated segment */
  let reparsed =
    switch (Parser.to_segment(migrated.backup_text)) {
    | Some(seg) => seg
    | None => Alcotest.fail("Failed to parse migrated backup_text")
    };

  let migrated_seg =
    Sexplib.Sexp.of_string(migrated.segment) |> Segment.t_of_sexp;

  check(
    EditingPrelude.segment,
    "Reparsing migrated backup_text matches migrated segment",
    migrated_seg,
    reparsed,
  );
};

/* Test that Format doesn't change already-formatted content */
let format_idempotent = () => {
  let (title, original) = Web.BasicReference.out;
  print_endline("Testing format idempotency on: " ++ title);

  let migrated = DocSlideMigration.migrate(original);

  /* Apply format again - should be idempotent */
  let double_migrated = DocSlideMigration.migrate(migrated);

  check(
    string,
    "Format is idempotent",
    migrated.backup_text,
    double_migrated.backup_text,
  );
};

/* Generate the migrated ML file content for manual inspection */
let generate_ml_file = () => {
  let (title, original) = Web.BasicReference.out;
  let migrated = DocSlideMigration.migrate(original);

  print_endline(
    "=== Generated ML file content (write to BasicReference.ml) ===",
  );
  print_endline("");
  print_string(DocSlideMigration.generate_ml_content(title, migrated));
};

/* Write migrated web docs files directly to disk */
let write_migrated_files = () => {
  let write_slide = ((filename, title, original)) => {
    let migrated = DocSlideMigration.migrate(original);
    let content = DocSlideMigration.generate_ml_content(title, migrated);
    let path = "src/web/init/docs/" ++ filename;
    let oc = open_out(path);
    output_string(oc, content);
    close_out(oc);
    print_endline("Wrote: " ++ path);
  };
  List.iter(write_slide, web_docs_slides);
};

/* Write migrated B2T2 files directly to disk */
let write_migrated_b2t2_files = () => {
  let write_slide = ((base_path, filename, title, original)) => {
    let migrated = DocSlideMigration.migrate(original);
    let content = DocSlideMigration.generate_ml_content(title, migrated);
    let path = base_path ++ filename;
    let oc = open_out(path);
    output_string(oc, content);
    close_out(oc);
    print_endline("Wrote: " ++ path);
  };
  List.iter(write_slide, b2t2_slides);
};

/* Migrate a single slide and output the ML file content.
 * Output format:
 * ===FILE:path/to/File.ml===
 * <ML content>
 * ===END:path/to/File.ml===
 */
let migrate_and_output =
    (
      base_path: string,
      filename: string,
      title: string,
      original: PersistentSegment.t,
    ) => {
  let migrated = DocSlideMigration.migrate(original);
  let ml_content = DocSlideMigration.generate_ml_content(title, migrated);
  let full_path = base_path ++ filename;
  print_endline("===FILE:" ++ full_path ++ "===");
  print_string(ml_content);
  print_endline("===END:" ++ full_path ++ "===");
};

/* Generate migrated ML content for all web docs slides */
let migrate_all_web_docs = () => {
  print_endline("=== Migrating all web docs slides ===");
  print_endline("");
  List.iter(
    ((filename, title, original)) =>
      migrate_and_output("src/web/init/docs/", filename, title, original),
    web_docs_slides,
  );
};

/* Test that verifies all web docs slides can be migrated without error */
let test_all_web_docs_migration = () => {
  List.iter(
    ((filename, title, original)) => {
      print_endline("Migrating: " ++ filename);
      let migrated = DocSlideMigration.migrate(original);

      /* Verify round-trip */
      let reparsed =
        switch (Parser.to_segment(migrated.backup_text)) {
        | Some(seg) => seg
        | None => Alcotest.fail("Failed to parse migrated " ++ filename)
        };

      let migrated_seg =
        Sexplib.Sexp.of_string(migrated.segment) |> Segment.t_of_sexp;

      check(
        EditingPrelude.segment,
        "Round-trip for " ++ title,
        migrated_seg,
        reparsed,
      );
    },
    web_docs_slides,
  );
};

/* Debug: compare migrated segment structure */
let debug_adts_migration = () => {
  let (_, original) = Web.ADTs.out;

  /* Migrate */
  let migrated = DocSlideMigration.migrate(original);

  /* Get migrated segment from sexp */
  let migrated_seg =
    Sexplib.Sexp.of_string(migrated.segment) |> Segment.t_of_sexp;

  /* Parse migrated backup_text */
  let reparsed =
    switch (Parser.to_segment(migrated.backup_text)) {
    | Some(seg) => seg
    | None => Alcotest.fail("Failed to parse migrated backup_text")
    };

  print_endline(
    "Migrated segment has "
    ++ string_of_int(List.length(migrated_seg))
    ++ " pieces",
  );
  print_endline(
    "Reparsed backup_text has "
    ++ string_of_int(List.length(reparsed))
    ++ " pieces",
  );

  /* Compare using the test's equality */
  check(
    EditingPrelude.segment,
    "ADTs migration round-trip",
    migrated_seg,
    reparsed,
  );
};

let tests = [
  (
    "DocSlideMigration",
    [
      test_case(
        "BasicReference round-trip after migration",
        `Quick,
        basic_reference_migration,
      ),
      test_case("Format is idempotent", `Quick, format_idempotent),
      test_case("Generate ML file", `Quick, generate_ml_file),
      test_case(
        "All web docs slides migrate successfully",
        `Quick,
        test_all_web_docs_migration,
      ),
      test_case(
        "Output all web docs migrations",
        `Quick,
        migrate_all_web_docs,
      ),
      test_case("Debug ADTs migration", `Quick, debug_adts_migration),
      test_case("Write migrated files to disk", `Quick, write_migrated_files),
      test_case(
        "Write migrated B2T2 files to disk",
        `Quick,
        write_migrated_b2t2_files,
      ),
    ],
  ),
];
