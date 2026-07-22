/* DISPOSAL: disposable migration tooling for the tile FormId change.
 * Delete this file (together with LegacyBase.re and the migration
 * sections of scripts/README_migrate_tile_format.md) once tile-datatype
 * has merged to dev and active feature branches have run the recipe in
 * scripts/README_migrate_tile_format.md.
 * (Migrate_exercises.re and scripts/split_migrate_output.py outlive the
 * migration as the example-module re-export tool.) Nothing at runtime
 * depends on it. */

/* Serialized-slide renormalizer for the tile-datatype flip.
 *
 * The 49 slide files under src/web/init/docs and src/b2t2/slides embed
 * segment sexps as strings inside PersistentSegment.t records. This tool
 * decodes each embedded segment and re-encodes it in the current format,
 * emitting full replacement .ml files. backup_text and refractors are
 * passed through byte-for-byte.
 *
 * Two-tier decode: CURRENT format first (Segment.t_of_sexp: v2
 * families + Tok/TokInfix + explicit tile sort), then the pre-FormId
 * label+mold format (LegacyBase), upgraded id-preservingly. So the
 * tool is IDEMPOTENT and usable by branches in either state;
 * re-encoding normalizes and drops default-valued sort/shards/children
 * fields. (A FormId-v1 tier existed transiently; v1 sexps only ever
 * appeared on intermediate commits of the tile-datatype branch and
 * were regenerated to v2 there.)
 *
 * Output format (stdout), consumed by scripts/split_migrate_output.py:
 *   ===FILE: <path relative to repo root>===
 *   <content>
 *   ===END===
 *   ...
 *   ===SUMMARY===
 *   <per-file lines w/ decode path, upgrade-path histogram, registry
 *   warnings>
 *   ===END===
 *
 * If any slide's segment fails BOTH decodes, the tool fails before
 * emitting ANY file blocks (all slides are migrated up front).
 *
 * Build & run (see scripts/README_migrate_tile_format.md):
 *   dune build src/web/migrate_slides.bc.js --profile dev
 *   node --stack-size=8192 --require ./test/idb_stub.js \
 *     _build/default/src/web/migrate_slides.bc.js
 */
open Haz3lcore;

module Buffer = Stdlib.Buffer;
module Printf = Stdlib.Printf;
module String = Stdlib.String;

/* Emission mirrors src/CLI/Slide.re (render_slide_file/escape_for_ocaml)
 * so migrated files match slide-encode output; the CLI is an executable,
 * not a library, so the two functions are replicated here. */
let escape_for_ocaml = (s: string): string => {
  let buf = Buffer.create(String.length(s) + 16);
  String.iter(
    fun
    | '\n' => Buffer.add_string(buf, "\\n")
    | '\r' => Buffer.add_string(buf, "\\r")
    | '\t' => Buffer.add_string(buf, "\\t")
    | '"' => Buffer.add_string(buf, "\\\"")
    | '\\' => Buffer.add_string(buf, "\\\\")
    | c when Char.code(c) < 32 || Char.code(c) >= 127 =>
      Buffer.add_string(buf, Printf.sprintf("\\%03d", Char.code(c)))
    | c => Buffer.add_char(buf, c),
    s,
  );
  Buffer.contents(buf);
};

let render_slide_file = (title: string, p: PersistentSegment.t): string =>
  Printf.sprintf(
    "let out : string * Haz3lcore.PersistentSegment.t =\n  ( \"%s\",\n    {\n      segment = \"%s\";\n      backup_text = \"%s\";\n      refractors = \"%s\";\n    } )\n",
    escape_for_ocaml(title),
    escape_for_ocaml(p.segment),
    escape_for_ocaml(p.backup_text),
    escape_for_ocaml(p.refractors),
  );

/* The migration targets: every slide module embedding a serialized
 * segment. B2T2 / Datasheet is absent by design: it is built at load
 * time from Datasheet.md, nothing serialized. Feature branches with
 * custom slide files: extend this table with (path, YourSlide.out). */
let entries: list((string, (string, PersistentSegment.t))) = [
  ("src/web/init/docs/BasicReference.ml", Web.BasicReference.out),
  ("src/web/init/docs/Projectors.ml", Web.Projectors.out),
  ("src/web/init/docs/ADTs.ml", Web.ADTs.out),
  ("src/web/init/docs/Tuples.ml", Web.Tuples.out),
  ("src/web/init/docs/Modules.ml", Web.Modules.out),
  ("src/web/init/docs/Tables.ml", Web.Tables.out),
  ("src/web/init/docs/Polymorphism.ml", Web.Polymorphism.out),
  ("src/web/init/docs/Cards.ml", Web.Cards.out),
  ("src/web/init/docs/Probes.ml", Web.Probes.out),
  ("src/web/init/docs/Livelits.ml", Web.Livelits.out),
  ("src/b2t2/slides/B2T2ExampleTables.ml", B2t2.B2T2ExampleTables.out),
  (
    "src/b2t2/slides/table_api/B2T2TableAPIConstructorsemptyTable.ml",
    B2t2.B2T2TableAPIConstructorsemptyTable.out,
  ),
  (
    "src/b2t2/slides/table_api/B2T2TableAPIConstructorsaddRows.ml",
    B2t2.B2T2TableAPIConstructorsaddRows.out,
  ),
  (
    "src/b2t2/slides/table_api/B2T2TableAPIConstructorsaddColumn.ml",
    B2t2.B2T2TableAPIConstructorsaddColumn.out,
  ),
  (
    "src/b2t2/slides/table_api/B2T2TableAPIConstructorsbuildColumn.ml",
    B2t2.B2T2TableAPIConstructorsbuildColumn.out,
  ),
  (
    "src/b2t2/slides/table_api/B2T2TableAPIConstructorsvcat.ml",
    B2t2.B2T2TableAPIConstructorsvcat.out,
  ),
  (
    "src/b2t2/slides/table_api/B2T2TableAPIConstructorshcat.ml",
    B2t2.B2T2TableAPIConstructorshcat.out,
  ),
  (
    "src/b2t2/slides/table_api/B2T2TableAPIConstructorsvalues.ml",
    B2t2.B2T2TableAPIConstructorsvalues.out,
  ),
  (
    "src/b2t2/slides/table_api/B2T2TableAPIConstructorscrossJoin.ml",
    B2t2.B2T2TableAPIConstructorscrossJoin.out,
  ),
  (
    "src/b2t2/slides/table_api/B2T2TableAPIConstructorsleftJoin.ml",
    B2t2.B2T2TableAPIConstructorsleftJoin.out,
  ),
  (
    "src/b2t2/slides/table_api/B2T2TableAPIProperties.ml",
    B2t2.B2T2TableAPIProperties.out,
  ),
  (
    "src/b2t2/slides/table_api/B2T2TableAPIAccessSubcomponents.ml",
    B2t2.B2T2TableAPIAccessSubcomponents.out,
  ),
  (
    "src/b2t2/slides/table_api/B2T2TableAPISubtable.ml",
    B2t2.B2T2TableAPISubtable.out,
  ),
  (
    "src/b2t2/slides/table_api/B2T2TableAPIOrdering.ml",
    B2t2.B2T2TableAPIOrdering.out,
  ),
  (
    "src/b2t2/slides/table_api/B2T2TableAPIAggregate.ml",
    B2t2.B2T2TableAPIAggregate.out,
  ),
  (
    "src/b2t2/slides/table_api/B2T2TableAPIMissingValues.ml",
    B2t2.B2T2TableAPIMissingValues.out,
  ),
  (
    "src/b2t2/slides/table_api/B2T2TableAPIDataCleaning.ml",
    B2t2.B2T2TableAPIDataCleaning.out,
  ),
  (
    "src/b2t2/slides/table_api/B2T2TableAPIUtilitiesFlatten.ml",
    B2t2.B2T2TableAPIUtilitiesFlatten.out,
  ),
  (
    "src/b2t2/slides/table_api/B2T2TableAPIUtilitiestransformColumn.ml",
    B2t2.B2T2TableAPIUtilitiestransformColumn.out,
  ),
  (
    "src/b2t2/slides/table_api/B2T2TableAPIUtilitiesrenameColumns.ml",
    B2t2.B2T2TableAPIUtilitiesrenameColumns.out,
  ),
  (
    "src/b2t2/slides/table_api/B2T2TableAPIUtilitiesfind.ml",
    B2t2.B2T2TableAPIUtilitiesfind.out,
  ),
  (
    "src/b2t2/slides/table_api/B2T2TableAPIUtilitiesgroupByRetentive.ml",
    B2t2.B2T2TableAPIUtilitiesgroupByRetentive.out,
  ),
  (
    "src/b2t2/slides/table_api/B2T2TableAPIUtilitiesgroupBySubtractive.ml",
    B2t2.B2T2TableAPIUtilitiesgroupBySubtractive.out,
  ),
  (
    "src/b2t2/slides/table_api/B2T2TableAPIUtilitiesupdate.ml",
    B2t2.B2T2TableAPIUtilitiesupdate.out,
  ),
  (
    "src/b2t2/slides/table_api/B2T2TableAPIUtilitiesselect.ml",
    B2t2.B2T2TableAPIUtilitiesselect.out,
  ),
  (
    "src/b2t2/slides/table_api/B2T2TableAPIUtilitiesselectMany.ml",
    B2t2.B2T2TableAPIUtilitiesselectMany.out,
  ),
  (
    "src/b2t2/slides/table_api/B2T2TableAPIUtilitiesgroupJoin.ml",
    B2t2.B2T2TableAPIUtilitiesgroupJoin.out,
  ),
  (
    "src/b2t2/slides/table_api/B2T2TableAPIUtilitiesjoin.ml",
    B2t2.B2T2TableAPIUtilitiesjoin.out,
  ),
  (
    "src/b2t2/slides/example_programs/B2T2ExampleProgramsDotProduct.ml",
    B2t2.B2T2ExampleProgramsDotProduct.out,
  ),
  (
    "src/b2t2/slides/example_programs/B2T2ExampleProgramspHackingHomogeneous.ml",
    B2t2.B2T2ExampleProgramspHackingHomogeneous.out,
  ),
  (
    "src/b2t2/slides/example_programs/B2T2ExampleProgramspHackingHeterogeneous.ml",
    B2t2.B2T2ExampleProgramspHackingHeterogeneous.out,
  ),
  (
    "src/b2t2/slides/example_programs/B2T2ExampleProgramsquizScoreFilter.ml",
    B2t2.B2T2ExampleProgramsquizScoreFilter.out,
  ),
  (
    "src/b2t2/slides/example_programs/B2T2ExampleProgramsquizScoreSelect.ml",
    B2t2.B2T2ExampleProgramsquizScoreSelect.out,
  ),
  (
    "src/b2t2/slides/example_programs/B2T2ExampleProgramsgroupByRetentive.ml",
    B2t2.B2T2ExampleProgramsgroupByRetentive.out,
  ),
  (
    "src/b2t2/slides/example_programs/B2T2ExampleProgramsgroupBySubtractive.ml",
    B2t2.B2T2ExampleProgramsgroupBySubtractive.out,
  ),
  (
    "src/b2t2/slides/errors/B2T2ErrorsMalformedTables.ml",
    B2t2.B2T2ErrorsMalformedTables.out,
  ),
  (
    "src/b2t2/slides/errors/B2T2ErrorsUsingTablesPart1.ml",
    B2t2.B2T2ErrorsUsingTablesPart1.out,
  ),
  (
    "src/b2t2/slides/errors/B2T2ErrorsUsingTablesPart2.ml",
    B2t2.B2T2ErrorsUsingTablesPart2.out,
  ),
  (
    "src/b2t2/slides/errors/B2T2ErrorsUsingTablesPart3.ml",
    B2t2.B2T2ErrorsUsingTablesPart3.out,
  ),
];

/* Decode (current format, then label+mold legacy) => re-encode.
 * backup_text and refractors pass through untouched. Raises (before
 * anything is emitted) if all decodes fail: such a slide must be
 * investigated, not silently regenerated from backup_text. */
let migrate =
    (title: string, p: PersistentSegment.t): (PersistentSegment.t, string) => {
  let sexp =
    try(p.segment |> Sexplib.Sexp.of_string) {
    | exn =>
      failwith(
        "segment sexp UNPARSEABLE for slide \""
        ++ title
        ++ "\": "
        ++ Printexc.to_string(exn),
      )
    };
  let (upgraded, path) =
    switch (Segment.t_of_sexp(sexp)) {
    | seg => (seg, "current")
    | exception _ =>
      switch (LegacyBase.segment_of_sexp(sexp)) {
      | legacy => (LegacyBase.upgrade_segment(legacy), "legacy")
      | exception exn =>
        failwith(
          "segment decode FAILED (current AND legacy) for slide \""
          ++ title
          ++ "\": "
          ++ Printexc.to_string(exn),
        )
      }
    };
  (
    {
      ...p,
      segment: upgraded |> Segment.sexp_of_t |> Sexplib.Sexp.to_string,
    },
    path,
  );
};

/* Cross-check: every slide registered at startup is either in the table
 * above or the runtime-generated Datasheet. */
let registry_warnings = (): list(string) => {
  let covered = entries |> List.map(((_, (title, _))) => title);
  let registered = Web.Init.documentation_slides |> List.map(fst);
  let missing =
    registered
    |> List.filter(t => !List.mem(t, covered) && t != "B2T2 / Datasheet")
    |> List.map(t => "registered slide not covered by table: " ++ t);
  let extra =
    covered
    |> List.filter(t => !List.mem(t, registered))
    |> List.map(t => "table entry not registered at startup: " ++ t);
  missing @ extra;
};

let () = {
  LegacyBase.reset_counts();
  /* migrate everything first: a decode failure aborts the whole run
   * before any ===FILE block reaches stdout */
  let migrated =
    entries
    |> List.map(((path, (title, p))) => (path, title, migrate(title, p)));
  migrated
  |> List.iter(((path, title, (p, _))) => {
       print_string("===FILE: " ++ path ++ "===\n");
       print_string(render_slide_file(title, p));
       print_string("===END===\n");
     });
  let warnings = registry_warnings();
  print_string("===SUMMARY===\n");
  migrated
  |> List.iter(((path, _, (p: PersistentSegment.t, decode_path))) =>
       print_endline(
         path
         ++ ": PASS ("
         ++ decode_path
         ++ ")"
         ++ (p.refractors == "()" ? "" : " (non-trivial refractors)"),
       )
     );
  Printf.printf(
    "upgrade paths: a(compound)=%d b(atomic)=%d c(any-fallback)=%d d(classified/stale-mold)=%d\n",
    LegacyBase.count_compound^,
    LegacyBase.count_atomic^,
    LegacyBase.count_any_fallback^,
    LegacyBase.count_classified^,
  );
  List.iter(print_endline, List.rev(LegacyBase.classified_log^));
  List.iter(print_endline, warnings);
  print_string("===END===\n");
  List.iter(prerr_endline, warnings);
};
