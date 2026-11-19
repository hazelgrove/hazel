/* Dev tool (nothing at runtime depends on this file): regenerates the
 * derivation exercise modules' shipped form (code and theorem exercises
 * are .hzt text, hazel-programs/exercises/). Each registered module is
 * reprinted with every editor as a PersistentZipper.t literal
 * ({zipper: "<Zipper sexp>", backup_text: "<program text>"}) decoded via
 * *.of_persistent at load time: sexp decode is ~free at startup, and a
 * decode failure after a zipper representation change self-heals by
 * re-parsing backup_text (with a printed warning). Re-run this after such
 * a change to refresh the sexps and silence the warnings.
 *
 * Output format (stdout):
 *   ===FILE: <path relative to repo root>===
 *   <content>
 *   ===END===
 *   ...
 *   ===SUMMARY===
 *   <per-file PASS/FAIL lines>
 *   ===END===
 *
 * A file is only emitted if every editor passes (a) the parse-fixpoint
 * check (text |> PersistentZipper.parse_text |> print == text; guards the
 * backup_text fallback, which parses the same way) and (b) the
 * persistent-decode equivalence check (the emitted zipper sexp decodes and
 * is segment-structurally equal to the reparse of backup_text); failures
 * are reported in the summary (and on stderr) and the original file should
 * be left in place.
 *
 * `--reindent` also re-indents every editor (the Format(Indent) action)
 * before writing it. Indentation is the text's own spaces, owned by the
 * author, so a plain refresh leaves it alone; the flag is for modules
 * authored when the editor computed indentation at layout time, which
 * were stored flat (every line at column 0).
 *
 * Build & run:
 *   dune build src/web/migrate_exercises.bc.js --profile dev
 *   node --stack-size=8192 --require ./test/idb_stub.js \
 *     _build/default/src/web/migrate_exercises.bc.js [--reindent] \
 *     > /tmp/migrate_out.txt
 * then split the dump into files and format them, e.g.:
 *   awk '/^===FILE: /{f=substr($0,10,length($0)-12); next}
 *        /^===END===$/{f=""; next} f{print > f}' /tmp/migrate_out.txt
 *   dune fmt
 */
open Haz3lcore;
open Web;

let reindent = Array.mem("--reindent", Sys.argv);

/* Reset non-persistable refractor state before serialization (keeps
 * manuals, resets multis/sample_focus). */
let persist_zipper = (z: Zipper.t): PersistentZipper.t =>
  (reindent ? Perform.reindent_zipper(z) : z)
  |> Zipper.update_refractors(_, Refractors.for_serialization)
  |> PersistentZipper.persist;

/* Print a zipper as a PersistentZipper.t record literal. */
let persistent_pp = (fmt, z: Zipper.t) =>
  PersistentZipper.pp(fmt, persist_zipper(z));

/* One top-level `let` in an output file. The string is the value name. */
type item =
  | DrvEx(string, DerivationExercise.spec) /* let <name> : Exercise.t = Derivation(...) */
  | DrvSpec(string, DerivationExercise.spec); /* let <name> : DerivationExercise.spec = ... */

type file = {
  path: string,
  items: list(item),
};

let drv_spec = (e: Exercise.t): DerivationExercise.spec =>
  switch (e) {
  | Derivation(s) => s
  | _ => failwith("Migrate_exercises: expected Derivation exercise")
  };

/* The migration targets: every registered module that embeds zipper
 * literals. Feature branches with custom exercises: extend this table
 * (and only this table) with your own modules. BlankDerivationExercise.ml
 * is a template built from blank_spec (no zipper literals) — nothing to do. */
let files: list(file) = [
  {
    path: "src/web/exercises/examples/Ex_EvaluationDerivation.ml",
    items: [DrvEx("exercise", drv_spec(Ex_EvaluationDerivation.exercise))],
  },
  {
    path: "src/web/derivation/examples/Ex_Conjunction_Commutativity.ml",
    items: [DrvSpec("exercise", Ex_Conjunction_Commutativity.exercise)],
  },
  {
    path: "src/web/derivation/examples/Ex_Curried_Function_Derivation.ml",
    items: [DrvSpec("exercise", Ex_Curried_Function_Derivation.exercise)],
  },
  {
    path: "src/web/derivation/examples/Ex_PairMap_Derivation.ml",
    items: [DrvSpec("exercise", Ex_PairMap_Derivation.exercise)],
  },
  {
    path: "src/web/derivation/examples/Ex_Shadowing_And_Closures.ml",
    items: [DrvSpec("exercise", Ex_Shadowing_And_Closures.exercise)],
  },
  {
    path: "src/web/derivation/examples/Ex_Type_Validation_Derivation.ml",
    items: [DrvSpec("exercise", Ex_Type_Validation_Derivation.exercise)],
  },
];

/* ---------- emission ---------- */

let emit_item = (item: item): string =>
  switch (item) {
  | DrvEx(name, spec) =>
    "let "
    ++ name
    ++ " : Exercise.t =\n  Derivation\n    (DerivationExercise.of_persistent\n       "
    ++ DerivationExercise.show_p(persistent_pp, spec)
    ++ ")\n"
  | DrvSpec(name, spec) =>
    "let "
    ++ name
    ++ " : DerivationExercise.spec =\n  DerivationExercise.of_persistent\n    "
    ++ DerivationExercise.show_p(persistent_pp, spec)
    ++ "\n"
  };

let emit_file = (f: file): string =>
  f.items |> List.map(emit_item) |> String.concat("\n");

/* ---------- safety checks ----------
 * The backup_text fallback re-parses program text and the exercise loader
 * FAILWITHS on parse failure, so before converting we verify for every
 * editor:
 *  (a) parse fixpoint:
 *      text |> PersistentZipper.parse_text(~root) |> persist == text
 *  (b) persistent-decode equivalence: the zipper sexp we emit decodes, and
 *      its segment structurally equals (ids ignored) the segment obtained
 *      by re-parsing backup_text. */

/* Id-ignoring structural segment equality (copy of
 * test/EditingPrelude.equal_segment; the test lib is not linkable here). */
let rec equal_segment = (a: Base.segment, b: Base.segment) =>
  List.equal(equal_piece, a, b)
and equal_piece = (a: Base.piece, b: Base.piece) =>
  switch (a, b) {
  | (Tile(t1), Tile(t2)) =>
    Tile.label(t1) == Tile.label(t2)
    && List.equal(equal_segment, t1.children, t2.children)
    && Tile.mold(t1) == Tile.mold(t2)
    && t1.shards == t2.shards
  | (Grout(g1), Grout(g2)) => g1.shape == g2.shape
  | (Secondary(s1), Secondary(s2)) => s1.content == s2.content
  | (Projector(p1), Projector(p2)) =>
    p1.kind == p2.kind
    && p1.model == p2.model
    && equal_piece(p1.syntax, p2.syntax)
  | _ => false
  };

type field = {
  label: string,
  root: Sort.t,
  zipper: Zipper.t,
};

let drv_fields = (name, s: DerivationExercise.spec): list(field) => {
  let acc = ref([]);
  let _: DerivationExercise.p(Zipper.t) =
    DerivationExercise.mapi(
      s,
      (pos, zipper) => {
        acc :=
          [
            {
              label: name ++ "." ++ DerivationExercise.show_pos(pos),
              root: DerivationExercise.root_of_pos(pos),
              zipper,
            },
            ...acc^,
          ];
        zipper;
      },
    );
  List.rev(acc^);
};

let fields_of_item = (item: item): list(field) =>
  switch (item) {
  | DrvEx(name, s)
  | DrvSpec(name, s) => drv_fields(name, s)
  };

let seg_of_zipper = (z: Zipper.t): Base.segment =>
  Zipper.unselect_and_zip(~erase_buffer=true, z);

let check_field = ({label, root, zipper}: field): option(string) => {
  let persisted = persist_zipper(zipper);
  let code = persisted.backup_text;
  switch (PersistentZipper.parse_text(~source=label, ~root, code)) {
  | None => Some(label ++ ": reparse FAILED (PersistentZipper.parse_text)")
  | Some(z2) =>
    let code2 = PersistentZipper.persist(z2).backup_text;
    if (code2 != code) {
      Some(
        label
        ++ ": fixpoint MISMATCH\n  original: "
        ++ String.escaped(code)
        ++ "\n  reprinted: "
        ++ String.escaped(code2),
      );
    } else {
      /* what the emitted module will yield at init (bypassing
       * PersistentZipper.unpersist so a decode failure can't silently
       * take the backup_text fallback) */
      switch (Sexplib.Sexp.of_string(persisted.zipper) |> Zipper.t_of_sexp) {
      | exception exn =>
        Some(
          label
          ++ ": persistent sexp decode FAILED ("
          ++ Printexc.to_string(exn)
          ++ ")",
        )
      | decoded =>
        equal_segment(seg_of_zipper(decoded), seg_of_zipper(z2))
          ? None
          : Some(
              label
              ++ ": persistent decode / reparse segment MISMATCH\n"
              ++ "  decoded:  "
              ++ String.escaped(Segment.show(seg_of_zipper(decoded)))
              ++ "\n  reparsed: "
              ++ String.escaped(Segment.show(seg_of_zipper(z2))),
            )
      };
    };
  };
};

let check_file = (f: file): list(string) =>
  f.items |> List.concat_map(fields_of_item) |> List.filter_map(check_field);

/* ---------- registry coverage cross-check ----------
 * Make sure every registered exercise/tutorial/derivation-slide is reached
 * by the table above (so nothing registered is silently left in zipper
 * form). Extra table entries not in a registry are fine. */

let registry_warnings = (): list(string) => {
  let covered_ids =
    files
    |> List.concat_map(f => f.items)
    |> List.map(
         fun
         | DrvEx(_, s)
         | DrvSpec(_, s) => s.id,
       );
  let missing = (kind, title, id) =>
    List.mem(id, covered_ids)
      ? None
      : Some(
          "registered "
          ++ kind
          ++ " not covered by Migrate_exercises table: "
          ++ title,
        );
  /* Tutorial lessons and code/theorem exercises are .hzt text (no zipper
     literals) — nothing to migrate. */
  List.filter_map(
    (e: Exercise.t) =>
      switch (e) {
      | Derivation(s) => missing("exercise", s.title, s.id)
      | Code(_)
      | Theorem(_) => None
      },
    ExerciseSettings_base.exercises,
  )
  @ List.filter_map(
      ((name, s): (string, DerivationExercise.spec)) =>
        missing("derivation slide", name, s.id),
      Init.documentation_drv_slides,
    );
};

/* ---------- main ---------- */

let () = {
  let summary = ref([]);
  files
  |> List.iter(f => {
       switch (check_file(f)) {
       | [] =>
         print_string("===FILE: " ++ f.path ++ "===\n");
         print_string(emit_file(f));
         print_string("===END===\n");
         summary := [f.path ++ ": PASS", ...summary^];
       | errors =>
         prerr_endline("FIXPOINT FAILURE — NOT converting " ++ f.path);
         List.iter(prerr_endline, errors);
         summary :=
           [
             f.path
             ++ ": FAIL (left unconverted)\n  "
             ++ String.concat("\n  ", errors),
             ...summary^,
           ];
       }
     });
  let warnings = registry_warnings();
  print_string("===SUMMARY===\n");
  List.iter(print_endline, List.rev(summary^));
  List.iter(print_endline, warnings);
  print_string("===END===\n");
  List.iter(prerr_endline, warnings);
};
