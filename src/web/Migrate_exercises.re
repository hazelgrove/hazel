/* DISPOSAL: originally disposable migration tooling for the tile FormId
 * change; now also the re-export tool for the example exercise modules'
 * shipped (persistent) format, so keep it as long as those modules exist.
 * The migration-specific companions (LegacyBase.re,
 * Migrate_slides.re) can still be deleted once tile-datatype has merged to
 * dev and active feature branches have run the recipe in
 * scripts/README_migrate_tile_format.md. Nothing at runtime depends on
 * this file. */

/* Exporter for the example exercise modules (tile-datatype migration
 * endpoint, reusable for any future re-export).
 *
 * Prints PERSISTENT versions of every registered example module: each
 * editor becomes a PersistentZipper.t literal ({zipper: "<Zipper sexp>",
 * backup_text: "<program text>"}, the same scheme doc slides use) and the
 * module calls CodeExercise.of_persistent / Tutorial.of_persistent /
 * TheoremExercise.of_persistent / DerivationExercise.of_persistent at load
 * time. Decoding the sexp is ~free (no per-character re-parse at startup),
 * and a decode failure self-heals by re-parsing backup_text.
 *
 * (The string-based "transitionary" format this exporter previously
 * emitted — see the *.transition functions, which remain — is the
 * type-independent migration interchange format: it survives Zipper.t
 * datatype changes at the cost of a startup re-parse. Recipe for feature
 * branches in scripts/README_migrate_tile_format.md.)
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
 * check (text |> Parser.to_zipper |> print == text; guards the backup_text
 * fallback) and (b) the persistent-decode equivalence check (the emitted
 * zipper sexp decodes and is segment-structurally equal to the reparse of
 * backup_text); failures are reported in the summary (and on stderr) and
 * the original file should be left in place.
 *
 * Build & run (see scripts/README_migrate_tile_format.md):
 *   dune build src/web/migrate_exercises.bc.js --profile dev
 *   node --stack-size=8192 --require ./test/idb_stub.js \
 *     _build/default/src/web/migrate_exercises.bc.js
 */
open Haz3lcore;
open Web;

/* Reset non-persistable refractor state before serialization, mirroring
 * CodeExercise.editor_pp (keeps manuals, resets multis/sample_focus). */
let persist_zipper = (z: Zipper.t): PersistentZipper.t =>
  z
  |> Zipper.update_refractors(_, Refractors.for_serialization)
  |> PersistentZipper.persist;

/* Print a zipper as a PersistentZipper.t record literal. */
let persistent_pp = (fmt, z: Zipper.t) =>
  PersistentZipper.pp(fmt, persist_zipper(z));

/* One top-level `let` in an output file. The string is the value name. */
type item =
  | CodeEx(string, CodeExercise.spec) /* let <name> : Exercise.t = Code(...) */
  | DrvEx(string, DerivationExercise.spec) /* let <name> : Exercise.t = Derivation(...) */
  | ThmEx(string, TheoremExercise.spec) /* let <name> : Exercise.t = Theorem(...) */
  | TutorialSpec(string, Tutorial.spec) /* let <name> : Tutorial.spec = ... */
  | DrvSpec(string, DerivationExercise.spec); /* let <name> : DerivationExercise.spec = ... */

type file = {
  path: string,
  items: list(item),
};

let code_spec = (e: Exercise.t): CodeExercise.spec =>
  switch (e) {
  | Code(s) => s
  | _ => failwith("Migrate_exercises: expected Code exercise")
  };

let drv_spec = (e: Exercise.t): DerivationExercise.spec =>
  switch (e) {
  | Derivation(s) => s
  | _ => failwith("Migrate_exercises: expected Derivation exercise")
  };

let thm_spec = (e: Exercise.t): TheoremExercise.spec =>
  switch (e) {
  | Theorem(s) => s
  | _ => failwith("Migrate_exercises: expected Theorem exercise")
  };

/* The migration targets: every registered module that embeds zipper
 * literals. Feature branches with custom exercises: extend this table
 * (and only this table) with your own modules. Blank*Exercise.ml are
 * templates built from blank_spec (no zipper literals) — nothing to do. */
let files: list(file) = [
  {
    path: "src/web/exercises/examples/Ex_OddlyRecursive.ml",
    items: [CodeEx("exercise", code_spec(Ex_OddlyRecursive.exercise))],
  },
  {
    path: "src/web/exercises/examples/Ex_RecursiveFibonacci.ml",
    items: [CodeEx("exercise", code_spec(Ex_RecursiveFibonacci.exercise))],
  },
  {
    path: "src/web/exercises/examples/Ex_ReverseReverse.ml",
    items: [ThmEx("exercise", thm_spec(Ex_ReverseReverse.exercise))],
  },
  {
    path: "src/web/exercises/examples/Ex_EvaluationDerivation.ml",
    items: [DrvEx("exercise", drv_spec(Ex_EvaluationDerivation.exercise))],
  },
  {
    path: "src/web/exercises/examples/Tu_ExpressiveProgramming.ml",
    items: [TutorialSpec("exercise", Tu_ExpressiveProgramming.exercise)],
  },
  {
    path: "src/web/exercises/examples/Tu_ComposingArithmetic_and_Scope.ml",
    items: [
      TutorialSpec("exercise", Tu_ComposingArithmetic_and_Scope.exercise),
      TutorialSpec(
        "scope_exercise",
        Tu_ComposingArithmetic_and_Scope.scope_exercise,
      ),
    ],
  },
  {
    path: "src/web/exercises/examples/Tu_ComputingEquationally_Shadow.ml",
    items: [
      TutorialSpec("exercise", Tu_ComputingEquationally_Shadow.exercise),
      TutorialSpec(
        "shadow_exercise",
        Tu_ComputingEquationally_Shadow.shadow_exercise,
      ),
    ],
  },
  {
    path: "src/web/exercises/examples/Tu_Variables_and_Compositionality.ml",
    items: [
      TutorialSpec("exercise", Tu_Variables_and_Compositionality.exercise),
      TutorialSpec(
        "comp_exercise",
        Tu_Variables_and_Compositionality.comp_exercise,
      ),
    ],
  },
  {
    path: "src/web/exercises/examples/Tu_More.ml",
    items: [
      TutorialSpec("bools_ex", Tu_More.bools_ex),
      TutorialSpec("cond_ex", Tu_More.cond_ex),
      TutorialSpec("func_ex", Tu_More.func_ex),
    ],
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
  | CodeEx(name, spec) =>
    "let "
    ++ name
    ++ " : Exercise.t =\n  Code\n    (CodeExercise.of_persistent\n       "
    ++ CodeExercise.show_p(persistent_pp, spec)
    ++ ")\n"
  | DrvEx(name, spec) =>
    "let "
    ++ name
    ++ " : Exercise.t =\n  Derivation\n    (DerivationExercise.of_persistent\n       "
    ++ DerivationExercise.show_p(persistent_pp, spec)
    ++ ")\n"
  | ThmEx(name, spec) =>
    let ps: TheoremExercise.persistent_spec = {
      id: spec.id,
      title: spec.title,
      module_name: spec.module_name,
      prompt: spec.prompt,
      max_points: spec.max_points,
      prelude: persist_zipper(spec.prelude),
      lemmas: persist_zipper(spec.lemmas),
      theorem: persist_zipper(spec.theorem),
    };
    "let "
    ++ name
    ++ " : Exercise.t =\n  Theorem\n    (TheoremExercise.of_persistent\n       "
    ++ TheoremExercise.show_persistent_spec(ps)
    ++ ")\n";
  | TutorialSpec(name, spec) =>
    "let "
    ++ name
    ++ " : Tutorial.spec =\n  Tutorial.of_persistent\n    "
    ++ Tutorial.show_p(persistent_pp, spec)
    ++ "\n"
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
 *      text |> Parser.to_zipper(~root) |> PersistentZipper.to_string == text
 *  (b) persistent-decode equivalence: the zipper sexp we emit decodes, and
 *      its segment structurally equals (ids ignored) the segment obtained
 *      by re-parsing backup_text — i.e. what transition(text) would have
 *      produced. */

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

let code_fields = (name, s: CodeExercise.spec): list(field) => {
  let f = (label, zipper) => {
    label: name ++ "." ++ label,
    root: Sort.Exp,
    zipper,
  };
  [
    f("prelude", s.prelude),
    f("correct_impl", s.correct_impl),
    f("your_tests.tests", s.your_tests.tests),
    f("your_impl", s.your_impl),
  ]
  @ List.mapi(
      (i, wi: CodeExercise.wrong_impl(Zipper.t)) =>
        f("hidden_bugs[" ++ string_of_int(i) ++ "].impl", wi.impl),
      s.hidden_bugs,
    )
  @ [f("hidden_tests.tests", s.hidden_tests.tests)];
};

let tutorial_fields = (name, s: Tutorial.spec): list(field) => [
  {
    label: name ++ ".your_impl",
    root: Sort.Exp,
    zipper: s.your_impl,
  },
  {
    label: name ++ ".hidden_tests.tests",
    root: Sort.Exp,
    zipper: s.hidden_tests.tests,
  },
];

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

let thm_fields = (name, s: TheoremExercise.spec): list(field) => {
  let f = (label, zipper) => {
    label: name ++ "." ++ label,
    root: Sort.Exp,
    zipper,
  };
  [
    f("prelude", s.prelude),
    f("lemmas", s.lemmas),
    f("theorem", s.theorem),
  ];
};

let fields_of_item = (item: item): list(field) =>
  switch (item) {
  | CodeEx(name, s) => code_fields(name, s)
  | DrvEx(name, s)
  | DrvSpec(name, s) => drv_fields(name, s)
  | ThmEx(name, s) => thm_fields(name, s)
  | TutorialSpec(name, s) => tutorial_fields(name, s)
  };

let seg_of_zipper = (z: Zipper.t): Base.segment =>
  Zipper.unselect_and_zip(~erase_buffer=true, z);

let check_field = ({label, root, zipper}: field): option(string) => {
  let persisted = persist_zipper(zipper);
  let code = persisted.backup_text;
  switch (Parser.to_zipper(~root, code)) {
  | None => Some(label ++ ": reparse FAILED (Parser.to_zipper => None)")
  | Some(z2) =>
    let code2 = PersistentZipper.to_string(z2);
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
         | CodeEx(_, s) => s.id
         | DrvEx(_, s)
         | DrvSpec(_, s) => s.id
         | ThmEx(_, s) => s.id
         | TutorialSpec(_, s) => s.id,
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
  List.filter_map(
    (e: Exercise.t) =>
      missing("exercise", Exercise.title_of(e), Exercise.id_of(e)),
    ExerciseSettings_base.exercises,
  )
  @ List.filter_map(
      (t: Tutorial.spec) => missing("tutorial", t.title, t.id),
      TutorialSettings_base.lessons,
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
