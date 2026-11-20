/* Documentation reference slides: the committed .hz files in
 * hazel-programs/docs/reference ARE the slides — embedded at compile
 * time, parsed at load (FastParse, MarkerParse fallback). Holes:
 * ? = explicit hole tile, ¿ = implicit (Grout). Probe/statics pins
 * are ^^probe/^^statics triggers in the text (^^probe_table selects
 * the table renderer). */
let all_slides: list((string, Haz3lcore.PersistentZipper.t)) =
  [
    ("Basic Reference", [%blob "basic-reference.hz"]),
    ("Projectors", [%blob "projectors.hz"]),
    ("ADTs", [%blob "adts.hz"]),
    ("Tuples", [%blob "tuples.hz"]),
    ("Modules", [%blob "modules.hz"]),
    ("Tables", [%blob "tables.hz"]),
    ("Polymorphism", [%blob "polymorphism.hz"]),
    ("Cards", [%blob "cards.hz"]),
    ("Probes", [%blob "probes.hz"]),
    ("Livelits / Builtins", [%blob "livelits-builtins.hz"]),
    ("Prover / Theorems", [%blob "prover-theorems.hz"]),
    ("Prover / The Stepper", [%blob "prover-stepper.hz"]),
    ("Prover / Variables and Lemmas", [%blob "prover-lemmas.hz"]),
    ("Prover / Cases and Induction", [%blob "prover-induction.hz"]),
    ("Prover / Side Conditions", [%blob "prover-side-conditions.hz"]),
    ("Prover / Every Step", [%blob "prover-reference.hz"]),
    /* Refactoring (hazel-programs/docs/refactoring): a Start Here hub,
     * then one example-heavy slide per family */
    ("Refactoring / Start Here", [%blob "refactoring-start-here.hz"]),
    (
      "Refactoring / Extract + Inline",
      [%blob "refactoring-extract-inline.hz"],
    ),
    ("Refactoring / Moving Definitions", [%blob "refactoring-moving.hz"]),
    ("Refactoring / Cases + Ifs", [%blob "refactoring-cases-ifs.hz"]),
    (
      "Refactoring / Functions + Tuples",
      [%blob "refactoring-functions-tuples.hz"],
    ),
    ("Refactoring / Evaluation Steps", [%blob "refactoring-stepping.hz"]),
    ("Refactoring / Types", [%blob "refactoring-types.hz"]),
    ("Refactoring / Drag Tour", [%blob "refactoring-drag-tour.hz"]),
  ]
  |> List.map(~f=((name, text)) =>
       (name, Haz3lcore.PersistentZipper.of_slide_text(text))
     );
