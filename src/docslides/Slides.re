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
    /* Blackboard: an experiment embedding the core logic of the
       Blackboard proof assistant as its own sort. */
    ("Blackboard / 0. Overview", [%blob "overview.hz"]),
    ("Blackboard / 1. Terms", [%blob "terms.hz"]),
    ("Blackboard / 2. Signatures", [%blob "signatures.hz"]),
    ("Blackboard / 3. Checking", [%blob "checking.hz"]),
    ("Blackboard / 4. Metatheory", [%blob "metatheory.hz"]),
    ("Blackboard / 5. Case Studies", [%blob "case-studies.hz"]),
  ]
  |> List.map(((name, text)) =>
       (name, Haz3lcore.PersistentZipper.of_slide_text(text))
     );
