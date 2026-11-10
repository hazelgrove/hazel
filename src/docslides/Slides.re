/* Documentation reference slides: the committed .hz files in
 * hazel-programs/docs/reference ARE the slides — embedded at compile
 * time, parsed at load (FastParse, MarkerParse fallback). Holes:
 * ? = explicit hole tile, ¿ = implicit (Grout). Probe/statics pins
 * are ^^probe/^^statics triggers in the text (^^probe_table selects
 * the table renderer). */
/* BRANCH (exolivelits): the exolivelit demo slides ship instead of the
 * standard reference set, which stays commented out here. */
let all_slides: list((string, Haz3lcore.PersistentZipper.t)) =
  [
    ("exolivelits", [%blob "exolivelits.hz"]),
    ("nool", [%blob "nool.hz"]),
    ("petrinaut", [%blob "petrinaut.hz"]),
    // ("Basic Reference", [%blob "basic-reference.hz"]),
    // ("Projectors", [%blob "projectors.hz"]),
    // ("ADTs", [%blob "adts.hz"]),
    // ("Tuples", [%blob "tuples.hz"]),
    // ("Modules", [%blob "modules.hz"]),
    // ("Tables", [%blob "tables.hz"]),
    // ("Polymorphism", [%blob "polymorphism.hz"]),
    // ("Cards", [%blob "cards.hz"]),
    // ("Probes", [%blob "probes.hz"]),
    // ("Livelits / Builtins", [%blob "livelits-builtins.hz"]),
  ]
  |> List.map(((name, text)) =>
       (name, Haz3lcore.PersistentZipper.of_slide_text(text))
     );
