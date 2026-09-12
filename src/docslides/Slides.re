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
    /* Fumola, integrated as a sort in the tile grammar. Named apart from the
       livelit integration -- "Fumola (Livelits)" on fumola-livelit-mvp -- so
       that the two can sit in one deck when they meet. */
    ("Fumola (Tiles) / Overview", [%blob "fumola-tiles-overview.hz"]),
    (
      "Fumola (Tiles) / Instance and mode",
      [%blob "fumola-tiles-instance.hz"],
    ),
    ("Fumola (Tiles) / A level tree", [%blob "fumola-tiles-leveltree.hz"]),
    ("Fumola (Tiles) / Hazel inside", [%blob "fumola-tiles-hazel-inside.hz"]),
    ("Fumola (Tiles) / The round trip", [%blob "fumola-tiles-roundtrip.hz"]),
  ]
  |> List.map(((name, text)) =>
       (name, Haz3lcore.PersistentZipper.of_slide_text(text))
     );
