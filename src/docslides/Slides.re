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
    /* Fumola through livelits, as a series in successive complexity: the map first, then
       what values are, then
       how they are named, then the state that makes editing incremental,
       then the runtime itself -- and finally the examples from the Fumola
       repo that have something to run. Each stands alone, so each opens with
       the same brief reminder of the four livelits. */
    ("Fumola (Livelits) / 0. Big picture", [%blob "fumola-0-overview.hz"]),
    ("Fumola (Livelits) / 1. Values", [%blob "fumola-1-values.hz"]),
    ("Fumola (Livelits) / 2. Symbols", [%blob "fumola-2-symbols.hz"]),
    ("Fumola (Livelits) / 3. Cells", [%blob "fumola-3-cells.hz"]),
    ("Fumola (Livelits) / 4. Thunks", [%blob "fumola-4-thunks.hz"]),
    ("Fumola (Livelits) / 5. Input", [%blob "fumola-5-input.hz"]),
    ("Fumola (Livelits) / 6. Runtimes", [%blob "fumola-6-runtimes.hz"]),
    ("Fumola (Livelits) / 7. Gcd", [%blob "fumola-7-gcd.hz"]),
    ("Fumola (Livelits) / 8. Delayed put", [%blob "fumola-8-delayed-put.hz"]),
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
