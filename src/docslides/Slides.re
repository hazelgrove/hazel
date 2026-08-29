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
    /* perf/outline stress corpora (hazel-programs/mega): realistic
       module-heavy programs in the thousands of lines */
    ("Mega 1k", [%blob "mega-1k.hz"]),
    ("Mega 2k", [%blob "mega-2k.hz"]),
    ("Mega 4k", [%blob "mega-4k.hz"]),
  ]
  |> List.map(((name, text)) =>
       (name, Haz3lcore.PersistentZipper.of_slide_text(text))
     );

/* MOD-ROOTED variants of the mega corpus (plans/mod-root.md): the top
   level is a module body (`;`-separated items), editor root = Mod. */
let mod_slides: list((string, Haz3lcore.PersistentZipper.t)) =
  [
    ("Mega-Mod 1k", [%blob "mega-mod-1k.hz"]),
    ("Mega-Mod 2k", [%blob "mega-mod-2k.hz"]),
    ("Mega-Mod 4k", [%blob "mega-mod-4k.hz"]),
  ]
  |> List.map(((name, text)) =>
       (name, Haz3lcore.PersistentZipper.of_slide_text(text))
     );
