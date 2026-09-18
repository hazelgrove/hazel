/* Constellation canvas demo programs, shipped as documentation slides.
 *
 * Each exercises a specific canvas behavior (see the header comment in
 * each .hz). The committed .hz files in hazel-programs/constellation ARE
 * the slides: embedded at compile time, parsed at load (FastParse,
 * MarkerParse fallback). Edit an .hz and the slide changes.
 * Test_FastParseCorpus keeps every .hz on the fast path.
 *
 * The "Constellation / " prefix collapses these into a nested dropdown,
 * matching the "MVU / ..." and "B2T2 / ..." groups. */
let all_slides: list((string, Haz3lcore.PersistentZipper.t)) =
  [
    ("Counter App", [%blob "counter-app.hz"]),
    ("Many Callers", [%blob "many-callers.hz"]),
    ("Scorekeeper Module", [%blob "module-scorekeeper.hz"]),
    ("Nested Modules", [%blob "modules-demo.hz"]),
    ("Pipeline Depth", [%blob "pipeline-depth.hz"]),
    ("View Widgets", [%blob "view-widgets.hz"]),
  ]
  |> List.map(((name, text)) =>
       (
         "Constellation / " ++ name,
         Haz3lcore.PersistentZipper.of_slide_text(text),
       )
     );
