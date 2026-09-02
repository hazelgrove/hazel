/* HTML/MVU example apps, shipped as documentation slides.
 *
 * Each is a working Elm-style app: the program ends in a
 * `^^html_sidebar((init, update, view, subs))` tuple, so the slide opens
 * with the app already running and docked in the projector panel, leaving
 * a chip at the code site. The `_sidebar` suffix is part of the invoke
 * token, so the placement lives in the TEXT — a slide stores both a zipper
 * and its backup_text, and if placement were patched into the zipper
 * afterwards the two would disagree.
 *
 * The committed .hz files in hazel-programs/docs/mvu ARE the slides:
 * embedded at compile time, parsed at load (FastParse, MarkerParse
 * fallback). There is no encoding step — edit an .hz and the slide
 * changes. Test_FastParseCorpus keeps every .hz on the fast path.
 *
 * The "MVU / " prefix collapses these into a nested dropdown, matching
 * how the "B2T2 / ..." and "Derivations / ..." slides are grouped. */
let all_slides: list((string, Haz3lcore.PersistentZipper.t)) =
  [
    ("Counter", [%blob "mvu-counter.hz"]),
    ("Watering Timer", [%blob "timer.hz"]),
    ("Planting List", [%blob "todo-list.hz"]),
    ("Firefly", [%blob "keyboard-game.hz"]),
    ("Crop Plotter", [%blob "crop-plotter.hz"]),
    ("Sprouts and Shrooms", [%blob "tictactoe.hz"]),
    ("Garden of Life", [%blob "gameoflife.hz"]),
    ("Seed Catalog", [%blob "seed-catalog.hz"]),
    ("Harvest Ledger", [%blob "harvest-streak.hz"]),
    ("Nutrient Tracker", [%blob "nutrient-rotation.hz"]),
  ]
  |> List.map(((name, text)) =>
       ("MVU / " ++ name, Haz3lcore.PersistentZipper.of_slide_text(text))
     );
