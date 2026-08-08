/* HTML/MVU example apps, shipped as documentation slides.
 *
 * Each is a working Elm-style app ending in ^^html_sidebar((init,
 * update, view, subs)), so the slide opens with the app already
 * running and docked.
 *
 * The committed .hz files in hazel-programs/docs/mvu ARE the slides:
 * their text is embedded here at compile time and parsed at load
 * (FastParse, typing-parser fallback), so there is no encode step to
 * go stale. Human indentation in the .hz is flattened at load
 * (PersistentSegment.of_text) because Hazel computes indentation at
 * layout time. Test_FastParseCorpus keeps every .hz on the fast path.
 *
 * The "MVU / " prefix collapses these into a nested dropdown, matching
 * how the "B2T2 / ..." slides are grouped. */
let all_slides: list((string, Haz3lcore.PersistentSegment.t)) =
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
       ("MVU / " ++ name, Haz3lcore.PersistentSegment.of_text(text))
     );
