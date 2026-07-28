/* HTML/MVU example apps, shipped as documentation slides.
 *
 * Each is a working Elm-style app: the program ends in a
 * `(init, update, view, subs)` tuple carrying an HTML projector, so the
 * slide opens with the app already running.
 *
 * Generated from the .hz files in hazel-programs/html-examples via
 * `hazel slide-encode`. Those files are the editable source — edit one
 * WITHOUT re-encoding and the slide silently keeps the old program.
 *
 * The "MVU / " prefix collapses these into a nested dropdown, matching
 * how the "B2T2 / ..." and "Derivations / ..." slides are grouped. */
let all_slides: list((string, Haz3lcore.PersistentSegment.t)) =
  [
    MvuCounter.out,
    MvuTimer.out,
    MvuTodoList.out,
    MvuFirefly.out,
    MvuCropPlotter.out,
    MvuTicTacToe.out,
    MvuGameOfLife.out,
    MvuSeedCatalog.out,
    MvuHarvestStreak.out,
    MvuNutrientRotation.out,
  ]
  |> List.map(((name, seg)) => ("MVU / " ++ name, seg));
