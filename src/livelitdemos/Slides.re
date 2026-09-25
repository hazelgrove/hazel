/* User-defined-livelit example slides, shipped as documentation.
 * The committed .hz files in hazel-programs/docs/livelits ARE the
 * slides (embedded at compile time, parsed at load) — the ^^livelit
 * triggers are written in the text itself. */
let all_slides: list((string, Haz3lcore.PersistentZipper.t)) =
  [
    ("Overview", [%blob "overview.hz"]),
    ("Define a Slider", [%blob "defined-slider.hz"]),
    ("The Expansion", [%blob "expansion.hz"]),
    ("SpliceRef, MVP", [%blob "splices-mvp.hz"]),
    ("Emotion", [%blob "emotion.hz"]),
    ("Color Picker", [%blob "color-picker.hz"]),
    ("Tree Care", [%blob "tree-care.hz"]),
    /* The same widget with its layout walk in Fumola. Beside the
       baseline rather than replacing it, so both run on one build and
       a comparison is a same-session measurement -- which is also why
       it stays registered while it is broken: it draws no tree yet,
       for Adapton/fumola#142, and the slide says so at its head. */
    ("Tree Care (Fumola)", [%blob "tree-care-fumola.hz"]),
    ("Timings", [%blob "timings.hz"]),
    ("JavaScript (advanced)", [%blob "javascript-advanced.hz"]),
  ]
  |> List.map(((name, text)) =>
       (
         "Livelits / " ++ name,
         Haz3lcore.PersistentZipper.of_slide_text(text),
       )
     );
