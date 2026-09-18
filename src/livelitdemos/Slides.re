/* User-defined-livelit example slides, shipped as documentation.
 * The committed .hz files in hazel-programs/docs/livelits ARE the
 * slides (embedded at compile time, parsed at load) — the ^^livelit
 * triggers are written in the text itself. */
let all_slides: list((string, Haz3lcore.PersistentZipper.t)) =
  [
    ("Define a Slider", [%blob "defined-slider.hz"]),
    ("Emotion", [%blob "emotion.hz"]),
    ("Color Picker", [%blob "color-picker.hz"]),
  ]
  |> List.map(((name, text)) =>
       (
         "Livelits / " ++ name,
         Haz3lcore.PersistentZipper.of_slide_text(text),
       )
     );
