/* User-defined-livelit example slides, shipped as documentation.
 * The committed .hz files in hazel-programs/docs/livelits ARE the
 * slides (embedded at compile time, parsed at load) — the ^^livelit
 * triggers are written in the text itself. */
let all_slides: list((string, Haz3lcore.PersistentZipper.t)) =
  (
    [
      ("Define a Slider", [%blob "defined-slider.hz"]),
      ("Emotion", [%blob "emotion.hz"]),
      ("Color Picker", [%blob "color-picker.hz"]),
      ("Graph Editor", [%blob "graph-editor.hz"]),
      ("Graph Round Trip", [%blob "graph-roundtrip.hz"]),
      ("Curve Editor", [%blob "curve-editor.hz"]),
      ("Logic Circuit", [%blob "logic-circuit.hz"]),
      ("Task DAG", [%blob "task-dag.hz"]),
    ]
    |> List.map(((name, text)) => ("Livelits / " ++ name, text))
  )
  @ (
    /* Hazel as computational glue: Automerge documents in and out */
    [
      ("TLDraw to Petrinaut", [%blob "tldraw-petrinaut.hz"]),
      ("TLDraw Shadow", [%blob "tldraw-shadow.hz"]),
    ]
    |> List.map(((name, text)) => ("Glue / " ++ name, text))
  )
  |> List.map(((name, text)) =>
       (name, Haz3lcore.PersistentZipper.of_slide_text(text))
     );
