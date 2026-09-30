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
      ("Dispatch", [%blob "dispatch.hz"]),
      ("Cold Chain", [%blob "cold-chain.hz"]),
      ("Range Picker", [%blob "range-picker.hz"]),
      ("Dial", [%blob "dial.hz"]),
    ]
    |> List.map(((name, text)) => ("Livelits / " ++ name, text))
  )
  @ (
    /* type cards on the canvas: every type has a view; the app is a
       value in the program and each step is one call of a frame function */
    [
      ("Tic-Tac-Toe", [%blob "tic-tac-toe.hz"]),
      ("Thermostat", [%blob "thermostat.hz"]),
      ("Trip Planner", [%blob "trip-planner.hz"]),
      ("Greenhouse", [%blob "greenhouse.hz"]),
    ]
    |> List.map(((name, text)) => ("Type Cards / " ++ name, text))
  )
  |> List.map(((name, text)) =>
       (name, Haz3lcore.PersistentZipper.of_slide_text(text))
     );
