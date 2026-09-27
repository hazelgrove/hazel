/* User-defined-livelit example slides, shipped as documentation.
 * The committed .hz files in hazel-programs/docs/livelits ARE the
 * slides (embedded at compile time, parsed at load) — the ^^livelit
 * triggers are written in the text itself. */
let all_slides: list((string, Haz3lcore.PersistentZipper.t)) =
  [
    ("Overview", [%blob "overview.hz"]),
    ("Color (Figure 3)", [%blob "color-fig3.hz"]),
    ("Define a Slider", [%blob "defined-slider.hz"]),
    ("Parameters", [%blob "parameters.hz"]),
    ("Emotion", [%blob "emotion.hz"]),
    ("Color Picker", [%blob "color-picker.hz"]),
    ("Functional Expansion", [%blob "expansion.hz"]),
    ("Parameterized Slider (two SpliceRefs)", [%blob "splices-mvp.hz"]),
    ("Tree Care", [%blob "tree-care.hz"]),
    ("Timings", [%blob "timings.hz"]),
    ("JavaScript (advanced)", [%blob "javascript-advanced.hz"]),
    ("Dynamic Row or Column", [%blob "splice-row.hz"]),
    /* A subfolder, Livelits / Hygiene: a slide per part, so each loads
       only the livelit it shows (hazel-programs/docs/livelits/hygiene). */
    ("Hygiene / About", [%blob "about.hz"]),
    ("Hygiene / 1. Capture Avoidance", [%blob "capture.hz"]),
    ("Hygiene / 2. Context Independence", [%blob "context.hz"]),
    ("Hygiene / 3. Generated Binders", [%blob "generated-binders.hz"]),
    ("Hygiene / 4. Abs", [%blob "abs.hz"]),
    /* Livelits / Expansion Type Errors: where a mismatch with
       Expansion is reported, and how, for each kind of expand
       (hazel-programs/docs/livelits/expansion-errors). */
    ("Expansion Type Errors / About", [%blob "errors-about.hz"]),
    (
      "Expansion Type Errors / Functional 1. The Result",
      [%blob "functional-result.hz"],
    ),
    (
      "Expansion Type Errors / Functional 2. Inside the Body",
      [%blob "functional-body.hz"],
    ),
    (
      "Expansion Type Errors / Functional 3. The Model",
      [%blob "functional-model.hz"],
    ),
    (
      "Expansion Type Errors / Macro 1. The Result",
      [%blob "macro-result.hz"],
    ),
    (
      "Expansion Type Errors / Macro 2. A Parameter",
      [%blob "macro-parameter.hz"],
    ),
    (
      "Expansion Type Errors / Macro 3. Not a Function of the Splices",
      [%blob "macro-not-a-function.hz"],
    ),
    /* Livelits / Either, Two Versions: one livelit whose expansion type
       varies by use, written with Expansion = ? and with a type
       parameter (hazel-programs/docs/livelits/either). */
    ("Either, Two Versions / About", [%blob "either-about.hz"]),
    (
      "Either, Two Versions / 1. Unknown Expansion",
      [%blob "either-unknown.hz"],
    ),
    ("Either, Two Versions / 2. Type Parameter", [%blob "either-typed.hz"]),
  ]
  |> List.map(((name, text)) =>
       (
         "Livelits / " ++ name,
         Haz3lcore.PersistentZipper.of_slide_text(text),
       )
     );
