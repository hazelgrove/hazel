/* Charting, shipped as documentation slides.
 *
 * There is no chart type built into Hazel and no chart projector. A chart
 * is an ordinary HTML value, drawn with the generic `Node`/`Create` escape
 * hatches (which render SVG), and the charting vocabulary — scales, ticks,
 * axes, marks, legends — is Hazel source the reader can fork.
 *
 * "Charts" carries the library plus a gallery of all five kinds, viewed
 * through `^^probe_html`: the rich probe that renders an expression's
 * evaluated HTML.
 *
 * "Linked Views" is an app: a chart with event handlers is an input as well
 * as an output, and a second chart derived inside `view` tracks the model as
 * you drag. It carries the Svg and Chart modules trimmed to exactly what a
 * pie needs — no Scale module at all — so the copy here stays small enough
 * to read rather than being a second full library to keep in sync.
 *
 * "Calculator" is the probe's other advantage: it is ADDITIVE, so the html
 * it draws can rewrite its own source while that source stays on screen and
 * stays typeable. Every key press commits another `|>` stage, so the
 * pipeline that accumulates is the calculation. It descends from an OCaml
 * CalculatorRenderer that probed an Int and rewrote it to `n + 3`; here the
 * calculator is a Hazel program and nothing about it is built in.
 *
 * The committed .hz files in hazel-programs/charts ARE the slides:
 * embedded at compile time, parsed at load. There is no encoding step —
 * edit an .hz and the slide changes. */
let all_slides: list((string, Haz3lcore.PersistentZipper.t)) =
  [
    ("Charts", [%blob "charts.hz"]),
    ("Linked Views", [%blob "linked.hz"]),
    ("Calculator", [%blob "calculator.hz"]),
  ]
  |> List.map(((name, text)) =>
       ("Charts / " ++ name, Haz3lcore.PersistentZipper.of_slide_text(text))
     );
