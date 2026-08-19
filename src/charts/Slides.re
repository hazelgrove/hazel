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
 * "Table Explorer" is an app built by a function that is polymorphic in its
 * table's row type, instantiated over two B2T2 tables. That is the shape a
 * literal 4-tuple cannot take: its `view` fixes one row type, so the same
 * feature written that way is one whole app per schema.
 *
 * "Steps" is the pair of things a probe can do that the projector cannot.
 * It records MANY samples: its first probe sits inside a recursion, so the
 * navigator scrubs the run step by step, where a projector shows one live
 * value — the latest — and would have replaced the call to show it. And it
 * is ADDITIVE: the html at the bottom rewrites its own source when clicked,
 * with that source still on screen and still typeable.
 *
 * The committed .hz files in hazel-programs/charts ARE the slides:
 * embedded at compile time, parsed at load. There is no encoding step —
 * edit an .hz and the slide changes. */
let all_slides: list((string, Haz3lcore.PersistentZipper.t)) =
  [
    ("Charts", [%blob "charts.hz"]),
    ("Linked Views", [%blob "linked.hz"]),
    ("Table Explorer", [%blob "b2t2-explorer.hz"]),
    ("Steps", [%blob "steps.hz"]),
  ]
  |> List.map(((name, text)) =>
       ("Charts / " ++ name, Haz3lcore.PersistentZipper.of_slide_text(text))
     );
