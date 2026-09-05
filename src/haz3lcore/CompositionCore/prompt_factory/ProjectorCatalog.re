/** Single source of truth for projector / livelit terminology in agent prompts.
    Keeps copy aligned with [[Language.ProjectorKind]] where possible. */
open Language;

let probe_tools_crossref =
  "Runtime probes are **refractors** (GUI overlays on bindings), not livelit projectors. "
  ++ "When probes are active, the agent view shows evaluated values inline as `≡`. "
  ++ "See the system prompt **Projectors and the agent view** for folds, statics overlays, "
  ++ "and livelit kinds (checkbox, slider, csv, …).";

let refractors_vs_syntax_sentence =
  "**Refractors** (Probe, Statics) are additive overlays on existing syntax; "
  ++ "**syntax projectors** / **livelits** (checkbox, slider, card, csv, text, livelit, fold) "
  ++ "replace or wrap pieces in the program structure for alternate editing UIs.";

let livelit_line = (k: ProjectorKind.t): option(string) =>
  switch (k) {
  /* Never chosen by a user: translation puts it there. */
  | FumolaPeek => None
  | Csv =>
    Some(
      "- **csv** — CSV **file import** UI. Attaches only when the definition is the **empty list** `[]`; after placement, import fills it with row/tuple syntax. Non-empty lists (e.g. `[1, 2, 3]`) **cannot** take this projector.",
    )
  | Card =>
    Some(
      "- **card** — **Playing-card** editor: a tuple `(Suit, Rank)` such as `(Hearts, Ace)` or a list of those tuples. **Not** for records, modules, or `{ let …; … }` blocks.",
    )
  | Checkbox => Some("- **checkbox** — Toggle UI for boolean literals.")
  | Slider => Some("- **slider** — Numeric literal editor (integer).")
  | SliderF =>
    Some("- **sliderf** — Numeric literal editor (floating-point).")
  | TextArea => Some("- **text** — Multiline string literal editor.")
  | Livelit =>
    Some(
      "- **livelit** — Custom livelit projector for structured literal editing.",
    )
  | Table =>
    Some(
      "- **table** — Spreadsheet-style editor for a **list of labeled tuples** sharing the same labels; the labels become column headers. Attaches off the **elaborated** form, so it can apply where the surface syntax alone is not yet a table.",
    )
  | Fold
  | Probe
  | Statics => None
  };

let uniq_livelit_lines: list(string) = {
  let (lines, _) =
    List.fold_left(
      ((acc_lines, seen), k) =>
        if (List.mem(k, seen)) {
          (acc_lines, seen);
        } else {
          switch (livelit_line(k)) {
          | Some(line) => ([line, ...acc_lines], [k, ...seen])
          | None => (acc_lines, [k, ...seen])
          };
        },
      ([], []),
      ProjectorKind.livelit_projectors,
    );
  List.rev(lines);
};

/** Lines to splice into [[CompositionPrompt]] (markdown-friendly). */
let blurb_for_composition_prompt: list(string) = {
  let livelit_block =
    ["### Livelits (menu projectors)"]
    @ uniq_livelit_lines
    @ [
      "",
      "These are alternative UIs for literals and structured values that may already appear in code.",
    ];
  [
    "## Projectors and the agent view",
    "",
    "### Fold (`⋱`)",
    "A **fold** is a collapsed definition in the GUI: the definition shows as `⋱` (not source text).",
    "Use `expand` / `collapse` (see **Paths** and **View Tools** above).",
    "",
    "### Runtime probe refractor",
    "The **probe** refractor attaches to a binding and shows evaluated values in the agent view (e.g. `expr ≡ value`).",
    "Tools: `place_probe`, `remove_probe`, `toggle_probe`.",
    "",
    "### Statics refractor (type overlay)",
    "The **statics** refractor shows a type ascription overlay in the GUI (offside type cell) for human clarity.",
    "It does not change program text; the agent’s printed context may not mirror the overlay unless printing is extended.",
    "",
  ]
  @ livelit_block
  @ [
    "",
    "### Syntax projector tools (Filbert)",
    "`place_syntax_projector(kind, paths)`, `remove_syntax_projector(paths)`, and `toggle_syntax_projector(kind, paths)` apply **syntax projector** UIs to the term selected by each path.",
    "`kind` must be a menu projector name (`fold`, `slider`, `sliderf`, `check`, `text`, `card`, `csv`, `table`, `livelit`) — not `probe` or `statics` (those use the probe/statics tools).",
    "",
    "**CRITICAL — livelits are not automatic:** Ordinary Hazel code (`let speed = 50`, `let is_active = true`, list literals, records) renders as **plain text** until you call **`place_syntax_projector`**.",
    "If the user asks how livelits work, or wants sliders/checkboxes/CSV editors/text boxes, you **must** call `place_syntax_projector` with the right `kind` on each binding path (e.g. `slider` on `\"speed\"`, `check` on `\"is_active\"`, `text` on `\"message\"`, `csv` only when the RHS is `[]`, `card` only on playing-card tuples/lists—see livelit lines above; nested paths like `\"config/volume\"`).",
    "**Never** describe interactive widgets as present based only on `insert_*`, `update_*`, or other edit tools — that is false unless the matching `place_syntax_projector` calls succeeded.",
    "",
  ]
  @ [refractors_vs_syntax_sentence, ""];
};

/** Short taxonomy for the compaction summarizer (3–6 lines). */
let blurb_for_compaction: list(string) = [
  "**Projectors**: **Folds** display as `⋱` (collapsed defs; expand/collapse in the full prompt).",
  "**Refractors** overlay bindings: **probes** show runtime values (`≡` in context); **statics** add a type overlay in the GUI.",
  "**Livelits** are alternate UIs for literals (checkbox, slider/sliderf, text, card, csv, table, livelit, plus fold in the menu).",
  "**Agent tools**: `place_syntax_projector` / `remove_syntax_projector` / `toggle_syntax_projector` (see full prompt).",
  "Full detail lives in Filbert’s live system prompt **Projectors and the agent view** section (this excerpt may be truncated).",
];
