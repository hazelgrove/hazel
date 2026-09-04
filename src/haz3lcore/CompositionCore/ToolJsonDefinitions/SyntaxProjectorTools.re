open Util_web;

let allowed_kinds_line =
  "kind must be one of: fold, slider, sliderf, check, text, card, table, csv, livelit (same as the editor projector menu). "
  ++ "Do not use probe or statics here — use place_probe / place_statics instead.";

let catalog_crossref =
  "See the system prompt **Projectors and the agent view** for what each projector expects. "
  ++ "**csv** applies only when the definition is the empty list `[]`. **card** applies to playing-card syntax `(Suit, Rank)` or lists of those—not arbitrary lists or record-like blocks.";

let place_syntax_projector_description =
  {|
Wraps the expression at each path in a **syntax projector** (livelit-style GUI: slider, checkbox, fold, etc.).
This changes program structure like the editor’s projector menu; it is not the same as probe/statics refractors.

Plain literals in the source (e.g. `50`, `true`, a string) do **not** show sliders or other widgets until this tool (or the human projector menu) applies a projector. Do not tell the user interactive livelits are active based only on `insert_*`, `update_*`, or other edit tools.

If the binding already has that projector kind, the path is left unchanged (idempotent).

Parameters:
- kind: string — projector name (see allowed kinds below).
- paths: list(string) — **binding paths** (pattern names): top-level `let` names like `speed` or `map`, or nested paths like `outer/inner`. The tool wraps each binding's **definition** (right-hand side), not the whole `let … in …`. Do **not** pass pretty-printed expressions, type-applied text (e.g. containing `@<Int>`), or fragments from statics overlays — those will not resolve and the tool will fail.

|}
  ++ allowed_kinds_line
  ++ "\n\n"
  ++ catalog_crossref;

let place_syntax_projector: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("place_syntax_projector")),
        ("description", `String(place_syntax_projector_description)),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "kind",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "Syntax projector kind: fold, slider, sliderf, check, text, card, table, csv, or livelit.",
                      ),
                    ),
                  ]),
                ),
                (
                  "paths",
                  `Assoc([
                    ("type", `String("array")),
                    (
                      "description",
                      `String(
                        "Paths to terms to wrap with the given projector (e.g. binding whose body is a literal).",
                      ),
                    ),
                    ("items", `Assoc([("type", `String("string"))])),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("kind"), `String("paths")])),
          ]),
        ),
      ]),
    ),
  ]);

let remove_syntax_projector_description =
  {|
Removes a **syntax projector** (livelit-style) at each path, restoring the underlying expression.
Does nothing at a path if there is no projector on the selected term.

Parameters:
paths: list(string) — paths to nodes where a projector should be removed

|}
  ++ catalog_crossref;

let remove_syntax_projector: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("remove_syntax_projector")),
        ("description", `String(remove_syntax_projector_description)),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "paths",
                  `Assoc([
                    ("type", `String("array")),
                    (
                      "description",
                      `String(
                        "Paths where a syntax projector should be removed (unproject).",
                      ),
                    ),
                    ("items", `Assoc([("type", `String("string"))])),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("paths")])),
          ]),
        ),
      ]),
    ),
  ]);

let toggle_syntax_projector_description =
  {|
Toggles a **syntax projector** at each path (same as the editor menu): if that kind is already active, removes it; otherwise places or switches to it.

Parameters:
- kind: string — projector name (see allowed kinds).
- paths: list(string) — paths to nodes.

|}
  ++ allowed_kinds_line
  ++ "\n\n"
  ++ catalog_crossref;

let toggle_syntax_projector: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("toggle_syntax_projector")),
        ("description", `String(toggle_syntax_projector_description)),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "kind",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "Syntax projector kind: fold, slider, sliderf, check, text, card, table, csv, or livelit.",
                      ),
                    ),
                  ]),
                ),
                (
                  "paths",
                  `Assoc([
                    ("type", `String("array")),
                    (
                      "description",
                      `String("Paths to terms to toggle projection on."),
                    ),
                    ("items", `Assoc([("type", `String("string"))])),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("kind"), `String("paths")])),
          ]),
        ),
      ]),
    ),
  ]);
