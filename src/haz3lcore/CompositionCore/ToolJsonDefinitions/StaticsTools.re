open Util_web;

let place_statics_description = {|
Places the **statics** refractor on the bindings at the given paths: a GUI type-ascription overlay (offside type cell) for human-readable types on expressions.
This does not change program text. If a runtime probe is on the same binding, it is replaced by statics (same transition as the editor’s statics toggle).
Collapsed definitions are auto-expanded when statics are placed so overlays are visible.

Parameters:
paths: list(string) — paths to the bindings

See the system prompt **Projectors and the agent view** for how statics relate to probes and livelits.
|};

let place_statics: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("place_statics")),
        ("description", `String(place_statics_description)),
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
                        "Paths to bindings where the statics (type overlay) refractor should be shown.",
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

let remove_statics_description = {|
Removes **only** the statics refractor from the bindings at the given paths.
Runtime probes on the same binding are left unchanged (unlike remove_probe).

Parameters:
paths: list(string) — paths to the bindings to clear statics overlays from
|};

let remove_statics: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("remove_statics")),
        ("description", `String(remove_statics_description)),
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
                        "Paths to bindings to remove statics overlays from.",
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

let toggle_statics_description = {|
Toggles the statics refractor on or off for the bindings at the given paths (same behavior as the editor statics toggle).
Collapsed definitions are auto-expanded when statics end up enabled.

Parameters:
paths: list(string) — paths to the bindings to toggle
|};

let toggle_statics: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("toggle_statics")),
        ("description", `String(toggle_statics_description)),
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
                        "Paths to bindings to toggle statics overlays on.",
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
