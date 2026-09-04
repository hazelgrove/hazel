open Util_web;

let place_probe_description =
  {|
Places probes on the bindings at the given paths to observe their runtime values.
When a probe is active, the agent view shows evaluated values inline (e.g., `expr ≡ value`).
This is a powerful debugging and verification tool — use it to check whether code behaves as expected.
If the probed definition is collapsed (⋱), it will be auto-expanded so results are visible.

Parameters:
paths: list(string) — paths to the bindings to probe

Example:
Given the program:
```
let double = fun x -> x * 2 in
let result = double(5) in
result
```
Calling place_probe(paths=["result"]) shows:
```
let double = ⋱ in
let result = double(5) in     ≡ 10
result
```
The ≡ 10 shows that `result` evaluates to 10.

|}
  ++ "\n\n"
  ++ ProjectorCatalog.probe_tools_crossref;

let place_probe: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("place_probe")),
        ("description", `String(place_probe_description)),
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
                        "The paths to the bindings to probe for runtime values.",
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

let remove_probe_description =
  {|
Removes probes from the bindings at the given paths.
Use this to clean up probes after verifying code behavior.

Parameters:
paths: list(string) — paths to the bindings to remove probes from

Example:
Calling remove_probe(paths=["result"]) removes the probe from "result",
and the view no longer shows evaluated values for that binding.

|}
  ++ "\n\n"
  ++ ProjectorCatalog.probe_tools_crossref;

let remove_probe: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("remove_probe")),
        ("description", `String(remove_probe_description)),
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
                        "The paths to the bindings to remove probes from.",
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

let toggle_probe_description =
  {|
Toggles probes on or off for the bindings at the given paths.
If a probe is already placed, it is removed. If no probe exists, one is placed.
Collapsed definitions are auto-expanded when a probe is placed.

Parameters:
paths: list(string) — paths to the bindings to toggle probes on

Example:
If "result" has no probe, calling toggle_probe(paths=["result"]) places one.
Calling toggle_probe(paths=["result"]) again removes it.

|}
  ++ "\n\n"
  ++ ProjectorCatalog.probe_tools_crossref;

let toggle_probe: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("toggle_probe")),
        ("description", `String(toggle_probe_description)),
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
                        "The paths to the bindings to toggle probes on.",
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
