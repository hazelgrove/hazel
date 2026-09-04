open Util_web;

let expand_description = {|
Reveals the definitions of collapsed bindings so their code is visible.
Use this to inspect code before editing. Expand only what you need to keep context manageable.
Works for both let bindings and module bindings (e.g. module M = { ... }).

Parameters:
paths: list(string) — paths to the bindings to expand (e.g. "a", "M", "outer/inner")

Example:
Given:
```
let a = ⋱ in
let b = ⋱ in
?
```
Calling expand(paths=["a", "b"]) reveals:
```
let a = 4 + 5 in
let b = "hello" in
?
```
|};

let expand: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("expand")),
        ("description", `String(expand_description)),
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
                        "The paths to the bindings to expand (let or module).",
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

let collapse_description = {|
Hides the definitions of expanded bindings, showing ⋱ instead.
Use this after editing to reduce clutter and keep context focused.
Works for both let bindings and module bindings (e.g. module M = { ... }).

Parameters:
paths: list(string) — paths to the bindings to collapse (e.g. "a", "M")

Example:
Given:
```
let a = 4 + 5 in
let b = "hello" in
?
```
Calling collapse(paths=["a", "b"]) hides them:
```
let a = ⋱ in
let b = ⋱ in
?
```
|};

let collapse: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("collapse")),
        ("description", `String(collapse_description)),
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
                        "The paths to the bindings to collapse (let or module).",
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
