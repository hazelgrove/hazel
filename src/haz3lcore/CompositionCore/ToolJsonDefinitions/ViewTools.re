open Util;

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

let modify_view_description = {|
The ONLY way to change which bindings you can see. A fast relevance model opens the bindings your intent needs (plus their parents).
Ask for EVERYTHING you need in one call: name the bindings, behaviours or errors. Mentioned binding paths are opened exactly.
Calls add to what is open; pass replace=true only when switching to an unrelated part of the program. Things you already see stay open.

Parameters:
intent: string — e.g. "fix the off-by-one in paginate's last page; also show its callers and render_page"
replace: bool (optional, default false) — true resets the view to exactly this intent's bindings

Returns one line: `open: … · added: …`; the program view in the context shows the code.
|};

let modify_view: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("modify_view")),
        ("description", `String(modify_view_description)),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "intent",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "What you are about to do, naming concrete behaviours, bindings, or errors.",
                      ),
                    ),
                  ]),
                ),
                (
                  "replace",
                  `Assoc([
                    ("type", `String("boolean")),
                    (
                      "description",
                      `String(
                        "Default false (add to the view). True only for a new, unrelated focus.",
                      ),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("intent")])),
          ]),
        ),
      ]),
    ),
  ]);
