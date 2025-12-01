open Util;

let expand_description = {|
Description:
Expands the specified nodes'/variables' defintions.

Parameters:
paths: list(string) — the paths to the nodes/variables to expand

Example(s):
Given the program state:
```
let a = ⋱ in
let b = ⋱ in
let c : Int = ⋱ in
?
```
Calling expand(paths = ["a", "b"]) would result in the program:
```
let a = 4 + 5 in
let b = "hello, world!" in
let c : Int = ⋱ in
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
                      `String("The paths to the nodes/variables to expand."),
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
Description:
Collapses the specified nodes'/variables' defintions.

Parameters:
paths: list(string) — the paths to the nodes/variables to collapse

Example(s):
Given the program state:
```
let a = 4 + 5 in
let b = "hello, world!" in
let c : Int = ⋱ in
?
```
Calling collapse(paths = ["a", "b"]) would result in the program:
```
let a = ⋱ in
let b = ⋱ in
let c : Int = ⋱ in
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
                        "The paths to the nodes/variables to collapse.",
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
