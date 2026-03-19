open Util;

let view_entire_definition: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("view_entire_definition")),
        (
          "description",
          `String(
            "Displays the entire definition of the current node in the AST, leaving no child/sub definitions abstracted away.",
          ),
        ),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            ("properties", `Assoc([])),
            ("required", `List([])),
          ]),
        ),
      ]),
    ),
  ]);

let view_context: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("view_context")),
        (
          "description",
          `String(
            "Displays all the variables in the typing context/scope at the current let expression in the AST.",
          ),
        ),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            ("properties", `Assoc([])),
            ("required", `List([])),
          ]),
        ),
      ]),
    ),
  ]);

let show_references_description = {|
Description:
Displays all the variables referenced in the body of the current node in the AST.
This action permits the use of the navigation tool "go_to_reference(index)" to be called using
the index displayed from this action as the argument for the "index" parameter.

Parameters:
None

Example(s):
(Note: We show the entire program, without any collapsed child/sibling definitions
here for a wholistic context and clarity)
The current node is "b" and the sketch is:
```
let a = 200 in
let b = 10 in
let c : Int =
b *
let x = 20 in x
in
let d = c + 10 in
d * b
```
Calling show_references() on this program would result in the following message to be returned:
"References to "b":
[
(Index: 0, Definition of "c"): {
```
let c : Int =
b *
let x = ⋱ in x
in
⋱
```},
(Index: 1, Body of "d"): {
```
let d = ⋱ in
d * b
```
}
]
"
|};

let show_references: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("show_references")),
        ("description", `String(show_references_description)),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            ("properties", `Assoc([])),
            ("required", `List([])),
          ]),
        ),
      ]),
    ),
  ]);
