open Util;

let get_syntax_description = {|
Description:
Returns the pretty-printed code at the specified path without modifying the program.
Useful for inspecting the current definition, body, or any addressable binding.

Parameters:
path: string — slash-delimited path to the node (e.g. "a", "a/inner", "M/x", "#0")

Example(s):
Given the program:
```
let a = 1 + 2 in
let b =
  let inner = a * 3 in
  inner + 1
in
b
```
Calling get_syntax(path="b") returns:
```
let b =
  let inner = a * 3 in
  inner + 1
in
```
Calling get_syntax(path="b/inner") returns:
```
let inner = a * 3 in
```
|};

let get_syntax: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("get_syntax")),
        ("description", `String(get_syntax_description)),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "path",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "Slash-delimited path to the node (e.g. \"a\", \"a/inner\", \"M/x\").",
                      ),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("path")])),
          ]),
        ),
      ]),
    ),
  ]);

let get_statics_description = {|
Description:
Returns static type information for the binding at the specified path.
Shows the expected (analytic) type, synthesized type, error status, and any static errors.

Parameters:
path: string — slash-delimited path to the node

Example(s):
Given the program:
```
let x : Int = 1 + 2 in
let y = x + "hello" in
y
```
Calling get_statics(path="x") returns type info showing:
- Expected type: Int (from annotation)
- Synthesized type: Int
- Status: no errors

Calling get_statics(path="y") returns type info showing:
- Expected type: Unknown
- Synthesized type: Unknown (due to error)
- Errors: type inconsistency in definition
|};

let get_statics: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("get_statics")),
        ("description", `String(get_statics_description)),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "path",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "Slash-delimited path to the node (e.g. \"x\", \"M/x\").",
                      ),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("path")])),
          ]),
        ),
      ]),
    ),
  ]);

let get_context_description = {|
Description:
Returns the typing context (in-scope bindings) at the specified path.
Shows variables, type aliases, and constructors available at that program point.

Parameters:
path: string — slash-delimited path to the node

Example(s):
Given the program:
```
type Color = Red + Green + Blue in
let x : Int = 5 in
let y = x + 1 in
y
```
Calling get_context(path="y") returns:
Variables:
  x : Int
Type aliases:
  Color = Red + Green + Blue
Constructors:
  Red : Color
  Green : Color
  Blue : Color
|};

let get_context: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("get_context")),
        ("description", `String(get_context_description)),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "path",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "Slash-delimited path to the node (e.g. \"y\", \"M/x\").",
                      ),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("path")])),
          ]),
        ),
      ]),
    ),
  ]);

let select_description = {|
Description:
Uses a selector expression to find and return matching subtrees of the program.
Selectors are concise, surface-oriented patterns that address Hazel syntax.

Core operators:
- `_` matches one syntactic slot
- `_...` matches zero or more slots along the current spine
- `\` (backslash) is descendant search — match P, then find Q inside
- `*` marks the focused subtree to return

Common patterns:
- `let x = *` — select x's definition
- `let x _... in *` — select x's body
- `if *` — select the if condition
- `if _ then *` — select the then branch
- `if _... else *` — select the else branch
- `| Foo => *` — select arm body for constructor Foo
- `case *` — select the scrutinee
- `* let x` — select the whole let-binding expression
- `A/B/x = *` — binder chain: navigate into A's def, then B's, select x's def

Parameters:
selector: string — the selector expression

Example(s):
Given:
```
let f = fun x -> if x > 0 then x else 0 in f 5
```
- select(selector="let f = \ if *") returns: "x > 0"
- select(selector="let f = \ if _ then *") returns: "x"
- select(selector="let f = \ if _... else *") returns: "0"
|};

let select: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("select")),
        ("description", `String(select_description)),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "selector",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "Selector expression (e.g. \"let x = *\", \"if _... else *\", \"A/B/x = *\").",
                      ),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("selector")])),
          ]),
        ),
      ]),
    ),
  ]);

let get_completeness_description = {|
Description:
Reports whether the program has any unfilled holes.
Counts empty holes in expressions, patterns, and types.

Parameters:
None

Example(s):
For a complete program like `let x = 42 in x + 1`:
  Returns: "Complete: no unfilled holes."

For a program with holes like `let x = ? in let y : ? = x in y`:
  Returns: "Incomplete: 2 unfilled hole(s) (1 expression, 1 type)."
|};

let get_completeness: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("get_completeness")),
        ("description", `String(get_completeness_description)),
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
