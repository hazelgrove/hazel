open Util;

let initialize_description = {|
Replaces the entire program with the given code.
Use this when the program is EMPTY — just `?` or a standalone expression with no let/type/module bindings.
When empty, you MUST use initialize to write code; update_definition and update_body will fail (they require an existing binding).
Once the program has let/type/module bindings, use the other edit tools instead.

Parameters:
code: string — the complete new program

Example:
Current program: `?`
Calling initialize(code="let x = 42 in\nlet y = x + 1 in\ny") produces:
```
let x = 42 in
let y = x + 1 in
y
```
|};

let initialize: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("initialize")),
        ("description", `String(initialize_description)),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "code",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String("The new code to replace the definition with."),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("code")])),
          ]),
        ),
      ]),
    ),
  ]);

let update_definition_description = {|
Replaces the definition (the right-hand side of `=`, before `in`) of the binding at the given path.
This overwrites the ENTIRE definition — including any nested let bindings within it.
Works for both let bindings and module bindings (e.g. path "M" for module M = { ... }).

Parameters:
path: string — slash-delimited path to the binding (e.g. "b", "M", or "outer/inner")
code: string — the new definition code

Example:
Given the program:
```
let a = ⋱ in
let b = "hello" in
let c : Int = ⋱ in
?
```
Calling update_definition(path="b", code="\"world\"") produces:
```
let a = ⋱ in
let b = "world" in
let c : Int = ⋱ in
?
```
Note: Only the definition changes. The pattern, body, and surrounding bindings are untouched.
|};

let update_definition: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("update_definition")),
        ("description", `String(update_definition_description)),
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
                        "Slash-delimited path to the node to update (e.g. \"b\" or \"a/b\").",
                      ),
                    ),
                  ]),
                ),
                (
                  "code",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String("The new code to replace the definition with."),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("path"), `String("code")])),
          ]),
        ),
      ]),
    ),
  ]);

let update_body_description = {|
Replaces the body (everything after `in`) of the binding at the given path.
The body is the rest of the program that follows this binding.
Works for both let bindings and module bindings (e.g. path "M" for module M = { ... }).

Parameters:
path: string — slash-delimited path to the binding whose body to replace
code: string — the new body code

Example:
Given the program:
```
let a = ⋱ in
let b = "hello" in
let c : Int = ⋱ in
?
```
Calling update_body(path="b", code="a * a") produces:
```
let a = ⋱ in
let b = "hello" in
a * a
```
Note: The body of "b" was `let c : Int = ⋱ in ?`. It is now replaced entirely with `a * a`.
The definition of "b" ("hello") is unchanged.
|};
let update_body: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("update_body")),
        ("description", `String(update_body_description)),
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
                        "Slash-delimited path to the node whose body should be replaced.",
                      ),
                    ),
                  ]),
                ),
                (
                  "code",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String("The new code to replace the body with."),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("path"), `String("code")])),
          ]),
        ),
      ]),
    ),
  ]);

let update_pattern_description = {|
Renames or changes the pattern (left-hand side of `=`) of the binding at the given path.
Automatically updates all use sites of the variable throughout the program.

Parameters:
path: string — slash-delimited path to the binding to rename
code: string — the new pattern (may include type annotation)

Example:
Given the program:
```
let a = ⋱ in
let b = "hello" in
let c = b ++ " world" in
c
```
Calling update_pattern(path="b", code="greeting : String") produces:
```
let a = ⋱ in
let greeting : String = "hello" in
let c = greeting ++ " world" in
c
```
Note: All references to "b" in the body are automatically renamed to "greeting".
|};

let update_pattern: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("update_pattern")),
        ("description", `String(update_pattern_description)),
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
                        "Slash-delimited path to the node whose pattern should change.",
                      ),
                    ),
                  ]),
                ),
                (
                  "code",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String("The new pattern to assign to the node."),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("path"), `String("code")])),
          ]),
        ),
      ]),
    ),
  ]);

let update_binding_clause_description = {|
Replaces the entire binding clause (from `let`/`type`/`module` through `in`, inclusive) at the given path.
This changes the pattern, definition, and delimiters — but NOT the body after the final `in`.
The code you provide should end with `in` (not include a final body expression).
You can introduce multiple bindings in one call (e.g., `let x = 1 in let y = 2 in`).
Works for let, type, and module bindings (e.g. path "M" for module M = { ... }).

To update both the binding clause AND the body, call this tool followed by update_body.

Parameters:
path: string — slash-delimited path to the binding to replace
code: string — the new binding clause(s), ending with `in`

Example:
Given the program:
```
let a = ⋱ in
let b = "hello" in
let c : Int = ⋱ in
?
```
Calling update_binding_clause(path="b", code="let b : Int = 42 in let d = b + 1 in") produces:
```
let a = ⋱ in
let b : Int = 42 in
let d = b + 1 in
let c : Int = ⋱ in
?
```
Note: The body (everything after the original `in` of "b") is preserved — `let c` and `?` remain.
|};

let update_binding_clause: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("update_binding_clause")),
        ("description", `String(update_binding_clause_description)),
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
                        "Slash-delimited path to the node whose binding clause should change.",
                      ),
                    ),
                  ]),
                ),
                (
                  "code",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String("The new code to replace the expression with."),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("path"), `String("code")])),
          ]),
        ),
      ]),
    ),
  ]);

let delete_binding_clause_description = {|
Removes the entire binding (let...=...in, type...=...in, or module...=...in) at the given path.
The body that followed the binding is preserved and moves up.
Works for let, type, and module bindings (e.g. path "M" for module M = { ... }).

Parameters:
path: string — slash-delimited path to the binding to remove

Example:
Given the program:
```
let a = ⋱ in
let b = "hello" in
let c : Int = ⋱ in
?
```
Calling delete_binding_clause(path="b") produces:
```
let a = ⋱ in
let c : Int = ⋱ in
?
```
Note: The binding `let b = "hello" in` is removed. Its body (`let c ... ?`) is preserved.
|};

let delete_binding_clause: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("delete_binding_clause")),
        ("description", `String(delete_binding_clause_description)),
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
                        "Slash-delimited path to the binding clause that should be deleted.",
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

let delete_body_description = {|
Clears the body (everything after `in`) of the binding at the given path, replacing it with a hole (`?`).

Parameters:
path: string — slash-delimited path to the binding whose body to clear

Example:
Given the program:
```
let a = ⋱ in
let b = "hello" in
let c : Int = ⋱ in
some_expression
```
Calling delete_body(path="b") produces:
```
let a = ⋱ in
let b = "hello" in
?
```
Note: Everything after `in` for binding "b" (including `let c` and `some_expression`) is replaced with `?`.
|};

let delete_body: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("delete_body")),
        ("description", `String(delete_body_description)),
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
                        "Slash-delimited path to the node whose body should be deleted.",
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

let update_type_annotation_description = {|
Description:
Updates the type annotation of a binding's pattern.
This only applies to bindings that have a type annotation (e.g. `let x : Int = ...`).
It changes just the type part, leaving the variable name and definition unchanged.

Parameters:
path: string — slash-delimited path to the binding whose type annotation should change
code: string — new type annotation

Example(s):
Given path "x" and the program:
```
let x : Int = 5 in
x + 1
```
Calling update_type_annotation(path="x", code="Float") would result in:
```
let x : Float = 5 in
x + 1
```
Note: If the binding has no type annotation (e.g. `let x = 5 in ...`), this tool will fail.
|};

let update_type_annotation: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("update_type_annotation")),
        ("description", `String(update_type_annotation_description)),
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
                        "Slash-delimited path to the binding whose type annotation should change.",
                      ),
                    ),
                  ]),
                ),
                (
                  "code",
                  `Assoc([
                    ("type", `String("string")),
                    ("description", `String("The new type annotation.")),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("path"), `String("code")])),
          ]),
        ),
      ]),
    ),
  ]);

let insert_after_description = {|
Inserts a new binding immediately after the binding at the given path.
The inserted code becomes part of the program between the target binding and its original body.

Parameters:
path: string — slash-delimited path to the binding after which to insert
code: string — the code to insert (typically a let...in or type...in binding)

Example:
Given the program:
```
let a = 10 in
let b = "hello" in
?
```
Calling insert_after(path="a", code="let doubled = a * 2 in") produces:
```
let a = 10 in
let doubled = a * 2 in
let b = "hello" in
?
```
Note: The new binding is inserted between "a" and "b". The rest of the program is preserved.
|};

let insert_after: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("insert_after")),
        ("description", `String(insert_after_description)),
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
                        "Slash-delimited path to the node after which the code should be inserted.",
                      ),
                    ),
                  ]),
                ),
                (
                  "code",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "The code to insert after the referenced expression.",
                      ),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("path"), `String("code")])),
          ]),
        ),
      ]),
    ),
  ]);

let insert_before_description = {|
Inserts a new binding immediately before the binding at the given path.

Parameters:
path: string — slash-delimited path to the binding before which to insert
code: string — the code to insert (typically a let...in or type...in binding)

Example:
Given the program:
```
let a = ⋱ in
let b = "hello" in
?
```
Calling insert_before(path="b", code="let prefix = \"world\" in") produces:
```
let a = ⋱ in
let prefix = "world" in
let b = "hello" in
?
```
Note: The new binding is inserted between "a" and "b". The rest of the program is preserved.
|};

let insert_before: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("insert_before")),
        ("description", `String(insert_before_description)),
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
                        "Slash-delimited path to the node before which the code should be inserted.",
                      ),
                    ),
                  ]),
                ),
                (
                  "code",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "The code to insert before the referenced expression.",
                      ),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("path"), `String("code")])),
          ]),
        ),
      ]),
    ),
  ]);

/* === Selector-based edit tools === */

let selector_update_description = {|
Description:
Replaces the focused subtree matched by a selector expression with new code.
Selectors are pattern-based expressions that address any part of the AST.
The selector must contain exactly one `%` (focus marker) indicating which subtree to replace.
The replacement code is parsed according to the sort of the focused node (expression, pattern, or type).
When multiple locations match, selector edits are cursor-relative: matches are
ordered by source position, the edit targets the first match after the shared
editor cursor, and matching wraps to the top when the cursor is after all
matches. The shared cursor moves to the selected match before/after the edit.

Parameters:
selector: string — selector expression with exactly one `%` focus
code: string — replacement code

Example(s):
Given the program:
```
let x = 42 in x + 1
```
Calling selector_update(selector="let x = %", code="99") results in:
```
let x = 99 in x + 1
```

Given the program:
```
let f = fun x -> if x > 0 then x else 0 in f 5
```
Calling selector_update(selector="\... if _... else %", code="1") results in:
```
let f = fun x -> if x > 0 then x else 1 in f 5
```

Cross-sort example — updating a type annotation:
Given: `let x : Int = 42 in x`
Calling selector_update(selector="let x : %", code="Bool") results in:
```
let x : Bool = 42 in x
```
|};

let selector_update: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("selector_update")),
        ("description", `String(selector_update_description)),
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
                        "Selector expression with a % focus (e.g. \"let x = %\", \"\\... if _... else %\").",
                      ),
                    ),
                  ]),
                ),
                (
                  "code",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "The replacement code for the focused subtree.",
                      ),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("selector"), `String("code")])),
          ]),
        ),
      ]),
    ),
  ]);

let selector_delete_description = {|
Description:
Replaces the focused subtree matched by a selector expression with a hole.
The hole type matches the sort of the focused node: expression hole (?), pattern hole, or type hole.
If multiple locations match, deletes the active cursor-relative match: the first
match after the shared editor cursor, wrapping to the top when needed.

Parameters:
selector: string — selector expression with exactly one `%` focus

Example(s):
Given the program:
```
let x = 42 in x + 1
```
Calling selector_delete(selector="let x = %") results in:
```
let x = ? in x + 1
```
|};

let selector_delete: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("selector_delete")),
        ("description", `String(selector_delete_description)),
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
                        "Selector expression with a % focus (e.g. \"let x = %\", \"if _... else %\").",
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

let selector_insert_after_description = {|
Description:
Inserts code after the anchor matched by a selector expression.
The `%` in the selector marks the anchor point (the existing binding/item), and
new code is inserted immediately after it.
Works with let-bindings, module items, list elements, tuple elements, and case arms.
If multiple anchors match, inserts after the active cursor-relative match: the
first match after the shared editor cursor, wrapping to the top when needed.

Parameters:
selector: string — selector expression with `%` marking the anchor
code: string — code to insert after the anchor

Example(s):
Given the program:
```
let x = 1 in x + 1
```
Calling selector_insert_after(selector="% let x", code="let y = 2") results in:
```
let x = 1 in let y = 2 in x + 1
```
|};

let selector_insert_after: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("selector_insert_after")),
        ("description", `String(selector_insert_after_description)),
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
                        "Selector expression with % marking the insertion anchor.",
                      ),
                    ),
                  ]),
                ),
                (
                  "code",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String("The code to insert after the anchor."),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("selector"), `String("code")])),
          ]),
        ),
      ]),
    ),
  ]);

let selector_insert_before_description = {|
Description:
Inserts code before the anchor matched by a selector expression.
The `%` in the selector marks the anchor point (the existing binding/item), and
new code is inserted immediately before it.
Works with let-bindings, module items, list elements, tuple elements, and case arms.
If multiple anchors match, inserts before the active cursor-relative match: the
first match after the shared editor cursor, wrapping to the top when needed.

Parameters:
selector: string — selector expression with `%` marking the anchor
code: string — code to insert before the anchor

Example(s):
Given the program:
```
let x = 1 in x + 1
```
Calling selector_insert_before(selector="% let x", code="let y = 2") results in:
```
let y = 2 in let x = 1 in x + 1
```
|};

let selector_insert_before: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("selector_insert_before")),
        ("description", `String(selector_insert_before_description)),
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
                        "Selector expression with % marking the insertion anchor.",
                      ),
                    ),
                  ]),
                ),
                (
                  "code",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String("The code to insert before the anchor."),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("selector"), `String("code")])),
          ]),
        ),
      ]),
    ),
  ]);
