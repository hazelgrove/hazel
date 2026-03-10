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
