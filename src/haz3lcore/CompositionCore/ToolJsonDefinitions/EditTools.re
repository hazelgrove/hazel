open Util_web;

let update_definition_description = {|
Replaces the definition (the right-hand side of `=`, before `in`) of the binding at the given path.
This overwrites the ENTIRE definition — including any nested let bindings within it.
Works for both let bindings and module bindings (e.g. path "M" for module M = { ... }).

Parameters:
path: string — slash-delimited path to the binding (e.g. "b", "M", or "outer/inner"). Inner defs use "outer/inner". A function-definition binding `let f(x, y) = ...` is addressed by its bare name: path "f" (parameters are not part of the path). If the same name appears more than once in the sibling chain (shadowing), the bare path is ambiguous and the tool errors, listing disambiguated forms; retry with "name#k" (k-th occurrence in program order, 1-based, e.g. "b#2"), or use a nested path when one binding sits inside another's definition.
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

Syntax projectors (livelits): to keep a widget when overwriting the definition, include projector concrete syntax in `code`: `^^kind(expression)`. Examples: `^^slider(60)`, `^^sliderf(3.14)`, `^^check(true)`, `^^text("hello")`, `^^csv([])` (empty list only; import CSV in the UI), `^^card((Hearts, Ace))` (playing-card tuple or list of tuples—not records). Without `^^`, the term is usually a plain literal and the projector is not preserved—then use `place_syntax_projector` if needed (same term-shape rules as the editor menu).
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
                        "Slash-delimited path (e.g. \"b\", \"utils/helper\"). Duplicate names are ambiguous — disambiguate with name#k (1-based, program order); nested defs use outer/inner.",
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
path: string — slash-delimited path to the binding whose body to replace. Nested defs need ancestors (e.g. "wrap/is_odd"). Duplicate sibling names are ambiguous — disambiguate with "name#k" (k-th occurrence in program order, 1-based).
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
                        "Slash-delimited path; outer/inner for nested defs. Duplicate chain names are ambiguous — use name#k (1-based, program order).",
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
path: string — slash-delimited path to the binding to rename. Nested defs: use outer/inner. Duplicate sibling names are ambiguous — disambiguate with "name#k" (k-th occurrence in program order, 1-based).
code: string — the new pattern (may include type annotation)

For a function-definition binding `let f(x, y) = ...` the path is just "f" but the pattern is the whole head: rename with code "g(x, y)" (or rename a param with "f(x, z)"); call sites and param uses update automatically.

The rename is rejected (with an explanatory error) if:
- the new name already occurs as a binder or variable reference within this binding's scope (it could capture existing references), or
- the number of bound names changes (e.g. tuple pattern (x, y) → (a, b, c)), since old→new use-site mapping would be ambiguous, or
- rewriting the use sites would introduce new static errors.

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

The path is resolved against the program **as it exists before** this edit (the `code` argument does not change path lookup). Use the same paths you would get from reading the current buffer, not from hypothetical replacement text.
Bindings nested **inside another binding's definition** use paths like `outer/inner`. A chain `let a = ... in let b = ... in body` treats `a` and `b` as separate top-level path segments (`a`, `b`), not `a/b`, because `b` is in the outer body, not inside `a`'s definition.

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

If `path` is omitted, the code is inserted after the entire program (at the end). This is how you initialize an empty program: with the program at just `?`, call `insert_after(code="let x = 1 in")` (no path) to write the first binding.

Parameters:
path: string (optional) — slash-delimited path to the binding after which to insert. Omit to insert after the whole program.
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

Initialization example (empty program):
Current program: `?`
Calling insert_after(code="let x = 42 in") produces:
```
let x = 42 in
?
```
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
                        "Slash-delimited path to the node after which the code should be inserted. Omit to insert after the entire program (initializes an empty program).",
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
            ("required", `List([`String("code")])),
          ]),
        ),
      ]),
    ),
  ]);

let insert_before_description = {|
Inserts a new binding immediately before the binding at the given path.

If `path` is omitted, the code is inserted before the entire program (at the beginning). This is how you initialize an empty program: with the program at just `?`, call `insert_before(code="let x = 1 in")` (no path) to write the first binding.

Parameters:
path: string (optional) — slash-delimited path to the binding before which to insert. Omit to insert before the whole program.
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

Initialization example (empty program):
Current program: `?`
Calling insert_before(code="let x = 42 in") produces:
```
let x = 42 in
?
```
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
                        "Slash-delimited path to the node before which the code should be inserted. Omit to insert before the entire program (initializes an empty program).",
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
            ("required", `List([`String("code")])),
          ]),
        ),
      ]),
    ),
  ]);
