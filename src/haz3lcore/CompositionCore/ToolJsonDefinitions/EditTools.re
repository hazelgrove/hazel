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

/* === Selector-based edit tools === */

let selector_update_description = {|
Description:
Replaces the focused subtree matched by a selector expression with new code.
Selectors are pattern-based expressions that address any part of the AST.
The selector must contain exactly one `%` (focus marker) indicating which subtree to replace.
The replacement code is parsed according to the sort of the focused node (expression, pattern, or type).

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

let overwrite_description = {|
Description:
Replaces the focused subtree matched by a selector expression with new code.
The `$` token in `code` stands for the originally selected subtree.
The new code is parsed at the right sort and spliced into the parent context.
Use `$` to express insert-before / insert-after patterns, as well as
in-place wrapping of the original subtree.

The selector must contain exactly one `%` (focus marker) identifying the
target. The `code` may contain zero or more `$` placeholders. When `$` is
absent, overwrite behaves like a plain replace. When `$` appears, the original
subtree is substituted at each `$` position (re-parsed for fresh ids on the
second and later occurrences).

Splicing rules (only when the target is a *direct* child of the parent):
- target is a list element and parsed code is a tuple → splice tuple elements
  into the list at the target's index;
- target is a tuple element and parsed code is a tuple → splice tuple elements
  into the tuple at the target's index;
- target is a module item and parsed code is a `;`-sequence → splice items into
  the module.
Otherwise the target is replaced by the parsed (substituted) code.

Parameters:
selector: string — selector expression with exactly one `%` focus
code: string — replacement code, with optional `$` to mark the original

Example(s):
Plain replace (no `$`):
Given `let x = 1 in test x == 1 end; x`, calling
overwrite(selector="let x = _ in #0", code="test x == 2 end") results in
```
let x = 1 in test x == 2 end; x
```

Append in a `;`-sequence (`$` first, then new code):
Given `let x = 1 in test x == 1 end; test x > 0 end; x`, calling
overwrite(selector="let x = _ in #1 #0", code="$; test x < 10 end") results in
```
let x = 1 in test x == 1 end; test x > 0 end; test x < 10 end; x
```

Prepend in a `;`-sequence (new code first, then `$`):
Given `let x = 1 in test x == 1 end; x`, calling
overwrite(selector="let x = _ in #0", code="test x < 0 end; $") results in
```
let x = 1 in test x < 0 end; test x == 1 end; x
```

Wrap an expression (use `$` inside larger expression):
Given `let x = 1 in x`, calling
overwrite(selector="let x = %", code="$ + 1") results in
```
let x = 1 + 1 in x
```

Insert before a let-chain (wrap the whole `let` with another `let`):
Given `let x = 1 in x`, calling
overwrite(selector="% let x", code="let y = 0 in $") results in
```
let y = 0 in let x = 1 in x
```

List/tuple splice with `,`:
Given `let xs = [1, 2, 3] in xs`, calling
overwrite(selector="xs/ #1", code="$, 99") results in
```
let xs = [1, 2, 99, 3] in xs
```
Calling overwrite(selector="xs/ #1", code="99, $") results in
```
let xs = [1, 99, 2, 3] in xs
```

Limitation: `$` inside a string literal in `code` is also substituted.
|};

let overwrite: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("overwrite")),
        ("description", `String(overwrite_description)),
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
                        "Selector expression with exactly one % focus (e.g. \"let x = %\", \"\\... test %\").",
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
                        "The new code. May contain $ tokens that stand for the original focused subtree.",
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
