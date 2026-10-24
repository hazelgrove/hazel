open Util;

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
let a = ⋯ in
let b = "hello" in
let c : Int = ⋯ in
?
```
Calling update_definition(path="b", code="\"world\"") produces:
```
let a = ⋯ in
let b = "world" in
let c : Int = ⋯ in
?
```
Note: Only the definition changes. The pattern, body, and surrounding bindings are untouched.

Syntax projectors (livelits): to keep a widget when overwriting the definition, include projector concrete syntax in `code`: `^^kind(expression)`. Examples: `^^slider(60)`, `^^sliderf(3.14)`, `^^check(true)`, `^^text("hello")`, `^^csv([])` (empty list only; import CSV in the UI), `^^card((Hearts, Ace))` (playing-card tuple or list of tuples—not records). Without `^^`, the term is usually a plain literal and the projector is not preserved—then use `place_syntax_projector` if needed (same term-shape rules as the editor menu).

Module members: paths descend into module literals — `M/helper` (including type members: `M/Model`). Edit ONE member this way instead of re-emitting the whole module; every edit tool works at member paths.
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
                        "Slash-delimited path (e.g. \"b\", \"M/helper\"). Nested defs use outer/inner; module members use owner/member; duplicates use name#k (1-based, program order).",
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
A module MEMBER (path "M/helper") ends at its `;`: there, the body is the member's definition past any `fun x ->` heads.

Parameters:
path: string — slash-delimited path to the binding whose body to replace. Nested defs need ancestors (e.g. "wrap/is_odd"). Duplicate sibling names are ambiguous — disambiguate with "name#k" (k-th occurrence in program order, 1-based).
code: string — the new body code

Example:
Given the program:
```
let a = ⋯ in
let b = "hello" in
let c : Int = ⋯ in
?
```
Calling update_body(path="b", code="a * a") produces:
```
let a = ⋯ in
let b = "hello" in
a * a
```
Note: The body of "b" was `let c : Int = ⋯ in ?`. It is now replaced entirely with `a * a`.
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
                        "Slash-delimited path. Nested defs use outer/inner; module members use owner/member; duplicates use name#k (1-based, program order).",
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
Renames, or changes the pattern (left-hand side of `=`) of, the binding at the given path.
- `code` a single new name renames: the binder the path names, and every use of it, `M.member` uses included. A type annotation stays.
- `code` the old pattern with only names changed renames each of them: for `let f(x, y) = ...` (path "f"), "g(x, y)" renames f, "f(x, z)" renames the parameter y.
- Any other `code` replaces the pattern as written, uses untouched; if that leaves uses unbound, the static-error check refuses it.
A rename is refused if it would change what some name refers to (capture).

Parameters:
path: string — slash-delimited path to the binding. Nested defs: use outer/inner. Duplicate sibling names are ambiguous — disambiguate with "name#k" (k-th occurrence in program order, 1-based).
code: string — the new name, or the new pattern

Example:
Given the program:
```
let a = ⋯ in
let b : String = "hello" in
let c = b ++ " world" in
c
```
Calling update_pattern(path="b", code="greeting") produces:
```
let a = ⋯ in
let greeting : String = "hello" in
let c = greeting ++ " world" in
c
```
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
                        "Slash-delimited path to the node whose pattern should change. Nested defs use outer/inner; module members use owner/member; duplicates use name#k (1-based, program order).",
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
At a module MEMBER path, replaces the whole member — write it without a trailing `;` (e.g. code="let w = 8").
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
let a = ⋯ in
let b = "hello" in
let c : Int = ⋯ in
?
```
Calling update_binding_clause(path="b", code="let b : Int = 42 in let d = b + 1 in") produces:
```
let a = ⋯ in
let b : Int = 42 in
let d = b + 1 in
let c : Int = ⋯ in
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
                        "Slash-delimited path to the node whose binding clause should change. Nested defs use outer/inner; module members use owner/member; duplicates use name#k (1-based, program order).",
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
Works for let, type, and module bindings (e.g. path "M" for module M = { ... }), a single module MEMBER (e.g. "M/helper"), and a test (by its hint string, or "test#k").
Refused while its names are still used: the error lists where. Delete or change those uses in the same response.

Parameters:
path: string — slash-delimited path to the binding to remove

Example:
Given the program:
```
let a = ⋯ in
let b = "hello" in
let c : Int = ⋯ in
?
```
Calling delete_binding_clause(path="b") produces:
```
let a = ⋯ in
let c : Int = ⋯ in
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
                        "Slash-delimited path to the binding clause that should be deleted. Nested defs use outer/inner; module members use owner/member; duplicates use name#k (1-based, program order).",
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
At a module MEMBER path, clears the member's body (its definition past any `fun x ->` heads).

Parameters:
path: string — slash-delimited path to the binding whose body to clear

Example:
Given the program:
```
let a = ⋯ in
let b = "hello" in
let c : Int = ⋯ in
some_expression
```
Calling delete_body(path="b") produces:
```
let a = ⋯ in
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
                        "Slash-delimited path to the node whose body should be deleted. Nested defs use outer/inner; duplicates use name#k (1-based, program order). Module members have no body.",
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
At a module MEMBER path (e.g. "M/helper"), inserts a new member after that member — write the member without a trailing `;` (e.g. code="let helper2 = 5"); the separator is added automatically.

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
                        "Slash-delimited path to the node after which the code should be inserted. Omit to insert after the entire program (initializes an empty program). Nested defs use outer/inner; module members use owner/member; duplicates use name#k (1-based, program order).",
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
Inserts a new binding immediately before the binding at the given path. At a module MEMBER path, inserts a new member before it (write the member without a trailing `;`).

If `path` is omitted, the code is inserted before the entire program (at the beginning). This is how you initialize an empty program: with the program at just `?`, call `insert_before(code="let x = 1 in")` (no path) to write the first binding.

Parameters:
path: string (optional) — slash-delimited path to the binding before which to insert. Omit to insert before the whole program.
code: string — the code to insert (typically a let...in or type...in binding)

Example:
Given the program:
```
let a = ⋯ in
let b = "hello" in
?
```
Calling insert_before(path="b", code="let prefix = \"world\" in") produces:
```
let a = ⋯ in
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
                        "Slash-delimited path to the node before which the code should be inserted. Omit to insert before the entire program (initializes an empty program). Nested defs use outer/inner; module members use owner/member; duplicates use name#k (1-based, program order).",
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

let insert_test_description = {|
Adds a test, `test <code> end`, after the binding at the given path and after any tests that already directly follow it, so repeated calls keep source order and the tests stay together.
If `path` is omitted, the test goes after the program's last binding or test, before its final expression.
At a module MEMBER path (e.g. "M/helper"), the test is added as a member of that module.
A function's unit tests go right after it (path = the function); tests of several functions together go at the end (no path).
`code` may be a whole test, hint included: `hint "empty list" test sum([]) == 0 end`. A hinted test's path is its hint in quotes ("\"empty list\""), any test's "test#k".

Parameters:
path: string (optional) — slash-delimited path to the binding the test follows. Omit to add it after the program's last item.
code: string — the test's boolean expression (`test` and `end` are added), or a whole test

Example:
Given the program:
```
let double = fun x -> 2 * x in
test double(1) == 2 end;
double(5)
```
Calling insert_test(path="double", code="double(0) == 0") produces:
```
let double = fun x -> 2 * x in
test double(1) == 2 end;
test double(0) == 0 end;
double(5)
```
|};

let insert_test: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("insert_test")),
        ("description", `String(insert_test_description)),
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
                        "Slash-delimited path to the binding the test follows. Omit to add it after the program's last item. Nested defs use outer/inner; module members use owner/member; duplicates use name#k (1-based, program order).",
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
                        "The test's boolean expression, without `test` and `end`.",
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

let move_description = {|
Moves the entry at `target` to a new place, keeping its code, probes and identity. Give exactly one of `before`, `after`, `first_in`, `last_in`.
`before` / `after`: next to the entry at that path. `first_in` / `last_in`: first or last among the entries a module holds (its members) or a value holds (the local definitions in its body); an empty string is the program's top level.
Moving between a module and a let chain converts `;` and `in`.

Example:
```
let area = fun s -> ⋯ in
let total = fun xs -> ⋯ in
module Geo = { let perimeter = ⋯; } in
?
```
move(target="area", last_in="Geo") produces:
```
let total = fun xs -> ⋯ in
module Geo = { let perimeter = ⋯; let area = fun s -> ⋯; } in
?
```
|};

let move: API.Json.t = {
  let path = desc =>
    `Assoc([("type", `String("string")), ("description", `String(desc))]);
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("move")),
        ("description", `String(move_description)),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                ("target", path("Path of the entry to move.")),
                ("before", path("Path of the entry it goes before.")),
                ("after", path("Path of the entry it goes after.")),
                (
                  "first_in",
                  path(
                    "Path of the module or value it goes first in; \"\" for the top level.",
                  ),
                ),
                (
                  "last_in",
                  path(
                    "Path of the module or value it goes last in; \"\" for the top level.",
                  ),
                ),
              ]),
            ),
            ("required", `List([`String("target")])),
          ]),
        ),
      ]),
    ),
  ]);
};
