open Util;

let initialize_description = {|
Description:
Overwrites the program with the given code.
This may *only* ever be called on a program that has no let/type alias expressions.
This tool is only meant for this special case.
It may never be used once a program has let/type alias expressions.

Parameters:
code: string — new code to replace the program with

Example(s):
(Example 1)
The current program is:
```
?
```
Calling initialize(code="let a = 3 in
a * 2") would result in the program
```
let a = 3 in a * 2
```

(Example 2)
The current program is:
```
5 * 10
```
Calling initialize(code="let a  = 5
in let b = 10
in a * b") would result in the program
```
let a  = 5 in let b = 10 in a * b
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
Description:
Updates the definition of the node identified by the provided path.

Parameters:
path: string — slash-delimited path to the node to update (e.g. "b" or "bindings/b")
code: string — new definition

Example(s):
Given path "b" and the sketch:
```
let a = ⋱ in
let b = "hello, world" in
let c : Int = ⋱ in
?
```
Calling update_definition(path="b", code=""my new string"") will result in the sketch:
```
let a = ⋱ in
let b = "my new string" in
let c : Int = ⋱ in
?
```
Note(s):
This overwrites the ENTIRE definition of the targeted node. For example, if the path points to "b" and the sketch is:
```
let a = ⋱ in
let b =
    let x = ⋱ in
in
let c : Int = ⋱ in
?
```
Then calling update_definition(path="b", code="\"my new string\"") will again result in the sketch:
```
let a = ⋱ in
let b = "my new string" in
let c : Int = ⋱ in
?
```
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
Description:
Replaces the body of the node located at the provided path.

Parameters:
path: string — slash-delimited path to the node whose body should be replaced
code: string — new body

Example(s):
Given path "b" and the sketch:
```
let a = ⋱ in
let b = "hello, world!" in
let c : Int = ⋱ in
?
```
Calling update_body(path="b", code="a * a") will result in the program
```
let a = ⋱ in
let b = "my new string" in
a * a
```
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
Description:
Updates/renames the pattern of the node identified by the provided path.
A unique perk of this tool is that it will also update all use sites of the variable in the program.
If the pattern is a tuple, or some other higher-order pattern, it will recursively find
the atomic variables within that pattern, and update all use sites of those variables if and only if
the number of old and new variables are the same (this is only a requirement for this feature to work,
but you may very well change the pattern however you'd like to achieve your desired outcome!).

Parameters:
path: string — slash-delimited path to the node whose pattern should change
code: string — new pattern to assign

Example(s):
Given path "b" and the sketch:
```
let a = ⋱ in
let b = "hello, world" in
let c : Int = ⋱ in
?
```
Calling update_pattern(path="b", code="s : String") would result in the sketch:
```
let a = ⋱ in
let s : String = "hello, world" in
let c : Int = ⋱ in
?
```
*Note: If there were any references to "b" in the body of "b"'s variable definition
(such as in the definition of "c"), they would be updated to "s" as well.
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
Description:
Updates the pattern, definition, and enclosing delimiters of the node located via the provided path (everything exclusive of the body).
eg. calling update_binding_clause for the path to `let x = 3 in x` will overwrite "let x = 3 in".
It is important to note that this does NOT update the body of the node. If you wish to update the
binding along with the body, you should call this tool along with update_body, sequentially. This also means
the code argument you pass here should not contain a final body.
(Eg. ```let x = 3 in x``` would be bad, but ```let x = 3 in``` would be good, so would ```let x = 3 in let y = 4 in```).

Parameters:
path: string — slash-delimited path to the node whose binding clause should change
code: string — new expression (which may contain multiple expressions; see example below for more information)

Example(s):
Given path "b" and the sketch:
```
let a = ⋱ in
let b = "hello, world!" in
let c : Int = ⋱ in
?
```
Calling update_binding_clause(path="b", code="let b : (Int, Int) = (0, ?) in let d : Int = b + 1 in") would result in the program
```
let a = ⋱ in
let b : (Int, Int) = ⋱ in
let d : Int = b + 1 in
let c : Int = ⋱ in
?
```
(Note that this is the only tool that can be called in the special case where there are no let or type alias expressions in the program, in which case, calling this tool will overwrite the entire program with the argument passed into 'code'.)
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
Description:
Removes the entire type/value-binding of the node identified by the provided path.

Parameters:
path: string — slash-delimited path to the binding to remove

Example(s):
Given path "b" and the sketch:
```
let a = ⋱ in
let b = "hello, world!" in
let c : Int = ⋱ in
?
```
Calling delete_binding_clause(path="b") would result in the program
```
let a = 3 in
let c : Int = ⋱ in
?
```
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
Description:
Deletes the body of the node identified by the provided path.

Parameters:
path: string — slash-delimited path to the node whose body should be cleared

Example(s):
Given path "b" and the sketch:
```
let a = ⋱ in
let b = "hello, world!" in
let c : Int = ⋱ in
?
```
Calling delete_body(path="b") would result in the program
```
let a = ⋱ in
let b = ⋱ in
?
```
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
Description:
Inserts code immediately after the definition located at the provided path.

Parameters:
path: string — slash-delimited path to the node after which the code should be inserted
code: string — code to insert

Example(s):
Given the program:
```
let a = ⋱ in
let b = "hello, world!" in
let c : Int = ⋱ in
?
```
Calling insert_after(path="b", code = "let x = string_sub(b ,0, 7) ++ "big " ++ string_sub(b, 7, 6)") would result in the program
```
let a = ⋱ in
let b = ⋱ in
let x = string_sub(b ,0, 7) ++ "big " ++ string_sub(b, 7, 6)
let c : Int = ⋱ in
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
Description:
Inserts code before the let/type alias expression located at the provided path.

Parameters:
path: string — slash-delimited path to the node before which the code should be inserted
code: string — code to insert

Example(s):
Given path "b" and the sketch:
```
let a = ⋱ in
let b = "hello, world!" in
let c : Int = ⋱ in
?
```
Calling insert_before(path="b", code = "let x = a * a in") would result in the program
```
let a = ⋱ in
let x = a * a in
let b = ⋱ in
let c : Int = ⋱ in
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
The selector must contain exactly one `*` (focus marker) indicating which subtree to replace.
The replacement code is parsed according to the sort of the focused node (expression, pattern, or type).

Parameters:
selector: string — selector expression with exactly one `*` focus
code: string — replacement code

Example(s):
Given the program:
```
let x = 42 in x + 1
```
Calling selector_update(selector="let x = *", code="99") results in:
```
let x = 99 in x + 1
```

Given the program:
```
let f = fun x -> if x > 0 then x else 0 in f 5
```
Calling selector_update(selector="\... if _... else *", code="1") results in:
```
let f = fun x -> if x > 0 then x else 1 in f 5
```

Cross-sort example — updating a type annotation:
Given: `let x : Int = 42 in x`
Calling selector_update(selector="let x : *", code="Bool") results in:
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
                        "Selector expression with a * focus (e.g. \"let x = *\", \"\\... if _... else *\").",
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
selector: string — selector expression with exactly one `*` focus

Example(s):
Given the program:
```
let x = 42 in x + 1
```
Calling selector_delete(selector="let x = *") results in:
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
                        "Selector expression with a * focus (e.g. \"let x = *\", \"if _... else *\").",
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
The `*` in the selector marks the anchor point (the existing binding/item), and
new code is inserted immediately after it.
Works with let-bindings, module items, list elements, tuple elements, and case arms.

Parameters:
selector: string — selector expression with `*` marking the anchor
code: string — code to insert after the anchor

Example(s):
Given the program:
```
let x = 1 in x + 1
```
Calling selector_insert_after(selector="* let x", code="let y = 2") results in:
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
                        "Selector expression with * marking the insertion anchor.",
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
The `*` in the selector marks the anchor point (the existing binding/item), and
new code is inserted immediately before it.
Works with let-bindings, module items, list elements, tuple elements, and case arms.

Parameters:
selector: string — selector expression with `*` marking the anchor
code: string — code to insert before the anchor

Example(s):
Given the program:
```
let x = 1 in x + 1
```
Calling selector_insert_before(selector="* let x", code="let y = 2") results in:
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
                        "Selector expression with * marking the insertion anchor.",
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
