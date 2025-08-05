let update_definition_description = {|
Description:
Updates the definition of the current node

Parameters:
code: string — new definition

Example(s):
The current node is "b" and the sketch is:
```
let a = ⋱ in
let b = "hello, world" in
let c : Int = ⋱ in
?
```
Calling update_definition(code=""my new string"") will result in the sketch:
```
let a = ⋱ in
let b = "my new string" in
let c : Int = ⋱ in
?
```
Note(s):
This overwrites the ENTIRE definition of the current node. For example, if the current node is "b" and the sketch is:
```
let a = ⋱ in
let b =
    let x = ⋱ in
in
let c : Int = ⋱ in
?
```
Then calling update_definition(code=""my new string"") will again result in the sketch:
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

let update_body_description = {|
Description:
Replaces the body of the current node

Parameters:
code: string — new body

Example(s):
The current node is "b" and the sketch is:
```
let a = ⋱ in
let b = "hello, world!" in
let c : Int = ⋱ in
?
```
Calling update_body(code="a * a") will result in the program
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
            ("required", `List([`String("code")])),
          ]),
        ),
      ]),
    ),
  ]);

let update_pattern_description = {|
Description:
Updates/renames the pattern of the current node

Parameters:
code: string — new pattern to assign

Example(s):
The current node is "b" and the sketch is:
```
let a = ⋱ in
let b = "hello, world" in
let c : Int = ⋱ in
?
```
Calling update_pattern(code="b : String"), while the current node is "b", would result in the sketch:
```
let a = ⋱ in
let b : String = "hello, world" in
let c : Int = ⋱ in
?
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
                  "code",
                  `Assoc([
                    ("type", `String("string")),
                    ("description", `String(update_pattern_description)),
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

let update_expression_description = {|
Description:
Updates the pattern, definition, and enclosing delimiters of the current node (everything exclusive of the body).
eg. calling update_expression at a current node of let x = 3 in x will overwrite "let x = 3 in"

Parameters:
code: string — new expression (which may contain multiple expressions; see example below for more information)

Example(s):
The current node is "b" and the sketch is:
```
let a = ⋱ in
let b = "hello, world!" in
let c : Int = ⋱ in
?
```
Calling update_expression(variable_name = "b", new_binding = "let b : (Int, Int) = (0, ?) in let d : Int = b + 1 in") would result in the program
```
let a = ⋱ in
let b : (Int, Int) = ⋱ in
let d : Int = b + 1 in
let c : Int = ⋱ in
?
```
(Note how the cursor/node position has also changed, and is now at the latest newly defined expression "d")
|};

let update_expression: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("update_expression")),
        ("description", `String(update_expression_description)),
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
                      `String("The new code to replace the expression with."),
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

let delete_expression_description = {|
Description:
Removes the entire type/value-binding of the current node

Parameters:
None

Example(s):
The current node is "b" and the sketch is:
```
let a = ⋱ in
let b = "hello, world!" in
let c : Int = ⋱ in
?
```
Calling delete_expression() would result in the program
```
let a = 3 in
let c : Int = ⋱ in
?
```
(Note that the cursor has been placed at the succeeding sibling. If no succeeding sibling exists, the cursor is placed at the preceding sibling. If no preceding sibling exists, the cursor is placed at the parent. If no parent exists, then the program is empty.)
|};

let delete_expression: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("delete_expression")),
        ("description", `String(delete_expression_description)),
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

let delete_body_description = {|
Description:
Deletes the body of the current node

Parameters:
None

Example(s):
The current node is "b" and the sketch is:
```
let a = ⋱ in
let b = "hello, world!" in
let c : Int = ⋱ in
?
```
Calling delete_body() would result in the program
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
            ("properties", `Assoc([])),
            ("required", `List([])),
          ]),
        ),
      ]),
    ),
  ]);

let insert_after_description = {|
Description:
Inserts code after the definition of the current node.

Parameters:
code: string — code to insert

Example(s):
Given the program:
```
let a = ⋱ in
let b = "hello, world!" in
let c : Int = ⋱ in
?
```
Calling insert_after(code = "let x = string_sub(b ,0, 7) ++ "big " ++ string_sub(b, 7, 6)") would result in the program
```
let a = ⋱ in
let b = ⋱ in
let x = string_sub(b ,0, 7) ++ "big " ++ string_sub(b, 7, 6)
let c : Int = ⋱ in
?
```
Again, like insert_before, we place the cursor at the latest expression from the newly inserted code.
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
                  "code",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "The code to insert after the current expression.",
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
Description:
Inserts code before the let/type alias expression (current node)

Parameters:
code: string — code to insert

Example(s):
The current node is "b" and the sketch is:
```
let a = ⋱ in
let b = "hello, world!" in
let c : Int = ⋱ in
?
```
Calling insert_before(code = "let x = a * a in") would result in the program
```
let a = ⋱ in
let x = a * a in
let b = ⋱ in
let c : Int = ⋱ in
?
```
Note that the current node the cursor has selected is that of the newly inserted expression (todo: in the case the agent inserts multiple expressions in a single paste, we should select the most recent node as the current).
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
                  "code",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "The code to insert before the current expression.",
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
