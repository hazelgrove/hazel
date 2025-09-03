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
