open Util;

let update_definition: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("update_definition")),
        (
          "description",
          `String(
            "Replaces the definition of the current variable with the code provided.",
          ),
        ),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "code",
                  `String("The new code to replace the definition with."),
                ),
              ]),
            ),
            ("required", `List([`String("code")])),
          ]),
        ),
      ]),
    ),
  ]);

// update_body
// update_pattern
// update_expression

let delete: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("delete")),
        (
          "description",
          `String("Deletes the definition of the current variable."),
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

let add_after: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("add_after")),
        (
          "description",
          `String("Adds the code provided after the current variable."),
        ),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "code",
                  `String("The code to add after the current variable."),
                ),
              ]),
            ),
            ("required", `List([`String("code")])),
          ]),
        ),
      ]),
    ),
  ]);

let add_before: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("add_before")),
        (
          "description",
          `String("Adds the code provided before the current variable."),
        ),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "code",
                  `String("The code to add before the current variable."),
                ),
              ]),
            ),
            ("required", `List([`String("code")])),
          ]),
        ),
      ]),
    ),
  ]);
