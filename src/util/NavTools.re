let go_to_parent: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("go_to_parent")),
        (
          "description",
          `String(
            "Moves the cursor to the parent node of the current node in the AST.",
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

let go_to_child: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("go_to_child")),
        (
          "description",
          `String(
            "Moves the cursor to the specified child node of the current node in the AST.",
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
                  "name",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "The name of the child node to move the cursor to.",
                      ),
                    ),
                  ]),
                ),
                (
                  "index",
                  `Assoc([
                    ("type", `String("integer")),
                    (
                      "description",
                      `String(
                        "An optional index of the child node to move the cursor to. Index is derived from the list of displayed children nodes of the current node.",
                      ),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("name")])),
          ]),
        ),
      ]),
    ),
  ]);

let go_to_sibling: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("go_to_sibling")),
        (
          "description",
          `String(
            "Moves the cursor to the specified sibling node of the current node in the AST.",
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
                  "name",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "The name of the sibling node to move the cursor to.",
                      ),
                    ),
                  ]),
                ),
                (
                  "index",
                  `Assoc([
                    ("type", `String("integer")),
                    (
                      "description",
                      `String(
                        "An optional index of the sibling node to move the cursor to. Index is derived from the list of displayed sibling nodes of the current node. If there are multiple siblings with the same name, you must specify the index to disambiguate.",
                      ),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("name")])),
          ]),
        ),
      ]),
    ),
  ]);
