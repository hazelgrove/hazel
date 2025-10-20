open Util;

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

let go_to_binding_site_description = {|
Moves the cursor to the specified binding site of the variable reference.

Notice how you are always given a list of variables referenced at the current node.
You may navigate to the binding sites (aka definitions) of these variables to view
their definitions directly! Use either the name or the index of the variable (or both!) to
navigate to its binding site.
|};

let go_to_binding_site: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("go_to_binding_site")),
        ("description", `String(go_to_binding_site_description)),
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
                        "The name of the variable to move the cursor to.",
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
                        "An optional index of the variable to move the cursor to. Index is derived from the list of displayed variables referenced. If there are multiple variables with the same name, you must specify the index to disambiguate.",
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
