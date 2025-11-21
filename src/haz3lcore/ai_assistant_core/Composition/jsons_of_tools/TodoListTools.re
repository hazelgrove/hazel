open Util;

let new_todo_list_description = {|
Description:
Initializes a new todo list, or overwrites the existing todo list with the given list of todo items.

Parameters:
todo_items: list({
  title: string,
  description: string
}) — the list of todo items to initialize

Example(s):
Calling new_todo_list(todo_items = [{"title": "a", "description": "b"}, {"title": "c", "description": "d"}]) would result in the todo list:
[{"title": "a", "description": "b", "completed": false}, {"title": "c", "description": "d", "completed": false}]
|};

let new_todo_list: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("new_todo_list")),
        ("description", `String(new_todo_list_description)),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "todo_items",
                  `Assoc([
                    ("type", `String("array")),
                    (
                      "description",
                      `String("The list of todo items to initialize."),
                    ),
                    (
                      "items",
                      `Assoc([
                        ("type", `String("object")),
                        (
                          "properties",
                          `Assoc([
                            (
                              "title",
                              `Assoc([
                                ("type", `String("string")),
                                (
                                  "description",
                                  `String(
                                    "The title of the todo item. This will also serve as a unique identifier for the todo item in the future. Try to keep short and concise.",
                                  ),
                                ),
                              ]),
                            ),
                            (
                              "description",
                              `Assoc([
                                ("type", `String("string")),
                                (
                                  "description",
                                  `String(
                                    "The description of the todo item.",
                                  ),
                                ),
                              ]),
                            ),
                          ]),
                        ),
                        (
                          "required",
                          `List([`String("title"), `String("description")]),
                        ),
                      ]),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("todo_items")])),
          ]),
        ),
      ]),
    ),
  ]);

let add_todo_items_description = {|
Description:
Adds new todo items to the existing todo list, or, creates a new todo list with the given todo items.

Parameters:
todo_items: list({
  title: string,
  description: string
}) — the list of todo items to add

Example(s):
Calling add_todo_items(todo_items = [{"title": "Task 1", "description": "Do something"}]) would add the todo item to the existing list.
|};

let add_todo_items: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("add_todo_items")),
        ("description", `String(add_todo_items_description)),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "todo_items",
                  `Assoc([
                    ("type", `String("array")),
                    (
                      "description",
                      `String("The list of todo items to add."),
                    ),
                    (
                      "items",
                      `Assoc([
                        ("type", `String("object")),
                        (
                          "properties",
                          `Assoc([
                            (
                              "title",
                              `Assoc([
                                ("type", `String("string")),
                                (
                                  "description",
                                  `String(
                                    "The title of the todo item. This will also serve as a unique identifier for the todo item in the future. Try to keep short and concise.",
                                  ),
                                ),
                              ]),
                            ),
                            (
                              "description",
                              `Assoc([
                                ("type", `String("string")),
                                (
                                  "description",
                                  `String(
                                    "The description of the todo item.",
                                  ),
                                ),
                              ]),
                            ),
                          ]),
                        ),
                        (
                          "required",
                          `List([`String("title"), `String("description")]),
                        ),
                      ]),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("todo_items")])),
          ]),
        ),
      ]),
    ),
  ]);

let delete_todo_list_description = {|
Description:
Deletes the entire todo list, removing all todo items.

Parameters:
None

Example(s):
Calling delete_todo_list() would result in an empty todo list.
|};

let delete_todo_list: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("delete_todo_list")),
        ("description", `String(delete_todo_list_description)),
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

let check_todo_items_description = {|
Description:
Marks todo items as completed by their titles.

Parameters:
titles: list(string) — the list of titles of the todo items to mark as completed

Example(s):
Given a todo list with items [{"title": "Task 1", "description": "Do something", "completed": false}, {"title": "Task 2", "description": "Do something else", "completed": false}],
Calling check_todo_items(titles = ["Task 1", "Task 2"]) would result in:
[{"title": "Task 1", "description": "Do something", "completed": true}, {"title": "Task 2", "description": "Do something else", "completed": true}]
|};

let check_todo_items: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("check_todo_items")),
        ("description", `String(check_todo_items_description)),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "titles",
                  `Assoc([
                    ("type", `String("array")),
                    (
                      "description",
                      `String(
                        "The list of titles of the todo items to mark as completed. Each title must match the title of an existing todo item.",
                      ),
                    ),
                    ("items", `Assoc([("type", `String("string"))])),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("titles")])),
          ]),
        ),
      ]),
    ),
  ]);

let uncheck_todo_items_description = {|
Description:
Marks todo items as not completed (unchecks them) by their titles.

Parameters:
titles: list(string) — the list of titles of the todo items to mark as not completed

Example(s):
Given a todo list with items [{"title": "Task 1", "description": "Do something", "completed": true}, {"title": "Task 2", "description": "Do something else", "completed": true}],
Calling uncheck_todo_items(titles = ["Task 1", "Task 2"]) would result in:
[{"title": "Task 1", "description": "Do something", "completed": false}, {"title": "Task 2", "description": "Do something else", "completed": false}]
|};

let uncheck_todo_items: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("uncheck_todo_items")),
        ("description", `String(uncheck_todo_items_description)),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "titles",
                  `Assoc([
                    ("type", `String("array")),
                    (
                      "description",
                      `String(
                        "The list of titles of the todo items to mark as not completed. Each title must match the title of an existing todo item.",
                      ),
                    ),
                    ("items", `Assoc([("type", `String("string"))])),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("titles")])),
          ]),
        ),
      ]),
    ),
  ]);
