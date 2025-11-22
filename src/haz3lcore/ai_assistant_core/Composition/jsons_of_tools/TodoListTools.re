open Util;

let new_todo_list_description = {|
Description:
If a todo list with the given name does not exist, creates a new todo list with the provided todo items
and sets it as the active todo list. If a todo list with the given name already exists in the archive,
overwrites the existing todo list with the provided one and sets it as the active todo list. Note that this
will set all todo items in the new todo list as incomplete by default.

Parameters:
todo_list: {
  title: string,
  description: string,
  items: list({
    title: string,
    description: string
  })
} — the todo list to create or overwrite
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
                  "todo_list",
                  `Assoc([
                    ("type", `String("object")),
                    (
                      "description",
                      `String("The todo list to create or overwrite."),
                    ),
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
                                "The title of the todo list. Be concise. This will be used as a unique identifing key in the todo list archive.",
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
                                "A high-level description of the todo list. Be descriptive here. This will help provide context and clarity about the purpose and scope of the todo list and task at hand.",
                              ),
                            ),
                          ]),
                        ),
                        (
                          "items",
                          `Assoc([
                            ("type", `String("array")),
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
                                            "The description of the todo item. Be as detailed as necessary to convey the subtask at hand. This will help provide clarity on what needs to be done.",
                                          ),
                                        ),
                                      ]),
                                    ),
                                  ]),
                                ),
                                (
                                  "required",
                                  `List([
                                    `String("title"),
                                    `String("description"),
                                  ]),
                                ),
                              ]),
                            ),
                          ]),
                        ),
                      ]),
                    ),
                    (
                      "required",
                      `List([
                        `String("title"),
                        `String("description"),
                        `String("items"),
                      ]),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("todo_list")])),
          ]),
        ),
      ]),
    ),
  ]);

let archive_todo_list_description = {|
Description:
Archives the active todo list, and sets the active todo list to None.

Parameters:
None

Example(s):
Calling archive_todo_list() will result in the active todo list being set to None,
and the previous active todo list being stored in the todo list archive.
|};

let archive_todo_list: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("archive_todo_list")),
        ("description", `String(archive_todo_list_description)),
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

let mark_todo_items_complete_description = {|
Description:
Marks todo items as completed by their titles.

Parameters:
titles: list(string) — the list of titles of the todo items to mark as completed

Example(s):
Given a todo list with items [{"title": "Task 1", "description": "Do something", "completed": false}, {"title": "Task 2", "description": "Do something else", "completed": false}],
Calling mark_todo_items_complete(titles = ["Task 1", "Task 2"]) would result in:
[{"title": "Task 1", "description": "Do something", "completed": true}, {"title": "Task 2", "description": "Do something else", "completed": true}]
|};

let mark_todo_items_complete: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("mark_todo_items_complete")),
        ("description", `String(mark_todo_items_complete_description)),
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

let mark_todo_items_incomplete_description = {|
Description:
Marks todo items as not completed (unchecks them) by their titles.

Parameters:
titles: list(string) — the list of titles of the todo items to mark as not completed

Example(s):
Given a todo list with items [{"title": "Task 1", "description": "Do something", "completed": true}, {"title": "Task 2", "description": "Do something else", "completed": true}],
Calling mark_todo_items_incomplete(titles = ["Task 1", "Task 2"]) would result in:
[{"title": "Task 1", "description": "Do something", "completed": false}, {"title": "Task 2", "description": "Do something else", "completed": false}]
|};

let mark_todo_items_incomplete: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("mark_todo_items_incomplete")),
        ("description", `String(mark_todo_items_incomplete_description)),
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
