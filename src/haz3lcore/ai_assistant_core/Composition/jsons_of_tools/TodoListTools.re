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

let mark_todo_item_complete_description = {|
Description:
Marks a todo item as complete via title key lookup. Sets the summary of changes made to complete the todo item.

Parameters:
title: string — the title of the todo item to mark as completed
summary: string — a summary of changes made to complete the todo item

Example(s):
Given a todo list with items [{"title": "Task 1", "description": "Do something", "task_completion_info": None}, {"title": "Task 2", "description": "Do something else", "task_completion_info": None}],
Calling mark_todo_item_complete(title = "Task 1", summary = "Completed the task") would result in:
[{"title": "Task 1", "description": "Do something", "task_completion_info": {"summary": "Completed the task"}}, {"title": "Task 2", "description": "Do something else", "task_completion_info": None}]
|};

let mark_todo_item_complete: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("mark_todo_item_complete")),
        ("description", `String(mark_todo_item_complete_description)),
        (
          "parameters",
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
                        "The title of the todo item to mark as completed. The title must match the title of an existing todo item.",
                      ),
                    ),
                  ]),
                ),
                (
                  "summary",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "The summary of changes made to complete the todo item. Be descriptive.",
                      ),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("title"), `String("summary")])),
          ]),
        ),
      ]),
    ),
  ]);

let mark_todo_item_incomplete_description = {|
Description:
Marks the todo item as incomplete via title key lookup.

Parameters:
title: string — the title of the todo item to mark as not completed

Example(s):
Given a todo list with items [{"title": "Task 1", "description": "Do something", "completion_status": {"summary": "We have completed this item"}}, {"title": "Task 2", "description": "Do something else", "completion_info": None}],
Calling mark_todo_item_incomplete(title = "Task 1") would result in:
[{"title": "Task 1", "description": "Do something", "completion_status": None}, {"title": "Task 2", "description": "Do something else", "completion_status": None}}]
|};

let mark_todo_item_incomplete: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("mark_todo_item_incomplete")),
        ("description", `String(mark_todo_item_incomplete_description)),
        (
          "parameters",
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
                        "The title of the todo item to mark as not completed. Must match an existing todo item title.",
                      ),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("title")])),
          ]),
        ),
      ]),
    ),
  ]);

let set_active_todo_item_description = {|
Description:
Sets the active todo item by its title. This is useful for focusing the agent's attention on a specific task.

Parameters:
title: string — the title of the todo item to set as active

Example(s):
Calling set_active_todo_item(title = "Task 1") would set "Task 1" as the active todo item.
|};

let set_active_todo_item: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("set_active_todo_item")),
        ("description", `String(set_active_todo_item_description)),
        (
          "parameters",
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
                        "The title of the todo item to set as active. Must match an existing todo item title.",
                      ),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("title")])),
          ]),
        ),
      ]),
    ),
  ]);

let unset_active_todo_item_description = {|
Description:
Unsets the currently active todo item. This indicates that no specific task is currently being focused on.

Parameters:
None

Example(s):
Calling unset_active_todo_item() will result in no todo item being active.
|};

let unset_active_todo_item: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("unset_active_todo_item")),
        ("description", `String(unset_active_todo_item_description)),
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
