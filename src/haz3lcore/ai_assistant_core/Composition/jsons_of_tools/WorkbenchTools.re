open Util;

let create_new_task_description = {|
# Description
## Effects
Creates a new task with the given title, description, and subtasks.
If a task with the same title already exists, it will be overwritten.
All subtasks will be initialized to incomplete and no active subtask will be set.
This new task will be set as the active task in the model.
## Use Cases
Use this tool before starting work on a new feature, bug fix, or code implementation.
Since you are only allowed to make edit actions while an active subtask is set,
this is a mandatory step.
Use this tool to organize your thoughts, plan out implementation steps, and break down
complex tasks into manageable subtasks.
This will help you stay focused, track progress, and ensure that you complete all necessary steps
to successfully implement the desired functionality.

Parameters:
task: {
  title: string,
  description: string,
  subtasks: list({
    title: string,
    description: string
  })
}
|};

let create_new_task: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("create_new_task")),
        ("description", `String(create_new_task_description)),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "task",
                  `Assoc([
                    ("type", `String("object")),
                    ("description", `String("The task to create.")),
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
                                "The title of the task. Be concise. This will be used as a unique identifing key in a dictionary of tasks (to store/persist).",
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
                                "A high-level description of the task. Be descriptive here. This will help provide context and clarity about the purpose and scope of the task at hand. Jot down any relevant details and/or pseudocode that will help guide the implementation process.",
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
                                            "The title of the subtask. This will also serve as a unique identifier for the subtask relative to the rest of the subtasks belonging to this specific task. Try to keep short and concise.",
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
                                            "The description of the subtask. Be as detailed as necessary to convey the subtask at hand. This will help provide clarity on what needs to be done. Jot down any relevant details and/or pseudocode that will help guide the implementation process.",
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
                        `String("subtasks"),
                      ]),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("task")])),
          ]),
        ),
      ]),
    ),
  ]);

let unset_active_task_description = {|
Description:
Unsets the currently active task in the composition model.
This results in no active task being set.

Parameters:
None

Example(s):
Calling unset_active_task() will result in no active task being set in the composition model.
|};

let unset_active_task: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("unset_active_task")),
        ("description", `String(unset_active_task_description)),
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

let set_active_task_description = {|
Description:
Sets the active task in the composition model to the task with the given title.

Parameters:
title: string — the title of the task to set as active

Example(s):
Calling set_active_task(title = "Task 1") will set the active task in the composition model to the task with the title "Task 1".
The title must match the title of an existing task in the task dictionary.
|};

let set_active_task: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("set_active_task")),
        ("description", `String(set_active_task_description)),
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
                      `String("The title of the task to set as active."),
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

let unset_active_subtask_description = {|
Description:
Unsets the currently active subtask in the active task.
This results in no active subtask being set for the currently active task.
This requires an active task to be set, otherwise nothing will happen.

Parameters:
None

Example(s):
Calling unset_active_subtask() will result in no active subtask being set for the currently active task.
|};

let unset_active_subtask: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("unset_active_subtask")),
        ("description", `String(unset_active_subtask_description)),
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

let set_active_subtask_description = {|
Description:
Sets the active subtask in the active task to the subtask with the given title.

Parameters:
title: string — the title of the subtask to set as active

Example(s):
Calling set_active_subtask(title = "Subtask 1") will set the active subtask in the active task to the subtask with the title "Subtask 1".
The title must match the title of an existing subtask in the subtasks of the active task.
|};

let set_active_subtask: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("set_active_subtask")),
        ("description", `String(set_active_subtask_description)),
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
                      `String("The title of the subtask to set as active."),
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

let mark_active_task_complete_description = {|
Description:
Marks the currently active task as complete.
Sets the completion summary of the active task with the given summary.
This requires an active task to be set, otherwise nothing will happen.

Parameters:
summary: string — a summary of changes made to complete the active task
|};

let mark_active_task_complete: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("mark_active_task_complete")),
        ("description", `String(mark_active_task_complete_description)),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "summary",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "The summary of changes made to complete the active task. Be descriptive here. This should help the user and future developers/agents understand what changes were made to the code to accomplish this task.",
                      ),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("summary")])),
          ]),
        ),
      ]),
    ),
  ]);

let mark_active_task_incomplete_description = {|
Description:
Marks the active task as incomplete.
This requires an active task to be set, otherwise nothing will happen.

Parameters:
None
|};

let mark_active_task_incomplete: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("mark_active_task_incomplete")),
        ("description", `String(mark_active_task_incomplete_description)),
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

let mark_active_subtask_complete_description = {|
Description:
Marks the currently active subtask as complete.
Sets the completion summary of the active subtask with the given summary.

Parameters:
summary: string — a summary of changes made to complete the active subtask
|};

let mark_active_subtask_complete: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("mark_active_subtask_complete")),
        ("description", `String(mark_active_subtask_complete_description)),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "summary",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "The summary of changes made to complete the active subtask. Be descriptive here. This should help the user and future developers/agents understand what changes were made to the code to accomplish this subtask.",
                      ),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("summary")])),
          ]),
        ),
      ]),
    ),
  ]);

let mark_active_subtask_incomplete_description = {|
Description:
Marks the active subtask as incomplete.
This requires an active subtask to be set, otherwise nothing will happen.

Parameters:
None
|};

let mark_active_subtask_incomplete: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("mark_active_subtask_incomplete")),
        ("description", `String(mark_active_subtask_incomplete_description)),
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
