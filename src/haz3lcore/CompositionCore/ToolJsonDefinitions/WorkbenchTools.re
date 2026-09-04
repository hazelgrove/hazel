open Util_web;

let create_new_task_description = {|
Creates a task plan with a title, description, and subtask milestones.
Sets this as the active task. The first subtask becomes active automatically.

Does NOT modify the Hazel program, probes, statics, or syntax projectors — only the workbench.
You must still call edit tools (update_*, insert_*, delete_*) and placement tools (place_probe, place_statics, place_syntax_projector, …) to change what the user sees.

Optional: use this for **larger or multi-turn** efforts where a visible plan helps (features, big refactors, many steps). **Skip** for trivial or one-shot edits — small fixes do not need a task; edits are not required to be tied to any task.

When you do use it: subtasks should be meaningful milestones (e.g., "Implement core logic", "Add tests"),
not individual tool calls (e.g., NOT "update definition", "add line break").
Aim for 1-4 subtasks per task. Each subtask represents a logical stage of work.

Parameters:
task: {
  title: string — concise name (serves as unique identifier)
  description: string — detailed explanation of the goal, approach, and relevant context
  subtasks: list({
    title: string — concise milestone name (serves as unique identifier)
    description: string — what this milestone involves and any implementation notes
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
                          "subtasks",
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
Marks the active task as complete with a summary describing what was accomplished.
This tool only updates the workbench — it does NOT edit code or place probes/statics/projectors.
Only use it after the corresponding program and overlay changes were already applied via the appropriate tools.

The summary is visible to the user — make it clear and informative.
Any subtasks that are still open are **automatically marked complete** with a short system note when you call this tool, so you do not need to call `mark_active_subtask_complete` on each one first.

Parameters:
summary: string — a clear description of what was built/changed and how it works (must match edits/placements you actually ran)
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
Marks the active subtask as complete and automatically advances to the next incomplete subtask.
Does NOT modify the program or overlays — only the workbench. Call edit and placement tools first.

If all subtasks are complete, no subtask will be active; you can then call `mark_active_task_complete` (which also auto-completes any remaining open subtasks if you skip straight to it).

Parameters:
summary: string — a brief description of what was done to complete this milestone (must reflect real tool-driven changes)
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

let add_new_subtask_to_active_task_description = {|
Adds a new subtask milestone to the active task.
Use this when you discover additional work needed during implementation.
The new subtask is appended to the end of the ordering.

Parameters:
subtask: {
  title: string — concise milestone name
  description: string — what this milestone involves
}
|};

let add_new_subtask_to_active_task: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("add_new_subtask_to_active_task")),
        ("description", `String(add_new_subtask_to_active_task_description)),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "subtask",
                  `Assoc([
                    ("type", `String("object")),
                    ("description", `String("The subtask to add.")),
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
                      `List([`String("title"), `String("description")]),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("subtask")])),
          ]),
        ),
      ]),
    ),
  ]);

let mark_active_subtask_failed_description = {|
Description:
Marks the currently active subtask as failed.
Use this when you are unable to complete the subtask due to errors, constraints, or other blockers.
Also automatically tries to set the next incomplete subtask (if any) as the active subtask.

Parameters:
reason: string — a brief explanation of why the subtask failed
|};

let mark_active_subtask_failed: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("mark_active_subtask_failed")),
        ("description", `String(mark_active_subtask_failed_description)),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "reason",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "A brief explanation of why the subtask could not be completed.",
                      ),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("reason")])),
          ]),
        ),
      ]),
    ),
  ]);

let mark_active_task_failed_description = {|
Description:
Marks the currently active task as failed.
Use this when you are unable to complete the overall task. This will also unset the active subtask.

Parameters:
reason: string — a brief explanation of why the task failed
|};

let mark_active_task_failed: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("mark_active_task_failed")),
        ("description", `String(mark_active_task_failed_description)),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "reason",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "A brief explanation of why the task could not be completed.",
                      ),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("reason")])),
          ]),
        ),
      ]),
    ),
  ]);

let reorder_subtasks_in_active_task_description = {|
Description:
Reorders the subtasks in the currently active task according to the given new ordering.
The new ordering is a list of subtask titles.
This requires an active task to be set, otherwise nothing will happen.
Make sure to use the exact titles of the subtasks as they appear in the active task's subtasks, otherwise this tool call will fail.

Parameters:
subtasks_ordering: list(string) — the new ordering of subtask titles
|};

let reorder_subtasks_in_active_task: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("reorder_subtasks_in_active_task")),
        (
          "description",
          `String(reorder_subtasks_in_active_task_description),
        ),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "subtasks_ordering",
                  `Assoc([
                    ("type", `String("array")),
                    ("items", `Assoc([("type", `String("string"))])),
                    (
                      "description",
                      `String(
                        "The new ordering of subtasks (referenced by their titles (aka keys/unique identifiers)) for the active task.",
                      ),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("subtasks_ordering")])),
          ]),
        ),
      ]),
    ),
  ]);

let update_active_task_description = {|
Updates the currently active task's title and/or description.
Provide either or both fields — only the fields you pass are changed.
Changing the title renames the task's key in the dictionary; pointers (active_task, display_task) follow automatically.
Fails if the new title collides with an existing task, or if no active task is set.

Parameters:
new_title: string (optional) — the new title for the active task
new_description: string (optional) — the new description for the active task
|};

let update_active_task: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("update_active_task")),
        ("description", `String(update_active_task_description)),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "new_title",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "Optional. The new title for the active task. Omit to keep the current title.",
                      ),
                    ),
                  ]),
                ),
                (
                  "new_description",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "Optional. The new description for the active task. Omit to keep the current description.",
                      ),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([])),
          ]),
        ),
      ]),
    ),
  ]);

let update_active_subtask_description = {|
Updates the currently active subtask's title and/or description.
Provide either or both fields — only the fields you pass are changed.
Changing the title renames the subtask's key within the active task and updates the subtask ordering and active_subtask pointer automatically.
Fails if the new title collides with an existing subtask in the active task, or if no active subtask is set.

Parameters:
new_title: string (optional) — the new title for the active subtask
new_description: string (optional) — the new description for the active subtask
|};

let update_active_subtask: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("update_active_subtask")),
        ("description", `String(update_active_subtask_description)),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "new_title",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "Optional. The new title for the active subtask. Omit to keep the current title.",
                      ),
                    ),
                  ]),
                ),
                (
                  "new_description",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "Optional. The new description for the active subtask. Omit to keep the current description.",
                      ),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([])),
          ]),
        ),
      ]),
    ),
  ]);

let delete_task_description = {|
Hard-deletes the task with the given title from the task dictionary.
If the deleted task was the active task, active_task is cleared.
If it was the currently-displayed task in the UI, display_task is cleared.
Fails if no task with that title exists.

Parameters:
title: string — the title of the task to delete
|};

let delete_task: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("delete_task")),
        ("description", `String(delete_task_description)),
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
                      `String("The title of the task to delete."),
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

let delete_subtask_description = {|
Hard-deletes the subtask with the given title from the currently active task.
Removes it from the subtask ordering. If it was the active subtask, active_subtask is cleared.
Fails if no active task is set or if no subtask with that title exists in the active task.

Parameters:
title: string — the title of the subtask to delete
|};

let delete_subtask: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("delete_subtask")),
        ("description", `String(delete_subtask_description)),
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
                        "The title of the subtask to delete from the active task.",
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
