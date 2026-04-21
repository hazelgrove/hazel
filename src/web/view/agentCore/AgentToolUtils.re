open Util;

let get_name = (tool: API.Json.t): option(string) => {
  switch (API.Json.dot("function", tool)) {
  | Some(func) =>
    switch (API.Json.dot("name", func)) {
    | Some(name_json) => API.Json.str(name_json)
    | None => None
    }
  | None => None
  };
};

let get_description = (tool: API.Json.t): option(string) => {
  switch (API.Json.dot("function", tool)) {
  | Some(func) =>
    switch (API.Json.dot("description", func)) {
    | Some(desc_json) => API.Json.str(desc_json)
    | None => None
    }
  | None => None
  };
};

let category_of_tool = (name: string): string => {
  switch (name) {
  | n
      when
        List.mem(
          n,
          [
            "expand",
            "collapse",
            "place_probe",
            "remove_probe",
            "toggle_probe",
            "place_statics",
            "remove_statics",
            "toggle_statics",
            "place_syntax_projector",
            "remove_syntax_projector",
            "toggle_syntax_projector",
          ],
        ) => "View"
  | n
      when
        List.mem(
          n,
          [
            "update_definition",
            "update_body",
            "update_pattern",
            "update_binding_clause",
            "delete_binding_clause",
            "delete_body",
            "insert_after",
            "insert_before",
          ],
        ) => "Edit"
  | n
      when
        List.mem(
          n,
          [
            "create_new_task",
            "set_active_task",
            "unset_active_task",
            "set_active_subtask",
            "unset_active_subtask",
            "mark_active_task_complete",
            "mark_active_task_incomplete",
            "mark_active_subtask_complete",
            "mark_active_subtask_incomplete",
            "mark_active_subtask_failed",
            "mark_active_task_failed",
            "add_new_subtask_to_active_task",
            "reorder_subtasks_in_active_task",
          ],
        ) => "Workbench"
  | _ => "Other"
  };
};
