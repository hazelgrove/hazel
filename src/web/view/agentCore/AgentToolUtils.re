open Util_web;

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

/** How a tool is gated by session mode (see [[AgentUpdate.tool_allowed_in_mode]]):
    EditGated tools are blocked in Plan and Converse; WorkbenchGated and
    OverlayGated tools are blocked in Converse; Ungated tools run anywhere. */
type gating =
  | Ungated
  | EditGated
  | WorkbenchGated
  | OverlayGated;

type entry = {
  category: string, // settings-panel grouping
  gating,
};

/** Single source of truth mapping every declared tool
    ([[CompositionUtils.Public.tools]]) to its settings-panel category and its
    session-mode gating; [[category_of_tool]] and the mode lists below derive
    from it. Overlay tools group under "View" in settings but gate separately.
    A completeness test in [[Test_AgentTools]] keeps this in sync with the
    JSON definitions. */
let registry: list((string, entry)) = {
  let entries = (category, gating, names) =>
    List.map(
      name =>
        (
          name,
          {
            category,
            gating,
          },
        ),
      names,
    );
  entries("View", Ungated, ["expand", "collapse"])
  @ entries(
      "View",
      OverlayGated,
      [
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
    )
  @ entries(
      "Edit",
      EditGated,
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
    )
  @ entries(
      "Workbench",
      WorkbenchGated,
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
    )
  /* Newer workbench tools: ungated and grouped under "Other", matching the
     pre-registry lists. */
  @ entries(
      "Other",
      Ungated,
      [
        "update_active_task",
        "update_active_subtask",
        "delete_task",
        "delete_subtask",
      ],
    );
};

let category_of_tool = (name: string): string =>
  switch (List.assoc_opt(name, registry)) {
  | Some(entry) => entry.category
  | None => "Other"
  };

let names_with_gating = (g: gating): list(string) =>
  List.filter_map(
    ((name, entry)) => entry.gating == g ? Some(name) : None,
    registry,
  );

/** Tools that mutate the program (EditTools.*). Blocked in Plan mode. */
let edit_tool_names = names_with_gating(EditGated);

/** Tools that mutate the workbench task board (WorkbenchTools.*).
    Blocked in Converse mode (along with edit + overlay tools). */
let workbench_tool_names = names_with_gating(WorkbenchGated);

/** Overlay-placement tools (probes / statics / syntax projectors).
    Blocked in Converse mode; allowed in Plan and Edit. */
let overlay_tool_names = names_with_gating(OverlayGated);
