open Util;
open CompositionActions;
open Language;

/* The tools offered to the model, and a call's action */

[@deriving (show({with_path: false}), sexp, yojson)]
type action_wrapper =
  | Action(action)
  /* read_docs: answered from DocPacks, no editor change */
  | DocsRequest(string)
  | Failure(string);

/* read_docs is offered only when the pack registry is non-empty */
let docs_tools = DocPacks.all == [] ? [] : [DocsTools.read_docs];
let tools =
  [ViewTools.open_, ViewTools.close]
  @ docs_tools
  @ [
    ProbeTools.place_probe,
    ProbeTools.remove_probe,
    ProbeTools.toggle_probe,
    StaticsTools.place_statics,
    StaticsTools.remove_statics,
    StaticsTools.toggle_statics,
    SyntaxProjectorTools.place_syntax_projector,
    SyntaxProjectorTools.remove_syntax_projector,
    SyntaxProjectorTools.toggle_syntax_projector,
    EditTools.insert,
    EditTools.replace,
    EditTools.delete,
    EditTools.move,
    EditTools.rename,
    WorkbenchTools.create_new_task,
    WorkbenchTools.set_active_task,
    WorkbenchTools.unset_active_task,
    WorkbenchTools.set_active_subtask,
    WorkbenchTools.unset_active_subtask,
    WorkbenchTools.mark_active_task_complete,
    WorkbenchTools.mark_active_task_incomplete,
    WorkbenchTools.mark_active_subtask_complete,
    WorkbenchTools.mark_active_subtask_incomplete,
    WorkbenchTools.mark_active_subtask_failed,
    WorkbenchTools.mark_active_task_failed,
    WorkbenchTools.add_new_subtask_to_active_task,
    WorkbenchTools.reorder_subtasks_in_active_task,
    WorkbenchTools.update_active_task,
    WorkbenchTools.update_active_subtask,
    WorkbenchTools.delete_task,
    WorkbenchTools.delete_subtask,
  ];

/* an optional string field: None when absent or empty */
let get_optional_string = (args: API.Json.t, field: string): option(string) => {
  switch (API.Json.dot(field, args)) {
  | Some(`String("")) => None
  | Some(`String(s)) => Some(s)
  | _ => None
  };
};

let syntax_projector_kind_of_string = (s: string): ProjectorKind.t => {
  let k = ProjectorKind.of_name(String.trim(s));
  if (ProjectorKind.is_refractor(k)) {
    raise(
      Failure(
        "syntax projector kind cannot be probe or statics — use the probe or statics tools",
      ),
    );
  };
  k;
};

let action_of = (~tool_name: string, ~args: API.Json.t): action_wrapper =>
  try(
    {
      open API.Json.Parsers;
      /* exactly one place; "" is the top level */
      let place = (): Action.Structural.place =>
        switch (
          List.filter_map(
            ((key, mk)) =>
              switch (API.Json.dot(key, args)) {
              | Some(`String(s)) => Some(mk(String.trim(s)))
              | _ => None
              },
            [
              ("before", s => Action.Structural.Before(s)),
              ("after", s => After(s)),
              ("first_in", s => FirstIn(s)),
              ("last_in", s => LastIn(s)),
            ],
          )
        ) {
        | [place] => place
        | _ =>
          raise(
            Failure(
              tool_name
              ++ " takes exactly one of before, after, first_in, last_in",
            ),
          )
        };
      tool_name == "read_docs"
        ? DocsRequest(get_string(args, "topic"))
        : Action(
            switch (tool_name) {
            | "open" =>
              AgentContextAction(
                OpenPaths(get_string_list(args, "targets")),
              )
            | "close" =>
              switch (API.Json.dot("all", args)) {
              | Some(`Bool(true)) => AgentContextAction(CloseAllPaths)
              | _ =>
                AgentContextAction(
                  ClosePaths(get_string_list(args, "targets")),
                )
              }
            | "place_probe" =>
              ProbeAction(PlaceProbe(get_string_list(args, "paths")))
            | "remove_probe" =>
              ProbeAction(RemoveProbe(get_string_list(args, "paths")))
            | "toggle_probe" =>
              ProbeAction(ToggleProbe(get_string_list(args, "paths")))
            | "place_statics" =>
              StaticsAction(PlaceStatics(get_string_list(args, "paths")))
            | "remove_statics" =>
              StaticsAction(RemoveStatics(get_string_list(args, "paths")))
            | "toggle_statics" =>
              StaticsAction(ToggleStatics(get_string_list(args, "paths")))
            | "place_syntax_projector" =>
              SyntaxProjectorAction(
                PlaceSyntaxProjector(
                  syntax_projector_kind_of_string(get_string(args, "kind")),
                  get_string_list(args, "paths"),
                ),
              )
            | "remove_syntax_projector" =>
              SyntaxProjectorAction(
                RemoveSyntaxProjector(get_string_list(args, "paths")),
              )
            | "toggle_syntax_projector" =>
              SyntaxProjectorAction(
                ToggleSyntaxProjector(
                  syntax_projector_kind_of_string(get_string(args, "kind")),
                  get_string_list(args, "paths"),
                ),
              )
            | "insert" =>
              EditorAction(Insert(place(), get_string(args, "code")))
            | "replace" =>
              EditorAction(
                Replace(
                  get_string(args, "target"),
                  get_string(args, "code"),
                ),
              )
            | "delete" => EditorAction(Delete(get_string(args, "target")))
            | "move" =>
              EditorAction(Move(get_string(args, "target"), place()))
            | "rename" =>
              EditorAction(
                Rename(
                  get_string(args, "target"),
                  Option.map(String.trim, get_optional_string(args, "name")),
                  get_string(args, "to"),
                ),
              )
            | "create_new_task" =>
              WorkbenchAction(
                CreateNewTask(
                  {
                    let task_json =
                      switch (API.Json.dot("task", args)) {
                      | Some(task_obj) =>
                        // Format 1: nested in "task" field (from schema)
                        task_obj
                      | None =>
                        // Format 2: direct format (LLM sends task object directly)
                        args
                      };
                    AgentWorkbench.Utils.TaskUtils.json_to_task(task_json);
                  },
                ),
              )
            | "unset_active_task" => WorkbenchAction(UnsetActiveTask)
            | "set_active_task" =>
              WorkbenchAction(SetActiveTask(get_string(args, "title")))
            | "unset_active_subtask" => WorkbenchAction(UnsetActiveSubtask)
            | "set_active_subtask" =>
              WorkbenchAction(SetActiveSubtask(get_string(args, "title")))
            | "mark_active_task_complete" =>
              WorkbenchAction(
                MarkActiveTaskComplete(get_string(args, "summary")),
              )
            | "mark_active_task_incomplete" =>
              WorkbenchAction(MarkActiveTaskIncomplete)
            | "mark_active_subtask_complete" =>
              WorkbenchAction(
                MarkActiveSubtaskComplete(get_string(args, "summary")),
              )
            | "mark_active_subtask_incomplete" =>
              WorkbenchAction(MarkActiveSubtaskIncomplete)
            | "mark_active_subtask_failed" =>
              WorkbenchAction(
                MarkActiveSubtaskFailed(get_string(args, "reason")),
              )
            | "mark_active_task_failed" =>
              WorkbenchAction(
                MarkActiveTaskFailed(get_string(args, "reason")),
              )
            | "add_new_subtask_to_active_task" =>
              WorkbenchAction(
                AddNewSubtaskToActiveTask(
                  {
                    let subtask_json =
                      API.Json.Parsers.get_json(args, "subtask");
                    AgentWorkbench.Utils.SubtaskUtils.json_to_subtask(
                      subtask_json,
                    );
                  },
                ),
              )
            | "reorder_subtasks_in_active_task" =>
              WorkbenchAction(
                ReorderSubtasksInActiveTask(
                  get_string_list(args, "subtasks_ordering"),
                ),
              )
            | "update_active_task" =>
              WorkbenchAction(
                UpdateActiveTask(
                  get_optional_string(args, "new_title"),
                  get_optional_string(args, "new_description"),
                ),
              )
            | "update_active_subtask" =>
              WorkbenchAction(
                UpdateActiveSubtask(
                  get_optional_string(args, "new_title"),
                  get_optional_string(args, "new_description"),
                ),
              )
            | "delete_task" =>
              WorkbenchAction(DeleteTask(get_string(args, "title")))
            | "delete_subtask" =>
              WorkbenchAction(DeleteSubtask(get_string(args, "title")))
            | _ => raise(Failure("The tool called does not exist."))
            },
          );
    }
  ) {
  | Failure(s) => Failure(s)
  };
