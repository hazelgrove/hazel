open Util;
open OptUtil.Syntax;

module Model = {
  module UI = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type active_view =
      | Chat
      | Todos;

    [@deriving (show({with_path: false}), sexp, yojson)]
    type subtask_ui = {expanded: bool};

    [@deriving (show({with_path: false}), sexp, yojson)]
    type task_ui = {expanded: bool};

    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      active_view,
      display_task: option(string),
      show_archive: bool,
    };
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type tool_usage_info = {
    tool: string,
    sketch_before: string,
    sketch_after: string,
    timestamp: float,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type completion_info = {
    summary: string,
    elapsed_time: float,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type metadata = {
    began_at: float,
    completed_at: option(float),
    last_updated_at: float,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type subtask = {
    title: string, // Will also serve as a unique identifier for the subtask item
    description: string, // Description of the subtask item
    tools_used: list(tool_usage_info), // TODO: List of tools used for this subtask item
    completion_info: option(completion_info),
    subtask_ui: UI.subtask_ui,
    metadata,
    // TODO: Add fields to tie it to code?
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type task = {
    title: string,
    description: string,
    subtasks: Maps.StringMap.t(subtask), // Map of title -> subtask, (titles are keys)
    subtask_ordering: list(string), // List of subtask titles in the order they should be completed
    tools_used: list(tool_usage_info),
    completion_info: option(completion_info),
    active_subtask: option(string),
    task_ui: UI.task_ui,
    metadata,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type task_dict = Maps.StringMap.t(task); // Map of title -> todo_list, (titles are keys)

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    active_task: option(string), // Title of the active task
    task_dict,
    t_ui: UI.t,
  };
};

module Utils = {
  module SubtaskUtils = {
    // == IMPORTS ==
    open Model;
    // =============

    let is_completed = (subtask: subtask): bool => {
      switch (subtask.completion_info) {
      | Some(_) => true
      | None => false
      };
    };

    let find_subtask = (task: task, subtask_name: string): option(subtask) => {
      // Finds the todo item with the given name
      Maps.StringMap.find_opt(
        subtask_name,
        task.subtasks,
      );
    };

    let is_active = (task: task, subtask_name: string): bool => {
      switch (task.active_subtask) {
      | Some(active_subtask_name) => active_subtask_name == subtask_name
      | None => false
      };
    };

    let subtask_to_json_string = (subtask: subtask): string =>
      yojson_of_subtask(subtask) |> Yojson.Safe.pretty_to_string;

    let subtask_to_pretty_string = (task: task, subtask: subtask): string => {
      "=== Subtask ===\n"
      ++ "Title: "
      ++ subtask.title
      ++ "\n"
      ++ "Description: "
      ++ subtask.description
      ++ "\n"
      ++ "Is Active: "
      ++ string_of_bool(is_active(task, subtask.title))
      ++ "\n"
      ++ "Completion Info: "
      ++ (
        switch (subtask.completion_info) {
        | Some(info) =>
          "Summary: "
          ++ info.summary
          ++ ", Elapsed Time (s): "
          ++ string_of_float(info.elapsed_time)
        | None =>
          is_active(task, subtask.title)
            ? "Not Completed but In Progress."
            : "Not Completed and Not in Progress."
        }
      )
      ++ "\n";
    };

    let mk = (~title: string, ~description: string): subtask => {
      {
        title,
        description,
        tools_used: [],
        completion_info: None,
        subtask_ui: {
          expanded: false,
        },
        metadata: {
          began_at: JsUtil.timestamp(),
          completed_at: None,
          last_updated_at: JsUtil.timestamp(),
        },
      };
    };

    let json_to_subtask = (item: API.Json.t): subtask => {
      let title = API.Json.Parsers.get_string(item, "title");
      let description = API.Json.Parsers.get_string(item, "description");
      mk(~title, ~description);
    };
  };

  module TaskUtils = {
    // == IMPORTS ==
    open Model;
    open SubtaskUtils;
    // =============

    [@deriving (show({with_path: false}), sexp, yojson)]
    type task = Model.task;

    let ordered_subtasks_of = (task: task): list(subtask) => {
      // Returns the subtasks in the order specified by subtask_ordering
      task.subtask_ordering
      |> List.filter_map((title: string) =>
           Maps.StringMap.find_opt(title, task.subtasks)
         );
    };

    let get_incompleted_subtasks = (task: task): list(subtask) => {
      // Returns a list of all incompleted subtasks in the given task
      ordered_subtasks_of(task)
      |> List.filter((subtask: subtask) => !is_completed(subtask));
    };

    let get_next_incomplete_subtask_title = (task: task): option(string) => {
      // Returns the next incompleted subtask in the given task, if any
      let* subtask = get_incompleted_subtasks(task) |> ListUtil.hd_opt;
      Some(subtask.title);
    };

    let find_task = (model: t, task_name: string): option(task) => {
      Maps.StringMap.find_opt(task_name, model.task_dict);
    };

    let is_active = (model: t, task_name: string): bool => {
      switch (model.active_task) {
      | Some(active_task_name) => active_task_name == task_name
      | None => false
      };
    };

    let task_to_json_string = (task: task): string =>
      yojson_of_task(task) |> Yojson.Safe.pretty_to_string;

    let task_to_pretty_string = (model: t, task: task): string => {
      "=== Task ===\n"
      ++ "Title: "
      ++ task.title
      ++ "\n"
      ++ "Description: "
      ++ task.description
      ++ "\n"
      ++ "Is Active: "
      ++ string_of_bool(is_active(model, task.title))
      ++ "\n"
      ++ "Last Updated: "
      ++ string_of_float(task.metadata.last_updated_at)
      ++ "\n"
      ++ "Completion Info: "
      ++ (
        switch (task.completion_info) {
        | Some(info) =>
          "Summary: "
          ++ info.summary
          ++ ", Elapsed Time (s): "
          ++ string_of_float(info.elapsed_time)
        | None =>
          is_active(model, task.title)
            ? "Not Completed but In Progress."
            : "Not Completed and Not in Progress."
        }
      )
      ++ "\nSubtasks:\n"
      ++ List.fold_left(
           (acc: string, subtask: subtask) =>
             acc ++ subtask_to_pretty_string(task, subtask),
           "",
           ordered_subtasks_of(task),
         );
    };

    let active_subtask = (task: task): option(subtask) => {
      switch (task.active_subtask) {
      | Some(active_subtask_name) => find_subtask(task, active_subtask_name)
      | None => None
      };
    };

    let write_subtask = (~task: task, ~subtask: subtask): task => {
      ...task,
      subtasks: Maps.StringMap.add(subtask.title, subtask, task.subtasks),
    };

    let mk =
        (~title: string, ~description: string, ~subtasks: list(subtask))
        : task => {
      let subtask_map =
        List.fold_left(
          (acc: Maps.StringMap.t(subtask), subtask: subtask) =>
            Maps.StringMap.add(subtask.title, subtask, acc),
          Maps.StringMap.empty,
          subtasks,
        );
      let subtask_ordering =
        List.map((subtask: subtask) => subtask.title, subtasks);
      {
        title,
        description,
        subtasks: subtask_map,
        subtask_ordering,
        tools_used: [],
        completion_info: None,
        active_subtask: None,
        task_ui: {
          expanded: false,
        },
        metadata: {
          began_at: JsUtil.timestamp(),
          completed_at: None,
          last_updated_at: JsUtil.timestamp(),
        },
      };
    };
    let json_to_task = (item: API.Json.t): task => {
      let title = API.Json.Parsers.get_string(item, "title");
      let description = API.Json.Parsers.get_string(item, "description");
      let subtasks_json = API.Json.Parsers.get_json_list(item, "subtasks");
      let subtasks =
        List.map(
          (json: API.Json.t) => SubtaskUtils.json_to_subtask(json),
          subtasks_json,
        );
      mk(~title, ~description, ~subtasks);
    };
  };

  module TaskDictUtils = {
    // == Description ==
    // All model updates should occur at this level
    // =================
    // == IMPORTS ==
    open Model;
    open TaskUtils;
    // =============

    let sorted_task_dict = (task_dict: task_dict): list(task) => {
      // Sorts the given todo archive by last updated time, most recent first
      task_dict
      |> Maps.StringMap.bindings
      |> List.map(((_, task: task)) => task)
      |> List.sort((a: task, b: task) =>
           int_of_float(
             b.metadata.last_updated_at -. a.metadata.last_updated_at,
           )
         );
    };

    let task_dict_to_string = (task_dict: task_dict): string => {
      let header = "=== Task Archive (Sorted by Last Updated, Most Recent First) ===";
      header
      ++ List.fold_left(
           (acc: string, task: task) => acc ++ task_to_json_string(task),
           header,
           sorted_task_dict(task_dict),
         );
    };
  };

  module MainUtils = {
    // == IMPORTS ==
    open Model;
    open TaskUtils;
    // =============

    let active_task = (model: t): option(task) => {
      // Returns:
      //    The active task if model.active_task is not None,
      //    o.w. None
      switch (model.active_task) {
      | Some(active_task) =>
        Some(
          TaskUtils.find_task(model, active_task)
          |> OptUtil.get_or_fail(
               "Active task not found: "
               ++ active_task
               ++ "\nThis implies a bug. The string title in active_task should never point to a non-existent task. It either became stale or was ill-set.",
             ),
        )
      | None => None // No active task set
      };
    };

    let displayed_task = (model: t): option(task) => {
      // Returns:
      //    The displayed task if model.t_ui.display_task is not None,
      //    o.w. None
      switch (model.t_ui.display_task) {
      | Some(displayed_task) =>
        Some(
          TaskUtils.find_task(model, displayed_task)
          |> OptUtil.get_or_fail(
               "Displayed task not found: "
               ++ displayed_task
               ++ "\nThis implies a bug. The string title in t_ui.display_task should never point to a non-existent task. It either became stale or was ill-set.",
             ),
        )
      | None => None // No displayed task set
      };
    };

    let active_task_to_pretty_string = (model: t): string => {
      let active_task_str =
        switch (active_task(model)) {
        | Some(active_task) =>
          "Active Task:\n" ++ task_to_pretty_string(model, active_task)
        | None => "No Active Task Set, set one using the set_active_task tool."
        };
      "=== Composition Active Task Information ===\n"
      ++ active_task_str
      ++ "\n========================";
    };

    let init = (): t => {
      {
        active_task: None,
        task_dict: Maps.StringMap.empty,
        t_ui: {
          active_view: UI.Chat,
          display_task: None,
          show_archive: false,
        },
      };
    };

    module SafeTaskAssertion = {
      module StringSet = Set.Make(String);
      let assert_no_duplicates_in_ordering = (~task: task): unit => {
        // Assert no duplicates in subtask_ordering
        let unique_subtask_titles =
          task.subtask_ordering
          |> List.fold_left(
               (acc: StringSet.t, title: string) => {
                 StringSet.add(title, acc)
               },
               StringSet.empty,
             );
        StringSet.cardinal(unique_subtask_titles)
        == List.length(task.subtask_ordering)
          ? ()
          : raise(
              Failure("Duplicate subtask titles found in subtask_ordering"),
            );
      };

      let assert_all_subtasks_from_ordering_exist_in_map = (~task: task): unit =>
        // Assert all subtasks in ordering exist in the subtasks map
        List.for_all(
          (subtask_title: string) => {
            Maps.StringMap.mem(subtask_title, task.subtasks)
          },
          task.subtask_ordering,
        )
          ? ()
          : raise(
              Failure("Subtask titles in ordering not found in subtasks map"),
            );

      let assert_all_subtasks_from_map_referenced_in_ordering =
          (~task: task): unit =>
        // Assert all subtasks in the subtasks map are referenced in the ordering
        Maps.StringMap.for_all(
          (_, subtask: subtask) => {
            List.mem(subtask.title, task.subtask_ordering)
          },
          task.subtasks,
        )
          ? ()
          : raise(
              Failure(
                "Subtask titles in subtasks map not referenced in ordering",
              ),
            );
    };

    [@deriving (show({with_path: false}), sexp, yojson)]
    type task_write_result =
      | Success(t)
      | Failure(string);

    let write_task = (~model: t, ~task: task): task_write_result =>
      try(
        {
          SafeTaskAssertion.assert_no_duplicates_in_ordering(~task);
          SafeTaskAssertion.assert_all_subtasks_from_ordering_exist_in_map(
            ~task,
          );
          SafeTaskAssertion.assert_all_subtasks_from_map_referenced_in_ordering(
            ~task,
          );
          Success({
            ...model,
            task_dict: Maps.StringMap.add(task.title, task, model.task_dict),
          });
        }
      ) {
      | Failure(msg) => Failure(msg)
      };
  };
};

module Update = {
  open Utils;
  open Model;

  module Action = {
    module UIAction = {
      [@deriving (show({with_path: false}), sexp, yojson)]
      type action =
        // Switches the view between Chat and Todos
        | SwitchView(UI.active_view)
        // Toggles whether the task dictionary archive is shown
        | ToggleShowTaskDictionary
        | SetDisplayTask(string)
        | ExpandSubtask(string);
    };

    module BackendAction = {
      [@deriving (show({with_path: false}), sexp, yojson)]
      type action =
        | CreateNewTask(task)
        | UnsetActiveTask
        | SetActiveTask(string) // task title
        | UnsetActiveSubtask
        | SetActiveSubtask(string) // subtask title
        | MarkActiveTaskComplete(string) // summary
        | MarkActiveTaskIncomplete
        | MarkActiveSubtaskComplete(string) // summary
        | MarkActiveSubtaskIncomplete
        | AddNewSubtaskToActiveTask(subtask) // title, description;
        | ReorderSubtasksInActiveTask(list(string)); // list of subtask titles in new order
    };

    [@deriving (show({with_path: false}), sexp, yojson)]
    type action =
      | UIAction(UIAction.action)
      | BackendAction(BackendAction.action);

    [@deriving (show({with_path: false}), sexp, yojson)]
    type result =
      | Success(Model.t)
      | Failure(string);
  };

  module UpdateUtils = {
    let write_task = (~model: Model.t, ~task: task): Action.result => {
      // Attempts to safely write the task to the model's task dictionary
      // If it fails, an error is bubbled up and caught, failing this update action
      // with the respective error message
      switch (MainUtils.write_task(~model, ~task)) {
      | Success(updated_model) => Success(updated_model)
      | Failure(msg) => Failure(msg)
      };
    };

    let set_active_subtask_dispatch =
        (~model: t, ~subtask_name: option(string)): Action.result => {
      switch (MainUtils.active_task(model)) {
      | Some(active_task) =>
        let updated_task = {
          ...active_task,
          active_subtask: subtask_name,
        };
        let updated_task =
          switch (TaskUtils.active_subtask(updated_task)) {
          | None => updated_task
          | Some(active_subtask) =>
            let updated_subtask = {
              ...active_subtask,
              metadata: {
                ...active_subtask.metadata,
                began_at: JsUtil.timestamp(),
              },
            };
            TaskUtils.write_subtask(
              ~task=updated_task,
              ~subtask=updated_subtask,
            );
          };
        write_task(~model, ~task=updated_task);
      | None => Failure("No active task to unset active subtask from")
      };
    };
  };

  let update = (~model: Model.t, ~action: Action.action): Action.result => {
    switch (action) {
    | UIAction(a) =>
      switch (a) {
      | SwitchView(new_view) =>
        Success({
          ...model,
          t_ui: {
            ...model.t_ui,
            active_view: new_view,
          },
        })
      | ToggleShowTaskDictionary =>
        Success({
          ...model,
          t_ui: {
            ...model.t_ui,
            show_archive: !model.t_ui.show_archive,
          },
        })
      | SetDisplayTask(task_title) =>
        Success({
          ...model,
          t_ui: {
            ...model.t_ui,
            display_task: Some(task_title),
          },
        })
      | ExpandSubtask(subtask_title) =>
        switch (MainUtils.displayed_task(model)) {
        | None => Failure("No displayed task to expand subtask in")
        | Some(displayed_task) =>
          switch (SubtaskUtils.find_subtask(displayed_task, subtask_title)) {
          | None =>
            Failure("Subtask not found in displayed task: " ++ subtask_title)
          | Some(subtask) =>
            let updated_subtask = {
              ...subtask,
              subtask_ui: {
                expanded: !subtask.subtask_ui.expanded,
              },
            };
            let updated_task =
              TaskUtils.write_subtask(
                ~task=displayed_task,
                ~subtask=updated_subtask,
              );
            UpdateUtils.write_task(~model, ~task=updated_task);
          }
        }
      }
    | BackendAction(a) =>
      switch (a) {
      | CreateNewTask(new_task) =>
        /*
           - Writes the new task to the model's task dictionary
           - Sets it as the active task
         */
        let model = {
          ...model,
          active_task: Some(new_task.title),
          t_ui: {
            ...model.t_ui,
            display_task:
              model.t_ui.display_task == None
                ? Some(new_task.title) : model.t_ui.display_task,
          },
        };
        UpdateUtils.write_task(~model, ~task=new_task);
      | UnsetActiveTask =>
        /*
           - Unsets the active task in the model
         */
        Success({
          ...model,
          active_task: None,
        })
      | SetActiveTask(task_title) =>
        /*
           - Sets the active task in the model to the given task title
         */
        Success({
          ...model,
          active_task: Some(task_title),
        })
      | UnsetActiveSubtask =>
        /*
         - If an active task exists, unsets the active subtask in that task
         */
        UpdateUtils.set_active_subtask_dispatch(~model, ~subtask_name=None)
      | SetActiveSubtask(subtask_title) =>
        /*
         - If an active task exists, sets the active subtask in that task to the given subtask title
         */
        UpdateUtils.set_active_subtask_dispatch(
          ~model,
          ~subtask_name=Some(subtask_title),
        )
      | MarkActiveTaskComplete(summary) =>
        /*
         - Sets/records the completion info of the active task
         */
        switch (MainUtils.active_task(model)) {
        | None => Failure("No active task to mark complete")
        | Some(active_task) =>
          switch (TaskUtils.get_incompleted_subtasks(active_task)) {
          | [] =>
            let clock_it = JsUtil.timestamp();
            let task = {
              ...active_task,
              completion_info:
                Some({
                  summary,
                  elapsed_time: clock_it -. active_task.metadata.began_at,
                }),
              metadata: {
                ...active_task.metadata,
                completed_at: Some(clock_it),
                last_updated_at: clock_it,
              },
            };
            UpdateUtils.write_task(~model, ~task);
          | incompleted_subtasks =>
            let incompleted_titles =
              incompleted_subtasks
              |> List.map((subtask: subtask) => subtask.title);
            Failure(
              "Cannot mark active task complete. Please complete the following subtasks first before marking the task complete: "
              ++ String.concat(", ", incompleted_titles),
            );
          }
        }
      | MarkActiveTaskIncomplete =>
        /*
         - Clears the completion info of the active task
         - Keeps the active task set in the model
         */
        switch (MainUtils.active_task(model)) {
        | None => Failure("No active task to mark complete")
        | Some(active_task) =>
          let clock_it = JsUtil.timestamp();
          let task = {
            ...active_task,
            completion_info: None,
            metadata: {
              ...active_task.metadata,
              completed_at: None,
              last_updated_at: clock_it,
            },
          };
          UpdateUtils.write_task(~model, ~task);
        }
      | MarkActiveSubtaskComplete(summary) =>
        /*
         - Sets/records the completion info of the active subtask in the active task
         */
        switch (MainUtils.active_task(model)) {
        | None => Failure("No active task to mark subtask complete")
        | Some(active_task) =>
          switch (TaskUtils.active_subtask(active_task)) {
          | None => Failure("No active subtask to mark complete")
          | Some(active_subtask) =>
            let clock_it = JsUtil.timestamp();
            let updated_subtask = {
              ...active_subtask,
              completion_info:
                Some({
                  summary,
                  elapsed_time: clock_it -. active_subtask.metadata.began_at,
                }),
              metadata: {
                ...active_subtask.metadata,
                completed_at: Some(clock_it),
                last_updated_at: clock_it,
              },
            };
            let updated_task =
              TaskUtils.write_subtask(
                ~task={
                  ...active_task,
                  active_subtask:
                    Utils.TaskUtils.get_next_incomplete_subtask_title(
                      active_task,
                    ),
                },
                ~subtask=updated_subtask,
              );
            UpdateUtils.write_task(~model, ~task=updated_task);
          }
        }
      | MarkActiveSubtaskIncomplete =>
        /*
         - Clears the completion info of the active subtask in the active task
         */
        switch (MainUtils.active_task(model)) {
        | None => Failure("No active task to mark subtask incomplete")
        | Some(active_task) =>
          switch (TaskUtils.active_subtask(active_task)) {
          | None => Failure("No active subtask to mark incomplete")
          | Some(active_subtask) =>
            let clock_it = JsUtil.timestamp();
            let updated_subtask = {
              ...active_subtask,
              completion_info: None,
              metadata: {
                ...active_subtask.metadata,
                completed_at: None,
                last_updated_at: clock_it,
              },
            };
            let updated_task =
              TaskUtils.write_subtask(
                ~task=active_task,
                ~subtask=updated_subtask,
              );
            UpdateUtils.write_task(~model, ~task=updated_task);
          }
        }
      // | _ => Failure("unimplemented")
      | AddNewSubtaskToActiveTask(new_subtask) =>
        /*
         - Adds the given new subtask to the active task's subtasks
         - Appends the new subtask's title to the active task's subtask ordering
         */
        switch (MainUtils.active_task(model)) {
        | None => Failure("No active task to add new subtask to")
        | Some(active_task) =>
          let updated_task =
            TaskUtils.write_subtask(~task=active_task, ~subtask=new_subtask);
          let updated_task = {
            ...updated_task,
            subtask_ordering:
              List.append(
                updated_task.subtask_ordering,
                [new_subtask.title],
              ),
          };
          UpdateUtils.write_task(~model, ~task=updated_task);
        }
      | ReorderSubtasksInActiveTask(new_ordering) =>
        /*
         - Reorders the subtasks in the active task according to the given new ordering
         - The new ordering is a list of subtask titles
         */
        switch (MainUtils.active_task(model)) {
        | None => Failure("No active task to reorder subtasks in")
        | Some(active_task) =>
          let updated_task = {
            ...active_task,
            subtask_ordering: new_ordering,
          };
          UpdateUtils.write_task(~model, ~task=updated_task);
        }
      }
    };
  };
};
