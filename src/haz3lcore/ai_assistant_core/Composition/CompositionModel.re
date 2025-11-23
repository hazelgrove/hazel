open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type active_view =
  | Chat
  | Todos;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type task_completion_info = {summary: string};

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type todo_item = {
  title: string, // Will also serve as a unique identifier for the todo item
  description: string, // Description of the todo item
  task_completion_info: option(task_completion_info), // Summary of changes made to complete this todo item, provided when marking the item as complete
  expanded: bool,
  // TODO: Add fields to tie it to code?
};

let is_completed = (item: todo_item): bool =>
  switch (item.task_completion_info) {
  | Some(_) => true
  | None => false
  };

let todo_item_to_string = (todo_item: todo_item): string => {
  " [ "
  ++ (is_completed(todo_item) ? "X" : " ")
  ++ " ] "
  ++ "Title: "
  ++ todo_item.title
  ++ " - "
  ++ "Description: "
  ++ todo_item.description
  ++ " - "
  ++ (
    switch (todo_item.task_completion_info) {
    | Some(info) => "Complete. Summary of changes:\n" ++ info.summary
    | None => "Incomplete."
    }
  )
  ++ " - ";
};

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type todo_list = {
  title: string,
  description: string,
  items: list(todo_item),
  last_updated: float,
};

let todo_list_to_string = (todo_list: todo_list): string => {
  let items_str =
    List.map(todo_item_to_string, todo_list.items) |> String.concat("\n");
  "Todo List: "
  ++ todo_list.title
  ++ "\nHigh-Level Overview: "
  ++ todo_list.description
  ++ "\nLast Updated: "
  ++ string_of_float(todo_list.last_updated)
  ++ "\nItems:\n"
  ++ items_str;
};

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type todo_archive = Maps.StringMap.t(todo_list); // Map of title -> todo_list, (titles are keys)

let sorted_todo_archive = (todo_archive: todo_archive): list(todo_list) => {
  // Sorts the given todo archive by last updated time, most recent first
  todo_archive
  |> Maps.StringMap.bindings
  |> List.map(((_, todo_list)) => todo_list)
  |> List.sort((a, b) => int_of_float(b.last_updated -. a.last_updated));
};

let todo_archive_to_string = (todo_archive: todo_archive): string => {
  let sorted_archive = sorted_todo_archive(todo_archive);
  let header = "=== Todo Archive (Sorted by Last Updated, Most Recent First) ===";
  header
  ++ List.fold_left(
       (acc, todo_list) => acc ++ "\n\n" ++ todo_list_to_string(todo_list),
       header,
       sorted_archive,
     );
};

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type active_task = {
  active_todo_list: todo_list,
  active_todo_item: option(todo_item),
};

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type view_settings = {
  active_view,
  show_archive: bool,
};

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = {
  active_task: option(active_task),
  todo_archive,
  view_settings,
};

let active_task_to_string = (active_task: option(active_task)): string => {
  let active_task_str =
    switch (active_task) {
    | Some(active_task) =>
      "Active Todo List:\n"
      ++ todo_list_to_string(active_task.active_todo_list)
      ++ "\nActive Todo Item:\n"
      ++ (
        switch (active_task.active_todo_item) {
        | Some(item) => todo_item_to_string(item)
        | None => "No active todo item. Set one using the set_active_todo_item tool."
        }
      )
    | None => "No Active Todo List."
    };
  "=== Composition Active Task Todo Information ===\n"
  ++ active_task_str
  ++ "\n========================";
};

let init = (): t => {
  {
    active_task: None,
    todo_archive: Maps.StringMap.empty,
    view_settings: {
      active_view: Chat,
      show_archive: false,
    },
  };
};

let update_model = (new_model: t): t => {
  new_model;
};

let remove_active_todo_list = (model: t): t => {
  let active_stashed =
    switch (model.active_task) {
    | Some(active_task) =>
      Maps.StringMap.add(
        active_task.active_todo_list.title,
        active_task.active_todo_list,
        model.todo_archive,
      )
    | None => model.todo_archive
    };
  {
    ...model,
    active_task: None,
    todo_archive: active_stashed,
  };
};

let update_active_todo_list = (~model: t, ~new_todo_list: todo_list): t => {
  let active_todo_item = {
    switch (model.active_task) {
    | Some(active_task) =>
      if (active_task.active_todo_list.title == new_todo_list.title) {
        // Keep the same active todo item if the todo list title matches
        active_task.
          active_todo_item;
      } else {
        None;
            // Otherwise, unset the active todo item
      }
    | None => None
    };
  };
  // We archive all todo lists just in case
  let active_stashed =
    switch (model.active_task) {
    | Some(active_task) =>
      Maps.StringMap.add(
        active_task.active_todo_list.title,
        active_task.active_todo_list,
        model.todo_archive,
      )
    | None => model.todo_archive
    };
  {
    ...model,
    active_task:
      Some({
        active_todo_list: new_todo_list,
        active_todo_item,
      }),
    todo_archive: active_stashed,
  };
};

let change_active_todo_item =
    (~model: t, ~new_active_todo: option(todo_item)): t => {
  {
    ...model,
    active_task:
      switch (model.active_task) {
      | Some(active_task) =>
        Some({
          ...active_task,
          active_todo_item: new_active_todo,
        })
      | None => None
      },
  };
};
