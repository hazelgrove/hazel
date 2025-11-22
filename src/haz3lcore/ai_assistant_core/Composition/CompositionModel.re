open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type active_view =
  | Chat
  | Todos;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type todo_item = {
  title: string, // Will also serve as a unique identifier for the todo item
  description: string, // Description of the todo item
  completed: bool // Whether the todo item has been completed
  // TODO: Add fields to tie it to code?
};

let todo_item_to_string = (todo_item: todo_item): string => {
  " [ "
  ++ (todo_item.completed ? "X" : " ")
  ++ " ] "
  ++ "Title: "
  ++ todo_item.title
  ++ " - "
  ++ "Description: "
  ++ todo_item.description
  ++ " - "
  ++ "This item is marked as "
  ++ (todo_item.completed ? "complete" : "incomplete")
  ++ " - ";
};

[@deriving (show({with_path: false}), sexp, yojson)]
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

[@deriving (show({with_path: false}), sexp, yojson)]
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

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  active_todo_list: option(todo_list),
  todo_archive,
  active_view,
};

let init = (): t => {
  {
    active_todo_list: None,
    todo_archive: Maps.StringMap.empty,
    active_view: Chat,
  };
};

let update_model = (new_model: t): t => {
  new_model;
};

let add_or_update_active_todo_list =
    (~model: t, ~new_todo_list: option(todo_list)): t => {
  // We need to make sure we stash any prior active todo list into the archive
  // In case we switch currently active todo lists here.
  // This is mainly for an extra layer of safety.
  // Now we have in both our precondition and postcondition that the archive will always be up to date.
  let active_stashed =
    switch (model.active_todo_list) {
    | Some(active_todo_list) =>
      Maps.StringMap.add(
        active_todo_list.title,
        active_todo_list,
        model.todo_archive,
      )
    | None => model.todo_archive
    };
  {
    ...model,
    active_todo_list: new_todo_list,
    todo_archive:
      switch (new_todo_list) {
      | Some(new_todo_list) =>
        Maps.StringMap.add(new_todo_list.title, new_todo_list, active_stashed)
      | None => active_stashed
      },
  };
};
