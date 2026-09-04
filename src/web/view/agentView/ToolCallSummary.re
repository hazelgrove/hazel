open Util_web;

[@deriving (show({with_path: false}), sexp)]
type category =
  | Edit
  | Read
  | View
  | Projector
  | Probe
  | Statics
  | Workbench;

[@deriving (show({with_path: false}), sexp)]
type t = {
  category,
  signifier: option(string),
  jump_paths: list(string),
  persists: bool,
};

let category_class = (c: category): string =>
  switch (c) {
  | Edit => "tool-cat-edit"
  | Read => "tool-cat-read"
  | View => "tool-cat-view"
  | Projector => "tool-cat-projector"
  | Probe => "tool-cat-probe"
  | Statics => "tool-cat-statics"
  | Workbench => "tool-cat-workbench"
  };

let max_free_text_len = 40;

let truncate_free_text = (s: string): string => {
  let s = String.trim(s);
  if (String.length(s) > max_free_text_len) {
    String.sub(s, 0, max_free_text_len) ++ "…";
  } else {
    s;
  };
};

let join_paths = (paths: list(string)): string =>
  switch (paths) {
  | [] => ""
  | [a] => a
  | [a, b] => a ++ ", " ++ b
  | [a, b, ...rest] =>
    a ++ ", " ++ b ++ " +" ++ string_of_int(List.length(rest))
  };

let get_field = (args: API.Json.t, key: string): option(API.Json.t) =>
  switch (args) {
  | `Assoc(pairs) => List.assoc_opt(key, pairs)
  | _ => None
  };

let get_string_field = (args: API.Json.t, key: string): option(string) =>
  switch (get_field(args, key)) {
  | Some(`String(s)) => Some(s)
  | _ => None
  };

let get_string_list_field =
    (args: API.Json.t, key: string): option(list(string)) =>
  switch (get_field(args, key)) {
  | Some(`List(items)) =>
    let strs =
      List.filter_map(
        fun
        | `String(s) => Some(s)
        | _ => None,
        items,
      );
    Some(strs);
  | _ => None
  };

let paths_signifier = (paths: list(string)): option(string) =>
  switch (paths) {
  | [] => None
  | xs => Some(join_paths(xs))
  };

let paths_signifier_with_kind =
    (args: API.Json.t, paths: list(string)): option(string) => {
  let base = paths_signifier(paths);
  switch (base, get_string_field(args, "kind")) {
  | (Some(p), Some(k)) => Some(p ++ "  [" ++ k ++ "]")
  | (Some(p), None) => Some(p)
  | (None, Some(k)) => Some("[" ++ k ++ "]")
  | (None, None) => None
  };
};

let jump_paths_of_opt = (path: option(string)): list(string) =>
  switch (path) {
  | Some(p) => [p]
  | None => []
  };

/** UI-facing name for a tool_call. Overrides [tc.name] for cases where the raw
    tool name plus a placeholder signifier would mislead — e.g. [insert_before]
    with no [path] would otherwise render as "insert_before cursor", which reads
    like a variable named [cursor]. Display as plain "insert" instead. */
let display_name_for = (tc: OpenRouter.Reply.Model.tool_call): string =>
  switch (tc.name) {
  | "insert_after"
  | "insert_before" =>
    switch (get_string_field(tc.args, "path")) {
    | None => "insert"
    | Some(_) => tc.name
    }
  | _ => tc.name
  };

/** Builds a [[t]] for a given tool_call, or [[None]] if the tool name is unknown. */
let of_tool_call = (tc: OpenRouter.Reply.Model.tool_call): option(t) => {
  let args = tc.args;
  let single_path_edit = (~persists: bool) => {
    let path = get_string_field(args, "path");
    Some({
      category: Edit,
      signifier: path,
      jump_paths: jump_paths_of_opt(path),
      persists,
    });
  };
  let paths_tool = (~category: category, ~persists: bool) => {
    let paths =
      get_string_list_field(args, "paths") |> Option.value(~default=[]);
    Some({
      category,
      signifier: paths_signifier(paths),
      jump_paths: paths,
      persists,
    });
  };
  let paths_tool_with_kind = (~category: category, ~persists: bool) => {
    let paths =
      get_string_list_field(args, "paths") |> Option.value(~default=[]);
    Some({
      category,
      signifier: paths_signifier_with_kind(args, paths),
      jump_paths: paths,
      persists,
    });
  };
  let workbench = (~signifier: option(string)) =>
    Some({
      category: Workbench,
      signifier,
      jump_paths: [],
      persists: false,
    });
  switch (tc.name) {
  | "update_definition"
  | "update_body"
  | "update_pattern"
  | "update_binding_clause" => single_path_edit(~persists=true)
  | "delete_binding_clause"
  | "delete_body" => single_path_edit(~persists=false)
  | "insert_after"
  | "insert_before" =>
    let path = get_string_field(args, "path");
    Some({
      category: Edit,
      signifier: path,
      jump_paths: jump_paths_of_opt(path),
      persists: true,
    });
  | "view_entire_definition"
  | "view_context"
  | "show_references" =>
    Some({
      category: Read,
      signifier: None,
      jump_paths: [],
      persists: false,
    })
  | "expand"
  | "collapse" => paths_tool(~category=View, ~persists=true)
  | "place_syntax_projector" =>
    paths_tool_with_kind(~category=Projector, ~persists=true)
  | "toggle_syntax_projector" =>
    paths_tool_with_kind(~category=Projector, ~persists=true)
  | "remove_syntax_projector" =>
    paths_tool(~category=Projector, ~persists=false)
  | "place_probe"
  | "toggle_probe" => paths_tool(~category=Probe, ~persists=true)
  | "remove_probe" => paths_tool(~category=Probe, ~persists=false)
  | "place_statics"
  | "toggle_statics" => paths_tool(~category=Statics, ~persists=true)
  | "remove_statics" => paths_tool(~category=Statics, ~persists=false)
  | "create_new_task" =>
    let title =
      switch (get_field(args, "task")) {
      | Some(task_json) => get_string_field(task_json, "title")
      | None => None
      };
    workbench(~signifier=Option.map(truncate_free_text, title));
  | "add_new_subtask_to_active_task" =>
    let title =
      switch (get_field(args, "subtask")) {
      | Some(st_json) => get_string_field(st_json, "title")
      | None => None
      };
    workbench(~signifier=Option.map(truncate_free_text, title));
  | "set_active_task"
  | "set_active_subtask" =>
    let title = get_string_field(args, "title");
    workbench(~signifier=Option.map(truncate_free_text, title));
  | "mark_active_subtask_complete" =>
    let summary = get_string_field(args, "summary");
    workbench(~signifier=Option.map(truncate_free_text, summary));
  | "mark_active_task_failed"
  | "mark_active_subtask_failed" =>
    let reason = get_string_field(args, "reason");
    workbench(~signifier=Option.map(truncate_free_text, reason));
  | "reorder_subtasks_in_active_task" =>
    let count =
      switch (get_string_list_field(args, "subtasks_ordering")) {
      | Some(xs) => List.length(xs)
      | None => 0
      };
    workbench(~signifier=Some(string_of_int(count) ++ " subtasks"));
  | "unset_active_task"
  | "unset_active_subtask"
  | "mark_active_task_complete"
  | "mark_active_task_incomplete"
  | "mark_active_subtask_incomplete" => workbench(~signifier=None)
  | "update_active_task"
  | "update_active_subtask" =>
    let sig_text =
      switch (
        get_string_field(args, "new_title"),
        get_string_field(args, "new_description"),
      ) {
      | (Some(t), _) => Some(t)
      | (None, Some(d)) => Some(d)
      | (None, None) => None
      };
    workbench(~signifier=Option.map(truncate_free_text, sig_text));
  | "delete_task"
  | "delete_subtask" =>
    let title = get_string_field(args, "title");
    workbench(~signifier=Option.map(truncate_free_text, title));
  | _ => None
  };
};
