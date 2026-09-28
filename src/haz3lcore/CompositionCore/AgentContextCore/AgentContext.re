open Util;
open HighLevelNodeMap.Public;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    // Store paths, not ids, as ids may change with binding edits
    // Definitely a thought experiment, can revisit in future
    expanded_paths: list(string),
    /* Jev-owned view (docs/notes/jev-nav/plan.md §4): replaced wholesale on
       each new intent, never touches [expanded_paths], so a re-selection
       cannot close what the model deliberately opened. Defaulted so chats
       persisted before this field still load. */
    [@yojson.default []] [@sexp.default []]
    suggested_paths: list(string),
  };
};

module Utils = {
  let init = (): Model.t => {
    {
      expanded_paths: [],
      suggested_paths: [],
    };
  };

  let add_paths = (paths: list(string), agent_view: Model.t): Model.t => {
    ...agent_view,
    expanded_paths: List.append(paths, agent_view.expanded_paths),
  };

  /* Also drops Jev suggestions: the model outranks Jev, and a collapse that
     left the binding visible would cost the model another turn. */
  let remove_paths = (paths: list(string), agent_view: Model.t): Model.t => {
    let keep = p => !List.mem(p, paths);
    {
      expanded_paths: List.filter(keep, agent_view.expanded_paths),
      suggested_paths: List.filter(keep, agent_view.suggested_paths),
    };
  };

  let set_suggested = (paths: list(string), agent_view: Model.t): Model.t => {
    ...agent_view,
    suggested_paths: paths,
  };

  /** Additive modify_view: Jev's new picks join what it opened before. */
  let add_suggested = (paths: list(string), agent_view: Model.t): Model.t => {
    ...agent_view,
    suggested_paths:
      agent_view.suggested_paths
      @ List.filter(p => !List.mem(p, agent_view.suggested_paths), paths),
  };

  /** Every binding the renderer leaves unfolded: model-opened ∪ Jev-opened. */
  let open_paths = (agent_view: Model.t): list(string) =>
    agent_view.expanded_paths
    @ List.filter(
        p => !List.mem(p, agent_view.expanded_paths),
        agent_view.suggested_paths,
      );

  /** One-line tool result for modify_view. The snapshot already shows the
      code, so this only names what is open. */
  let names = (paths: list(string)): string =>
    paths == [] ? "(none)" : String.concat(", ", paths);

  let view_summary = (agent_view: Model.t): string =>
    "open: " ++ names(open_paths(agent_view));

  /** modify_view's result: what is open now, and what this call opened, so
      the planner sees whether the call changed anything. */
  let view_change_summary = (~before: Model.t, after: Model.t): string => {
    let was_open = open_paths(before);
    view_summary(after)
    ++ " · added: "
    ++ names(List.filter(p => !List.mem(p, was_open), open_paths(after)));
  };

  let freshen_paths = (model: Model.t, node_map: HighLevelNodeMap.t): Model.t => {
    // Removes stale references to outdated paths
    let live = (path: string) =>
      Option.is_some(path_to_id_opt(node_map, path));
    {
      expanded_paths: List.filter(live, model.expanded_paths),
      suggested_paths: List.filter(live, model.suggested_paths),
    };
  };
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | Expand(list(string))
    | Collapse(list(string))
    | SetSuggested(list(string))
    | AddSuggested(list(string));

  let update = (action: action, model: Model.t): Model.t => {
    switch (action) {
    | Expand(paths) => Utils.add_paths(paths, model)
    | Collapse(paths) => Utils.remove_paths(paths, model)
    | SetSuggested(paths) => Utils.set_suggested(paths, model)
    | AddSuggested(paths) => Utils.add_suggested(paths, model)
    };
  };
};
