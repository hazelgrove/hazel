open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = {
  // Store paths, not ids, as ids may change with binding edits
  // Definitely a thought experiment, can revisit in future
  expanded_paths: list(string),
};

let init = {expanded_paths: []};

let add_paths = (paths: list(string), agent_view: t): t => {
  expanded_paths: List.append(paths, agent_view.expanded_paths),
};

let remove_paths = (paths: list(string), agent_view: t): t => {
  expanded_paths:
    List.filter(p => !List.mem(p, paths), agent_view.expanded_paths),
};
