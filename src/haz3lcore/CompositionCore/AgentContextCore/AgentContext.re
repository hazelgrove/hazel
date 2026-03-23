open Util;
open HighLevelNodeMap.Public;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    // Store paths, not ids, as ids may change with binding edits
    // Definitely a thought experiment, can revisit in future
    expanded_paths: list(string),
  };
};

module Utils = {
  let init = (): Model.t => {
    {expanded_paths: []};
  };

  let add_paths = (paths: list(string), agent_view: Model.t): Model.t => {
    expanded_paths: List.append(paths, agent_view.expanded_paths),
  };

  let remove_paths = (paths: list(string), agent_view: Model.t): Model.t => {
    expanded_paths:
      List.filter(p => !List.mem(p, paths), agent_view.expanded_paths),
  };

  let freshen_paths = (model: Model.t, node_map: HighLevelNodeMap.t): Model.t => {
    {
      // Removes stale references to outdated paths

      expanded_paths:
        List.filter(
          (path: string) => {
            switch (path_to_id_opt(node_map, path)) {
            | Some(_) => true
            | None => false
            }
          },
          model.expanded_paths,
        ),
    };
  };
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | Expand(list(string))
    | Collapse(list(string));

  let update = (action: action, model: Model.t): Model.t => {
    switch (action) {
    | Expand(paths) => Utils.add_paths(paths, model)
    | Collapse(paths) => Utils.remove_paths(paths, model)
    };
  };
};
