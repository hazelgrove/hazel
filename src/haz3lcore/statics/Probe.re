open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type binding_tree_frame' =
  | Single
  | Fork;
[@deriving (show({with_path: false}), sexp, yojson)]
type binding_tree_frame = {
  id: Id.t,
  frame: binding_tree_frame',
};
let info_to_frame = (info: Info.t): option(binding_tree_frame') =>
  switch (info) {
  | InfoExp({cls: Let, _}) => Some(Single)
  | InfoExp({cls: Match, _}) => Some(Single)
  | InfoExp({cls: Fun, _}) => Some(Fork)
  | InfoExp({cls: _, _}) => None
  | InfoPat(_)
  | InfoTyp(_)
  | InfoTPat(_) => None
  };

let get_binding_stack =
    (id: Id.t, map: Statics.map): list(binding_tree_frame) =>
  switch (Id.Map.find_opt(id, map) |> Option.map(Info.ancestors_of)) {
  | Some(ancestors) =>
    ancestors
    |> List.map(id => (id, Id.Map.find_opt(id, map)))
    |> List.map(((id, x)) =>
         x
         |> OptUtil.and_then(info_to_frame)
         |> Option.map(frame => {id, frame})
       )
    |> List.filter_map(x => x)
  | None => []
  };

let rec get_exp_parent = (map: Statics.map, id: Id.t): option(Id.t) =>
  switch (Id.Map.find_opt(id, map)) {
  | None => None
  | Some(InfoExp(_)) => Some(id)
  | Some(info) =>
    switch (Info.ancestors_of(info)) {
    | [] => None
    | [pid, ..._] => get_exp_parent(map, pid)
    }
  };
