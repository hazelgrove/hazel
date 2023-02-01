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
let info_to_frame = (info: Statics.t): option(binding_tree_frame') =>
  switch (info) {
  | InfoExp({cls: Let, _}) => Some(Single)
  | InfoExp({cls: Match, _}) => Some(Single)
  | InfoExp({cls: Fun, _}) => Some(Fork)
  | InfoExp({cls: _, _}) => None
  | InfoPat(_)
  | InfoTyp(_)
  | InfoTPat(_)
  | Invalid(_) => None
  };
let ancestors = (info: Statics.t): option(Statics.ancestors) =>
  switch (info) {
  | InfoExp({ancestors, _})
  | InfoPat({ancestors, _})
  | InfoTyp({ancestors, _})
  | InfoTPat({ancestors, _}) => Some(ancestors)
  | Invalid(_) => None
  };
let get_binding_stack =
    (id: Id.t, map: Statics.map): list(binding_tree_frame) =>
  switch (Id.Map.find_opt(id, map) |> OptUtil.and_then(ancestors)) {
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
