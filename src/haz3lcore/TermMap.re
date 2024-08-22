include Id.Map;
type t = Id.Map.t(Any.t(IdTag.t));

let add_all = (ids: list(Id.t), tm: Any.t(IdTag.t), map: t) =>
  ids |> List.fold_left((map, id) => add(id, tm, map), map);
