include Id.Map;
type t = Id.Map.t(Language.Any.t);

let add_all = (ids: list(Id.t), tm: Language.Any.t, map: t) =>
  ids |> List.fold_left((map, id) => add(id, tm, map), map);
