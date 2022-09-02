include Id.Map;
type t = Id.Map.t(Term.t);

let add_all = (ids: list(Id.t), tm: Term.t, map: t) =>
  ids |> List.fold_left((map, id) => add(id, tm, map), map);
