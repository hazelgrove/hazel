include Id.Map;
type t = Id.Map.t(Tile.t);

let rec mk = (seg: Segment.t): t =>
  Segment.tiles(seg)
  |> List.map((t: Tile.t) => {
       let t_map = singleton(t.id, t);
       let kid_map =
         t.children |> List.map(mk) |> List.fold_left(disj_union, empty);
       disj_union(t_map, kid_map);
     })
  |> List.fold_left(disj_union, empty);
