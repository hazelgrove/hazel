include Id.Map;
type t = Id.Map.t(Tile.t(Id.t));

// tail-recursive
let rec mk = (~map=empty, seg: Segment.t(Id.t)): t =>
  Segment.tiles(seg)
  |> List.fold_left(
       (map, t: Tile.t(Id.t)) => {
         t.children
         |> List.fold_left(
              (map, kid) => mk(~map, kid),
              add(t.extra, t, map),
            )
       },
       map,
     );
