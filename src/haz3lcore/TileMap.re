include Base.TileMap;

// tail-recursive
let rec mk = (~map=empty, seg: Segment.t): t =>
  Segment.tiles(seg)
  |> List.fold_left(
       (map, t: Tile.t) => {
         t.children
         |> List.fold_left((map, kid) => mk(~map, kid), add(t.id, t, map))
       },
       map,
     );
