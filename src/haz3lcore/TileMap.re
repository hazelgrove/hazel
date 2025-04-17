include Id.Map;
type t('p) = Id.Map.t(Tile.t('p));

// tail-recursive
let rec mk = (~map=empty, seg: Segment.t('p)): t('p) =>
  Segment.tiles(seg)
  |> List.fold_left(
       (map, t: Tile.t('p)) => {
         t.children
         |> List.fold_left((map, kid) => mk(~map, kid), add(t.id, t, map))
       },
       map,
     );
