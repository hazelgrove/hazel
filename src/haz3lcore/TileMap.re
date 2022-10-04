// TODO(d) rename to reflect grout

type elem =
  | T(Tile.t)
  | G(Grout.t);

include Id.Map;
type t = Id.Map.t(elem);

// tail-recursive
let rec mk = (~map=empty, seg: Segment.t): t =>
  seg
  |> List.fold_left(
       (map, p: Piece.t) =>
         switch (p) {
         | Whitespace(_) => map
         | Grout(g) => add(g.id, G(g), map)
         | Tile(t) =>
           t.children
           |> List.fold_left(
                (map, kid) => mk(~map, kid),
                add(t.id, T(t), map),
              )
         },
       map,
     );
