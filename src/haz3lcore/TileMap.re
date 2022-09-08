// TODO(d) rename to reflect grout

type elem =
  | T(Tile.t)
  | G(Grout.t);

include Id.Map;
type t = Id.Map.t(elem);

let rec mk = (seg: Segment.t): t =>
  seg
  |> List.filter_map(
       fun
       | Piece.Whitespace(_) => None
       | Grout(g) => Some(singleton(g.id, G(g)))
       | Tile(t) => {
           let t_map = singleton(t.id, T(t));
           let kid_map =
             t.children |> List.map(mk) |> List.fold_left(disj_union, empty);
           Some(disj_union(t_map, kid_map));
         },
     )
  |> List.fold_left(disj_union, empty);
