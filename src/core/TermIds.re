open Util;

include Id.Map;
type t = Id.Map.t(list(Id.t));

let join = List.fold_left(disj_union, empty);

let rec mk = (seg: Segment.t) => {
  let rec go = (skel: Skel.t) => {
    let root = Skel.root(skel);
    let root_ids =
      Aba.get_as(root) |> List.map(List.nth(seg)) |> List.map(Piece.id);
    let root_map =
      root_ids |> List.map(id => singleton(id, root_ids)) |> join;
    let between_map = Aba.get_bs(root) |> List.map(go) |> join;
    let outside_map =
      switch (skel) {
      | Op(_) => empty
      | Pre(_, r) => go(r)
      | Post(l, _) => go(l)
      | Bin(l, _, r) => join([go(l), go(r)])
      };
    join([root_map, between_map, outside_map]);
  };

  let inner_map = Segment.children(seg) |> List.map(mk) |> join;
  let outer_map = go(Segment.skel(seg));
  join([outer_map, inner_map]);
};
