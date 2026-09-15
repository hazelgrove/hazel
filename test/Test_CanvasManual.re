open Alcotest;
module L = Web.CanvasLayout;
module G = Web.CanvasGraph;
let at = (l: L.t, key) =>
  List.find((n: L.node_layout) => n.node.key == key, l.nodes).p;
let xy = (p: L.pos) => (p.x, p.y);
let same = (label, a: L.pos, b: L.pos) =>
  check(pair(float(0.001), float(0.001)), label, xy(a), xy(b));
let graph = () => Test_CanvasGraphFold.graph_of(Test_CanvasGraphFold.prog);
let move_without_bystanders = () => {
  let g = graph();
  let base = L.layout(~cards=[], g);
  let satellites =
    List.filter_map(
      (n: G.tynode) =>
        switch (n.sat) {
        | Some(("Pos", _)) => Some(n.key)
        | _ => None
        },
      g.nodes,
    );
  check(bool, "fixture includes attached terminals", true, satellites != []);
  List.iter(
    ((dx, dy)) => {
      let next =
        L.layout(
          ~cards=[],
          ~origin_override=Some(base.origin),
          ~offsets=[("Pos", (dx, dy))],
          g,
        );
      List.iter(
        (n: L.node_layout) => {
          let should_move =
            n.node.key == "Pos" || List.mem(n.node.key, satellites);
          let expected: L.pos =
            should_move
              ? {
                x: n.p.x +. dx,
                y: n.p.y +. dy,
              }
              : n.p;
          same("drag: " ++ n.node.key, expected, at(next, n.node.key));
        },
        base.nodes,
      );
    },
    [(280., 84.), ((-280.), 140.), (0., (-280.)), (0., 0.)],
  );
};
let pinned_and_previously_moved = () => {
  let g = graph();
  let base = L.layout(~cards=[], g);
  let pos = at(base, "Pos");
  let offsets = [("Deck", (84., 140.))];
  let fixed =
    List.hd(List.filter((n: G.tynode) => n.sat != None, g.nodes)).key;
  let fp = at(base, fixed);
  let pins = [("Pos", xy(pos)), (fixed, xy(fp))];
  let before =
    L.layout(
      ~cards=[],
      ~origin_override=Some(base.origin),
      ~offsets,
      ~pins,
      g,
    );
  let after =
    L.layout(
      ~cards=[],
      ~origin_override=Some(base.origin),
      ~offsets,
      ~pins=[("Pos", (pos.x +. 280., pos.y -. 140.)), (fixed, xy(fp))],
      g,
    );
  same(
    "explicit satellite pin stays fixed",
    at(before, fixed),
    at(after, fixed),
  );
  same(
    "previously moved node stays fixed",
    at(before, "Deck"),
    at(after, "Deck"),
  );
  List.iter(
    (n: L.node_layout) =>
      if (n.node.key != "Pos" && n.node.sat == None) {
        same("pin drag: " ++ n.node.key, n.p, at(after, n.node.key));
      },
    before.nodes,
  );
};
let tests = (
  "Canvas manual placement",
  [
    test_case(
      "dragging leaves unrelated nodes fixed and carries terminals",
      `Quick,
      move_without_bystanders,
    ),
    test_case(
      "pinned drag respects explicit terminal pins and previous placement",
      `Quick,
      pinned_and_previously_moved,
    ),
  ],
);
