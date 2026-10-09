open Alcotest;
module E = Web.CanvasLayoutExperiments;
module L = Web.CanvasLayout;
let item = (key, x, y): E.item => {
  key,
  p: {
    x,
    y,
  },
  w: 80.,
  h: 70.,
  anchor: None,
};
let fixtures = [
  item("a", 100., 100.),
  item("b", 300., 100.),
  item("c", 100., 300.),
];
let same = (msg, a: E.point, b: E.point) =>
  check(pair(float(0.001), float(0.001)), msg, (a.x, a.y), (b.x, b.y));
let at = (ns, k) => Option.get(E.at(ns, k)).p;
let tests = (
  "Canvas layout experiments",
  [
    test_case(
      "only private unknown components dock to their consumer",
      `Quick,
      () => {
        module G = Web.CanvasGraph;
        let unknown = G.mk_node(~kind=Ghost, ~label="?", "?component");
        let consumer = name =>
          G.mk_node(~kind=Alias, ~label=name, ~parts=[unknown.key], name);
        let base = Test_CanvasGraphFold.graph_of("()");
        let layout = hosts =>
          L.layout_impl({
            ...base,
            nodes: [unknown, ...hosts],
            edges: [],
            values: [],
          });
        let one = layout([consumer("Carrier")]);
        let at = l =>
          List.find(
            (n: L.node_layout) => n.node.key == unknown.key,
            l.L.nodes,
          );
        check(
          option(string),
          "private component has a host",
          Some("Carrier"),
          Option.map(fst, at(one).node.sat),
        );
        let shared = layout([consumer("Carrier"), consumer("Other")]);
        check(
          option(string),
          "shared component remains independent",
          None,
          Option.map(fst, at(shared).node.sat),
        );
      },
    ),
    test_case(
      "host fans remain local in every placement",
      `Quick,
      () => {
        let fan = [
          item("host", 240., 180.),
          {
            ...item("i0", 160., 160.),
            anchor: Some("host"),
          },
          {
            ...item("i1", 160., 200.),
            anchor: Some("host"),
          },
          item("peer", 540., 180.),
        ];
        List.iter(
          ((mode, _)) => {
            let result = E.place(mode, fan, [("host", "peer")], []);
            let h = at(result, "host");
            List.iter(
              ((k, dy)) => {
                let p = at(result, k);
                same(
                  mode ++ "/" ++ k,
                  {
                    x: (-80.),
                    y: dy,
                  },
                  {
                    x: p.x -. h.x,
                    y: p.y -. h.y,
                  },
                );
              },
              [("i0", (-20.)), ("i1", 20.)],
            );
          },
          E.modes,
        );
      },
    ),
    test_case(
      "feedback curves reserve a label corridor",
      `Quick,
      () => {
        let g = Test_CanvasGraphFold.graph_of(Test_CanvasGraphFold.prog);
        let l = L.layout_impl(g);
        let e =
          List.find((e: L.edge_layout) => e.edge.e_name == "step", l.edges);
        check(bool, "separate smooth handles", true, e.c1 != e.c2);
        check(bool, "label stays on its own function", true, e.on_wire);
      },
    ),
    test_case(
      "stable growth and deletion retain survivors",
      `Quick,
      () => {
        let before = E.place("stable", fixtures, [("a", "b")], []);
        let after =
          E.place(
            "stable",
            fixtures @ [item("d", 100., 100.)],
            [("a", "b"), ("b", "d")],
            before,
          );
        List.iter(
          (n: E.item) => same(n.key, n.p, at(after, n.key)),
          before,
        );
        check(
          bool,
          "new node clears existing nodes",
          false,
          List.exists(
            n => E.overlaps(n, Option.get(E.at(after, "d")), 20.),
            before,
          ),
        );
        let survivors = List.filter((n: E.item) => n.key != "b", after);
        let removed = E.place("stable", survivors, [], after);
        List.iter(
          (n: E.item) => same("delete " ++ n.key, n.p, at(removed, n.key)),
          survivors,
        );
      },
    ),
    test_case(
      "all placements preserve graph membership and finite coordinates",
      `Quick,
      () => {
      List.iter(
        ((mode, _)) => {
          let result = E.place(mode, fixtures, [("a", "b")], []);
          check(
            list(string),
            mode,
            ["a", "b", "c"],
            List.map((n: E.item) => n.key, result) |> List.sort(compare),
          );
          List.iter(
            (n: E.item) =>
              check(
                bool,
                "finite",
                true,
                Float.is_finite(n.p.x) && Float.is_finite(n.p.y),
              ),
            result,
          );
          check(
            bool,
            "deterministic",
            true,
            result == E.place(mode, fixtures, [("a", "b")], []),
          );
        },
        E.modes,
      )
    }),
    test_case(
      "circuit route detours around an intervening card",
      `Quick,
      () => {
        let obstacle = {
          ...item("card", 200., 100.),
          w: 120.,
          h: 120.,
        };
        let a: E.point = {
          x: 20.,
          y: 100.,
        }
        and b: E.point = {
          x: 380.,
          y: 100.,
        };
        let ps = E.route(~obstacles=[obstacle], ~src="a", ~dst="b", a, b);
        same("starts at source", a, List.hd(ps));
        same("ends at destination", b, List.hd(List.rev(ps)));
        List.iter(
          ((u: E.point, v: E.point)) => {
            check(bool, "axis aligned", true, u.x == v.x || u.y == v.y);
            let hit =
              u.x == v.x
                ? u.x > 140.
                  && u.x < 260.
                  && max(u.y, v.y) > 40.
                  && min(u.y, v.y) < 160.
                : u.y > 40.
                  && u.y < 160.
                  && max(u.x, v.x) > 140.
                  && min(u.x, v.x) < 260.;
            check(bool, "clear of card", false, hit);
          },
          E.segments(ps),
        );
      },
    ),
    test_case(
      "circuit routes separate shared corridors and leave rims outward",
      `Quick,
      () => {
        let a: E.point = {
          x: 20.,
          y: 100.,
        };
        let b: E.point = {
          x: 380.,
          y: 100.,
        };
        let first = E.route(~obstacles=[], ~src="a", ~dst="b", a, b);
        let second =
          E.route(
            ~obstacles=[],
            ~src="a",
            ~dst="b",
            ~occupied=[first],
            a,
            b,
          );
        check(
          bool,
          "second route leaves occupied horizontal corridor",
          true,
          List.exists((p: E.point) => abs_float(p.y -. 100.) > 10., second),
        );
        let vertical =
          E.route(
            ~obstacles=[item("a", 100., 100.), item("b", 100., 300.)],
            ~src="a",
            ~dst="b",
            {
              x: 100.,
              y: 140.,
            },
            {
              x: 100.,
              y: 260.,
            },
          );
        check(
          bool,
          "source stub points out",
          true,
          List.nth(vertical, 1).y > 140.,
        );
        check(
          bool,
          "destination stub points out",
          true,
          List.nth(List.rev(vertical), 1).y < 260.,
        );
      },
    ),
    test_case(
      "all modes keep manual dragging local",
      `Quick,
      () => {
        let g = Test_CanvasGraphFold.graph_of(Test_CanvasGraphFold.prog);
        Fun.protect(
          ~finally=
            () => {
              E.mode := "current";
              E.wires := "curves";
              E.scene := "";
            },
          () =>
            List.iter(
              ((mode, _)) =>
                List.iter(
                  wires => {
                    E.mode := mode;
                    E.wires := wires;
                    E.scene := "test/" ++ mode ++ wires;
                    E.reset();
                    let base = L.layout(~cards=[], g);
                    let moved =
                      L.layout(
                        ~cards=[],
                        ~origin_override=Some(base.origin),
                        ~offsets=[("Pos", (280., 84.))],
                        g,
                      );
                    List.iter(
                      (n: L.node_layout) =>
                        if (n.node.key != "Pos" && n.node.sat == None) {
                          let other =
                            List.find(
                              (q: L.node_layout) => q.node.key == n.node.key,
                              moved.nodes,
                            );
                          same(
                            mode ++ wires,
                            L.to_lab(n.p),
                            L.to_lab(other.p),
                          );
                        },
                      base.nodes,
                    );
                    let unchanged =
                      L.layout(
                        ~cards=[("Pos", (360., 250.))],
                        ~origin_override=Some(base.origin),
                        g,
                      );
                    List.iter(
                      (n: L.node_layout) => {
                        let other =
                          List.find(
                            (q: L.node_layout) => q.node.key == n.node.key,
                            unchanged.nodes,
                          );
                        same(
                          "card: " ++ mode,
                          L.to_lab(n.p),
                          L.to_lab(other.p),
                        );
                      },
                      base.nodes,
                    );
                  },
                  ["curves", "circuit"],
                ),
              E.modes,
            ),
        );
      },
    ),
  ],
);
