open Alcotest;
module GL = Util.GraphLayout;

/* Within-rank ordering is damped (Spec.order_hysteresis): a node passes a
 * neighbor only on a clear barycenter win, so weak pulls and ties keep
 * program order while strong pulls still reorganize. Pins the behavior
 * the constellation's layout stability relies on.
 *
 * Scenario: sources a, b, c (rank 0) feed x, y (rank 1) with
 *   b -> x,  a -> y,  b -> y.
 * Barycenters put b half a row above a (0.5 vs 1.0): a WEAK pull. The
 * plain sort flips them; the damped sort keeps a, b, c in program order. */

let node = (id: string): GL.Spec.node => {
  id,
  radius: 20.,
  extent_above: 0.,
  extent_below: 0.,
};
let edge = ((src, dst): (string, string)): GL.Spec.edge => {
  src,
  dst,
  ranked: true,
};
let spec = (~hysteresis, nodes, edges): GL.Spec.t => {
  ...GL.Spec.default,
  nodes: List.map(node, nodes),
  edges: List.map(edge, edges),
  order_hysteresis: hysteresis,
};

/* ids at a rank, top to bottom */
let column = (res: GL.result, r: int): list(string) =>
  res.ranks
  |> List.filter(((_, rk)) => rk == r)
  |> List.map(fst)
  |> List.sort((a, b) => {
       let y = id =>
         switch (GL.pos_of(res, id)) {
         | Some(p) => p.y
         | None => 0.
         };
       compare(y(a), y(b));
     });

let sources = ["a", "b", "c"];
let weak_edges = [("b", "x"), ("a", "y"), ("b", "y")];

let tests = [
  test_case(
    "weak pull keeps program order",
    `Quick,
    () => {
      let res =
        GL.layout(spec(~hysteresis=0.6, sources @ ["x", "y"], weak_edges));
      check(list(string), "rank 0 order", ["a", "b", "c"], column(res, 0));
      check(list(string), "rank 1 order", ["x", "y"], column(res, 1));
    },
  ),
  test_case(
    "hysteresis 0 is the plain barycenter sort",
    `Quick,
    () => {
      let res =
        GL.layout(spec(~hysteresis=0., sources @ ["x", "y"], weak_edges));
      check(
        list(string),
        "rank 0 flips on a half-row pull",
        ["b", "a", "c"],
        column(res, 0),
      );
    },
  ),
  test_case(
    "strong pull still reorders",
    `Quick,
    () => {
      /* b -> x, a -> y: a full row apart, so b climbs above a even damped */
      let res =
        GL.layout(
          spec(
            ~hysteresis=0.6,
            sources @ ["x", "y"],
            [("b", "x"), ("a", "y")],
          ),
        );
      check(list(string), "rank 0 order", ["b", "a", "c"], column(res, 0));
    },
  ),
  test_case(
    "appending a definition leaves the prefix alone",
    `Quick,
    () => {
      let before =
        GL.layout(spec(~hysteresis=0.6, sources @ ["x", "y"], weak_edges));
      let after =
        GL.layout(
          spec(
            ~hysteresis=0.6,
            sources @ ["x", "y", "z"],
            weak_edges @ [("c", "z")],
          ),
        );
      check(
        list(string),
        "rank 0 unchanged",
        column(before, 0),
        column(after, 0),
      );
      check(
        list(string),
        "rank 1 prefix unchanged, z appended",
        column(before, 1) @ ["z"],
        column(after, 1),
      );
    },
  ),
];
