/* Opt-in layout lab. Research engines do not change the default. Placement consumes geometry,
   not editor state; histories are scoped to a slide and a variant. */
open Poly;

type point = {
  x: float,
  y: float,
};
type item = {
  key: string,
  p: point,
  w: float,
  h: float,
  anchor: option(string),
};
let modes = [
  ("current", "Current"),
  ("packed", "Packed groups"),
  ("stable", "Stable growth"),
  ("grid", "Grid"),
  ("relaxed", "Soft anchors"),
  ("flow", "Function layers"),
  ("dependencies", "Type layers"),
  ("elk", "ELK layers →"),
  ("elk-down", "ELK layers ↓"),
  ("cola", "CoLa modules"),
  ("cola-live", "CoLa anchored growth"),
];
let research = k =>
  List.mem(["elk", "elk-down", "cola", "cola-live"], k, ~equal=Poly.equal);
let revision = ref(0);
let on_result = ref(() => ());
let awaiting_fit = ref(false);
let mode = ref("current");
let wires = ref("curves");
let scene = ref("");
let generation = ref(0);
let histories: ref(list((string, list(item)))) = ref([]);
let known = s => List.Assoc.mem(modes, s, ~equal=Poly.equal);
let history_key = () => scene^ ++ "/" ++ mode^;
let previous = () =>
  Option.value(
    ~default=[],
    List.Assoc.find(histories^, history_key(), ~equal=Poly.equal),
  );
let reset = () => {
  histories := List.Assoc.remove(histories^, history_key(), ~equal=Poly.equal);
  incr(generation);
};
let remember = items => {
  histories :=
    [
      (history_key(), items),
      ...List.filteri(
           ~f=(i, _) => i < 20,
           List.Assoc.remove(histories^, history_key(), ~equal=Poly.equal),
         ),
    ];
};
let snap = x => Stdlib.Float.round(x /. 14.) *. 14.;
let distance = (a: point, b: point) => Float.hypot(a.x -. b.x, a.y -. b.y);
let at = (items, key) => List.find(~f=(n: item) => n.key == key, items);
let overlaps = (a: item, b: item, pad) =>
  Float.abs(a.p.x -. b.p.x) < (a.w +. b.w)
  /. 2.
  +. pad
  && Float.abs(a.p.y -. b.p.y) < (a.h +. b.h)
  /. 2.
  +. pad;
let links_of = (items, links) =>
  links
  @ List.filter_map(
      ~f=(n: item) => Option.map(~f=a => (n.key, a), n.anchor),
      items,
    );
let groups = (items, links) => {
  let rec grow = ks => {
    let next =
      List.dedup_and_sort(
        ~compare,
        ks
        @ List.concat_map(
            ~f=
              ((a, b)) =>
                List.mem(ks, a, ~equal=Poly.equal)
                  ? [b] : List.mem(ks, b, ~equal=Poly.equal) ? [a] : [],
            links,
          ),
      )
      |> List.filter(~f=k => at(items, k) != None);
    next == List.dedup_and_sort(~compare, ks) ? next : grow(next);
  };
  List.fold(items, ~init=[], ~f=(acc, n: item) =>
    List.exists(~f=c => List.mem(c, n.key, ~equal=Poly.equal), acc)
      ? acc : acc @ [grow([n.key])]
  );
};
/* Remove rows occupied only by other components, then shelf-pack component
   rectangles. This is a deliberately stateless comparison to stable growth. */
let packed = (items, links) => {
  let blocks =
    List.map(
      ~f=
        keys => {
          let ns =
            List.filter(
              ~f=(n: item) => List.mem(keys, n.key, ~equal=Poly.equal),
              items,
            );
          let minx =
            List.fold(ns, ~init=Float.infinity, ~f=(x, n: item) =>
              min(x, n.p.x -. n.w /. 2.)
            );
          /* Remove only genuinely empty strips. Rows in different columns
             may overlap in y; forcing a gap between every y-coordinate would
             turn a compact graph into a staircase. */
          let intervals =
            List.map(
              ~f=(n: item) => (n.p.y -. n.h /. 2., n.p.y +. n.h /. 2.),
              ns,
            )
            |> List.sort(~compare);
          let (_, gaps) =
            List.fold(
              intervals,
              ~init=(Float.neg_infinity, []),
              ~f=((end_y, gaps), (lo, hi)) =>
              (
                max(end_y, hi),
                lo -. end_y > 100. && end_y != Float.neg_infinity
                  ? gaps @ [(lo, lo -. end_y -. 100.)] : gaps,
              )
            );
          let miny =
            List.fold(ns, ~init=Float.infinity, ~f=(v, n: item) =>
              min(v, n.p.y -. n.h /. 2.)
            );
          let ns =
            List.map(
              ~f=
                (n: item) =>
                  {
                    ...n,
                    p: {
                      x: n.p.x -. minx,
                      y:
                        n.p.y
                        -. miny
                        -. List.fold(gaps, ~init=0., ~f=(v, (at, amount)) =>
                             n.p.y >= at ? v +. amount : v
                           ),
                    },
                  },
              ns,
            );

          let w =
            List.fold(ns, ~init=0., ~f=(v, n: item) =>
              max(v, n.p.x +. n.w /. 2.)
            );
          let h =
            List.fold(ns, ~init=0., ~f=(v, n: item) =>
              max(v, n.p.y +. n.h /. 2.)
            );
          (ns, w, h);
        },
      groups(items, links),
    );
  let area =
    List.fold(blocks, ~init=0., ~f=(a, (_, w, h)) =>
      a +. (w +. 60.) *. (h +. 60.)
    );
  let target = max(650., Float.sqrt(area *. 1.5));
  let (_, _, _, out) =
    List.fold(
      blocks,
      ~init=(70., 70., 0., []),
      ~f=((x, y, rowh, acc), (ns, w, h)) => {
        let (x, y, rowh) =
          x > 70. && x +. w > target
            ? (70., y +. rowh +. 70., 0.) : (x, y, rowh);
        (
          x +. w +. 70.,
          y,
          max(rowh, h),
          acc
          @ List.map(
              ~f=
                (n: item) =>
                  {
                    ...n,
                    p: {
                      x: snap(n.p.x +. x),
                      y: snap(n.p.y +. y),
                    },
                  },
              ns,
            ),
        );
      },
    );
  out;
};
/* Keep per-use terminals with their host rather than spending a grid cell
   on every Int/String leaf. A cluster occupies its full local envelope. */
let grid = (items, links) => {
  let rec root = (seen, n: item) =>
    switch (n.anchor) {
    | Some(k) when !List.mem(seen, k, ~equal=Poly.equal) =>
      switch (at(items, k)) {
      | Some(host) => root([n.key, ...seen], host)
      | None => n.key
      }
    | _ => n.key
    };
  let keys = groups(items, links) |> List.concat;
  let roots =
    List.filter_map(
      ~f=k => Option.map(~f=n => root([], n), at(items, k)),
      keys,
    )
    |> List.fold(~init=[], ~f=(acc, k) =>
         List.mem(acc, k, ~equal=Poly.equal) ? acc : acc @ [k]
       );
  let clusters =
    List.map(
      ~f=
        k => {
          let ns = List.filter(~f=(n: item) => root([], n) == k, items);
          let x0 =
            List.fold(ns, ~init=Float.infinity, ~f=(v, n: item) =>
              min(v, n.p.x -. n.w /. 2.)
            )
          and y0 =
            List.fold(ns, ~init=Float.infinity, ~f=(v, n: item) =>
              min(v, n.p.y -. n.h /. 2.)
            );
          let w =
            List.fold(ns, ~init=0., ~f=(v, n: item) =>
              max(v, n.p.x +. n.w /. 2. -. x0)
            )
          and h =
            List.fold(ns, ~init=0., ~f=(v, n: item) =>
              max(v, n.p.y +. n.h /. 2. -. y0)
            );
          (ns, x0, y0, w, h);
        },
      roots,
    );
  let columns =
    max(
      2,
      int_of_float(
        Float.round_up(
          Float.sqrt(float_of_int(List.length(clusters)) *. 1.3),
        ),
      ),
    );
  let w =
    List.fold(clusters, ~init=196., ~f=(v, (_, _, _, w, _)) =>
      max(v, w +. 75.)
    )
  and h =
    List.fold(clusters, ~init=154., ~f=(v, (_, _, _, _, h)) =>
      max(v, h +. 70.)
    );
  List.mapi(
    ~f=
      (i, (ns, x0, y0, _, _)) =>
        List.map(
          ~f=
            (n: item) =>
              {
                ...n,
                p: {
                  x:
                    snap(
                      70. +. float_of_int(i mod columns) *. w +. n.p.x -. x0,
                    ),
                  y:
                    snap(
                      70. +. float_of_int(i / columns) *. h +. n.p.y -. y0,
                    ),
                },
              },
          ns,
        ),
    clusters,
  )
  |> List.concat;
};
let stable = (items, links, old) => {
  let kept =
    List.filter_map(
      ~f=
        (n: item) =>
          Option.map(
            ~f=
              (p: item) =>
                {
                  ...n,
                  p: p.p,
                },
            at(old, n.key),
          ),
      items,
    );
  if (kept == []) {
    packed(items, links);
  } else {
    List.fold(items, ~init=kept, ~f=(placed, n: item) =>
      if (at(placed, n.key) != None) {
        placed;
      } else {
        let peers =
          List.filter_map(
            ~f=
              ((a, b)) =>
                a == n.key
                  ? at(placed, b) : b == n.key ? at(placed, a) : None,
            links,
          );
        let desired =
          peers == []
            ? {
              x: 100.,
              y: 100.,
            }
            : {
              x:
                List.fold(peers, ~init=0., ~f=(s, p: item) => s +. p.p.x)
                /. float_of_int(List.length(peers)),
              y:
                List.fold(peers, ~init=0., ~f=(s, p: item) => s +. p.p.y)
                /. float_of_int(List.length(peers)),
            };
        let candidates =
          List.init(12, ~f=r =>
            List.init(
              12,
              ~f=i => {
                let angle = float_of_int(i) *. Float.pi /. 6.;
                let radius = float_of_int(r + 1) *. 84.;
                {
                  x: snap(max(84., desired.x +. Float.cos(angle) *. radius)),
                  y:
                    snap(max(84., desired.y +. Float.sin(angle) *. radius)),
                };
              },
            )
          )
          |> List.concat;
        let score = p => {
          let candidate = {
            ...n,
            p,
          };
          let hits =
            List.fold(placed, ~init=0., ~f=(s, q) =>
              overlaps(candidate, q, 24.) ? s +. 1000000. : s
            );
          hits +. distance(p, desired) +. 0.05 *. (p.x +. p.y);
        };
        let best =
          List.fold(
            List.tl_exn(candidates),
            ~init=List.hd_exn(candidates),
            ~f=(a, b) =>
            score(b) < score(a) ? b : a
          );
        placed
        @ [
          {
            ...n,
            p: best,
          },
        ];
      }
    );
  };
};
/* Bounded relaxation, settled before animation. Old positions exert a soft
   pull; overlaps push apart. This intentionally trades stability for space. */
let relaxed = (items, links, old) => {
  let start = stable(items, links, old);
  let current = ref(start);
  for (_ in 1 to 28) {
    current :=
      List.map(
        ~f=
          (n: item) => {
            let target = Option.value(~default=n, at(old, n.key));
            let fx = ref((target.p.x -. n.p.x) *. 0.13)
            and fy = ref((target.p.y -. n.p.y) *. 0.13);
            List.iter(
              ~f=
                (q: item) =>
                  if (q.key != n.key && overlaps(n, q, 35.)) {
                    let dx = n.p.x -. q.p.x
                    and dy = n.p.y -. q.p.y;
                    let ox = (n.w +. q.w) /. 2. +. 35. -. Float.abs(dx)
                    and oy = (n.h +. q.h) /. 2. +. 35. -. Float.abs(dy);
                    if (ox < oy) {
                      fx := fx^ +. (dx >= 0. ? 1. : (-1.)) *. ox *. 0.3;
                    } else {
                      fy := fy^ +. (dy >= 0. ? 1. : (-1.)) *. oy *. 0.3;
                    };
                  },
              current^,
            );
            List.iter(
              ~f=
                ((a, b)) => {
                  let other =
                    a == n.key
                      ? at(current^, b) : b == n.key ? at(current^, a) : None;
                  switch (other) {
                  | Some(q) =>
                    let d = distance(n.p, q.p);
                    if (d > 220.) {
                      fx := fx^ +. (q.p.x -. n.p.x) *. 0.012;
                      fy := fy^ +. (q.p.y -. n.p.y) *. 0.012;
                    };
                  | None => ()
                  };
                },
              links,
            );
            {
              ...n,
              p: {
                x: max(n.w /. 2. +. 40., n.p.x +. max(-18., min(18., fx^))),
                y: max(n.h /. 2. +. 40., n.p.y +. max(-18., min(18., fy^))),
              },
            };
          },
        current^,
      );
  };
  List.map(
    ~f=
      (n: item) =>
        {
          ...n,
          p: {
            x: snap(n.p.x),
            y: snap(n.p.y),
          },
        },
    current^,
  );
};
/* Engines solve off-thread, returning complete geometry snapshots. Existing
   placements remain visible while pending. Pin/drag offsets are applied later. */
let engine_status = () => {
  Js_of_ocaml.(
    try(
      Js.to_string(
        Js.Unsafe.get(
          Js.Unsafe.get(Js.Unsafe.global, "__canvasResearchState"),
          "status",
        ),
      )
    ) {
    | _ => "Layout engine unavailable"
    }
  );
};
let engine_place = (kind, items, links, old, clusters, topology) => {
  let fallback = stable(items, links, old);
  Js_of_ocaml.(
    try({
      let fn = Js.Unsafe.get(Js.Unsafe.global, "__canvasResearchLayout");
      if (Js.to_string(Js.typeof(fn)) != "function") {
        fallback;
      } else {
        let node = (n: item) =>
          `Assoc([
            ("key", `String(n.key)),
            ("x", `Float(n.p.x)),
            ("y", `Float(n.p.y)),
            ("w", `Float(n.w)),
            ("h", `Float(n.h)),
          ]);
        let payload =
          Yojson.Safe.to_string(
            `Assoc([
              (
                "scope",
                `String(
                  scene^ ++ "/" ++ kind ++ "/" ++ string_of_int(generation^),
                ),
              ),
              ("kind", `String(kind)),
              ("topology", topology),
              ("items", `List(List.map(~f=node, items))),
              ("old", `List(List.map(~f=node, old))),
              (
                "links",
                `List(
                  List.map(
                    ~f=((a, b)) => `List([`String(a), `String(b)]),
                    links,
                  ),
                ),
              ),
              (
                "clusters",
                `List(
                  List.map(
                    ~f=ks => `List(List.map(~f=k => `String(k), ks)),
                    clusters,
                  ),
                ),
              ),
            ]),
          );
        let result =
          Js.Unsafe.fun_call(
            fn,
            [|
              Js.Unsafe.inject(Js.string(payload)),
              Js.Unsafe.inject(
                Js.Unsafe.callback(() => {
                  incr(revision);
                  on_result^();
                }),
              ),
            |],
          )
          |> Js.to_string
          |> Yojson.Safe.from_string;
        let float = j =>
          switch (j) {
          | `Float(f) => f
          | `Int(i) => float_of_int(i)
          | _ => failwith("coordinate")
          };
        let positions =
          Yojson.Safe.Util.to_list(result)
          |> List.map(~f=j => {
               let get = k => Yojson.Safe.Util.member(k, j);
               (
                 Yojson.Safe.Util.to_string(get("key")),
                 {
                   x: float(get("x")),
                   y: float(get("y")),
                 },
               );
             });
        if (positions != []) {
          awaiting_fit := false;
        };
        positions == []
          ? fallback
          : List.map(
              ~f=
                (n: item) =>
                  {
                    ...n,
                    p:
                      Option.value(
                        ~default=n.p,
                        List.Assoc.find(positions, n.key, ~equal=Poly.equal),
                      ),
                  },
              items,
            );
      };
    }) {
    | _ => fallback
    }
  );
};

/* Layout operates on host groups. A terminal remains at its local dock;
   it must not acquire an independent grid cell, relaxation force, or stable
   insertion slot. The root reserves the group's entire occupied envelope. */
let place = (~clusters=[], ~labels=[], kind, items, links, old) => {
  let topology =
    `List([
      `List(List.map(~f=k => `String(k), labels)),
      `List(List.map(~f=(n: item) => `String(n.key), items)),
      `List(
        List.map(~f=((a, b)) => `List([`String(a), `String(b)]), links),
      ),
    ]);

  let rec root = (seen, n: item) =>
    switch (n.anchor) {
    | Some(k) when !List.mem(seen, k, ~equal=Poly.equal) =>
      switch (at(items, k)) {
      | Some(host) => root([n.key, ...seen], host)
      | None => n.key
      }
    | _ => n.key
    };
  let root_of = k => Option.map(~f=n => root([], n), at(items, k));
  let roots = List.filter(~f=(n: item) => root([], n) == n.key, items);
  let units =
    List.map(
      ~f=
        (host: item) => {
          let members =
            List.filter(~f=(n: item) => root([], n) == host.key, items);
          let (left, top, right, bottom) =
            List.fold(
              members,
              ~init=(
                Float.infinity,
                Float.infinity,
                Float.neg_infinity,
                Float.neg_infinity,
              ),
              ~f=((left, top, right, bottom), n: item) =>
              (
                min(left, n.p.x -. n.w /. 2.),
                min(top, n.p.y -. n.h /. 2.),
                max(right, n.p.x +. n.w /. 2.),
                max(bottom, n.p.y +. n.h /. 2.),
              )
            );
          {
            ...host,
            p: {
              x: (left +. right) /. 2.,
              y: (top +. bottom) /. 2.,
            },
            w: right -. left,
            h: bottom -. top,
            anchor: None,
          };
        },
      roots,
    );
  let links =
    links_of(items, links)
    |> List.filter_map(~f=((a, b)) =>
         switch (root_of(a), root_of(b)) {
         | (Some(a), Some(b)) when a != b => Some((a, b))
         | _ => None
         }
       )
    |> List.dedup_and_sort(~compare);
  let old_units =
    List.filter_map(
      ~f=
        (n: item) =>
          switch (at(old, n.key), at(items, n.key)) {
          | (Some(previous), Some(host)) =>
            Some({
              ...n,
              p: {
                x: previous.p.x +. n.p.x -. host.p.x,
                y: previous.p.y +. n.p.y -. host.p.y,
              },
            })
          | _ => None
          },
      units,
    );

  let result =
    switch (kind) {
    | "packed"
    | "components" => packed(units, links)
    | "stable" => stable(units, links, old_units)
    | "grid" => grid(units, links)
    | "relaxed" => relaxed(units, links, old_units)
    | k when research(k) =>
      engine_place(
        k,
        units,
        links,
        old_units,
        List.map(
          ~f=
            ks =>
              List.filter_map(~f=root_of, ks)
              |> List.dedup_and_sort(~compare),
          clusters,
        ),
        topology,
      )
    | _ => units
    };
  List.map(
    ~f=
      (n: item) => {
        let k = root([], n);
        switch (at(units, k), at(result, k)) {
        | (Some(before), Some(after)) => {
            ...n,
            p: {
              x: n.p.x +. after.p.x -. before.p.x,
              y: n.p.y +. after.p.y -. before.p.y,
            },
          }
        | _ => n
        };
      },
    items,
  );
};

/* Bounded Manhattan candidate search. A route may use an outside corridor
   when its short alternatives hit nodes; no shared trunks imply junctions. */
let segments = points => {
  let rec loop = ps =>
    switch (ps) {
    | [a, b, ...rest] => [(a, b), ...loop([b, ...rest])]
    | _ => []
    };
  loop(points);
};
let route =
    (
      ~obstacles: list(item),
      ~src,
      ~dst,
      ~lane=0,
      ~occupied: list(list(point))=[],
      a: point,
      b: point,
    ) => {
  let pad = 12.
  and stub = min(30., max(8., distance(a, b) *. 0.2));
  let outward = (key, p: point, fallback) =>
    switch (at(obstacles, key)) {
    | None => {
        x: p.x +. fallback *. stub,
        y: p.y,
      }
    | Some(n) =>
      let dx = p.x -. n.p.x
      and dy = p.y -. n.p.y;
      Float.abs(dx) >= Float.abs(dy)
        ? {
          x: p.x +. (dx >= 0. ? stub : -. stub),
          y: p.y,
        }
        : {
          x: p.x,
          y: p.y +. (dy >= 0. ? stub : -. stub),
        };
    };
  let direction = b.x >= a.x ? 1. : (-1.);
  let s = outward(src, a, direction)
  and d = outward(dst, b, -. direction);
  let shift = float_of_int(lane) *. 18.;
  let xs = [
    (a.x +. b.x) /. 2. +. shift,
    min(a.x, b.x) -. 50. -. shift,
    max(a.x, b.x) +. 50. +. shift,
  ];
  let ys = [
    (a.y +. b.y) /. 2. +. shift,
    min(a.y, b.y) -. 50. -. shift,
    max(a.y, b.y) +. 50. +. shift,
  ];
  let mid = {
    x: (a.x +. b.x) /. 2.,
    y: (a.y +. b.y) /. 2.,
  };
  let near =
    List.sort(
      ~compare=
        (u: item, v: item) =>
          compare(distance(u.p, mid), distance(v.p, mid)),
      obstacles,
    )
    |> List.filteri(~f=(i, _) => i < 8);
  let xs =
    xs
    @ List.concat_map(
        ~f=
          (n: item) =>
            [n.p.x -. n.w /. 2. -. 24., n.p.x +. n.w /. 2. +. 24.],
        near,
      )
  and ys =
    ys
    @ List.concat_map(
        ~f=
          (n: item) =>
            [n.p.y -. n.h /. 2. -. 24., n.p.y +. n.h /. 2. +. 24.],
        near,
      );
  let candidates =
    List.map(
      ~f=
        x =>
          [
            a,
            s,
            {
              x,
              y: s.y,
            },
            {
              x,
              y: d.y,
            },
            d,
            b,
          ],
      xs,
    )
    @ List.map(
        ~f=
          y =>
            [
              a,
              s,
              {
                x: s.x,
                y,
              },
              {
                x: d.x,
                y,
              },
              d,
              b,
            ],
        ys,
      );
  let score = ps => {
    let nonzero =
      segments(ps) |> List.filter(~f=((p, q)) => distance(p, q) > 0.01);
    let rec turns = ss =>
      switch (ss) {
      | [(p, q), (r, s), ...rest] =>
        let dot = (q.x -. p.x) *. (s.x -. r.x) +. (q.y -. p.y) *. (s.y -. r.y);
        (dot < 0. ? 100000. : dot == 0. ? 16. : 0.)
        +. turns([(r, s), ...rest]);
      | _ => 0.
      };
    List.fold(
      segments(ps),
      ~init=turns(nonzero),
      ~f=(score, (u: point, v: point)) => {
        let hits =
          List.fold(obstacles, ~init=0., ~f=(hits, n: item) =>
            if (n.key == src && u == a || n.key == dst && v == b) {
              hits;
            } else {
              let left = n.p.x -. n.w /. 2. -. pad
              and right = n.p.x +. n.w /. 2. +. pad
              and top = n.p.y -. n.h /. 2. -. pad
              and bottom = n.p.y +. n.h /. 2. +. pad;
              let hit =
                Float.abs(u.x -. v.x) < 0.01
                  ? u.x > left
                    && u.x < right
                    && max(u.y, v.y) > top
                    && min(u.y, v.y) < bottom
                  : u.y > top
                    && u.y < bottom
                    && max(u.x, v.x) > left
                    && min(u.x, v.x) < right;
              hits +. (hit ? 100000. : 0.);
            }
          );
        let overlap =
          u == a || v == b
            ? 0.
            : List.fold(occupied, ~init=0., ~f=(sum, path) =>
                List.fold(
                  segments(path),
                  ~init=sum,
                  ~f=(sum, (p: point, q: point)) => {
                    let length =
                      Float.abs(u.x -. v.x) < 0.01
                      && Float.abs(p.x -. q.x) < 0.01
                      && Float.abs(u.x -. p.x) < 8.
                        ? max(
                            0.,
                            min(max(u.y, v.y), max(p.y, q.y))
                            -. max(min(u.y, v.y), min(p.y, q.y)),
                          )
                        : Float.abs(u.y -. v.y) < 0.01
                          && Float.abs(p.y -. q.y) < 0.01
                          && Float.abs(u.y -. p.y) < 8.
                            ? max(
                                0.,
                                min(max(u.x, v.x), max(p.x, q.x))
                                -. max(min(u.x, v.x), min(p.x, q.x)),
                              )
                            : 0.;
                    sum +. max(0., length -. 12.) *. 30.;
                  },
                )
              );
        let crossings =
          List.fold(occupied, ~init=0., ~f=(sum, path) =>
            List.fold(
              segments(path),
              ~init=sum,
              ~f=(sum, (p: point, q: point)) => {
                let cross = (a: point, b: point, c: point) =>
                  (b.x -. a.x) *. (c.y -. a.y) -. (b.y -. a.y) *. (c.x -. a.x);
                sum
                +. (
                  cross(u, v, p)
                  *. cross(u, v, q) < 0.
                  && cross(p, q, u)
                  *. cross(p, q, v) < 0.
                    ? 40. : 0.
                );
              },
            )
          );
        score +. distance(u, v) +. hits +. overlap +. crossings;
      },
    );
  };
  List.fold(
    List.tl_exn(candidates), ~init=List.hd_exn(candidates), ~f=(best, ps) =>
    score(ps) < score(best) ? ps : best
  );
};
let along = (points, t) => {
  let segs = segments(points);
  let total =
    List.fold(segs, ~init=0., ~f=(v, (a, b)) => v +. distance(a, b));
  let rec walk = (ss, remaining) =>
    switch (ss) {
    | [(a, b), ...rest] =>
      let d = distance(a, b);
      if (remaining <= d || rest == []) {
        let u = d < 0.001 ? 0. : min(1., remaining /. d);
        {
          x: a.x +. (b.x -. a.x) *. u,
          y: a.y +. (b.y -. a.y) *. u,
        };
      } else {
        walk(rest, remaining -. d);
      };
    | [] => {
        x: 0.,
        y: 0.,
      }
    };
  walk(segs, max(0., min(1., t)) *. total);
};
/* Fixed command topology across routes lets SVG morphs interpolate rather
   than popping when the chosen corridor changes. A small quadratic rounds
   each corner. Degenerate segments are retained deliberately. */
let path = points =>
  switch (points) {
  | [] => ""
  | [first, ...rest] =>
    let fmt = (p: point) => Printf.sprintf("%.1f,%.1f", p.x, p.y);
    let rec draw = (prev, ps) =>
      switch (ps) {
      | [p, next, ...tail] =>
        let back = distance(prev, p)
        and ahead = distance(p, next);
        let r = min(6., min(back /. 2., ahead /. 2.));
        let inset = (q: point, d) =>
          d < 0.001
            ? p
            : {
              x: p.x +. (q.x -. p.x) *. r /. d,
              y: p.y +. (q.y -. p.y) *. r /. d,
            };
        " L "
        ++ fmt(inset(prev, back))
        ++ " Q "
        ++ fmt(p)
        ++ " "
        ++ fmt(inset(next, ahead))
        ++ draw(p, [next, ...tail]);
      | [last] => " L " ++ fmt(last)
      | [] => ""
      };
    "M " ++ fmt(first) ++ draw(first, rest);
  };
