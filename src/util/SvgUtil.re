open Virtual_dom.Vdom;

let icon_size = 20.;

/* Takes a list of paths as strings, a viewport as a string, and an optional
   (string) transform to apply to each. */
let simple_icon = (~transform="", ~view: string, ds: list(string)) =>
  Node.create_svg(
    "svg",
    ~attrs=
      Attr.[
        create("viewBox", view),
        create("width", Printf.sprintf("%fpx", icon_size)),
        create("height", Printf.sprintf("%fpx", icon_size)),
        create("preserveAspectRatio", "none"),
      ],
    List.map(
      d =>
        Node.create_svg(
          "path",
          ~attrs=
            [Attr.create("d", d)]
            @ (transform == "" ? [] : [Attr.create("transform", transform)]),
          [],
        ),
      ds,
    ),
  );

module Point = {
  type t = {
    x: float,
    y: float,
  };
};

module Vector = {
  type t = {
    dx: float,
    dy: float,
  };
};

module Rect = {
  type t = {
    min: Point.t,
    width: float,
    height: float,
  };
};

module Path = {
  type t = list(cmd)
  and cmd =
    | Z
    | M(Point.t)
    | M_(Vector.t)
    | L(Point.t)
    | L_(Vector.t)
    | H({x: float})
    | H_({dx: float})
    | V({y: float})
    | V_({dy: float})
    | A_({
        rx: float,
        ry: float,
        x_axis_rotation: float,
        large_arc_flag: bool,
        sweep_flag: bool,
        dx: float,
        dy: float,
      });

  let cmdfudge = (~x=0., ~y=0., c: cmd): cmd => {
    let (h, v) = (x, y);
    switch (c) {
    | M({x, y}) =>
      M({
        x: x +. h,
        y: y +. v,
      })
    | L({x, y}) =>
      L({
        x: x +. h,
        y: y +. v,
      })
    | H({x}) => H({x: x +. h})
    | V({y}) => V({y: y +. v})
    | _ => c
    };
  };

  let m = (~x, ~y) =>
    M({
      x: Float.of_int(x),
      y: Float.of_int(y),
    });
  let h = (~x) => H({x: Float.of_int(x)});
  let v = (~y) => V({y: Float.of_int(y)});
  let scale_cmd = (~scale_x=1., ~scale_y=1.) =>
    fun
    | (Z | M(_) | L(_) | H(_) | V(_) | A_(_)) as cmd => cmd
    | M_({dx, dy}) =>
      M_({
        dx: scale_x *. dx,
        dy: scale_y *. dy,
      })
    | L_({dx, dy}) =>
      L_({
        dx: scale_x *. dx,
        dy: scale_y *. dy,
      })
    | H_({dx}) => H_({dx: scale_x *. dx})
    | V_({dy}) => V_({dy: scale_y *. dy});

  let scale_x = s => List.map(scale_cmd(~scale_x=s));
  let scale_y = s => List.map(scale_cmd(~scale_y=s));

  let reverse = List.rev_map(scale_cmd(~scale_x=-1., ~scale_y=-1.));

  /* The point a command sequence has reached, with the current subpath's
     first point for Z. */
  type cursor = {
    at: Point.t,
    start: Point.t,
  };

  let cursor_origin: cursor = {
    at: {
      x: 0.,
      y: 0.,
    },
    start: {
      x: 0.,
      y: 0.,
    },
  };

  let step = ({at, start}: cursor, cmd: cmd): cursor =>
    switch (cmd) {
    | Z => {
        at: start,
        start,
      }
    | M(p) => {
        at: p,
        start: p,
      }
    | M_({dx, dy}) =>
      let p =
        Point.{
          x: at.x +. dx,
          y: at.y +. dy,
        };
      {
        at: p,
        start: p,
      };
    | L(p) => {
        at: p,
        start,
      }
    | L_({dx, dy})
    | A_({dx, dy, _}) => {
        at: {
          x: at.x +. dx,
          y: at.y +. dy,
        },
        start,
      }
    | H({x}) => {
        at: {
          ...at,
          x,
        },
        start,
      }
    | H_({dx}) => {
        at: {
          ...at,
          x: at.x +. dx,
        },
        start,
      }
    | V({y}) => {
        at: {
          ...at,
          y,
        },
        start,
      }
    | V_({dy}) => {
        at: {
          ...at,
          y: at.y +. dy,
        },
        start,
      }
    };

  /* Re-express a path through `pt`, a map on absolute points. Relative
     commands are resolved against the point reached so far, so the result
     is absolute. `H` keeps the y it had reached and `V` the x, so a
     horizontal or vertical edge stays one when `pt` translates each cell
     differently. Arc radii are scaled by `scale`. */
  let transform = (~pt: Point.t => Point.t, ~scale: Vector.t, path: t): t => {
    let (_, cmds) =
      List.fold_left(
        ((cur, acc), cmd) => {
          let next = step(cur, cmd);
          let mapped =
            switch (cmd) {
            | Z => Z
            | M(_)
            | M_(_) => M(pt(next.at))
            | L(_)
            | L_(_) => L(pt(next.at))
            | H(_)
            | H_(_) => H({x: pt(next.at).x})
            | V(_)
            | V_(_) => V({y: pt(next.at).y})
            | A_({rx, ry, x_axis_rotation, large_arc_flag, sweep_flag, _}) =>
              let (from, to_) = (pt(cur.at), pt(next.at));
              A_({
                rx: rx *. scale.dx,
                ry: ry *. scale.dy,
                x_axis_rotation,
                large_arc_flag,
                sweep_flag,
                dx: to_.x -. from.x,
                dy: to_.y -. from.y,
              });
            };
          (next, [mapped, ...acc]);
        },
        (cursor_origin, []),
        path,
      );
    List.rev(cmds);
  };

  /* The extent of a path's points; an arc may bulge past it. */
  let bbox = (path: t): option(Rect.t) =>
    switch (path) {
    | [] => None
    | _ =>
      let (_, points) =
        List.fold_left(
          ((cur, acc), cmd) => {
            let next = step(cur, cmd);
            (next, [next.at, ...acc]);
          },
          (cursor_origin, []),
          path,
        );
      let xs = List.map((p: Point.t) => p.x, points);
      let ys = List.map((p: Point.t) => p.y, points);
      let lo = List.fold_left(min, infinity);
      let hi = List.fold_left(max, neg_infinity);
      Some(
        Rect.{
          min: {
            x: lo(xs),
            y: lo(ys),
          },
          width: hi(xs) -. lo(xs),
          height: hi(ys) -. lo(ys),
        },
      );
    };

  /* A closed polygon of absolute straight edges, its corners rounded by
     `radius`: at a corner one of whose edges is shorter than twice that,
     by half the shorter edge. The arc at each corner turns the way the
     polygon does there, so a concave corner is rounded too. */
  let rounded = (~radius: float, path: t): t => {
    let (_, vertices) =
      List.fold_left(
        ((cur, acc), cmd) => {
          let next = step(cur, cmd);
          switch (cmd) {
          | Z => (next, acc)
          | _ => (next, [next.at, ...acc])
          };
        },
        (cursor_origin, []),
        path,
      );
    /* a last vertex back at the first is the path returning to its start */
    let vertices =
      switch (List.rev(vertices), vertices) {
      | ([first, ..._], [last, ...rest])
          when Float.equal(first.x, last.x) && Float.equal(first.y, last.y) =>
        List.rev(rest)
      | _ => List.rev(vertices)
      };
    switch (vertices) {
    | []
    | [_]
    | [_, _] => path
    | _ =>
      let v = Array.of_list(vertices);
      let n = Array.length(v);
      let at = i => v[(i + n) mod n];
      let dist = (p: Point.t, q: Point.t) =>
        Float.hypot(q.x -. p.x, q.y -. p.y);
      let r = i =>
        min(
          radius,
          min(dist(at(i - 1), at(i)), dist(at(i), at(i + 1))) /. 2.,
        );
      /* `d` along the edge from `p` toward `q` */
      let toward = (p: Point.t, q: Point.t, d: float): Point.t => {
        let len = dist(p, q);
        len == 0.
          ? p
          : {
            x: p.x +. (q.x -. p.x) /. len *. d,
            y: p.y +. (q.y -. p.y) /. len *. d,
          };
      };
      let line = (from: Point.t, to_: Point.t): cmd =>
        if (Float.equal(from.y, to_.y)) {
          H({x: to_.x});
        } else if (Float.equal(from.x, to_.x)) {
          V({y: to_.y});
        } else {
          L(to_);
        };
      let start = toward(at(0), at(1), r(0));
      let (_, cmds) =
        List.fold_left(
          ((cur: Point.t, cmds), i) => {
            let (p, q, s) = (at(i - 1), at(i), at(i + 1));
            let ri = r(i);
            let before = toward(q, p, ri);
            let after = toward(q, s, ri);
            /* no edge is left between two arcs that meet */
            let cmds =
              Float.equal(cur.x, before.x) && Float.equal(cur.y, before.y)
                ? cmds : [line(cur, before), ...cmds];
            if (ri > 0.) {
              let cross =
                (q.x -. p.x) *. (s.y -. q.y) -. (q.y -. p.y) *. (s.x -. q.x);
              (
                after,
                [
                  A_({
                    rx: ri,
                    ry: ri,
                    x_axis_rotation: 0.,
                    large_arc_flag: false,
                    sweep_flag: cross > 0.,
                    dx: after.x -. before.x,
                    dy: after.y -. before.y,
                  }),
                  ...cmds,
                ],
              );
            } else {
              (before, cmds);
            };
          },
          (start, []),
          List.init(n, i => i + 1),
        );
      [M(start), ...List.rev(cmds)] @ [Z];
    };
  };

  let string_of_flag =
    fun
    | false => "0"
    | true => "1";

  let string_of_command =
    fun
    | Z => "Z"
    | M({x, y}) => Printf.sprintf("M %f %f", x, y)
    | M_({dx, dy}) => Printf.sprintf("m %f %f", dx, dy)
    | L({x, y}) => Printf.sprintf("L %f %f", x, y)
    | L_({dx, dy}) => Printf.sprintf("l %f %f", dx, dy)
    | H({x}) => Printf.sprintf("H %f", x)
    | H_({dx}) => Printf.sprintf("h %f", dx)
    | V({y}) => Printf.sprintf("V %f", y)
    | V_({dy}) => Printf.sprintf("v %f", dy)
    | A_({rx, ry, x_axis_rotation, large_arc_flag, sweep_flag, dx, dy}) =>
      Printf.sprintf(
        "a %f %f %f %s %s %f %f",
        rx,
        ry,
        x_axis_rotation,
        string_of_flag(large_arc_flag),
        string_of_flag(sweep_flag),
        dx,
        dy,
      );

  let view = (~attrs: Attrs.t, path: t): Node.t => {
    let buffer = Buffer.create(List.length(path) * 20);
    path
    |> List.iter(cmd => {
         Buffer.add_string(buffer, string_of_command(cmd));
         Buffer.add_string(buffer, " ");
       });
    Node.create_svg(
      "path",
      ~attrs=[Attr.create("d", Buffer.contents(buffer)), ...attrs],
      [],
    );
  };
};

/* Outlines of a span of text laid out in rows: a box per row, stacked top
   to bottom, in pixels or any other unit. */
module Rows = {
  type box = {
    row: int,
    top: float,
    bottom: float,
    left: float,
    right: float,
  };

  /* Consecutive rows whose boxes overlap horizontally, as groups: a step
     between boxes sharing no columns would make the outline cross itself. */
  let split = (boxes: list(box)): list(list(box)) =>
    List.fold_left(
      (groups, b) =>
        switch (groups) {
        | [[prev, ...group], ...rest]
            when
              b.row == prev.row
              + 1
              && prev.left < b.right
              && b.left < prev.right => [
            [b, prev, ...group],
            ...rest,
          ]
        | _ => [[b], ...groups]
        },
      [],
      boxes,
    )
    |> List.rev_map(List.rev);

  /* One closed path, clockwise from the top-left, around a group's boxes.
     Where a row is narrower than the one above it the step sits at the
     upper box's bottom, where wider at the lower box's top: boxes that
     overlap vertically outline their union, and boxes inset from their rows
     keep every edge of a child apart from its parent's. The corners are
     rounded by `radius` (Path.rounded). */
  let outline = (~radius=0., boxes: list(box)): Path.t =>
    switch (boxes) {
    | [] => []
    | [first, ...rest] =>
      let last = List.fold_left((_, b) => b, first, rest);
      let rec down = (boxes: list(box)) =>
        switch (boxes) {
        | []
        | [_] => []
        | [a, b, ...rest] =>
          (
            if (b.right < a.right) {
              Path.[V({y: a.bottom}), H({x: b.right})];
            } else if (b.right > a.right) {
              Path.[V({y: b.top}), H({x: b.right})];
            } else {
              [];
            }
          )
          @ down([b, ...rest])
        };
      /* `a` is the box above `b` */
      let rec up = (boxes: list(box)) =>
        switch (boxes) {
        | []
        | [_] => []
        | [b, a, ...rest] =>
          (
            if (a.left > b.left) {
              Path.[V({y: b.top}), H({x: a.left})];
            } else if (a.left < b.left) {
              Path.[V({y: a.bottom}), H({x: a.left})];
            } else {
              [];
            }
          )
          @ up([a, ...rest])
        };
      Path.[
        M({
          x: first.left,
          y: first.top,
        }),
        H({x: first.right}),
      ]
      @ down(boxes)
      @ Path.[V({y: last.bottom}), H({x: last.left})]
      @ up(List.rev(boxes))
      @ Path.[V({y: first.top}), Z]
      |> (radius > 0. ? Path.rounded(~radius) : Fun.id);
    };
};
