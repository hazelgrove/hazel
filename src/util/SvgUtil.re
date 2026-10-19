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
     keep every edge of a child apart from its parent's. */
  let outline = (boxes: list(box)): Path.t =>
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
      @ Path.[V({y: first.top}), Z];
    };
};
