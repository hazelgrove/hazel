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

/* Outlines of spans of text laid out on a monospace grid: a row is a run of
   columns, a span is a list of rows. */
module Rows = {
  type row = {
    row: int,
    left: int,
    right: int /* exclusive */
  };

  type bbox = {
    top: int,
    bottom: int, /* inclusive */
    left: int,
    right: int /* exclusive */
  };

  let bbox = (rows: list(row)): option(bbox) =>
    switch (rows) {
    | [] => None
    | [first, ...rest] =>
      Some(
        List.fold_left(
          (bb, r) =>
            {
              top: min(bb.top, r.row),
              bottom: max(bb.bottom, r.row),
              left: min(bb.left, r.left),
              right: max(bb.right, r.right),
            },
          {
            top: first.row,
            bottom: first.row,
            left: first.left,
            right: first.right,
          },
          rest,
        ),
      )
    };

  /* A row's columns moved `inset_x` inward on both sides; a row narrower than
     twice the inset collapses to its midline rather than crossing itself. */
  let inset_cols = (~inset_x: float, {left, right, _}: row): (float, float) => {
    let (left, right) = (float_of_int(left), float_of_int(right));
    let mid = (left +. right) /. 2.;
    (min(left +. inset_x, mid), max(right -. inset_x, mid));
  };

  /* Consecutive rows whose inset columns overlap, as groups. A step between
     rows that share no columns would make the outline cross itself. */
  let split = (~inset_x: float, rows: list(row)): list(list(row)) => {
    let joins = (a: row, b: row) => {
      let (al, ar) = inset_cols(~inset_x, a);
      let (bl, br) = inset_cols(~inset_x, b);
      b.row == a.row + 1 && al < br && bl < ar;
    };
    List.fold_left(
      (groups, r) =>
        switch (groups) {
        | [[prev, ...group], ...rest] when joins(prev, r) => [
            [r, prev, ...group],
            ...rest,
          ]
        | _ => [[r], ...groups]
        },
      [],
      rows,
    )
    |> List.rev_map(List.rev);
  };

  let sign = (a: float, b: float) => a > b ? 1. : a < b ? (-1.) : 0.;

  /* The outline of one group from `split`, every edge moved `inset_x` or
     `inset_y` toward the interior, traced clockwise from the top-left in
     units of columns and rows relative to the group's bounding box. A step
     between two rows sits on their shared boundary, moved into whichever row
     the step cuts into, so insetting a child by more than its parent keeps
     all of their edges apart. */
  let inset_outline =
      (~inset_x: float, ~inset_y: float, rows: list(row)): Path.t =>
    switch (bbox(rows)) {
    | None => []
    | Some(bb) =>
      let origin = float_of_int(bb.left);
      let cols =
        rows
        |> List.map(r => {
             let (l, r) = inset_cols(~inset_x, r);
             (l -. origin, r -. origin);
           })
        |> Array.of_list;
      let n = Array.length(cols);
      let (left, right) = ((i => fst(cols[i])), (i => snd(cols[i])));
      let y = i => float_of_int(i);
      let top =
        Path.[
          M({
            x: left(0),
            y: inset_y,
          }),
          H({x: right(0)}),
        ];
      let right_side =
        List.init(n, i =>
          if (i == n - 1) {
            Path.[V({y: y(n) -. inset_y})];
          } else if (right(i) != right(i + 1)) {
            Path.[
              V({y: y(i + 1) -. inset_y *. sign(right(i), right(i + 1))}),
              H({x: right(i + 1)}),
            ];
          } else {
            [];
          }
        )
        |> List.concat;
      let bottom = Path.[H({x: left(n - 1)})];
      let left_side =
        List.init(n, j => n - 1 - j)
        |> List.map(i =>
             if (i == 0) {
               Path.[V({y: inset_y})];
             } else if (left(i) != left(i - 1)) {
               Path.[
                 V({y: y(i) -. inset_y *. sign(left(i), left(i - 1))}),
                 H({x: left(i - 1)}),
               ];
             } else {
               [];
             }
           )
        |> List.concat;
      top @ right_side @ bottom @ left_side @ [Path.Z];
    };
};
