open Virtual_dom.Vdom;

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
    | M({x, y}) => M({x: x +. h, y: y +. v})
    | L({x, y}) => L({x: x +. h, y: y +. v})
    | H({x}) => H({x: x +. h})
    | V({y}) => V({y: y +. v})
    | _ => c
    };
  };

  let m = (~x, ~y) => M({x: Float.of_int(x), y: Float.of_int(y)});
  let l_ = (~dx, ~dy) => L_({dx: Float.of_int(dx), dy: Float.of_int(dy)});
  let h = (~x) => H({x: Float.of_int(x)});
  let h_ = (~dx) => H_({dx: Float.of_int(dx)});
  let v = (~y) => V({y: Float.of_int(y)});
  let v_ = (~dy) => V_({dy: Float.of_int(dy)});

  let scale_cmd = (~scale_x=1., ~scale_y=1.) =>
    fun
    | (Z | M(_) | L(_) | H(_) | V(_) | A_(_)) as cmd => cmd
    | M_({dx, dy}) => M_({dx: scale_x *. dx, dy: scale_y *. dy})
    | L_({dx, dy}) => L_({dx: scale_x *. dx, dy: scale_y *. dy})
    | H_({dx}) => H_({dx: scale_x *. dx})
    | V_({dy}) => V_({dy: scale_y *. dy});

  let scale = s => List.map(scale_cmd(~scale_x=s, ~scale_y=s));
  let scale_x = s => List.map(scale_cmd(~scale_x=s));
  let scale_y = s => List.map(scale_cmd(~scale_y=s));

  let reverse = List.rev_map(scale_cmd(~scale_x=-1., ~scale_y=-1.));

  let transpose_cmd = (v: Vector.t) =>
    fun
    | (Z | M_(_) | L_(_) | H_(_) | V_(_) | A_(_)) as cmd => cmd
    | M({x, y}) => M({x: x +. v.dx, y: y +. v.dy})
    | L({x, y}) => L({x: x +. v.dx, y: y +. v.dy})
    | H({x}) => H({x: x +. v.dx})
    | V({y}) => V({y: y +. v.dy});
  let transpose = v => List.map(transpose_cmd(v));

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
      [Attr.create("d", Buffer.contents(buffer)), ...attrs],
      [],
    );
  };
};
