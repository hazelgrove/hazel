module Vdom = Virtual_dom.Vdom;

module Path = {
  /**
   * SVG <path> element
   * https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths
   */
  type t = list(command)
  /**
   * Path commands. Constructors with underscore
   * suffix correspond to lower-case variants.
   */
  and command =
    | M({
        x: float,
        y: float,
      })
    | M_({
        dx: float,
        dy: float,
      })
    | H_({dx: float})
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

  let empty = [];
  let cat = (@);
  let cats = List.concat;

  let string_of_flag =
    fun
    | false => "0"
    | true => "1";

  let string_of_command =
    fun
    | M({x, y}) =>
      Printf.sprintf(
        "M %s %s",
        FloatUtil.to_string_zero(x),
        FloatUtil.to_string_zero(y),
      )
    | M_({dx, dy}) =>
      Printf.sprintf(
        "m %s %s",
        FloatUtil.to_string_zero(dx),
        FloatUtil.to_string_zero(dy),
      )
    | H_({dx}) => Printf.sprintf("h %s", FloatUtil.to_string_zero(dx))
    | V_({dy}) => Printf.sprintf("v %s", FloatUtil.to_string_zero(dy))
    | A_({rx, ry, x_axis_rotation, large_arc_flag, sweep_flag, dx, dy}) =>
      Printf.sprintf(
        "a %s %s %s %s %s %s %s",
        FloatUtil.to_string_zero(rx),
        FloatUtil.to_string_zero(ry),
        FloatUtil.to_string_zero(x_axis_rotation),
        string_of_flag(large_arc_flag),
        string_of_flag(sweep_flag),
        FloatUtil.to_string_zero(dx),
        FloatUtil.to_string_zero(dy),
      );

  let view = (attrs: list(Vdom.Attr.t), path: t): Vdom.Node.t => {
    let buffer = Buffer.create(List.length(path) * 20);
    path
    |> List.iter(cmd => {
         Buffer.add_string(buffer, string_of_command(cmd));
         Buffer.add_string(buffer, " ");
       });
    Vdom.(
      Node.create_svg(
        "path",
        [Attr.create("d", Buffer.contents(buffer)), ...attrs],
        [],
      )
    );
  };
};
