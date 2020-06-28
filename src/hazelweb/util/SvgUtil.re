module Vdom = Virtual_dom.Vdom;

module Path = {
  /**
   * SVG <path> element
   * https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths
   */

  type t = list(command)
  // constructors with underscore suffix
  // correspond to lower-case variants
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

  // non-standard
  type axis =
    | X
    | Y;
  type direction =
    | Pos
    | Neg;
  type orientation = (axis, direction);

  let turn_90 =
      (
        clockwise: bool,
        initial_orientation: orientation,
        (rx: float, ry: float),
      )
      : command => {
    let (dx, dy) =
      switch (initial_orientation) {
      | (X, Pos) => (rx, ry)
      | (X, Neg) => (Float.neg(rx), Float.neg(ry))
      | (Y, Pos) => (Float.neg(rx), ry)
      | (Y, Neg) => (rx, Float.neg(ry))
      };
    A_({
      rx,
      ry,
      x_axis_rotation: 0.0,
      large_arc_flag: false,
      sweep_flag: !clockwise,
      dx,
      dy,
    });
  };
  let turn_left = turn_90(true);
  let turn_right = turn_90(false);

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

  let view = (attrs: list(Vdom.Attr.t), path: t) =>
    Vdom.(
      Node.create_svg(
        "path",
        [
          Attr.create(
            "d",
            path |> List.map(string_of_command) |> StringUtil.sep,
          ),
          ...attrs,
        ],
        [],
      )
    );
};

/*
 module BoxPath = {
   type t = list(cmd)
   and cmd =
     | GoStraight(float)
     | Turn90(rotation)
   and rotation =
     | CW
     | CCW;

   let concat = List.concat;

   type axis =
     | X
     | Y;
   type direction =
     | Pos
     | Neg;
   type orientation = (axis, direction);

   let turn_180 =
     fun
     | (axis, Pos) => (axis, Neg)
     | (axis, Neg) => (axis, Pos);

   let go_straight = (orientation, d) =>
     switch (orientation) {
     | (X, direction) =>
       let dx =
         switch (direction) {
         | Pos => d
         | Neg => Float.neg(d)
         };
       Printf.sprintf("h %s ", FloatUtil.to_string_zero(dx));
     | (Y, direction) =>
       let dy =
         switch (direction) {
         | Pos => d
         | Neg => Float.neg(d)
         };
       Printf.sprintf("v %s ", FloatUtil.to_string_zero(dy));
     };

   let turn_90 = ((rx, ry), orientation, rotation) => {
     let (dx, dy) =
       switch (orientation, rotation) {
       | ((X, Pos), CW)
       | ((Y, Pos), CCW) => (Float.neg(rx), Float.neg(ry))
       | ((X, Pos), CCW)
       | ((Y, Neg), CW) => (Float.neg(rx), ry)
       | ((X, Neg), CW)
       | ((Y, Neg), CCW) => (rx, ry)
       | ((X, Neg), CCW)
       | ((Y, Pos), CW) => (rx, Float.neg(ry))
       };
     let sweep =
       switch (rotation) {
       | CW => "1"
       | CCW => "0"
       };
     Printf.sprintf(
       "a %s %s 0 0 %s %s %s ",
       FloatUtil.to_string_zero(rx),
       FloatUtil.to_string_zero(ry),
       sweep,
       FloatUtil.to_string_zero(dx),
       FloatUtil.to_string_zero(dy),
     );
   };

   let next_orientation = (orientation, rotation_90) =>
     switch (orientation, rotation) {
     | ((X, Pos), CCW)
     | ((X, Neg), CW) => (Y, Pos)
     | ((X, Pos), CW)
     | ((X, Neg), CCW) => (Y, Neg)
     | ((Y, Pos), CCW)
     | ((Y, Neg), CW) => (X, Neg)
     | ((Y, Pos), CW)
     | ((Y, Neg), CCW) => (X, Pos)
     };

   // TODO maybe revisit and compress turns too
   let rec compress =
     fun
     | [] => []
     | [GoStraight(d1), GoStraight(d2), ...path] =>
       compress([GoStraight(d1 +. d2), ...path])
     | [cmd, ...path] => [cmd, ...compress(path)];

   let view =
       (
         ~initial_position as (x, y): (float, float),
         ~initial_orientation: orientation,
         ~radii as (rx, ry): (float, float),
         path: t,
       )
       : Vdom.Node.t => {
     // TODO refine initial capacity estimate
     let buffer = Buffer.create(List.length(path) * 20);
     Buffer.add_string(
       buffer,
       Printf.sprintf(
         "M %s %s ",
         FloatUtil.to_string_zero(x),
         FloatUtil.to_string_zero(y),
       ),
     );

     // TODO simplify
     let rec going_straight = (orientation, gone_so_far: float, path: t): unit =>
       switch (path) {
       | [] => ()
       | [GoStraight(d), ...path] =>
         going_straight(orientation, gone_so_far +. d, path)
       | [Turn90(rotation), ...path] =>
         turning_corner((gone_so_far, orientation), rotation, path)
       }
     and turning_corner =
         ((entering_len, entering_orientation), rotation, path: t): unit =>
       switch (path) {
       | [] =>
         Buffer.add(
           buffer,
           go_straight(initial_orientation, entering_edge_len),
         )
       | [Turn90(rotation'), ...path] =>
         let new_orientation =
           switch (rotation, rotation') {
           | (CW, CCW)
           | (CCW, CW) => entering_orientation
           | (CW, CW)
           | (CCW, CCW) => turn_180(entering_orientation)
           };
         going_straight(new_orientation, 0.0, path);
       | [GoStraight(exiting_len), ...path] =>
         // Corner rounding cuts into the lengths of
         // the entering and exiting edges. Find the
         // maximum (proportionally scaled) radii possible
         // given lengths of entering and exiting edges.
         let (rx, ry) = {
           let (corner_x, corner_y) =
             switch (orientation) {
             | (X, _) => (entering_len, exiting_len *. 0.5)
             | (Y, _) => (exiting_len *. 0.5, entering_len)
             };
           let rx_min = min(rx, corner_x);
           let ry_min = min(ry, corner_y);
           ry_min *. rx >= rx_min *. ry
             ? (rx_min, rx_min *. ry /. rx) : (ry_min *. rx /. ry, ry_min);
         };
         let (r_entering, r_exiting) =
           switch (orientation) {
           | (X, _) => (rx, ry)
           | (Y, _) => (ry, rx)
           };
         Buffer.add(
           buffer,
           go_straight(entering_orientation, entering_len -. r_entering),
         );
         Buffer.add(
           buffer,
           turn_90((rx, ry), entering_orientation, rotation),
         );
         going_straight(
           next_orientation(entering_orientation, rotation),
           exiting_len -. r_exiting,
           path,
         );
       };

     going_straight(initial_orientation, 0.0, compress(path));
     Vdom.(
       Node.create_svg(
         "path",
         [Attr.create("d", Buffer.contents(buffer))],
         [],
       )
     );
   };
 };
 */
