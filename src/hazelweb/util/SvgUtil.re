open Sexplib.Std;

module Vdom = Virtual_dom.Vdom;

[@deriving sexp]
type point = {
  x: float,
  y: float,
};

module Rect = {
  [@deriving sexp]
  type t = {
    min: point,
    width: float,
    height: float,
  };
};

module Path = {
  type t = list(cmd)
  and cmd =
    | M(point)
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

  let string_of_flag =
    fun
    | false => "0"
    | true => "1";

  let string_of_command =
    fun
    | M({x, y}) => Printf.sprintf("M %f %f", x, y)
    | M_({dx, dy}) => Printf.sprintf("m %f %f", dx, dy)
    | H_({dx}) => Printf.sprintf("h %f", dx)
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

  let view = (~attrs: list(Vdom.Attr.t), path: t): Vdom.Node.t => {
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

module OrthogonalPolygon = {
  type t = Path.t;

  [@deriving sexp]
  type linked_edge = {
    src: point,
    dst: point,
    mutable next: [@sexp.opaque] option(linked_edge),
  };

  let linked_edge_eq = (e1, e2) => e1.src == e2.src && e1.dst == e2.dst;

  let cmd_of_linked_edge = (edge): Path.cmd => {
    let src = edge.src;
    let dst = edge.dst;
    src.x == dst.x ? V_({dy: dst.y -. src.y}) : H_({dx: dst.x -. src.x});
  };

  /**
   * Corner rounding cuts into the lengths of the entering and
   * exiting edges. Find the maximum (proportionally scaled)
   * radii possible given lengths of entering and exiting edges.
   */
  let max_radii =
      ((rx: float, ry: float), (dx: float, dy: float)): (float, float) => {
    let rx_min = min(rx, Float.abs(dx));
    let ry_min = min(ry, Float.abs(dy));
    ry_min *. rx >= rx_min *. ry
      ? (rx_min, rx_min *. ry /. rx) : (ry_min *. rx /. ry, ry_min);
  };

  let round_corners = ((rx: float, ry: float), path: Path.t): Path.t => {
    path
    |> List.map(
         fun
         | Path.H_({dx}) => Path.[H_({dx: dx *. 0.5}), H_({dx: dx *. 0.5})]
         | V_({dy}) => [V_({dy: dy *. 0.5}), V_({dy: dy *. 0.5})]
         | cmd => [cmd],
       )
    |> List.flatten
    |> ListUtil.rotate
    |> ListUtil.disjoint_pairs
    |> List.map(((cmd1: Path.cmd, cmd2: Path.cmd)) => {
         switch (cmd1, cmd2) {
         | (H_({dx}), V_({dy})) =>
           let (rx, ry) = max_radii((rx, ry), (dx, dy));
           let clockwise = Float.sign_bit(dx) == Float.sign_bit(dy);
           Path.[
             H_({dx: Float.copy_sign(Float.abs(dx) -. rx, dx)}),
             A_({
               rx,
               ry,
               x_axis_rotation: 0.,
               large_arc_flag: false,
               sweep_flag: clockwise,
               dx: Float.copy_sign(rx, dx),
               dy: Float.copy_sign(ry, dy),
             }),
             V_({dy: Float.copy_sign(Float.abs(dy) -. ry, dy)}),
           ];
         | (V_({dy}), H_({dx})) =>
           let (rx, ry) = max_radii((rx, ry), (dx, dy));
           let clockwise = Float.sign_bit(dy) != Float.sign_bit(dx);
           [
             V_({dy: Float.copy_sign(Float.abs(dy) -. ry, dy)}),
             A_({
               rx,
               ry,
               x_axis_rotation: 0.,
               large_arc_flag: false,
               sweep_flag: clockwise,
               dx: Float.copy_sign(rx, dx),
               dy: Float.copy_sign(ry, dy),
             }),
             H_({dx: Float.copy_sign(Float.abs(dx) -. rx, dx)}),
           ];
         | _ => [cmd1, cmd2]
         }
       })
    |> List.flatten;
  };

  /**
   * Implements algorithm described in Section 8.5 of
   * Computational Geometry: An Introduction by Preparata
   * & Shamos. If you need to understand the algorithm in
   * detail, you should first read Sections 1.2.3.1 + 8.3.
   */
  let mk = (~corner_radii: (float, float), rects: list(Rect.t)): t => {
    assert(rects != []);

    let is_left_side = (edge: linked_edge): bool => {
      edge.src.y > edge.dst.y;
    };

    let sorted_vertical_sides: list(linked_edge) =
      rects
      |> List.map((Rect.{min, width, height}) => {
           let max_x = min.x +. width;
           let max_y = min.y +. height;
           [
             // left sides point in negative direction
             {
               src: {
                 x: min.x,
                 y: max_y,
               },
               dst: {
                 x: min.x,
                 y: min.y,
               },
               next: None,
             },
             // right sides point in positive direction
             {
               src: {
                 x: max_x,
                 y: min.y,
               },
               dst: {
                 x: max_x,
                 y: max_y,
               },
               next: None,
             },
           ];
         })
      |> List.flatten
      |> List.sort((v1, v2) =>
           if (v1.src.x < v2.src.x) {
             (-1);
           } else if (v1.src.x > v2.src.x) {
             1;
           } else {
             // for vertical sides of equal abscissa,
             // need to sort left sides before right sides
             let is_left1 = is_left_side(v1);
             let is_left2 = is_left_side(v2);
             if (is_left1 && !is_left2) {
               (-1);
             } else if (!is_left1 && is_left2) {
               1;
             } else {
               0;
             };
           }
         );

    let ys =
      rects
      |> List.map((Rect.{min, height, _}) => [min.y, min.y +. height])
      |> List.flatten;

    let vertical_contour_edges =
      sorted_vertical_sides
      |> ListUtil.map_with_accumulator(
           (tree, v) => {
             let x = v.src.x;
             let ys = (v.src.y, v.dst.y);
             let mk_contour_edge = ((y_src, y_dst)) => {
               {
                 src: {
                   x,
                   y: y_src,
                 },
                 dst: {
                   x,
                   y: y_dst,
                 },
                 next: None,
               };
             };
             if (is_left_side(v)) {
               let new_contour_edges =
                 SegmentTree.complement_intersection(ys, tree)
                 |> List.map(mk_contour_edge);
               let updated_tree = SegmentTree.insert(ys, tree);
               (updated_tree, new_contour_edges);
             } else {
               let updated_tree = SegmentTree.delete(ys, tree);
               let new_contour_edges =
                 SegmentTree.complement_intersection(ys, updated_tree)
                 |> List.map(mk_contour_edge);
               (updated_tree, new_contour_edges);
             };
           },
           SegmentTree.mk(ys),
         )
      |> snd
      |> List.flatten;

    // TODO figure out if there's a cleaner + more efficient method for this pass
    let merged_vertical_contour_edges = {
      let (left_edges, right_edges) =
        List.partition(
          edge => edge.src.y > edge.dst.y,
          vertical_contour_edges,
        );
      let merged_left_edges =
        left_edges
        |> List.sort((v1, v2) =>
             if (v1.src.x < v2.src.x) {
               (-1);
             } else if (v1.src.x > v2.src.x) {
               1;
             } else {
               (-1) * Float.compare(v1.src.y, v2.src.y);
             }
           )
        |> List.fold_left(
             (stack, v) =>
               switch (stack) {
               | [] => [v]
               | [hd, ...tl] as stack =>
                 if (v.src.x == hd.dst.x && v.src.y >= hd.dst.y) {
                   [{...hd, dst: v.dst}, ...tl];
                 } else {
                   [v, ...stack];
                 }
               },
             [],
           );
      let merged_right_edges =
        right_edges
        |> List.sort((v1, v2) =>
             if (v1.src.x < v2.src.x) {
               (-1);
             } else if (v1.src.x > v2.src.x) {
               1;
             } else {
               Float.compare(v1.src.y, v2.src.y);
             }
           )
        |> List.fold_left(
             (stack, v) =>
               switch (stack) {
               | [] => [v]
               | [hd, ...tl] as stack =>
                 if (v.src.x == hd.dst.x && v.src.y <= hd.dst.y) {
                   [{...hd, dst: v.dst}, ...tl];
                 } else {
                   [v, ...stack];
                 }
               },
             [],
           );
      merged_left_edges @ merged_right_edges;
    };

    merged_vertical_contour_edges
    |> List.map(v => [(false, v), (true, v)])
    |> List.flatten
    |> List.sort(((is_src1, v1), (is_src2, v2)) => {
         let pt1 = is_src1 ? v1.src : v1.dst;
         let pt2 = is_src2 ? v2.src : v2.dst;
         if (pt1.y < pt2.y) {
           (-1);
         } else if (pt1.y > pt2.y) {
           1;
         } else {
           Float.compare(pt1.x, pt2.x);
         };
       })
    |> ListUtil.disjoint_pairs
    |> List.iter((((is_src1, v1), (is_src2, v2))) => {
         let pt1 = is_src1 ? v1.src : v1.dst;
         let pt2 = is_src2 ? v2.src : v2.dst;
         assert(pt1.y == pt2.y);
         let y = pt1.y;

         let (x_src, x_dst, prev, next) =
           is_src1 ? (pt2.x, pt1.x, v2, v1) : (pt1.x, pt2.x, v1, v2);

         let h = {
           src: {
             x: x_src,
             y,
           },
           dst: {
             x: x_dst,
             y,
           },
           next: Some(next),
         };
         prev.next = Some(h);
       });

    let start = List.hd(merged_vertical_contour_edges);
    let rec build_path = (edge: linked_edge): Path.t => {
      switch (edge.next) {
      | None => failwith("expected single cycle")
      | Some(next) =>
        linked_edge_eq(next, start)
          ? [] : [cmd_of_linked_edge(next), ...build_path(next)]
      };
    };
    let path = [cmd_of_linked_edge(start), ...build_path(start)];

    path
    |> round_corners(corner_radii)
    |> List.cons(
         Path.M({
           x: (start.src.x +. start.dst.x) *. 0.5,
           y: (start.src.y +. start.dst.y) *. 0.5,
         }),
       );
  };

  let view = Path.view;
};
