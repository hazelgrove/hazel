open Virtual_dom.Vdom;
open Node;
open Util;

/* Decorations are drawn in cell units relative to a cell of the code; this
   is the one place that turns them into pixels, through the editor's Grid. */

type fdims = {
  width: float,
  height: float,
  left: float,
  top: float,
};

/* A point in cells relative to `lo`, for a shape attached to the text from
   `lo` to `hi`. Within half a cell of `lo` it rides with the start of that
   text and within half a cell of `hi` with its end, so an overhang stays
   attached across a gadget on the boundary; gadgets strictly inside the
   span stretch the shape. */
let px_of_cell =
    (~grid: Grid.t, ~lo: Point.t, ~hi: Point.t, {x, y}: SvgUtil.Point.t)
    : SvgUtil.Point.t => {
  let {col_width, row_height}: FontMetrics.t = Grid.font_metrics(grid);
  let row_offset =
    min(max(0, int_of_float(Float.floor(y))), hi.row - lo.row);
  let row = lo.row + row_offset;
  let col = float_of_int(lo.col) +. x;
  let near = (c: int) => Float.abs(col -. float_of_int(c)) < 0.5;
  let from = (edge: float, c: int) =>
    edge +. (col -. float_of_int(c)) *. col_width;
  let px_x =
    if (row == lo.row && near(lo.col)) {
      from(Grid.x_start(grid, ~row, lo.col), lo.col);
    } else if (row == hi.row && near(hi.col)) {
      from(Grid.x_end(grid, ~row, hi.col), hi.col);
    } else {
      Grid.x(grid, ~row, col);
    };
  {
    x: px_x,
    y:
      Grid.y_top(grid, row) +. (y -. float_of_int(row_offset)) *. row_height,
  };
};

let empty_rect: SvgUtil.Rect.t = {
  min: {
    x: 0.,
    y: 0.,
  },
  width: 0.,
  height: 0.,
};

/* An SVG of a path already in pixels, positioned over the code. Its box is
   the path's extent, at least a pixel each way since a zero-sized viewBox
   draws nothing; overflow is visible (style.css), so an arc bulging past
   the box still draws. `offset` is subtracted from the box's position, for
   a box inside an element that itself sits at that pixel; `extra` adds
   children given the box. */
let svg_px =
    (
      ~base_cls=[],
      ~path_cls=[],
      ~attr=[],
      ~id="",
      ~offset: SvgUtil.Point.t={
                                 x: 0.,
                                 y: 0.,
                               },
      ~extra: SvgUtil.Rect.t => list(Node.t)=_ => [],
      path: SvgUtil.Path.t,
    )
    : Node.t => {
  let box = Option.value(SvgUtil.Path.bbox(path), ~default=empty_rect);
  let box = {
    ...box,
    width: max(box.width, 1.),
    height: max(box.height, 1.),
  };
  create_svg(
    "svg",
    ~attrs=
      (id == "" ? [] : [Attr.id(id)])
      @ [
        Attr.classes(base_cls),
        Attr.create(
          "style",
          Printf.sprintf(
            "position: absolute; left: %fpx; top: %fpx; width: %fpx; height: %fpx;",
            box.min.x -. offset.x,
            box.min.y -. offset.y,
            box.width,
            box.height,
          ),
        ),
        Attr.create(
          "viewBox",
          Printf.sprintf(
            "%f %f %f %f",
            box.min.x,
            box.min.y,
            box.width,
            box.height,
          ),
        ),
      ]
      @ attr,
    [
      SvgUtil.Path.view(~attrs=[Attr.classes(path_cls)], path),
      ...extra(box),
    ],
  );
};

/* An SVG of `paths`, drawn in cells relative to `lo` (see `px_of_cell`).
   `relative_to` positions it inside an element that sits at that cell. */
let svg =
    (
      ~grid: Grid.t,
      ~lo: Point.t,
      ~hi: Point.t,
      ~base_cls=[],
      ~path_cls=[],
      ~attr=[],
      ~id="",
      ~relative_to: option(Point.t)=None,
      paths: SvgUtil.Path.t,
    )
    : Node.t => {
  let {col_width, row_height}: FontMetrics.t = Grid.font_metrics(grid);
  let offset: SvgUtil.Point.t =
    switch (relative_to) {
    | None => {
        x: 0.,
        y: 0.,
      }
    | Some(p) => {
        x: Grid.x_start(grid, ~row=p.row, p.col),
        y: Grid.y_top(grid, p.row),
      }
    };
  svg_px(
    ~base_cls,
    ~path_cls,
    ~attr,
    ~id,
    ~offset,
    SvgUtil.Path.transform(
      ~pt=px_of_cell(~grid, ~lo, ~hi),
      ~scale={
        dx: col_width,
        dy: row_height,
      },
      paths,
    ),
  );
};

/* The pixel box of `width` by `height` cells from `origin`. */
let box = (~grid: Grid.t, ~width: int, ~height: int, origin: Point.t): fdims => {
  let left = Grid.x_start(grid, ~row=origin.row, origin.col);
  let top = Grid.y_top(grid, origin.row);
  {
    left,
    top,
    width: Grid.x_end(grid, ~row=origin.row, origin.col + width) -. left,
    height: Grid.y_bottom(grid, origin.row + height - 1) -. top,
  };
};

let style_of_box = ({left, top, width, height}: fdims): string =>
  Printf.sprintf(
    "left: %fpx; top: %fpx; width: %fpx; height: %fpx;",
    left,
    top,
    width,
    height,
  );

let position = (~grid: Grid.t, ~width=1, ~height=1, origin: Point.t): Attr.t =>
  Attr.create("style", style_of_box(box(~grid, ~width, ~height, origin)));

let abs_position =
    (~grid: Grid.t, ~width=1, ~height=1, origin: Point.t): Attr.t =>
  Attr.create(
    "style",
    "position: absolute; "
    ++ style_of_box(box(~grid, ~width, ~height, origin)),
  );

let abs_style =
    (~grid: Grid.t, {origin, last}: Haz3lcore.Measured.measurement): Attr.t =>
  abs_position(
    ~grid,
    ~width=abs(last.col - origin.col),
    ~height=abs(last.row - origin.row + 1),
    origin,
  );
