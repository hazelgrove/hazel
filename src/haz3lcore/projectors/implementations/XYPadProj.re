open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

/* An XY Pad projector for controlling two Float values simultaneously.
 * 2D touch/drag surface with modular synth aesthetic. */

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = {
    dragging: bool,
    x_min: float,
    x_max: float,
    y_min: float,
    y_max: float,
  };
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | StartDrag
    | StopDrag;

  let default_min = 0.0;
  let default_max = 1.0;

  /* Extract tuple of two floats - handles both bare Tuple and Parens(Tuple) */
  let tuple_of = (any: Language.Any.t): option((float, float)) =>
    switch (any) {
    | Exp({
        term:
          Tuple([{term: Atom(Float(x)), _}, {term: Atom(Float(y)), _}]),
        _,
      }) =>
      Some((x, y))
    | Exp({
        term:
          Parens({
            term:
              Tuple([
                {term: Atom(Float(x)), _},
                {term: Atom(Float(y)), _},
              ]),
            _,
          }),
        _,
      }) =>
      Some((x, y))
    | _ => None
    };

  let init = (any: Language.Any.t) =>
    switch (tuple_of(any)) {
    | Some(_) =>
      Some({
        dragging: false,
        x_min: default_min,
        x_max: default_max,
        y_min: default_min,
        y_max: default_max,
      })
    | None => None
    };

  let get = (info: info): (float, float) =>
    switch (
      info.syntax |> info.utility.seg_to_term |> OptUtil.and_then(tuple_of)
    ) {
    | Some(xy) => xy
    | None => (0.5, 0.5)
    };

  let put = (info: info, x: float, y: float): Base.segment =>
    switch (
      info.utility.lift_syntax(
        fun
        | Exp({term: Tuple([tx, ty]), _} as t) =>
          Exp({
            ...t,
            term:
              Tuple([
                {
                  ...tx,
                  term: Atom(Float(x)),
                },
                {
                  ...ty,
                  term: Atom(Float(y)),
                },
              ]),
          })
        | Exp({term: Parens({term: Tuple([tx, ty]), _} as inner), _} as t) =>
          Exp({
            ...t,
            term:
              Parens({
                ...inner,
                term:
                  Tuple([
                    {
                      ...tx,
                      term: Atom(Float(x)),
                    },
                    {
                      ...ty,
                      term: Atom(Float(y)),
                    },
                  ]),
              }),
          })
        | _ => failwith("XYPad: Put: not tuple"),
        info.syntax,
      )
    ) {
    | Some(s) => s
    | None => failwith("XYPad: Put: lift failed")
    };

  let focusable = Focusable.non;
  let dynamics = false;
  /* XY Pad needs square space - 5 rows for a good touch target */
  let placeholder = (_, _) => {
    ProjectorShape.horizontal: 10,
    vertical: Block(5),
  };

  let update = (model, _, action) =>
    switch (action) {
    | StartDrag => {
        ...model,
        dragging: true,
      }
    | StopDrag => {
        ...model,
        dragging: false,
      }
    };

  /* Convert normalized (0-1) position to value */
  let pos_to_value = (pos: float, min_val: float, max_val: float): float => {
    let clamped = max(0.0, min(1.0, pos));
    min_val +. clamped *. (max_val -. min_val);
  };

  /* Convert value to normalized (0-1) position */
  let value_to_pos = (v: float, min_val: float, max_val: float): float => {
    let normalized = (v -. min_val) /. (max_val -. min_val);
    max(0.0, min(1.0, normalized));
  };

  let view = ({model, info, parent, local, _}: View.args(model, action)) => {
    let {x_min, x_max, y_min, y_max, _} = model;
    let (x_val, y_val) = get(info);
    let x_pos = value_to_pos(x_val, x_min, x_max) *. 100.0;
    let y_pos = (1.0 -. value_to_pos(y_val, y_min, y_max)) *. 100.0;

    View.mk(
      Node.div(
        ~attrs=[
          Attr.classes(["xypad-projector", model.dragging ? "dragging" : ""]),
          Attr.on_mousedown(_ => local(StartDrag)),
          Attr.on_mouseup(_ => local(StopDrag)),
          Attr.on_mouseleave(_ => local(StopDrag)),
          Attr.on_mousemove(evt =>
            if (model.dragging) {
              /* Use offsetX/offsetY for position relative to element */
              let offset_x = Float.of_int(evt##.offsetX);
              let offset_y = Float.of_int(evt##.offsetY);
              /* Estimate element size (placeholder is 10 cols x 5 rows * ~15px) */
              let width = 100.0;
              let height = 75.0;
              let rel_x = offset_x /. width;
              let rel_y = offset_y /. height;
              let new_x = pos_to_value(rel_x, x_min, x_max);
              let new_y = pos_to_value(1.0 -. rel_y, y_min, y_max);
              parent(SetSyntax(put(info, new_x, new_y)));
            } else {
              Virtual_dom.Vdom.Effect.Ignore;
            }
          ),
        ],
        [
          /* SVG pad */
          Node.create_svg(
            "svg",
            ~attrs=[
              Attr.create("viewBox", "0 0 100 100"),
              Attr.classes(["xypad-svg"]),
            ],
            [
              /* Background grid lines */
              Node.create_svg(
                "line",
                ~attrs=[
                  Attr.create("x1", "50"),
                  Attr.create("y1", "0"),
                  Attr.create("x2", "50"),
                  Attr.create("y2", "100"),
                  Attr.classes(["xypad-grid"]),
                ],
                [],
              ),
              Node.create_svg(
                "line",
                ~attrs=[
                  Attr.create("x1", "0"),
                  Attr.create("y1", "50"),
                  Attr.create("x2", "100"),
                  Attr.create("y2", "50"),
                  Attr.classes(["xypad-grid"]),
                ],
                [],
              ),
              /* Crosshair lines to cursor */
              Node.create_svg(
                "line",
                ~attrs=[
                  Attr.create("x1", Printf.sprintf("%.1f", x_pos)),
                  Attr.create("y1", "0"),
                  Attr.create("x2", Printf.sprintf("%.1f", x_pos)),
                  Attr.create("y2", "100"),
                  Attr.classes(["xypad-crosshair"]),
                ],
                [],
              ),
              Node.create_svg(
                "line",
                ~attrs=[
                  Attr.create("x1", "0"),
                  Attr.create("y1", Printf.sprintf("%.1f", y_pos)),
                  Attr.create("x2", "100"),
                  Attr.create("y2", Printf.sprintf("%.1f", y_pos)),
                  Attr.classes(["xypad-crosshair"]),
                ],
                [],
              ),
              /* Cursor dot */
              Node.create_svg(
                "circle",
                ~attrs=[
                  Attr.create("cx", Printf.sprintf("%.1f", x_pos)),
                  Attr.create("cy", Printf.sprintf("%.1f", y_pos)),
                  Attr.create("r", "6"),
                  Attr.classes(["xypad-cursor"]),
                ],
                [],
              ),
              /* Glow effect */
              Node.create_svg(
                "circle",
                ~attrs=[
                  Attr.create("cx", Printf.sprintf("%.1f", x_pos)),
                  Attr.create("cy", Printf.sprintf("%.1f", y_pos)),
                  Attr.create("r", "10"),
                  Attr.classes(["xypad-glow"]),
                ],
                [],
              ),
            ],
          ),
          /* Value display */
          Node.span(
            ~attrs=[Attr.classes(["xypad-value"])],
            [Node.text(Printf.sprintf("%.2f, %.2f", x_val, y_val))],
          ),
        ],
      ),
    );
  };
};
