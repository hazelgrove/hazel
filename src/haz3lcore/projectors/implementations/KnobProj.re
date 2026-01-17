open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Js_of_ocaml;

/* A rotary knob projector for Float values.
 * Circular dial with modular synth aesthetic.
 * Point mouse toward center to set angle. */

/* Module-level state for pointer capture during drag */
let capture_target: ref(option(Js.t(Dom_html.element))) = ref(None);

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = {
    min_val: float,
    max_val: float,
    dragging: bool,
  };
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | StartDrag
    | StopDrag;

  let default_min = 0.0;
  let default_max = 1.0;

  let float_of = (any: Language.Any.t): option(float) =>
    switch (any) {
    | Exp({term: Atom(Float(f)), _}) => Some(f)
    | _ => None
    };

  let init = (any: Language.Any.t) =>
    switch (float_of(any)) {
    | Some(_) =>
      Some({
        min_val: default_min,
        max_val: default_max,
        dragging: false,
      })
    | None => None
    };

  let get = (info: info): float =>
    switch (
      info.syntax |> info.utility.seg_to_term |> OptUtil.and_then(float_of)
    ) {
    | Some(f) => f
    | None => 0.0
    };

  let put = (info: info, v: float): Base.segment =>
    switch (
      info.utility.lift_syntax(
        fun
        | Exp(t) =>
          Exp({
            ...t,
            term: Atom(Float(v)),
          })
        | _ => failwith("Knob: Put: not float literal"),
        info.syntax,
      )
    ) {
    | Some(s) => s
    | None => failwith("Knob: Put: lift failed")
    };

  let focusable = Focusable.non;
  let dynamics = false;
  /* Knob needs square space - 3 rows height for the dial */
  let placeholder = (_, _) => {
    ProjectorShape.horizontal: 6,
    vertical: Tab(3),
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

  /* Convert value (0-1) to rotation angle (-135 to 135 degrees) */
  let value_to_angle = (v: float, min_val: float, max_val: float): float => {
    let normalized = (v -. min_val) /. (max_val -. min_val);
    let clamped = max(0.0, min(1.0, normalized));
    (-135.0) +. clamped *. 270.0;
  };

  /* Convert mouse angle (from atan2) to value */
  let angle_to_value =
      (angle_deg: float, min_val: float, max_val: float): float => {
    /* Clamp angle to valid knob range (-135 to 135 degrees) */
    let clamped_angle = max(-135.0, min(135.0, angle_deg));
    /* Map -135..135 to 0..1 */
    let normalized = (clamped_angle +. 135.0) /. 270.0;
    min_val +. normalized *. (max_val -. min_val);
  };

  let view = ({model, info, parent, local, _}: View.args(model, action)) => {
    let {min_val, max_val, dragging} = model;
    let value = get(info);
    let angle = value_to_angle(value, min_val, max_val);

    /* Calculate indicator position on the knob edge */
    let angle_rad = angle *. Float.pi /. 180.0;
    let indicator_x = 50.0 +. 30.0 *. sin(angle_rad);
    let indicator_y = 50.0 -. 30.0 *. cos(angle_rad);

    /* Check if at min/max for tick highlighting */
    let at_min = value <= min_val +. 0.001;
    let at_max = value >= max_val -. 0.001;

    /* Handler for updating value based on mouse position */
    let handle_mouse = evt => {
      let target =
        evt##.currentTarget |> Js.Opt.get(_, _ => failwith("no target"));
      let rect = target##getBoundingClientRect;
      let width = rect##.right -. rect##.left;
      let height = rect##.bottom -. rect##.top;
      /* Map to SVG coordinates (0-100) */
      let svg_x =
        (Float.of_int(evt##.clientX) -. rect##.left) /. width *. 100.0;
      let svg_y =
        (Float.of_int(evt##.clientY) -. rect##.top) /. height *. 100.0;
      /* Center is at (50, 50) */
      let dx = svg_x -. 50.0;
      let dy = svg_y -. 50.0;
      /* atan2(dx, -dy) gives angle from 12 o'clock position */
      let angle_rad = atan2(dx, -. dy);
      let angle_deg = angle_rad *. 180.0 /. Float.pi;
      let new_value = angle_to_value(angle_deg, min_val, max_val);
      parent(SetSyntax(put(info, new_value)));
    };

    View.mk(
      Node.div(
        ~attrs=[
          Attr.classes(["knob-projector"] @ (dragging ? ["dragging"] : [])),
          Attr.on_pointerdown(evt => {
            let target =
              evt##.currentTarget |> Js.Opt.get(_, _ => failwith("no target"));
            JsUtil.setPointerCapture(target, evt##.pointerId);
            capture_target := Some(target);
            Effect.Many([handle_mouse(evt), local(StartDrag)]);
          }),
          Attr.on_pointerup(evt => {
            switch (capture_target^) {
            | Some(target) =>
              if (JsUtil.hasPointerCapture(target, evt##.pointerId)) {
                JsUtil.releasePointerCapture(target, evt##.pointerId);
              };
              capture_target := None;
            | None => ()
            };
            local(StopDrag);
          }),
          Attr.on_mousemove(evt =>
            if (dragging) {
              handle_mouse(evt);
            } else {
              Virtual_dom.Vdom.Effect.Ignore;
            }
          ),
        ],
        [
          /* SVG knob dial */
          Node.create_svg(
            "svg",
            ~attrs=[
              Attr.create("viewBox", "0 0 100 100"),
              Attr.classes(["knob-svg"]),
            ],
            [
              /* Outer ring */
              Node.create_svg(
                "circle",
                ~attrs=[
                  Attr.create("cx", "50"),
                  Attr.create("cy", "50"),
                  Attr.create("r", "40"),
                  Attr.classes(["knob-ring"]),
                ],
                [],
              ),
              /* Knob body */
              Node.create_svg(
                "circle",
                ~attrs=[
                  Attr.create("cx", "50"),
                  Attr.create("cy", "50"),
                  Attr.create("r", "35"),
                  Attr.classes(["knob-body"]),
                ],
                [],
              ),
              /* Min tick mark at -135° (7 o'clock) */
              Node.create_svg(
                "line",
                ~attrs=[
                  Attr.create("x1", "24.5"),
                  Attr.create("y1", "75.5"),
                  Attr.create("x2", "18.9"),
                  Attr.create("y2", "81.1"),
                  Attr.classes(["knob-tick"] @ (at_min ? ["active"] : [])),
                ],
                [],
              ),
              /* Max tick mark at +135° (5 o'clock) */
              Node.create_svg(
                "line",
                ~attrs=[
                  Attr.create("x1", "75.5"),
                  Attr.create("y1", "75.5"),
                  Attr.create("x2", "81.1"),
                  Attr.create("y2", "81.1"),
                  Attr.classes(["knob-tick"] @ (at_max ? ["active"] : [])),
                ],
                [],
              ),
            ]
            @ (
              /* Debug line from center to indicator (shown when dragging) */
              dragging
                ? [
                  Node.create_svg(
                    "line",
                    ~attrs=[
                      Attr.create("x1", "50"),
                      Attr.create("y1", "50"),
                      Attr.create("x2", Printf.sprintf("%.1f", indicator_x)),
                      Attr.create("y2", Printf.sprintf("%.1f", indicator_y)),
                      Attr.classes(["knob-debug-line"]),
                    ],
                    [],
                  ),
                ]
                : []
            )
            @ [
              /* Position indicator */
              Node.create_svg(
                "circle",
                ~attrs=[
                  Attr.create("cx", Printf.sprintf("%.1f", indicator_x)),
                  Attr.create("cy", Printf.sprintf("%.1f", indicator_y)),
                  Attr.create("r", "5"),
                  Attr.classes(["knob-indicator"]),
                ],
                [],
              ),
              /* Center dot */
              Node.create_svg(
                "circle",
                ~attrs=[
                  Attr.create("cx", "50"),
                  Attr.create("cy", "50"),
                  Attr.create("r", "3"),
                  Attr.classes(["knob-center"]),
                ],
                [],
              ),
            ],
          ),
          /* Value display */
          Node.span(
            ~attrs=[Attr.classes(["knob-value"])],
            [Node.text(Printf.sprintf("%.2f", value))],
          ),
        ],
      ),
    );
  };
};
