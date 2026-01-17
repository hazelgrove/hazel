open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

/* A rotary knob projector for Float values.
 * Circular dial with modular synth aesthetic.
 * Drag up/down to change value. */

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = {
    min_val: float,
    max_val: float,
  };
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

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

  let update = (model, _, _) => model;

  /* Convert value (0-1) to rotation angle (-135 to 135 degrees) */
  let value_to_angle = (v: float, min_val: float, max_val: float): float => {
    let normalized = (v -. min_val) /. (max_val -. min_val);
    let clamped = max(0.0, min(1.0, normalized));
    (-135.0) +. clamped *. 270.0;
  };

  let view = ({model, info, parent, _}: View.args(model, action)) => {
    let {min_val, max_val} = model;
    let value = get(info);
    let angle = value_to_angle(value, min_val, max_val);

    /* Calculate indicator position on the knob edge */
    let angle_rad = angle *. Float.pi /. 180.0;
    let indicator_x = 50.0 +. 30.0 *. sin(angle_rad);
    let indicator_y = 50.0 -. 30.0 *. cos(angle_rad);

    View.mk(
      Node.div(
        ~attrs=[
          Attr.classes(["knob-projector"]),
          /* Click to set value based on Y position within element */
          Attr.on_mousedown(evt => {
            /* Use offsetY for position relative to element */
            let offset_y = Float.of_int(evt##.offsetY);
            /* Estimate element height (placeholder is 3 rows * ~15px) */
            let height = 45.0;
            let rel_y = offset_y /. height;
            /* Invert: top = max, bottom = min */
            let new_value = min_val +. (1.0 -. rel_y) *. (max_val -. min_val);
            let clamped = max(min_val, min(max_val, new_value));
            parent(SetSyntax(put(info, clamped)));
          }),
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
