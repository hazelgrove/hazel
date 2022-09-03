open Virtual_dom.Vdom;
open Haz3lcore;

let get_goal = (~font_metrics: FontMetrics.t, ~target_id, e) => {
  let rect = JSUtil.force_get_elem_by_id(target_id)##getBoundingClientRect;
  let goal_x = float_of_int(e##.clientX);
  let goal_y = float_of_int(e##.clientY);
  Measured.Point.{
    row: Float.to_int((goal_y -. rect##.top) /. font_metrics.row_height),
    col:
      Float.(
        to_int(round((goal_x -. rect##.left) /. font_metrics.col_width))
      ),
  };
};

let mousedown_overlay = (~inject, ~font_metrics, ~target_id) =>
  Node.div(
    ~attr=
      Attr.many(
        Attr.[
          id("mousedown-overlay"),
          on_mouseup(_ => inject(Update.Mouseup)),
          on_mousemove(e => {
            let goal = get_goal(~font_metrics, ~target_id, e);
            inject(Update.PerformAction(Select(Goal(goal))));
          }),
        ],
      ),
    [],
  );

let mousedown_handler =
    (~inject, ~font_metrics, ~target_id, ~additional_updates=[], e) => {
  let goal = get_goal(~font_metrics, ~target_id, e);
  Virtual_dom.Vdom.Effect.Many(
    List.map(
      inject,
      Update.[
        Mousedown,
        PerformAction(Move(Goal(goal))),
        ...additional_updates,
      ],
    ),
  );
};

let view =
    (
      ~inject,
      ~font_metrics,
      ~clss=[],
      ~selected: bool,
      ~mousedown: bool,
      ~mousedown_updates: list(Update.t)=[],
      ~show_code: bool,
      ~code_id: string,
      ~caption: option(Node.t)=?,
      code: Node.t,
    )
    : Node.t => {
  let mousedown_overlay =
    selected && mousedown
      ? [mousedown_overlay(~inject, ~font_metrics, ~target_id=code_id)] : [];
  Node.div(
    ~attr=
      Attr.many([
        Attr.classes(["cell", ...clss] @ (selected ? ["selected"] : [])),
        Attr.on_mousedown(
          mousedown_handler(
            ~inject,
            ~font_metrics,
            ~target_id=code_id,
            ~additional_updates=mousedown_updates,
          ),
        ),
      ]),
    Option.to_list(caption) @ (show_code ? mousedown_overlay @ [code] : []),
  );
};
