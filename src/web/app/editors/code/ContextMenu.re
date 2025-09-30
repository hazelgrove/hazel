open Haz3lcore;
open Virtual_dom.Vdom;
open Util;
open WebUtil;
open Node;

let pos_str = (~left, ~top, font_metrics: FontMetrics.t) =>
  Printf.sprintf(
    "position: absolute; left: %fpx; top: %fpx;",
    Float.of_int(left) *. font_metrics.col_width,
    Float.of_int(top) *. font_metrics.row_height,
  );

let pos_attr = (point: Point.t, font_metrics: FontMetrics.t) =>
  Attr.create(
    "style",
    pos_str(~left=point.col, ~top=point.row + 1, font_metrics),
  );

let menu_item =
    (name: string, inject: Action.t => Ui_effect.t(unit), action: Action.t) =>
  div(
    ~attrs=[
      Attr.on_pointerdown(_ =>
        Effect.Many([
          Effect.Stop_propagation,
          Effect.Prevent_default,
          //inject(Project(FocusIndicated)),
          inject(action),
        ])
      ),
      clss(["named-menu-item"]),
    ],
    [text(name)],
  );

let probes_menu = (~inject: Action.t => Ui_effect.t(unit)) =>
  div_c(
    "group",
    [
      // div_c("name", [text("Probes")]),
      div_c(
        "contents",
        [
          menu_item("Toggle probe", inject, Refractor(ToggleProbeManual)),
          menu_item("Toggle auto", inject, Refractor(ToggleProbeREPL)),
          menu_item("Step into", inject, Refractor(ProbeJump)),
        ],
      ),
    ],
  );

let view =
    (
      ~inject: Action.t => Ui_effect.t(unit),
      ~measured: Haz3lcore.Measured.t,
      ~font_metrics: FontMetrics.t,
      z: Haz3lcore.Zipper.t,
    )
    : Node.t => {
  let caret_point = Zipper.Caret.point(measured, z);
  div(
    ~attrs=[
      Attr.classes(["context-menu", "nut-menu"]),
      pos_attr(caret_point, font_metrics),
    ],
    [NutMenu.submenu(~tooltip="", ~icon=div([]), [probes_menu(~inject)])],
  );
};
