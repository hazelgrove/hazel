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
          inject(action),
        ])
      ),
      clss(["named-menu-item"]),
    ],
    [text(name)],
  );

let manual_probe =
    (
      ~inject: Action.t => Ui_effect.t(unit),
      probe_status: Refractors.probe_status,
      ci: option(Language.Info.t),
    ) =>
  switch (ci) {
  /* These can be applied to expressions and patterns */
  | Some(InfoExp(_) | InfoPat(_)) => [
      menu_item(
        switch (probe_status) {
        | Refractors.Manual(_) => "Remove probe"
        | Refractors.REPL => "Switch to manual"
        | Refractors.Non => "Add probe"
        },
        inject,
        Refractor(ToggleProbeManual),
      ),
    ]
  | _ => []
  };

let auto_probe =
    (
      ~inject: Action.t => Ui_effect.t(unit),
      probe_status: Refractors.probe_status,
      ci: option(Language.Info.t),
    ) =>
  switch (ci) {
  /* Not much reason to put these on patterns... */
  | Some(InfoExp(_)) => [
      menu_item(
        switch (probe_status) {
        | Refractors.Manual(_) => "Switch to auto"
        | Refractors.REPL => "Remove auto probe"
        | Refractors.Non => "Add auto probe"
        },
        inject,
        Refractor(ToggleProbeREPL),
      ),
    ]
  | _ => []
  };

let step_into =
    (
      ~inject: Action.t => Ui_effect.t(unit),
      info_map: Language.Statics.Map.t,
      ci: option(Language.Info.t),
      z: Zipper.t,
    ) =>
  switch (ci) {
  | Some(InfoExp(_)) =>
    switch (Refractors.is_jump_target(info_map, z)) {
    | Some(_) => [menu_item("Step into", inject, Refractor(ProbeJump))]
    | None => []
    }
  | _ => []
  };

let probes_actions =
    (
      ~inject: Action.t => Ui_effect.t(unit),
      info_map: Language.Statics.Map.t,
      z: Zipper.t,
    ) => {
  let id = Indicated.index(z) |> Option.value(~default=Id.invalid);
  let ci = Indicated.ci_of(z, info_map);
  let probe_status = Refractors.probe_status(id, info_map, z.refractors);

  manual_probe(~inject, probe_status, ci)
  @ auto_probe(~inject, probe_status, ci)
  @ step_into(~inject, info_map, ci, z);
};

let probes_menu = probes_items =>
  NutMenu.submenu(
    ~tooltip="",
    ~icon=div([]),
    [
      div_c(
        "group",
        [
          // div_c("name", [text("Probes")]),
          div_c("contents", probes_items),
        ],
      ),
    ],
  );

let view =
    (
      ~inject: Action.t => Ui_effect.t(unit),
      ~syntax: Haz3lcore.CachedSyntax.t,
      ~info_map: Language.Statics.Map.t,
      ~font_metrics: FontMetrics.t,
      z: Haz3lcore.Zipper.t,
    )
    : Node.t => {
  let caret_point = Zipper.Caret.point(syntax.measured, z);
  let probes_items = probes_actions(~inject, info_map, z);
  probes_items == []
    ? div([])
    : div(
        ~attrs=[
          Attr.classes(["context-menu", "nut-menu"]),
          pos_attr(caret_point, font_metrics),
        ],
        [probes_menu(probes_items)],
      );
};
