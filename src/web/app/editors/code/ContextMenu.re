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

/* Keyboard shortcut display - abstracts the format for easy updates */
let shortcut_view = (shortcut: string) =>
  span(~attrs=[clss(["menu-shortcut"])], [text(shortcut)]);

let menu_item =
    (
      ~shortcut: option(string)=?,
      name: string,
      inject: Action.t => Ui_effect.t(unit),
      action: Action.t,
    ) =>
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
    [text(name)]
    @ (
      switch (shortcut) {
      | Some(s) => [shortcut_view(s)]
      | None => []
      }
    ),
  );

/* Keyboard shortcuts for probe actions - platform-dependent */
module Shortcuts = {
  let manual_probe = () => Os.is_mac^ ? "⌘E" : "Ctrl+E";
  let auto_probe = () => Os.is_mac^ ? "⇧⌘E" : "Ctrl+Shift+E";
  let goto_definition = "F12";
};

let manual_probe =
    (
      ~inject: Action.t => Ui_effect.t(unit),
      ~can_probe: bool,
      probe_status: ProbePerform.probe_status,
      ci: option(Language.Info.t),
    ) =>
  switch (ci) {
  /* These can be applied to expressions and patterns, but only if
     the term is actually probeable (not types, labels, etc.) */
  | Some(InfoExp(_) | InfoPat(_)) when can_probe => [
      menu_item(
        ~shortcut=Shortcuts.manual_probe(),
        switch (probe_status) {
        | Manual(_) => "Remove probe"
        | REPL => "Switch to manual"
        | Non => "Add probe"
        },
        inject,
        Probe(ToggleManual),
      ),
    ]
  | _ => []
  };

let auto_probe =
    (
      ~inject: Action.t => Ui_effect.t(unit),
      ~can_probe: bool,
      probe_status: ProbePerform.probe_status,
      ci: option(Language.Info.t),
    ) =>
  switch (ci) {
  /* Auto probes only make sense on expressions, and only if
     the term is actually probeable (not types, labels, etc.) */
  | Some(InfoExp(_)) when can_probe => [
      menu_item(
        ~shortcut=Shortcuts.auto_probe(),
        switch (probe_status) {
        | Manual(_) => "Switch to auto"
        | REPL => "Remove auto probe"
        | Non => "Add auto probe"
        },
        inject,
        Probe(ToggleAuto),
      ),
    ]
  | _ => []
  };

let jump_to_binding =
    (~inject: Action.t => Ui_effect.t(unit), ci: option(Language.Info.t)) =>
  switch (OptUtil.and_then(Language.Info.get_binding_site, ci)) {
  | Some(_) => [
      menu_item(
        ~shortcut=Shortcuts.goto_definition,
        "Goto definition",
        inject,
        Move(Goal(BindingSiteOfIndicatedVar)),
      ),
    ]
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
  let probe_status = ProbePerform.probe_status(id, info_map, z.refractors);
  let can_probe = ProbePerform.can_probe(id, info_map);
  jump_to_binding(~inject, ci)
  @ manual_probe(~inject, ~can_probe, probe_status, ci)
  @ auto_probe(~inject, ~can_probe, probe_status, ci);
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
