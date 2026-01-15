open Js_of_ocaml;
open Haz3lcore;
open Virtual_dom.Vdom;
open Util;
open Util.OptUtil.Syntax;
open WebUtil;
open Node;

/* Menu dimensions for viewport calculations */
let menu_height_estimate = 200.0; /* px */
let menu_width_estimate = 180.0; /* px - based on min-width: 160px + padding */

/* Opening direction types */
type vertical_dir = [
  | `Up
  | `Down
];
type horizontal_dir = [
  | `Left
  | `Right
];
type open_direction = {
  vertical: vertical_dir,
  horizontal: horizontal_dir,
};

/* Available space in each direction from a point */
type available_space = {
  above: float,
  below: float,
  left: float,
  right: float,
};

/* Get available space from a caret point relative to the #main viewport */
let get_available_space =
    (
      point: Point.t,
      font_metrics: FontMetrics.t,
      code_container: Js.t(Dom_html.element),
    )
    : available_space => {
  /* Get the #main viewport rect */
  let main_rect =
    switch (JsUtil.get_elem_by_id_opt("main")) {
    | Some(main) => main##getBoundingClientRect
    | None =>
      /* Fallback to window dimensions */
      Js.Unsafe.obj([|
        ("top", Js.Unsafe.inject(0.0)),
        ("bottom", Js.Unsafe.inject(Js.Unsafe.global##.innerHeight)),
        ("left", Js.Unsafe.inject(0.0)),
        ("right", Js.Unsafe.inject(Js.Unsafe.global##.innerWidth)),
      |])
    };

  /* Get code-container rect for coordinate conversion */
  let container_rect = code_container##getBoundingClientRect;

  /* Calculate pixel position of caret point in viewport coordinates */
  let caret_left =
    container_rect##.left +. Float.of_int(point.col) *. font_metrics.col_width;
  let caret_top =
    container_rect##.top
    +. Float.of_int(point.row + 1)
    *. font_metrics.row_height;

  {
    above: caret_top -. main_rect##.top,
    below: main_rect##.bottom -. caret_top,
    left: caret_left -. main_rect##.left,
    right: main_rect##.right -. caret_left,
  };
};

/* Determine which direction the menu should open based on available space */
let determine_direction = (space: available_space): open_direction => {
  vertical: space.below >= menu_height_estimate ? `Down : `Up,
  horizontal: space.right >= menu_width_estimate ? `Right : `Left,
};

/* Get CSS class for direction */
let direction_class = (dir: open_direction): string =>
  switch (dir) {
  | {vertical: `Down, horizontal: `Right} => "open-down-right"
  | {vertical: `Down, horizontal: `Left} => "open-down-left"
  | {vertical: `Up, horizontal: `Right} => "open-up-right"
  | {vertical: `Up, horizontal: `Left} => "open-up-left"
  };

/* Gap between caret bottom and menu top (in pixels) */
let caret_menu_gap = 0.0;

/* Calculate the caret's bottom edge offset from row origin.
   The caret occupies one row height plus a small shadow.
   Note: shaped carets (chevrons) don't extend beyond the row -
   the chevron shape stays within the row boundary. */
let caret_bottom_offset = (font_metrics: FontMetrics.t): float => {
  let row_height = font_metrics.row_height;
  /* Shadow extends slightly below the caret */
  let shadow = ShardDec.shadow_dy *. row_height;
  row_height +. shadow;
};

/* Calculate position style based on direction */
let pos_style =
    (point: Point.t, font_metrics: FontMetrics.t, direction: open_direction)
    : string => {
  let left = Float.of_int(point.col) *. font_metrics.col_width;

  /* Calculate precise top position based on caret bottom edge */
  let caret_top = Float.of_int(point.row) *. font_metrics.row_height;
  let caret_bottom = caret_top +. caret_bottom_offset(font_metrics);

  let top =
    switch (direction.vertical) {
    | `Down => caret_bottom +. caret_menu_gap
    | `Up => caret_top -. caret_menu_gap /* CSS transform will flip */
    };

  Printf.sprintf("position: absolute; left: %fpx; top: %fpx;", left, top);
};

/* Keyboard shortcut display - abstracts the format for easy updates */
let shortcut_view = (shortcut: string) =>
  span(~attrs=[clss(["menu-shortcut"])], [text(shortcut)]);

/* Styled colon separator */
let colon_sep = span(~attrs=[clss(["menu-colon"])], [text(" : ")]);

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

/* Keyboard shortcuts - platform-dependent */
module Shortcuts = {
  let manual_probe = () => Os.is_mac^ ? "⌘E" : "Ctrl+E";
  let auto_probe = () => Os.is_mac^ ? "⇧⌘E" : "Ctrl+Shift+E";
  let goto_definition = "F12";
  let fold = () => Os.is_mac^ ? "⌥F" : "Alt+F";
  let type_annotation = () => Os.is_mac^ ? "⌥T" : "Alt+T";
  let livelit = () => Os.is_mac^ ? "⌥L" : "Alt+L";
};

let manual_probe =
    (
      ~inject: Action.t => Ui_effect.t(unit),
      ~can_probe: bool,
      ~has_statics: bool,
      probe_status: ProbePerform.probe_status,
      ci: option(Language.Info.t),
    ) =>
  switch (ci) {
  /* These can be applied to expressions and patterns, but only if
     the term is actually probeable (not types, labels, etc.) */
  | Some(InfoExp(_) | InfoPat(_)) when can_probe => [
      menu_item(
        ~shortcut=Shortcuts.manual_probe(),
        switch (probe_status, has_statics) {
        | (Manual(_), false) => "Remove probe"
        | (Manual(_), true)
        | (REPL, _)
        | (Non, true) => "Switch to manual"
        | (Non, false) => "Add probe"
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
      ~has_statics: bool,
      probe_status: ProbePerform.probe_status,
      ci: option(Language.Info.t),
    ) =>
  switch (ci) {
  /* Auto probes only make sense on expressions, and only if
     the term is actually probeable (not types, labels, etc.) */
  | Some(InfoExp(_)) when can_probe => [
      menu_item(
        ~shortcut=Shortcuts.auto_probe(),
        switch (probe_status, has_statics) {
        | (Manual(_), _)
        | (REPL, true)
        | (Non, true) => "Switch to auto"
        | (REPL, false) => "Remove auto probe"
        | (Non, false) => "Add auto probe"
        },
        inject,
        Probe(ToggleAuto),
      ),
    ]
  | _ => []
  };

let type_annotation =
    (
      ~inject: Action.t => Ui_effect.t(unit),
      ~can_type: bool,
      ~has_statics: bool,
      probe_status: ProbePerform.probe_status,
      ci: option(Language.Info.t),
    ) =>
  switch (ci) {
  /* Type annotations can be placed on expressions and patterns */
  | Some(InfoExp(_) | InfoPat(_)) when can_type => [
      menu_item(
        ~shortcut=Shortcuts.type_annotation(),
        switch (has_statics, probe_status) {
        | (true, _) => "Remove statics"
        | (false, Manual(_))
        | (false, REPL) => "Switch to statics"
        | (false, Non) => "Add statics"
        },
        inject,
        Probe(ToggleStatics),
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

/* Divider element for separating menu sections */
let divider = div(~attrs=[clss(["menu-divider"])], []);

/* Module for determining applicable projectors */
module Projectors = {
  /* Get the term to target for projection from the zipper */
  let target_term = (z: Zipper.t, info_map: Language.Statics.Map.t) =>
    switch (z.selection.content) {
    | [] =>
      switch (Indicated.for_index(z)) {
      | Some((Projector({syntax, _}), _, _)) =>
        MakeTerm.for_projection(Piece.unparenthesize(syntax))
      | _ =>
        let* info = Indicated.ci_of(z, info_map);
        Language.Info.any_of(info);
      }
    | [Projector({syntax, _})] =>
      MakeTerm.for_projection(Piece.unparenthesize(syntax))
    | seg => MakeTerm.for_projection(seg)
    };

  /* Check if a projector kind is applicable to the current term */
  let is_applicable =
      (
        z: Zipper.t,
        info_map: Language.Statics.Map.t,
        kind: ProjectorCore.Kind.t,
      )
      : option(ProjectorCore.Kind.t) => {
    let (module P) = ProjectorInit.to_module(kind);
    let* term = target_term(z, info_map);
    let+ _ = P.init(term);
    kind;
  };

  /* Get the kind of projector on the indicated piece, if any */
  let indicated_kind = (z: Zipper.t): option(ProjectorCore.Kind.t) => {
    let* (piece, _, _) = Indicated.for_index(z);
    switch (piece) {
    | Projector({kind, _}) => Some(kind)
    | _ => None
    };
  };

  /* Get keyboard shortcut for a projector kind */
  let shortcut_of = (kind: ProjectorCore.Kind.t): string =>
    switch (kind) {
    | Fold => Shortcuts.fold()
    | Statics => Shortcuts.type_annotation()
    | _ => Shortcuts.livelit()
    };

  /* Get display name for a projector kind */
  let display_name = (kind: ProjectorCore.Kind.t): string =>
    switch (kind) {
    | Fold => "Fold"
    | Statics => "Statics"
    | Checkbox => "Checkbox"
    | Slider => "Slider"
    | SliderF => "SliderF"
    | Card => "Card"
    | TextArea => "Text"
    | Csv => "CSV"
    | Livelit => "Livelit"
    | Probe => "Probe" /* shouldn't appear in menu */
    };

  /* Generate menu items for projectors */
  let actions =
      (
        ~inject: Action.t => Ui_effect.t(unit),
        z: Zipper.t,
        info_map: Language.Statics.Map.t,
      )
      : list(Node.t) => {
    let current_kind = indicated_kind(z);

    /* Get applicable projectors: Fold and first applicable livelit */
    let fold_applicable = is_applicable(z, info_map, Fold);
    let livelit_applicable =
      List.find_map(
        is_applicable(z, info_map),
        ProjectorCore.Kind.livelit_projectors,
      );

    let applicable =
      List.filter_map(Fun.id, [fold_applicable, livelit_applicable]);

    /* Generate menu item for a projector kind with styled "Project : Name" format */
    let make_item = (kind: ProjectorCore.Kind.t): Node.t => {
      let name = display_name(kind);
      let shortcut = shortcut_of(kind);
      let is_current = current_kind == Some(kind);
      let prefix = is_current ? "Remove" : "Add";
      div(
        ~attrs=[
          Attr.on_pointerdown(_ =>
            Effect.Many([
              Effect.Stop_propagation,
              Effect.Prevent_default,
              inject(Project(SetIndicated(Specific(kind)))),
            ])
          ),
          clss(["named-menu-item"]),
        ],
        [
          text(prefix ++ " "),
          /*colon_sep,*/ text(name),
          shortcut_view(shortcut),
        ],
      );
    };

    List.map(make_item, applicable);
  };
};

let refractor_actions =
    (
      ~inject: Action.t => Ui_effect.t(unit),
      info_map: Language.Statics.Map.t,
      z: Zipper.t,
    ) => {
  let id = Indicated.index(z) |> Option.value(~default=Id.invalid);
  let ci = Indicated.ci_of(z, info_map);
  let probe_status = ProbePerform.probe_status(id, info_map, z.refractors);
  let can_probe = ProbePerform.can_probe(id, info_map);
  let can_statics = ProbePerform.can_statics(id, info_map);
  let has_statics =
    Id.Map.find_opt(id, z.refractors.manuals)
    |> Option.map((e: Refractors.entry) => e.kind == Statics)
    |> Option.value(~default=false);
  manual_probe(~inject, ~can_probe, ~has_statics, probe_status, ci)
  @ auto_probe(~inject, ~can_probe, ~has_statics, probe_status, ci)
  @ type_annotation(
      ~inject,
      ~can_type=can_statics,
      ~has_statics,
      probe_status,
      ci,
    );
};

let context_menu = menu_items =>
  NutMenu.submenu(
    ~tooltip="",
    ~icon=div([]),
    [div_c("group", [div_c("contents", menu_items)])],
  );

/* Get direction by querying the DOM for container position */
let get_direction =
    (point: Point.t, font_metrics: FontMetrics.t): open_direction => {
  /* Try to find a code-container to calculate viewport position */
  let container_opt =
    try(Some(JsUtil.get_elem_by_selector(".code-container"))) {
    | _ => None
    };

  switch (container_opt) {
  | Some(container) =>
    let space = get_available_space(point, font_metrics, container);
    determine_direction(space);
  | None =>
    /* Fallback to default direction */
    {
      vertical: `Down,
      horizontal: `Right,
    }
  };
};

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

  /* Get all action categories */
  let navigation_items = {
    let ci = Indicated.ci_of(z, info_map);
    jump_to_binding(~inject, ci);
  };

  let refractor_items = refractor_actions(~inject, info_map, z);

  let projector_items = Projectors.actions(~inject, z, info_map);

  /* Combine with dividers between non-empty sections */
  let sections = [navigation_items, refractor_items, projector_items];
  let non_empty_sections = List.filter(s => s != [], sections);
  let menu_items =
    List.concat(
      List.mapi(
        (i, section) =>
          if (i > 0) {
            [divider, ...section];
          } else {
            section;
          },
        non_empty_sections,
      ),
    );

  if (menu_items == []) {
    div([]);
  } else {
    /* Calculate opening direction based on viewport space */
    let direction = get_direction(caret_point, font_metrics);
    let dir_class = direction_class(direction);
    let style = pos_style(caret_point, font_metrics, direction);

    div(
      ~attrs=[
        Attr.classes(["context-menu", "nut-menu", dir_class]),
        Attr.create("style", style),
      ],
      [context_menu(menu_items)],
    );
  };
};
