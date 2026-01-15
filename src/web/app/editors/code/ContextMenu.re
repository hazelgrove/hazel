open Js_of_ocaml;
open Haz3lcore;
open Virtual_dom.Vdom;
open Util;
open Util.OptUtil.Syntax;
open WebUtil;
open Node;

/* Context menu state management - moved here for better encapsulation */
module State = {
  /* Menu state: None = closed, Some(n) = open with item n selected */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = option(int);

  let is_open = (state: t): bool => state != None;

  /* Actions that can be performed on the context menu */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | Toggle
    | Open
    | Close
    | Up
    | Down
    | Activate;

  /* Pure update function for context menu state */
  let update = (action: action, state: t): t =>
    switch (action) {
    | Toggle =>
      switch (state) {
      | None => Some(0) /* Open with first item selected */
      | Some(_) => None /* Close */
      }
    | Open => Some(0)
    | Close => None
    | Up =>
      switch (state) {
      | None => None
      | Some(n) => Some(max(0, n - 1))
      }
    | Down =>
      switch (state) {
      | None => None
      | Some(n) => Some(n + 1) /* Will be clamped in view */
      }
    | Activate =>
      /* Activation is handled by the caller which executes the action */
      state
    };
};

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

/* Menu item data - separates action info from rendering */
type menu_item_data = {
  name: string,
  shortcut: option(string),
  action: Action.t,
};

/* Keyboard shortcut display - abstracts the format for easy updates */
let shortcut_view = (shortcut: string) =>
  span(~attrs=[clss(["menu-shortcut"])], [text(shortcut)]);

/* Styled colon separator */
let colon_sep = span(~attrs=[clss(["menu-colon"])], [text(" : ")]);

/* Render a menu item with optional selection highlight */
let menu_item_view =
    (
      ~inject: Action.t => Ui_effect.t(unit),
      ~is_selected: bool,
      item: menu_item_data,
    ) =>
  div(
    ~attrs=[
      Attr.on_pointerdown(_ =>
        Effect.Many([
          Effect.Stop_propagation,
          Effect.Prevent_default,
          inject(item.action),
        ])
      ),
      clss(["named-menu-item"] @ (is_selected ? ["selected"] : [])),
    ],
    [text(item.name)]
    @ (
      switch (item.shortcut) {
      | Some(s) => [shortcut_view(s)]
      | None => []
      }
    ),
  );

/* Legacy menu_item for compatibility with projector items */
let menu_item =
    (
      ~shortcut: option(string)=?,
      name: string,
      inject: Action.t => Ui_effect.t(unit),
      action: Action.t,
    ) =>
  menu_item_view(
    ~inject,
    ~is_selected=false,
    {
      name,
      shortcut,
      action,
    },
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

/* Data-returning versions for keyboard navigation */
let manual_probe_data =
    (
      ~can_probe: bool,
      ~has_statics: bool,
      probe_status: ProbePerform.probe_status,
      ci: option(Language.Info.t),
    )
    : list(menu_item_data) =>
  switch (ci) {
  | Some(InfoExp(_) | InfoPat(_)) when can_probe => [
      {
        name:
          switch (probe_status, has_statics) {
          | (Manual(_), false) => "Remove probe"
          | (Manual(_), true)
          | (REPL, _)
          | (Non, true) => "Switch to manual"
          | (Non, false) => "Add probe"
          },
        shortcut: Some(Shortcuts.manual_probe()),
        action: Probe(ToggleManual),
      },
    ]
  | _ => []
  };

let auto_probe_data =
    (
      ~can_probe: bool,
      ~has_statics: bool,
      probe_status: ProbePerform.probe_status,
      ci: option(Language.Info.t),
    )
    : list(menu_item_data) =>
  switch (ci) {
  | Some(InfoExp(_)) when can_probe => [
      {
        name:
          switch (probe_status, has_statics) {
          | (Manual(_), _)
          | (REPL, true)
          | (Non, true) => "Switch to auto"
          | (REPL, false) => "Remove auto probe"
          | (Non, false) => "Add auto probe"
          },
        shortcut: Some(Shortcuts.auto_probe()),
        action: Probe(ToggleAuto),
      },
    ]
  | _ => []
  };

let type_annotation_data =
    (
      ~can_type: bool,
      ~has_statics: bool,
      probe_status: ProbePerform.probe_status,
      ci: option(Language.Info.t),
    )
    : list(menu_item_data) =>
  switch (ci) {
  | Some(InfoExp(_) | InfoPat(_)) when can_type => [
      {
        name:
          switch (has_statics, probe_status) {
          | (true, _) => "Remove statics"
          | (false, Manual(_))
          | (false, REPL) => "Switch to statics"
          | (false, Non) => "Add statics"
          },
        shortcut: Some(Shortcuts.type_annotation()),
        action: Probe(ToggleStatics),
      },
    ]
  | _ => []
  };

let jump_to_binding_data =
    (ci: option(Language.Info.t)): list(menu_item_data) =>
  switch (OptUtil.and_then(Language.Info.get_binding_site, ci)) {
  | Some(_) => [
      {
        name: "Goto definition",
        shortcut: Some(Shortcuts.goto_definition),
        action: Move(Goal(BindingSiteOfIndicatedVar)),
      },
    ]
  | _ => []
  };

/* Legacy versions that return rendered nodes */
let manual_probe =
    (
      ~inject: Action.t => Ui_effect.t(unit),
      ~can_probe: bool,
      ~has_statics: bool,
      probe_status: ProbePerform.probe_status,
      ci: option(Language.Info.t),
    ) =>
  manual_probe_data(~can_probe, ~has_statics, probe_status, ci)
  |> List.map(menu_item_view(~inject, ~is_selected=false));

let auto_probe =
    (
      ~inject: Action.t => Ui_effect.t(unit),
      ~can_probe: bool,
      ~has_statics: bool,
      probe_status: ProbePerform.probe_status,
      ci: option(Language.Info.t),
    ) =>
  auto_probe_data(~can_probe, ~has_statics, probe_status, ci)
  |> List.map(menu_item_view(~inject, ~is_selected=false));

let type_annotation =
    (
      ~inject: Action.t => Ui_effect.t(unit),
      ~can_type: bool,
      ~has_statics: bool,
      probe_status: ProbePerform.probe_status,
      ci: option(Language.Info.t),
    ) =>
  type_annotation_data(~can_type, ~has_statics, probe_status, ci)
  |> List.map(menu_item_view(~inject, ~is_selected=false));

let jump_to_binding =
    (~inject: Action.t => Ui_effect.t(unit), ci: option(Language.Info.t)) =>
  jump_to_binding_data(ci)
  |> List.map(menu_item_view(~inject, ~is_selected=false));

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

  /* Get applicable projector kinds */
  let applicable_kinds =
      (z: Zipper.t, info_map: Language.Statics.Map.t)
      : list(ProjectorCore.Kind.t) => {
    let fold_applicable = is_applicable(z, info_map, Fold);
    let livelit_applicable =
      List.find_map(
        is_applicable(z, info_map),
        ProjectorCore.Kind.livelit_projectors,
      );
    List.filter_map(Fun.id, [fold_applicable, livelit_applicable]);
  };

  /* Data-returning version for keyboard navigation */
  let actions_data =
      (z: Zipper.t, info_map: Language.Statics.Map.t): list(menu_item_data) => {
    let current_kind = indicated_kind(z);
    let applicable = applicable_kinds(z, info_map);

    let make_item_data = (kind: ProjectorCore.Kind.t): menu_item_data => {
      let name = display_name(kind);
      let shortcut = shortcut_of(kind);
      let is_current = current_kind == Some(kind);
      let prefix = is_current ? "Remove" : "Add";
      {
        name: prefix ++ " " ++ name,
        shortcut: Some(shortcut),
        action: Project(SetIndicated(Specific(kind))),
      };
    };

    List.map(make_item_data, applicable);
  };

  /* Generate menu items for projectors */
  let actions =
      (
        ~inject: Action.t => Ui_effect.t(unit),
        z: Zipper.t,
        info_map: Language.Statics.Map.t,
      )
      : list(Node.t) =>
    actions_data(z, info_map)
    |> List.map(menu_item_view(~inject, ~is_selected=false));
};

/* Data-returning version of refractor_actions */
let refractor_actions_data =
    (info_map: Language.Statics.Map.t, z: Zipper.t): list(menu_item_data) => {
  let id = Indicated.index(z) |> Option.value(~default=Id.invalid);
  let ci = Indicated.ci_of(z, info_map);
  let probe_status = ProbePerform.probe_status(id, info_map, z.refractors);
  let can_probe = ProbePerform.can_probe(id, info_map);
  let can_statics = ProbePerform.can_statics(id, info_map);
  let has_statics =
    Id.Map.find_opt(id, z.refractors.manuals)
    |> Option.map((e: Refractors.entry) => e.kind == Statics)
    |> Option.value(~default=false);
  manual_probe_data(~can_probe, ~has_statics, probe_status, ci)
  @ auto_probe_data(~can_probe, ~has_statics, probe_status, ci)
  @ type_annotation_data(
      ~can_type=can_statics,
      ~has_statics,
      probe_status,
      ci,
    );
};

let refractor_actions =
    (
      ~inject: Action.t => Ui_effect.t(unit),
      info_map: Language.Statics.Map.t,
      z: Zipper.t,
    ) =>
  refractor_actions_data(info_map, z)
  |> List.map(menu_item_view(~inject, ~is_selected=false));

/* Get all menu items as data (for keyboard navigation) */
let get_all_items =
    (~info_map: Language.Statics.Map.t, z: Zipper.t): list(menu_item_data) => {
  let ci = Indicated.ci_of(z, info_map);
  let navigation_items = jump_to_binding_data(ci);
  let refractor_items = refractor_actions_data(info_map, z);
  let projector_items = Projectors.actions_data(z, info_map);
  navigation_items @ refractor_items @ projector_items;
};

/* Get action at index (for Enter key activation) */
let get_action_at_index =
    (~info_map: Language.Statics.Map.t, z: Zipper.t, index: int)
    : option(Action.t) => {
  let items = get_all_items(~info_map, z);
  List.nth_opt(items, index) |> Option.map(item => item.action);
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
      ~selected_index: int,
      z: Haz3lcore.Zipper.t,
    )
    : Node.t => {
  let caret_point = Zipper.Caret.point(syntax.measured, z);

  /* Get menu item data for keyboard-navigable items */
  let all_items = get_all_items(~info_map, z);
  let item_count = List.length(all_items);
  /* Clamp selected_index to valid range */
  let selected_index = max(0, min(selected_index, item_count - 1));

  /* Render navigation items with selection highlighting */
  let ci = Indicated.ci_of(z, info_map);
  let nav_data = jump_to_binding_data(ci);
  let nav_count = List.length(nav_data);
  let navigation_items =
    List.mapi(
      (i, item) =>
        menu_item_view(~inject, ~is_selected=i == selected_index, item),
      nav_data,
    );

  /* Render refractor items with selection highlighting (offset by nav count) */
  let refractor_data = refractor_actions_data(info_map, z);
  let refractor_count = List.length(refractor_data);
  let refractor_items =
    List.mapi(
      (i, item) =>
        menu_item_view(
          ~inject,
          ~is_selected=i + nav_count == selected_index,
          item,
        ),
      refractor_data,
    );

  /* Render projector items with selection highlighting (offset by nav + refractor count) */
  let projector_data = Projectors.actions_data(z, info_map);
  let projector_offset = nav_count + refractor_count;
  let projector_items =
    List.mapi(
      (i, item) =>
        menu_item_view(
          ~inject,
          ~is_selected=i + projector_offset == selected_index,
          item,
        ),
      projector_data,
    );

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
