open Js_of_ocaml;
open Haz3lcore;
open Virtual_dom.Vdom;
open Util;
open Util.OptUtil.Syntax;
open WebUtil;
open Node;

/* Context menu state management - moved here for better encapsulation */
module Model = {
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
      | Some(n) => Some(n + 1) /* Clamped by WithContext.update */
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

/* Keyboard shortcuts - platform-dependent */
module Shortcuts = {
  let manual_probe = () => Os.is_mac^ ? "⌘E" : "Ctrl+E";
  let auto_probe = () => Os.is_mac^ ? "⇧⌘E" : "Ctrl+Shift+E";
  let goto_definition = "F12";
  let fold = () => Os.is_mac^ ? "⌥F" : "Alt+F";
  let type_annotation = () => Os.is_mac^ ? "⌥T" : "Alt+T";
  let livelit = () => Os.is_mac^ ? "⌥L" : "Alt+L";
  let introduce = () => Os.is_mac^ ? "⌘I" : "Ctrl+I";
  let select_current_term = () => Os.is_mac^ ? "⌘D" : "Ctrl+D";
};

/* Data-returning versions for keyboard navigation */
let manual_probe_data =
    (
      ~can_probe: bool,
      probe_status: ProbePerform.probe_status,
      ci: option(Language.Info.t),
    )
    : list(menu_item_data) =>
  switch (ci) {
  | Some(InfoExp(_) | InfoPat(_)) when can_probe => [
      {
        name:
          switch (probe_status) {
          | Manual(_) => "Remove probe"
          | Statics(_)
          | Auto => "Switch to manual"
          | Non => "Add probe"
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
      probe_status: ProbePerform.probe_status,
      ci: option(Language.Info.t),
    )
    : list(menu_item_data) =>
  switch (ci) {
  | Some(InfoExp(_)) when can_probe => [
      {
        name:
          switch (probe_status) {
          | Manual(_)
          | Statics(_) => "Switch to auto"
          | Auto => "Remove auto probe"
          | Non => "Add auto probe"
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
      probe_status: ProbePerform.probe_status,
      ci: option(Language.Info.t),
    )
    : list(menu_item_data) =>
  switch (ci) {
  | Some(InfoExp(_) | InfoPat(_)) when can_type => [
      {
        name:
          switch (probe_status) {
          | Statics(_) => "Remove statics"
          | Manual(_)
          | Auto => "Switch to statics"
          | Non => "Add statics"
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

/* Check if Introduce is applicable (empty hole with introducable type) */
let introduce_data = (ci: option(Language.Info.t)): list(menu_item_data) =>
  switch (ci) {
  | Some(
      Language.Info.InfoExp({
        cls: Exp(EmptyHole),
        status: NotInHole(Common(Ana(Consistent({ana, _})))),
        ctx,
        _,
      }),
    )
      when
        Introduce.can_introduce_exp_type(
          Language.Typ.weak_head_normalize(ctx, ana),
        ) => [
      {
        name: "Introduce",
        shortcut: Some(Shortcuts.introduce()),
        action: Introduce,
      },
    ]
  | Some(
      Language.Info.InfoPat({
        cls: Pat(EmptyHole),
        status: NotInHole(Ana(Consistent({ana, _}))),
        ctx,
        _,
      }),
    )
      when
        Introduce.can_introduce_pat_type(
          Language.Typ.weak_head_normalize(ctx, ana),
        ) => [
      {
        name: "Introduce",
        shortcut: Some(Shortcuts.introduce()),
        action: Introduce,
      },
    ]
  | _ => []
  };

/* Select current term - always available */
let select_current_term_data = (): list(menu_item_data) => [
  {
    name: "Select term",
    shortcut: Some(Shortcuts.select_current_term()),
    action: Select(Term(Current)),
  },
];

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
    | Graph => "Graph"
    | Patchwork => "Patchwork"
    | ObservablePlot => "Plot"
    | Automerge => "Automerge"
    | Exo(exo_kind) => Exo.name(exo_kind)
    };

  /* Get applicable projector kinds - iterates over all projectors */
  let applicable_kinds =
      (z: Zipper.t, info_map: Language.Statics.Map.t)
      : list(ProjectorCore.Kind.t) =>
    List.filter_map(
      is_applicable(z, info_map),
      ProjectorCore.Kind.projectors,
    );

  /* Data-returning version for keyboard navigation */
  let actions_data =
      (z: Zipper.t, info_map: Language.Statics.Map.t): list(menu_item_data) => {
    let current_kind = indicated_kind(z);
    let applicable = applicable_kinds(z, info_map);

    let make_item_data = (kind: ProjectorCore.Kind.t): menu_item_data => {
      let name = display_name(kind);
      let shortcut = shortcut_of(kind);
      let prefix =
        switch (current_kind) {
        | Some(k) when k == kind => "Remove"
        | Some(_) => "Switch to"
        | None => "Add"
        };
      {
        name: prefix ++ " " ++ name,
        shortcut: Some(shortcut),
        action: Project(SetIndicated(Specific(kind))),
      };
    };

    List.map(make_item_data, applicable);
  };
};

/* Data-returning version of refractor_actions */
let refractor_actions_data =
    (
      ~ci: option(Language.Info.t),
      info_map: Language.Statics.Map.t,
      z: Zipper.t,
    )
    : list(menu_item_data) => {
  let id = Indicated.index(z) |> Option.value(~default=Id.invalid);
  let probe_status = ProbePerform.probe_status(id, info_map, z.refractors);
  let can_probe = ProbePerform.can_probe(id, info_map);
  let can_statics = ProbePerform.can_statics(id, info_map);
  manual_probe_data(~can_probe, probe_status, ci)
  @ auto_probe_data(~can_probe, probe_status, ci)
  @ type_annotation_data(~can_type=can_statics, probe_status, ci);
};

/*
 * ============================================================================
 * MENU STRUCTURE
 * ============================================================================
 * To add a new menu item:
 * 1. Create a `*_data` function that returns list(menu_item_data)
 * 2. Add it to the appropriate section in get_sections below
 * That's it!
 * ============================================================================
 */

/* Get menu sections - each section is separated by a divider.
   This is the single source of truth for menu structure. */
let get_sections =
    (~info_map: Language.Statics.Map.t, z: Zipper.t)
    : list(list(menu_item_data)) => {
  let ci = Indicated.ci_of(z, info_map);

  [
    /* Section 1: Navigation & Selection */
    jump_to_binding_data(ci) @ select_current_term_data(),
    /* Section 2: Refactoring */
    introduce_data(ci),
    /* Section 3: Probes/Statics (refractors) */
    refractor_actions_data(~ci, info_map, z),
    /* Section 4: Projectors (fold, livelits) */
    Projectors.actions_data(z, info_map),
  ]
  |> List.filter(section => section != []);
};

/* Get all menu items as a flat list */
let get_all_items =
    (~info_map: Language.Statics.Map.t, z: Zipper.t): list(menu_item_data) =>
  List.concat(get_sections(~info_map, z));

/* Get action at index (for Enter key activation) */
let get_action_at_index =
    (~info_map: Language.Statics.Map.t, z: Zipper.t, index: int)
    : option(Action.t) => {
  let items = get_all_items(~info_map, z);
  List.nth_opt(items, index) |> Option.map(item => item.action);
};

/* Operations that need editor context (info_map, zipper).
   This module consolidates context-menu logic that would otherwise
   leak into CodeEditable, keeping the coupling explicit. */
module WithContext = {
  /* Result of handling a key event */
  type key_result =
    | MenuUpdate(Model.action) /* Update menu state */
    | EditorAction(Action.t) /* Dispatch editor action */
    | Unhandled; /* Key not handled, fall through */

  /* Update menu state with clamping to valid item range */
  let update =
      (~info_map: Language.Statics.Map.t, ~zipper: Zipper.t, action, state)
      : Model.t => {
    let new_state = Model.update(action, state);
    switch (new_state) {
    | Some(n) =>
      let item_count = List.length(get_all_items(~info_map, zipper));
      Some(max(0, min(n, item_count - 1)));
    | None => None
    };
  };

  /* Handle keyboard input when menu is open */
  let handle_key =
      (
        ~info_map: Language.Statics.Map.t,
        ~zipper: Zipper.t,
        key: Key.key,
        state: Model.t,
      )
      : key_result =>
    switch (state) {
    | None => Unhandled
    | Some(selected_index) =>
      switch (key) {
      | Key.D("Escape") => MenuUpdate(Close)
      | Key.D("ArrowUp") => MenuUpdate(Up)
      | Key.D("ArrowDown") => MenuUpdate(Down)
      | Key.D("Enter") =>
        switch (get_action_at_index(~info_map, zipper, selected_index)) {
        | Some(action) => EditorAction(action)
        | None => MenuUpdate(Close)
        }
      | _ => Unhandled
      }
    };
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
  let sections = get_sections(~info_map, z);

  /* Clamp selected_index to valid range */
  let item_count =
    List.fold_left((acc, s) => acc + List.length(s), 0, sections);
  let selected_index = max(0, min(selected_index, item_count - 1));

  /* Render all sections with automatic index tracking and dividers */
  let (menu_items, _) =
    List.fold_left(
      ((nodes, idx), section) => {
        /* Render items in this section with selection highlighting */
        let section_nodes =
          List.mapi(
            (i, item) =>
              menu_item_view(
                ~inject,
                ~is_selected=idx + i == selected_index,
                item,
              ),
            section,
          );
        /* Add divider before non-first sections */
        let with_divider =
          if (nodes != [] && section_nodes != []) {
            nodes @ [divider] @ section_nodes;
          } else {
            nodes @ section_nodes;
          };
        (with_divider, idx + List.length(section));
      },
      ([], 0),
      sections,
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
