open Haz3lcore;
open Util;
open Util.OptUtil.Syntax;
open WebUtil;
open Node;

/* Editor right-click context menu.
 *
 * State, rendering, and keyboard handling all live in `Util.Menu`. This
 * file is the editor-specific *contents*: builders for the rows (probe,
 * statics, goto, introduce, select term, projectors) plus caret-anchored
 * positioning. Each `*_data` builder returns a `list(Menu.item(command))`
 * for one menu section; `get_sections` assembles them in order. */

/* Backward-compatible alias so call sites that reference
 * `ContextMenu.Model.{t, action, Open, Close, Toggle}` keep working. */
module Model = Menu;

/* Menu dimensions for viewport calculations */
let menu_height_estimate = 260.0; /* px */
let menu_width_estimate = 180.0; /* px - based on min-width: 160px + padding */

/* CSS class for the editor menu's open direction. The column menu has its
 * own `cm-*` class scheme keyed to th-anchored transforms, so this mapping
 * stays local to the editor menu. */
let direction_class = (dir: Menu.open_direction): string =>
  switch (dir) {
  | {vertical: `Down, horizontal: `Right} => "open-down-right"
  | {vertical: `Down, horizontal: `Left} => "open-down-left"
  | {vertical: `Up, horizontal: `Right} => "open-up-right"
  | {vertical: `Up, horizontal: `Left} => "open-up-left"
  };

let caret_menu_gap = 0.0;

let caret_bottom_offset = (font_metrics: FontMetrics.t): float => {
  let row_height = font_metrics.row_height;
  let shadow = ShardDec.shadow_dy *. row_height;
  row_height +. shadow;
};

let pos_style =
    (
      point: Point.t,
      font_metrics: FontMetrics.t,
      direction: Menu.open_direction,
    )
    : string => {
  let left = Float.of_int(point.col) *. font_metrics.col_width;
  let caret_top = Float.of_int(point.row) *. font_metrics.row_height;
  let caret_bottom = caret_top +. caret_bottom_offset(font_metrics);
  let top =
    switch (direction.vertical) {
    | `Down => caret_bottom +. caret_menu_gap
    | `Up => caret_top -. caret_menu_gap
    };
  Printf.sprintf("position: absolute; left: %fpx; top: %fpx;", left, top);
};

/* ============================================================
 * Menu item builders
 * ============================================================ */

/* Keyboard shortcuts - platform-dependent */
module Shortcuts = {
  let manual_probe = () => Os.is_mac^ ? "⌘E" : "Ctrl+E";
  let goto_definition = "F12";
  let fold = () => Os.is_mac^ ? "⌥F" : "Alt+F";
  let type_annotation = () => Os.is_mac^ ? "⌥T" : "Alt+T";
  let livelit = () => Os.is_mac^ ? "⌥L" : "Alt+L";
  let introduce = () => Os.is_mac^ ? "⌘I" : "Ctrl+I";
  let select_current_term = () => Os.is_mac^ ? "⌘D" : "Ctrl+D";
  let cut = () => Os.is_mac^ ? "⌘X" : "Ctrl+X";
  let copy = () => Os.is_mac^ ? "⌘C" : "Ctrl+C";
  let paste = () => Os.is_mac^ ? "⌘V" : "Ctrl+V";
};

/* What a menu row dispatches. Most rows are a plain editor action, but
 * Paste can't be: the text it inserts is only available from an async
 * system-clipboard read at click time, so the row carries the intent and
 * the view layer supplies the text (see CodeEditable.perform_from_menu). */
type command =
  | Perform(Action.t)
  | PasteFromClipboard;

let action_item = (~shortcut=?, ~tooltip=?, ~enabled=true, label, action) =>
  Menu.action_item(
    ~decoration=?shortcut,
    ~tooltip?,
    ~enabled,
    label,
    Perform(action),
  );

let command_item = (~shortcut=?, ~tooltip=?, ~enabled=true, label, command) =>
  Menu.action_item(
    ~decoration=?shortcut,
    ~tooltip?,
    ~enabled,
    label,
    command,
  );

let probe_data =
    (
      ~can_probe: bool,
      ~is_def: bool,
      probe_status: ProbePerform.probe_status,
      ci: option(Language.Info.t),
    )
    : list(Menu.item(command)) =>
  switch (ci) {
  | Some(InfoExp(_) | InfoPat(_)) when can_probe => [
      action_item(
        ~shortcut=Shortcuts.manual_probe(),
        if (is_def) {
          switch (probe_status) {
          | Multi => "Remove multi probe"
          | Manual(_) => "Remove probe"
          | Statics(_) => "Switch to multi probe"
          | Ephemeral(_) => "Hide probe"
          | Suppressed(_) => "Show probe"
          | Non => "Add multi probe"
          };
        } else {
          switch (probe_status) {
          | Manual(_) => "Remove probe"
          | Multi => "Remove probe"
          | Statics(_) => "Switch to probe"
          | Ephemeral(_) => "Hide probe"
          | Suppressed(_) => "Show probe"
          | Non => "Add probe"
          };
        },
        Action.Probe(ToggleManual),
      ),
    ]
  | _ => []
  };

let type_annotation_data =
    (
      ~can_type: bool,
      probe_status: ProbePerform.probe_status,
      ci: option(Language.Info.t),
    )
    : list(Menu.item(command)) =>
  switch (ci) {
  | Some(InfoExp(_) | InfoPat(_)) when can_type => [
      action_item(
        ~shortcut=Shortcuts.type_annotation(),
        switch (probe_status) {
        | Statics(_) => "Remove statics"
        | Manual(_)
        | Multi => "Switch to statics"
        | Ephemeral(_)
        | Suppressed(_)
        | Non => "Add statics"
        },
        Action.Probe(ToggleStatics),
      ),
    ]
  | _ => []
  };

let jump_to_binding_data =
    (ci: option(Language.Info.t)): list(Menu.item(command)) =>
  switch (OptUtil.and_then(Language.Info.get_binding_site, ci)) {
  | Some(_) => [
      action_item(
        ~shortcut=Shortcuts.goto_definition,
        "Goto definition",
        Action.Move(Goal(BindingSiteOfIndicatedVar)),
      ),
    ]
  | _ => []
  };

let introduce_data =
    (ci: option(Language.Info.t)): list(Menu.item(command)) =>
  switch (ci) {
  | Some(
      Language.Info.InfoExp({
        cls: Exp(EmptyHole),
        message: Language.Message.Exp(Common(Ana(Consistent({ana, _})))),
        ctx,
        _,
      }),
    )
      when
        Introduce.can_introduce_exp_type(
          Language.Typ.weak_head_normalize(ctx, ana),
        ) => [
      action_item(
        ~shortcut=Shortcuts.introduce(),
        "Introduce",
        Action.Introduce,
      ),
    ]
  | Some(
      Language.Info.InfoPat({
        cls: Pat(EmptyHole),
        message: Language.Message.Pat(Common(Ana(Consistent({ana, _})))),
        ctx,
        _,
      }),
    )
      when
        Introduce.can_introduce_pat_type(
          Language.Typ.weak_head_normalize(ctx, ana),
        ) => [
      action_item(
        ~shortcut=Shortcuts.introduce(),
        "Introduce",
        Action.Introduce,
      ),
    ]
  | _ => []
  };

/* Cut/Copy/Paste rows. Dispatch goes through CodeEditable's
 * clipboard-aware inject: Copy/Cut write the system clipboard before the
 * action is performed, and Paste asks for a system-clipboard read whose
 * result becomes the real Paste action.
 *
 * Cut/Copy gray out on an empty selection; Paste never does. The web has
 * no way to see whether the clipboard is empty short of reading it, and
 * that read is permission-gated and async (Chrome prompts once per
 * origin, Firefox and Safari on every read) — so checking on menu-open
 * would prompt on every right-click. Rich web editors (Google Docs, VS
 * Code for the Web) leave Paste enabled for the same reason and only
 * find out at click time. */
let clipboard_data = (z: Zipper.t): list(Menu.item(command)) => {
  let has_selection = !Selection.is_empty(z.selection);
  [
    action_item(
      ~shortcut=Shortcuts.cut(),
      ~enabled=has_selection,
      "Cut",
      Action.Cut,
    ),
    action_item(
      ~shortcut=Shortcuts.copy(),
      ~enabled=has_selection,
      "Copy",
      Action.Copy,
    ),
    command_item(~shortcut=Shortcuts.paste(), "Paste", PasteFromClipboard),
  ];
};

let select_current_term_data = (): list(Menu.item(command)) => [
  action_item(
    ~shortcut=Shortcuts.select_current_term(),
    "Select term",
    Action.Select(Term(Current)),
  ),
];

module Projectors = {
  let target_term = (z: Zipper.t, info_map: Language.Statics.Map.t) =>
    switch (z.selection.content) {
    | [] =>
      switch (Indicated.for_index(z)) {
      | Some({piece: Projector({syntax, _}), _}) =>
        MakeTerm.for_projection(Piece.unparenthesize(syntax))
      | _ =>
        let* info = Indicated.ci_of(z, info_map);
        Language.Info.any_of(info);
      }
    | [Projector({syntax, _})] =>
      MakeTerm.for_projection(Piece.unparenthesize(syntax))
    | seg => MakeTerm.for_projection(seg)
    };

  let is_applicable =
      (
        z: Zipper.t,
        info_map: Language.Statics.Map.t,
        ~elaborated: Language.Exp.t,
        kind: ProjectorCore.Kind.t,
      )
      : option(ProjectorCore.Kind.t) => {
    let (module P) = ProjectorInit.to_module(kind);
    let* term = target_term(z, info_map);
    switch (P.init(term)) {
    | Some(_) => Some(kind)
    | None =>
      if (P.elaborate_syntax) {
        switch (term) {
        | Exp(exp) =>
          let term_id = Language.Exp.rep_id(exp);
          switch (Language.Exp.find_by_id(term_id, elaborated)) {
          | Some(elab_exp) =>
            let+ _ = P.init(Exp(elab_exp));
            kind;
          | None => None
          };
        | _ => None
        };
      } else {
        None;
      }
    };
  };

  let indicated_kind = (z: Zipper.t): option(ProjectorCore.Kind.t) => {
    let* {piece, _} = Indicated.for_index(z);
    switch (piece) {
    | Projector({kind, _}) => Some(kind)
    | _ => None
    };
  };

  let shortcut_of =
      (
        ~chosen_livelit: option(ProjectorCore.Kind.t),
        kind: ProjectorCore.Kind.t,
      )
      : option(string) =>
    switch (kind) {
    | Fold => Some(Shortcuts.fold())
    | Statics => Some(Shortcuts.type_annotation())
    | _ when chosen_livelit == Some(kind) => Some(Shortcuts.livelit())
    | _ => None
    };

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
    | Table => "Table"
    | Livelit => "Livelit"
    | Probe => "Probe" /* shouldn't appear in menu */
    };

  let applicable_kinds =
      (
        z: Zipper.t,
        info_map: Language.Statics.Map.t,
        ~elaborated: Language.Exp.t,
      )
      : list(ProjectorCore.Kind.t) => {
    let fold_applicable =
      is_applicable(z, info_map, ~elaborated, Fold) |> Option.to_list;
    let livelit_applicable =
      List.filter_map(
        is_applicable(z, info_map, ~elaborated),
        ProjectorCore.Kind.livelit_projectors,
      );
    ListUtil.dedup(fold_applicable @ livelit_applicable);
  };

  let actions_data =
      (
        z: Zipper.t,
        info_map: Language.Statics.Map.t,
        ~elaborated: Language.Exp.t,
      )
      : list(Menu.item(command)) => {
    let current_kind = indicated_kind(z);
    let applicable = applicable_kinds(z, info_map, ~elaborated);
    let kinds =
      switch (current_kind) {
      | Some(k) when !List.mem(k, applicable) => applicable @ [k]
      | _ => applicable
      };
    let chosen_livelit =
      List.find_opt(
        kind => List.mem(kind, ProjectorCore.Kind.livelit_projectors),
        applicable,
      );

    let make_item = (kind: ProjectorCore.Kind.t): Menu.item(command) => {
      let name = display_name(kind);
      let shortcut = shortcut_of(~chosen_livelit, kind);
      let prefix =
        switch (current_kind) {
        | Some(k) when k == kind => "Remove"
        | Some(_) => "Switch to"
        | None => "Add"
        };
      action_item(
        ~shortcut?,
        prefix ++ " " ++ name,
        Action.Project(SetIndicated(Specific(kind))),
      );
    };

    List.map(make_item, kinds);
  };
};

let refractor_actions_data =
    (
      ~ci: option(Language.Info.t),
      info_map: Language.Statics.Map.t,
      z: Zipper.t,
    )
    : list(Menu.item(command)) => {
  let id = Indicated.index(z) |> Option.value(~default=Id.invalid);
  let probe_status = ProbePerform.probe_status(id, info_map, z.refractors);
  let can_probe = ProbePerform.can_probe(id, info_map);
  let can_statics = ProbePerform.can_statics(id, info_map);
  let is_def = ProbePerform.is_definition_form(id, info_map);
  probe_data(~can_probe, ~is_def, probe_status, ci)
  @ type_annotation_data(~can_type=can_statics, probe_status, ci);
};

/* ============================================================
 * Menu assembly
 * ============================================================
 * To add a new menu item:
 * 1. Create a `*_data` function returning list(Menu.item(command))
 * 2. Add it to the appropriate section in get_sections below
 * ============================================================ */

let get_sections =
    (
      ~info_map: Language.Statics.Map.t,
      ~elaborated: Language.Exp.t,
      z: Zipper.t,
    )
    : list(list(Menu.item(command))) => {
  let ci = Indicated.ci_of(z, info_map);
  [
    /* Section 1: Navigation & Selection */
    jump_to_binding_data(ci) @ select_current_term_data(),
    /* Section 2: Clipboard */
    clipboard_data(z),
    /* Section 3: Refactoring */
    introduce_data(ci),
    /* Section 4: Probes/Statics (refractors) */
    refractor_actions_data(~ci, info_map, z),
    /* Section 5: Projectors (fold, livelits) */
    Projectors.actions_data(z, info_map, ~elaborated),
  ]
  |> List.filter(section => section != []);
};

/* Flatten sections by interspersing `Menu.Divider` between non-empty ones. */
let flatten_sections =
    (sections: list(list(Menu.item(command)))): list(Menu.item(command)) =>
  List.fold_left(
    (acc, section) =>
      switch (acc, section) {
      | (_, []) => acc
      | ([], items) => items
      | (acc, items) => acc @ [Menu.divider] @ items
      },
    [],
    sections,
  );

let get_all_items =
    (
      ~info_map: Language.Statics.Map.t,
      ~elaborated: Language.Exp.t,
      z: Zipper.t,
    )
    : list(Menu.item(command)) =>
  flatten_sections(get_sections(~info_map, ~elaborated, z));

/* ============================================================
 * Update + keyboard
 * ============================================================ */

module WithContext = {
  /* Menu.update is pure; render/keyboard clamp internally. The context
   * params remain in the signature to avoid churning call sites. */
  let update =
      (
        ~info_map as _: Language.Statics.Map.t,
        ~elaborated as _: Language.Exp.t,
        ~zipper as _: Zipper.t,
        action: Menu.action,
        state: Menu.t,
      )
      : Menu.t =>
    Menu.update(action, state);

  /* Adapter for ContextMenuListener.sync(~handle_key). Returns
   * `Some(effect)` for handled keys, `None` to let the editor see them. */
  let handle_listener_key =
      (
        ~info_map: Language.Statics.Map.t,
        ~elaborated: Language.Exp.t,
        ~zipper: Zipper.t,
        ~dispatch_menu: Menu.action => Ui_effect.t(unit),
        ~dispatch_action: command => Ui_effect.t(unit),
        state: Menu.t,
        key_str: string,
      )
      : option(Ui_effect.t(unit)) => {
    let items = get_all_items(~info_map, ~elaborated, zipper);
    Menu.key_dispatcher(
      ~items,
      ~dispatch_menu,
      ~dispatch_action,
      state,
      key_str,
    );
  };
};

/* ============================================================
 * View
 * ============================================================ */

/* Pick a direction by treating the caret as a zero-size anchor at
 * (caret_left, caret_bottom) in viewport coordinates, then routing
 * through the shared `Menu.{space_from, direction_of}` helpers. */
let get_direction =
    (point: Point.t, font_metrics: FontMetrics.t): Menu.open_direction => {
  let container_opt =
    try(Some(JsUtil.get_elem_by_selector(".code-container"))) {
    | _ => None
    };
  switch (container_opt) {
  | None => {
      vertical: `Down,
      horizontal: `Right,
    }
  | Some(container) =>
    let rect = container##getBoundingClientRect;
    let caret_left =
      rect##.left +. Float.of_int(point.col) *. font_metrics.col_width;
    let caret_top =
      rect##.top +. Float.of_int(point.row + 1) *. font_metrics.row_height;
    let space =
      Menu.space_from(
        ~anchor_top=caret_top,
        ~anchor_bot=caret_top,
        ~anchor_left=caret_left,
        ~anchor_right=caret_left,
      );
    Menu.direction_of(
      ~menu_height=menu_height_estimate,
      ~menu_width=menu_width_estimate,
      space,
    );
  };
};

let view =
    (
      ~inject: command => Ui_effect.t(unit),
      ~inject_menu: Menu.action => Ui_effect.t(unit),
      ~syntax: Haz3lcore.CachedSyntax.t,
      ~info_map: Language.Statics.Map.t,
      ~elaborated: Language.Exp.t,
      ~font_metrics: FontMetrics.t,
      ~model: Menu.t,
      z: Haz3lcore.Zipper.t,
    )
    : Node.t => {
  let caret_point = Zipper.Caret.point(syntax.measured, z);
  let items = get_all_items(~info_map, ~elaborated, z);
  let menu_items =
    Menu.render(
      ~inject_action=inject,
      ~inject_menu,
      ~item_class="named-menu-item",
      ~items,
      model,
    );

  if (menu_items == []) {
    div([]);
  } else {
    let direction = get_direction(caret_point, font_metrics);
    let dir_class = direction_class(direction);
    let style = pos_style(caret_point, font_metrics, direction);

    div(
      ~attrs=[
        Attr.classes(["context-menu", "nut-menu", dir_class]),
        Attr.create("style", style),
      ],
      [div_c("group", [div_c("contents", menu_items)])],
    );
  };
};
