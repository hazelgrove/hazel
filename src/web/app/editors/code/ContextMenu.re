open Js_of_ocaml;
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

/* `data-open` value for the editor menu's open direction, read by the CSS
 * that hangs the menu off its anchor. The column menu has its own `cm-*`
 * class scheme keyed to th-anchored transforms, so this mapping stays local
 * to the editor menu. */
let direction_name = (dir: Menu.open_direction): string =>
  switch (dir) {
  | {vertical: `Down, horizontal: `Right} => "down-right"
  | {vertical: `Down, horizontal: `Left} => "down-left"
  | {vertical: `Up, horizontal: `Right} => "up-right"
  | {vertical: `Up, horizontal: `Left} => "up-left"
  };

let caret_bottom_offset = (font_metrics: FontMetrics.t): float => {
  let row_height = font_metrics.row_height;
  let shadow = ShardDec.shadow_dy *. row_height;
  row_height +. shadow;
};

/* The menu's anchor spans the caret's row, from its top to the bottom of
 * its shadow, in the editor's coordinates. A menu opening down hangs from
 * its bottom, one opening up from its top. */
let anchor_style = (point: Point.t, font_metrics: FontMetrics.t): string =>
  Printf.sprintf(
    "left: %fpx; top: %fpx; height: %fpx;",
    Float.of_int(point.col) *. font_metrics.col_width,
    Float.of_int(point.row) *. font_metrics.row_height,
    caret_bottom_offset(font_metrics),
  );

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
      probe_status: ProbeTargets.probe_status,
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
      probe_status: ProbeTargets.probe_status,
      ci: option(Language.Info.t),
    )
    : list(Menu.item(command)) =>
  switch (ci) {
  | Some(InfoExp(_) | InfoPat(_)) when can_type => [
      action_item(
        ~shortcut=Shortcuts.type_annotation(),
        switch (probe_status) {
        | Statics(_) => "Remove type probe"
        | Manual(_)
        | Multi => "Switch to type probe"
        | Ephemeral(_)
        | Suppressed(_)
        | Non => "Add type probe"
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
    | Keybinding => "Keybinding"
    | Color => "Color"
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
  let probe_status = ProbeTargets.probe_status(id, info_map, z.refractors);
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

/* The menu is drawn in the browser's top layer, as a popover.
 *
 * An editor can sit inside an isolated stacking context or a clipped box (a
 * result cell, a proof's inline editors). A menu laid out in such an editor
 * can't paint or catch clicks outside it, whatever its z-index, so
 * page-level UI such as the sidebar covers it. The top layer is above every
 * stacking context and outside every clip. The menu keeps its place in the
 * DOM, so its events still bubble through the editor that opened it.
 *
 * The popover is the full-viewport backdrop, and the menu is its child. Its
 * parent is a zero-width anchor at the caret, laid out in the editor.
 * Top-layer elements are positioned against the viewport, so the hook below
 * measures the anchor on every render, picks the direction from the space
 * around it, and hands both to the CSS (`--menu-left`, `--menu-top`,
 * `data-open`).
 *
 * The hook also owns ContextMenuListener (outside clicks and menu keys)
 * while the menu is in the DOM. Every editor renders, but only the one with
 * an open menu has the hook, so no other editor's render can switch the
 * listener off. */
module Layer = {
  type input = {
    on_close: unit => Ui_effect.t(unit),
    handle_key: string => option(Ui_effect.t(unit)),
  };

  /* The hook instance that last claimed the listener. An instance destroyed
   * after another one has claimed it must not release it. */
  let owner = ref(0);
  let next_id = ref(0);

  let place = (layer: Js.t(Dom_html.element)): unit =>
    switch (
      Js.Opt.to_option(layer##.parentNode)
      |> Option.map(n => Js.Opt.to_option(Dom_html.CoerceTo.element(n)))
      |> Option.join
    ) {
    | None => ()
    | Some(anchor) =>
      let dir =
        Menu.direction_from_elem(
          ~menu_height=menu_height_estimate,
          ~menu_width=menu_width_estimate,
          anchor,
        );
      let rect = anchor##getBoundingClientRect;
      let top =
        switch (dir.vertical) {
        | `Down => rect##.bottom
        | `Up => rect##.top
        };
      let set_px = (name, v) =>
        layer##.style##setProperty(
          Js.string(name),
          Js.string(Printf.sprintf("%fpx", v)),
          Js.undefined,
        )
        |> ignore;
      set_px("--menu-left", rect##.left);
      set_px("--menu-top", top);
      layer##setAttribute(
        Js.string("data-open"),
        Js.string(direction_name(dir)),
      );
    };

  let show = (layer: Js.t(Dom_html.element)): unit => {
    let el = Js.Unsafe.coerce(layer);
    if (Js.to_bool(el##.isConnected)
        && Js.Optdef.test(el##.showPopover)
        && !Js.to_bool(el##matches(Js.string(":popover-open")))) {
      Js.Unsafe.meth_call(el, "showPopover", [||]);
    };
  };

  /* Whether a press landed on the backdrop itself (not the menu) outside
   * the editor that owns the menu. */
  let press_outside_editor = (evt: Js.t(Dom_html.pointerEvent)): bool =>
    switch (
      Js.Opt.to_option(evt##.target),
      Js.Opt.to_option(evt##.currentTarget),
    ) {
    | (Some(target), Some(layer)) when target == layer =>
      switch (JsUtil.find_ancestor_with_class(layer, "code-editor")) {
      | None => true
      | Some(editor) =>
        let r = editor##getBoundingClientRect;
        let x = Float.of_int(evt##.clientX)
        and y = Float.of_int(evt##.clientY);
        x < r##.left || x > r##.right || y < r##.top || y > r##.bottom;
      }
    | _ => false
    };

  let claim = ({on_close, handle_key}: input, id: int): unit => {
    owner := id;
    ContextMenuListener.sync(~menu_open=true, ~on_close, ~handle_key, ());
  };

  module Hook =
    Attr.Hooks.Make({
      module State = Int;
      module Input = {
        type t = input;
        let sexp_of_t = (_: t) => Sexplib0.Sexp.Atom("context-menu-layer");
        let combine = (_: t, second: t) => second;
      };
      let init = (_: Input.t, _) => {
        incr(next_id);
        next_id^;
      };
      /* Placed before it is shown, so it never paints at the origin. */
      let on_mount = (input: Input.t, id: State.t, layer) => {
        place(layer);
        show(layer);
        claim(input, id);
      };
      /* Re-shown in case the node was moved, which hides a popover. */
      let update =
          (~old_input as _: Input.t, ~new_input: Input.t, id: State.t, layer) => {
        place(layer);
        show(layer);
        claim(new_input, id);
      };
      let destroy = ({on_close, _}: Input.t, id: State.t, _) =>
        if (owner^ == id) {
          owner := 0;
          ContextMenuListener.sync(~menu_open=false, ~on_close, ());
        };
    });
};

let view =
    (
      ~inject: command => Ui_effect.t(unit),
      ~inject_menu: Menu.action => Ui_effect.t(unit),
      ~on_close: unit => Ui_effect.t(unit),
      ~handle_key: string => option(Ui_effect.t(unit)),
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
    div(
      ~attrs=[
        Attr.classes(["context-menu-anchor"]),
        Attr.create("style", anchor_style(caret_point, font_metrics)),
      ],
      [
        div(
          ~attrs=[
            Attr.classes(["context-menu-backdrop"]),
            Attr.create("popover", "manual"),
            /* The menu is fixed to the viewport, so scrolling closes it. */
            Attr.on_wheel(_ => on_close()),
            /* ContextMenuListener closes the menu on any press outside it.
               A press on the backdrop over the editor that owns the menu
               also reaches the editor, to move the caret there; elsewhere
               (sidebar, other cells, top bar) it only closes the menu. */
            Attr.on_pointerdown(evt =>
              Layer.press_outside_editor(evt)
                ? Virtual_dom.Vdom.Effect.Stop_propagation
                : Virtual_dom.Vdom.Effect.Ignore
            ),
            Attr.create_hook(
              "context-menu-layer",
              Layer.Hook.create({
                on_close,
                handle_key,
              }),
            ),
          ],
          [
            div(
              ~attrs=[Attr.classes(["context-menu", "nut-menu"])],
              [div_c("group", [div_c("contents", menu_items)])],
            ),
          ],
        ),
      ],
    );
  };
};
