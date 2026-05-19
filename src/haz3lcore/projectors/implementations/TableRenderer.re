open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;
open TableCore;
open TableTransforms;

/* TableRenderer - A reusable module for rendering interactive tables with column operations */

[@deriving (show({with_path: false}), sexp, yojson)]
type v = (list(option(string)), list(list(Exp.t))); /* (headers, rows) */

/* (column index, submenu path, selected action-item index) */
[@deriving (show({with_path: false}), sexp, yojson)]
type menu_state = option((int, list(string), int));
[@deriving (show({with_path: false}), sexp, yojson)]
type m = {menu_state};
[@deriving (show({with_path: false}), sexp, yojson)]
type a =
  | CloseMenu
  | ShowMenu(int)
  | ShowSubmenu(list(string))
  | MenuSelect(int);

[@deriving (show({with_path: false}), sexp, yojson)]
type model = m;

/* Table actions that can be performed on columns */
[@deriving (show({with_path: false}), sexp, yojson)]
type action = a;

[@deriving (show({with_path: false}), sexp, yojson)]
type value = v;

/* Column menu items use the shared `Util.Menu` framework. */
type menu_data = list(Menu.item(unit => Ui_effect.t(unit)));

/* Reusable UI components */
let icon_button = (~tooltip="", icon_text, action) =>
  Node.div(
    ~attrs=[
      Attr.classes(["icon", "closure-nav-button"]),
      Attr.on_click(action),
      Attr.title(tooltip),
    ],
    [Node.text(icon_text)],
  );

/* Parse an expression into table structure */
let parse = (_sort: Sort.t, exp: Exp.t) => parse_table(exp);

/* Initialize table model from parsed value */
let init = (_: v) => {menu_state: None};

/* Local builders that wrap Menu.item constructors with the column menu's
 * conventions: hover updates selection, tooltips on every leaf row. */
let leaf = (~tooltip, label, action) =>
  Menu.action_item(~tooltip, ~on_hover=true, label, action);

let submenu_row = (~tooltip, label, target_path) =>
  Menu.submenu_item(~tooltip, label, target_path);

let build_column_menu =
    (
      info: info,
      h: string,
      dyn_type: option(Typ.t),
      local: action => Ui_effect.t(unit),
      parent: external_action => Ui_effect.t(unit),
      menu_path: list(string),
    ) => {
  let column_type =
    dyn_type |> Option.bind(_, ty => get_column_type_from_ty(ty, h));
  let columns_opt = dyn_type |> Option.bind(_, get_columns);
  let can_move_left = can_move_column(columns_opt, h, true);
  let can_move_right = can_move_column(columns_opt, h, false);
  let apply = ts =>
    switch (to_segment(info, ts)) {
    | Some(seg) => Effect.Many([local(CloseMenu), parent(SetSyntax(seg))])
    | None => local(CloseMenu)
    };

  // If we're in a submenu, show that submenu
  switch (menu_path) {
  | ["Filter"] =>
    let numeric_comparators: list((string, string, Operators.op_bin_num)) = [
      (
        "Greater than",
        "Keep rows where this column is greater than a value",
        GreaterThan,
      ),
      (
        "Greater than or equal",
        "Keep rows where this column is at least a value",
        GreaterThanOrEqual,
      ),
      (
        "Less than",
        "Keep rows where this column is less than a value",
        LessThan,
      ),
      (
        "Less than or equal",
        "Keep rows where this column is at most a value",
        LessThanOrEqual,
      ),
    ];
    let column_cls = Option.bind(column_type, atom_cls_of_typ);
    let numeric_items =
      switch (column_cls) {
      | None => []
      | Some(cls) =>
        numeric_comparators
        |> List.filter_map(((text, tooltip, op_num)) =>
             Operators.numeric_bin_op(cls, op_num)
             |> Option.map(op =>
                  leaf(~tooltip, text, () =>
                    apply([filter_by_column(op, h)])
                  )
                )
           )
      };
    let poly_items = [
      leaf(~tooltip="Keep rows where this column equals a value", "Equals", () =>
        apply([filter_by_column(Poly(Equals), h)])
      ),
      leaf(
        ~tooltip="Keep rows where this column doesn't equal a value",
        "Not equal",
        () =>
        apply([filter_by_column(Poly(NotEquals), h)])
      ),
    ];
    let string_items =
      switch (column_cls) {
      | Some(String) => [
          leaf(
            ~tooltip="Keep rows where this column matches a regex pattern",
            "Matches regex",
            () =>
            apply([string_match_filter(h)])
          ),
        ]
      | _ => []
      };
    let custom_item =
      leaf(~tooltip="Write your own predicate over the row", "Custom…", () =>
        apply([custom_filter()])
      );
    [Menu.back_item()]
    @ numeric_items
    @ poly_items
    @ string_items
    @ [custom_item];
  | ["Transform"] =>
    // Merged Transform submenu: conversion options + Clear + Identity
    let conversion_items =
      switch (Option.bind(column_type, atom_cls_of_typ)) {
      | Some(cls) =>
        Atom.conversions_from(cls)
        |> List.map(((func, to_)) => {
             let display = Atom.show_cls(to_);
             leaf(~tooltip="Convert column values to " ++ display, display, () =>
               apply([convert_column(h, func)])
             );
           })
      | None => []
      };

    [Menu.back_item()]
    @ conversion_items
    @ (List.length(conversion_items) > 0 ? [Menu.divider] : [])
    @ [
      leaf(~tooltip="Replace all values with holes", "Clear", () =>
        apply([clear_column(h)])
      ),
      leaf(
        ~tooltip=
          "Reassigns each value to itself; useful as a starting point for custom edits",
        "Identity",
        () =>
        apply([noop_column(h)])
      ),
    ];
  | ["Sort"] => [
      Menu.back_item(),
      leaf(~tooltip="Sort from lowest to highest", "Ascending", () =>
        switch (sort_column(column_type, h, false)) {
        | Some(ts) => apply(ts)
        | None => local(CloseMenu)
        }
      ),
      leaf(~tooltip="Sort from highest to lowest", "Descending", () =>
        switch (sort_column(column_type, h, true)) {
        | Some(ts) => apply(ts)
        | None => local(CloseMenu)
        }
      ),
    ]
  | ["Move"] =>
    [Menu.back_item()]
    @ (
      can_move_left
        ? [
          leaf(
            ~tooltip="Move this column one position to the left",
            "Move Left",
            () =>
            switch (move_column(dyn_type, h, true)) {
            | Some(t) => apply([t])
            | None => local(CloseMenu)
            }
          ),
        ]
        : []
    )
    @ (
      can_move_right
        ? [
          leaf(
            ~tooltip="Move this column one position to the right",
            "Move Right",
            () =>
            switch (move_column(dyn_type, h, false)) {
            | Some(t) => apply([t])
            | None => local(CloseMenu)
            }
          ),
        ]
        : []
    )
  | [] =>
    /* Group 1: Structural, frequently used actions */
    let structural_items = [
      leaf(~tooltip="Remove this column from every row", "Drop Column", () =>
        apply([drop_column(h)])
      ),
      leaf(~tooltip="Change this column's label", "Rename", () => {
        let new_column_name = JsUtil.prompt("New column name:", h);
        switch (new_column_name) {
        | None => local(CloseMenu)
        | Some(new_name) => apply([rename_column(h, new_name)])
        };
      }),
      leaf(~tooltip="Group rows by the values in this column", "Group By", () =>
        apply([group_by_column(h)])
      ),
    ];

    /* Group 2: Data operation submenus */
    let sort_submenu =
      switch (sort_column(column_type, h, false)) {
      | Some(_) => [
          submenu_row(~tooltip="Sort rows by this column", "Sort", ["Sort"]),
        ]
      | None => []
      };

    let filter_submenu =
      switch (Option.bind(column_type, atom_cls_of_typ)) {
      | Some(_) => [
          submenu_row(
            ~tooltip="Keep rows matching a condition on this column",
            "Filter",
            ["Filter"],
          ),
        ]
      | None => []
      };

    let transform_submenu = [
      submenu_row(
        ~tooltip="Modify the values in this column",
        "Transform",
        ["Transform"],
      ),
    ];

    let move_submenu =
      can_move_left || can_move_right
        ? [
          submenu_row(
            ~tooltip="Reorder this column's position",
            "Move",
            ["Move"],
          ),
        ]
        : [];

    let data_items =
      sort_submenu @ filter_submenu @ transform_submenu @ move_submenu;

    /* Group 3: Option-type actions */
    let option_items =
      switch (column_type) {
      | Some(ty) =>
        is_option_type(ty)
          ? [
            leaf(
              ~tooltip="Remove rows where this column is None",
              "Drop Nones",
              () =>
              apply([drop_nones_column(h)])
            ),
            leaf(
              ~tooltip="Replace None values with a default you specify",
              "Provide Default",
              () =>
              apply([provide_default_column(h)])
            ),
          ]
          : []
      | None => []
      };

    structural_items
    @ (List.length(data_items) > 0 ? [Menu.divider] @ data_items : [])
    @ (List.length(option_items) > 0 ? [Menu.divider] @ option_items : []);
  | _ => []
  };
};

/* Lookup the menu data for the currently-open column at the given path. */
let menu_at =
    (
      info: info,
      exp: Exp.t,
      headers: list(option(string)),
      local: action => Ui_effect.t(unit),
      parent: external_action => Ui_effect.t(unit),
      col: int,
      path: list(string),
    )
    : menu_data =>
  switch (List.nth_opt(headers, col) |> Option.value(~default=None)) {
  | None => []
  | Some(name) =>
    let dyn_type =
      switch (get_type_from_info(info)) {
      | Some(_) as ty => ty
      | None => get_dynamic_type(exp)
      };
    build_column_menu(info, name, dyn_type, local, parent, path);
  };

/* Main table rendering function */
let render =
    (
      ~info: info,
      ~exp: Exp.t,
      ~value: value,
      ~view_seg: (Sort.t, Segment.t) => Node.t,
      ~model: model, /* (column_index, menu_path) */
      ~local: action => Ui_effect.t(unit),
      ~parent: external_action => Ui_effect.t(unit),
      ~sort: Sort.t,
      _: unit,
    )
    : Node.t => {
  let is_readonly = sort != Sort.Exp;
  let (headers, rows) = value;
  let make_menu_button = i =>
    icon_button(~tooltip="Column options", "⋮", _ => local(ShowMenu(i)));

  let header_cells =
    List.mapi(
      (i, h) => {
        let (label_node, has_name) =
          switch (h) {
          | Some(name) => (Node.text(name), true)
          | None => (WebUtil.empty_hole_svg(), false)
          };
        let menu_button = make_menu_button(i);
        let content = [
          label_node,
          is_readonly || !has_name ? Node.none : menu_button,
        ];

        let full_content =
          switch (h, model.menu_state) {
          | (Some(_), Some((j, menu_path, sel_idx))) when i == j =>
            let menu_data =
              menu_at(info, exp, headers, local, parent, j, menu_path);
            let clamped = Menu.clamp_against(menu_data, sel_idx);
            let menu_nodes =
              Menu.render(
                ~inject_action=thunk => thunk(),
                ~inject_menu=
                  fun
                  | Close => local(CloseMenu)
                  | EnterSubmenu(p)
                  | BackSubmenu(p) => local(ShowSubmenu(p))
                  | SetSelected(idx) => local(MenuSelect(idx))
                  | Toggle
                  | Open
                  | Up
                  | Down => Effect.Ignore,
                ~item_class="named-menu-item",
                ~selected_idx=clamped,
                menu_data,
              );
            content
            @ [
              Node.div(
                ~attrs=[
                  Attr.id("column-menu-" ++ string_of_int(i)),
                  Attr.classes([
                    "context-menu",
                    "nut-menu",
                    "open-down-right",
                    "column-menu",
                  ]),
                ],
                [
                  WebUtil.div_c(
                    "group",
                    [WebUtil.div_c("contents", menu_nodes)],
                  ),
                ],
              ),
            ];
          | _ => content
          };
        let is_menu_open =
          switch (model.menu_state) {
          | Some((j, _, _)) => i == j
          | None => false
          };
        Node.th(
          ~attrs=is_menu_open ? [Attr.classes(["menu-open"])] : [],
          full_content,
        );
      },
      headers,
    );

  let header_cells =
    if (!is_readonly) {
      header_cells
      @ [
        Node.th(
          ~attrs=[
            Attr.classes(["add-column-header"]),
            Attr.on_click(_ =>
              switch (to_segment(info, [add_column()])) {
              | Some(seg) => parent(SetSyntax(seg))
              | None => Effect.Ignore
              }
            ),
            Attr.create("title", "Add column"),
          ],
          [Node.text("+")],
        ),
      ];
    } else {
      header_cells;
    };

  /* Sync the document-level click-outside + keyboard listeners. We use a
   * global listener (rather than tabindex+on_keydown on the menu div)
   * because the editor's #page on_focus reclaims focus to the clipboard
   * shim, which would otherwise eat the menu's key events.
   *
   * ArrowUp/Down wrap modulo the selectable count — the shared Menu model
   * clamps instead of wrapping, so we intercept Up/Down here. */
  let handle_key = (key: string): option(Ui_effect.t(unit)) =>
    switch (model.menu_state) {
    | None => None
    | Some((col, path, sel_idx)) =>
      let items = menu_at(info, exp, headers, local, parent, col, path);
      let menu_t: Menu.t =
        Some({
          selected_idx: sel_idx,
          path,
        });
      switch (Menu.handle_key(~items, Key.D(key), menu_t)) {
      | RunAction(thunk) => Some(thunk())
      | MenuUpdate(Close) => Some(local(CloseMenu))
      | MenuUpdate(Up) =>
        let n = Menu.count_selectable(items);
        n == 0 ? None : Some(local(MenuSelect((sel_idx - 1 + n) mod n)));
      | MenuUpdate(Down) =>
        let n = Menu.count_selectable(items);
        n == 0 ? None : Some(local(MenuSelect((sel_idx + 1) mod n)));
      | MenuUpdate(EnterSubmenu(p))
      | MenuUpdate(BackSubmenu(p)) => Some(local(ShowSubmenu(p)))
      | MenuUpdate(SetSelected(i)) => Some(local(MenuSelect(i)))
      | MenuUpdate(Toggle | Open)
      | Unhandled => None
      };
    };
  ColumnMenuListener.sync(
    ~menu_open=model.menu_state != None,
    ~on_close=local(CloseMenu),
    ~handle_key,
    (),
  );
  table_view(
    ~header_cells,
    ~rows=
      List.map(
        row => {
          let cells = row_cells(info.utility, view_seg, row);
          is_readonly ? cells : cells @ [Node.td([])];
        },
        rows,
      ),
  );
};

let menu_col = (st: menu_state): option(int) =>
  Option.map(((c, _, _)) => c, st);

let update: (model, action) => model =
  (model, action) => {
    switch (action) {
    | CloseMenu => {menu_state: None}
    | ShowMenu(i) when Some(i) == menu_col(model.menu_state) => {
        menu_state: None,
      }
    | ShowMenu(i) => {menu_state: Some((i, [], 0))}
    | ShowSubmenu(path) =>
      switch (model.menu_state) {
      | Some((col, _, _)) => {menu_state: Some((col, path, 0))}
      | None => model
      }
    | MenuSelect(idx) =>
      switch (model.menu_state) {
      | Some((col, path, _)) => {menu_state: Some((col, path, idx))}
      | None => model
      }
    };
  };

let icon_size = 20.;

let simple_icon = (~transform="", ~view: string, ds: list(string)) =>
  /* takes a list of paths as strings, a viewport as a string,
     and an optional (string) transform to apply to each */
  Node.create_svg(
    "svg",
    ~attrs=
      Attr.[
        create("viewBox", view),
        create("width", Printf.sprintf("%fpx", icon_size)),
        create("height", Printf.sprintf("%fpx", icon_size)),
        create("preserveAspectRatio", "none"),
      ],
    List.map(
      d =>
        Node.create_svg(
          "path",
          ~attrs=
            [Attr.create("d", d)]
            @ (transform == "" ? [] : [Attr.create("transform", transform)]),
          [],
        ),
      ds,
    ),
  );

let table_icon =
  simple_icon(
    ~view="0 0 8 8",
    [
      "m 1.32307 3.96929 a 0.2645835 0.2645835 0 0 0 -0.26563 0.26367 0.2645835 0.2645835 0 0 0 0.26563 0.26562 h 5.82031 a 0.2645835 0.2645835 0 0 0 0.26562 -0.26562 0.2645835 0.2645835 0 0 0 -0.26562 -0.26367 z",
      "m 1.85236 1.05913 c -0.43516 0 -0.79492 0.35781 -0.79492 0.79297 v 4.76172 c 0 0.43517 0.35976 0.79492 0.79492 0.79492 h 4.76172 c 0.43517 0 0.79492 -0.35975 0.79492 -0.79492 v -4.76172 c 0 -0.43516 -0.35975 -0.79297 -0.79492 -0.79297 z m 0 0.5293 h 4.76172 c 0.15117 0 0.26563 0.11251 0.26563 0.26367 v 4.76172 c 0 0.15117 -0.11446 0.26562 -0.26563 0.26562 h -4.76172 c -0.15115 0 -0.26562 -0.11445 -0.26562 -0.26562 v -4.76172 c 0 -0.15116 0.11447 -0.26367 0.26562 -0.26367 z",
      "m 1.32307 5.42437 a 0.2645835 0.2645835 0 0 0 -0.26563 0.26367 0.2645835 0.2645835 0 0 0 0.26563 0.26562 h 5.82031 a 0.2645835 0.2645835 0 0 0 0.26562 -0.26562 0.2645835 0.2645835 0 0 0 -0.26562 -0.26367 z",
      "m 5.02619 2.91069 v 4.23243 h 0.5293 v -4.23243 z",
      "m 2.91096 2.91069 v 4.23243 h 0.52929 v -4.23243 z",
      "m 1.32307 2.51421 a 0.2645835 0.2645835 0 0 0 -0.26563 0.26367 0.2645835 0.2645835 0 0 0 0.26563 0.26563 h 5.82031 a 0.2645835 0.2645835 0 0 0 0.26562 -0.26563 0.2645835 0.2645835 0 0 0 -0.26562 -0.26367 z",
    ],
  );
let badge =
  Node.span(
    ~attrs=[
      Attr.classes(["table-badge"]),
      Attr.title("Click to view as table"),
    ],
    [table_icon],
  );
