open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;
open TableCore;
open TableTransforms;

/* TableRenderer - A reusable module for rendering interactive tables with column operations */

[@deriving (show({with_path: false}), sexp, yojson)]
type v = (list(option(string)), list(list(Exp.t))); /* (headers, rows) */

[@deriving (show({with_path: false}), sexp, yojson)]
type menu_state = option((int, list(string)));
[@deriving (show({with_path: false}), sexp, yojson)]
type m = {menu_state};
[@deriving (show({with_path: false}), sexp, yojson)]
type a =
  | CloseMenu
  | ShowMenu(int)
  | ShowSubmenu(list(string))
  | DropColumn(string)
  | ConversionColumn(string, string)
  | RenameColumn(string, string)
  | AddColumn(string, string)
  | GroupByColumn(string)
  | FilterGreaterThan(string)
  | FilterLessThan(string)
  | FilterEquals(string);

[@deriving (show({with_path: false}), sexp, yojson)]
type model = m;

/* Table actions that can be performed on columns */
[@deriving (show({with_path: false}), sexp, yojson)]
type action = a;

[@deriving (show({with_path: false}), sexp, yojson)]
type value = v;

/* Menu item types for the column menu system */
[@deriving (show({with_path: false}), sexp, yojson)]
type menu_item =
  | Action({
      text: string,
      tooltip: string,
      action: unit => Ui_effect.t(unit),
    })
  | Submenu({
      text: string,
      subitems: list(menu_item),
    })
  | Separator;

[@deriving (show({with_path: false}), sexp, yojson)]
type menu_data = list(menu_item);

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

/* Menu system */
let menu_item = (~tooltip="", text, action) =>
  Node.div(
    ~attrs=[
      Attr.classes(["menu-item"]),
      Attr.on_click(action),
      Attr.title(tooltip),
    ],
    [Node.text(text)],
  );

let menu_divider = Node.div(~attrs=[Attr.classes(["menu-divider"])], []);

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
    Effect.Many([
      local(CloseMenu),
      parent(SetSyntax(to_segment(info, ts))),
    ]);

  // If we're in a submenu, show that submenu
  switch (menu_path) {
  | ["Filter"] =>
    [
      Action({
        text: "← Back",
        tooltip: "",
        action: () => local(ShowSubmenu([])),
      }),
    ]
    @ {
      let gt_op: option(Operators.op_bin) =
        switch (Option.map(Typ.term_of, column_type)) {
        | Some(Atom(Int)) => Some(Int(GreaterThan))
        | Some(Atom(Float)) => Some(Float(GreaterThan))
        | _ => None
        };
      let lt_op: option(Operators.op_bin) =
        switch (Option.map(Typ.term_of, column_type)) {
        | Some(Atom(Int)) => Some(Int(LessThan))
        | Some(Atom(Float)) => Some(Float(LessThan))
        | _ => None
        };
      (
        switch (gt_op) {
        | Some(op) => [
            Action({
              text: "Greater than",
              tooltip: "Keep rows where this column is greater than a value",
              action: () => apply([filter_by_column(op, h)]),
            }),
          ]
        | None => []
        }
      )
      @ (
        switch (lt_op) {
        | Some(op) => [
            Action({
              text: "Less than",
              tooltip: "Keep rows where this column is less than a value",
              action: () => apply([filter_by_column(op, h)]),
            }),
          ]
        | None => []
        }
      )
      @ [
        Action({
          text: "Equals",
          tooltip: "Keep rows where this column equals a value",
          action: () => apply([filter_by_column(Poly(Equals), h)]),
        }),
      ];
    }
  | ["Transform"] =>
    // Merged Transform submenu: conversion options + Clear + Identity
    let conversion_items =
      switch (column_type) {
      | Some(ty) =>
        switch (Typ.cls_of_term(ty.term)) {
        | Typ.Atom(atom) =>
          List.map(
            ((display, func)) =>
              Action({
                text: display,
                tooltip: "Convert column values to " ++ display,
                action: () => apply([convert_column(h, func)]),
              }),
            conversion_functions(atom),
          )
        | _ => []
        }
      | None => []
      };

    [
      Action({
        text: "← Back",
        tooltip: "",
        action: () => local(ShowSubmenu([])),
      }),
    ]
    @ conversion_items
    @ (List.length(conversion_items) > 0 ? [Separator] : [])
    @ [
      Action({
        text: "Clear",
        tooltip: "Replace all values with holes",
        action: () => apply([clear_column(h)]),
      }),
      Action({
        text: "Identity",
        tooltip: "Reassigns each value to itself; useful as a starting point for custom edits",
        action: () => apply([noop_column(h)]),
      }),
    ];
  | ["Sort"] => [
      Action({
        text: "← Back",
        tooltip: "",
        action: () => local(ShowSubmenu([])),
      }),
      Action({
        text: "Ascending",
        tooltip: "Sort from lowest to highest",
        action: () =>
          switch (sort_column(column_type, h, false)) {
          | Some(ts) => apply(ts)
          | None => local(CloseMenu)
          },
      }),
      Action({
        text: "Descending",
        tooltip: "Sort from highest to lowest",
        action: () =>
          switch (sort_column(column_type, h, true)) {
          | Some(ts) => apply(ts)
          | None => local(CloseMenu)
          },
      }),
    ]
  | ["Move"] =>
    [
      Action({
        text: "← Back",
        tooltip: "",
        action: () => local(ShowSubmenu([])),
      }),
    ]
    @ (
      can_move_left
        ? [
          Action({
            text: "Move Left",
            tooltip: "Move this column one position to the left",
            action: () =>
              apply([
                OptUtil.get_or_fail(
                  "move left failed",
                  move_column(dyn_type, h, true),
                ),
              ]),
          }),
        ]
        : []
    )
    @ (
      can_move_right
        ? [
          Action({
            text: "Move Right",
            tooltip: "Move this column one position to the right",
            action: () =>
              apply([
                OptUtil.get_or_fail(
                  "move right failed",
                  move_column(dyn_type, h, false),
                ),
              ]),
          }),
        ]
        : []
    )
  | [] =>
    /* Group 1: Structural, frequently used actions */
    let structural_items = [
      Action({
        text: "Drop Column",
        tooltip: "Remove this column from every row",
        action: () => apply([drop_column(h)]),
      }),
      Action({
        text: "Rename",
        tooltip: "Change this column's label",
        action: () => {
          let new_column_name = JsUtil.prompt("New column name:", h);
          switch (new_column_name) {
          | None => local(CloseMenu)
          | Some(new_name) => apply([rename_column(h, new_name)])
          };
        },
      }),
      Action({
        text: "Group By",
        tooltip: "Group rows by the values in this column",
        action: () => apply([group_by_column(h)]),
      }),
    ];

    /* Group 2: Data operation submenus */
    let sort_submenu =
      switch (sort_column(column_type, h, false)) {
      | Some(_) => [
          Action({
            text: "Sort →",
            tooltip: "Sort rows by this column",
            action: () => local(ShowSubmenu(["Sort"])),
          }),
        ]
      | None => []
      };

    let filter_submenu =
      switch (column_type) {
      | Some(ty) =>
        switch (Typ.cls_of_term(ty.term)) {
        | Typ.Atom(Atom.Int | Atom.Float) => [
            Action({
              text: "Filter →",
              tooltip: "Keep rows matching a condition on this column",
              action: () => local(ShowSubmenu(["Filter"])),
            }),
          ]
        | _ => []
        }
      | None => []
      };

    let transform_submenu = [
      Action({
        text: "Transform →",
        tooltip: "Modify the values in this column",
        action: () => local(ShowSubmenu(["Transform"])),
      }),
    ];

    let move_submenu =
      can_move_left || can_move_right
        ? [
          Action({
            text: "Move →",
            tooltip: "Reorder this column's position",
            action: () => local(ShowSubmenu(["Move"])),
          }),
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
            Action({
              text: "Drop Nones",
              tooltip: "Remove rows where this column is None",
              action: () => apply([drop_nones_column(h)]),
            }),
            Action({
              text: "Provide Default",
              tooltip: "Replace None values with a default you specify",
              action: () => apply([provide_default_column(h)]),
            }),
          ]
          : []
      | None => []
      };

    structural_items
    @ (List.length(data_items) > 0 ? [Separator] @ data_items : [])
    @ (List.length(option_items) > 0 ? [Separator] @ option_items : []);
  | _ => []
  };
};

let render_menu = menu_data => {
  List.map(
    item =>
      switch (item) {
      | Action({text, tooltip, action}) =>
        menu_item(~tooltip, text, _ => action())
      | Submenu({text, subitems: _}) => menu_item(text, _ => Effect.Ignore)
      | Separator => menu_divider
      },
    menu_data,
  );
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
          | (Some(name), Some((j, menu_path))) when i == j =>
            let dyn_type = get_dynamic_type(exp); /* Is there a better way to get the types of the columns? */
            let menu_data =
              build_column_menu(
                info,
                name,
                dyn_type,
                local,
                parent,
                menu_path,
              );
            let menu_content = render_menu(menu_data);
            content
            @ [
              Node.div(
                ~attrs=[Attr.classes(["column-menu"])],
                menu_content,
              ),
            ];
          | _ => content
          };
        Node.th(full_content);
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
            Attr.on_click(_ => {
              let new_column_name = JsUtil.prompt("New column name:", "");
              switch (new_column_name) {
              | None => Effect.Ignore
              | Some(new_name) =>
                parent(
                  SetSyntax(to_segment(info, [add_column(new_name)])),
                )
              };
            }),
            Attr.create("title", "Add column"),
          ],
          [Node.text("+")],
        ),
      ];
    } else {
      header_cells;
    };

  Node.table(
    ~attrs=[Attr.classes(["table"])],
    [
      Node.thead([Node.tr(header_cells)]),
      Node.tbody(
        List.map(
          row => {
            let cells =
              List.map(
                e => Node.td([value_view(info.utility, view_seg, e)]),
                row,
              );
            let cells =
              if (!is_readonly) {
                cells @ [Node.td([])];
              } else {
                cells;
              };
            Node.tr(cells);
          },
          rows,
        ),
      ),
    ],
  );
};

let update: (model, action) => model =
  (model, action) => {
    switch (action) {
    | CloseMenu => {menu_state: None}
    | ShowMenu(i) when Some(i) == Option.map(fst, model.menu_state) => {
        menu_state: None,
      }
    | ShowMenu(i) => {menu_state: Some((i, []))}
    | ShowSubmenu(path) =>
      switch (model.menu_state) {
      | Some((col, _)) => {menu_state: Some((col, path))}
      | None => model
      }
    | _ => model /* Other actions do not affect the menu state */
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
