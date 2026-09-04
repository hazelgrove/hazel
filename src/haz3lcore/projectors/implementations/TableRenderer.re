open Util_web;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;
open TableCore;
open TableTransforms;

/* TableRenderer - A reusable module for rendering interactive tables with column operations */

[@deriving (show({with_path: false}), sexp, yojson)]
type v = (list(option(string)), list(list(Exp.t))); /* (headers, rows) */

/* Open column menu: column index + Menu state (path + selected_idx).
 * The Menu module owns the inner state — projector code never reads it. */
[@deriving (show({with_path: false}), sexp, yojson)]
type menu_state = option((int, Menu.t));
[@deriving (show({with_path: false}), sexp, yojson)]
type m = {menu_state};
[@deriving (show({with_path: false}), sexp, yojson)]
type a =
  | CloseMenu
  | ShowMenu(int)
  | MenuAction(Menu.action);

[@deriving (show({with_path: false}), sexp, yojson)]
type model = m;

/* Table actions that can be performed on columns */
[@deriving (show({with_path: false}), sexp, yojson)]
type action = a;

[@deriving (show({with_path: false}), sexp, yojson)]
type value = v;

/* Column menu items use the shared `Util_web.Menu` framework. */
type menu_data = list(Menu.item(unit => Ui_effect.t(unit)));

/* Parse an expression into table structure */
let parse = (_sort: Sort.t, exp: Exp.t) => parse_table(exp);

/* Initialize table model from parsed value */
let empty = {menu_state: None};
let init = (_: v) => empty;

/* Local builders that wrap Menu.item constructors with the column menu's
 * conventions: hover updates selection, tooltips on every leaf row. */
let leaf = (~tooltip, label, action) =>
  Menu.action_item(~tooltip, ~on_hover=true, label, action);

let submenu = (~tooltip, label, children) =>
  Menu.submenu_item(~tooltip, label, children);

/* Build the column menu as a single tree. Submenus carry their children
 * inline; `Util_web.Menu` walks the path and synthesises the Back row. */
let build_column_menu =
    (
      info: info,
      h: string,
      dyn_type: option(Typ.t),
      local: action => Ui_effect.t(unit),
      parent: external_action => Ui_effect.t(unit),
    )
    : menu_data => {
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

  let column_cls = Option.bind(column_type, atom_cls_of_typ);

  let filter_children = {
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
    numeric_items @ poly_items @ string_items @ [custom_item];
  };

  let transform_children = {
    let conversion_items =
      switch (column_cls) {
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
    conversion_items
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
  };

  let sort_children = [
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
  ];

  let move_children =
    (
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
    );

  /* Root: structural actions, then submenus, then option-type actions. */
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

  let sort_submenu =
    switch (sort_column(column_type, h, false)) {
    | Some(_) => [
        submenu(~tooltip="Sort rows by this column", "Sort", sort_children),
      ]
    | None => []
    };

  let filter_submenu =
    switch (column_cls) {
    | Some(_) => [
        submenu(
          ~tooltip="Keep rows matching a condition on this column",
          "Filter",
          filter_children,
        ),
      ]
    | None => []
    };

  let transform_submenu = [
    submenu(
      ~tooltip="Modify the values in this column",
      "Transform",
      transform_children,
    ),
  ];

  let move_submenu =
    can_move_left || can_move_right
      ? [
        submenu(
          ~tooltip="Reorder this column's position",
          "Move",
          move_children,
        ),
      ]
      : [];

  let data_items =
    sort_submenu @ filter_submenu @ transform_submenu @ move_submenu;

  let option_items =
    switch (column_type) {
    | Some(ty) =>
      is_option_type(ty)
        ? [
          leaf(
            ~tooltip="Remove rows where this column is None", "Drop Nones", () =>
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
};

/* Build the column menu tree for a specific column. */
let items_for_column =
    (
      info: info,
      exp: Exp.t,
      headers: list(option(string)),
      local: action => Ui_effect.t(unit),
      parent: external_action => Ui_effect.t(unit),
      col: int,
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
    build_column_menu(info, name, dyn_type, local, parent);
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
  let menu_button_id = i => "column-menu-button-" ++ string_of_int(i);
  let make_menu_button = i =>
    Node.div(
      ~attrs=[
        Attr.id(menu_button_id(i)),
        /* `menu-trigger` exempts the button from MenuListener's
         * click-outside detection so opening + closing both flow
         * through ShowMenu, letting it toggle the same column. */
        Attr.classes(["icon", "closure-nav-button", "menu-trigger"]),
        Attr.on_click(_ => local(ShowMenu(i))),
        Attr.title("Column options"),
      ],
      [Node.text("⋮")],
    );

  let header_cells =
    List.mapi(
      (i, h) => {
        let (label_node, has_name) =
          switch (h) {
          | Some(name) => (
              Node.span(
                ~attrs=[Attr.classes(["column-label"])],
                [Node.text(name)],
              ),
              true,
            )
          | None => (WebUtil.empty_hole_svg(), false)
          };
        let menu_button = make_menu_button(i);
        let content = [
          label_node,
          is_readonly || !has_name ? Node.none : menu_button,
        ];

        let full_content =
          switch (h, model.menu_state) {
          | (Some(_), Some((j, menu_t))) when i == j =>
            let items =
              items_for_column(info, exp, headers, local, parent, j);
            let menu_nodes =
              Menu.render(
                ~inject_action=thunk => thunk(),
                ~inject_menu=a => local(MenuAction(a)),
                ~item_class="named-menu-item",
                ~items,
                menu_t,
              );
            let dir =
              Menu.direction_from_id(
                ~menu_height=200.0,
                ~menu_width=180.0,
                menu_button_id(i),
              );
            let dir_class =
              switch (dir) {
              | {vertical: `Down, horizontal: `Right} => "cm-down-right"
              | {vertical: `Down, horizontal: `Left} => "cm-down-left"
              | {vertical: `Up, horizontal: `Right} => "cm-up-right"
              | {vertical: `Up, horizontal: `Left} => "cm-up-left"
              };
            content
            @ [
              Node.div(
                ~attrs=[
                  Attr.id("column-menu-" ++ string_of_int(i)),
                  Attr.classes([
                    "context-menu",
                    "nut-menu",
                    "column-menu",
                    dir_class,
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
          | Some((j, _)) => i == j
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

  /* Sync the document-level click-outside + keyboard listeners. */
  let handle_key = (key: string): option(Ui_effect.t(unit)) =>
    switch (model.menu_state) {
    | None => None
    | Some((col, menu_t)) =>
      let items = items_for_column(info, exp, headers, local, parent, col);
      Menu.key_dispatcher(
        ~items,
        ~dispatch_menu=a => local(MenuAction(a)),
        ~dispatch_action=thunk => thunk(),
        menu_t,
        key,
      );
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
  Option.map(((c, _)) => c, st);

let update: (model, action) => model =
  (model, action) => {
    switch (action) {
    | CloseMenu => {menu_state: None}
    | ShowMenu(i) when Some(i) == menu_col(model.menu_state) => {
        menu_state: None,
      }
    | ShowMenu(i) => {menu_state: Some((i, Menu.opened))}
    | MenuAction(a) =>
      switch (model.menu_state) {
      | Some((col, menu_t)) =>
        let new_menu = Menu.update(a, menu_t);
        switch (new_menu) {
        | None => {menu_state: None}
        | Some(_) => {menu_state: Some((col, new_menu))}
        };
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
