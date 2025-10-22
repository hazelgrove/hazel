open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

/* Import the reusable table renderer */
module TR = TableRenderer;

let icon_button = (~tooltip="", icon_text, action) =>
  Node.div(
    ~attrs=[
      Attr.classes(["icon", "closure-nav-button"]),
      Attr.on_click(action),
      Attr.title(tooltip),
    ],
    [Node.text(icon_text)],
  );

let table_from_exp = TR.table_from_exp;
let table_of = TR.table_of;
let get_dynamic_type = TR.get_dynamic_type;

/* Import other needed functions from TableRenderer */
let get_column_type_from_ty = TR.get_column_type_from_ty;
let get_columns = TR.get_columns;
let can_move_column = TR.can_move_column;
let convert_column = TR.convert_column;
let drop_column = TR.drop_column;
let rename_column = TR.rename_column;
let add_column_after = TR.add_column_after;
let move_column = TR.move_column;
let value_view = TR.value_view;

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type menu_item =
    | Action({
        text: string,
        action: unit => Ui_effect.t(unit),
      })
    | Submenu({
        text: string,
        subitems: list(menu_item),
      });

  [@deriving (show({with_path: false}), sexp, yojson)]
  type menu_data = list(menu_item);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type menu_state = list(string); // Path of opened submenus

  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = {
    closure: option(int),
    menu: option((int, menu_state)),
  };
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | Next
    | Previous
    | DropColumn(string)
    | ShowMenu(int)
    | ShowSubmenu(list(string))
    | CloseMenu
    | ConversionColumn(string, string)
    | RenameColumn(string, string)
    | AddColumnAfter(string, string);

  let menu_item = (text, action) =>
    Node.div(
      ~attrs=[Attr.classes(["menu-item"]), Attr.on_click(action)],
      [Node.text(text)],
    );

  let conversion_functions = (cls: Atom.cls) =>
    switch (cls) {
    | Atom.String => [
        ("int", "int_of_string"),
        ("float", "float_of_string"),
        ("bool", "bool_of_string"),
      ]
    | Atom.Int => [
        ("string", "string_of_int"),
        ("float", "float_of_int"),
        ("bool", "bool_of_int"),
      ]
    | Atom.Float => [
        ("string", "string_of_float"),
        ("int", "int_of_float"),
        ("bool", "bool_of_float"),
      ]
    | Atom.Bool => [
        ("string", "string_of_bool"),
        ("int", "int_of_bool"),
        ("float", "float_of_bool"),
      ]
    | _ => []
    };

  let sort_column_with_direction =
      (
        info: info,
        column_type: option(Typ.t),
        header: string,
        descending: bool,
      )
      : option(Base.segment) => {
    let compare_fn =
      switch (column_type) {
      | Some(ty) =>
        switch (Typ.term_of(ty)) {
        | Atom(Atom.Int) => Some("int_compare")
        | Atom(Atom.Float) => Some("float_compare")
        | Atom(Atom.String) => Some("string_compare")
        | _ => None
        }
      | None => None
      };

    switch (compare_fn) {
    | Some(compare_fn_name) =>
      IdTagged.FreshGrammar.(
        switch (
          info.utility.lift_syntax(
            fun
            | Exp({term: exp_term, _}) => {
                let sort_expr =
                  Exp.(
                    ap(
                      Reverse,
                      ap(
                        Forward,
                        var("sort"),
                        tuple([
                          fn(
                            Pat.tuple([Pat.var("r1"), Pat.var("r2")]),
                            ap(
                              Forward,
                              var(compare_fn_name),
                              tuple([
                                dot(var("r1"), label(header)),
                                dot(var("r2"), label(header)),
                              ]),
                            ),
                            None,
                            None,
                          ),
                          deferral(InAp),
                        ]),
                      ),
                      exp_term |> DHExp.fresh,
                    )
                  );

                let final_expr =
                  if (descending) {
                    Exp.(ap(Reverse, var("reverse"), sort_expr));
                  } else {
                    sort_expr;
                  };

                Exp(final_expr);
              }
            | _ => failwith("TableProj: sort_column: not an expression"),
            info.syntax,
          )
        ) {
        | Some(segment) => Some(segment)
        | None => None
        }
      )
    | None => None
    };
  };

  let build_column_menu = (info, h, dyn_type, local, parent, menu_path) => {
    let column_type =
      dyn_type |> Option.bind(_, ty => get_column_type_from_ty(ty, h));
    let columns_opt = dyn_type |> Option.bind(_, get_columns);
    let can_move_left = can_move_column(columns_opt, h, true);
    let can_move_right = can_move_column(columns_opt, h, false);

    // If we're in a submenu, show that submenu
    switch (menu_path) {
    | ["Convert"] =>
      // Show conversion submenu
      switch (column_type) {
      | Some(ty) =>
        switch (Typ.term_of(ty)) {
        | Atom(cls) =>
          let conversions = conversion_functions(cls);
          [
            Action({
              text: "← Back",
              action: () => local(ShowSubmenu([])) // Go back to main menu
            }),
          ]
          @ List.map(
              ((display, func)) =>
                Action({
                  text: display,
                  action: () =>
                    Effect.Many([
                      local(CloseMenu),
                      parent(SetSyntax(convert_column(info, h, func))),
                    ]),
                }),
              conversions,
            );
        | _ => []
        }
      | None => []
      }
    | ["Sort"] =>
      // Show sort submenu
      [
        Action({
          text: "← Back",
          action: () => local(ShowSubmenu([])) // Go back to main menu
        }),
        Action({
          text: "Ascending",
          action: () =>
            switch (sort_column_with_direction(info, column_type, h, false)) {
            | Some(segment) =>
              Effect.Many([local(CloseMenu), parent(SetSyntax(segment))])
            | None => local(CloseMenu)
            },
        }),
        Action({
          text: "Descending",
          action: () =>
            switch (sort_column_with_direction(info, column_type, h, true)) {
            | Some(segment) =>
              Effect.Many([local(CloseMenu), parent(SetSyntax(segment))])
            | None => local(CloseMenu)
            },
        }),
      ]
    | [] =>
      // Show main menu
      let base_items = [
        Action({
          text: "Drop Column",
          action: () =>
            Effect.Many([
              local(CloseMenu),
              parent(SetSyntax(drop_column(info, h))),
            ]),
        }),
        Action({
          text: "Rename",
          action: () => {
            let new_column_name = JsUtil.prompt("New column name:", h);
            switch (new_column_name) {
            | None => local(CloseMenu) // User cancelled
            | Some(new_name) =>
              Effect.Many([
                local(CloseMenu),
                parent(SetSyntax(rename_column(info, h, new_name))),
              ])
            };
          },
        }),
        Action({
          text: "Add Column After",
          action: () =>
            Effect.Many([
              local(CloseMenu),
              parent(SetSyntax(add_column_after(info, h, "new_column"))),
            ]),
        }),
      ];

      let conversion_submenu =
        switch (column_type) {
        | Some(ty) =>
          switch (Typ.cls_of_term(ty.term)) {
          | Typ.Atom(atom) =>
            let conversions = conversion_functions(atom);
            if (List.length(conversions) == 0) {
              [];
            } else {
              [
                Action({
                  text: "Convert →",
                  action: () => local(ShowSubmenu(["Convert"])) // Navigate to conversion submenu
                }),
              ];
            };
          | _ => []
          }
        | None => []
        };

      let move_items =
        (can_move_left ? [true] : [])
        @ (can_move_right ? [false] : [])
        |> List.map(left =>
             Action({
               text: left ? "Move Left" : "Move Right",
               action: () =>
                 Effect.Many([
                   local(CloseMenu),
                   parent(
                     SetSyntax(
                       OptUtil.get_or_fail(
                         (left ? "move left" : "move right") ++ " failed",
                         move_column(info, dyn_type, h, left),
                       ),
                     ),
                   ),
                 ]),
             })
           );

      let sort_submenu =
        switch (sort_column_with_direction(info, column_type, h, false)) {
        | Some(_) => [
            Action({
              text: "Sort →",
              action: () => local(ShowSubmenu(["Sort"])) // Navigate to sort submenu
            }),
          ]
        | None => []
        };

      base_items @ conversion_submenu @ move_items @ sort_submenu;
    | _ => [] // Unknown menu path
    };
  };

  let render_menu = menu_data => {
    List.map(
      item =>
        switch (item) {
        | Action({text, action}) => menu_item(text, _ => action())
        | Submenu({text, subitems: _}) =>
          // Submenu navigation is handled by the Action in build_column_menu
          menu_item(text, _ => Effect.Ignore)
        },
      menu_data,
    );
  };

  let table_with_column_menus =
      (
        model: model,
        info: info,
        ~local: action => Ui_effect.t(unit),
        ~parent: external_action => Ui_effect.t(unit),
        (headers, rows): (list(LabeledTuple.label), list(list(Exp.t))),
        ~view_seg: (Sort.t, Segment.t) => Node.t,
        prev_button: option(Node.t),
        next_button: option(Node.t),
        _make_menu_button: (int, string) => Node.t,
      ) => {
    /* Create action handler for the table renderer */
    let action_handler: TR.action_handler = {
      set_syntax: segment => parent(SetSyntax(segment)),
      local_action: table_action => {
        switch (table_action) {
        | CloseMenu => local(CloseMenu)
        | ShowMenu(i) => local(ShowMenu(i))
        | ShowSubmenu(path) => local(ShowSubmenu(path))
        | DropColumn(_) => local(CloseMenu) /* Will be handled by set_syntax */
        | ConversionColumn(_) => local(CloseMenu) /* Will be handled by set_syntax */
        | RenameColumn(_) => local(CloseMenu) /* Will be handled by set_syntax */
        | AddColumnAfter(_) => local(CloseMenu) /* Will be handled by set_syntax */
        };
      },
    };

    /* Prepare closure navigation buttons */
    let closure_nav =
      switch (prev_button, next_button) {
      | (Some(prev), Some(next_)) => Some((prev, next_))
      | _ => None
      };

    /* Prepare menu state */
    let menu_state =
      switch (model.menu) {
      | Some((column_index, menu_path)) => Some((column_index, menu_path))
      | None => None
      };

    /* Use TableRenderer to render the table */
    TR.render_table(
      ~headers,
      ~rows,
      ~info,
      ~view_seg,
      ~action_handler,
      ~closure_nav,
      ~menu_state,
      (),
    );
  };

  let init = (_any: Any.t) =>
    Some({
      closure: None,
      menu: None,
    });

  let focusable =
    Focusable.{
      pointer: None,
      keyboard: None,
    };
  let dynamics = true;
  let placeholder = (_, _info) => {
    ProjectorCore.Shape.{
      vertical: Block(11), // +1 for header row
      /* +2 for left and right padding */
      horizontal: 50 // +2 for left and right padding
    };
  };
  let update = (model, info, action) => {
    switch (action) {
    | CloseMenu => {
        ...model,
        menu: None,
      }
    | ShowMenu(i) => {
        ...model,
        menu:
          switch (model.menu) {
          | Some((j, _)) when i == j => None
          | _ => Some((i, [])) // Empty path for main menu
          },
      }
    | ShowSubmenu(new_path) => {
        ...model,
        menu:
          switch (model.menu) {
          | Some((column_index, _)) => Some((column_index, new_path))
          | None => None // Shouldn't happen, but just in case
          },
      }
    | ConversionColumn(_, _) =>
      // This action will be handled by the parent through the view
      model
    | DropColumn(_) =>
      // This action will be handled by the parent through the view
      model
    | _ =>
      let dynamics =
        info.dynamics
        |> Option.map((d: Dynamics.Info.t) => d.closures)
        |> Option.value(~default=[]);
      let length = List.length(dynamics);
      if (length == 0) {
        model;
      } else {
        let current = Option.value(model.closure, ~default=0);
        switch (action) {
        | Next => {
            ...model,
            closure: Some((current + 1) mod length),
          }
        | Previous => {
            ...model,
            closure: Some((current + length - 1) mod length),
          }
        | DropColumn(_) => model // Already handled above
        | ShowMenu(_) => model // Already handled above
        | ShowSubmenu(_) => model // Already handled above
        | ConversionColumn(_) => model // Already handled above
        | CloseMenu => model // Already handled above
        | RenameColumn(_, _) => model // This action will be handled by the parent through the view
        | AddColumnAfter(_, _) => model // This action will be handled by the parent through the view
        };
      };
    };
  };

  let view = (model, info, ~local, ~parent, ~view_seg: View.seg) => {
    let dynamics: list(Dynamics.Probe.Closure.t) =
      info.dynamics
      |> Option.map((d: Dynamics.Info.t) => d.closures)
      |> Option.value(~default=[]);

    let v =
      if (List.length(dynamics) == 0) {
        Node.div([Node.text("Loading dynamics...")]);
      } else {
        let length = List.length(dynamics);
        let observed = Option.value(model.closure, ~default=0) mod length;
        let closure = List.nth(dynamics, observed);

        let (prev_button, next_button) =
          if (length <= 1) {
            (None, None);
          } else {
            (
              Some(
                icon_button(~tooltip="Previous closure", "⬅", _ =>
                  local(Previous)
                ),
              ),
              Some(
                icon_button(~tooltip="Next closure", "➡", _ => local(Next)),
              ),
            );
          };

        let make_menu_button = (i, _h) =>
          icon_button(~tooltip="Column options", "⋮", _ =>
            local(ShowMenu(i))
          );

        let table_node =
          switch (table_from_exp(closure.value)) {
          | Some((hd, tl)) =>
            table_with_column_menus(
              model,
              info,
              ~local,
              ~parent,
              ~view_seg,
              (hd, tl),
              prev_button,
              next_button,
              make_menu_button,
            )
          | _ => Node.div([Node.text("No table data")])
          };

        table_node;
      };

    View.mk(v);
  };
};
