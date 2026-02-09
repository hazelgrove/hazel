open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

/* TableRenderer - A reusable module for rendering interactive tables with column operations */

[@deriving (show({with_path: false}), sexp, yojson)]
type v = (list(string), list(list(Exp.t))); /* (headers, rows) */

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
  | AddColumnAfter(string, string)
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
      action: unit => Ui_effect.t(unit),
    })
  | Submenu({
      text: string,
      subitems: list(menu_item),
    });

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

let max_column_length = 12;

/* Parse an expression into table structure */
let parse = (_sort: Sort.t, exp: Exp.t) => {
  switch (exp.term) {
  | ListLit(es) =>
    let data: list(option((list(string), list(TermBase.exp_t)))) =
      List.map(
        e => {
          switch (Unboxing.unbox(LabeledTupleEntries, e)) {
          | IndetMatch => None
          | DoesNotMatch => None
          | Matches(entries: list((option(string), TermBase.exp_t))) =>
            let f: option(list((string, TermBase.exp_t))) =
              OptUtil.sequence(
                List.map(
                  ((label, value)) =>
                    switch (label) {
                    | Some(l) => Some((l, value))
                    | None => None
                    },
                  entries,
                ),
              );

            f |> Option.map(List.split);
          }
        },
        es,
      );

    let data_opt = OptUtil.sequence(data);
    switch (data_opt) {
    | Some(data) =>
      let (headers: list(list(string)), rows: list(list(TermBase.exp_t))) =
        List.split(data);

      // If all the headers aren't the same or empty table
      switch (headers) {
      | [] => None
      | [h, ..._] when List.for_all(x => x == h, headers) => Some((h, rows)) // convert TermBase.exp_t to Exp.t
      | _ => None
      };
    | None => None
    };
  | _ => None
  };
};

/* Initialize table model from parsed value */
let init = (_: v) => {menu_state: None};

/* Type utilities for column operations */
let get_column_type_from_ty = (ty: Typ.t, column: string) => {
  switch (ty.term) {
  | List({term: Prod(tys), _}) =>
    let ty =
      List.find_map(
        ty => {
          open OptUtil.Syntax;
          let* (label, value_ty) = Typ.match_tup_label(ty);
          if (label == column) {
            Some(value_ty);
          } else {
            None;
          };
        },
        tys,
      );
    ty;
  | _ => None
  };
};

let get_columns = (ty: Typ.t): option(list(string)) => {
  switch (ty.term) {
  | List({term: Prod(tys), _}) =>
    let labels: option(list(string)) =
      OptUtil.traverse(
        ty => {
          open OptUtil.Syntax;
          let* (label, _value_ty) = Typ.match_tup_label(ty);
          Some(label);
        },
        tys,
      );
    labels;
  | _ => None
  };
};

/* Check if a type is an Option type (+None +Some(?))  */
let is_option_type = (ty: Typ.t): bool => {
  let ctx = Builtins.ctx_init(Some(Int));
  Typ.is_consistent(ctx, ty, BuiltinsADT.Option.t)
  && Typ.is_more_precise(ctx, ty, BuiltinsADT.Option.t);
};

let strip_parens =
  Exp.map_term(~f_exp=(continue, e) =>
    switch (e.term) {
    | Parens(inner) => continue(inner)
    | _ => continue(e)
    }
  );

/* Core transformation functions */
let apply_transformation = (info: info, transformation: Exp.t) => {
  IdTagged.FreshGrammar.(
    switch (
      info.utility.lift_syntax(
        ~inline=false,
        fun
        | Exp({term: exp_term, _}) =>
          Exp(
            Exp.(
              ap(
                Reverse,
                transformation,
                strip_parens(exp_term |> DHExp.fresh),
              )
            ),
          )

        | _ =>
          failwith("TableRenderer: apply_transformation: not an expression"),
        info.syntax,
      )
    ) {
    | Some(s) => s
    | None => failwith("TableRenderer: apply_transformation: lift failed")
    }
  );
};

let apply_rowwise_transformation =
    (info: info, row_transformation: Exp.t): Base.segment => {
  IdTagged.FreshGrammar.(
    apply_transformation(
      info,
      Exp.(
        ap(
          Forward,
          var("map"),
          tuple([deferral(InAp), row_transformation]),
        )
      ),
    )
  );
};

/* Column transformation operations */
let drop_column = (info: info, column: string): Base.segment => {
  IdTagged.FreshGrammar.(
    apply_rowwise_transformation(
      info,
      Exp.(
        ap(
          Forward,
          var("omit_labels"),
          tuple([deferral(InAp), label(column)]),
        )
      ),
    )
  );
};

let convert_column =
    (info: info, column: string, conversion_fn: string): Base.segment => {
  IdTagged.FreshGrammar.(
    Exp.(
      apply_rowwise_transformation(
        info,
        fn(
          Pat.var("r"),
          tuple_extension(
            var("r"),
            tuple([
              tup_label(
                label(column),
                ap(
                  Forward,
                  var(conversion_fn),
                  dot(var("r"), label(column)),
                ),
              ),
            ]),
          ),
          None,
          None,
        ),
      )
    )
  );
};

let rename_column =
    (info: info, old_name: string, new_name: string): Base.segment => {
  apply_rowwise_transformation(
    info,
    IdTagged.FreshGrammar.(
      Exp.(
        fn(
          Pat.var("r"),
          tuple_extension(
            ap(
              Forward,
              var("omit_labels"),
              tuple([var("r"), label(old_name)]),
            ),
            tuple([
              tup_label(label(new_name), dot(var("r"), label(old_name))),
            ]),
          ),
          None,
          None,
        )
      )
    ),
  );
};

let add_column_after =
    (info: info, _after_column: string, new_column: string): Base.segment => {
  IdTagged.FreshGrammar.(
    Exp.(
      apply_rowwise_transformation(
        info,
        fn(
          Pat.var("r"),
          tuple_extension(
            var("r"),
            tuple([tup_label(label(new_column), empty_hole())]),
          ),
          None,
          None,
        ),
      )
    )
  );
};

/* Replace column values with expression holes for manual reentry */
let clear_column = (info: info, column: string): Base.segment => {
  IdTagged.FreshGrammar.(
    Exp.(
      apply_rowwise_transformation(
        info,
        fn(
          Pat.var("r"),
          tuple_extension(
            var("r"),
            tuple([tup_label(label(column), empty_hole())]),
          ),
          None,
          None,
        ),
      )
    )
  );
};

/* No-op: places the field projection back into the field for reference */
let noop_column = (info: info, column: string): Base.segment => {
  IdTagged.FreshGrammar.(
    Exp.(
      apply_rowwise_transformation(
        info,
        fn(
          Pat.var("r"),
          tuple_extension(
            var("r"),
            tuple([
              tup_label(label(column), dot(var("r"), label(column))),
            ]),
          ),
          None,
          None,
        ),
      )
    )
  );
};

let group_by_column = (info: info, column: string): Base.segment => {
  IdTagged.FreshGrammar.(
    apply_transformation(
      info,
      Exp.(
        ap(
          Forward,
          var("group_on_key"),
          tuple([
            deferral(InAp),
            fn(
              Pat.var("row"),
              dot(var("row"), label(column)),
              None,
              None,
            ),
          ]),
        )
      ),
    )
  );
};

let filter_by_column = (op, info: info, column: string): Base.segment => {
  IdTagged.FreshGrammar.(
    apply_transformation(
      info,
      Exp.(
        ap(
          Forward,
          var("filter"),
          tuple([
            deferral(InAp),
            fn(
              Pat.var("row"),
              bin_op(op, dot(var("row"), label(column)), empty_hole()),
              None,
              None,
            ),
          ]),
        )
      ),
    )
  );
};

/* Drop rows where the option column is None, unwrapping Some values */
let drop_nones_column = (info: info, column: string): Base.segment => {
  IdTagged.FreshGrammar.(
    apply_transformation(
      info,
      Exp.(
        ap(
          Forward,
          var("filter_map"),
          tuple([
            deferral(InAp),
            fn(
              Pat.var("row"),
              ap(
                Forward,
                var("option_map"),
                tuple([
                  dot(var("row"), label(column)),
                  fn(
                    Pat.var("v"),
                    tuple_extension(
                      var("row"),
                      tuple([tup_label(label(column), var("v"))]),
                    ),
                    None,
                    None,
                  ),
                ]),
              ),
              None,
              None,
            ),
          ]),
        )
      ),
    )
  );
};

/* Replace None values with an expression hole for user to fill in default */
let provide_default_column = (info: info, column: string): Base.segment => {
  IdTagged.FreshGrammar.(
    apply_rowwise_transformation(
      info,
      Exp.(
        fn(
          Pat.var("row"),
          tuple_extension(
            var("row"),
            tuple([
              tup_label(
                label(column),
                match(
                  dot(var("row"), label(column)),
                  [
                    /* None => hole for user to fill in */
                    (BuiltinsADT.Option.pat_none, empty_hole()),
                    /* Some(x) => x */
                    (
                      Pat.ap(BuiltinsADT.Option.pat_some, Pat.var("v")),
                      var("v"),
                    ),
                  ],
                ),
              ),
            ]),
          ),
          None,
          None,
        )
      ),
    )
  );
};

let get_dynamic_type = (exp: Exp.t): option(Typ.t) => {
  let statics = Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)));
  IdTagged.rep_id(exp)
  |> Id.Map.find_opt(_, statics(exp))
  |> Option.bind(
       _,
       fun
       | InfoExp(e) => {
           Some(e.ty);
         }
       | _ => None,
     );
};

let can_move_column =
    (columns_opt: option(list(string)), column: string, left: bool) =>
  switch (columns_opt) {
  | Some(columns) =>
    switch (List.find_index(x => x == column, columns)) {
    | Some(idx) => left ? idx > 0 : idx < List.length(columns) - 1
    | None => false
    }
  | None => false
  };

let move_column =
    (info: info, dyn_type: option(Typ.t), column: string, left: bool)
    : option(Base.segment) => {
  let columns_opt = Option.bind(dyn_type, get_columns);
  switch (columns_opt) {
  | Some(columns) =>
    let idx_opt = List.find_index(x => x == column, columns);
    switch (idx_opt) {
    | Some(idx) =>
      let new_idx = left ? idx - 1 : idx + 1;
      if (new_idx < 0 || new_idx >= List.length(columns)) {
        None;
      } else {
        let new_columns =
          List.mapi(
            (i, x) =>
              if (i == idx) {
                List.nth(columns, new_idx);
              } else if (i == new_idx) {
                List.nth(columns, idx);
              } else {
                x;
              },
            columns,
          );
        IdTagged.FreshGrammar.Exp.(
          apply_rowwise_transformation(
            info,
            ap(
              Forward,
              var("select_labels"),
              tuple([deferral(InAp)] @ List.map(label, new_columns)),
            ),
          )
        )
        |> Option.some;
      };
    | None => None
    };
  | None => None
  };
};

/* Cell rendering utilities */
let len_seg = (utility: utility, seg: Segment.t): int =>
  seg |> utility.seg_to_string |> String.length;

let seg_of_exp = (utility: utility, exp: Exp.t): (Segment.t, int) => {
  let seg = utility.term_to_seg(~inline=true, Exp(exp));
  (seg, len_seg(utility, seg));
};

let abbreviated_seg_of =
    (utility: utility, available: int, exp: Exp.t): (Segment.t, int) => {
  let (abbr_exp, _length) =
    exp |> DHExp.strip_ascriptions |> Abbreviate.abbreviate_exp(~available);
  seg_of_exp(utility, abbr_exp);
};

let length_cls = (length: int): string =>
  if (length > 10) {
    "extra";
  } else if (length > 9) {
    "s6";
  } else if (length > 8) {
    "s5";
  } else if (length > 7) {
    "s4";
  } else if (length > 6) {
    "s3";
  } else if (length > 5) {
    "s2";
  } else if (length > 4) {
    "s1";
  } else {
    "s0";
  };

let value_view = (_info: info, utility: utility, view_seg, exp) => {
  let (seg, length) = abbreviated_seg_of(utility, max_column_length, exp);

  Node.div(
    ~attrs=[Attr.classes(["value", length_cls(length)])],
    [view_seg(Sort.Exp, seg)],
  );
};

/* Menu system */
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
      switch (Typ.cls_of_term(ty.term)) {
      | Typ.Atom(Atom.Int) => Some("int_compare")
      | Typ.Atom(Atom.Float) => Some("float_compare")
      | Typ.Atom(Atom.String) => Some("string_compare")
      | _ => None
      }
    | None => None
    };
  switch (compare_fn) {
  | Some(compare_fn_name) =>
    IdTagged.FreshGrammar.(
      switch (
        info.utility.lift_syntax(
          ~inline=false,
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
          | _ => failwith("TableRenderer: sort_column: not an expression"),
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

  // If we're in a submenu, show that submenu
  switch (menu_path) {
  | ["Filter"] =>
    // Show filter submenu

    [
      Action({
        text: "← Back",
        action: () => local(ShowSubmenu([])) // Go back to main menu
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
              action: () =>
                Effect.Many([
                  local(CloseMenu),
                  parent(SetSyntax(filter_by_column(op, info, h))),
                ]),
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
              action: () =>
                Effect.Many([
                  local(CloseMenu),
                  parent(SetSyntax(filter_by_column(op, info, h))),
                ]),
            }),
          ]
        | None => []
        }
      )
      @ [
        Action({
          text: "Equals",
          action: () =>
            Effect.Many([
              local(CloseMenu),
              parent(SetSyntax(filter_by_column(Poly(Equals), info, h))),
            ]),
        }),
      ];
    }
  | ["Convert"] =>
    // Show conversion submenu
    switch (column_type) {
    | Some(ty) =>
      switch (Typ.cls_of_term(ty.term)) {
      | Typ.Atom(atom) =>
        let conversions = conversion_functions(atom);
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
  | ["Transform"] =>
    // Show transform submenu
    [
      Action({
        text: "← Back",
        action: () => local(ShowSubmenu([])) // Go back to main menu
      }),
      Action({
        text: "Clear",
        action: () =>
          Effect.Many([
            local(CloseMenu),
            parent(SetSyntax(clear_column(info, h))),
          ]),
      }),
      Action({
        text: "Identity",
        action: () =>
          Effect.Many([
            local(CloseMenu),
            parent(SetSyntax(noop_column(info, h))),
          ]),
      }),
    ]
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
        action: () => {
          let new_column_name = JsUtil.prompt("New column name:", "");
          switch (new_column_name) {
          | None => local(CloseMenu) // User cancelled
          | Some(new_name) =>
            Effect.Many([
              local(CloseMenu),
              parent(SetSyntax(add_column_after(info, h, new_name))),
            ])
          };
        },
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

    /* Transform submenu is always available */
    let transform_submenu = [
      Action({
        text: "Transform →",
        action: () => local(ShowSubmenu(["Transform"])) // Navigate to transform submenu
      }),
    ];

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

    let filter_submenu =
      switch (column_type) {
      | Some(ty) =>
        switch (Typ.cls_of_term(ty.term)) {
        | Typ.Atom(Atom.Int | Atom.Float) => [
            Action({
              text: "Filter →",
              action: () => local(ShowSubmenu(["Filter"])) // Navigate to filter submenu
            }),
          ]
        | _ => []
        }
      | None => []
      };

    /* Option type actions: Drop Nones and Provide Default */
    let option_items =
      switch (column_type) {
      | Some(ty) =>
        is_option_type(ty)
          ? [
            Action({
              text: "Drop Nones",
              action: () =>
                Effect.Many([
                  local(CloseMenu),
                  parent(SetSyntax(drop_nones_column(info, h))),
                ]),
            }),
            Action({
              text: "Provide Default",
              action: () =>
                Effect.Many([
                  local(CloseMenu),
                  parent(SetSyntax(provide_default_column(info, h))),
                ]),
            }),
          ]
          : []
      | None => []
      };

    base_items
    @ [
      Action({
        text: "Group By",
        action: () =>
          Effect.Many([
            local(CloseMenu),
            parent(SetSyntax(group_by_column(info, h))),
          ]),
      }),
    ]
    @ conversion_submenu
    @ transform_submenu
    @ move_items
    @ sort_submenu
    @ filter_submenu
    @ option_items;
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
  // Parse the current expression instead of using the stale stored value
  let make_menu_button = (i, _h) =>
    icon_button(~tooltip="Column options", "⋮", _ => local(ShowMenu(i)));

  let header_cells =
    List.mapi(
      (i, h) => {
        let menu_button = make_menu_button(i, h);
        let base_content = [
          Node.text(h),
          is_readonly ? Node.none : menu_button,
        ];

        /* Add navigation buttons to first and last columns if provided */
        let content = base_content;

        let full_content =
          switch (model.menu_state) {
          | Some((j, menu_path)) when i == j =>
            let dyn_type = get_dynamic_type(exp); /* Is there a better way to get the types of the columns? */
            let menu_data =
              build_column_menu(info, h, dyn_type, local, parent, menu_path);
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

  Node.table(
    ~attrs=[Attr.classes(["table"])],
    [
      Node.thead([Node.tr(header_cells)]),
      Node.tbody(
        List.map(
          row =>
            Node.tr(
              List.map(
                e => Node.td([value_view(info, info.utility, view_seg, e)]),
                row,
              ),
            ),
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
