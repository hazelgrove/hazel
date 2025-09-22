open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

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

let table_from_exp = (exp: Exp.t) => {
  switch (exp.term) {
  | ListLit(es) =>
    let data: list(option((list(string), list(TermBase.exp_t)))) =
      List.map(
        e => {
          switch (Unboxing.unbox(LabeledTupleEntries, e)) {
          // TODO Stop doing this with unboxing and deconstruct it here with the parens
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

            let g: option((list(string), list(TermBase.exp_t))) =
              f |> Option.map(List.split);

            g;
          }
        },
        es,
      );

    let data: option(list((list(string), list(TermBase.exp_t)))) =
      OptUtil.sequence(data);
    switch (data) {
    | Some(data: list((list(string), list(TermBase.exp_t)))) =>
      let (headers: list(list(string)), rows: list(list(TermBase.exp_t))) =
        List.split(data);

      // If all the headers aren't the same return None
      switch (headers) {
      | [] => None
      | [h, ..._] when List.for_all(x => x == h, headers) =>
        let headers = h;
        Some((headers, rows));

      | _ => None
      };
    | _ => None
    };
  | _ => None
  };
};
let drop_column = (info: info, column: string): Base.segment => {
  IdTagged.FreshGrammar.(
    switch (
      info.utility.lift_syntax(
        fun
        | Exp({term: exp_term, _}) =>
          Exp(
            Exp.(
              ap(
                Reverse,
                ap(
                  Forward,
                  var("map"),
                  tuple([
                    deferral(InAp),
                    ap(
                      Forward,
                      var("omit_labels"),
                      tuple([deferral(InAp), label(column)]),
                    ),
                  ]),
                ),
                exp_term |> DHExp.fresh,
              )
            ),
          )
        | _ => failwith("TableProj: drop_column: not an expression"),
        info.syntax,
      )
    ) {
    | Some(s) => s
    | None => failwith("TableProj: drop_column: lift failed")
    }
  );
};

let convert_column_int = (info: info, column: string): Base.segment => {
  IdTagged.FreshGrammar.(
    switch (
      info.utility.lift_syntax(
        fun
        | Exp({term: exp_term, _}) =>
          Exp(
            Exp.(
              ap(
                Reverse,
                ap(
                  Forward,
                  var("map"),
                  tuple([
                    deferral(InAp),
                    fn(
                      Pat.var("r"),
                      tuple_extension(
                        var("r"),
                        tuple([
                          tup_label(
                            label(column),
                            ap(
                              Forward,
                              var("int_of_string"),
                              dot(var("r"), label(column)),
                            ),
                          ),
                        ]),
                      ),
                      None,
                      None,
                    ),
                  ]),
                ),
                exp_term |> DHExp.fresh,
              )
            ),
          )
        | _ => failwith("TableProj: convert_column_int: not an expression"),
        info.syntax,
      )
    ) {
    | Some(s) => s
    | None => failwith("TableProj: convert_column_int: lift failed")
    }
  );
};

let convert_column_float = (info: info, column: string): Base.segment => {
  IdTagged.FreshGrammar.(
    switch (
      info.utility.lift_syntax(
        fun
        | Exp({term: exp_term, _}) =>
          Exp(
            Exp.(
              ap(
                Reverse,
                ap(
                  Forward,
                  var("map"),
                  tuple([
                    deferral(InAp),
                    fn(
                      Pat.var("r"),
                      tuple_extension(
                        var("r"),
                        tuple([
                          tup_label(
                            label(column),
                            ap(
                              Forward,
                              var("float_of_string"),
                              dot(var("r"), label(column)),
                            ),
                          ),
                        ]),
                      ),
                      None,
                      None,
                    ),
                  ]),
                ),
                exp_term |> DHExp.fresh,
              )
            ),
          )
        | _ => failwith("TableProj: convert_column_float: not an expression"),
        info.syntax,
      )
    ) {
    | Some(s) => s
    | None => failwith("TableProj: convert_column_float: lift failed")
    }
  );
};

let convert_column_bool = (info: info, column: string): Base.segment => {
  IdTagged.FreshGrammar.(
    switch (
      info.utility.lift_syntax(
        fun
        | Exp({term: exp_term, _}) =>
          Exp(
            Exp.(
              ap(
                Reverse,
                ap(
                  Forward,
                  var("map"),
                  tuple([
                    deferral(InAp),
                    fn(
                      Pat.var("r"),
                      tuple_extension(
                        var("r"),
                        tuple([
                          tup_label(
                            label(column),
                            ap(
                              Forward,
                              var("bool_of_string"),
                              dot(var("r"), label(column)),
                            ),
                          ),
                        ]),
                      ),
                      None,
                      None,
                    ),
                  ]),
                ),
                exp_term |> DHExp.fresh,
              )
            ),
          )
        | _ => failwith("TableProj: convert_column_bool: not an expression"),
        info.syntax,
      )
    ) {
    | Some(s) => s
    | None => failwith("TableProj: convert_column_bool: lift failed")
    }
  );
};

let convert_column_string_from_int =
    (info: info, column: string): Base.segment => {
  IdTagged.FreshGrammar.(
    switch (
      info.utility.lift_syntax(
        fun
        | Exp({term: exp_term, _}) =>
          Exp(
            Exp.(
              ap(
                Reverse,
                ap(
                  Forward,
                  var("map"),
                  tuple([
                    deferral(InAp),
                    fn(
                      Pat.var("r"),
                      tuple_extension(
                        var("r"),
                        tuple([
                          tup_label(
                            label(column),
                            ap(
                              Forward,
                              var("string_of_int"),
                              dot(var("r"), label(column)),
                            ),
                          ),
                        ]),
                      ),
                      None,
                      None,
                    ),
                  ]),
                ),
                exp_term |> DHExp.fresh,
              )
            ),
          )
        | _ =>
          failwith(
            "TableProj: convert_column_string_from_int: not an expression",
          ),
        info.syntax,
      )
    ) {
    | Some(s) => s
    | None =>
      failwith("TableProj: convert_column_string_from_int: lift failed")
    }
  );
};

let convert_column_string_from_float =
    (info: info, column: string): Base.segment => {
  IdTagged.FreshGrammar.(
    switch (
      info.utility.lift_syntax(
        fun
        | Exp({term: exp_term, _}) =>
          Exp(
            Exp.(
              ap(
                Reverse,
                ap(
                  Forward,
                  var("map"),
                  tuple([
                    deferral(InAp),
                    fn(
                      Pat.var("r"),
                      tuple_extension(
                        var("r"),
                        tuple([
                          tup_label(
                            label(column),
                            ap(
                              Forward,
                              var("string_of_float"),
                              dot(var("r"), label(column)),
                            ),
                          ),
                        ]),
                      ),
                      None,
                      None,
                    ),
                  ]),
                ),
                exp_term |> DHExp.fresh,
              )
            ),
          )
        | _ =>
          failwith(
            "TableProj: convert_column_string_from_float: not an expression",
          ),
        info.syntax,
      )
    ) {
    | Some(s) => s
    | None =>
      failwith("TableProj: convert_column_string_from_float: lift failed")
    }
  );
};

let convert_column_string_from_bool =
    (info: info, column: string): Base.segment => {
  IdTagged.FreshGrammar.(
    switch (
      info.utility.lift_syntax(
        fun
        | Exp({term: exp_term, _}) =>
          Exp(
            Exp.(
              ap(
                Reverse,
                ap(
                  Forward,
                  var("map"),
                  tuple([
                    deferral(InAp),
                    fn(
                      Pat.var("r"),
                      tuple_extension(
                        var("r"),
                        tuple([
                          tup_label(
                            label(column),
                            ap(
                              Forward,
                              var("string_of_bool"),
                              dot(var("r"), label(column)),
                            ),
                          ),
                        ]),
                      ),
                      None,
                      None,
                    ),
                  ]),
                ),
                exp_term |> DHExp.fresh,
              )
            ),
          )
        | _ =>
          failwith(
            "TableProj: convert_column_string_from_bool: not an expression",
          ),
        info.syntax,
      )
    ) {
    | Some(s) => s
    | None =>
      failwith("TableProj: convert_column_string_from_bool: lift failed")
    }
  );
};
let get_column_type' = (ty: Typ.t, column: string) => {
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

let get_dynamic_type = (info: info): option(Typ.t) => {
  info.dynamics
  |> Option.bind(
       _,
       (d: Dynamics.Info.t) => {
         let statics =
           Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)));
         let type_of = (c: Dynamics.Probe.Closure.t) => {
           IdTagged.rep_id(c.value)
           |> Id.Map.find_opt(_, statics(c.value))
           |> Option.bind(
                _,
                fun
                | InfoExp(e) => {
                    Some(e.ty);
                  }
                | _ => None,
              );
         };
         let types = List.map(type_of, d) |> Util.OptUtil.sequence;

         Option.bind(
           types,
           Typ.join_all(~empty=Unknown(Internal) |> Typ.temp, Ctx.empty),
         );
       },
     );
};

let get_column_type = (info: info, column: string) => {
  switch (info.statics) {
  | Some(InfoExp({ty, _})) => get_column_type'(ty, column)
  | _ => None
  };
};

let table_of =
    (any: Any.t): option((list(LabeledTuple.label), list(list(Exp.t)))) =>
  switch (any) {
  | Exp(exp) => table_from_exp(exp)
  | _ => None
  };

let get = (info: info): (list(LabeledTuple.label), list(list(Exp.t))) =>
  switch (info.syntax |> info.utility.seg_to_term) {
  | Some(s) =>
    switch (table_of(s)) {
    | Some(s) => s
    | None => failwith("TextArea: get: Not a table")
    }
  | None => failwith("TextArea: get: Not a table")
  };

let key_handler = (id, ~parent, evt) => {
  open Effect;
  let key = Key.mk(KeyDown, evt);

  switch (key.key) {
  | D("ArrowRight" | "ArrowDown")
      when WebUtil.TextArea.is_last_pos(Id.cls(id)) =>
    JsUtil.get_elem_by_id(Id.cls(id))##blur;
    Many([parent(Escape(Right)), Stop_propagation]);
  | D("ArrowLeft" | "ArrowUp")
      when WebUtil.TextArea.is_first_pos(Id.cls(id)) =>
    JsUtil.get_elem_by_id(Id.cls(id))##blur;
    Many([parent(Escape(Left)), Stop_propagation]);
  /* Defer to parent editor undo for now */
  | D("z" | "Z" | "y" | "Y") when Key.ctrl_held(evt) || Key.meta_held(evt) =>
    Many([Prevent_default])
  | D("z" | "Z")
      when Key.shift_held(evt) && (Key.ctrl_held(evt) || Key.meta_held(evt)) =>
    Many([Prevent_default])
  | D("\"") =>
    /* Hide quotes from both the textarea and parent editor */
    Many([Prevent_default, Stop_propagation])
  | _ => Stop_propagation
  };
};

let len_seg = (utility: utility, seg: Segment.t): int =>
  seg |> utility.seg_to_string |> String.length;

let seg_of_exp = (utility: utility, exp: Exp.t): (Segment.t, int) => {
  let seg = utility.term_to_seg(Exp(exp));
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
    ~attrs=[
      //Attr.title(DynCursor.Debug.str(info, closure)),
      Attr.classes([
        "value",
        length_cls(length),
        // @ DynCursor.clss(info, closure)
        // @ (Option.is_some(cur_ap(info)) ? ["ap"] : [])
        // @ (!is_value(closure.value) ? ["indet"] : []),
      ]),
      // Attr.on_double_click(_ => local(ToggleShowAllVals(index))),
      // Attr.on_pointerdown(val_pointerdown),
      // Attr.on_pointerup(val_pointerup),
      // Attr.on_mousemove(val_mousemove),
    ],
    [view_seg(Sort.Exp, seg)],
  );
};

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type menu_state =
    | MainMenu
    | ConversionSubmenu;

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
    | ShowConversionMenu(int)
    | CloseMenu
    | BackToMainMenu(int)
    | ConversionColumn(string, string);

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
        make_menu_button: (int, string) => Node.t,
      ) => {
    let header_cells =
      List.mapi(
        (i, h) => {
          let menu_button = make_menu_button(i, h);
          let base_content = [Node.text(h), menu_button];
          let content =
            switch (i, prev_button, next_button) {
            | (0, Some(btn), _) => [btn] @ base_content
            | (i, _, Some(btn)) when i == List.length(headers) - 1 =>
              base_content @ [btn]
            | _ => base_content
            };
          let cell_content = content;
          let full_content =
            switch (model.menu) {
            | Some((j, menu_state)) when i == j =>
              let menu_content =
                switch (menu_state) {
                | MainMenu => [
                    Node.div(
                      ~attrs=[
                        Attr.classes(["menu-item"]),
                        Attr.on_click(_ =>
                          Effect.Many([
                            local(CloseMenu),
                            parent(SetSyntax(drop_column(info, h))),
                          ])
                        ),
                      ],
                      [Node.text("Drop Column")],
                    ),
                    Node.div(
                      ~attrs=[
                        Attr.classes(["menu-item"]),
                        Attr.on_click(_ => local(CloseMenu)),
                      ],
                      [Node.text("Rename")],
                    ),
                    Node.div(
                      ~attrs=[
                        Attr.classes(["menu-item"]),
                        Attr.on_click(_ => local(CloseMenu)),
                      ],
                      [Node.text("Add Column After")],
                    ),
                    Node.div(
                      ~attrs=[
                        Attr.classes(["menu-item"]),
                        Attr.on_click(_ => local(CloseMenu)),
                      ],
                      [Node.text("Move Left")],
                    ),
                    Node.div(
                      ~attrs=[
                        Attr.classes(["menu-item"]),
                        Attr.on_click(_ => local(CloseMenu)),
                      ],
                      [Node.text("Move Right")],
                    ),
                    Node.div(
                      ~attrs=[
                        Attr.classes(["menu-item"]),
                        Attr.on_click(_ => local(ShowConversionMenu(i))),
                      ],
                      [Node.text("Convert →")],
                    ),
                  ]
                | ConversionSubmenu =>
                  let back_button =
                    Node.div(
                      ~attrs=[
                        Attr.classes(["submenu-back"]),
                        Attr.on_click(_ => local(BackToMainMenu(i))),
                      ],
                      [Node.text("← Back")],
                    );
                  let column_type =
                    get_dynamic_type(info)
                    |> Option.bind(_, get_column_type'(_, h));

                  let submenu_items =
                    switch (column_type) {
                    | Some(ty) =>
                      switch (Typ.cls_of_term(ty.term)) {
                      | Typ.Atom(Atom.String) => [
                          Node.div(
                            ~attrs=[
                              Attr.classes(["submenu-item"]),
                              Attr.on_click(_ =>
                                Effect.Many([
                                  local(CloseMenu),
                                  parent(
                                    SetSyntax(convert_column_int(info, h)),
                                  ),
                                ])
                              ),
                            ],
                            [Node.text("int")],
                          ),
                          Node.div(
                            ~attrs=[
                              Attr.classes(["submenu-item"]),
                              Attr.on_click(_ =>
                                Effect.Many([
                                  local(CloseMenu),
                                  parent(
                                    SetSyntax(convert_column_float(info, h)),
                                  ),
                                ])
                              ),
                            ],
                            [Node.text("float")],
                          ),
                          Node.div(
                            ~attrs=[
                              Attr.classes(["submenu-item"]),
                              Attr.on_click(_ =>
                                Effect.Many([
                                  local(CloseMenu),
                                  parent(
                                    SetSyntax(convert_column_bool(info, h)),
                                  ),
                                ])
                              ),
                            ],
                            [Node.text("bool")],
                          ),
                        ]
                      | Typ.Atom(Atom.Int) => [
                          Node.div(
                            ~attrs=[
                              Attr.classes(["submenu-item"]),
                              Attr.on_click(_ =>
                                Effect.Many([
                                  local(CloseMenu),
                                  parent(
                                    SetSyntax(
                                      convert_column_string_from_int(info, h),
                                    ),
                                  ),
                                ])
                              ),
                            ],
                            [Node.text("string")],
                          ),
                        ]
                      | Typ.Atom(Atom.Float) => [
                          Node.div(
                            ~attrs=[
                              Attr.classes(["submenu-item"]),
                              Attr.on_click(_ =>
                                Effect.Many([
                                  local(CloseMenu),
                                  parent(
                                    SetSyntax(
                                      convert_column_string_from_float(
                                        info,
                                        h,
                                      ),
                                    ),
                                  ),
                                ])
                              ),
                            ],
                            [Node.text("string")],
                          ),
                        ]
                      | Typ.Atom(Atom.Bool) => [
                          Node.div(
                            ~attrs=[
                              Attr.classes(["submenu-item"]),
                              Attr.on_click(_ =>
                                Effect.Many([
                                  local(CloseMenu),
                                  parent(
                                    SetSyntax(
                                      convert_column_string_from_bool(
                                        info,
                                        h,
                                      ),
                                    ),
                                  ),
                                ])
                              ),
                            ],
                            [Node.text("string")],
                          ),
                        ]
                      | _ => []
                      }
                    | None => []
                    };
                  [back_button] @ submenu_items;
                };
              cell_content
              @ [
                Node.div(
                  ~attrs=[Attr.classes(["column-menu"])],
                  menu_content,
                ),
              ];
            | _ => cell_content
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
                  e =>
                    Node.td([value_view(info, info.utility, view_seg, e)]),
                  row,
                ),
              ),
            rows,
          ),
        ),
      ],
    );
  };

  let init = (_any: Term.Any.t) =>
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
          | _ => Some((i, MainMenu))
          },
      }
    | ShowConversionMenu(i) => {
        ...model,
        menu: Some((i, ConversionSubmenu)),
      }
    | BackToMainMenu(i) => {
        ...model,
        menu: Some((i, MainMenu)),
      }
    | ConversionColumn(_, _) =>
      // This action will be handled by the parent through the view
      model
    | DropColumn(_) =>
      // This action will be handled by the parent through the view
      model
    | _ =>
      let dynamics = info.dynamics |> Option.value(~default=[]);
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
        | ShowConversionMenu(_) => model // Already handled above
        | BackToMainMenu(_) => model // Already handled above
        | ConversionColumn(_, _) => model // Already handled above
        | CloseMenu => model // Already handled above
        };
      };
    };
  };

  let view = (model, info, ~local, ~parent, ~view_seg: View.seg) => {
    let dynamics: list(Dynamics.Probe.Closure.t) =
      info.dynamics |> Option.value(~default=[]);

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
