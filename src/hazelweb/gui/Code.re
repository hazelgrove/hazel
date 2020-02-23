module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;
open ViewUtil;

module Contenteditable = {
  let contenteditable_false = Vdom.Attr.create("contenteditable", "false");

  let whitespace = (~clss=[], n: int) =>
    Vdom.(
      Node.span(
        [contenteditable_false, Attr.classes(clss)],
        [Node.text(StringUtil.replicat(n, UnicodeConstants.nbsp))],
      )
    );
  let leading_whitespace_cls = "leading-whitespace";
  let leading_whitespace = whitespace(~clss=[leading_whitespace_cls]);
  let trailing_whitespace = whitespace(~clss=["trailing-whitespace"]);

  let view_of_layout =
      (
        ~inject: Update.Action.t => Vdom.Event.t,
        ~show_contenteditable: bool,
        ~width: int,
        l: TermLayout.t,
      )
      : Vdom.Node.t => {
    open Vdom;

    let caret_position_Op = (~side: Side.t, ~has_caret: bool) => {
      Node.span(
        [
          Attr.classes([
            "caret-position",
            "Op",
            Side.to_string(side),
            ...has_caret ? ["has-caret"] : [],
          ]),
        ],
        // TODO: Once we figure out contenteditable cursor use `Node.text("")`
        [Node.text(UnicodeConstants.zwsp)],
      );
    };

    let caret_position_Delim =
        (~index: DelimIndex.t, ~side: Side.t, ~has_caret: bool) => {
      Node.span(
        [
          Attr.classes([
            "caret-position",
            "Delim",
            Side.to_string(side),
            ...has_caret ? ["has-caret"] : [],
          ]),
          Attr.create("index", string_of_int(index)),
        ],
        [Node.text(UnicodeConstants.zwsp)],
      );
    };

    let caret_position_EmptyLine = (~has_caret: bool) =>
      Node.span(
        [
          Attr.classes([
            "caret-position-EmptyLine",
            ...has_caret ? ["has-caret"] : [],
          ]),
        ],
        [Node.text(UnicodeConstants.zwsp)],
      );

    let found_linebreak = ref(false);
    l
    |> Layout.make_of_layout({
         imp_of_string: s => [Node.text(s)],
         imp_append: (@),
         imp_newline: (~last_col, ~indent) => {
           found_linebreak := true;
           [
             trailing_whitespace(width - last_col),
             Node.br([]),
             leading_whitespace(indent),
           ];
         },
         imp_of_annot: (annot: TermAnnot.t, vs) =>
           switch (annot) {
           | Step(step) => [
               Node.span([Attr.create("step", string_of_int(step))], vs),
             ]
           | Delim({caret, index}) => [
               caret_position_Delim(
                 ~index,
                 ~side=Before,
                 ~has_caret=caret == Some(Before),
               ),
               Node.span(
                 [
                   Attr.classes(["code-delim"]),
                   Attr.create("index", string_of_int(index)),
                   contenteditable_false,
                 ],
                 vs,
               ),
               caret_position_Delim(
                 ~index,
                 ~side=After,
                 ~has_caret=caret == Some(After),
               ),
             ]
           | Op({caret}) => [
               caret_position_Op(
                 ~side=Before,
                 ~has_caret=caret == Some(Before),
               ),
               Node.span([contenteditable_false], vs),
               caret_position_Op(
                 ~side=After,
                 ~has_caret=caret == Some(After),
               ),
             ]
           | EmptyLine({has_caret}) => [
               Node.span(
                 [Attr.classes(["EmptyLine"])],
                 [caret_position_EmptyLine(~has_caret)],
               ),
             ]
           | SpaceOp => [
               Node.span(
                 [contenteditable_false, Attr.classes(["SpaceOp"])],
                 vs,
               ),
             ]
           | Text(_) => [Node.span([Attr.classes(["code-text"])], vs)]
           | Padding => [
               Node.span(
                 [contenteditable_false, Attr.classes(["Padding"])],
                 vs,
               ),
             ]
           | Indent => [
               Node.span(
                 [
                   contenteditable_false,
                   Attr.classes([leading_whitespace_cls]),
                 ],
                 vs,
               ),
             ]
           | UserNewline => []
           | OpenChild(_)
           | ClosedChild(_)
           | HoleLabel(_)
           | DelimGroup
           | LetLine
           | Term(_) => vs
           },
         t_of_imp: (~last_col, vs) => {
           let vs =
             found_linebreak^
               ? vs : vs @ [trailing_whitespace(width - last_col)];
           Node.div(
             [
               // TODO clean up attrs
               Attr.id("contenteditable"),
               Attr.classes(
                 ["code", "contenteditable"]
                 @ (show_contenteditable ? [] : ["hiddencontenteditable"]),
               ),
               Attr.create("contenteditable", "true"),
               Attr.on("drop", _ => Event.Prevent_default),
               Attr.on_focus(_ => inject(Update.Action.FocusCell)),
               Attr.on_blur(_ => inject(Update.Action.BlurCell)),
             ],
             vs,
           );
         },
       });
  };

  let caret_position_of_path = ((steps, cursor): CursorPath.t) => {
    let (cursor_selector, anchor_offset) =
      switch (cursor) {
      | OnText(j) => (".code-text", j)
      | OnDelim(index, side) => (
          Printf.sprintf(
            ".code-delim[index=%d] .caret-position.%s",
            index,
            Side.to_string(side),
          ),
          side == Before ? 1 : 0,
        )
      | OnOp(side) => (
          Printf.sprintf(
            ".code-op .caret-position.%s",
            Side.to_string(side),
          ),
          side == Before ? 1 : 0,
        )
      };
    let selector =
      Js.string(
        StringUtil.cat(
          (steps |> List.map(step => Printf.sprintf("[step=%d]", step)))
          @ [cursor_selector],
        ),
      );
    let anchor_parent =
      JSUtil.force_get_elem_by_id("contenteditable")
      |> JSUtil.force_query_selector(selector);
    (anchor_parent, anchor_offset);
  };

  let has_cls = JSUtil.elem_has_cls;
  let has_any_cls = (clss, elem) =>
    clss |> List.exists(cls => elem |> has_cls(cls));

  let steps_of_caret_position =
      (elem: Js.t(Dom_html.element)): CursorPath.steps => {
    let steps = ref([]);
    let ancestor = ref(elem);
    // TODO use better root condition
    while (!(ancestor^ |> has_cls("code"))) {
      switch (ancestor^ |> JSUtil.get_attr("step")) {
      | None => ()
      | Some(step) =>
        let step = int_of_string(step);
        steps := [step, ...steps^];
      };
      ancestor := ancestor^ |> JSUtil.force_get_parent_elem;
    };
    steps^;
  };

  let rec schedule_move_or_transport =
          (
            ~schedule_action: Update.Action.t => unit,
            ~setting_caret: ref(bool),
            anchor_offset: int,
            anchor_parent: Js.t(Dom_html.element),
          )
          : unit => {
    let schedule_move_or_transport' =
      schedule_move_or_transport(~schedule_action, ~setting_caret);

    let set_caret = (anchor_parent, anchor_offset) => {
      setting_caret := true;
      JSUtil.set_caret(anchor_parent, anchor_offset);
      setting_caret := false;
    };

    /**
     * Transport to last caret position
     * preceding elem in pre-order traversal
     */
    let rec transport_prev = elem =>
      if (elem |> has_cls("code")) {
        JSUtil.log("Caret position not found");
        ();
      } else {
        switch (elem |> JSUtil.get_prev_sibling_elem) {
        | None =>
          let parent = elem |> JSUtil.force_get_parent_elem;
          transport_prev(parent);
        | Some(prev) =>
          if (prev |> has_cls("caret-position")) {
            let anchor_offset = prev |> has_cls("Before") ? 1 : 0;
            set_caret(prev, anchor_offset);
          } else if (prev |> has_cls("code-text")) {
            let anchor_offset = prev |> JSUtil.inner_text |> String.length;
            set_caret(prev, anchor_offset);
          } else if (prev |> JSUtil.has_attr("step")) {
            let prev_last_child = prev |> JSUtil.force_get_last_child_elem;
            transport_prev(prev_last_child);
          } else {
            transport_prev(prev);
          }
        };
      };

    /**
     * Transport move to next caret position
     * following elem in post-order traversal
     */
    let rec transport_next = elem =>
      if (elem |> has_cls("code")) {
        JSUtil.log("Caret position not found");
        ();
      } else {
        switch (elem |> JSUtil.get_next_sibling_elem) {
        | None =>
          let parent = elem |> JSUtil.force_get_parent_elem;
          transport_next(parent);
        | Some(next) =>
          if (next |> has_cls("caret-position")) {
            let anchor_offset = next |> has_cls("Before") ? 1 : 0;
            set_caret(next, anchor_offset);
          } else if (next |> has_cls("code-text")) {
            let anchor_offset = next |> JSUtil.inner_text |> String.length;
            set_caret(next, anchor_offset);
          } else if (next |> JSUtil.has_attr("step")) {
            let next_last_child = next |> JSUtil.force_get_first_child_elem;
            transport_next(next_last_child);
          } else {
            transport_next(next);
          }
        };
      };

    if (anchor_parent |> has_cls("caret-position")) {
      if (anchor_parent |> has_cls("has-caret")) {
        let transport =
          anchor_parent |> has_cls("Before")
            ? transport_prev : transport_next;
        transport(anchor_parent);
      } else {
        let cursor: CursorPosition.t = {
          let side: Side.t =
            anchor_parent |> has_cls("Before") ? Before : After;
          if (anchor_parent |> has_cls("Delim")) {
            let index =
              int_of_string(anchor_parent |> JSUtil.force_get_attr("index"));
            OnDelim(index, side);
          } else {
            OnOp(side);
          };
        };
        let steps = steps_of_caret_position(anchor_parent);
        schedule_action(Update.Action.EditAction(MoveTo((steps, cursor))));
      };
    } else if (anchor_parent |> has_cls("caret-position-EmptyLine")) {
      if (anchor_parent |> has_cls("has-caret")) {
        let transport = anchor_offset == 0 ? transport_prev : transport_next;
        transport(anchor_parent);
      } else {
        let steps = steps_of_caret_position(anchor_parent);
        schedule_action(
          Update.Action.EditAction(MoveTo((steps, OnText(0)))),
        );
      };
    } else if (anchor_parent |> has_cls("code-text")) {
      let cursor = CursorPosition.OnText(anchor_offset);
      let steps = steps_of_caret_position(anchor_parent);
      schedule_action(Update.Action.EditAction(MoveTo((steps, cursor))));
    } else if (anchor_parent
               |> has_any_cls(["Padding", "SpaceOp", "code-delim", "code-op"])) {
      let n = {
        let s = Js.Opt.get(anchor_parent##.textContent, () => assert(false));
        s##.length;
      };
      let get_sibling_elem =
        anchor_offset * 2 <= n
          ? JSUtil.force_get_prev_sibling_elem
          : JSUtil.force_get_next_sibling_elem;
      schedule_move_or_transport'(0, anchor_parent |> get_sibling_elem);
    } else if (anchor_parent |> has_cls("trailing-whitespace")) {
      schedule_move_or_transport'(
        0,
        anchor_parent |> JSUtil.force_get_prev_sibling_elem,
      );
    } else if (anchor_parent |> has_cls("leading-whitespace")) {
      schedule_move_or_transport'(
        0,
        anchor_parent |> JSUtil.force_get_next_sibling_elem,
      );
    };
  };
};

module Presentation = {
  let clss_of_err: ErrStatus.t => list(cls) =
    fun
    | NotInHole => []
    | InHole(_) => ["InHole"];

  let clss_of_verr: VarErrStatus.t => list(cls) =
    fun
    | NotInVarHole => []
    | InVarHole(_) => ["InVarHole"];

  let cursor_clss = (has_cursor: bool): list(cls) =>
    has_cursor ? ["Cursor"] : [];

  let family_clss: TermFamily.t => list(cls) =
    fun
    | Typ => ["Typ"]
    | Pat => ["Pat"]
    | Exp => ["Exp"];

  let shape_clss: TermShape.t => list(cls) =
    fun
    | Rule => ["Rule"]
    | Case({err}) => ["Case", ...clss_of_err(err)]
    | Var({err, verr, show_use}) =>
      ["Operand", "Var", ...clss_of_err(err)]
      @ clss_of_verr(verr)
      @ (show_use ? ["show-use"] : [])
    | Operand({err}) => ["Operand", ...clss_of_err(err)]
    | BinOp({err, op_index: _}) => ["BinOp", ...clss_of_err(err)]
    | NTuple({err, comma_indices: _}) => ["NTuple", ...clss_of_err(err)]
    | SubBlock(_) => ["SubBlock"];

  let open_child_clss = (has_inline_OpenChild: bool, has_para_OpenChild: bool) =>
    List.concat([
      has_inline_OpenChild ? ["has-Inline-OpenChild"] : [],
      has_para_OpenChild ? ["has-Para-OpenChild"] : [],
    ]);

  let has_child_clss = (has_child: bool) =>
    has_child ? ["has-child"] : ["no-children"];

  let caret_from_left = (from_left: float): Vdom.Node.t => {
    assert(0.0 <= from_left && from_left <= 100.0);
    let left_attr =
      Vdom.Attr.create(
        "style",
        "left: " ++ string_of_float(from_left) ++ "0%;",
      );
    Vdom.Node.span([Vdom.Attr.id("caret"), left_attr], []);
  };

  let caret_of_side: Side.t => Vdom.Node.t =
    fun
    | Before => caret_from_left(0.0)
    | After => caret_from_left(100.0);

  let view_of_layout = (l: TermLayout.t): Vdom.Node.t => {
    open Vdom;

    let rec go = (l: TermLayout.t): list(Node.t) =>
      switch (l) {
      | Text(str) => [Node.text(str)]
      | Cat(l1, l2) => go(l1) @ go(l2)
      | Linebreak => [Node.br([])]
      | Align(l) => [Node.div([Attr.classes(["Align"])], go(l))]

      // TODO adjust width to num digits, use visibility none
      | Annot(HoleLabel(_), l) => [
          Node.span([Attr.classes(["SEmptyHole-num"])], go(l)),
        ]

      | Annot(DelimGroup, l) => [
          Node.span([Attr.classes(["DelimGroup"])], go(l)),
        ]
      | Annot(LetLine, l) => [
          Node.span([Attr.classes(["LetLine"])], go(l)),
        ]
      | Annot(Padding, l) => [
          Node.span([Attr.classes(["Padding"])], go(l)),
        ]
      | Annot(Indent, l) => [
          Node.span([Attr.classes(["Indent"])], go(l)),
        ]

      | Annot(UserNewline, l) => [
          Node.span([Attr.classes(["UserNewline"])], go(l)),
        ]

      | Annot(OpenChild({is_inline}), l) => [
          Node.span(
            [Attr.classes(["OpenChild", is_inline ? "Inline" : "Para"])],
            go(l),
          ),
        ]
      | Annot(ClosedChild({is_inline}), l) => [
          Node.span(
            [Attr.classes(["ClosedChild", is_inline ? "Inline" : "Para"])],
            go(l),
          ),
        ]

      | Annot(EmptyLine({has_caret}), _) =>
        let children = {
          let zwsp = Node.text(UnicodeConstants.zwsp);
          has_caret ? [zwsp] : [caret_from_left(0.0), zwsp];
        };
        [Node.span([Attr.classes(["EmptyLine"])], children)];

      | Annot(Delim({caret, _}), l) =>
        let children =
          switch (caret) {
          | None => go(l)
          | Some(side) => [caret_of_side(side), ...go(l)]
          };
        [Node.span([Attr.classes(["code-delim"])], children)];

      | Annot(Op({caret}), l) =>
        let children =
          switch (caret) {
          | None => go(l)
          | Some(side) => [caret_of_side(side), ...go(l)]
          };
        [Node.span([Attr.classes(["code-op"])], children)];

      | Annot(SpaceOp, l) => go(l)

      | Annot(Text({caret, length}), l) =>
        let children =
          switch (caret) {
          | None => go(l)
          | Some(char_index) =>
            let from_left =
              if (length == 0) {
                0.0;
              } else {
                let index = float_of_int(char_index);
                let length = float_of_int(length);
                100.0 *. index /. length;
              };
            [caret_from_left(from_left), ...go(l)];
          };
        [Node.span([Attr.classes(["code-text"])], children)];

      | Annot(Step(_), l) => go(l)

      | Annot(Term({has_cursor, shape, family}), l) => [
          Node.span(
            [
              Attr.classes(
                List.concat([
                  ["Term"],
                  cursor_clss(has_cursor),
                  family_clss(family),
                  shape_clss(shape),
                  open_child_clss(
                    l |> TermLayout.has_inline_OpenChild,
                    l |> TermLayout.has_para_OpenChild,
                  ),
                  has_child_clss(l |> TermLayout.has_child),
                ]),
              ),
            ],
            go(l),
          ),
        ]
      };
    Node.div([Attr.classes(["code", "presentation"])], go(l));
  };
};

let editor_view_of_layout =
    (
      ~inject: Update.Action.t => Vdom.Event.t,
      ~path: option(CursorPath.t)=?,
      ~ci: option(CursorInfo.t)=?,
      ~show_contenteditable: bool,
      ~width: int,
      l: TermLayout.t,
    )
    : (Vdom.Node.t, Vdom.Node.t) => {
  let l =
    switch (path) {
    | None => l
    | Some((steps, _) as path) =>
      switch (l |> TermLayout.find_and_decorate_caret(~path)) {
      | None =>
        JSUtil.log(
          Js.string(Sexplib.Sexp.to_string_hum(TermLayout.sexp_of_t(l))),
        );
        failwith(__LOC__ ++ ": could not find caret");
      | Some(l) =>
        switch (l |> TermLayout.find_and_decorate_cursor(~steps)) {
        | None => failwith(__LOC__ ++ ": could not find cursor")
        | Some(l) => l
        }
      }
    };
  let l =
    switch (ci) {
    | None
    | Some({uses: None, _}) => l
    | Some({uses: Some(uses), _}) =>
      uses
      |> List.fold_left(
           (l, use) =>
             l
             |> TermLayout.find_and_decorate_var_use(~steps=use)
             |> OptUtil.get(() => {
                  failwith(
                    __LOC__
                    ++ ": could not find var use"
                    ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_steps(use)),
                  )
                }),
           l,
         )
    };
  (
    Contenteditable.view_of_layout(~inject, ~width, ~show_contenteditable, l),
    Presentation.view_of_layout(l),
  );
};

let view_of_htyp = (~width=30, ~pos=0, ty: HTyp.t): Vdom.Node.t => {
  let l =
    ty
    |> TermDoc.Typ.mk_htyp(~enforce_inline=false)
    |> LayoutOfDoc.layout_of_doc(~width, ~pos);
  switch (l) {
  | None => failwith("unimplemented: view_of_htyp on layout failure")
  | Some(l) => Presentation.view_of_layout(l)
  };
};

let editor_view_of_exp =
    (
      ~inject: Update.Action.t => Vdom.Event.t,
      ~width=80,
      ~pos=0,
      ~path: option(CursorPath.t)=?,
      ~ci: option(CursorInfo.t)=?,
      ~show_contenteditable: bool,
      e: UHExp.t,
    )
    : (Vdom.Node.t, Vdom.Node.t) => {
  let l =
    e
    |> TermDoc.Exp.mk(~enforce_inline=false)
    |> LayoutOfDoc.layout_of_doc(~width, ~pos);
  switch (l) {
  | None => failwith("unimplemented: view_of_exp on layout failure")
  | Some(l) =>
    editor_view_of_layout(
      ~inject,
      ~width,
      ~path?,
      ~ci?,
      ~show_contenteditable,
      l,
    )
  };
};
