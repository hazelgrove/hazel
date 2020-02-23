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

    /*
     let on_click_noneditable =
         (
           ~cursor_before: CursorPosition.t,
           ~cursor_after: CursorPosition.t,
           ~rev_steps: CursorPath.rev_steps,
           evt,
         )
         : Vdom.Event.t => {
       let steps = rev_steps |> List.rev;
       let path_before = (steps, cursor_before);
       let path_after = (steps, cursor_after);
       switch (Js.Opt.to_option(evt##.target)) {
       | None => inject(Update.Action.EditAction(MoveTo(path_before)))
       | Some(target) =>
         let from_left =
           float_of_int(evt##.clientX) -. target##getBoundingClientRect##.left;
         let from_right =
           target##getBoundingClientRect##.right -. float_of_int(evt##.clientX);
         let path = from_left <= from_right ? path_before : path_after;
         inject(Update.Action.EditAction(MoveTo(path)));
       };
     };

     let on_click_text =
         (~rev_steps: CursorPath.rev_steps, ~length: int, evt): Vdom.Event.t => {
       let steps = rev_steps |> List.rev;
       switch (Js.Opt.to_option(evt##.target)) {
       | None => inject(Update.Action.EditAction(MoveToBefore(steps)))
       | Some(target) =>
         let from_left =
           float_of_int(evt##.clientX) -. target##getBoundingClientRect##.left;
         let from_right =
           target##getBoundingClientRect##.right -. float_of_int(evt##.clientX);
         let char_index =
           floor(
             from_left
             /. (from_left +. from_right)
             *. float_of_int(length)
             +. 0.5,
           )
           |> int_of_float;
         inject(
           Update.Action.EditAction(MoveTo((steps, OnText(char_index)))),
         );
       };
     };
     */

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
          0,
        )
      | OnOp(side) => (
          Printf.sprintf(
            ".code-op .caret-position.%s",
            Side.to_string(side),
          ),
          0,
        )
      };
    let selector =
      Js.string(
        StringUtil.cat(
          (steps |> List.map(step => Printf.sprintf("[step=%d]", step)))
          @ [cursor_selector],
        ),
      );
    let anchor_parent = (
      JSUtil.force_get_elem_by_id("contenteditable")
      |> JSUtil.force_query_selector(selector):
        Js.t(Dom_html.element) :>
        Js.t(Dom.node)
    );
    let anchor_elem =
      Js.Opt.get(anchor_parent##.firstChild, () => assert(false));
    (anchor_elem, anchor_offset);
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
