module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;
open ViewUtil;

type tag = TermTag.t;

let contenteditable_false = Vdom.Attr.create("contenteditable", "false");

let clss_of_err: ErrStatus.t => list(cls) =
  fun
  | NotInHole => []
  | InHole(_) => ["InHole"];

let clss_of_verr: VarErrStatus.t => list(cls) =
  fun
  | NotInVarHole => []
  | InVarHole(_) => ["InVarHole"];

module Typ = {
  let clss_of_operand: UHTyp.operand => list(cls) =
    fun
    | Hole => ["Hole"]
    | Unit => ["Unit"]
    | Num => ["Num"]
    | Bool => ["Bool"]
    | Parenthesized(_) => ["Parenthesized"]
    | List(_) => ["List"];
};
module Pat = {
  let clss_of_operand: UHPat.operand => list(cls) =
    fun
    | EmptyHole(_) => ["EmptyHole"]
    | Wild(err) => ["Wild", ...clss_of_err(err)]
    | Var(err, verr, _) => [
        "Var",
        ...clss_of_err(err) @ clss_of_verr(verr),
      ]
    | NumLit(err, _) => ["NumLit", ...clss_of_err(err)]
    | BoolLit(err, _) => ["BoolLit", ...clss_of_err(err)]
    | ListNil(err) => ["ListNil", ...clss_of_err(err)]
    | Parenthesized(_) => ["Parenthesized"]
    | Inj(err, _, _) => ["Inj", ...clss_of_err(err)];
};
module Exp = {
  let clss_of_operand: UHExp.operand => list(cls) =
    fun
    | EmptyHole(_) => ["EmptyHole"]
    | Var(err, verr, _) => [
        "Var",
        ...clss_of_err(err) @ clss_of_verr(verr),
      ]
    | NumLit(err, _) => ["NumLit", ...clss_of_err(err)]
    | BoolLit(err, _) => ["BoolLit", ...clss_of_err(err)]
    | ListNil(err) => ["ListNil", ...clss_of_err(err)]
    | Lam(err, _, _, _) => ["Lam", ...clss_of_err(err)]
    | Parenthesized(_) => ["Parenthesized"]
    | Inj(err, _, _) => ["Inj", ...clss_of_err(err)]
    | Case(err, _, _, _) => ["Case", ...clss_of_err(err)]
    | ApPalette(_) => failwith(__LOC__ ++ ":unimplemented");
};

let cursor_clss = (has_cursor: bool): list(cls) =>
  has_cursor ? ["Cursor"] : [];

let family_clss: TermTag.term_family => list(cls) =
  fun
  | Typ => ["Typ"]
  | Pat => ["Pat"]
  | Exp => ["Exp"];

let shape_clss: TermTag.term_shape => list(cls) =
  fun
  | Rule => ["Rule"]
  | Operand({err, verr}) =>
    ["Operand", ...clss_of_err(err)] @ clss_of_verr(verr)
  | BinOp({err, op_index: _}) => ["BinOp", ...clss_of_err(err)]
  | NTuple({err, comma_indices: _}) => ["NTuple", clss_of_err(err)]
  | SubBlock(_) => ["SubBlock"];

let open_child_clss = (has_inline_OpenChild: bool, has_para_OpenChild: bool) =>
  List.concat([
    has_inline_OpenChild ? ["has-Inline-OpenChild"] : [],
    has_para_OpenChild ? ["has-Para-OpenChild"] : [],
  ]);

let on_click_noneditable =
    (
      ~inject: Update.Action.t => Vdom.Event.t,
      path_before: CursorPath.t,
      path_after: CursorPath.t,
      evt,
    )
    : Vdom.Event.t =>
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

let on_click_text =
    (
      ~inject: Update.Action.t => Vdom.Event.t,
      steps: CursorPath.steps,
      length: int,
      evt,
    )
    : Vdom.Event.t =>
  switch (Js.Opt.to_option(evt##.target)) {
  | None => inject(Update.Action.EditAction(MoveToBefore(steps)))
  | Some(target) =>
    let from_left =
      float_of_int(evt##.clientX) -. target##getBoundingClientRect##.left;
    let from_right =
      target##getBoundingClientRect##.right -. float_of_int(evt##.clientX);
    let char_index =
      floor(
        from_left /. (from_left +. from_right) *. float_of_int(length) +. 0.5,
      )
      |> int_of_float;
    inject(Update.Action.EditAction(MoveTo((steps, OnText(char_index)))));
  };

let caret_from_left = (from_left: float): Vdom.Node.t => {
  assert(0.0 <= from_left && from_left <= 100.0);
  let left_attr =
    Vdom.Attr.create(
      "style",
      "left: " ++ string_of_float(from_left) ++ "0%;",
    );
  Vdom.Node.span(
    [Vdom.Attr.id("caret"), contenteditable_false, left_attr],
    [],
  );
};

let caret_of_side: Side.t => Vdom.Node.t =
  fun
  | Before => caret_from_left(0.0)
  | After => caret_from_left(100.0);

let contenteditable_of_layout = (l: TermLayout.t): Vdom.Node.t => {
  open Vdom;
  let caret_position = (path: CursorPath.t): Node.t =>
    Node.span(
      [Attr.id(path_id(path))],
      [Node.text(LangUtil.nondisplay1)],
    );
  let record: Layout.text(tag, list(Node.t), Node.t) = {
    /* All DOM text nodes are expected to be wrapped in an
     * element either with contenteditable set to false or
     * tagged with the appropriate path-related metadata.
     * cf SelectionChange clause in Update.apply_action
     */
    imp_of_tag: (tag, vs) =>
      switch (tag) {
      | Delim({path: (steps, index), _}) =>
        let path_before: CursorPath.t = (steps, OnDelim(index, Before));
        let path_after: CursorPath.t = (steps, OnDelim(index, After));
        [
          caret_position(path_before),
          Node.span([contenteditable_false], vs),
          caret_position(path_after),
        ];
      | Op({steps, _}) =>
        let path_before: CursorPath.t = (steps, OnOp(Before));
        let path_after: CursorPath.t = (steps, OnOp(After));
        [
          caret_position(path_before),
          Node.span([contenteditable_false], vs),
          caret_position(path_after),
        ];
      | SpaceOp => [
          Node.span([contenteditable_false, Attr.classes(["SpaceOp"])], vs),
        ]
      | Text({steps, _}) => [Node.span([Attr.id(text_id(steps))], vs)]
      | Padding => [
          Node.span([contenteditable_false, Attr.classes(["Padding"])], vs),
        ]
      | Indent => [
          Node.span([contenteditable_false, Attr.classes(["Indent"])], vs),
        ]
      | OpenChild(_)
      | ClosedChild(_)
      | HoleLabel(_)
      | DelimGroup
      | Step(_)
      | Term(_) => vs
      },
    imp_append: (vs1, vs2) => vs1 @ vs2,
    imp_of_string: str => [Node.text(str)],
    imp_newline: indent => [
      Node.span(
        [contenteditable_false],
        [
          Node.br([]),
          Node.span(
            [Attr.create("style", "white-space: pre;")],
            [
              Node.text(
                String.concat(
                  "",
                  GeneralUtil.replicate(indent, LangUtil.nbsp1),
                ),
              ),
            ],
          ),
        ],
      ),
    ],
    t_of_imp: vs => Node.span([Attr.classes(["code"])], vs) // TODO: use something other than `span`?
  };
  Layout.make_of_layout(record, l);
};

let caret_position_of_path =
    ((steps, cursor) as path: CursorPath.t): (Js.t(Dom.node), int) =>
  switch (cursor) {
  | OnOp(side)
  | OnDelim(_, side) =>
    let anchor_parent = (
      JSUtil.force_get_elem_by_id(path_id(path)): Js.t(Dom_html.element) :>
        Js.t(Dom.node)
    );
    (
      Js.Opt.get(anchor_parent##.firstChild, () =>
        failwith(__LOC__ ++ ": Found caret position without child text")
      ),
      switch (side) {
      | Before => 1
      | After => 0
      },
    );
  | OnText(j) =>
    let anchor_parent = (
      JSUtil.force_get_elem_by_id(text_id(steps)): Js.t(Dom_html.element) :>
        Js.t(Dom.node)
    );
    (
      Js.Opt.get(anchor_parent##.firstChild, () =>
        failwith(__LOC__ ++ ": Found Text node without child text")
      ),
      j,
    );
  };

let presentation_of_layout =
    (~inject: Update.Action.t => Vdom.Event.t, l: TermLayout.t): Vdom.Node.t => {
  open Vdom;

  let on_click_noneditable = on_click_noneditable(~inject);
  let on_click_text = on_click_text(~inject);

  let rec go = (l: TermLayout.t): list(Node.t) =>
    switch (l) {
    | Text(str) => [Node.text(str)]
    | Cat(l1, l2) => go(l1) @ go(l2)
    | Linebreak => [Node.br([])]
    | Align(l) => [Node.div([Attr.classes(["Align"])], go(l))]

    // TODO adjust width to num digits, use visibility none
    | Tagged(HoleLabel(_), l) => [
        Node.span([Attr.classes(["SEmptyHole-num"])], go(l)),
      ]

    | Tagged(DelimGroup, l) => [
        Node.span([Attr.classes(["DelimGroup"])], go(l)),
      ]
    | Tagged(Padding, l) => [
        Node.span(
          [contenteditable_false, Attr.classes(["Padding"])],
          go(l),
        ),
      ]
    | Tagged(Indent, l) => [
        Node.span(
          [contenteditable_false, Attr.classes(["Indent"])],
          go(l),
        ),
      ]

    | Tagged(OpenChild({is_inline}), l) => [
        Node.span(
          [Attr.classes(["OpenChild", is_inline ? "Inline" : "Para"])],
          go(l),
        ),
      ]
    | Tagged(ClosedChild({is_inline}), l) => [
        Node.span(
          [Attr.classes(["ClosedChild", is_inline ? "Inline" : "Para"])],
          go(l),
        ),
      ]

    | Tagged(Delim({path: (steps, delim_index), caret}), l) =>
      let attrs = {
        let path_before: CursorPath.t = (
          steps,
          OnDelim(delim_index, Before),
        );
        let path_after: CursorPath.t = (steps, OnDelim(delim_index, After));
        [
          Attr.on_click(on_click_noneditable(path_before, path_after)),
          Attr.classes(["code-delim"]),
        ];
      };
      let children =
        switch (caret) {
        | None => go(l)
        | Some(side) => [caret_of_side(side), ...go(l)]
        };
      [Node.span(attrs, children)];

    | Tagged(Op({steps, caret}), l) =>
      let attrs = {
        let path_before: CursorPath.t = (steps, OnOp(Before));
        let path_after: CursorPath.t = (steps, OnOp(After));
        [
          Attr.on_click(on_click_noneditable(path_before, path_after)),
          Attr.classes(["code-op"]),
        ];
      };
      let children =
        switch (caret) {
        | None => go(l)
        | Some(side) => [caret_of_side(side), ...go(l)]
        };
      [Node.span(attrs, children)];

    | Tagged(SpaceOp(_), l) => go(l)

    | Tagged(Text({caret, length, steps}), l) =>
      let attrs = [
        Attr.on_click(on_click_text(steps, length)),
        Attr.classes(["code-text"]),
      ];
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
      [Node.span(attrs, children)];

    | Tagged(Step(_), l) => go(l)

    | Tagged(Term({has_cursor, shape, family}), l) => [
        Node.span(
          [
            Attr.classes(
              List.concat([
                ["Term"],
                cursor_clss(has_cursor),
                family_clss(family),
                shape_clss(shape),
                open_child_clss(
                  l |> has_inline_OpenChild,
                  l |> has_para_OpenChild,
                ),
              ]),
            ),
          ],
          go(l),
        ),
      ]
    };
  Node.div([Attr.classes(["code"])], go(l));
};

let editor_view_of_layout =
    (
      ~inject: Update.Action.t => Vdom.Event.t,
      ~decorate_cursor: option(CursorPath.t)=?,
      l: TermLayout.t,
    )
    : (Vdom.Node.t, Vdom.Node.t) => {
  let l =
    switch (decorate_cursor) {
    | None => l
    | Some((steps, _) as path) =>
      l
      |> TermLayout.find_and_decorate_caret(~path)
      |> TermLayout.find_and_decorate_cursor(~steps)
    };
  (contenteditable_of_layout(l), presentation_of_layout(~inject, l));
};

let view_of_htyp =
    (~inject: Update.Action.t => Vdom.Event.t, ~width=20, ~pos=0, ty: HTyp.t)
    : Vdom.Node.t => {
  let l =
    ty
    |> DocOfTerm.Typ.doc_of_htyp(~steps=[], ~enforce_inline=false)
    |> LayoutOfDoc.layout_of_doc(~width, ~pos);
  switch (l) {
  | None => failwith("unimplemented: view_of_htyp on layout failure")
  | Some(l) => presentation_of_layout(~inject, l)
  };
};

let editor_view_of_exp =
    (
      ~inject: Update.Action.t => Vdom.Event.t,
      ~width=80,
      ~pos=0,
      ~decorate_cursor: option(CursorPath.t)=?,
      e: UHExp.t,
    )
    : (Vdom.Node.t, Vdom.Node.t) => {
  let l =
    e
    |> DocOfTerm.Exp.doc(~steps=[], ~enforce_inline=false)
    |> LayoutOfDoc.layout_of_doc(~width, ~pos);
  switch (l) {
  | None => failwith("unimplemented: view_of_exp on layout failure")
  | Some(l) => editor_view_of_layout(~inject, ~decorate_cursor?, l)
  };
};
