module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;
open ViewUtil;

type tag = TermTag.t;

let contenteditable_false = Vdom.Attr.create("contenteditable", "false");

let term_attrs =
    (has_cursor: bool, shape: TermTag.term_shape): list(Vdom.Attr.t) => {
  let has_cursor_clss = has_cursor ? ["cursor"] : [];
  let shape_clss =
    switch (shape) {
    | TypOperand(Hole)
    | PatOperand(EmptyHole(_))
    | ExpOperand(EmptyHole(_)) => ["EmptyHole"]
    | _ => []
    };
  [Vdom.Attr.classes(has_cursor_clss @ shape_clss)];
};

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

let contenteditable_of_layout = (l: Layout.t(tag)): Vdom.Node.t => {
  open Vdom;
  let caret_position = (path: CursorPath.t): Node.t =>
    Node.span(
      [Attr.id(path_id(path))],
      [Node.text(LangUtil.nondisplay1)],
    );
  let record: Layout.text(tag, list(Node.t), Node.t) = {
    imp_of_string: str => [Node.text(str)],
    imp_of_tag: (tag, vs) =>
      switch (tag) {
      | Delim({path: (steps, delim_index), _}) =>
        let path_before: CursorPath.t = (
          steps,
          OnDelim(delim_index, Before),
        );
        let path_after: CursorPath.t = (steps, OnDelim(delim_index, After));
        [caret_position(path_before)]
        @ [Node.span([contenteditable_false], vs)]
        @ [caret_position(path_after)];
      | Op({steps, _}) =>
        let path_before: CursorPath.t = (steps, OnOp(Before));
        let path_after: CursorPath.t = (steps, OnOp(After));
        [caret_position(path_before)]
        @ [Node.span([contenteditable_false], vs)]
        @ [caret_position(path_after)];
      | Text({steps, _}) => [Node.span([Attr.id(text_id(steps))], vs)]
      | Padding => [Node.span([contenteditable_false], vs)]
      | HoleLabel
      | DelimGroup
      | Term(_) => vs
      },
    imp_append: (vs1, vs2) => vs1 @ vs2,
    imp_newline: indent => [
      Node.br([]),
      Node.span(
        [contenteditable_false],
        [Node.text(String.make(indent, ' '))],
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

let exp_cursor_before = _ =>
  Vdom.(
    Node.svg(
      "svg",
      [
        Attr.id("cursor-before"),
        Attr.classes(["exp"]),
        Attr.create("viewBox", "0 0 1 1"),
      ],
      [Node.svg("polygon", [Attr.create("points", "0,0 1,0 0,1")], [])],
    )
  );
let exp_cursor_after = _ =>
  Vdom.(
    Node.svg(
      "svg",
      [
        Attr.id("cursor-after"),
        Attr.classes(["exp"]),
        Attr.create("viewBox", "0 0 1 1"),
      ],
      [Node.svg("polygon", [Attr.create("points", "1,1 1,0 0,1")], [])],
    )
  );

let presentation_of_layout =
    (~inject: Update.Action.t => Vdom.Event.t, l: Layout.t(tag)): Vdom.Node.t => {
  open Vdom;

  let on_click_noneditable = on_click_noneditable(~inject);
  let on_click_text = on_click_text(~inject);

  let rec go = (l: Layout.t(tag)): list(Node.t) =>
    switch (l) {
    | Text(str) => [Node.text(str)]
    | Cat(l1, l2) => go(l1) @ go(l2)
    | Linebreak => [Node.br([])]
    | Align(l) => [Node.div([Attr.classes(["Align"])], go(l))]

    | Tagged(HoleLabel, l) => [
        Node.span([Attr.classes(["SEmptyHole-num"])], go(l)),
      ]

    | Tagged(DelimGroup, l) => [
        Node.span([Attr.classes(["DelimGroup"])], go(l)),
      ]

    | Tagged(Padding, l) => go(l)
    /*
     | Tagged(Padding({path_before, path_after}), l) => [
         Node.span(
           [Attr.on_click(on_click_noneditable(path_before, path_after))],
           go(l),
         ),
       ]
     */

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

    | Tagged(Term({has_cursor, shape}), l) =>
      let children =
        has_cursor
          ? [exp_cursor_before(shape), ...go(l)]
            @ [exp_cursor_after(shape)]
          : go(l);
      [Node.span(term_attrs(has_cursor, shape), children)];
    };
  Node.div([Attr.classes(["code"])], go(l));
};

let editor_view_of_layout =
    (~inject: Update.Action.t => Vdom.Event.t, l: Layout.t(tag))
    : (Vdom.Node.t, Vdom.Node.t) => (
  contenteditable_of_layout(l),
  presentation_of_layout(~inject, l),
);

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
    (~inject: Update.Action.t => Vdom.Event.t, ~width=80, ~pos=0, e: UHExp.t)
    : (Vdom.Node.t, Vdom.Node.t) => {
  let l =
    e
    |> DocOfTerm.Exp.doc(~steps=[], ~enforce_inline=false)
    |> LayoutOfDoc.layout_of_doc(~width, ~pos);
  switch (l) {
  | None => failwith("unimplemented: view_of_exp on layout failure")
  | Some(l) => editor_view_of_layout(~inject, l)
  };
};

let editor_view_of_zexp =
    (~inject: Update.Action.t => Vdom.Event.t, ~width=80, ~pos=0, ze: ZExp.t)
    : (Vdom.Node.t, Vdom.Node.t) => {
  let l =
    ze
    |> DocOfTerm.Exp.doc_of_z(~steps=[], ~enforce_inline=false)
    |> LayoutOfDoc.layout_of_doc(~width, ~pos);
  switch (l) {
  | None => failwith("unimplemented: view_of_zexp on layout failure")
  | Some(l) => editor_view_of_layout(~inject, l)
  };
};
