module Js = Js_of_ocaml.Js;
module Vdom = Virtual_dom.Vdom;
open GeneralUtil;
open ViewUtil;
open SemanticsCommon;

let contenteditable_of_layout: Layout.t('tag) => Vdom.Node.t =
  layout => {
    let record: Layout.text('tag, list(Vdom.Node.t), Vdom.Node.t) = {
      imp_of_string: string => [Vdom.Node.text(string)],
      imp_of_tag: (_, string) => [Vdom.Node.span([], string)], // TODO: add span data
      imp_append: (s1, s2) => s1 @ s2,
      imp_newline: [Vdom.Node.br([])],
      t_of_imp: s => Vdom.Node.span([], s) // TODO: use something other than `span`?
    };
    Layout.make_of_layout(record, layout);
  };

type term_shape =
  | TypOperand(UHTyp.operand)
  | TypSkel(UHTyp.skel)
  | PatOperand(UHPat.operand)
  | PatSkel(UHPat.skel)
  | ExpOperand(UHExp.operand)
  | ExpRule(UHExp.rule)
  | ExpSkel(UHExp.skel)
  | ExpSubBlock(UHExp.line);

type tag =
  | DelimGroup
  | Padding({
      path_before: CursorPath.t,
      path_after: CursorPath.t,
    })
  | Delim({
      path: delim_path,
      caret: option(Side.t),
    })
  | Op({
      steps: CursorPath.steps,
      caret: option(Side.t),
    })
  | Text({
      caret: option(Side.t),
      length: int,
    })
  | Term({
      shape: term_shape,
      has_cursor: bool,
    });

module Vdom = Virtual_dom.Vdom;
module Node = Vdom.Node;
module Attr = Vdom.Attr;

let view = (~inject: Action.t => Vdom.Event.t): Node.t => {
  let on_click_noneditable =
      (path_before: CursorPath.t, path_after: CursorPath.t, evt) =>
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

  let on_click_text = (steps: CursorPath.steps, length: int, evt) =>
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
        );
      inject(
        Update.Action.EditAction(MoveTo((steps, OnText(char_index)))),
      );
    };

  let caret_from_left = (from_left: float): Node.t => {
    assert(0.0 <= from_left && from_left <= 100.0);
    let left_attr =
      Attr.create("style", "left: " ++ string_of_float(from_left) ++ "%;");
    Node.span([Attr.id("caret"), left_attr], []);
  };

  let caret_of_side: Side.t => Node.t =
    fun
    | Before => caret_from_left(0.0)
    | After => caret_from_left(100.0);

  let contenteditable_of_layout: Layout.t(tag) => Node.t =
    layout => {
      let caret_position = (path: CursorPath.t): Node.t =>
        Node.span([Attr.id(path_id(path))], [Node.text(nondisplay1)]);
      let record: Layout.text('tag, list(Node.t), Node.t) = {
        imp_of_string: str => [Node.text(str)],
        imp_of_tag: (tag, vs) =>
          switch (tag) {
          | Delim({path: (steps, delim_index), _}) =>
            let path_before = (steps, OnDelim(delim_index, Before));
            let path_after = (steps, OnDelim(delim_index, After));
            [caret_position(path_before)]
            @ [Node.span([Attr.create("contenteditable", "false")], vs)]
            @ [caret_position(path_after)];
          | Op({steps, _}) =>
            let path_before = (steps, OnOp(Before));
            let path_after = (steps, OnOp(After));
            [caret_position(path_before)]
            @ [Node.span([Attr.create("contenteditable", "false")], vs)]
            @ [caret_position(path_after)];
          | DelimGroup
          | Padding(_)
          | Text(_)
          | Term(_) => vs
          },
        imp_append: (vs1, vs2) => vs1 @ vs2,
        imp_newline: [Node.br([])],
        t_of_imp: vs => Node.span([], vs) // TODO: use something other than `span`?
      };
      Layout.make_of_layout(record, layout);
    };

  let view_of_layout = (l: Layout.t(tag)): Node.t => {
    let go = (l: Layout.t(_)): list(Node.t) =>
      switch (l) {
      | Text(str) => [Node.text(str)]
      | Cat(l1, l2) => go(l1) @ go(l2)
      | Linebreak => [Node.br([])]
      | Align(l) => [Node.div([Attr.classes(["Align"])], go(l))]

      | Tagged(DelimGroup, l) => [
          Node.span([Attr.classes(["DelimGroup"])], go(l)),
        ]

      | Tagged(Padding({path_before, path_after}), l) => [
          Node.span(
            [Attr.on_click(on_click_noneditable(path_before, path_after))],
            go(l),
          ),
        ]

      | Tagged(Delim({path: (steps, delim_index), caret})) =>
        let attrs = {
          let path_before = (steps, OnDelim(delim_index, Before));
          let path_after = (steps, OnDelim(delim_index, After));
          [Attr.on_click(on_click_noneditable(path_before, path_after))];
        };
        let children =
          switch (caret) {
          | None => go(l)
          | Some(side) => [caret_of_side(side), ...go(l)]
          };
        [Node.span(attrs, children)];

      | Tagged(Op({steps, caret}), l) =>
        let attrs = {
          let path_before = (steps, OnOp(Before));
          let path_after = (steps, OnOp(After));
          [Attr.on_click(on_click_noneditable(path_before, path_after))];
        };
        let children =
          switch (caret) {
          | None => go(l)
          | Some(side) => [caret_of_side(side), ...go(l)]
          };
        [Node.span(attrs, children)];

      | Tagged(Text({caret, length}), l) =>
        let attrs = [Attr.on_click(on_click_text(steps, length))];
        let children =
          switch (caret) {
          | None => go(l)
          | Some(char_index) =>
            let index = float_of_int(char_index);
            let length = float_of_int(length);
            [caret_from_left(100.0 *. (index /. length)), ...go(l)];
          };
        [Node.span(attrs, children)];

      | Tagged(Term({has_cursor, _}), l) =>
        let attrs = has_cursor ? [Attr.classes("cursor")] : [];
        [Node.span(attrs, go(l))];
      };
    Node.div([], go(l));
  };

  Node.div([], []);
};
