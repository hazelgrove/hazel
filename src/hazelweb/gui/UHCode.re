module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;
open Pretty;
open ViewUtil;

let contenteditable_false = Vdom.Attr.create("contenteditable", "false");

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

let sort_clss: TermSort.t => list(cls) =
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

let _view_of_layout =
    (~inject as _: Update.Action.t => Vdom.Event.t, l: UHLayout.t)
    : (option(((int, int), CursorPath.rev_t)), CursorMap.t, Vdom.Node.t) => {
  let row = ref(0);
  let col = ref(0);
  let z = ref(None);
  let builder = CursorMap.Builder.init();

  let rec go = (~rev_steps, ~indent, l: UHLayout.t): list(Vdom.Node.t) => {
    open Vdom;
    let go' = go(~rev_steps, ~indent);
    switch (l) {
    | Text(s) =>
      col := col^ + StringUtil.utf8_length(s);
      [Node.text(s)];
    | Linebreak =>
      row := row^ + 1;
      col := indent;
      [Node.br([])];
    | Align(l) => [
        Node.div(
          [Attr.classes(["Align"])],
          go(~rev_steps, ~indent=col^, l),
        ),
      ]
    | Cat(l1, l2) =>
      let vs1 = go'(l1);
      let vs2 = go'(l2);
      vs1 @ vs2;

    | Annot(Step(step), l) =>
      go(~rev_steps=[step, ...rev_steps], ~indent, l)

    | Annot(CursorPosition({has_cursor, cursor}), _) =>
      if (has_cursor) {
        z := Some(((row^, col^), (cursor, rev_steps)));
      };
      builder |> CursorMap.Builder.add((row^, col^), (cursor, rev_steps));
      [];

    | Annot(Text({cursor}), l) =>
      let col_before = col^;
      let vs = go'(l);
      let col_after = col^;
      builder
      |> CursorMap.Builder.add(
           (row^, col_before),
           (CursorPosition.OnText(0), rev_steps),
         );
      builder
      |> CursorMap.Builder.add(
           (row^, col_after),
           (CursorPosition.OnText(col_after - col_before), rev_steps),
         );
      switch (cursor) {
      | None => ()
      | Some(j) =>
        z :=
          Some((
            (row^, col_before + j),
            (CursorPosition.OnText(j), rev_steps),
          ))
      };
      [Node.span([Attr.classes(["code-text"])], vs)];

    | Annot(EmptyLine({has_cursor}), l) =>
      if (has_cursor) {
        z := Some(((row^, col^), (CursorPosition.OnText(0), rev_steps)));
      };
      builder
      |> CursorMap.Builder.add(
           (row^, col^),
           (CursorPosition.OnText(0), rev_steps),
         );
      go'(l);

    | Annot(DelimGroup, l) => [
        Node.span([Attr.classes(["DelimGroup"])], go'(l)),
      ]
    | Annot(LetLine, l) => [
        Node.span([Attr.classes(["LetLine"])], go'(l)),
      ]

    | Annot(Padding, l) => [
        Node.span([Attr.classes(["Padding"])], go'(l)),
      ]
    | Annot(Indent, l) => [Node.span([Attr.classes(["Indent"])], go'(l))]

    | Annot(HoleLabel(_), l) => [
        Node.span([Attr.classes(["HoleLabel"])], go'(l)),
      ]
    | Annot(UserNewline, l) => [
        Node.span([Attr.classes(["UserNewline"])], go'(l)),
      ]

    | Annot(OpenChild({is_inline}), l) => [
        Node.span(
          [Attr.classes(["OpenChild", is_inline ? "Inline" : "Para"])],
          go'(l),
        ),
      ]
    | Annot(ClosedChild({is_inline}), l) => [
        Node.span(
          [Attr.classes(["ClosedChild", is_inline ? "Inline" : "Para"])],
          go'(l),
        ),
      ]

    | Annot(Delim, l) => [
        Node.span([Attr.classes(["code-delim"])], go'(l)),
      ]

    | Annot(Op, l) => [Node.span([Attr.classes(["code-op"])], go'(l))]

    | Annot(SpaceOp, l) => go'(l)

    | Annot(Term({has_cursor, shape, sort}), l) => [
        Node.span(
          [
            Attr.classes(
              List.concat([
                ["Term"],
                cursor_clss(has_cursor),
                sort_clss(sort),
                shape_clss(shape),
                open_child_clss(
                  l |> UHLayout.has_inline_OpenChild,
                  l |> UHLayout.has_para_OpenChild,
                ),
                has_child_clss(l |> UHLayout.has_child),
              ]),
            ),
          ],
          go'(l),
        ),
      ]
    };
  };

  (
    z^,
    CursorMap.Builder.build(builder),
    Vdom.(
      Node.div(
        [Attr.classes(["code", "presentation"])],
        [
          Node.div([Attr.id("caret"), Attr.classes(["blink"])], []),
          ...go(~indent=0, ~rev_steps=[], l),
        ],
      )
    ),
  );
};

let unfocused_view_of_layout =
    (~inject: Update.Action.t => Vdom.Event.t, l: UHLayout.t) => {
  let (_, cursor_map, view) = _view_of_layout(~inject, l);
  (cursor_map, view);
};

let focused_view_of_layout =
    (
      ~inject: Update.Action.t => Vdom.Event.t,
      ~path: CursorPath.t,
      ~ci: CursorInfo.t,
      l: UHLayout.t,
    ) => {
  let (steps, _) = path;
  let l =
    switch (l |> UHLayout.find_and_decorate_caret(~path)) {
    | None => failwith(__LOC__ ++ ": could not find caret")
    | Some(l) =>
      switch (l |> UHLayout.find_and_decorate_cursor(~steps)) {
      | None => failwith(__LOC__ ++ ": could not find cursor")
      | Some(l) => l
      }
    };
  let l =
    switch (ci.uses) {
    | None => l
    | Some(uses) =>
      uses
      |> List.fold_left(
           (l, use) =>
             l
             |> UHLayout.find_and_decorate_var_use(~steps=use)
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
  let (z, map, view) = _view_of_layout(~inject, l);
  let zmap = ZCursorMap.mk(~z=Option.get(z), ~map);
  (zmap, view);
};

let focused_view =
    (
      ~inject: Update.Action.t => Vdom.Event.t,
      ~width=80,
      ~pos=0,
      ~path: CursorPath.t,
      ~ci: CursorInfo.t,
      doc: UHDoc.t,
    )
    : (ZCursorMap.t, Vdom.Node.t) => {
  let l = LayoutOfDoc.layout_of_doc(~width, ~pos, doc);
  switch (l) {
  | None => failwith("unimplemented: view_of_exp on layout failure")
  | Some(l) => focused_view_of_layout(~inject, ~path, ~ci, l)
  };
};

let unfocused_view =
    (
      ~inject: Update.Action.t => Vdom.Event.t,
      ~width=80,
      ~pos=0,
      doc: UHDoc.t,
    )
    : (CursorMap.t, Vdom.Node.t) => {
  let l = LayoutOfDoc.layout_of_doc(~width, ~pos, doc);
  switch (l) {
  | None => failwith("unimplemented: view_of_exp on layout failure")
  | Some(l) => unfocused_view_of_layout(~inject, l)
  };
};
