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

let shape_clss: UHAnnot.term_shape => list(cls) =
  fun
  | Rule => ["Rule"]
  | Case({err}) => ["Case", ...clss_of_err(err)]
  | Var({err, verr, show_use}) =>
    ["Operand", "Var", ...clss_of_err(err)]
    @ clss_of_verr(verr)
    @ (show_use ? ["show-use"] : [])
  | Operand({err}) => ["Operand", ...clss_of_err(err)]
  | FreeLivelit => ["FreeLivelit"]
  | ApLivelit(_) => ["ApLivelit"]
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
    [
      Vdom.Attr.id("caret"),
      contenteditable_false,
      left_attr,
      Vdom.Attr.classes(["blink"]),
    ],
    [],
  );
};

let caret_of_side: Side.t => Vdom.Node.t =
  fun
  | Before => caret_from_left(0.0)
  | After => caret_from_left(100.0);

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

    | Annot(Term({shape: ApLivelit({lln, llview, splice_docs}), _}), _) =>
      switch (llview) {
      | Inline(view, _) => [
          Node.span([], [Node.span([], [Node.text(lln), view])]),
        ]
      | MultiLine(vdom_with_splices) =>
        let rec fill_splices =
                (vdom_with_splices: Livelits.VdomWithSplices.t): Vdom.Node.t => {
          switch (vdom_with_splices) {
          | NewSpliceFor(splice_name) =>
            let splice_doc_opt = NatMap.lookup(splice_docs, splice_name);
            switch (splice_doc_opt) {
            | None =>
              failwith("Invalid splice name " ++ string_of_int(splice_name))
            | Some(_splice_doc) =>
              // TODO splice recursion should respect the contenteditable vs presentation
              // distinction - this only does contenteditable

              /* TODO restore
                 view(
                   ~inject,
                   ~show_contenteditable,
                   ~width,
                   ~pos,
                   splice_doc,
                 ),
                 */
              Vdom.Node.div([], [])
            // ^ @d: not an issue anymore
            };
          | Bind(vdom_with_splices, f) =>
            let vdom = fill_splices(vdom_with_splices);
            fill_splices(f(vdom));
          | Ret(vdom) => vdom
          };
        };
        [fill_splices(vdom_with_splices)];
      }

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
