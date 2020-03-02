module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;
open Pretty;
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

  let merge_text_nodes = (vs: list(Vdom.Node.t)): list(Vdom.Node.t) => {
    vs
    |> List.fold_left(
         ((text_so_far, rev_merged), v) => {
           switch (text_so_far, v) {
           | (None, Vdom.Node.Text(s)) => (Some(s), rev_merged)
           | (Some(text_so_far), Text(s)) => (
               Some(text_so_far ++ s),
               rev_merged,
             )
           | (None, _) => (None, [v, ...rev_merged])
           | (Some(text_so_far), _) => (
               None,
               [v, Vdom.Node.Text(text_so_far), ...rev_merged],
             )
           }
         },
         (None, []),
       )
    |> (
      fun
      | (None, vs) => vs
      | (Some(text), vs) => [Vdom.Node.Text(text), ...vs]
    )
    |> List.rev;
  };

  let tag_rows = (vs: list(Vdom.Node.t)): list(Vdom.Node.t) => {
    open Vdom;
    let tagged_row = vs => Node.span([Attr.classes(["code-row"])], vs);
    vs
    |> List.fold_left(
         ((rev_untagged_row, rev_tagged_rows), node: Vdom.Node.t) =>
           switch (node) {
           | Element(elem) when Node.Element.tag(elem) == "br" =>
             let new_tagged_row = tagged_row(List.rev(rev_untagged_row));
             ([], [new_tagged_row, ...rev_tagged_rows]);
           | _ => ([node, ...rev_untagged_row], rev_tagged_rows)
           },
         ([], []),
       )
    |> (
      fun
      | ([], rev_rows) => rev_rows
      | ([_, ..._] as rev_untagged_row, rev_rows) => {
          let last_row = tagged_row(List.rev(rev_untagged_row));
          [last_row, ...rev_rows];
        }
    )
    |> List.rev;
  };

  let view_of_layout =
      (
        ~inject: Update.Action.t => Vdom.Event.t,
        ~show_contenteditable: bool,
        l: TermLayout.t,
      )
      : Vdom.Node.t => {
    open Vdom;
    let row = ref(0);
    let col = ref(0);
    let rec go = (~rev_steps, ~indent, l: TermLayout.t): list(Vdom.Node.t) => {
      let go' = go(~rev_steps, ~indent);
      switch (l) {
      | Text(s) =>
        col := col^ + StringUtil.utf8_length(s);
        [Node.text(s)];
      | Linebreak =>
        row := row^ + 1;
        col := indent;
        [
          Node.br([]),
          Node.text(StringUtil.replicat(indent, UnicodeConstants.nbsp)),
        ];
      | Align(l) => go(~rev_steps, ~indent=col^, l)
      | Cat(l1, l2) =>
        let vs1 = go'(l1);
        let vs2 = go'(l2);
        vs1 @ vs2;
      | Annot(Step(step), l) =>
        go(~rev_steps=[step, ...rev_steps], ~indent, l)
      | Annot(Delim({index, _}), l) =>
        let col_before = col^;
        let vs = go'(l);
        let col_after = col^;
        CaretMap.add(
          (row^, col_before),
          (CursorPosition.OnDelim(index, Before), rev_steps),
        );
        CaretMap.add(
          (row^, col_after),
          (CursorPosition.OnDelim(index, After), rev_steps),
        );
        [Node.span([contenteditable_false], vs)];
      | Annot(Op(_), l) =>
        let col_before = col^;
        let vs = go'(l);
        let col_after = col^;
        CaretMap.add(
          (row^, col_before),
          (CursorPosition.OnOp(Before), rev_steps),
        );
        CaretMap.add(
          (row^, col_after),
          (CursorPosition.OnOp(After), rev_steps),
        );
        [Node.span([contenteditable_false], vs)];
      | Annot(Text(_), l) =>
        let col_before = col^;
        let vs = go'(l);
        let col_after = col^;
        CaretMap.add(
          (row^, col_before),
          (CursorPosition.OnText(0), rev_steps),
        );
        CaretMap.add(
          (row^, col_after),
          (CursorPosition.OnText(col_after - col_before), rev_steps),
        );
        vs;
      | Annot(EmptyLine(_), l) =>
        CaretMap.add((row^, col^), (CursorPosition.OnText(0), rev_steps));
        go'(l);
      | Annot(_, l) => go'(l)
      };
    };
    let vs =
      l |> go(~indent=0, ~rev_steps=[]) |> merge_text_nodes |> tag_rows;
    Node.div(
      [
        Attr.id("contenteditable"),
        Attr.classes(
          ["code", "contenteditable"]
          @ (
            if (show_contenteditable) {
              [];
            } else {
              ["hiddencontenteditable"];
            }
          ),
        ),
        Attr.create("contenteditable", "true"),
        Attr.on("drop", _ => Event.Prevent_default),
        Attr.on_focus(_ => inject(Update.Action.FocusCell)),
        Attr.on_blur(_ => inject(Update.Action.BlurCell)),
      ],
      vs,
    );
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

  let caret_from_left = (from_left: float): Vdom.Node.t => {
    assert(0.0 <= from_left && from_left <= 100.0);
    open Vdom;
    let left_attr =
      Attr.create("style", "left: " ++ string_of_float(from_left) ++ "0%;");
    Node.span([Attr.id("caret"), Attr.classes(["blink"]), left_attr], []);
  };

  let caret_of_side: Side.t => Vdom.Node.t =
    fun
    | Before => caret_from_left(0.0)
    | After => caret_from_left(100.0);

  let restart_caret_animation = () => {
    let caret = JSUtil.force_get_elem_by_id("caret");
    caret##.classList##remove(Js.string("blink"));
    let _ = caret##getBoundingClientRect;
    caret##.classList##add(Js.string("blink"));
  };

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
          has_caret ? [caret_from_left(0.0), zwsp] : [zwsp];
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
      l: TermLayout.t,
    )
    : (Vdom.Node.t, Vdom.Node.t) => {
  let l =
    switch (path) {
    | None => l
    | Some((steps, _) as path) =>
      switch (l |> TermLayout.find_and_decorate_caret(~path)) {
      | None => failwith(__LOC__ ++ ": could not find caret")
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
    Contenteditable.view_of_layout(~inject, ~show_contenteditable, l),
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
    editor_view_of_layout(~inject, ~path?, ~ci?, ~show_contenteditable, l)
  };
};
