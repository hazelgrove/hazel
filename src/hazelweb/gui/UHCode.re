module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;
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
  | ApLivelit => ["ApLivelit"]
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
  Vdom.Node.span(
    [Vdom.Attr.id("caret"), left_attr, Vdom.Attr.classes(["blink"])],
    [],
  );
};

let view =
    (~inject as _: Update.Action.t => Vdom.Event.t, l: UHLayout.t)
    : Vdom.Node.t => {
  open Vdom;
  let rec go: UHLayout.t => _ =
    fun
    | Text(s) => StringUtil.is_empty(s) ? [] : [Node.text(s)]
    | Linebreak => [Node.br([])]
    | Align(l) => [Node.div([Attr.classes(["Align"])], go(l))]
    | Cat(l1, l2) => go(l1) @ go(l2)

    | Annot(Step(_) | EmptyLine | SpaceOp, l) => go(l)

    | Annot(Token({shape, len, has_cursor}), l) => {
        let clss =
          switch (shape) {
          | Text => ["code-text"]
          | Op => ["code-op"]
          | Delim(_) => ["code-delim"]
          };
        let children =
          switch (has_cursor) {
          | None => go(l)
          | Some(j) => [
              caret_from_left(
                len == 0
                  ? 0.0 : float_of_int(j) /. float_of_int(len) *. 100.0,
              ),
              ...go(l),
            ]
          };
        [Node.span([Attr.classes(clss)], children)];
      }

    | Annot(DelimGroup, l) => [
        Node.span([Attr.classes(["DelimGroup"])], go(l)),
      ]
    | Annot(LetLine, l) => [
        Node.span([Attr.classes(["LetLine"])], go(l)),
      ]

    | Annot(Padding, l) => [
        Node.span([Attr.classes(["Padding"])], go(l)),
      ]
    | Annot(Indent, l) => [Node.span([Attr.classes(["Indent"])], go(l))]

    | Annot(HoleLabel(_), l) => [
        Node.span([Attr.classes(["HoleLabel"])], go(l)),
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

    | Annot(LivelitView({llview, splice_docs}), _) =>
      switch (llview) {
      | Inline(view, _) => [view]
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
          go(l),
        ),
      ];

  Node.div([Attr.classes(["code", "presentation"])], go(l));
};
