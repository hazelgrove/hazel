module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;
open ViewUtil;

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
    (
      ~inject,
      ~font_metrics: FontMetrics.t,
      (l, splice_ls): UHLayout.with_splices,
    )
    : Vdom.Node.t => {
  open Vdom;

  let on_mousedown =
      (~id: string, ~splice: option((MetaVar.t, SpliceName.t)), evt) => {
    let container_rect =
      JSUtil.force_get_elem_by_id(id)##getBoundingClientRect;
    let (target_x, target_y) = (
      float_of_int(evt##.clientX),
      float_of_int(evt##.clientY),
    );
    let row_col = (
      Float.to_int(
        (target_y -. container_rect##.top) /. font_metrics.row_height,
      ),
      Float.to_int(
        Float.round(
          (target_x -. container_rect##.left) /. font_metrics.col_width,
        ),
      ),
    );
    Event.Many([
      inject(Update.Action.MoveAction(Click(splice, row_col))),
      Event.Stop_propagation,
    ]);
  };

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

    | Annot(LivelitView({llu, llview, splice_map_opt}), _) => {
        let trigger = serialized_action =>
          inject(Update.Action.LivelitAction(llu, serialized_action));
        let livelit_view = llview(trigger);
        let vs = {
          let splice_getters_to_vdom =
            Livelits.LivelitView.get_splice_getters_to_vdom(livelit_view);
          let uhcode = splice_name => {
            let splice_layout =
              splice_ls |> SpliceMap.get_splice(llu, splice_name);
            let id = Printf.sprintf("code-splice-%d-%d", llu, splice_name);
            Node.div(
              [
                Attr.id(id),
                Attr.classes(["splice"]),
                Attr.on_mousedown(
                  on_mousedown(~id, ~splice=Some((llu, splice_name))),
                ),
              ],
              go(splice_layout),
            );
          };

          let dhcode = splice_name =>
            splice_map_opt
            |> OptUtil.map(splice_map =>
                 switch (NatMap.lookup(splice_map, splice_name)) {
                 | None => raise(Not_found)
                 | Some((_, d)) => (
                     d,
                     DHCode.view(
                       ~inject,
                       // TODO undo hardcoded width
                       ~width=80,
                       d,
                     ),
                   )
                 }
               );

          [splice_getters_to_vdom({uhcode, dhcode})];
        };
        Vdom.[
          Node.div(
            [
              switch (livelit_view) {
              | Inline(_, width) =>
                Attr.create(
                  "style",
                  Printf.sprintf(
                    "display: inline-block; width: %dch;",
                    width,
                  ),
                )
              | MultiLine(_, height) =>
                Attr.create(
                  "style",
                  Printf.sprintf(
                    "display: block; height: %fpx;",
                    float_of_int(height) *. font_metrics.row_height,
                  ),
                )
              },
              Attr.on_mousedown(_ => Event.Stop_propagation),
            ],
            vs,
          ),
        ];
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

  let id = "code-root";
  Node.div(
    [
      Attr.id(id),
      Attr.classes(["code", "presentation"]),
      Attr.on_mousedown(on_mousedown(~id, ~splice=None)),
    ],
    go(l),
  );
};
