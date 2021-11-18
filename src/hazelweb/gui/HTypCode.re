open Virtual_dom.Vdom;
open Pretty;

// let view_of_layout =
//     (
//       ~inject,
//       ~font_metrics: FontMetrics.t,
//       ~selected_tag_hole: option(MetaVar.t),
//       diff_steps: list(CursorPath.steps),
//       l: HTypLayout.t,
//     )
//     : Node.t => {
//   let corner_radii = Decoration_common.corner_radii(font_metrics);
//   let (text, decorations) =
//     HTypMeasuredLayout.mk(l)
//     |> MeasuredLayout.pos_fold(
//          ~linebreak=_ => ([Node.br([])], []),
//          ~text=(_, s) => ([Node.text(s)], []),
//          ~align=
//            (_, (txt, ds)) =>
//              ([Node.div([Attr.classes(["Align"])], txt)], ds),
//          ~cat=(_, (txt1, ds1), (txt2, ds2)) => (txt1 @ txt2, ds1 @ ds2),
//          ~annot=
//            (~go, ~indent, ~start, annot: HTypAnnot.t, m) => {
//              let (txt, ds) = go(m);
//              switch (annot) {
//              | Delim => ([Node.span([Attr.classes(["Delim"])], txt)], ds)
//              | HoleLabel => (
//                  [Node.span([Attr.classes(["HoleLabel"])], txt)],
//                  ds,
//                )
//              | EmptyTagHole(u) =>
//                let selected =
//                  selected_tag_hole
//                  |> Option.map(MetaVar.eq(u))
//                  |> Option.value(~default=false);
//                (
//                  [
//                    Node.span(
//                      [
//                        Attr.classes([
//                          "HoleLabel",
//                          "EmptyTagHole",
//                          ...selected ? ["selected"] : [],
//                        ]),
//                        Attr.on_click(_ =>
//                          inject(ModelAction.SelectTagHole(u))
//                        ),
//                      ],
//                      txt,
//                    ),
//                  ],
//                  ds,
//                );
//              | NonEmptyTagHole(_, _) =>
//                let offset = start.col - indent;
//                let decoration =
//                  Decoration_common.container(
//                    ~font_metrics,
//                    ~height=MeasuredLayout.height(m),
//                    ~width=MeasuredLayout.width(~offset, m),
//                    ~origin=MeasuredPosition.{row: start.row, col: indent},
//                    ~cls="err-hole",
//                    [DHDecoration.ErrHole.view(~corner_radii, (offset, m))],
//                  );
//                (txt, [decoration, ...ds]);
//              };
//            },
//        );
//   Node.div(
//     [Attr.classes(["HTypCode"])],
//     [with_cls("code", text), ...decorations],
//   );
// };

let take_step = step =>
  List.filter_map(
    fun
    | [step', ...steps] when step == step' => Some(steps)
    | _ => None,
  );

let view_of_layout =
    (
      ~inject,
      ~font_metrics: FontMetrics.t,
      ~selected_tag_hole: option(MetaVar.t),
      diff_steps: list(CursorPath.steps),
      l: HTypLayout.t,
    )
    : list(Node.t) => {
  let row: ref(int) = ref(0);
  let col: ref(int) = ref(0);
  let rec go = (indent: int, dpaths, l: HTypLayout.t) =>
    switch (l) {
    | Linebreak =>
      row := row^ + 1;
      col := indent;
      [
        Node.br([]),
        Node.span(
          [],
          [Node.text(StringUtil.replicat(indent, Unicode.nbsp))],
        ),
      ];
    | Text(s) =>
      col := col^ + Unicode.length(s);
      [Node.text(s)];
    | Cat(l1, l2) =>
      let vs1 = go(indent, dpaths, l1);
      let vs2 = go(indent, dpaths, l2);
      vs1 @ vs2;
    | Align(l) => go(col^, dpaths, l)
    | Annot(annot, l) =>
      switch (annot) {
      | Term =>
        let vs = go(indent, dpaths, l);
        List.exists((==)([]), dpaths)
          ? [Node.span([Attr.classes(["Diff"])], vs)] : vs;
      | Step(step) =>
        let dpaths' = take_step(step, dpaths);
        go(indent, dpaths', l);
      | Delim =>
        let vs = go(indent, dpaths, l);
        [Node.span([Attr.classes(["Delim"])], vs)];
      | HoleLabel =>
        let vs = go(indent, dpaths, l);
        [Node.span([Attr.classes(["HoleLabel"])], vs)];
      | EmptyTagHole(u) =>
        let selected =
          selected_tag_hole
          |> Option.map(MetaVar.eq(u))
          |> Option.value(~default=false);
        let vs = go(indent, dpaths, l);
        [
          Node.span(
            [
              Attr.classes([
                "HoleLabel",
                "EmptyTagHole",
                ...selected ? ["selected"] : [],
              ]),
              Attr.on_click(_ => inject(ModelAction.SelectTagHole(u))),
            ],
            vs,
          ),
        ];
      | NonEmptyTagHole(_) =>
        let corner_radii = Decoration_common.corner_radii(font_metrics);
        let offset = col^ - indent;
        let vs = go(indent, dpaths, l);
        let m = HTypMeasuredLayout.mk(l);
        let decoration =
          Decoration_common.container(
            ~font_metrics,
            ~height=MeasuredLayout.height(m),
            ~width=MeasuredLayout.width(~offset, m),
            ~origin=MeasuredPosition.{row: row^, col: indent},
            ~cls="err-hole",
            [DHDecoration.ErrHole.view(~corner_radii, (offset, m))],
          );
        vs @ [decoration];
      }
    };
  go(0, diff_steps, l);
};

let view =
    (
      ~inject: ModelAction.t => Ui_event.t,
      ~selected_tag_hole: option(MetaVar.t),
      ~font_metrics: FontMetrics.t,
      ~width: int=30,
      ~pos: int=0,
      ~diff_steps: list(CursorPath.steps)=[],
      ty: HTyp.t,
    )
    : Node.t => {
  let l =
    ty
    |> HTypDoc.mk(~enforce_inline=false)
    |> LayoutOfDoc.layout_of_doc(~width, ~pos);
  switch (l) {
  | None => failwith("unimplemented: view_of_htyp on layout failure")
  | Some(l) =>
    Node.div(
      [Attr.classes(["code", "HTypCode"])],
      view_of_layout(
        ~inject,
        ~selected_tag_hole,
        ~font_metrics,
        diff_steps,
        l,
      ),
    )
  };
};
