open Virtual_dom.Vdom;
open Pretty;

let with_cls = cls => Node.span([Attr.classes([cls])]);

let view_of_layout =
    (
      ~inject,
      ~font_metrics: FontMetrics.t,
      ~selected_tag_hole: option(MetaVar.t),
      l: HTypLayout.t,
    )
    : Node.t => {
  let corner_radii = Decoration_common.corner_radii(font_metrics);
  let (text, decorations) =
    HTypMeasuredLayout.mk(l)
    |> MeasuredLayout.pos_fold(
         ~linebreak=_ => ([Node.br([])], []),
         ~text=(_, s) => ([Node.text(s)], []),
         ~align=
           (_, (txt, ds)) =>
             ([Node.div([Attr.classes(["Align"])], txt)], ds),
         ~cat=(_, (txt1, ds1), (txt2, ds2)) => (txt1 @ txt2, ds1 @ ds2),
         ~annot=
           (~go, ~indent, ~start, annot: HTypAnnot.t, m) => {
             let (txt, ds) = go(m);
             switch (annot) {
             | Delim => ([with_cls("Delim", txt)], ds)
             | HoleLabel => ([with_cls("HoleLabel", txt)], ds)
             | EmptyTagHole(u) =>
               let selected =
                 selected_tag_hole
                 |> Option.map(MetaVar.eq(u))
                 |> Option.value(~default=false);
               (
                 [
                   Node.span(
                     [
                       Attr.classes([
                         "HoleLabel",
                         "EmptyTagHole",
                         ...selected ? ["selected"] : [],
                       ]),
                       Attr.on_click(_ =>
                         inject(ModelAction.SelectTagHole(u))
                       ),
                     ],
                     txt,
                   ),
                 ],
                 ds,
               );
             | NonEmptyTagHole(_, _) =>
               let offset = start.col - indent;
               let decoration =
                 Decoration_common.container(
                   ~font_metrics,
                   ~height=MeasuredLayout.height(m),
                   ~width=MeasuredLayout.width(~offset, m),
                   ~origin=MeasuredPosition.{row: start.row, col: indent},
                   ~cls="err-hole",
                   [DHDecoration.ErrHole.view(~corner_radii, (offset, m))],
                 );
               (txt, [decoration, ...ds]);
             | Step(_) => ([with_cls("Step", txt)], ds)
             | Term => ([with_cls("Term", txt)], ds)
             };
           },
       );
  Node.div(
    [Attr.classes(["HTypCode"])],
    [with_cls("code", text), ...decorations],
  );
};

// let view_of_layout =
//     (
//       ~inject: ModelAction.t => Ui_event.t,
//       ~selected_tag_hole: option(MetaVar.t),
//     ) =>
//   Layout.mk_of_layout(
//     Vdom.{
//       imp_of_string: s => [Node.text(s)],
//       imp_append: (@),
//       imp_newline: indent => [
//         Node.br([]),
//         Node.span(
//           [],
//           [Node.text(StringUtil.replicat(indent, Unicode.nbsp))],
//         ),
//       ],
//       imp_of_annot: (annot: HTypAnnot.t, vs) =>
//         switch (annot) {
//         | Delim => [Node.span([Attr.classes(["Delim"])], vs)]
//         | HoleLabel => [Node.span([Attr.classes(["HoleLabel"])], vs)]
//         | EmptyTagHole(u) =>
//           let selected =
//             selected_tag_hole
//             |> Option.map(MetaVar.eq(u))
//             |> Option.value(~default=false);
//           [
//             Node.span(
//               [
//                 Attr.classes([
//                   "EmptyTagHole",
//                   ...selected ? ["selected"] : [],
//                 ]),
//                 Attr.on_click(_ => inject(ModelAction.SelectTagHole(u))),
//               ],
//               vs,
//             ),
//           ];
//         | NonEmptyTagHole(_, u) =>
//           let offset = start.col - indent;
//           let decoration =
//             Decoration_common.container(
//               ~font_metrics,
//               ~height=MeasuredLayout.height(m),
//               ~width=MeasuredLayout.width(~offset, m),
//               ~origin=MeasuredPosition.{row: start.row, col: indent},
//               ~cls="err-hole",
//               [DHDecoration.ErrHole.view(~corner_radii, (offset, m))],
//             );
//           (txt, [decoration, ...ds]);
//         },
//       t_of_imp: vs => Node.div([Attr.classes(["code", "HTypCode"])], vs),
//     },
//   );

let view =
    (
      ~inject: ModelAction.t => Ui_event.t,
      ~selected_tag_hole: option(MetaVar.t),
      ~font_metrics: FontMetrics.t,
      ~width: int=30,
      ~pos: int=0,
      tag: UHTag.t,
    )
    : Node.t => {
  tag
  |> HTypDoc_Tag.mk
  |> LayoutOfDoc.layout_of_doc(~width, ~pos)
  |> OptUtil.get(() =>
       failwith("unimplemented: vieW_of_htyp on layout failure")
     )
  |> view_of_layout(~inject, ~selected_tag_hole, ~font_metrics);
};

// let view =
//     (
//       ~inject: ModelAction.t => Ui_event.t,
//       ~selected_tag_hole: option(MetaVar.t),
//       ~width=30,
//       ~pos=0,
//       tag: UHTag.t,
//     )
//     : Vdom.Node.t => {
//   let l = tag |> HTypDoc_Tag.mk |> LayoutOfDoc.layout_of_doc(~width, ~pos);
//   switch (l) {
//   | None => failwith("unimplemented: view_of_htag on layout failure")
//   | Some(l) => view_of_layout(~inject, ~selected_tag_hole, l)
//   };
// };
