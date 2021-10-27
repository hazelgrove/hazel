open Virtual_dom.Vdom;
open Pretty;

<<<<<<< HEAD
let view_of_layout = (~inject, l: DHLayout.t): Vdom.Node.t => {
  open Vdom;
  let rec go = (l: DHLayout.t) =>
    switch (l) {
    | Text(s) => [Node.text(s)]
    | Cat(l1, l2) => go(l1) @ go(l2)
    | Linebreak => [Node.br([])]
    | Align(l) => [Node.div([Attr.classes(["Align"])], go(l))]
    | Annot(Collapsed, l) => [
        Node.span([Attr.classes(["Collapsed"])], go(l)),
      ]
    | Annot(HoleLabel, l) => [
        Node.span([Attr.classes(["HoleLabel"])], go(l)),
      ]
    | Annot(Delim, l) => [
        Node.span([Attr.classes(["code-delim"])], go(l)),
      ]
    | Annot(NonEmptyHole(_), l) => [
        Node.span([Attr.classes(["InHole"])], go(l)),
      ]
    | Annot(InconsistentBranches(_), l) => [
        Node.span([Attr.classes(["InconsistentBranches"])], go(l)),
      ]
    | Annot(Invalid(_), l) => [
        Node.span([Attr.classes(["InHole"])], go(l)),
      ]
    | Annot(VarHole(_), l) => [
        Node.span([Attr.classes(["InVarHole"])], go(l)),
      ]
    | Annot(EmptyHole(selected, inst), l) => [
        Node.span(
          [
            Attr.classes(["EmptyHole", ...selected ? ["selected"] : []]),
            Attr.on_click(_ => inject(ModelAction.SelectHoleInstance(inst))),
          ],
          go(l),
        ),
      ]
    | Annot(FailedCastDelim, l) => [
        Node.span([Attr.classes(["FailedCastDelim"])], go(l)),
      ]
    | Annot(AssertionFail, l) => [
        Node.span([Attr.classes(["AssertionFail"])], go(l)),
      ]
    | Annot(AssertPass, l) => [
        Node.span([Attr.classes(["AssertPass"])], go(l)),
      ]
    | Annot(AssertFail, l) => [
        Node.span([Attr.classes(["AssertFail"])], go(l)),
      ]
    | Annot(AssertIndet, l) => [
        Node.span([Attr.classes(["AssertIndet"])], go(l)),
      ]
    | Annot(AssertComp, l) => [
        Node.span([Attr.classes(["AssertComp"])], go(l)),
      ]
    | Annot(FailedCastDecoration, l) => [
        Node.span([Attr.classes(["FailedCastDecoration"])], go(l)),
      ]
    | Annot(CastDecoration, l) => [
        Node.div([Attr.classes(["CastDecoration"])], go(l)),
      ]
    | Annot(DivideByZero, l) => [
        Node.span([Attr.classes(["DivideByZero"])], go(l)),
      ]
    };
  Node.div([Attr.classes(["code", "DHCode"])], go(l));
=======
let with_cls = cls => Node.span([Attr.classes([cls])]);

let view_of_layout =
    (~inject, ~font_metrics: FontMetrics.t, l: DHLayout.t): Node.t => {
  let corner_radii = Decoration_common.corner_radii(font_metrics);
  let (text, decorations) =
    DHMeasuredLayout.mk(l)
    |> MeasuredLayout.pos_fold(
         ~linebreak=_ => ([Node.br([])], []),
         ~text=(_, s) => ([Node.text(s)], []),
         ~align=
           (_, (txt, ds)) =>
             ([Node.div([Attr.classes(["Align"])], txt)], ds),
         ~cat=(_, (txt1, ds1), (txt2, ds2)) => (txt1 @ txt2, ds1 @ ds2),
         ~annot=
           (~go, ~indent, ~start, annot: DHAnnot.t, m) => {
             let (txt, ds) = go(m);
             switch (annot) {
             | Collapsed => ([with_cls("Collapsed", txt)], ds)
             | HoleLabel => ([with_cls("HoleLabel", txt)], ds)
             | Delim => ([with_cls("code-delim", txt)], ds)
             | EmptyHole(selected, inst) => (
                 [
                   Node.span(
                     [
                       Attr.classes([
                         "EmptyHole",
                         ...selected ? ["selected"] : [],
                       ]),
                       Attr.on_click(_ =>
                         inject(ModelAction.SelectHoleInstance(inst))
                       ),
                     ],
                     txt,
                   ),
                 ],
                 ds,
               )
             | FailedCastDelim => ([with_cls("FailedCastDelim", txt)], ds)
             | FailedCastDecoration => (
                 [with_cls("FailedCastDecoration", txt)],
                 ds,
               )
             | CastDecoration => ([with_cls("CastDecoration", txt)], ds)
             | DivideByZero => ([with_cls("DivideByZero", txt)], ds)
             | VarHole(_) => ([with_cls("InVarHole", txt)], ds)
             | NonEmptyHole(_)
             | InconsistentBranches(_)
             | Invalid(_) =>
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
             };
           },
       );
  Node.div(
    [Attr.classes(["DHCode"])],
    [with_cls("code", text), ...decorations],
  );
>>>>>>> 52706d44e926e57ad234412b9cb02109860cb18b
};

let view =
    (
      ~inject,
      ~settings: Settings.Evaluation.t,
      ~selected_instance: option(HoleInstance.t),
      ~font_metrics: FontMetrics.t,
      ~width: int,
      ~pos=0,
      d: DHExp.t,
      map: AssertMap.t,
    )
    : Node.t => {
  d
  |> DHDoc_Exp.mk(~settings, ~enforce_inline=false, ~selected_instance, map)
  |> LayoutOfDoc.layout_of_doc(~width, ~pos)
  |> OptUtil.get(() =>
       failwith("unimplemented: view_of_dhexp on layout failure")
     )
  |> view_of_layout(~inject, ~font_metrics);
};

let view_of_hole_instance =
    (
      ~inject,
      ~width: int,
      ~pos=0,
      ~selected_instance,
      ~settings: Settings.Evaluation.t,
      ~font_metrics: FontMetrics.t,
      (u, i): HoleInstance.t,
    )
    : Node.t =>
  view(
    ~inject,
    ~settings,
    ~selected_instance,
    ~font_metrics,
    ~width,
    ~pos,
    DHExp.EmptyHole(u, i, []),
    AssertMap.empty,
  );

let view_of_var = x => Node.text(x);
