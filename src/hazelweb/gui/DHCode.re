open Virtual_dom.Vdom;
open Pretty;

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
             | Step(_)
             | Term => (txt, ds)
             | Collapsed => ([with_cls("Collapsed", txt)], ds)
             | HoleLabel => ([with_cls("HoleLabel", txt)], ds)
             | Delim => ([with_cls("code-delim", txt)], ds)
             | EmptyHole(selected, hc) => (
                 [
                   Node.span(
                     [
                       Attr.classes([
                         "EmptyHole",
                         ...selected ? ["selected"] : [],
                       ]),
                       Attr.on_click(_ =>
                         inject(ModelAction.SelectHoleClosure(hc))
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
};

let view =
    (
      ~inject,
      ~settings: Settings.Evaluation.t,
      ~selected_hole_closure: option(HoleClosure.t),
      ~font_metrics: FontMetrics.t,
      ~width: int,
      ~pos=0,
      d: DHExp.t,
    )
    : Node.t => {
  d
  |> DHDoc_Exp.mk(~settings, ~enforce_inline=false, ~selected_hole_closure)
  |> LayoutOfDoc.layout_of_doc(~width, ~pos)
  |> OptUtil.get(() =>
       failwith("unimplemented: view_of_dhexp on layout failure")
     )
  |> view_of_layout(~inject, ~font_metrics);
};

let view_of_hole_closure =
    (
      ~inject,
      ~width: int,
      ~pos=0,
      ~selected_hole_closure,
      ~settings: Settings.Evaluation.t,
      ~font_metrics: FontMetrics.t,
      (u, i): HoleClosure.t,
    )
    : Node.t => {
  view(
    ~inject,
    ~settings,
    ~selected_hole_closure,
    ~font_metrics,
    ~width,
    ~pos,
    /* Environment is a don't-care value. For display purposes,
       the environment is ignored, only u, i are used.

       It is here because hole expressions in the postprocessed result
       must exist in a closure. */
    DHExp.Closure(ClosureEnvironment.placeholder, DHExp.EmptyHole(u, i)),
  );
};

let view_of_var = x => Node.text(x);
