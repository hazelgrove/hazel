open Virtual_dom;
open Virtual_dom.Vdom;
open Util;
open Pretty;
open Haz3lcore;

let with_cls = cls => Node.span(~attr=Attr.classes([cls]));

let view_of_layout =
    (~inject, ~font_metrics: FontMetrics.t, l: DHLayout.t): Node.t => {
  let corner_radii = Decoration_common.corner_radii(font_metrics);
  let (text, decorations) =
    DHMeasuredLayout.mk(l)
    |> MeasuredLayout.pos_fold(
         ~linebreak=_ => ([Node.br()], []),
         ~text=(_, s) => ([Node.text(s)], []),
         ~align=
           (_, (txt, ds)) =>
             ([Node.div(~attr=Attr.classes(["Align"]), txt)], ds),
         ~cat=(_, (txt1, ds1), (txt2, ds2)) => (txt1 @ txt2, ds1 @ ds2),
         ~annot=
           (~go, ~indent, ~start, annot: DHAnnot.t, m) => {
             let (txt, ds) = go(m);
             switch (annot) {
             | Step(ind) => (
                 [
                   Node.span(
                     ~attr=
                       Attr.many([
                         Attr.classes(["Steppable"]),
                         Attr.on_click(_ =>
                           inject(UpdateAction.StepForward(ind))
                         ),
                       ]),
                     txt,
                   ),
                 ],
                 ds,
               )
             | Term => (txt, ds)
             | Collapsed => ([with_cls("Collapsed", txt)], ds)
             | HoleLabel => ([with_cls("HoleLabel", txt)], ds)
             | Delim => ([with_cls("code-delim", txt)], ds)
             | EmptyHole(selected, _inst) => (
                 [
                   Node.span(
                     ~attr=
                       Attr.many([
                         Attr.classes([
                           "EmptyHole",
                           ...selected ? ["selected"] : [],
                         ]),
                         Attr.on_click(_ =>
                           Vdom.Effect.Many([
                             Vdom.Effect.Stop_propagation,
                             // TODO fix
                             //  inject(ModelAction.SelectHoleInstance(inst)),
                           ])
                         ),
                       ]),
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
             | OperationError(DivideByZero) => (
                 [with_cls("DivideByZero", txt)],
                 ds,
               )
             | OperationError(OutOfFuel) => (
                 //TODO: custom class
                 [with_cls("DivideByZero", txt)],
                 ds,
               )
             | OperationError(InvalidProjection) => (
                 //TODO: custom class
                 [with_cls("DivideByZero", txt)],
                 ds,
               )
             | VarHole(_) => ([with_cls("InVarHole", txt)], ds)
             | NonEmptyHole(_)
             | InconsistentBranches(_)
             | Invalid(_) =>
               let offset = start.col - indent;
               let decoration =
                 Decoration_common.container(
                   ~container_type=Svg,
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
    ~attr=Attr.classes(["DHCode"]),
    [with_cls("code", text), ...decorations],
  );
};

let assign_step_indices = (layout): Layout.t(DHAnnot.t) => {
  // print_endline(
  //   "before index: "
  //   ++ Sexplib.Sexp.to_string_hum(
  //        Layout.sexp_of_t(DHAnnot.sexp_of_t, layout),
  //      ),
  // );
  let rec assign = (index: int, l: Layout.t(DHAnnot.t)) =>
    switch (l) {
    | Text(_)
    | Linebreak => (index, l)
    | Cat(l1, l2) =>
      let (n1, l1') = assign(index, l1);
      let (n2, l2') = assign(n1, l2);
      (n2, Cat(l1', l2'));
    | Align(l1) =>
      let (n1, l1') = assign(index, l1);
      (n1, Align(l1'));
    | Annot(ann, l1) =>
      switch (ann) {
      | Step(_) => (index + 1, Annot(Step(index), l1))
      | _ => (index, l)
      }
    };
  let (_, layout') = assign(0, layout);
  // print_endline(
  //   "after index: "
  //   ++ Sexplib.Sexp.to_string_hum(
  //        Layout.sexp_of_t(DHAnnot.sexp_of_t, layout'),
  //      ),
  // );
  layout';
};

let view =
    (
      ~inject,
      ~settings: Settings.Evaluation.t,
      ~selected_hole_instance: option(HoleInstance.t),
      ~font_metrics: FontMetrics.t,
      ~width: int,
      ~pos=0,
      d: DHExp.t,
    )
    : Node.t => {
  d
  |> DHDoc_Exp.mk(~settings, ~enforce_inline=false, ~selected_hole_instance)
  |> LayoutOfDoc.layout_of_doc(~width, ~pos)
  |> OptUtil.get(() =>
       failwith("unimplemented: view_of_dhexp on layout failure")
     )
  |> assign_step_indices
  |> view_of_layout(~inject, ~font_metrics);
};

let view_of_hole_instance =
    (
      ~inject,
      ~width: int,
      ~pos=0,
      ~selected_hole_instance,
      ~settings: Settings.Evaluation.t,
      ~font_metrics: FontMetrics.t,
      (u, i): HoleInstance.t,
    )
    : Node.t =>
  view(
    ~inject,
    ~settings,
    ~selected_hole_instance,
    ~font_metrics,
    ~width,
    ~pos,
    DHExp.EmptyHole(u, i),
  );

let view_of_var = x => Node.text(x);

let view_of_layout_tylr =
    (~inject, ~font_metrics: FontMetrics.t, l: DHLayout.t): Node.t => {
  let corner_radii = Decoration_common.corner_radii(font_metrics);
  let (text, decorations) =
    DHMeasuredLayout.mk(l)
    |> MeasuredLayout.pos_fold(
         ~linebreak=_ => ([Node.br()], []),
         ~text=(_, s) => ([Node.text(s)], []),
         ~align=
           (_, (txt, ds)) =>
             ([Node.div(~attr=Attr.classes(["Align"]), txt)], ds),
         ~cat=(_, (txt1, ds1), (txt2, ds2)) => (txt1 @ txt2, ds1 @ ds2),
         ~annot=
           (~go, ~indent, ~start, annot: DHAnnot.t, m) => {
             let (txt, ds) = go(m);
             switch (annot) {
             | Step(ind) => (
                 [
                   Node.span(
                     ~attr=
                       Attr.many([
                         Attr.classes(["Steppable"]),
                         Attr.on_click(_ =>
                           inject(UpdateAction.StepForward(ind))
                         ),
                       ]),
                     txt,
                   ),
                 ],
                 ds,
               )
             | Term => (txt, ds)
             | Collapsed => ([with_cls("Collapsed", txt)], ds)
             | HoleLabel => ([with_cls("HoleLabel", txt)], ds)
             | Delim => ([with_cls("code-delim", txt)], ds)
             | EmptyHole(selected, _inst) => (
                 [
                   Node.span(
                     ~attr=
                       Attr.many([
                         Attr.classes([
                           "EmptyHole",
                           ...selected ? ["selected"] : [],
                         ]),
                         Attr.on_click(_ =>
                           Vdom.Effect.Many([
                             Vdom.Effect.Stop_propagation,
                             //inject(ModelAction.SelectHoleInstance(inst)),
                           ])
                         ),
                       ]),
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
             | OperationError(DivideByZero) => (
                 [with_cls("DivideByZero", txt)],
                 ds,
               )
             | OperationError(OutOfFuel) => (
                 //TODO: custom class
                 [with_cls("DivideByZero", txt)],
                 ds,
               )
             | OperationError(InvalidProjection) => (
                 //TODO: custom class
                 [with_cls("DivideByZero", txt)],
                 ds,
               )
             | VarHole(_) => ([with_cls("InVarHole", txt)], ds)
             | NonEmptyHole(_)
             | InconsistentBranches(_)
             | Invalid(_) =>
               let offset = start.col - indent;
               let decoration =
                 Decoration_common.container(
                   ~container_type=Svg,
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
    ~attr=Attr.classes(["DHCode"]),
    [with_cls("code", text), ...decorations],
  );
};

let view_tylr =
    (
      ~inject,
      ~settings: Settings.Evaluation.t,
      ~selected_hole_instance: option(HoleInstance.t),
      ~font_metrics: FontMetrics.t,
      ~width: int,
      ~pos=0,
      d: DHExp.t,
    )
    : Node.t => {
  d
  |> DHDoc_Exp.mk(~settings, ~enforce_inline=false, ~selected_hole_instance)
  |> LayoutOfDoc.layout_of_doc(~width, ~pos)
  |> OptUtil.get(() =>
       failwith("unimplemented: view_of_dhexp on layout failure")
     )
  |> assign_step_indices
  |> view_of_layout_tylr(~inject, ~font_metrics);
};

type font_metrics = FontMetrics.t;
