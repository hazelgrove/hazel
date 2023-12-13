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
             | Steppable(obj) => (
                 [
                   Node.span(
                     ~attr=
                       Attr.many([
                         Attr.class_("steppable"),
                         Attr.on_click(_ =>
                           inject(UpdateAction.StepForward(obj))
                         ),
                       ]),
                     txt,
                   ),
                 ],
                 ds,
               )
             | Stepped => (
                 [
                   Node.span(~attr=Attr.many([Attr.class_("stepped")]), txt),
                 ],
                 ds,
               )
             | Step(_)
             | Substituted => (
                 [
                   Node.span(
                     ~attr=Attr.many([Attr.class_("substituted")]),
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
             | OperationError(
                 DivideByZero | InvalidOfString | IndexOutOfBounds,
               ) => (
                 [with_cls("OperationError", txt)],
                 ds,
               )
             | OperationError(NegativeExponent) => (
                 [with_cls("OperationError", txt)],
                 ds,
               )
             | OperationError(OutOfFuel) => (
                 [with_cls("OperationError", txt)],
                 ds,
               )
             | OperationError(InvalidProjection) => (
                 [with_cls("OperationError", txt)],
                 ds,
               )
             | VarHole(_) => ([with_cls("InVarHole", txt)], ds)
             | Invalid((_, (-666))) =>
               /* Evaluation and Elaboration exceptions */
               ([with_cls("exception", txt)], ds)
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

let view =
    (
      ~inject,
      ~settings: Settings.Evaluation.t,
      ~selected_hole_instance: option(HoleInstance.t),
      ~font_metrics: FontMetrics.t,
      ~width: int,
      ~pos=0,
      ~previous_step: option(EvaluatorStep.EvalObj.t)=None, // The step that will be displayed above this one
      ~hidden_steps: list(EvaluatorStep.EvalObj.t)=[], // The hidden steps between the above and the current one
      ~chosen_step: option(EvaluatorStep.EvalObj.t)=None, // The step that will be taken next
      ~next_steps: list(EvaluatorStep.EvalObj.t)=[],
      d: DHExp.t,
    )
    : Node.t => {
  let maker =
    settings.postprocess
      ? DHDoc_Exp.mk(~parenthesize=false)
      : DHDoc_Step.mk(
          ~previous_step,
          ~hidden_steps,
          ~chosen_step,
          ~next_steps,
          ~env=ClosureEnvironment.empty,
        );
  let c = maker(~settings, ~enforce_inline=false, ~selected_hole_instance);
  d
  |> c
  |> LayoutOfDoc.layout_of_doc(~width, ~pos)
  |> OptUtil.get(() =>
       failwith("unimplemented: view_of_dhexp on layout failure")
     )
  |> view_of_layout(~inject, ~font_metrics);
};

type font_metrics = FontMetrics.t;
