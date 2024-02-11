open Virtual_dom;
open Virtual_dom.Vdom;
open Haz3lcore;
open Node;
open Util;
open Pretty;
open Virtual_dom.Vdom;

let with_cls = cls => Node.span(~attr=Attr.classes([cls]));

let string_of_stepper_action = (action: UpdateAction.stepper_action) =>
  switch (action) {
  | Rewrite(r) => RewriteStep.string_of(r.rule)
  | _ => "Step"
  };

let stepper_action = (inject, result_key, action: UpdateAction.stepper_action) =>
  div(
    ~attr=
      Attr.many([
        Attr.class_("rewrite"),
        Attr.on_click(_ =>
          Effect.Many([
            Effect.Prevent_default,
            Effect.Stop_propagation,
            inject(UpdateAction.StepperAction(result_key, action)),
          ])
        ),
      ]),
    [action |> string_of_stepper_action |> Node.text],
  );

let steppable = (inject, txt, result_key, actions) => [
  div(
    ~attr=Attr.class_("steppable-wrapper"),
    [span(~attr=Attr.class_("steppable"), txt)]
    @ [
      div(~attr=Attr.class_("handle-pre"), []),
      div(~attr=Attr.class_("handle"), []),
      div(~attr=Attr.class_("handle-back"), []),
      div(
        ~attr=Attr.many([Attr.class_("menu")]),
        List.map(stepper_action(inject, result_key), actions),
      ),
    ],
  ),
];

let view_of_layout =
    (~inject, ~font_metrics: FontMetrics.t, ~result_key, l: DHLayout.t)
    : Node.t => {
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
             | Steppable(actions) => (
                 steppable(inject, txt, result_key, actions),
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
      ~settings: CoreSettings.Evaluation.t,
      ~selected_hole_instance: option(HoleInstance.t),
      ~font_metrics: FontMetrics.t,
      ~width: int,
      ~pos=0,
      ~previous_step: option(EvaluatorStep.step)=None, // The step that will be displayed above this one
      ~hidden_steps: list(EvaluatorStep.step)=[], // The hidden steps between the above and the current one
      ~chosen_step: option(EvaluatorStep.step)=None, // The step that will be taken next
      ~next_steps: list(EvaluatorStep.EvalObj.t)=[],
      ~show_steppable: bool=true,
      ~result_key: string,
      d: DHExp.t,
    )
    : Node.t => {
  DHDoc_Exp.mk(
    ~previous_step,
    ~hidden_steps,
    ~chosen_step,
    ~next_steps,
    ~env=ClosureEnvironment.empty,
    ~settings,
    ~show_steppable,
    ~enforce_inline=false,
    ~selected_hole_instance,
    d,
  )
  |> LayoutOfDoc.layout_of_doc(~width, ~pos)
  |> OptUtil.get(() =>
       failwith("unimplemented: view_of_dhexp on layout failure")
     )
  |> view_of_layout(~inject, ~font_metrics, ~result_key);
};

type font_metrics = FontMetrics.t;
