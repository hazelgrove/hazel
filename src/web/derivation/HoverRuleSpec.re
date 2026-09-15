open Virtual_dom.Vdom;
open Language;

/* rendered on every page render, hovered or not: the deduction view
   parses two editors and runs statics, so it is memoized on the rule
   and the globals it reads */
let memo: ref(option((Rule.t, FontMetrics.t, Settings.Model.t, Node.t))) =
  ref(None);

let view = (~globals: Globals.t) => {
  let rule = DerivationExerciseMode.NinjaKeys.current_hover_rule^;
  switch (memo^) {
  | Some((r, fm, st, node))
      when r === rule && fm === globals.font_metrics && st === globals.settings => node
  | _ =>
    let node =
      Node.div(
        ~attrs=[Attr.class_("hover-rule-spec")],
        DrvExplainThis.deduction_view(
          ~spec=RuleSpec.of_spec(rule),
          ~rule=Some(RuleImage.to_image(rule)),
          ~color_map=ColorSteps.empty,
          ~globals,
        ),
      );
    memo := Some((rule, globals.font_metrics, globals.settings, node));
    node;
  };
};
