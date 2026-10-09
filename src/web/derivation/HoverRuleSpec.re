open Virtual_dom.Vdom;
open Language;

/* rendered on every page render, hovered or not: the deduction view
   parses two editors and runs statics, so it is memoized on the rule
   and the globals it reads */
let memo:
  ref(option((Rule.t, FontMetrics.t, Settings.Model.t, bool, Node.t))) =
  ref(None);

let view = (~globals: Globals.t) => {
  let rule = DerivationExerciseMode.NinjaKeys.current_hover_rule^;
  switch (memo^) {
  | Some((r, fm, st, meta_down, node))
      when
        phys_equal(r, rule)
        && phys_equal(fm, globals.font_metrics)
        && phys_equal(st, globals.settings)
        && Bool.equal(meta_down, globals.meta_down) => node
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
    memo :=
      Some((
        rule,
        globals.font_metrics,
        globals.settings,
        globals.meta_down,
        node,
      ));
    node;
  };
};
