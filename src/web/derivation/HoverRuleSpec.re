open Virtual_dom.Vdom;
open Language;

let view = (~globals: Globals.t) => {
  let rule = DerivationMode.NinjaKeys.current_hover_rule^;
  Node.div(
    ~attrs=[Attr.class_("hover-rule-spec")],
    DrvExplainThis.deduction_view(
      ~spec=RuleSpec.of_spec(rule),
      ~rule=Some(RuleImage.to_image(rule)),
      ~color_map=ColorSteps.empty,
      ~globals,
    ),
  );
};
