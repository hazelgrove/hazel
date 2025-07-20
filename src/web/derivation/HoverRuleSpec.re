open Virtual_dom.Vdom;
open Haz3lcore;
open Language;

let show = (syntax: Drv.Exp.t, ~globals: Globals.t): Node.t => {
  let editor =
    Editor.Model.mk(
      syntax
      |> ExpToSegment.drv_exp_to_pretty(
           ~settings={
             inline: true,
             fold_case_clauses: false,
             fold_fn_bodies: false,
             hide_fixpoints: false,
             show_filters: false,
             show_unknown_as_hole: false,
           },
           ~sort=Jdmt,
         )
      |> Zipper.unzip,
      ~root=Drv(Jdmt),
    );
  let statics =
    CachedStatics.init_from_term(
      ~settings=CoreSettings.on,
      ~is_dynamic_term=false,
      DrvExp(Exp(syntax), Jdmt) |> Exp.fresh,
    );
  CodeWithStatics.View.view(
    ~globals,
    ~sort=Drv(Jdmt),
    {
      editor,
      statics,
      dynamics: Dynamics.Map.empty,
    },
  );
};

let show_without_statics = (pretty: Segment.t, ~globals: Globals.t): Node.t => {
  let editor = Editor.Model.mk(pretty |> Zipper.unzip, ~root=Drv(Jdmt));
  CodeWithStatics.View.view(
    ~globals,
    ~sort=Drv(Jdmt),
    {
      editor,
      statics: CachedStatics.empty,
      dynamics: Dynamics.Map.empty,
    },
  );
};

let conclusion_view = (~spec: Drv.Exp.t, ~globals: Globals.t) =>
  Node.div(
    ~attrs=[Attr.class_("deduction-concl"), Attr.class_("drv-explainthis")],
    [show(spec, ~globals)],
  );

let rule_to_label =
  fun
  | Some(rule) => RuleImage.repr(rule)
  | None => "?";

let label_view = (~label) =>
  Node.div(~attrs=[Attr.class_("deduction-label")], [Node.text(label)]);

let premises_view = (~spec as {prems, tests, _}: RuleSpec.t, ~rule, ~globals) => {
  let label = rule_to_label(rule);
  Node.div(
    ~attrs=[Attr.class_("deduction-prems-label")],
    [
      Node.div(
        ~attrs=[
          Attr.class_("deduction-prems"),
          Attr.class_("drv-explainthis"),
        ],
        List.map(
          spec =>
            Node.div(
              ~attrs=[Attr.class_("drv-explainthis")],
              [show(spec, ~globals)],
            ),
          prems,
        )
        @ [
          Node.div(
            ~attrs=[Attr.class_("deduction-test")],
            List.map(
              (test: RuleFormula.t(bool)) =>
                switch (test.term) {
                | Ignore(_) => Node.none
                | _ =>
                  Node.div(
                    ~attrs=[Attr.class_("drv-explainthis")],
                    [
                      show_without_statics(
                        ExpToSegment.drv_formula_to_pretty(test, Jdmt),
                        ~globals,
                      ),
                    ],
                  )
                },
              tests,
            ),
          ),
        ],
      ),
    ]
    @ [label_view(~label)],
  );
};

let view = (~globals: Globals.t) => {
  let rule = DerivationMode.NinjaKeys.current_hover_rule^;
  let RuleSpec.Spec.{concl, _} as spec = RuleSpec.of_spec(rule);
  Node.div(
    ~attrs=[Attr.class_("hover-rule-spec")],
    [
      premises_view(~spec, ~rule=Some(RuleImage.to_image(rule)), ~globals),
      conclusion_view(~spec=concl, ~globals),
    ],
  );
};
