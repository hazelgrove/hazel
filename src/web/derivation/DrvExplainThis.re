open Haz3lcore;
open Virtual_dom.Vdom;
open ExplainThisForm;
open Language;

let highlight = (msg: list(Node.t), id: Id.t, mapping: ColorSteps.t): Node.t => {
  let (c, _) = ColorSteps.get_color(id, mapping);
  let classes = Attr.class_("highlight-" ++ c);
  let attrs = [classes];
  Node.span(~attrs, msg);
};

let show =
    (syntax: Drv.Exp.t, ~color_map: ColorSteps.t, ~globals: Globals.t): Node.t => {
  let editor =
    Editor.Model.mk(
      syntax
      |> ExpToSegment.drv_exp_to_pretty(
           ~settings=
             ExpToSegment.Settings.of_core(
               ~inline=true,
               globals.settings.core,
             ),
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
  let highlight_deco = {
    module Deco =
      Deco.Deco({
        let editor = editor;
        let globals = {
          ...globals,
          color_highlights: Some(fst(color_map)),
        };
        let statics = statics;
      });
    [Deco.color_highlights()];
  };
  CodeWithStatics.View.view(
    ~globals,
    ~overlays=highlight_deco,
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

let copy_color_map =
    (terms: list(RuleVerify.specced), (map, idx): ColorSteps.t)
    : ColorSteps.t => {
  (
    List.fold_left(
      (new_map, (spec, syntax)) =>
        switch (Haz3lcore.Id.Map.find_opt(Drv.Any.rep_id(syntax), map)) {
        | None => new_map
        | Some(color) =>
          Haz3lcore.Id.Map.add(Drv.Any.rep_id(spec), color, new_map)
        },
      Haz3lcore.Id.Map.empty,
      terms,
    ),
    idx,
  );
};

let copy_color_map =
    (failure: RuleVerify.failure, color_map: ColorSteps.t): ColorSteps.t => {
  let terms: list(RuleVerify.specced) =
    switch (failure) {
    | Mismatch(_) => []
    | FailMatch(specced) => [specced]
    | NotEqual(specced1, specced2) => [specced1, specced2]
    | FailUnbox(specced, _) => [specced]
    | FailTest(map, test) =>
      test
      |> RuleSpec.Formula.get_symbols
      |> List.map(RuleVerify.Map.find_opt(_, map))
      |> List.filter_map(Fun.id)
    };
  copy_color_map(terms, color_map);
};

let conclusion_view =
    (~spec: Drv.Exp.t, ~color_map: ColorSteps.t, ~globals: Globals.t) =>
  Node.div(
    ~attrs=[Attr.class_("deduction-concl"), Attr.class_("drv-explainthis")],
    [show(spec, ~color_map, ~globals)],
  );

let rule_to_label =
  fun
  | Some(rule) => RuleImage.repr(rule)
  | None => "?";

let label_view = (~label) =>
  Node.div(~attrs=[Attr.class_("deduction-label")], [Node.text(label)]);

let premises_view =
    (
      ~spec as {prems, tests, _}: RuleSpec.t,
      ~rule,
      ~color_map: ColorSteps.t,
      ~globals,
    ) => {
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
              [show(spec, ~color_map, ~globals)],
            ),
          prems,
        )
        @ [
          Node.div(
            ~attrs=[Attr.class_("deduction-test")],
            List.map(
              test =>
                switch (test) {
                | RuleSpec.Formula.Ignore(_) => Node.none
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

let rule_example_view =
    (~info: DrvGrading.VerifiedTree.info, ~color_map: ColorSteps.t, ~globals)
    : Node.t => {
  let (rule, res) = (info.rule, info);
  let color_map =
    switch (res.res) {
    | Correct => color_map
    | PartialCorrect(specced) =>
      copy_color_map(FailMatch(specced), color_map)
    | Incorrect(failure) => copy_color_map(failure, color_map)
    | Pending(_) => color_map
    };
  Node.div(
    ~attrs=[
      Attr.class_("section"),
      Attr.class_("syntactic-form"),
      Attr.class_("drv-explainthis-section"),
    ],
    switch (rule) {
    | Some({spec, rule}) => [
        premises_view(
          ~spec,
          ~rule=Some(RuleImage.to_image(rule)),
          ~color_map,
          ~globals,
        ),
        conclusion_view(~spec=spec.concl, ~color_map, ~globals),
      ]
    | None => []
    },
  );
};

let rule_example_view =
    (
      ~info: option(DrvGrading.VerifiedTree.info),
      ~color_map: ColorSteps.t,
      ~globals,
    ) =>
  switch (info) {
  | Some(info) => rule_example_view(~info, ~color_map, ~globals)
  | None => Node.div([])
  };

let mk_explanation_title = () =>
  Node.div(
    ~attrs=[Attr.class_("section-title")],
    [Node.text("Verification Result")],
  );

let premise_mismatch: group = {
  id: Derivation,
  forms: [
    {
      id: Derivation,
      syntactic_form: [],
      expandable_id: None,
      explanation: "",
      examples: [],
    },
  ],
};
