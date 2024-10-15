open Haz3lcore;
open Virtual_dom.Vdom;
open Util;
open ExplainThisForm;

open DrvSyntax;

let highlight = (msg: list(Node.t), id: Id.t, mapping: ColorSteps.t): Node.t => {
  let (c, _) = ColorSteps.get_color(id, mapping);
  let classes = Attr.class_("highlight-" ++ c);
  let attrs = [classes];
  Node.span(~attrs, msg);
};

let rec show = (p: int, prop: t, ~color_map: ColorSteps.t): list(Node.t) =>
  prop
  |> repr(~sp=Unicode.nbsp, p)
  |> Aba.join(x => [Node.text(x)], show(~color_map, precedence(prop)))
  |> List.concat
  // |> (
  //   switch (IdTagged.term_of(prop)) {
  //   // TODO(zhiyao): not good to use the string representation of the
  //   | Atom(s) when s.[0] == 'n' => (x => [Node.u(x)])
  //   | _ => Fun.id
  //   }
  // )
  |> (
    switch (
      Haz3lcore.Id.Map.find_opt(IdTagged.rep_id(prop), fst(color_map))
    ) {
    | None => Fun.id
    | Some(_) => (x => [highlight(x, IdTagged.rep_id(prop), color_map)])
    }
  );

let show = show(P.min);

let copy_color_map =
    (terms: list(RuleSpec.specced), (map, idx): ColorSteps.t): ColorSteps.t => {
  (
    List.fold_left(
      (new_map, (spec, syntax)) =>
        switch (Haz3lcore.Id.Map.find_opt(IdTagged.rep_id(syntax), map)) {
        | None => new_map
        | Some(color) =>
          Haz3lcore.Id.Map.add(IdTagged.rep_id(spec), color, new_map)
        },
      Haz3lcore.Id.Map.empty,
      terms,
    ),
    idx,
  );
};

let copy_color_map =
    (failure: RuleVerify.failure, color_map: ColorSteps.t): ColorSteps.t => {
  let terms: list(RuleSpec.specced) =
    switch (failure) {
    | Mismatch(_) => []
    | FailSpec(FailUnbox(specced)) => [specced]
    | FailSpec(NotEqual(specced1, specced2)) => [specced1, specced2]
    | FailTest(map, test) =>
      test
      |> RuleTest.get_symbols
      |> List.map(RuleSpec.Map.find_opt(_, map))
      |> List.filter_map(Fun.id)
    };
  copy_color_map(terms, color_map);
};

let conclusion_view = (~spec: RuleSpec.t, ~color_map: ColorSteps.t) =>
  Node.div(
    ~attrs=[Attr.class_("deduction-concl"), Attr.class_("drv-explainthis")],
    [Node.span(show(RuleSpec.of_syntax(spec), ~color_map))],
  );

let rule_to_label =
  fun
  | Some(rule) => Rule.repr(rule)
  | None => "?";

let label_view = (~label) =>
  Node.div(~attrs=[Attr.class_("deduction-label")], [Node.text(label)]);

let premises_view =
    (
      ~specs: list(RuleSpec.t),
      ~tests: list(RuleTest.t),
      ~rule,
      ~color_map: ColorSteps.t,
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
              [
                Node.span(show(RuleSpec.of_syntax(spec), ~color_map)),
                Node.text(
                  String.concat("", List.init(3, _ => Unicode.nbsp)),
                ),
              ],
            ),
          specs,
        )
        @ List.map(
            test =>
              Node.div(
                ~attrs=[
                  Attr.class_("deduction-test"),
                  Attr.class_("drv-explainthis"),
                ],
                [
                  Node.text(RuleTest.show(test)),
                  Node.text(
                    String.concat("", List.init(3, _ => Unicode.nbsp)),
                  ),
                ],
              ),
            tests,
          ),
      ),
    ]
    @ [label_view(~label)],
  );
};

let rule_example_view =
    (
      ~info: Haz3lschool.ProofGrade.VerifiedTree.info,
      ~color_map: ColorSteps.t,
    )
    : Node.t => {
  let (rule, res) = (info.rule, info);
  let color_map =
    switch (res.res) {
    | Correct => color_map
    | Incorrect(failure) => copy_color_map(failure, color_map)
    | Pending(_) => color_map
    };
  Node.div(
    ~attrs=[Attr.class_("section"), Attr.class_("syntactic-form")],
    switch (rule) {
    | Some({spec: (concl, prems), tests, rule}) => [
        premises_view(~specs=prems, ~tests, ~rule=Some(rule), ~color_map),
        conclusion_view(~spec=concl, ~color_map),
      ]
    | None => []
    },
  );
};

let rule_example_view =
    (
      ~info: option(Haz3lschool.ProofGrade.VerifiedTree.info),
      ~color_map: ColorSteps.t,
    ) =>
  switch (info) {
  | Some(info) => rule_example_view(~info, ~color_map)
  | None => Node.div([])
  };

let mk_explanation_title = () =>
  Node.div(
    ~attrs=[Attr.class_("section-title")],
    [Node.text("Verification Result")],
  );

let show_ghost = (t: option(t)) =>
  switch (t) {
  | Some(t) => DrvSyntax.show(t) |> String.trim
  | None => "?"
  };

let premise_mismatch: group = {
  id: DrvPremiseMismatch,
  forms: [
    {
      id: DrvPremiseMismatch,
      syntactic_form: [],
      expandable_id: None,
      explanation: "",
      examples: [],
    },
  ],
};
