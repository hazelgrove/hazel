open Virtual_dom.Vdom;
open Js_of_ocaml;
open Haz3lcore;
open Node;

type verifyRes =
  | Correct
  | Partial
  | Pending
  | Incorrect(string)
  | NotAJudgment;

let to_verifyRes =
    (res: result(unit, DerivationError.VerErr.t), children_res): verifyRes =>
  switch (res) {
  | Ok(_) =>
    children_res |> List.for_all(res => res == Correct) ? Correct : Partial
  | Error(External("E-252")) => NotAJudgment
  | Error(External(_)) => Pending
  | Error(e) => Incorrect(DerivationError.VerErr.repr(e))
  };

let to_class = res =>
  Attr.class_(
    switch (res) {
    | Correct => "correct"
    | Partial => "partial"
    | NotAJudgment
    | Pending => "pending"
    | Incorrect(_) => "incorrect"
    },
  );

let proof_view =
    (
      ~inject,
      ~ui_state: Model.ui_state,
      ~settings: Settings.t,
      ~pos: Exercise.Proof.pos,
      ~header: Exercise.header,
      ~eds: Exercise.Proof.p(Editor.t),
      ~stitched_dynamics: Exercise.ProofStitch.p(Exercise.DynamicsItem.t),
      ~highlights,
    ) => {
  let Exercise.ProofStitch.{derivation_tree, prelude, _} = stitched_dynamics;

  let title_view = Cell.title_cell(header.title);

  let prompt_view =
    Cell.narrative_cell(
      div(~attr=Attr.class_("cell-prompt"), [header.prompt]),
    );

  let dropdown_option_view = (~pos, ~rule) =>
    div(
      ~attr=
        Attr.many([
          Attr.class_("rule-option"),
          Attr.on_click(_ =>
            inject(UpdateAction.SwitchDerivationRule(pos, rule))
          ),
        ]),
      [text(Derivation.Rule.repr(rule))],
    );

  let dropdown_options_view = (~pos): t =>
    div(
      ~attr=Attr.many([Attr.class_("rule-option-container")]),
      List.map(
        rule => dropdown_option_view(~pos, ~rule),
        Derivation.Rule.for_each(Fun.id),
      ),
    );

  let dropdown_res_view = (~res): t =>
    div(
      ~attr=Attr.class_("rule-block-info"),
      [
        text(
          res
          |> (
            fun
            | Correct => "✅"
            | NotAJudgment => "❓"
            | Partial => "✋"
            | Pending => "⌛️"
            | Incorrect(e) => "❌" ++ e
          ),
        ),
      ],
    );

  let dropdown_view = (~pos, ~res): t =>
    div(
      ~attr=
        Attr.many([
          Attr.class_("rule-block-content"),
          to_class(res),
          Attr.id(Util.Tree.show_pos(pos) ++ "-content"),
        ]),
      [dropdown_res_view(~res), dropdown_options_view(~pos)],
    );

  let rule_btn_on_mouseover_handler = (~pos) => {
    let show_pos = Util.Tree.show_pos(pos);
    let btn = JsUtil.get_elem_by_id(show_pos ++ "-btn");
    let content = JsUtil.get_elem_by_id(show_pos ++ "-content");
    let btn_rect = btn##getBoundingClientRect;
    let content_rect = content##getBoundingClientRect;
    let top =
      btn_rect##.top -. Js.Optdef.get(content_rect##.height, Fun.const(0.));
    let left =
      min(
        btn_rect##.left,
        float_of_int(Dom_html.window##.innerWidth)
        -. Js.Optdef.get(content_rect##.width, Fun.const(0.)),
      );
    content##setAttribute(
      Js.string("style"),
      // This may still cause the content to overflow the window
      Js.string(
        Printf.sprintf(
          "top: %fpx; left: %fpx;",
          top,
          left,
        ),
      ),
    );
    Ui_effect.Ignore;
  };

  let rule_btn_view = (~pos, ~res, ~rule) =>
    div(
      ~attr=
        Attr.many([
          Attr.class_("rule-block-btn"),
          to_class(res),
          Attr.id(Util.Tree.show_pos(pos) ++ "-btn"),
          Attr.on_mousemove(_ => rule_btn_on_mouseover_handler(~pos)),
        ]),
      [text(Derivation.Rule.repr(rule))],
    );

  let rule_block_view = (~pos, ~res, ~rule): t =>
    div(
      ~attr=Attr.many([Attr.class_("rule-block")]),
      [rule_btn_view(~pos, ~res, ~rule), dropdown_view(~pos, ~res)],
    );

  let add_premise_btn_view = (~pos, ~index) =>
    div(
      ~attr=
        Attr.many([
          Attr.class_("add-premise-btn"),
          Attr.on_click(_ => inject(UpdateAction.InsertPremise(pos, index))),
        ]),
      [text("+")],
    );

  let del_premise_btn_view = (~pos, ~index) =>
    div(
      ~attr=
        Attr.many([
          Attr.class_("add-premise-btn"),
          Attr.on_click(_ => inject(UpdateAction.RemovePremise(pos, index))),
        ]),
      [text("-")],
    );

  let premises_view = (~children_node, ~pos, ~res, ~rule): t => {
    let n = List.length(children_node);
    div(
      ~attr=Attr.many([Attr.class_("derivation-premises"), to_class(res)]),
      (
        children_node
        |> List.mapi((index, node) =>
             [
               add_premise_btn_view(~pos, ~index),
               node,
               del_premise_btn_view(~pos, ~index),
             ]
           )
        |> List.concat
      )
      @ [
        add_premise_btn_view(~pos, ~index=n),
        rule_block_view(~pos, ~res, ~rule),
      ],
    );
  };

  let conclusion_view =
      (~editor: Editor.t, ~di: Exercise.DynamicsItem.t, this_pos): t =>
    div(
      ~attr=Attr.class_("derivation-conclusion"),
      [
        Cell.editor_view(
          ~selected=(Proof(pos): Exercise.pos) == this_pos,
          ~error_ids=
            Statics.Map.error_ids(
              editor.state.meta.projected.term_ranges,
              di.info_map,
            ),
          ~inject,
          ~ui_state,
          ~mousedown_updates=[SwitchEditor(this_pos)],
          ~settings,
          ~highlights,
          ~target_id=Exercise.show_pos(this_pos),
          ~test_results=ModelResult.test_results(di.result),
          editor,
        ),
      ],
    );

  let single_derivation_view =
      (({Exercise.Proof.jdmt: ed, rule}, (pos, di)), children) => {
    let (children_node, children) = children |> List.split;
    let children_node = children_node |> List.concat;
    let (children_di, children_res) = children |> List.split;
    let res =
      Grading.ProofReport.DerivationReport.verify_single(
        di,
        rule,
        children_di,
      );
    let res = to_verifyRes(res, children_res);
    (
      [
        div(
          ~attr=Attr.class_("derivation-block"),
          [
            premises_view(~children_node, ~pos, ~res, ~rule),
            conclusion_view(~editor=ed, ~di, Proof(Derive(pos))),
          ],
        ),
      ],
      (di, res),
    );
  };

  // (ed * rule) * (pos * (di * res))
  let combined_tree =
    Util.Tree.combine((
      eds.derivation_tree,
      Util.Tree.mapi((pos, t) => (pos, t), derivation_tree),
    ));

  let derivation_view =
    Cell.simple_cell_view([
      Cell.simple_cell_item([
        Cell.caption("Derivation"),
        div(
          ~attr=Attr.class_("cell-derivation"),
          fst(Util.Tree.fold_deep(single_derivation_view, combined_tree)),
        ),
      ]),
    ]);

  let prelude_view =
    Cell.editor_view(
      ~selected=(Proof(pos): Exercise.pos) == Proof(Prelude),
      ~error_ids=
        Statics.Map.error_ids(
          eds.prelude.state.meta.projected.term_ranges,
          prelude.info_map,
        ),
      ~inject,
      ~ui_state,
      ~mousedown_updates=[SwitchEditor(Proof(Prelude))],
      ~settings,
      ~highlights,
      ~caption=Cell.caption("Prelude"),
      ~target_id=Exercise.show_pos(Proof(Prelude)),
      ~test_results=ModelResult.test_results(prelude.result),
      eds.prelude,
    );

  [title_view, prompt_view, prelude_view, derivation_view];
};
