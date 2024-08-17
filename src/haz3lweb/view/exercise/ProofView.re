open Virtual_dom.Vdom;
open Js_of_ocaml;
open Haz3lcore;
open Node;
open Util;

open Haz3lschool.ProofGrade.F(Exercise.ExerciseEnv);
open Exercise.Proof;

let get_model = (state: Exercise.p('a)) =>
  switch (state.model) {
  | Exercise.Proof(m) => m
  | _ => raise(Failure("Expected Exercise.Proof"))
  };

let to_class = (res: VerifiedTree.res) =>
  Attr.class_(
    switch (res) {
    | Ok({err: Some(_), _}) => "incorrect"
    | Ok(_) => "correct"
    | Error(_) => "pending"
    },
  );

type view_info = (pos, VerifiedTree.res, ed)
and ed =
  | Just(Derivation.Rule.t, Editor.t, Exercise.DynamicsItem.t)
  | Abbr(index);

let proof_view =
    (
      ~inject,
      ~ui_state: Model.ui_state,
      ~settings: Settings.t,
      ~pos: pos,
      ~grading_report: GradingReport.t,
      ~eds: model(Editor.t),
      ~stitched_dynamics: stitched(Exercise.DynamicsItem.t),
      ~highlights,
    ) => {
  let {prelude, _} = stitched_dynamics;
  // TODO: Implement this

  let dropdown_option_view = (~pos: pos, ~rule) =>
    div(
      ~attrs=[
        Attr.class_("rule-option"),
        Attr.on_click(_ =>
          inject(
            UpdateAction.MapExercise(
              state =>
                {
                  ...state,
                  model:
                    Proof(
                      ModelUtil.switch_derivation_rule(
                        ~pos,
                        ~m=state |> get_model,
                        ~rule,
                      ),
                    ),
                },
            ),
          )
        ),
      ],
      [text(Derivation.Rule.repr(rule))],
    );

  let dropdown_options_view = (~pos): t =>
    div(
      ~attrs=[Attr.class_("rule-option-container")],
      List.map(
        rule => dropdown_option_view(~pos, ~rule),
        Derivation.Rule.all,
      ),
    );

  let dropdown_res_view = (~res): t =>
    div(
      ~attrs=[Attr.class_("rule-block-info")],
      [
        text(
          (res: VerifiedTree.res)
          |> (
            fun
            | Ok({err: Some(err), _}) =>
              "❌" ++ (err |> DerivationError.repr)
            | Ok(_) => "✅"
            | Error(_) => "⌛️"
          ),
        ),
      ],
    );

  let dropdown_view = (~pos, ~res): t =>
    div(
      ~attrs=[
        Attr.class_("rule-block-content"),
        to_class(res),
        Attr.id(show_pos(pos) ++ "-content"),
      ],
      [dropdown_res_view(~res), dropdown_options_view(~pos)],
    );

  let rule_btn_on_mouseover_handler = (~pos) => {
    let show_pos = show_pos(pos);
    let btn = Util.JsUtil.get_elem_by_id(show_pos ++ "-btn");
    let content = Util.JsUtil.get_elem_by_id(show_pos ++ "-content");
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
        "top:"
        ++ string_of_float(top)
        ++ "px; left:"
        ++ string_of_float(left)
        ++ "px;",
      ),
    );
    Ui_effect.Ignore;
  };

  let rule_btn_view = (~pos, ~res, ~rule) =>
    div(
      ~attrs=[
        Attr.class_("rule-block-btn"),
        to_class(res),
        Attr.id(show_pos(pos) ++ "-btn"),
        Attr.on_mousemove(_ => rule_btn_on_mouseover_handler(~pos)),
      ],
      [text(Derivation.Rule.repr(rule))],
    );

  let rule_block_view = (~pos, ~res, ~rule): t =>
    div(
      ~attrs=[Attr.class_("rule-block")],
      [rule_btn_view(~pos, ~res, ~rule), dropdown_view(~pos, ~res)],
    );

  let add_premise_btn_view = (~pos, ~index) =>
    div(
      ~attrs=[
        Attr.class_("add-premise-btn"),
        Attr.on_click(_ =>
          inject(
            UpdateAction.MapExercise(
              state =>
                {
                  ...state,
                  model:
                    Exercise.Proof(
                      ModelUtil.add_premise(
                        ~pos,
                        ~m=state |> get_model,
                        ~index,
                        ~init=
                          ""
                          |> Exercise.zipper_of_code
                          |> Editor.init(~settings=settings.core)
                          |> Fun.const,
                      ),
                    ),
                },
            ),
          )
        ),
      ],
      [text("+")],
    );

  let del_premise_btn_view = (~pos, ~index) =>
    div(
      ~attrs=[
        Attr.class_("add-premise-btn"),
        Attr.on_click(_ =>
          inject(
            UpdateAction.MapExercise(
              (state): Exercise.state =>
                {
                  ...state,
                  pos: {
                    // TODO(zhiyao): avoid direct to the deleted premise
                    switch (pos, state.pos) {
                    | (Trees(i, pos), Proof(pos'))
                        when
                          Trees(
                            i,
                            Tree.pos_concat(
                              Tree.Children(index, Value),
                              pos,
                            ),
                          )
                          == pos' =>
                      Proof(Trees(i, pos))
                    | _ => state.pos
                    };
                  },
                  model:
                    Exercise.Proof(
                      ModelUtil.del_premise(
                        ~pos,
                        ~m=state |> get_model,
                        ~index,
                      ),
                    ),
                },
            ),
          )
        ),
      ],
      [text("-")],
    );

  let premises_view = (~children_node, ~pos, ~res, ~rule): t => {
    let n = List.length(children_node);
    div(
      ~attrs=[Attr.class_("derivation-premises"), to_class(res)],
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
      ~attrs=[Attr.class_("derivation-conclusion")],
      [
        Cell.editor_view(
          ~selected=(Proof(pos): Exercise.pos) == this_pos,
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

  let single_derivation_view = ((pos, res, ed), children_node: list(t)): t => {
    switch (ed) {
    | Just(rule, ed, di) =>
      div(
        ~attrs=[Attr.class_("derivation-block")],
        [
          premises_view(~children_node, ~pos, ~res, ~rule),
          conclusion_view(~editor=ed, ~di, Proof(pos)),
        ],
      )
    | Abbr(i) => text(string_of_int(i))
    };
  };

  // type view_info = (Exercise.pos, VerifiedTree.res, ed)
  // and ed =
  //   | Just(Derivation.Rule.t, Editor.t, Exercise.DynamicsItem.t)
  //   | Abbr(index);

  let info_tree: list(Tree.p(view_info)) =
    List.map2(Tree.combine, eds.trees, stitched_dynamics.trees)
    |> List.map(
         Tree.map(
           fun
           | (Exercise.Proof.Just({jdmt: ed, rule}), Some(di)) =>
             Just(rule, ed, di)
           | (Abbr(i), _) => Abbr(i)
           | _ => raise(Failure("DerivationTree.mk: ed<>di inconsistent")),
         ),
       )
    |> List.map2(Tree.combine, grading_report.proof_report.verified_tree)
    |> List.mapi(i =>
         Tree.mapi((pos, (res, ed)) => (Trees(i, pos), res, ed))
       );

  let derivation_view =
    Cell.simple_cell_view([
      Cell.simple_cell_item([
        Cell.caption("Derivation"),
        div(
          ~attrs=[Attr.class_("cell-derivation")],
          info_tree |> List.map(Tree.fold_deep(single_derivation_view)),
        ),
      ]),
    ]);

  let prelude_view =
    Cell.editor_view(
      ~selected=(Proof(pos): Exercise.pos) == Proof(Prelude),
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

  [prelude_view, derivation_view];
};
