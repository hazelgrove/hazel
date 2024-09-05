open Virtual_dom.Vdom;
open Haz3lcore;
open Node;
open Util;

open Haz3lschool.ProofGrade.F(Exercise.ExerciseEnv);
open Exercise.Proof;

module FakeCode = {
  let token_wrapper = (cls, s) =>
    span(~attrs=[Attr.class_(cls)], [text(s)]);
  let span_exp = token_wrapper("token default Exp poly");
  let span_var = token_wrapper("token default Exp mono");
  let span_pat = token_wrapper("token default Pat mono");
  let span_secondary = token_wrapper("secondary");
  let span_explicit_hole = token_wrapper("token explicit-hole Exp mono");
  let code_wrapper = code =>
    div(
      ~attrs=[Attr.class_("code fakecode")],
      [span(~attrs=[Attr.class_("code-text")], code)],
    );
};

type view_info = (pos, VerifiedTree.res, ed)
and ed =
  | Just(option(Derivation.Rule.t), Editor.t, Exercise.DynamicsItem.t)
  | Abbr(option(int));

let proof_view =
    (
      ~inject: UpdateAction.t => Ui_effect.t(unit),
      ~ui_state: Model.ui_state,
      ~settings: Settings.t,
      ~pos: pos,
      ~grading_report: GradingReport.t,
      ~eds: model(Editor.t),
      ~stitched_dynamics: stitched(Exercise.DynamicsItem.t),
      ~highlights,
    ) => {
  let init = () =>
    "" |> Exercise.zipper_of_code |> Editor.init(~settings=settings.core);

  let map_model = (f, state: Exercise.state): Exercise.state => {
    ...state,
    model:
      switch (state.model) {
      | Proof(m) => Proof(f(m))
      | _ => raise(Failure("Expected Exercise.Proof"))
      },
  };

  let add_premise_btn_view = (~pos, ~index) =>
    div(
      ~attrs=[
        Attr.class_("add-premise-btn"),
        Attr.on_click(_ =>
          inject(
            UpdateAction.MapExercise(
              map_model(Exercise.Proof.add_premise(~pos, ~index, ~init)),
            ),
          )
        ),
        // Attr.on_drop(drag_event => {
        //   let grabbed_pos =
        //     drag_event##.dataTransfer##getData(Js.string("pos"));
        //   print_endline(Js.to_string(grabbed_pos));
        //   Ui_effect.Ignore;
        // switch (grabbed_pos) {
        // | None => Ui_effect.Ignore
        // | Some(pos) =>
        //   inject(
        //     UpdateAction.MapExercise(
        //       map_model(add_premise(~pos, ~index)),
        //     ),
        //   )
        // }
        // }),
      ],
      [text("•")],
    );

  let del_premise_btn_view = (~pos) =>
    div(
      ~attrs=[
        Attr.class_("del-premise-btn"),
        Attr.id(show_pos(pos) ++ "-del-btn"),
        Attr.on_click(_ =>
          inject(
            UpdateAction.MapExercise(
              map_model(Exercise.Proof.del_premise(~pos)),
            ),
          )
        ),
      ],
      [text("x")],
    );

  let rule_to_label =
    fun
    | Some(rule) => Derivation.Rule.repr(rule)
    | None => "?";

  let abbr_to_label = index =>
    FakeCode.code_wrapper([
      switch (index) {
      | Some(index) => FakeCode.span_var("d" ++ string_of_int(index))
      | None => FakeCode.span_explicit_hole("?")
      },
    ]);

  let dropdown_option_abbr_view = (~pos: pos, ~index: option(int)) =>
    div(
      ~attrs=[
        Attr.class_("dropdown-option"),
        Attr.class_("abbr"),
        Attr.on_click(_ =>
          inject(
            UpdateAction.MapExercise(
              map_model(Exercise.Proof.switch_abbr(~pos, ~index)),
            ),
          )
        ),
      ],
      [abbr_to_label(index)],
    );

  let dropdown_option_abbr_container_view = (~pos): t =>
    div(
      ~attrs=[Attr.class_("dropdown-option-container")],
      Exercise.Proof.all_abbrs(pos)
      |> List.map(dropdown_option_abbr_view(~pos, ~index=_)),
    );

  let dropdown_result_view = (~res): t =>
    div(
      ~attrs=[Attr.class_("dropdown-result")],
      [text(VerifiedTree.show_res(res))],
    );

  let class_of_result: VerifiedTree.res => string =
    fun
    | Incorrect(_) => "incorrect"
    | Correct => "correct"
    | Pending(_) => "pending";

  let dropdown_view = (~pos, ~res: VerifiedTree.res): t =>
    div(
      ~attrs=[Attr.class_("dropdown"), Attr.class_(class_of_result(res))],
      [
        dropdown_result_view(~res),
        dropdown_option_abbr_container_view(~pos),
      ],
    );

  let label_view = (~pos, ~res, ~label) =>
    div(
      ~attrs=[
        Attr.class_("deduction-label"),
        Attr.class_(class_of_result(res)),
        Attr.on_click(_ => {
          NinjaKeysRules.pos := pos;
          // let nj = JsUtil.get_elem_by_id("ninja-keys-rules");
          // let em =
          //   nj##getElementsByTagName(Js_of_ocaml.Js.string("ninja-action"));
          // Js_of_ocaml.Dom.list_of_nodeList(em)
          // |> List.iter(e =>
          //      e##setAttribute(
          //        Js_of_ocaml.Js.string("style"),
          //        Js_of_ocaml.Js.string("display: none;"),
          //      )
          //    );
          NinjaKeysRules.open_command_palette();
          Effect.Ignore;
        }),
        // Attr.draggable(true),
        // Attr.on_dragstart(drag_event => {
        //   print_endline("drag_start");
        //   drag_event##.dataTransfer##setData(
        //     Js.string("pos"),
        //     Js.string(show_pos(pos)),
        //   );
        //   Ui_effect.Ignore;
        // }),
        // Attr.on_mousemove(_ => label_on_mouseover(~pos)),
      ],
      [text(label)],
    );

  let label_view = (~pos, ~res, ~label): t =>
    div(
      ~attrs=[Attr.class_("deduction-label-container")],
      [
        label_view(~pos, ~res, ~label),
        del_premise_btn_view(~pos),
        dropdown_view(~pos, ~res),
      ],
    );

  let premises_view = (~children_node, ~pos, ~res, ~rule) => {
    let n = List.length(children_node);
    let label = rule_to_label(rule);
    div(
      ~attrs=[
        Attr.class_("deduction-prems-label"),
        Attr.class_(class_of_result(res)),
      ],
      [
        div(
          ~attrs=[Attr.class_("deduction-prems")],
          List.init(n + 1, add_premise_btn_view(~pos, ~index=_))
          |> Aba.mk(_, children_node)
          |> Aba.join(Fun.id, Fun.id),
        ),
      ]
      @ [label_view(~pos, ~res, ~label)],
    );
  };

  let editor_view =
      (this_pos, ~editor, ~di: Exercise.DynamicsItem.t, ~caption) =>
    Cell.editor_view(
      ~selected=(Proof(pos): Exercise.pos) == this_pos,
      ~override_statics=di.statics,
      ~inject,
      ~ui_state,
      ~mousedown_updates=[SwitchEditor(this_pos)],
      ~settings,
      ~highlights,
      ~caption,
      ~target_id=Exercise.show_pos(this_pos),
      ~test_results=ModelResult.test_results(di.result),
      editor,
    );

  let conclusion_view = (~pos, ~editor, ~di) =>
    div(
      ~attrs=[Attr.class_("deduction-concl")],
      [editor_view(Proof(pos), ~editor, ~di, ~caption=None)],
    );

  let deduction_view = (~children_node, ~pos, ~res, ~rule, ~editor, ~di) =>
    div(
      ~attrs=[Attr.class_("deduction-just")],
      [
        premises_view(~children_node, ~pos, ~res, ~rule),
        conclusion_view(~pos, ~editor, ~di),
      ],
    );

  // TODO: Refactor this
  let abbreviation_view = (~pos, ~res, ~index) =>
    div(
      ~attrs=[Attr.class_("deduction-abbr")],
      [
        div(
          ~attrs=[
            Attr.class_("deduction-prems"),
            Attr.class_(class_of_result(res)),
          ],
          [label_view(~pos, ~res, ~label="•")],
        ),
        div(
          ~attrs=[Attr.class_("deduction-concl")],
          [abbr_to_label(index)],
        ),
      ],
    );

  let deduction_view = ((pos, res, ed): view_info, children_node: list(t)) =>
    switch (ed) {
    | Just(rule, editor, di) =>
      deduction_view(~children_node, ~pos, ~res, ~rule, ~editor, ~di)
    | Abbr(index) => abbreviation_view(~pos, ~res, ~index)
    };

  let abbr_wrapper = (i, t) => {
    open FakeCode;
    let upper_code =
      [
        span_exp("let"),
        span_secondary(" "),
        span_pat("d" ++ string_of_int(i)),
        span_secondary(" "),
        span_exp("="),
      ]
      |> code_wrapper;
    let lower_code = [span_exp("in")] |> code_wrapper;
    div([upper_code, t, lower_code]);
  };

  let add_abbr_btn_view = (~index) =>
    div(
      ~attrs=[
        Attr.class_("add-abbr-btn"),
        Attr.on_click(_ =>
          inject(
            UpdateAction.MapExercise(
              map_model(Exercise.Proof.add_abbr(~index, ~init)),
            ),
          )
        ),
      ],
      [text("•")],
    );

  // type view_info = (Exercise.pos, VerifiedTree.res, ed)
  // and ed =
  //   | Just(Derivation.Rule.t, Editor.t, Exercise.DynamicsItem.t)
  //   | Abbr(index);

  let derivation_wrapper = l =>
    switch (List.rev(l)) {
    | [] => []
    | [hd, ...tl] => (tl |> List.rev |> List.mapi(abbr_wrapper)) @ [hd]
    };

  let add_abbr_btn_wrapper = l => {
    let btns = List.init(List.length(l) + 1, add_abbr_btn_view(~index=_));
    Aba.mk(btns, l) |> Aba.join(Fun.id, Fun.id);
  };

  let info_tree =
    List.map2(Tree.combine, eds.trees, stitched_dynamics.trees)
    |> List.map(
         Tree.map(
           fun
           | (Abbr.Just({jdmt, rule}), Some(di)) => Just(rule, jdmt, di)
           | (Abbr(i), _) => Abbr(i)
           | _ => raise(Failure("DerivationTree.mk: ed<>di inconsistent")),
         ),
       )
    |> List.map2(Tree.combine, grading_report.proof_report.verified_tree)
    |> List.mapi(i =>
         Tree.mapi((pos, (res, ed)) => (Trees(i, pos), res, ed))
       );

  let derivation_view =
    div(
      ~attrs=[Attr.classes(["cell deselected unlocked"])],
      [
        Cell.simple_cell_item([
          Cell.caption("Derivation"),
          div(
            ~attrs=[Attr.class_("cell-derivation")],
            info_tree
            |> List.map(Tree.fold_deep(deduction_view))
            |> derivation_wrapper
            |> add_abbr_btn_wrapper,
          ),
        ]),
      ],
    );

  let prelude_view =
    editor_view(
      Proof(Prelude),
      ~editor=eds.prelude,
      ~di=stitched_dynamics.prelude,
      ~caption=
        Cell.caption(
          "Prelude",
          ~rest=settings.instructor_mode ? "" : " (Read-Only)",
        ),
    );

  let setup_view =
    editor_view(
      Proof(Setup),
      ~editor=eds.setup,
      ~di=stitched_dynamics.setup,
      ~caption=Cell.caption("Setup"),
    );

  [prelude_view, setup_view, derivation_view];
};
