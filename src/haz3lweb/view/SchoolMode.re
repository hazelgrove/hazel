open Virtual_dom.Vdom;
open Node;

module CoverageReport = {
  // TODO move to separate module
  open Haz3lcore;
  module DynamicsItem = SchoolExercise.DynamicsItem;

  let coverage_summary_str = (~total, ~found): string => {
    TestView.result_summary_str(
      ~n=total,
      ~p=found,
      ~q=0,
      ~n_str="bug",
      ~ns_str="bugs",
      ~p_str="exposed",
      ~q_str="",
      ~r_str="unrevealed",
    );
  };

  let coverage_text = (~total, ~found): Node.t =>
    div(
      ~attr=Attr.classes(["test-text"]),
      [
        TestView.percent_view(total, found),
        div([text(":")]),
        text(coverage_summary_str(~total, ~found)),
      ],
    );

  let coverage_bar = (~inject as _, instances) =>
    div(
      ~attr=Attr.classes(["test-bar"]),
      List.map(
        ((status, _)) =>
          div(
            ~attr=Attr.classes(["segment", TestStatus.to_string(status)]),
            [],
          ),
        instances,
      ),
    );

  let coverage_summary = (~inject, instances) => {
    let total = List.length(instances);
    let found =
      List.length(
        List.filter(((x: TestStatus.t, _)) => x == Pass, instances),
      );
    let status_class = total == found ? "Pass" : "Fail";
    div(
      ~attr=Attr.classes(["test-summary", status_class]),
      [coverage_text(~total, ~found), coverage_bar(~inject, instances)],
    );
  };

  let individual_report = (i, ~inject, ~font_metrics, ~hint: string, ~status) =>
    div(
      ~attr=
        Attr.many([
          Attr.classes(["test-report"]),
          Attr.on_click(TestView.jump_to_test(~inject)),
        ]),
      [
        div(
          ~attr=
            Attr.classes([
              "test-id",
              "Test" ++ TestStatus.to_string(status),
            ]),
          /* NOTE: prints lexical index, not unique id */
          [text(string_of_int(i + 1))],
        ),
        // TestView.test_instance_view(~font_metrics, instance),
      ]
      @ [
        div(
          ~attr=
            Attr.classes([
              "test-hint",
              "test-instance",
              TestStatus.to_string(status),
            ]),
          [text(hint)],
        ),
      ],
    );

  let individual_reports = (~inject, ~font_metrics, coverage_results) =>
    div(
      coverage_results
      |> List.mapi((i, (status, hint)) =>
           individual_report(i, ~inject, ~font_metrics, ~hint, ~status)
         ),
    );

  let passing_test_ids = test_map =>
    test_map
    |> List.filter(((_id, reports)) =>
         List.for_all(
           ((_, status)) => status == Haz3lcore.TestStatus.Pass,
           reports,
         )
       )
    |> List.split
    |> fst;

  let failing_test_ids = test_map =>
    test_map
    |> List.filter(((_id, reports)) =>
         List.for_all(
           ((_, status)) => status == Haz3lcore.TestStatus.Fail,
           reports,
         )
       )
    |> List.split
    |> fst;

  // let get_test_map = (editors: list(Haz3lcore.Editor.t)) => {
  //   let (reference_term, reference_map) = spliced_statics(editors);
  //   let result_reference =
  //     Interface.test_results(reference_map, reference_term);
  //   switch (result_reference) {
  //   | None => []
  //   | Some(test_results) => test_results.test_map
  //   };
  // };
  // let show_term = (editor: Editor.t, _) =>
  //   editor.state.zipper
  //   |> Zipper.zip
  //   |> MakeTerm.go
  //   |> fst
  //   |> Term.UExp.show
  //   |> print_endline
  //   |> (_ => Virtual_dom.Vdom.Effect.Ignore);

  // let get_first_common =
  //     (reference_passing, wrong): (TestStatus.t, option('a)) => {
  //   let wrong_test_map = wrong |> get_test_map;
  //   let wrong_failing = wrong_test_map |> failing_test_ids;
  //   let common =
  //     List.filter(x => List.mem(x, reference_passing), wrong_failing);
  //   let instance: option(list('a)) =
  //     switch (common) {
  //     | [] => None
  //     | [x, ..._] => List.assoc_opt(x, wrong_test_map)
  //     };
  //   switch (instance) {
  //   | Some([instance, ..._]) => (TestStatus.Pass, Some(instance))
  //   | _ => (TestStatus.Fail, None)
  //   };
  // };

  let hidden_bug_status =
      (test_validation_data: DynamicsItem.t, hidden_bug_data: DynamicsItem.t)
      : TestStatus.t => {
    switch (test_validation_data.simple_result, hidden_bug_data.simple_result) {
    | (None, _)
    | (_, None) => Indet
    | (Some(test_validation_data), Some(hidden_bug_data)) =>
      let validation_test_map = test_validation_data.test_results.test_map;
      let hidden_bug_test_map = hidden_bug_data.test_results.test_map;

      let found =
        hidden_bug_test_map
        |> List.find_opt(((id, instance_reports)) => {
             let status = TestMap.joint_status(instance_reports);
             switch (status) {
             | TestStatus.Pass
             | TestStatus.Indet => false
             | TestStatus.Fail =>
               let validation_test_reports =
                 validation_test_map |> TestMap.lookup(id);
               switch (validation_test_reports) {
               | None => false
               | Some(reports) =>
                 let status = TestMap.joint_status(reports);
                 switch (status) {
                 | TestStatus.Pass => true
                 | TestStatus.Fail
                 | TestStatus.Indet => false
                 };
               };
             };
           });
      switch (found) {
      | None => Fail
      | Some(_) => Pass
      };
    };
  }; // for each hidden bug
  //   in the test results data, find a test ID that passes test validation but fails against

  let view =
      (
        ~font_metrics,
        ~inject,
        ~test_validation_data: SchoolExercise.DynamicsItem.t,
        ~hidden_bugs_state: list(SchoolExercise.wrong_impl(Editor.t)),
        ~hidden_bugs_data: list(SchoolExercise.DynamicsItem.t),
      ) => {
    let results =
      List.map(hidden_bug_status(test_validation_data), hidden_bugs_data);
    let hints =
      List.map(
        (wrong_impl: SchoolExercise.wrong_impl(Editor.t)) => wrong_impl.hint,
        hidden_bugs_state,
      );
    let coverage_results = List.combine(results, hints);
    Cell.simple_cell_view([
      div(
        ~attr=Attr.classes(["panel", "test-panel"]),
        [
          Cell.simple_caption(
            "Acceptance Testing (Your Tests vs. Buggy Implementations)",
          ),
          individual_reports(~inject, ~font_metrics, coverage_results),
          coverage_summary(~inject, coverage_results),
        ],
      ),
    ]);
  };
};

// let show_term = (editor: Editor.t, _) =>
//   editor.state.zipper
//   |> Zipper.zip
//   |> MakeTerm.go
//   |> Term.UExp.show
//   |> print_endline
//   |> (_ => Virtual_dom.Vdom.Effect.Ignore);

// let cell_view =
//     (
//       ~result_bar: list(Node.t)=[],
//       ~settings: Model.settings,
//       ~inject: Update.t => 'a,
//       ~font_metrics,
//       ~selected,
//       ~mousedown,
//       ~show_backpack_targets,
//       ~show_code=true,
//       ~overlays=[],
//       idx,
//       editor: Editor.t,
//     ) => {
//   let zipper = editor.state.zipper;
//   let unselected = Zipper.unselect_and_zip(zipper);
//   let cell_caption_view =
//     //TODO(andrew): diable show term on release!!
//     div(
//       ~attr=
//         Attr.many([
//           Attr.classes(["cell-caption"]),
//           Attr.on_click(show_term(editor)),
//         ]),
//       [text(List.nth(School.captions, idx))],
//     );

//   let code_container_id = "code-container-" ++ string_of_int(idx);
//   let code_view =
//     SimpleMode.code_container(
//       ~id=code_container_id,
//       ~font_metrics,
//       ~unselected,
//       ~settings,
//       ~overlays,
//       ~show_backpack_targets,
//       ~show_deco=selected == idx,
//       ~measured=editor.state.meta.measured,
//       zipper,
//     );
//   let mousedown_overlay =
//     selected == idx && mousedown
//       ? [
//         SimpleMode.mousedown_overlay(
//           ~inject,
//           ~font_metrics,
//           ~target_id=code_container_id,
//         ),
//       ]
//       : [];
//   div(
//     [clss(["cell-container"])],
//     [cell_chapter_view]
//     @ [
//       div(
//         [
//           Attr.classes(["cell"] @ (selected == idx ? ["selected"] : [])),
//           Attr.on_mousedown(
//             SimpleMode.mousedown_handler(
//               ~inject,
//               ~font_metrics,
//               ~target_id=code_container_id,
//               ~additional_updates=[Update.SwitchEditor(idx)],
//             ),
//           ),
//         ],
//         [cell_caption_view]
//         @ (show_code ? mousedown_overlay @ [code_view] : []),
//       ),
//     ]
//     @ result_bar,
//   );
// };

// let get_school_data = (editors: list(Editor.t)) => {
//   switch (editors) {
//   | [
//       student_impl,
//       student_tests,
//       hidden_tests,
//       correct_impl,
//       wrong_impl_1,
//       wrong_impl_2,
//       wrong_impl_3,
//     ] =>
//     /* Note: splicing in student implementation
//        first in case they create helpers. Still
//        has problem if these get shadowed; make
//        sure we use uncommon names for helpers. */
//     Some((
//       [student_impl],
//       [student_impl, student_tests],
//       [student_impl, hidden_tests],
//       [student_impl, correct_impl, student_tests],
//       [
//         [student_impl, wrong_impl_1, student_tests],
//         [student_impl, wrong_impl_2, student_tests],
//         [student_impl, wrong_impl_3, student_tests],
//       ],
//     ))
//   | _ => None
//   };
// };

// let test_status_icon_view =
//     (~font_metrics, insts, ms: Measureds.Shards.t): option(Node.t) =>
//   switch (ms) {
//   | [(_, {origin: _, last}), ..._] =>
//     let status = insts |> TestMap.joint_status |> TestStatus.to_string;
//     let pos = DecUtil.abs_position(~font_metrics, last);
//     Some(div([clss(["test-result", status]), pos], []));
//   | _ => None
//   };

type vis_marked('a) =
  | InstructorOnly(unit => 'a)
  | Always('a);

let render_cells = (settings: Model.settings, v: list(vis_marked(Node.t))) => {
  List.filter_map(
    vis =>
      switch (vis) {
      | InstructorOnly(f) => settings.instructor_mode ? Some(f()) : None
      | Always(node) => Some(node)
      },
    v,
  );
};

let view =
    (
      ~font_metrics,
      ~show_backpack_targets,
      ~mousedown,
      ~state: SchoolExercise.state,
      ~results: option(ModelResults.t),
      ~settings,
      ~inject,
    ) => {
  let SchoolExercise.{pos, eds} = state;

  // partially apply for convenience below
  let editor_view = pos =>
    Cell.editor_view(
      ~inject,
      ~font_metrics,
      ~show_backpack_targets,
      ~mousedown,
      ~mousedown_updates=[
        Update.SwitchEditor(SchoolExercise.idx_of_pos(pos, eds)),
      ],
      ~settings,
    );

  // let cell_view =
  //   cell_view(
  //     ~settings,
  //     ~inject,
  //     ~font_metrics,
  //     ~mousedown,
  //     ~selected,
  //     ~show_backpack_targets,
  //   );
  // let combined_info_map =
  //   settings.statics
  //     ? {
  //       let (_, combined_info_map) = spliced_statics(editors);
  //       combined_info_map;
  //     }
  //     : Id.Map.empty;
  // let school_view_data = settings.dynamics ? get_school_data(editors) : None;
  // let your_test_results = {
  //   let* (_, your_tests, _, _, _) = school_view_data;
  //   let (term, map) = spliced_statics(your_tests);
  //   Interface.test_results(map, term);
  // };
  // let our_test_results = {
  //   let* (_, _, our_tests, _, _) = school_view_data;
  //   let (term, map) = spliced_statics(our_tests);
  //   let descriptions = School.TheExercise.hidden_test_descriptions;
  //   Interface.test_results(~descriptions, map, term);
  // };
  // let first_cell_res = {
  //   let* (statics_impl, _, _, _, _) = school_view_data;
  //   let (term, map) = spliced_statics(statics_impl);
  //   Interface.evaulation_result(map, term);
  // };
  // let student_imp_res_view =
  //   switch (first_cell_res) {
  //   | None => []
  //   | Some(dhexp) => [
  //       div(
  //         [clss(["cell-result"])],
  //         [SimpleMode.res_view(~font_metrics, dhexp)],
  //       ),
  //     ]
  //   };
  // let coverage_view =
  //   switch (school_view_data) {
  //   | Some((_, _, _, reference_tests, coverage_tests)) =>
  //     let descriptions = School.TheExercise.wrong_implementation_descriptions;
  //     [
  //       coverage_view(
  //         ~inject,
  //         ~font_metrics,
  //         ~descriptions,
  //         reference_tests,
  //         coverage_tests,
  //       ),
  //     ];
  //   | None => []
  //   };
  // let your_tests_view =
  //   switch (your_test_results) {
  //   | None => []
  //   | Some(test_results) => [TestView.test_summary(~inject, ~test_results)]
  //   };
  // let your_tests_layer =
  //   switch (your_test_results, editors) {
  //   | (Some(test_results), [_, your_tests, ..._]) =>
  //     let map = Measureds.of_segment(splice_editors([your_tests]));
  //     test_result_layer(~font_metrics, ~map: Measureds.t, test_results);
  //   | _ => []
  //   };
  // let our_tests_view =
  //   switch (our_test_results) {
  //   | None => []
  //   | Some(test_results) => [
  //       div(
  //         [clss(["cell", "cell-result"])],
  //         [
  //           TestView.test_summary(~inject, ~test_results),
  //           TestView.test_reports_view(~inject, ~font_metrics, ~test_results),
  //         ],
  //       ),
  //     ]
  //   };
  // let school_panel = [div([clss(["school-panel"])], coverage_view)];
  // let ci_view =
  //   settings.statics
  //     ? {
  //       [
  //         CursorInspector.view(
  //           ~inject,
  //           ~settings,
  //           Indicateds.index(focal_zipper),
  //           combined_info_map,
  //         ),
  //       ]
  //       @ (
  //         switch (Indicateds.index(focal_zipper), your_test_results) {
  //         | (Some(index), Some({test_map, _})) =>
  //           let view =
  //             TestView.inspector_view(
  //               ~inject,
  //               ~font_metrics,
  //               ~test_map,
  //               index,
  //             );
  //           switch (view) {
  //           | None => []
  //           | Some(view) => [view]
  //           };
  //         | _ => []
  //         }
  //       );
  //     }
  //     : [];
  // let prompt_view = prompt => div([clss(["cell-prompt"])], [prompt]);
  // let prelude_view = code => instructor_code_cell_view("Prelude", code);
  // let correct_impl_view = code =>
  //   instructor_code_cell_view("Reference Implementation", code);
  // let your_tests_view = code => student_code_cell_view("Your Tests", code);
  // let your_implementation_view = code =>
  //   student_code_cell_view("Your Implementation", code);
  // div(
  //   [Attr.classes(["editor", "column"])],
  //   (
  //     List.map(
  //       ((cell: SchoolCell.t, ed: option(Editor.t))) =>
  //         switch (cell) {
  //         | Prompt({content}) => [prompt_view(content)]
  //         | Prelude(_) => [prelude_view(Option.get(ed))]
  //         | CorrectImpl(_) => [correct_impl_view(Option.get(ed))]
  //         | YourTests(_) => [your_tests_view(Option.get(ed))]
  //         | YourImpl(_) => [your_implementation_view(Option.get(ed))]
  //         | HiddenBug(_) => [hidden_bug_view(Option.get(ed))]
  //         | HiddenTests({tests, descriptions}) => []
  //         },
  //       // switch (i) {
  //       // | 0 => [cell_view(~result_bar=student_imp_res_view, i, ed)]
  //       // | 1 =>
  //       //   [
  //       //     cell_view(
  //       //       ~result_bar=[
  //       //         div([clss(["cell", "cell-result"])], your_tests_view),
  //       //       ],
  //       //       ~overlays=your_tests_layer,
  //       //       i,
  //       //       ed,
  //       //     ),
  //       //   ]
  //       //   @ (settings.dynamics ? school_panel : [])
  //       // | 2 => [
  //       //     cell_view(
  //       //       ~show_code=!settings.student,
  //       //       ~result_bar=our_tests_view,
  //       //       i,
  //       //       ed,
  //       //     ),
  //       //   ]
  //       // | _ => [
  //       //     settings.student
  //       //       ? div([Attr.create("style", "display: none;")], [])
  //       //       : cell_view(i, ed),
  //       //   ]
  //       // },
  //       cells,
  //     )
  //     |> List.flatten
  //   )
  //   @ [div([clss(["bottom-bar"])], ci_view)],
  // );
  //   let combined_info_map =
  //     settings.statics
  //       ? {
  //         let (_, combined_info_map) = spliced_statics(editors);
  //         combined_info_map;
  //       }
  //       : Id.Map.empty;
  //   let school_view_data = settings.dynamics ? get_school_data(editors) : None;
  //   let your_test_results = {
  //     let* (_, your_tests, _, _, _) = school_view_data;
  //     let (term, map) = spliced_statics(your_tests);
  //     /* FIXME: Replace call with use of model. */
  //     Interface.test_results(map, term);
  //   };
  //   let our_test_results = {
  //     let* (_, _, our_tests, _, _) = school_view_data;
  //     let (term, map) = spliced_statics(our_tests);
  //     let descriptions = School.hidden_test_descriptions;
  //     /* FIXME: Replace call with use of model. */
  //     Interface.test_results(~descriptions, map, term);
  //   };
  //   let first_cell_res = {
  //     let* (statics_impl, _, _, _, _) = school_view_data;
  //     let (term, map) = spliced_statics(statics_impl);
  //     /* FIXME: Replace call with use of model. */
  //     Interface.evaluation_result(map, term);
  //   };
  //   let student_imp_res_view =
  //     switch (first_cell_res) {
  //     | None => []
  //     | Some(dhexp) => [
  //         div(
  //           ~attr=clss(["cell-result"]),
  //           [SimpleMode.res_view(~font_metrics, dhexp)],
  //         ),
  //       ]
  //     };
  //   let coverage_view =
  //     switch (school_view_data) {
  //     | Some((_, _, _, reference_tests, coverage_tests)) =>
  //       let descriptions = School.wrong_implementation_descriptions;
  //       [
  //         coverage_view(
  //           ~inject,
  //           ~font_metrics,
  //           ~descriptions,
  //           reference_tests,
  //           coverage_tests,
  //         ),
  //       ];
  //     | None => []
  //     };

  let prompt_view =
    Cell.narrative_cell(
      div(~attr=Attr.class_("cell-prompt"), [eds.prompt]),
    );

  let SchoolExercise.{
        test_validation,
        user_impl,
        user_tests,
        instructor,
        hidden_bugs,
      } =
    SchoolExercise.stitch_dynamic(state, results);

  let (focal_zipper, focal_info_map) =
    switch (pos) {
    | Prelude => (eds.prelude.state.zipper, user_tests.info_map)
    | CorrectImpl => (eds.correct_impl.state.zipper, instructor.info_map)
    | YourTests => (eds.your_tests.state.zipper, test_validation.info_map)
    | YourImpl => (eds.your_impl.state.zipper, user_impl.info_map)
    | HiddenBugs(idx) =>
      let editor = List.nth(eds.hidden_bugs, idx).impl;
      let info_map = List.nth(hidden_bugs, idx).info_map;
      (editor.state.zipper, info_map);
    | HiddenTests => (
        eds.hidden_tests.tests.state.zipper,
        instructor.info_map,
      )
    };

  let prelude_view =
    Always(
      editor_view(
        Prelude,
        ~selected=pos == Prelude,
        ~caption=
          Cell.simple_caption(
            "Prelude" ++ (settings.instructor_mode ? "" : " (Read-Only)"),
          ),
        ~code_id="prelude",
        ~info_map=user_tests.info_map, // TODO this is wrong for top-level let types
        ~test_results=
          ModelResult.unwrap_test_results(user_tests.simple_result),
        ~footer=None,
        eds.prelude,
      ),
    );

  let correct_impl_view =
    InstructorOnly(
      () =>
        editor_view(
          CorrectImpl,
          ~selected=pos == CorrectImpl,
          ~caption=Cell.simple_caption("Correct Implementation"),
          ~code_id="correct-impl",
          ~info_map=instructor.info_map,
          ~test_results=
            ModelResult.unwrap_test_results(instructor.simple_result),
          ~footer=None,
          eds.correct_impl,
        ),
    );

  // determine trailing hole
  let correct_impl_ctx_view =
    Always(
      {
        let exp_ctx_view = {
          let correct_impl_trailing_hole_ctx =
            Haz3lcore.Editor.trailing_hole_ctx(
              eds.correct_impl,
              instructor.info_map,
            );
          let prelude_trailing_hole_ctx =
            Haz3lcore.Editor.trailing_hole_ctx(
              eds.prelude,
              instructor.info_map,
            );
          switch (correct_impl_trailing_hole_ctx, prelude_trailing_hole_ctx) {
          | (None, _)
          | (_, None) => Node.div([text("No context available")]) // TODO show exercise configuration error
          | (
              Some(correct_impl_trailing_hole_ctx),
              Some(prelude_trailing_hole_ctx),
            ) =>
            let specific_ctx =
              Haz3lcore.Ctx.subtract_prefix(
                correct_impl_trailing_hole_ctx,
                prelude_trailing_hole_ctx,
              );
            switch (specific_ctx) {
            | None => Node.div([text("No context available")]) // TODO show exercise configuration error
            | Some(specific_ctx) => CtxInspector.exp_ctx_view(specific_ctx)
            };
          };
        };
        Cell.simple_cell_view([
          Cell.simple_cell_item([
            Cell.simple_caption(
              "Correct Implementation (Type Signatures Only)",
            ),
            exp_ctx_view,
          ]),
        ]);
      },
    );

  let test_validation_results =
    ModelResult.unwrap_test_results(test_validation.simple_result);

  let your_tests_view =
    Always(
      editor_view(
        YourTests,
        ~selected=pos == YourTests,
        ~caption=Cell.simple_caption("Your Tests"),
        ~code_id="your-tests",
        ~info_map=test_validation.info_map,
        ~test_results=
          ModelResult.unwrap_test_results(test_validation.simple_result),
        ~footer=
          Option.map(
            test_validation_results =>
              Cell.test_report_footer_view(
                ~inject,
                ~test_results=test_validation_results,
                ~title=
                  Cell.simple_caption(
                    "Test Validation (Your Tests vs. Correct Implementation)",
                  ),
              ),
            test_validation_results,
          ),
        eds.your_tests,
      ),
    );

  let hidden_bugs_views =
    List.mapi(
      (
        i,
        (
          SchoolExercise.{impl, _},
          SchoolExercise.DynamicsItem.{info_map, simple_result, _},
        ),
      ) => {
        InstructorOnly(
          () =>
            editor_view(
              HiddenBugs(i),
              ~selected=pos == HiddenBugs(i),
              ~caption=
                Cell.simple_caption(
                  "Wrong Implementation " ++ string_of_int(i + 1),
                ),
              ~code_id="wrong-implementation-" ++ string_of_int(i + 1),
              ~info_map,
              ~test_results=ModelResult.unwrap_test_results(simple_result),
              ~footer=None,
              impl,
            ),
        )
      },
      List.combine(eds.hidden_bugs, hidden_bugs),
    );

  let coverage_report_view =
    Always(
      CoverageReport.view(
        ~font_metrics,
        ~inject,
        ~test_validation_data=test_validation,
        ~hidden_bugs_state=eds.hidden_bugs,
        ~hidden_bugs_data=hidden_bugs,
      ),
    );

  let your_impl_view =
    Always(
      editor_view(
        YourImpl,
        ~selected=pos == YourImpl,
        ~caption=Cell.simple_caption("Your Implementation"),
        ~code_id="your-impl",
        ~info_map=user_impl.info_map,
        ~test_results=
          ModelResult.unwrap_test_results(user_impl.simple_result),
        ~footer=
          Some(
            Cell.eval_result_footer_view(
              ~font_metrics,
              user_impl.simple_result,
            ),
          ),
        eds.your_impl,
      ),
    );

  let hidden_tests_view =
    InstructorOnly(
      () =>
        editor_view(
          HiddenTests,
          ~selected=pos == HiddenTests,
          ~caption=Cell.simple_caption("Hidden Tests"),
          ~code_id="hidden-tests",
          ~info_map=instructor.info_map,
          ~test_results=
            ModelResult.unwrap_test_results(instructor.simple_result),
          ~footer=None,
          eds.hidden_tests.tests,
        ),
    );

  let hidden_test_results_view =
    Always(
      switch (ModelResult.unwrap_test_results(user_tests.simple_result)) {
      | None => Node.div([text("No test results available.")])
      | Some(test_results) =>
        TestView.test_reports_view(~inject, ~font_metrics, ~test_results)
      },
    );

  let ci_view =
    settings.statics
      ? [
        CursorInspector.view(
          ~inject,
          ~settings,
          focal_zipper,
          focal_info_map,
        ),
      ]
      : [];
  // @ (
  //   switch (Indicated.index(focal_zipper), your_test_results) {
  //   | (Some(index), Some({test_map, _})) =>
  //     let view =
  //       TestView.inspector_view(
  //         ~inject,
  //         ~font_metrics,
  //         ~test_map,
  //         index,
  //       );
  //     switch (view) {
  //     | None => []
  //     | Some(view) => [view]
  //     };
  //   | _ => []
  //   }
  // );

  // TODO: run prelude + your implementation to display result
  // TODO: run prelude + your implementation + your tests to evaluate your implementation
  // TODO: run prelude + reference implementation + your tests to evaluate tests + generate report
  // TODO: run prelude + your implementation (for helpers) + each wrong implementation (which shouldn't shadow) + your tests to evaluate test coverage + generate report
  // TODO: report views
  // TODO: == and && relative precedence
  // TODO: exercise export
  // TODO: cursor inspector is occluding last line of code
  // TODO: merge into haz3l-tests

  div(
    ~attr=Attr.classes(["editor", "column"]),
    [prompt_view]
    @ render_cells(
        settings,
        [
          prelude_view,
          correct_impl_view,
          correct_impl_ctx_view,
          your_tests_view,
        ]
        @ hidden_bugs_views  // TODO is it called acceptance testing?
        @ [
          coverage_report_view,
          your_impl_view,
          hidden_tests_view,
          hidden_test_results_view,
        ],
      )
    @ [div(~attr=Attr.class_("bottom-bar"), ci_view)],
  );
};
