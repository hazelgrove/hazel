open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
// open Util.Web;

// let splice_editors = (editors: list(Editor.t)): Segment.t =>
//   editors
//   |> List.map((ed: Editor.t) => Zipper.unselect_and_zip(ed.state.zipper))
//   |> (
//     xs =>
//       Util.ListUtil.interleave(
//         xs,
//         List.init(List.length(editors) - 1, i =>
//           [Piece.Tile(join_tile(i + 1000000))]
//         ) //TODO(andrew): id_gen hack
//       )
//   )
//   |> List.flatten;

// let spliced_statics = (editors: list(Editor.t)) => {
//   let term = editors |> splice_editors |> MakeTerm.go;
//   let (_, _, info_map) = term |> Statics.mk_map;
//   (term, info_map);
// };

// let coverage_summary_str = (~total, ~found): string => {
//   TestView.result_summary_str(
//     ~n=total,
//     ~p=found,
//     ~q=0,
//     ~n_str="bug",
//     ~ns_str="bugs",
//     ~p_str="exposed",
//     ~q_str="",
//     ~r_str="unrevealed",
//   );
// };

// let coverage_text = (~total, ~found): Node.t =>
//   div(
//     ~attr=Attr.classes(["test-text"]),
//     [
//       TestView.percent_view(total, found),
//       div([text(":")]),
//       text(coverage_summary_str(~total, ~found)),
//     ],
//   );

// let coverage_bar = (~inject as _, instances) =>
//   div(
//     ~attr=Attr.classes(["test-bar"]),
//     List.map(
//       ((status, _)) =>
//         div(
//           ~attr=Attr.classes(["segment", TestStatus.to_string(status)]),
//           [],
//         ),
//       instances,
//     ),
//   );

// let coverage_summary = (~inject, instances) => {
//   let total = List.length(instances);
//   let found =
//     List.length(
//       List.filter(((x: TestStatus.t, _)) => x == Pass, instances),
//     );
//   let status_class = total == found ? "Pass" : "Fail";
//   div(
//     ~attr=Attr.classes(["test-summary", status_class]),
//     [coverage_text(~total, ~found), coverage_bar(~inject, instances)],
//   );
// };

// let coverage_report_view =
//     (
//       ~inject,
//       ~font_metrics,
//       ~description: option(string)=None,
//       i,
//       (status, instance),
//     ) =>
//   div(
//     ~attr=
//       Attr.many([
//         Attr.classes(["test-report"]),
//         Attr.on_click(TestView.jump_to_test(~inject)),
//       ]),
//     [
//       div(
//         ~attr=
//           Attr.classes(["test-id", "Test" ++ TestStatus.to_string(status)]),
//         /* NOTE: prints lexical index, not unique id */
//         [text(string_of_int(i + 1))],
//       ),
//       TestView.test_instance_view(~font_metrics, instance),
//     ]
//     @ (
//       switch (description) {
//       | None => []
//       | Some(d) => [
//           div(~attr=Attr.classes(["test-description"]), [text(d)]),
//         ]
//       }
//     ),
//   );

// let passing_test_ids = test_map =>
//   test_map
//   |> List.filter(((_id, reports)) =>
//        List.for_all(((_, status)) => status == TestStatus.Pass, reports)
//      )
//   |> List.split
//   |> fst;

// let failing_test_ids = test_map =>
//   test_map
//   |> List.filter(((_id, reports)) =>
//        List.for_all(((_, status)) => status == TestStatus.Fail, reports)
//      )
//   |> List.split
//   |> fst;

// let get_test_map = (editors: list(Editor.t)) => {
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

// let coverage_view =
//     (~font_metrics, ~inject, ~descriptions=[], reference, wrongs) => {
//   let reference_passing = reference |> get_test_map |> passing_test_ids;
//   let instances = wrongs |> List.map(get_first_common(reference_passing));
//   let non_null_instances =
//     instances
//     |> List.filter_map(((x, instance: option('a))) =>
//          switch (instance) {
//          | None => None
//          | Some(inst) => Some((x, inst))
//          }
//        );
//   div(
//     ~attr=Attr.classes(["panel", "test-panel"]),
//     [
//       TestView.view_of_main_title_bar("Test Coverage"),
//       div(
//         ~attr=Attr.classes(["panel-body", "test-reports"]),
//         non_null_instances
//         |> List.mapi((i, r) =>
//              coverage_report_view(
//                ~inject,
//                ~font_metrics,
//                ~description=List.nth_opt(descriptions, i),
//                i,
//                r,
//              )
//            ),
//       ),
//       coverage_summary(~inject, instances),
//     ],
//   );
// };

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
//       reference_impl,
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
//       [student_impl, reference_impl, student_tests],
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
//     (~font_metrics, insts, ms: Measured.Shards.t): option(Node.t) =>
//   switch (ms) {
//   | [(_, {origin: _, last}), ..._] =>
//     let status = insts |> TestMap.joint_status |> TestStatus.to_string;
//     let pos = DecUtil.abs_position(~font_metrics, last);
//     Some(div([clss(["test-result", status]), pos], []));
//   | _ => None
//   };

// let test_result_layer =
//     (~font_metrics, ~map: Measured.t, test_results: Interface.test_results)
//     : list(Node.t) =>
//   List.filter_map(
//     ((id, insts)) =>
//       switch (Id.Map.find_opt(id, map.tiles)) {
//       | Some(ms) => test_status_icon_view(~font_metrics, insts, ms)
//       | _ => None
//       },
//     test_results.test_map,
//   );

let view =
    (
      ~font_metrics,
      ~show_backpack_targets,
      ~mousedown,
      ~state: SchoolExercise.state,
      ~settings,
      ~inject,
    ) => {
  let (pos, ed) = state;
  let editor_view = pos =>
    Cell.editor_view(
      ~inject,
      ~font_metrics,
      ~show_backpack_targets,
      ~mousedown,
      ~mousedown_updates=[
        Update.SwitchEditor(SchoolExercise.idx_of_pos(pos, ed)),
      ],
      ~settings,
    );

  let _ = (show_backpack_targets, settings, font_metrics, mousedown, inject);
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
  //     let map = Measured.of_segment(splice_editors([your_tests]));
  //     test_result_layer(~font_metrics, ~map: Measured.t, test_results);
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
  //           Indicated.index(focal_zipper),
  //           combined_info_map,
  //         ),
  //       ]
  //       @ (
  //         switch (Indicated.index(focal_zipper), your_test_results) {
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
  // let reference_impl_view = code =>
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
  //         | ReferenceImpl(_) => [reference_impl_view(Option.get(ed))]
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

  let prompt_view = div(~attr=Attr.classes(["cell-chapter"]), [ed.prompt]); // TODO rename "cell-chapter" to "prompt"

  let SchoolExercise.{user, instructor, hidden_bugs} =
    SchoolExercise.stitch(state);

  let (focal_zipper, focal_info_map) =
    switch (pos) {
    | Prelude => (ed.prelude.state.zipper, user.info_map)
    | ReferenceImpl => (ed.reference_impl.state.zipper, instructor.info_map)
    | YourTests => (ed.your_tests.state.zipper, user.info_map)
    | YourImpl => (ed.your_impl.state.zipper, user.info_map)
    | HiddenBugs(idx) =>
      let editor = List.nth(ed.hidden_bugs, idx).impl;
      let info_map = List.nth(hidden_bugs, idx).info_map;
      (editor.state.zipper, info_map);
    | HiddenTests => (ed.hidden_tests.tests.state.zipper, instructor.info_map)
    };
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
  // >>>>>>> eval-round-2
  //     };

  // TODO: hide in instructor mode
  // TODO: round out bottom when there is no result view
  // TODO: place cursor in correct place when clicking

  // TODO: make prelude read-only
  let prelude_view =
    editor_view(
      Prelude,
      ~selected=pos == Prelude,
      ~caption=Cell.simple_caption("Prelude"),
      ~code_id="prelude",
      ~info_map=user.info_map,
      ed.prelude,
      None,
    );

  let your_impl_view =
    editor_view(
      YourImpl,
      ~selected=pos == YourImpl,
      ~caption=Cell.simple_caption("Your Implementation"),
      ~code_id="your-impl",
      ~info_map=user.info_map,
      ed.your_impl,
      None,
    );

  let your_tests_view =
    editor_view(
      YourTests,
      ~selected=pos == YourTests,
      ~caption=Cell.simple_caption("Your Tests"),
      ~code_id="your-tests",
      ~info_map=user.info_map,
      ed.your_tests,
      None,
    );

  let reference_impl_view =
    editor_view(
      ReferenceImpl,
      ~selected=pos == ReferenceImpl,
      ~caption=Cell.simple_caption("Reference Implementation"),
      ~code_id="reference-impl",
      ~info_map=instructor.info_map,
      ed.reference_impl,
      None,
    );

  let hidden_tests_view =
    editor_view(
      HiddenTests,
      ~selected=pos == HiddenTests,
      ~caption=Cell.simple_caption("Hidden Tests"),
      ~code_id="hidden-tests",
      ~info_map=instructor.info_map,
      ed.hidden_tests.tests,
      None,
    );

  let hidden_bugs_views =
    List.mapi(
      (i, (SchoolExercise.{impl, _}, SchoolExercise.{info_map, _})) => {
        editor_view(
          HiddenBugs(i),
          ~selected=pos == HiddenBugs(i),
          ~caption=
            Cell.simple_caption(
              "Wrong Implementation " ++ string_of_int(i + 1),
            ),
          ~code_id="wrong-implementation-" ++ string_of_int(i + 1),
          ~info_map,
          impl,
          None,
        )
      },
      List.combine(ed.hidden_bugs, hidden_bugs),
    );

  let ci_view =
    settings.statics
      ? {
        [
          CursorInspector.view(
            ~inject,
            ~settings,
            Indicated.index(focal_zipper),
            focal_info_map,
          ),
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
        ];
      }
      : [];

  // TODO: cursor inspector
  // TODO: merge dev
  // TODO: merge dynamics
  // TODO: show reference implementation cells (with decorations coming from prelude only)
  // TODO: show hidden bugs cells (with decorations coming from prelude + reference impl only)
  // TODO: show hidden tests cells (with decorations coming from prelude + reference impl)
  // TODO: run prelude + your implementation to display result
  // TODO: run prelude + reference implementation + your tests to evaluate tests + generate report
  // TODO: run prelude + your implementation (for helpers) + each wrong implementation (which shouldn't shadow) + your tests to evaluate test coverage + generate report
  // TODO: run prelude + your implementation + your tests to evaluate your implementation
  // TODO: report views
  // TODO: == and && relative precedence
  // TODO: exercise export
  div(
    ~attr=Attr.classes(["editor", "column"]),
    [
      div(
        [
          prompt_view,
          prelude_view,
          your_impl_view,
          your_tests_view,
          reference_impl_view,
          hidden_tests_view,
        ]
        @ hidden_bugs_views  // TODO fix spacing
        @ [div(~attr=Attr.class_("bottom-bar"), ci_view)],
      ),
    ],
  );
};
