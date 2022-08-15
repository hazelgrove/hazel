open Virtual_dom.Vdom;
open Node;
open Core;
open Util.Web;
open OptUtil.Syntax;

let test_view =
    (
      ~title: string,
      ~inject,
      ~font_metrics,
      ~test_results: Interface.test_results,
    )
    : t =>
  div(
    [clss(["panel", "test-panel"])],
    [
      TestView.view_of_main_title_bar(title),
      TestView.test_reports_view(~inject, ~font_metrics, ~test_results),
      TestView.test_summary(~inject, ~test_results),
    ],
  );

let res_view = (~font_metrics: FontMetrics.t, eval_result): Node.t =>
  div(
    [Attr.classes(["result"])],
    [Interface.dhcode_view(~font_metrics, ~width=80, eval_result)],
  );

let single_editor_semantics_views =
    (~inject, ~font_metrics, ~settings: Model.settings, ~index, ~unselected) => {
  let term = MakeTerm.go(unselected);
  let (_, _, map) = Statics.mk_map(term);
  let test_results =
    settings.dynamics ? Interface.test_results(map, term) : None;
  let eval_result =
    settings.dynamics ? Interface.evaulation_result(map, term) : None;
  [
    div(
      [clss(["bottom-bar"])],
      [
        CursorInspector.view(~inject, ~settings, index, map),
        //CtxInspector.view(index, map),
      ]
      @ (
        switch (eval_result) {
        | _ when !settings.dynamics => []
        | None => []
        | Some(eval_result) => [res_view(~font_metrics, eval_result)]
        }
      ),
    ),
  ]
  @ (
    switch (test_results) {
    | _ when !settings.dynamics => []
    | None => []
    | Some(test_results) => [
        test_view(~title="Tests", ~inject, ~font_metrics, ~test_results),
      ]
    }
  );
};

let get_goal = (~font_metrics: FontMetrics.t, ~target_id, e) => {
  let rect = JSUtil.force_get_elem_by_id(target_id)##getBoundingClientRect;
  let goal_x = float_of_int(e##.clientX);
  let goal_y = float_of_int(e##.clientY);
  Measured.{
    row: Float.to_int((goal_y -. rect##.top) /. font_metrics.row_height),
    col:
      Float.(
        to_int(round((goal_x -. rect##.left) /. font_metrics.col_width))
      ),
  };
};

let mousedown_handler =
    (~inject, ~font_metrics, ~target_id, ~additional_updates=[], e) => {
  let goal = get_goal(~font_metrics, ~target_id, e);
  Event.Many(
    List.map(inject, additional_updates)
    @ [
      inject(Update.Mousedown),
      inject(Update.PerformAction(Move(Goal(goal)))),
    ],
  );
};

let mousedown_overlay = (~inject, ~font_metrics, ~target_id) =>
  div(
    Attr.[
      id("mousedown-overlay"),
      on_mouseup(_ => inject(Update.Mouseup)),
      on_mousemove(e => {
        let goal = get_goal(~font_metrics, ~target_id, e);
        inject(Update.PerformAction(Select(Goal(goal))));
      }),
    ],
    [],
  );

let deco = (~zipper, ~map, ~segment, ~font_metrics, ~show_backpack_targets) => {
  module Deco =
    Deco.Deco({
      let font_metrics = font_metrics;
      let map = map;
      let show_backpack_targets = show_backpack_targets;
    });
  Deco.all(zipper, segment);
};

let code_container =
    (
      ~font_metrics,
      ~unselected,
      ~settings,
      ~show_backpack_targets,
      ~show_deco,
      ~overlays=[],
      ~id,
      zipper,
    ) => {
  let segment = Zipper.zip(zipper);
  let map = Measured.of_segment(unselected);
  let code_view =
    Code.view(~font_metrics, ~segment, ~unselected, ~map, ~settings);
  let deco_view =
    show_deco
      ? deco(~zipper, ~map, ~segment, ~font_metrics, ~show_backpack_targets)
      : [];
  div(
    [Attr.id(id), Attr.class_("code-container")],
    [code_view] @ deco_view @ overlays,
  );
};

let single_editor =
    (
      ~inject,
      ~font_metrics,
      ~show_backpack_targets,
      ~mousedown,
      ~zipper: Zipper.t,
      ~settings: Model.settings,
    )
    : Node.t => {
  let unselected = Zipper.unselect_and_zip(zipper);
  let code_id = "code-container";
  let code_view =
    code_container(
      ~id=code_id,
      ~font_metrics,
      ~unselected,
      ~settings,
      ~show_backpack_targets,
      ~show_deco=true,
      zipper,
    );
  let semantics_views =
    settings.statics
      ? single_editor_semantics_views(
          ~inject,
          ~settings,
          ~font_metrics,
          ~index=Indicated.index(zipper),
          ~unselected,
        )
      : [];
  let mousedown_overlay =
    mousedown
      ? [mousedown_overlay(~inject, ~font_metrics, ~target_id=code_id)] : [];
  div(
    [
      clss(["editor", "single"]),
      Attr.on_mousedown(e =>
        mousedown_handler(~inject, ~font_metrics, ~target_id=code_id, e)
      ),
    ],
    [code_view] @ semantics_views @ mousedown_overlay,
  );
};

let show_term = (editor: Model.editor, _) =>
  editor.zipper
  |> Zipper.zip
  |> MakeTerm.go
  |> Term.UExp.show
  |> print_endline
  |> (_ => Event.Ignore);

let cell_view =
    (
      ~result_bar: list(Node.t)=[],
      ~settings: Model.settings,
      ~inject: Update.t => 'a,
      ~font_metrics,
      ~selected,
      ~mousedown,
      ~show_backpack_targets,
      ~show_code=true,
      ~overlays=[],
      idx,
      editor: Model.editor,
    ) => {
  let zipper = editor.zipper;
  let unselected = Zipper.unselect_and_zip(zipper);
  let cell_caption_view =
    div(
      [clss(["cell-caption"]), Attr.on_click(show_term(editor))],
      [text(List.nth(School.captions, idx))],
    );
  let cell_chapter_view =
    switch (List.nth(School.chapters, idx)) {
    | None => div([Attr.create("style", "display: none;")], [])
    | Some(chapter) => div([clss(["cell-chapter"])], [chapter])
    };
  let code_container_id = "code-container-" ++ string_of_int(idx);
  let code_view =
    code_container(
      ~id=code_container_id,
      ~font_metrics,
      ~unselected,
      ~settings,
      ~overlays,
      ~show_backpack_targets,
      ~show_deco=selected == idx,
      zipper,
    );
  let mousedown_overlay =
    selected == idx && mousedown
      ? [
        mousedown_overlay(
          ~inject,
          ~font_metrics,
          ~target_id=code_container_id,
        ),
      ]
      : [];
  div(
    [clss(["cell-container"])],
    [cell_chapter_view]
    @ [
      div(
        [
          Attr.classes(["cell"] @ (selected == idx ? ["selected"] : [])),
          Attr.on_mousedown(
            mousedown_handler(
              ~inject,
              ~font_metrics,
              ~target_id=code_container_id,
              ~additional_updates=[Update.SwitchEditor(idx)],
            ),
          ),
        ],
        [cell_caption_view]
        @ (show_code ? mousedown_overlay @ [code_view] : []),
      ),
    ]
    @ result_bar,
  );
};

let get_school_data = (editors: list(Model.editor)) => {
  switch (editors) {
  | [
      student_impl,
      student_tests,
      hidden_tests,
      reference_impl,
      wrong_impl_1,
      wrong_impl_2,
      wrong_impl_3,
    ] =>
    /* Note: splicing in student implementation
       first in case they create helpers. Still
       has problem if these get shadowed; make
       sure we use uncommon names for helpers. */
    Some((
      [student_impl],
      [student_impl, student_tests],
      [student_impl, hidden_tests],
      [student_impl, reference_impl, student_tests],
      [
        [student_impl, wrong_impl_1, student_tests],
        [student_impl, wrong_impl_2, student_tests],
        [student_impl, wrong_impl_3, student_tests],
      ],
    ))
  | _ => None
  };
};

let test_status_icon_view =
    (~font_metrics, insts, ms: Measured.Shards.t): option(Node.t) =>
  switch (ms) {
  | [(_, {origin: _, last}), ..._] =>
    let status = insts |> TestMap.joint_status |> TestStatus.to_string;
    let pos = DecUtil.abs_position(~font_metrics, last);
    Some(div([clss(["test-result", status]), pos], []));
  | _ => None
  };

let test_result_layer =
    (~font_metrics, ~map: Measured.t, test_results: Interface.test_results)
    : list(Node.t) =>
  List.filter_map(
    ((id, insts)) =>
      switch (Id.Map.find_opt(id, map.tiles)) {
      | Some(ms) => test_status_icon_view(~font_metrics, insts, ms)
      | _ => None
      },
    test_results.test_map,
  );

let multi_editor =
    (
      ~font_metrics,
      ~show_backpack_targets,
      ~mousedown,
      ~editors: list(Model.editor),
      ~selected,
      ~settings,
      ~focal_zipper: Zipper.t,
      ~inject,
    ) => {
  let cell_view =
    cell_view(
      ~settings,
      ~inject,
      ~font_metrics,
      ~mousedown,
      ~selected,
      ~show_backpack_targets,
    );
  let combined_info_map =
    settings.statics
      ? {
        let (_, combined_info_map) = SchoolView.spliced_statics(editors);
        combined_info_map;
      }
      : Id.Map.empty;
  let school_view_data = settings.dynamics ? get_school_data(editors) : None;
  let your_test_results = {
    let* (_, your_tests, _, _, _) = school_view_data;
    let (term, map) = SchoolView.spliced_statics(your_tests);
    Interface.test_results(map, term);
  };
  let our_test_results = {
    let* (_, _, our_tests, _, _) = school_view_data;
    let (term, map) = SchoolView.spliced_statics(our_tests);
    Interface.test_results(map, term);
  };
  let first_cell_res = {
    let* (statics_impl, _, _, _, _) = school_view_data;
    let (term, map) = SchoolView.spliced_statics(statics_impl);
    Interface.evaulation_result(map, term);
  };
  let first_cell_res_view =
    switch (first_cell_res) {
    | None => div([], [])
    | Some(dhexp) =>
      div([clss(["cell-result"])], [res_view(~font_metrics, dhexp)])
    };
  let coverage_view =
    switch (school_view_data) {
    | Some((_, _, _, reference_tests, coverage_tests)) => [
        SchoolView.coverage_view(
          ~inject,
          ~font_metrics,
          reference_tests,
          coverage_tests,
        ),
      ]
    | None => []
    };
  let your_tests_view =
    switch (your_test_results) {
    | None => []
    | Some(test_results) => [TestView.test_summary(~inject, ~test_results)]
    };
  let your_tests_layer =
    switch (your_test_results, editors) {
    | (Some(test_results), [_, your_tests, ..._]) =>
      let map = Measured.of_segment(SchoolView.splice_editors([your_tests]));
      test_result_layer(~font_metrics, ~map: Measured.t, test_results);
    | _ => []
    };
  let our_tests_view =
    switch (our_test_results) {
    | None => div([], [])
    | Some(test_results) =>
      div(
        [clss(["cell", "cell-result"])],
        [
          TestView.test_summary(~inject, ~test_results),
          TestView.test_reports_view(~inject, ~font_metrics, ~test_results),
        ],
      )
    };
  let school_panel = [div([clss(["school-panel"])], coverage_view)];
  let ci_view =
    settings.statics
      ? {
        [
          CursorInspector.view(
            ~inject,
            ~settings,
            Indicated.index(focal_zipper),
            combined_info_map,
          ),
        ];
      }
      : [];
  div(
    [Attr.classes(["editor", "column"])],
    List.mapi(
      (i, ed) =>
        switch (i) {
        | 0 => cell_view(~result_bar=[first_cell_res_view], i, ed)
        | 1 =>
          cell_view(
            ~result_bar=[
              div([clss(["cell", "cell-result"])], your_tests_view),
            ],
            ~overlays=your_tests_layer,
            i,
            ed,
          )
        | 2 =>
          cell_view(
            ~show_code=!settings.student,
            ~result_bar=[our_tests_view],
            i,
            ed,
          )
        | _ =>
          settings.student
            ? div([Attr.create("style", "display: none;")], [])
            : cell_view(i, ed)
        },
      editors,
    )
    @ (settings.dynamics ? school_panel : [])
    @ [div([clss(["bottom-bar"])], ci_view)],
  );
};

let view =
    (
      ~font_metrics,
      ~show_backpack_targets,
      ~settings: Model.settings,
      ~editor_model: Model.editor_model,
      ~mousedown,
      ~inject,
    )
    : Node.t => {
  let focal_zipper = Model.get_zipper'(editor_model);
  switch (editor_model) {
  | Simple(_)
  | Study(_) =>
    single_editor(
      ~inject,
      ~font_metrics,
      ~mousedown,
      ~show_backpack_targets,
      ~zipper=focal_zipper,
      ~settings,
    )
  | School(selected, editors) =>
    multi_editor(
      ~inject,
      ~font_metrics,
      ~settings,
      ~editors,
      ~mousedown,
      ~focal_zipper,
      ~selected,
      ~show_backpack_targets,
    )
  };
};
