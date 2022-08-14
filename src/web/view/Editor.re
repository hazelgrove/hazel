open Virtual_dom.Vdom;
open Node;
open Core;
open Util.Web;

let get_target = (~font_metrics: FontMetrics.t, e) => {
  let rect =
    JSUtil.force_get_elem_by_id("under-the-rail")##getBoundingClientRect;
  let target_x = float_of_int(e##.clientX);
  let target_y = float_of_int(e##.clientY);
  Measured.{
    row: Float.to_int((target_y -. rect##.top) /. font_metrics.row_height),
    col: Float.to_int((target_x -. rect##.left) /. font_metrics.col_width),
  };
};

let mousedown_overlay = (~inject, ~font_metrics) =>
  div(
    Attr.[
      id("mousedown-overlay"),
      on_mouseup(_ => inject(Update.Mouseup)),
      on_mousemove(e => {
        let target = get_target(~font_metrics, e);
        inject(Update.PerformAction(Select(Target(target))));
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
      ~inject,
      ~font_metrics,
      ~zipper,
      ~unselected,
      ~settings,
      ~show_backpack_targets,
      ~mousedown,
      ~show_deco,
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
    [
      Attr.class_("code-container"),
      Attr.on_mousedown(e => {
        let target = get_target(~font_metrics, e);
        Event.Many([
          inject(Update.Mousedown),
          inject(Update.PerformAction(Move(Target(target)))),
        ]);
      }),
    ],
    (mousedown ? [mousedown_overlay(~inject, ~font_metrics)] : [])
    @ [code_view]
    @ deco_view,
  );
};

let single_editor_semantics_views =
    (~inject, ~font_metrics, ~settings: Model.settings, ~index, ~unselected) => {
  let term = MakeTerm.go(unselected);
  let (_, _, info_map) = Statics.mk_map(term);
  [
    div(
      [clss(["bottom-bar"])],
      [
        CursorInspector.view(~inject, ~settings, index, info_map),
        //CtxInspector.view(index, info_map),
      ]
      @ (
        settings.dynamics
          ? [Interface.res_view(~font_metrics, term, info_map)] : []
      ),
    ),
  ]
  @ (
    settings.dynamics
      ? [
        TestView.view(
          ~title="Tests",
          ~font_metrics,
          Elaborator.uexp_elab(info_map, term),
        ),
      ]
      : []
  );
};

let school_panel_view =
    (
      ~inject,
      ~font_metrics,
      (your_tests, our_tests, reference_tests, coverage_tests),
    ) =>
  div(
    [clss(["school-panel"])],
    [
      SchoolView.test_section_view(
        ~title="Your Tests:",
        ~font_metrics,
        your_tests,
      ),
      SchoolView.test_section_view(
        ~title="Our Tests:",
        ~font_metrics,
        our_tests,
      ),
      SchoolView.coverage_view(
        ~inject,
        ~font_metrics,
        reference_tests,
        coverage_tests,
      ),
    ],
  );

let multi_editor_semantics_views =
    (
      ~inject,
      ~settings: Model.settings,
      ~font_metrics,
      ~focal_zipper,
      ~editors,
    ) => {
  let (_, combined_info_map) = SchoolView.spliced_statics(editors);
  [
    div(
      [clss(["bottom-bar"])],
      [
        CursorInspector.view(
          ~inject,
          ~settings,
          Indicated.index(focal_zipper),
          combined_info_map,
        ),
      ]
      @ (
        settings.dynamics
          ? switch (SchoolView.data(editors)) {
            | Some((
                statics_impl,
                your_tests,
                our_tests,
                reference_tests,
                coverage_tests,
              )) =>
              let school_panel_data = (
                your_tests,
                our_tests,
                reference_tests,
                coverage_tests,
              );
              let (implement_term, implement_map) =
                SchoolView.spliced_statics(statics_impl);
              [
                school_panel_view(~inject, ~font_metrics, school_panel_data),
                Interface.res_view(
                  ~font_metrics,
                  implement_term,
                  implement_map,
                ),
              ];
            | _ => [text("Error: SchoolView: Wrong number of editors")]
            }
          : []
      ),
    ),
  ];
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
  let code_view =
    code_container(
      ~inject,
      ~font_metrics,
      ~zipper,
      ~unselected,
      ~settings,
      ~mousedown,
      ~show_backpack_targets,
      ~show_deco=true,
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
  div([clss(["editor", "single"])], [code_view] @ semantics_views);
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
      idx,
      editor: Model.editor,
      ~settings: Model.settings,
      ~inject: Update.t => 'a,
      ~font_metrics,
      ~selected,
      ~mousedown,
      ~show_backpack_targets,
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
    | None => []
    | Some(chapter) => [div([clss(["cell-chapter"])], [chapter])]
    };
  let code_view =
    code_container(
      ~inject,
      ~font_metrics,
      ~zipper,
      ~unselected,
      ~settings,
      ~mousedown,
      ~show_backpack_targets,
      ~show_deco=idx == selected,
    );
  div(
    [clss(["cell-container"])],
    cell_chapter_view
    @ [
      div(
        [
          Attr.classes(["cell"] @ (selected == idx ? ["selected"] : [])),
          Attr.on_click(_ => inject(SwitchEditor(idx))),
        ],
        [cell_caption_view, code_view],
      ),
    ],
  );
};

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
  let semantics_view =
    settings.statics
      ? multi_editor_semantics_views(
          ~inject,
          ~settings,
          ~font_metrics,
          ~focal_zipper,
          ~editors,
        )
      : [];
  /* Hide hidden editors in student mode */
  let editors =
    settings.student
      ? List.filteri((i, _) => !List.nth(School.hiddens, i), editors)
      : editors;
  div(
    [Attr.classes(["editor", "column"])],
    List.mapi(cell_view, editors) @ semantics_view,
  );
};

let view =
    (
      ~font_metrics,
      ~show_backpack_targets,
      ~settings: Model.settings,
      ~editor_model: Model.editor_model,
      ~mousedown: bool,
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
