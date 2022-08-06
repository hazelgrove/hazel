open Virtual_dom.Vdom;
open Node;
open Core;
open Util.Web;

let ci_view = (index': option(int), info_map) => {
  let (index, ci) =
    switch (index') {
    | Some(index) => (index, Id.Map.find_opt(index, info_map))
    | None => ((-1), None)
    };
  switch (ci) {
  | None => div([clss(["cursor-inspector"])], [text("No Static Data")])
  | Some(ci) => CursorInspector.view(index, ci)
  };
};

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
      ~zipper,
      ~unselected,
      ~settings,
      ~show_backpack_targets,
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
  div([Attr.class_("code-container")], [code_view] @ deco_view);
};

let single_editor_dynamics_views = (~font_metrics, term, info_map) => {
  [
    TestView.view(
      ~title="Tests",
      ~font_metrics,
      Elaborator.uexp_elab(info_map, term),
    ),
    Interface.res_view(~font_metrics, term, info_map),
  ];
};

let single_editor_semantics_views =
    (~settings: Model.settings, ~font_metrics, ~index, ~unselected) => {
  let term = Term.uexp_of_seg(unselected);
  let (_, _, info_map) = Statics.uexp_to_info_map(term);
  [ci_view(index, info_map)]
  @ (
    settings.dynamics
      ? single_editor_dynamics_views(~font_metrics, term, info_map) : []
  );
};

let single_editor =
    (
      ~font_metrics,
      ~show_backpack_targets,
      ~zipper: Zipper.t,
      ~settings: Model.settings,
    )
    : Node.t => {
  let unselected = Zipper.unselect_and_zip(zipper);
  let code_view =
    code_container(
      ~font_metrics,
      ~zipper,
      ~unselected,
      ~settings,
      ~show_backpack_targets,
      ~show_deco=true,
    );
  let statics_view =
    settings.statics
      ? single_editor_semantics_views(
          ~settings,
          ~font_metrics,
          ~index=Indicated.index(zipper),
          ~unselected,
        )
      : [];
  div([clss(["editor", "single"])], [code_view] @ statics_view);
};

let cell_captions = [
  "Student Implementation",
  "Student Tests",
  "Teacher Tests",
];

let cell_view =
    (
      idx,
      editor: Model.editor,
      ~settings: Model.settings,
      ~inject: Update.t => 'a,
      ~font_metrics,
      ~selected,
      ~show_backpack_targets,
    ) => {
  let zipper = editor.zipper;
  let unselected = Zipper.unselect_and_zip(zipper);
  let cell_caption_view =
    div([clss(["cell-caption"])], [text(List.nth(cell_captions, idx))]);
  let code_view =
    code_container(
      ~font_metrics,
      ~zipper,
      ~unselected,
      ~settings,
      ~show_backpack_targets,
      ~show_deco=idx == selected,
    );
  div(
    [
      Attr.classes(["cell"] @ (selected == idx ? ["selected"] : [])),
      Attr.on_click(_ => inject(SwitchEditor(idx))),
    ],
    [cell_caption_view, code_view],
  );
};

let multi_editor_semantics_views =
    (~settings: Model.settings, ~font_metrics, ~focal_zipper, ~editors) => {
  let (_, combined_info_map) = TestView.spliced_statics(editors);
  [ci_view(Indicated.index(focal_zipper), combined_info_map)]
  @ (
    settings.dynamics ? [TestView.school_panel(~font_metrics, editors)] : []
  );
};

let multi_editor =
    (
      ~font_metrics,
      ~show_backpack_targets,
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
      ~selected,
      ~show_backpack_targets,
    );
  let semantics_view =
    settings.statics
      ? multi_editor_semantics_views(
          ~settings,
          ~font_metrics,
          ~focal_zipper,
          ~editors,
        )
      : [];
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
      ~inject,
    )
    : Node.t => {
  let focal_zipper = Model.get_zipper'(editor_model);
  switch (editor_model) {
  | Simple(_)
  | Study(_) =>
    single_editor(
      ~font_metrics,
      ~show_backpack_targets,
      ~zipper=focal_zipper,
      ~settings,
    )
  | School(selected, editors) =>
    multi_editor(
      ~font_metrics,
      ~show_backpack_targets,
      ~editors,
      ~selected,
      ~settings,
      ~focal_zipper,
      ~inject,
    )
  };
};
