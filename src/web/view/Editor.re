open Virtual_dom.Vdom;
open Node;
open Core;
open Util.Web;

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

let code_container_2 =
    (
      ~font_metrics,
      ~zipper,
      ~unselected,
      ~settings,
      ~show_backpack_targets,
      ~show_deco,
    ) => {
  let (z2, _) = Perform.drop_it_like_its_hot_regrout(zipper, 8000);
  let segment = Zipper.zip(zipper);
  let map1 = Measured.of_segment(unselected);
  let map_barf =
    Measured.of_segment_id_whitelist(
      ~id_whitelist=Segment.ids(unselected),
      Zipper.unselect_and_zip(z2),
    );
  let map = Measured.mergeq(map_barf, map1);
  let code_view =
    Code.view(~font_metrics, ~segment, ~unselected, ~map=map_barf, ~settings);
  let deco_view =
    show_deco
      ? deco(~zipper, ~map, ~segment, ~font_metrics, ~show_backpack_targets)
      : [];
  div([Attr.class_("code-container")], [code_view] /***/ @ deco_view);
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
    (~inject, ~font_metrics, ~settings: Model.settings, ~index, ~unselected) => {
  let term = MakeTerm.go(unselected);
  let (_, _, info_map) = Statics.mk_map(term);
  [
    CursorInspector.view(~inject, ~settings, index, info_map),
    //CtxInspector.view(index, info_map),
  ]
  @ (
    settings.dynamics
      ? single_editor_dynamics_views(~font_metrics, term, info_map) : []
  );
};

let single_editor =
    (
      ~inject,
      ~font_metrics,
      ~show_backpack_targets,
      ~zipper: Zipper.t,
      ~settings: Model.settings,
    )
    : Node.t => {
  let unselected = Zipper.unselect_and_zip(zipper);
  let code_view =
    code_container_2(
      ~font_metrics,
      ~zipper,
      ~unselected,
      ~settings,
      ~show_backpack_targets,
      ~show_deco=true,
    );
  let (z2, _) = Perform.drop_it_like_its_hot_regrout(zipper, 7000);
  let unselected2 = Zipper.unselect_and_zip(z2);
  let statics_view =
    settings.statics
      ? single_editor_semantics_views(
          ~inject,
          ~settings,
          ~font_metrics,
          ~index=Indicated.index(zipper),
          ~unselected=unselected2,
        )
      : [];
  div([clss(["editor", "single"])], [code_view] @ statics_view);
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
      ~font_metrics,
      ~zipper,
      ~unselected,
      ~settings,
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
    CursorInspector.view(
      ~inject,
      ~settings,
      Indicated.index(focal_zipper),
      combined_info_map,
    ),
  ]
  @ (
    settings.dynamics
      ? [SchoolView.view(~inject, ~font_metrics, editors)] : []
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
      ~focal_zipper,
      ~selected,
      ~show_backpack_targets,
    )
  };
};
