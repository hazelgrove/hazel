open Virtual_dom.Vdom;
open Node;
open Core;
open Util.Web;

/* zipper plus derived data. this is a convenience to help avoid recomputation */
type fat_zipper = {
  zipper: Zipper.t,
  tile_map: Measured.t,
  segment: Segment.t,
  unselected: Segment.t,
  term: Term.UExp.t,
  info_map: Statics.info_map,
};

let fat_zipper = (zipper: Zipper.t) => {
  let term = zipper |> Term.of_zipper;
  let (_, _, info_map) = term |> Statics.uexp_to_info_map;
  let segment = Zipper.zip(zipper);
  let unselected = Zipper.unselect_and_zip(zipper);
  let tile_map = Measured.of_segment(unselected);
  {zipper, term, info_map, segment, unselected, tile_map};
};

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

let deco = (~fat_zipper, ~font_metrics, show_backpack_targets) => {
  module Deco =
    Deco.Deco({
      let font_metrics = font_metrics;
      let map = fat_zipper.tile_map;
      let show_backpack_targets = show_backpack_targets;
    });
  Deco.all(fat_zipper.zipper, fat_zipper.segment);
};

let code_container =
    (
      ~font_metrics,
      ~fat_zipper: fat_zipper,
      ~settings,
      ~show_backpack_targets,
      ~show_deco,
    ) => {
  div(
    [Attr.class_("code-container")],
    [
      Code.view(
        ~font_metrics,
        ~sel_seg=fat_zipper.segment,
        ~unsel_seg=fat_zipper.unselected,
        ~map=fat_zipper.tile_map,
        ~settings,
      ),
    ]
    @ (
      show_deco ? deco(~fat_zipper, ~font_metrics, show_backpack_targets) : []
    ),
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
  let fat_zipper = fat_zipper(zipper);
  div(
    [clss(["editor", "single"])],
    [
      code_container(
        ~font_metrics,
        ~fat_zipper,
        ~settings,
        ~show_backpack_targets,
        ~show_deco=true,
      ),
      ci_view(Indicated.index(fat_zipper.zipper), fat_zipper.info_map),
      TestView.view(
        ~title="Tests",
        ~font_metrics,
        Elaborator.uexp_elab(fat_zipper.info_map, fat_zipper.term),
      ),
      Interface.res_view(~font_metrics, fat_zipper.term, fat_zipper.info_map),
    ],
  );
};

let cell_captions = [
  "Student Implementation",
  "Student Tests",
  "Teacher Tests",
];

let cell_caption_view = (_settings: Model.settings, n) =>
  div([clss(["cell-caption"])], [text(List.nth(cell_captions, n))]);

let cell_view =
    (
      idx,
      editor: Model.editor,
      ~settings: Model.settings,
      ~inject: Update.t => 'a,
      ~font_metrics,
      ~selected,
      ~show_backpack_targets,
    ) =>
  div(
    [
      Attr.classes(["cell"] @ (selected == idx ? ["selected"] : [])),
      Attr.on_click(_ => inject(SwitchEditor(idx))),
    ],
    [
      cell_caption_view(settings, idx),
      code_container(
        ~font_metrics,
        ~fat_zipper=fat_zipper(editor.zipper),
        ~settings,
        ~show_backpack_targets,
        ~show_deco=idx == selected,
      ),
    ],
  );

let join_tile = (id): Tile.t => {
  id,
  label: [";"],
  mold: Mold.mk_bin(10, Exp, []),
  shards: [0],
  children: [],
};

let splice_editors = (editors: list(Model.editor)): Segment.t =>
  editors
  |> List.map((ed: Model.editor) => Zipper.unselect_and_zip(ed.zipper))
  |> (
    xs =>
      Util.ListUtil.interleave(
        xs,
        List.init(List.length(editors) - 1, i =>
          [Piece.Tile(join_tile(i + 100000))]
        ),
      )
  )
  |> List.flatten;

let spliced_statics = (eds: list(Model.editor)) => {
  let term = eds |> splice_editors |> Term.uexp_of_seg;
  let (_, _, info_map) = term |> Statics.uexp_to_info_map;
  (term, info_map);
};

let test_multi_panel = (~font_metrics, editors) => {
  switch (editors) {
  | [student_impl, student_tests, teacher_tests] =>
    let (implement_term, implement_map) = spliced_statics([student_impl]);
    let (teacher_term, teacher_map) =
      spliced_statics([student_impl, teacher_tests]);
    let (student_term, student_map) =
      spliced_statics([student_impl, student_tests]);
    div(
      [clss(["test-multi-panel"])],
      [
        TestView.view(
          ~title="Student Tests",
          ~font_metrics,
          Elaborator.uexp_elab(student_map, student_term),
        ),
        TestView.view(
          ~title="Teacher Tests",
          ~font_metrics,
          Elaborator.uexp_elab(teacher_map, teacher_term),
        ),
        Interface.res_view(~font_metrics, implement_term, implement_map),
      ],
    );
  | _ => div([], [])
  };
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
  let (_, combined_info_map) = spliced_statics(editors);
  let focal_zipper = fat_zipper(focal_zipper);
  div(
    [Attr.classes(["editor", "column"])],
    List.mapi(
      cell_view(
        ~settings,
        ~inject,
        ~font_metrics,
        ~selected,
        ~show_backpack_targets,
      ),
      editors,
    )
    @ [
      ci_view(Indicated.index(focal_zipper.zipper), combined_info_map),
      test_multi_panel(~font_metrics, editors),
    ],
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
