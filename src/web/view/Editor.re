open Virtual_dom.Vdom;

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
    let measured = Model.get_editor'(editor_model).state.meta.measured;
    SimpleEditor.view(
      ~inject,
      ~font_metrics,
      ~mousedown,
      ~show_backpack_targets,
      ~zipper=focal_zipper,
      ~settings,
      ~measured,
    );
  | School(selected, editors) =>
    SchoolEditor.view(
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
