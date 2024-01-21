open Haz3lcore;

type state = (Id.t, Editor.t);

let view =
    (
      ~inject,
      ~ui_state: Model.ui_state,
      ~settings: Settings.t,
      ~color_highlighting,
      ~results,
      ~statics as {editor, error_ids, _}: Editor.statics,
    ) => {
  let result = ModelResults.get(results, ScratchSlide.scratch_key);
  // let footer =
  //   settings.core.statics
  //     ? {
  //       let elab =
  //         settings.core.elaborate
  //           ? Some(
  //               Interface.elaborate(~settings=settings.core, info_map, term),
  //             )
  //           : None;
  //       Some(Cell.footer(~settings, ~inject, ~ui_state, result));
  //     }
  //     : None;
  [
    Cell.editor_view(
      ~inject,
      ~ui_state,
      ~clss=["single"],
      ~settings,
      ~code_id="code-container",
      ~error_ids,
      ~test_results=result.tests,
      ~footer=
        settings.core.statics
          ? Some(Cell.footer(~settings, ~inject, ~ui_state, result)) : None,
      ~color_highlighting,
      Editor.get_syntax(editor),
    ),
  ];
};

let export_button = state =>
  Widgets.button_named(
    Icons.star,
    _ => {
      let json_data = ScratchSlide.export(state);
      JsUtil.download_json("hazel-scratchpad", json_data);
      Virtual_dom.Vdom.Effect.Ignore;
    },
    ~tooltip="Export Scratchpad",
  );
let import_button = inject =>
  Widgets.file_select_button_named(
    "import-scratchpad",
    Icons.star,
    file => {
      switch (file) {
      | None => Virtual_dom.Vdom.Effect.Ignore
      | Some(file) => inject(UpdateAction.InitImportScratchpad(file))
      }
    },
    ~tooltip="Import Scratchpad",
  );

let reset_button = inject =>
  Widgets.button_named(
    Icons.trash,
    _ => {
      let confirmed =
        JsUtil.confirm(
          "Are you SURE you want to reset this scratchpad? You will lose any existing code.",
        );
      if (confirmed) {
        inject(UpdateAction.ResetCurrentEditor);
      } else {
        Virtual_dom.Vdom.Effect.Ignore;
      };
    },
    ~tooltip="Reset Scratchpad",
  );
