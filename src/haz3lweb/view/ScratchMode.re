open Haz3lcore;

type editor_selection =
  | MainEditor
  | Stepper(int);

let view =
    (
      ~inject,
      ~ui_state: Model.ui_state,
      ~settings: Settings.t,
      ~highlights,
      ~results: ModelResults.t,
      ~result_key,
      ~statics as {error_ids, _}: CachedStatics.statics,
      ~selected,
      editor: Editor.t,
    ) => {
  let result = ModelResults.lookup(results, result_key);
  let test_results = Util.OptUtil.and_then(ModelResult.test_results, result);
  let footer =
    settings.core.elaborate || settings.core.dynamics
      ? result
        |> Option.map(result =>
             Cell.footer(
               ~locked=false,
               ~settings,
               ~inject,
               ~ui_state,
               ~result,
               ~result_key,
             )
           )
      : None;
  [
    Cell.editor_view(
      ~inject,
      ~ui_state,
      ~settings,
      ~error_ids,
      ~test_results,
      ~footer?,
      ~highlights,
      ~selected,
      editor,
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
