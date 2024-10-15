open Util;
open Haz3lcore;

type state = (Id.t, Editor.t);

let view =
    (
      ~inject,
      ~ui_state: Model.ui_state,
      ~settings: Settings.t,
      ~highlights,
      ~results: ModelResults.t,
      ~result_key,
      editor: Editor.t,
    ) => {
  let result = ModelResults.lookup(results, result_key);
  let test_results = Util.OptUtil.and_then(ModelResult.test_results, result);
  let target_id = "code-container";
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
      ~target_id,
      ~test_results,
      ~footer?,
      ~highlights,
      ~sort=Exp,
      editor,
    ),
  ];
};

let export_button = (inject: Update.t => Ui_effect.t(unit)) =>
  Widgets.button_named(
    Icons.export,
    _ => inject(Export(ExportScratchSlide)),
    ~tooltip="Export Scratchpad",
  );
let import_button = inject =>
  Widgets.file_select_button_named(
    "import-scratchpad",
    Icons.import,
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
    ~tooltip="Reset Editor",
  );
