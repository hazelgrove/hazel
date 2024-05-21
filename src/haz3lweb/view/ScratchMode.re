open Haz3lcore;

let view =
    (
      ~select,
      ~inject: UpdateAction.t => Ui_effect.t(unit),
      ~ui_state: Model.ui_state,
      ~settings: Settings.t,
      ~highlights,
      ~results: ModelResults.t,
      ~result_key,
      ~statics: CachedStatics.statics,
      ~selected,
      editor: Editor.t,
    ) => {
  let result = ModelResults.lookup(results, result_key) |> Option.get;
  [
    CellEditor.view(
      ~select,
      ~inject=
        fun
        | MainEditor(a) => inject(PerformAction(a))
        | ResultAction(StepperAction(a)) =>
          inject(StepperAction(result_key, a))
        | ResultAction(ToggleStepper) => inject(ToggleStepper(result_key))
        | ResultAction(StepperEditorAction(_, a)) =>
          inject(PerformAction(a)),
      ~inject_global=inject,
      ~ui_state,
      ~settings,
      ~highlights,
      ~selected,
      ~locked=false,
      {
        editor: {
          editor,
          statics,
        },
        result,
      },
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
