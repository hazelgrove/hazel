open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open Util.Web;

type state = (Id.t, Editor.t);

let view =
    (
      ~inject,
      ~ctx_init: Ctx.t,
      ~model as
        {
          editors,
          settings,
          explainThisModel,
          results,
          meta: {
            ui_state: {font_metrics, show_backpack_targets, mousedown, _},
            _,
          },
        }: Model.t,
    ) => {
  let editor = Editors.get_editor(editors);
  let zipper = editor.state.zipper;
  let (term, _) = MakeTerm.from_zip_for_sem(zipper);
  let info_map = Interface.Statics.mk_map_ctx(settings.core, ctx_init, term);
  let result_key =
    switch (editors) {
    | Scratch(slide_idx, _) =>
      ScratchSlide.scratch_key(string_of_int(slide_idx))
    | Examples(name, _) => ScratchSlide.scratch_key(name)
    | Exercise(_) => ""
    | DebugLoad => ""
    };
  let result =
    ModelResults.lookup(results, result_key)
    |> Option.value(~default=ModelResult.NoElab);
  let color_highlighting: option(ColorSteps.colorMap) =
    if (explainThisModel.highlight && explainThisModel.show) {
      //TODO(andrew): is indicated index appropriate below?
      Some(
        ExplainThis.get_color_map(
          ~doc=explainThisModel,
          Indicated.index(zipper),
          info_map,
        ),
      );
    } else {
      None;
    };

  let code_id = "code-container";
  let editor_view =
    Cell.editor_with_result_view(
      ~inject,
      ~font_metrics,
      ~show_backpack_targets,
      ~clss=["single"],
      ~selected=true,
      ~mousedown,
      ~code_id,
      ~settings,
      ~color_highlighting,
      ~info_map,
      ~result_key,
      ~result,
      editor,
    );
  let bottom_bar =
    CursorInspector.view(
      ~inject,
      ~settings,
      ~show_explain_this=explainThisModel.show,
      zipper,
      info_map,
    );
  let sidebar =
    explainThisModel.show && settings.core.statics
      ? ExplainThis.view(
          ~inject,
          ~font_metrics,
          ~settings,
          ~doc=explainThisModel,
          Indicated.index(zipper),
          info_map,
        )
      : div_empty;
  [
    div(
      ~attr=
        Attr.many([
          Attr.id("main"),
          Attr.classes([Settings.show_mode(settings.mode)]),
        ]),
      [div(~attr=clss(["editor", "single"]), [editor_view])],
    ),
    sidebar,
    bottom_bar,
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
