open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open Util.Web;

type state = (Id.t, Editor.t);

let view =
    (
      ~inject,
      ~model as
        {
          editors,
          font_metrics,
          show_backpack_targets,
          settings,
          mousedown,
          langDocMessages,
          results,
          _,
        }: Model.t,
    ) => {
  let editor = Editors.get_editor(editors);
  let zipper = editor.state.zipper;
  let unselected = Zipper.unselect_and_zip(zipper);
  let (term, _) = MakeTerm.go(unselected);
  let (info_map, ctx) = Statics.mk_map_and_inference_solutions(term);
  let global_inference_info =
    InferenceResult.mk_global_inference_info(
      langDocMessages.annotations,
      ctx,
    );
  let result =
    settings.dynamics
      ? ModelResult.get_simple(
          ModelResults.lookup(results, ScratchSlide.scratch_key),
        )
      : None;
  let color_highlighting: option(ColorSteps.colorMap) =
    if (langDocMessages.highlight && langDocMessages.show) {
      Some(
        LangDoc.get_color_map(
          ~global_inference_info,
          ~doc=langDocMessages,
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
      ~result,
      ~langDocMessages,
      editor,
    );
  let bottom_bar =
    settings.statics
      ? [
        CursorInspector.view(
          ~inject,
          ~settings,
          ~font_metrics,
          ~show_lang_doc=langDocMessages.show,
          zipper,
          info_map,
          global_inference_info,
        ),
      ]
      : [];
  let sidebar =
    langDocMessages.show && settings.statics
      ? LangDoc.view(
          ~inject,
          ~font_metrics,
          ~settings,
          ~doc=langDocMessages,
          Indicated.index(zipper),
          info_map,
          global_inference_info,
        )
      : div([]);

  [
    div(
      ~attr=Attr.id("main"),
      [div(~attr=clss(["editor", "single"]), [editor_view])],
    ),
    sidebar,
  ]
  @ bottom_bar;
};

let download_slide_state = state => {
  let json_data = ScratchSlide.export(state);
  JsUtil.download_json("hazel-scratchpad", json_data);
};

let toolbar_buttons = (~inject, state: ScratchSlide.state) => {
  let export_button =
    Widgets.button(
      Icons.export,
      _ => {
        download_slide_state(state);
        Virtual_dom.Vdom.Effect.Ignore;
      },
      ~tooltip="Export Scratchpad",
    );
  let import_button =
    Widgets.file_select_button(
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

  let reset_button =
    Widgets.button(
      Icons.trash,
      _ => {
        let confirmed =
          JsUtil.confirm(
            "Are you SURE you want to reset this scratchpad? You will lose any existing code.",
          );
        if (confirmed) {
          inject(ResetCurrentEditor);
        } else {
          Virtual_dom.Vdom.Effect.Ignore;
        };
      },
      ~tooltip="Reset Scratchpad",
    );
  [export_button, import_button] @ [reset_button];
};
