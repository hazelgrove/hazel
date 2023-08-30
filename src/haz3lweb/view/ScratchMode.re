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
          langDocMessages,
          meta: {
            results,
            ui_state: {font_metrics, show_backpack_targets, mousedown, _},
            _,
          },
        }: Model.t,
    ) => {
  let editor = Editors.get_editor(editors);
  let zipper = editor.state.zipper;
<<<<<<< HEAD
  let unselected = Zipper.unselect_and_zip(zipper);
  let (term, _) = MakeTerm.go(unselected);
  let (info_map, ctx) = Statics.mk_map_and_inference_solutions(term);
  let global_inference_info =
    InferenceResult.mk_global_inference_info(
      langDocMessages.annotations,
      ctx,
    );

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
=======
  let (term, _) = MakeTerm.from_zip_for_sem(zipper);
  let info_map = Interface.Statics.mk_map_ctx(settings.core, ctx_init, term);
  let result =
    ModelResult.get_simple(
      ModelResults.lookup(results, ScratchSlide.scratch_key),
    );
  let color_highlighting: option(ColorSteps.colorMap) =
    if (langDocMessages.highlight && langDocMessages.show) {
      Some(LangDoc.get_color_map(~settings, ~doc=langDocMessages, zipper));
>>>>>>> llmass
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
      ~term,
      ~result,
      ~langDocMessages,
      editor,
    );
<<<<<<< HEAD
  let ci_view =
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
  let bottom_bar = div(~attr=Attr.class_("bottom-bar"), ci_view);
  let right_panel =
    langDocMessages.show && settings.statics
      ? [
        LangDoc.view(
=======
  let bottom_bar =
    CursorInspector.view(
      ~inject,
      ~settings,
      ~show_lang_doc=langDocMessages.show,
      zipper,
      info_map,
    );
  let sidebar =
    langDocMessages.show && settings.core.statics
      ? LangDoc.view(
>>>>>>> llmass
          ~inject,
          ~font_metrics,
          ~settings,
          ~doc=langDocMessages,
          Indicated.index(zipper),
          info_map,
<<<<<<< HEAD
          global_inference_info,
        ),
      ]
      : [];

=======
        )
      : div_empty;
>>>>>>> llmass
  [
    div(
      ~attr=Attr.id("main"),
      [div(~attr=clss(["editor", "single"]), [editor_view])],
    ),
    sidebar,
    bottom_bar,
  ];
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
