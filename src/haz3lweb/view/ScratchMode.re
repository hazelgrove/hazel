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
  let (term, _) = MakeTerm.from_zip_for_sem(zipper);
  let (info_map, suggestions) = Statics.mk_map_and_inference_solutions(term);
  let _ctx_init = ctx_init;
  //let info_map = Interface.Statics.mk_map_ctx(settings.core, ctx_init, term); // TODO anand and raef: we need to use this instead; figure out how
  let global_inference_info =
    InferenceResult.mk_global_inference_info(
      settings.core.inference,
      suggestions,
    );
  let result =
    ModelResult.get_simple(
      ModelResults.lookup(results, ScratchSlide.scratch_key),
    );
  let color_highlighting: option(ColorSteps.colorMap) =
    if (langDocMessages.highlight && langDocMessages.show) {
      Some(
        LangDoc.get_color_map(
          ~global_inference_info,
          ~settings,
          ~doc=langDocMessages,
          zipper,
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
      ~term,
      ~result,
      editor,
    );
  let bottom_bar =
    CursorInspector.view(
      ~inject,
      ~settings,
      ~font_metrics,
      ~show_lang_doc=langDocMessages.show,
      zipper,
      info_map,
      global_inference_info,
    );
  let sidebar =
    langDocMessages.show && settings.core.statics
      ? LangDoc.view(
          ~inject,
          ~font_metrics,
          ~settings,
          ~doc=langDocMessages,
          Indicated.index(zipper),
          info_map,
          global_inference_info,
        )
      : div_empty;
  [
    div(
      ~attr=Attr.id("main"),
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
