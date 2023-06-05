open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open Util.Web;

type state = (Id.t, Editor.t);

let view =
    (
      ~model: Model.t,
      ~inject,
      ~font_metrics,
      ~show_backpack_targets,
      ~mousedown,
      ~editor: Editor.t,
      ~settings: ModelSettings.t,
      ~langDocMessages: LangDocMessages.t,
      ~result: ModelResult.simple,
      ~ctx_init: Ctx.t,
    ) => {
  let zipper = editor.state.zipper;
  let (term, _) = MakeTerm.from_zip_for_sem(zipper);
  let info_map = Statics.mk_map_ctx(ctx_init, term);
  let color_highlighting: option(ColorSteps.colorMap) =
    if (langDocMessages.highlight && langDocMessages.show) {
      Some(LangDoc.get_color_map(~doc=langDocMessages, zipper));
    } else {
      None;
    };

  let code_id = "code-container";
  let editor_view =
    Cell.editor_with_result_view(
      ~mvu_states=model.mvu_states,
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
      ~ctx_init: Ctx.t,
      editor,
    );
  let ci_view =
    settings.statics
      ? [
        CursorInspector.view(
          ~inject,
          ~settings,
          ~show_lang_doc=langDocMessages.show,
          zipper,
          info_map,
        ),
      ]
      : [];
  let bottom_bar = div(~attr=Attr.class_("bottom-bar"), ci_view);
  let right_panel =
    langDocMessages.show && settings.statics
      ? [
        LangDoc.view(
          ~inject,
          ~font_metrics,
          ~settings,
          ~doc=langDocMessages,
          Indicated.index(zipper),
          info_map,
        ),
      ]
      : [];

  [
    div(
      ~attr=Attr.id("main"),
      [div(~attr=clss(["editor", "single"]), [editor_view] @ right_panel)],
    ),
    bottom_bar,
  ];
};

let download_slide_state = state => {
  let json_data = ScratchSlide.export(state);
  JsUtil.download_json("hazel-scratchpad", json_data);
};

let download_slide_init_state = state => {
  let slide_init_state = ScratchSlide.export_init(state);
  let contents =
    "let slide : ScratchSlide.persistent_state = " ++ slide_init_state;
  JsUtil.download_string_file(
    ~filename="exported_slide_init_state.ml",
    ~content_type="text/plain",
    ~contents,
  );
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

  // for pasting into files like LanguageRefSlide.ml (note .ml extension)
  let export_init_button =
    SchoolSettings.show_instructor
      ? Some(
          Widgets.button(
            Icons.export,
            _ => {
              download_slide_init_state(state);
              Virtual_dom.Vdom.Effect.Ignore;
            },
            ~tooltip="Export Slide Persistent State Value",
          ),
        )
      : None;
  let reset_button =
    Widgets.button(
      Icons.trash,
      _ => {
        let confirmed =
          JsUtil.confirm(
            "Are you SURE you want to reset this scratchpad? You will lose any existing code.",
          );
        if (confirmed) {
          inject(ResetSlide);
        } else {
          Virtual_dom.Vdom.Effect.Ignore;
        };
      },
      ~tooltip="Reset Scratchpad",
    );
  [export_button, import_button]
  @ Option.to_list(export_init_button)
  @ [reset_button];
};
