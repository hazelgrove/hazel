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
  let info_map = Interface.Statics.mk_map_ctx(settings.core, ctx_init, term);
  let result =
    ModelResult.get_simple(
      ModelResults.lookup(results, ScratchSlide.scratch_key),
    );
  let color_highlighting: option(ColorSteps.colorMap) =
    if (langDocMessages.highlight && langDocMessages.show) {
      Some(LangDoc.get_color_map(~settings, ~doc=langDocMessages, zipper));
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
      ~show_lang_doc=langDocMessages.show,
      zipper,
      info_map,
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
        )
      : div_empty;
  let info =
    SlideContent.get_content(editors)
    |> Option.map(i => div(~attr=Attr.id("slide"), [i]))
    |> Option.to_list;
  // let img =
  //   img
  //   |> Option.map(i =>
  //        div(
  //          ~attr=Attr.id("slide"),
  //          [
  //            Node.create(
  //              "img",
  //              ~key="slide",
  //              ~attr=Attr.many([Attr.src(i), Attr.class_("slide-img")]),
  //              [],
  //            ),
  //          ],
  //        )
  //      )
  //   |> Option.to_list;
  [
    div(
      ~attr=Attr.id("main"),
      [div(~attr=clss(["editor", "single"]), info @ [editor_view])],
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
