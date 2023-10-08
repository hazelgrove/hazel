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
  let info_map = Statics.mk_map(term);
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
      editor,
    );
  let bottom_bar =
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
  let sidebar =
    langDocMessages.show && settings.statics
      ? LangDoc.view(
          ~inject,
          ~font_metrics,
          ~settings,
          ~doc=langDocMessages,
          Indicated.index(zipper),
          info_map,
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
let breadcrumb_bar =
    (
      ~inject,
      ~model as
        {
          editors,
          //font_metrics,
          //show_backpack_targets,
          //settings,
          //mousedown,
          //langDocMessages,
          _,
          //results,
        }: Model.t,
    ) => {
  let editor = Editors.get_editor(editors);
  let zipper = editor.state.zipper;
  let unselected = Zipper.unselect_and_zip(zipper);
  let (term, _) = MakeTerm.go(unselected);
  let info_map = Statics.mk_map(term);
  switch (zipper.backpack, Indicated.index(zipper)) {
  | ([_, ..._], _) => [div([text("")])]
  | (_, None) => [div([text("")])]
  | (_, Some(id)) =>
    switch (Id.Map.find_opt(id, info_map)) {
    | None => [div([text("")])]
    | Some(ci) =>
      let ancestors = Info.ancestors_of(ci);
      let check_fun_in_let = ancestors =>
        switch (Id.Map.find_opt(List.hd(ancestors), info_map)) {
        | Some(v) =>
          switch (v) {
          | Info.InfoExp({term, _}) =>
            switch (term.term) {
            | Let({term: Var(name), _}, {term: Fun(_), _}, _) => name
            | _ => ""
            }
          | _ => ""
          }
        | _ => ""
        };
      let check_parenet_fun = ancestor =>
        switch (Id.Map.find_opt(ancestor, info_map)) {
        | Some(v) =>
          switch (v) {
          | Info.InfoExp({term, ancestors, _}) =>
            switch (term.term) {
            | Fun(_) => check_fun_in_let(ancestors)
            | _ => ""
            }
          | _ => ""
          }
        | None => ""
        };
      let rec ancestors_string = ancestors => {
        switch (ancestors) {
        | [] => []
        | [x, ...xs] =>
          if (check_parenet_fun(x) == "") {
            ancestors_string(xs);
          } else {
            [
              div(
                ~attr=
                  Attr.many([
                    clss(["breadcrumb_bar_function"]),
                    Attr.on_click(_ =>
                      inject(UpdateAction.PerformAction(Jump(TileId(x))))
                    ),
                  ]),
                [text(check_parenet_fun(x)), text("    ->")],
              ),
            ]
            @ ancestors_string(xs);
          }
        };
      };
      //[
      //  div(
      //    ~attr=clss(["breadcrumb_bar"]),
      ancestors_string(List.rev(ancestors));
    //  ),
    //];
    }
  };
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
