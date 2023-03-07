open Virtual_dom.Vdom;
open Js_of_ocaml;
open Node;
open Util.Web;
open Haz3lcore;
open Widgets;

let next_slide = (~inject: Update.t => 'a, cur_slide, num_slides, _) => {
  let next_ed = (cur_slide + 1) mod num_slides;
  inject(SwitchSlide(next_ed));
};

let download_editor_state = (~instructor_mode) => {
  let data = Export.export_all(~instructor_mode);
  JsUtil.download_json(SchoolSettings.filename, data);
};

let prev_slide = (~inject: Update.t => 'a, cur_slide, num_slides, _) => {
  let prev_ed = Util.IntUtil.modulo(cur_slide - 1, num_slides);
  inject(SwitchSlide(prev_ed));
};

let slide_toggle_view = (~inject, ~model: Model.t, ~caption, ~control) => {
  let id = Attr.id("editor-mode");
  let tooltip = Attr.title("Toggle Mode");
  let toggle_mode = Attr.on_mousedown(_ => inject(Update.ToggleMode));
  let cur_slide = Editors.cur_slide(model.editors);
  let num_slides = Editors.num_slides(model.editors);
  let cur_slide_text = Printf.sprintf("%d / %d", cur_slide + 1, num_slides);
  div(
    ~attr=id,
    [
      div(~attr=Attr.many([toggle_mode, tooltip]), [text(caption)]),
      button(Icons.back, prev_slide(~inject, cur_slide, num_slides)),
      text(cur_slide_text),
      button(Icons.forward, next_slide(~inject, cur_slide, num_slides)),
    ]
    @ Option.to_list(control),
  );
};

let editor_mode_toggle_view = (~inject: Update.t => 'a, ~model: Model.t) => {
  switch (model.editors) {
  | DebugLoad => failwith("impossible")
  | Scratch(_) =>
    slide_toggle_view(~inject, ~model, ~caption="Scratch", ~control=None)
  | School(_) =>
    let control =
      if (SchoolSettings.show_instructor) {
        Some(
          toggle(
            "🎓",
            ~tooltip="Toggle Instructor Mode",
            model.settings.instructor_mode,
            _ =>
            inject(Set(InstructorMode))
          ),
        );
      } else {
        None;
      };
    slide_toggle_view(~inject, ~model, ~caption="Exercises", ~control);
  };
};

let menu_icon =
  div(
    ~attr=clss(["menu-icon"]),
    [
      div(
        ~attr=clss(["icon", "menu-icon-inner"]),
        [
          a(
            ~attr=
              Attr.many(
                Attr.[
                  href("https://hazel.org"),
                  title("Hazel"),
                  create("target", "_blank"),
                ],
              ),
            [Icons.hazelnut],
          ),
        ],
      ),
    ],
  );

let top_bar_view =
    (
      ~inject: Update.t => 'a,
      ~toolbar_buttons: list(Node.t),
      ~top_right: option(Node.t)=?,
      model: Model.t,
    ) => {
  let ed = Editors.get_editor(model.editors);
  let can_undo = Editor.can_undo(ed);
  let can_redo = Editor.can_redo(ed);
  let top_left_bar =
    div(
      ~attr=Attr.id("top-left-bar"),
      [
        menu_icon,
        div(
          ~attr=clss(["menu"]),
          [
            toggle("τ", ~tooltip="Toggle Statics", model.settings.statics, _ =>
              inject(Set(Statics))
            ),
            toggle(
              "𝛿", ~tooltip="Toggle Dynamics", model.settings.dynamics, _ =>
              inject(Set(Dynamics))
            ),
            toggle(
              "b",
              ~tooltip="Toggle Performance Benchmark",
              model.settings.benchmark,
              _ =>
              inject(Set(Benchmark))
            ),
            button(
              Icons.export,
              _ => {
                download_editor_state(
                  ~instructor_mode=model.settings.instructor_mode,
                );
                Virtual_dom.Vdom.Effect.Ignore;
              },
              ~tooltip="Export Submission",
            ),
            file_select_button(
              "import-submission",
              Icons.import,
              file => {
                switch (file) {
                | None => Virtual_dom.Vdom.Effect.Ignore
                | Some(file) => inject(InitImportAll(file))
                }
              },
              ~tooltip="Import Submission",
            ),
            file_select_button(
              "replay-submission",
              Icons.import,
              file => {
                switch (file) {
                | None => Virtual_dom.Vdom.Effect.Ignore
                | Some(file) => inject(ReplayAction(LoadReplay(file)))
                }
              },
              ~tooltip="Replay Submission",
            ),
            button(
              Icons.eye,
              _ => inject(Set(SecondaryIcons)),
              ~tooltip="Toggle Visible Secondary",
            ),
            link(
              Icons.github,
              "https://github.com/hazelgrove/hazel",
              ~tooltip="Hazel on GitHub",
            ),
          ],
        ),
        button_d(
          Icons.undo,
          inject(Undo),
          ~disabled=!can_undo,
          ~tooltip="Undo",
        ),
        button_d(
          Icons.redo,
          inject(Redo),
          ~disabled=!can_redo,
          ~tooltip="Redo",
        ),
        editor_mode_toggle_view(~inject, ~model),
      ]
      @ toolbar_buttons,
    );
  let top_right_bar =
    div(~attr=Attr.id("top-right-bar"), Option.to_list(top_right));
  div(
    ~attr=Attr.id("top-bar"),
    [div(~attr=Attr.id("top-bar-content"), [top_left_bar, top_right_bar])],
  );
};

let main_ui_view =
    (
      ~inject,
      {
        editors,
        font_metrics,
        show_backpack_targets,
        settings,
        mousedown,
        results,
        langDocMessages,
        replay,
        _,
      } as model: Model.t,
    ) => {
  switch (editors) {
  | DebugLoad => [DebugMode.view(~inject)]
  | Scratch(idx, slides) =>
    let toolbar_buttons =
      ScratchMode.toolbar_buttons(~inject, List.nth(slides, idx));
    let top_bar_view = top_bar_view(~inject, ~toolbar_buttons, model);
    let result_key = ScratchSlide.scratch_key;
    let editor = Editors.get_editor(editors);
    let result =
      settings.dynamics
        ? ModelResult.get_simple(ModelResults.lookup(results, result_key))
        : None;
    [
      top_bar_view,
      ScratchMode.view(
        ~inject,
        ~font_metrics,
        ~mousedown,
        ~show_backpack_targets,
        ~settings,
        ~langDocMessages,
        ~editor,
        ~replay,
        ~result,
      ),
    ];
  | School(_, _, exercise) =>
    let toolbar_buttons =
      SchoolMode.toolbar_buttons(~inject, ~settings, editors);
    let results = settings.dynamics ? Some(results) : None;
    let school_mode =
      SchoolMode.mk(~settings, ~exercise, ~results, ~langDocMessages);
    let grading_report = school_mode.grading_report;
    let overall_score =
      Grading.GradingReport.view_overall_score(grading_report);
    let top_bar_view =
      top_bar_view(
        ~inject,
        model,
        ~toolbar_buttons,
        ~top_right=overall_score,
      );
    [
      top_bar_view,
      SchoolMode.view(
        ~inject,
        ~font_metrics,
        ~mousedown,
        ~show_backpack_targets,
        school_mode,
      ),
    ];
  };
};

let page_id = "page";

let get_selection = (model: Model.t): string =>
  model.editors |> Editors.get_editor |> Printer.to_string_selection;

let view = (~inject, ~handlers, model: Model.t) => {
  let main_ui = main_ui_view(~inject, model);
  div(
    ~attr=
      Attr.many(
        Attr.[
          id(page_id),
          // safety handler in case mousedown overlay doesn't catch it
          on_mouseup(_ => inject(Update.Mouseup)),
          on_blur(_ => {
            JsUtil.focus_clipboard_shim();
            Virtual_dom.Vdom.Effect.Ignore;
          }),
          on_focus(_ => {
            JsUtil.focus_clipboard_shim();
            Virtual_dom.Vdom.Effect.Ignore;
          }),
          on_copy(_ => {
            JsUtil.copy(get_selection(model));
            Virtual_dom.Vdom.Effect.Ignore;
          }),
          on_cut(_ => {
            JsUtil.copy(get_selection(model));
            inject(UpdateAction.PerformAction(Destruct(Left)));
          }),
          on_paste(evt => {
            let pasted_text =
              Js.to_string(evt##.clipboardData##getData(Js.string("text")))
              |> Str.global_replace(Str.regexp("\n[ ]*"), "\n");
            Dom.preventDefault(evt);
            inject(UpdateAction.Paste(pasted_text));
          }),
          ...handlers(~inject, ~model),
        ],
      ),
    [
      FontSpecimen.view("font-specimen"),
      DecUtil.filters,
      JsUtil.clipboard_shim,
    ]
    @ main_ui,
  );
};
