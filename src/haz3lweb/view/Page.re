open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Haz3lcore;

let button = (~tooltip="", icon, action) =>
  div(
    ~attr=
      Attr.many([
        clss(["icon"]),
        Attr.on_mousedown(action),
        Attr.title(tooltip),
      ]),
    [icon],
  );

let button_d = (~tooltip="", icon, action, ~disabled: bool) =>
  div(
    ~attr=
      Attr.many([
        clss(["icon"] @ (disabled ? ["disabled"] : [])),
        Attr.title(tooltip),
        Attr.on_mousedown(_ => unless(disabled, action)),
      ]),
    [icon],
  );

let link = (~tooltip="", icon, url) =>
  div(
    ~attr=clss(["icon"]),
    [
      a(
        ~attr=
          Attr.many(
            Attr.[href(url), title(tooltip), create("target", "_blank")],
          ),
        [icon],
      ),
    ],
  );

let toggle = (~tooltip="", label, active, action) =>
  div(
    ~attr=
      Attr.many([
        clss(["toggle-switch"] @ (active ? ["active"] : [])),
        Attr.on_click(action),
        Attr.title(tooltip),
      ]),
    [div(~attr=clss(["toggle-knob"]), [text(label)])],
  );

let copy_log_to_clipboard = _ => {
  Log.append_json_updates_log();
  JsUtil.copy_to_clipboard(Log.get_json_update_log_string());
  Virtual_dom.Vdom.Effect.Ignore;
};

let next_slide = (~inject: Update.t => 'a, cur_slide, num_slides, _) => {
  let next_ed = (cur_slide + 1) mod num_slides;
  Log.append_json_updates_log();
  inject(SwitchEditor(next_ed));
};

let prev_slide = (~inject: Update.t => 'a, cur_slide, num_slides, _) => {
  let prev_ed = Util.IntUtil.modulo(cur_slide - 1, num_slides);
  Log.append_json_updates_log();
  inject(SwitchEditor(prev_ed));
};

let editor_mode_toggle_view = (~inject: Update.t => 'a, ~model: Model.t) => {
  let id = Attr.id("editor-mode");
  let toggle_mode = Attr.on_mousedown(_ => inject(ToggleMode));
  let num_slides = Editors.num_slides(model.editors);
  let tooltip = Attr.title("Toggle Mode");
  switch (model.editors) {
  | Scratch(_) =>
    let cur_slide = Editors.cur_slide(model.editors);
    let cur_slide_text = Printf.sprintf("%d / %d", cur_slide + 1, num_slides);
    div(
      ~attr=id,
      [
        div(~attr=toggle_mode, [text("Scratch")]),
        button(Icons.back, prev_slide(~inject, cur_slide, num_slides)),
        text(cur_slide_text),
        button(Icons.forward, next_slide(~inject, cur_slide, num_slides)),
      ],
    );
  | School(_) =>
    div(
      ~attr=id,
      [div(~attr=Attr.many([toggle_mode, tooltip]), [text("School")])]
      @ (
        if (SchoolSettings.show_instructor) {
          [
            toggle(
              "🎓",
              ~tooltip="Toggle Instructor Mode",
              model.settings.instructor_mode,
              _ =>
              inject(Set(InstructorMode))
            ),
          ];
        } else {
          [];
        }
      ),
    )
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

let top_bar_view = (~inject: Update.t => 'a, model: Model.t) => {
  let ed = Editors.get_editor(model.editors);
  let can_undo = Editor.can_undo(ed);
  let can_redo = Editor.can_redo(ed);
  div(
    ~attr=Attr.id("top-bar"),
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
          button(
            Icons.export,
            copy_log_to_clipboard,
            ~tooltip="Copy Log to Clipboard",
          ),
          button(
            Icons.eye,
            _ => inject(Set(WhitespaceIcons)),
            ~tooltip="Toggle Visible Whitespace",
          ),
          button(
            Icons.trash,
            _ => inject(LoadDefault),
            ~tooltip="Load Default",
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
    ],
  );
};

let editors_view =
    (
      ~inject,
      {
        editors,
        font_metrics,
        show_backpack_targets,
        settings,
        mousedown,
        results,
        _,
      }: Model.t,
    ) => {
  switch (editors) {
  | Scratch(_) =>
    let result_key = Editors.single_result_key;
    let editor = Editors.get_editor(editors);
    let result =
      settings.dynamics
        ? ModelResult.get_simple(ModelResults.lookup(results, result_key))
        : None;
    ScratchSlide.view(
      ~inject,
      ~font_metrics,
      ~mousedown,
      ~show_backpack_targets,
      ~settings,
      ~editor,
      ~result,
    );
  | School(state) =>
    let results = settings.dynamics ? Some(results) : None;
    let school_mode = SchoolMode.mk(~settings, ~state, ~results);
    SchoolMode.view(
      ~inject,
      ~font_metrics,
      ~mousedown,
      ~show_backpack_targets,
      school_mode,
    );
  };
};

let view = (~inject, ~handlers, model: Model.t) => {
  div(
    ~attr=
      Attr.many(
        Attr.[
          id("page"),
          // safety handler in case mousedown overlay doesn't catch it
          on_mouseup(_ => inject(Update.Mouseup)),
          ...handlers(~inject, ~model),
        ],
      ),
    [
      FontSpecimen.view("font-specimen"),
      DecUtil.filters,
      top_bar_view(~inject, model),
      editors_view(~inject, model),
      div(~attr=Attr.id("blorg"), []),
    ],
  );
};
