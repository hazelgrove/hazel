open Virtual_dom.Vdom;
open Node;
open Util.Web;

let button = (icon, action) =>
  div(
    ~attr=Attr.many([clss(["icon"]), Attr.on_mousedown(action)]),
    [icon],
  );

let button_d = (icon, action, ~disabled: bool) =>
  div(
    ~attr=
      Attr.many([
        clss(["icon"] @ (disabled ? ["disabled"] : [])),
        Attr.on_mousedown(_ => unless(disabled, action)),
      ]),
    [icon],
  );

let link = (icon, url) =>
  div(
    ~attr=clss(["icon"]),
    [
      a(
        ~attr=Attr.many(Attr.[href(url), create("target", "_blank")]),
        [icon],
      ),
    ],
  );

let toggle = (label, active, action) =>
  div(
    ~attr=
      Attr.many([
        clss(["toggle-switch"] @ (active ? ["active"] : [])),
        Attr.on_click(action),
      ]),
    [div(~attr=clss(["toggle-knob"]), [text(label)])],
  );

let copy_log_to_clipboard = _ => {
  Log.append_json_updates_log();
  JsUtil.copy_to_clipboard(Log.get_json_update_log_string());
  Effect.Ignore;
};

let increment_editor = (~inject: Update.t => 'a, cur_idx, num_editors, _) => {
  let next_ed = (cur_idx + 1) mod num_editors;
  Log.append_json_updates_log();
  inject(SwitchEditor(next_ed));
};

let decrement_editor = (~inject: Update.t => 'a, cur_idx, num_editors, _) => {
  let prev_ed = Util.IntUtil.modulo(cur_idx - 1, num_editors);
  Log.append_json_updates_log();
  inject(SwitchEditor(prev_ed));
};

let editor_mode_view = (~inject: Update.t => 'a, ~model: Model.t) => {
  let id = Attr.id("editor-mode");
  let toggle_mode = Attr.on_mousedown(_ => inject(ToggleMode));
  let num_editors = Model.num_editors(model);
  switch (model.editor_model) {
  | Simple(_) => div(~attr=Attr.many([id, toggle_mode]), [text("Sketch")])
  | School(_) =>
    div(
      ~attr=id,
      [
        div(~attr=toggle_mode, [text("School")]),
        toggle("🎓", model.settings.student, _ => inject(Set(Student))),
      ],
    )
  | Study(_) =>
    let cur_idx = Model.current_editor(model);
    let current_editor = Printf.sprintf("%d / %d", cur_idx + 1, num_editors);
    div(
      ~attr=id,
      [
        div(~attr=toggle_mode, [text("Studies")]),
        button(Icons.back, decrement_editor(~inject, cur_idx, num_editors)),
        text(current_editor),
        button(
          Icons.forward,
          increment_editor(~inject, cur_idx, num_editors),
        ),
      ],
    );
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
                Attr.[href("http://hazel.org"), create("target", "_blank")],
              ),
            [Icons.hazelnut],
          ),
        ],
      ),
    ],
  );

let top_bar_view = (~inject: Update.t => 'a, model: Model.t) => {
  let history = Model.get_history(model);
  let can_undo = ActionHistory.can_undo(history);
  let can_redo = ActionHistory.can_redo(history);
  div(
    ~attr=Attr.id("top-bar"),
    [
      menu_icon,
      div(
        ~attr=clss(["menu"]),
        [
          toggle("τ", model.settings.statics, _ => inject(Set(Statics))),
          toggle("𝛿", model.settings.dynamics, _ =>
            inject(Set(Dynamics))
          ),
          button(Icons.export, copy_log_to_clipboard),
          button(Icons.eye, _ => inject(Set(WhitespaceIcons))),
          button(Icons.trash, _ => inject(LoadDefault)),
          link(Icons.github, "https://github.com/hazelgrove/hazel"),
        ],
      ),
      button_d(Icons.undo, inject(Undo), ~disabled=!can_undo),
      button_d(Icons.redo, inject(Redo), ~disabled=!can_redo),
      editor_mode_view(~inject, ~model),
    ],
  );
};

let editor_view =
    (
      {
        editor_model,
        font_metrics,
        show_backpack_targets,
        settings,
        mousedown,
        _,
      }: Model.t,
    ) =>
  Editor.view(
    ~editor_model,
    ~font_metrics,
    ~show_backpack_targets,
    ~mousedown,
    ~settings,
  );

let view = (~inject, ~handlers, model: Model.t) => {
  div(
    ~attr=
      Attr.many(
        Attr.[
          id("page"),
          // necessary to make cell focusable
          create("tabindex", "0"),
          on_blur(_ => {
            JsUtil.get_elem_by_id("page")##focus;
            Effect.Many([]);
          }),
          // safety handler in case mousedown overlay doesn't catch it
          on_mouseup(_ => inject(Update.Mouseup)),
          ...handlers(~inject, ~model),
        ],
      ),
    [
      FontSpecimen.view("font-specimen"),
      DecUtil.filters,
      top_bar_view(~inject, model),
      editor_view(~inject, model),
      div(~attr=Attr.id("blorg"), []),
    ],
  );
};
