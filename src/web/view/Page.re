open Virtual_dom.Vdom;
open Node;
open Util.Web;

let button = (cls, icon, action) =>
  div([clss(["topbar-icon", cls]), Attr.on_mousedown(action)], [icon]);

let link = (id, url, icon) =>
  div(
    [Attr.id(id), clss(["topbar-icon"])],
    [a(Attr.[href(url), create("target", "_blank")], [icon])],
  );

let undo = (~inject, ~disabled: bool) => {
  let clss = disabled ? ["disabled"] : [];
  let undo = _ => unless(disabled, inject(Update.Undo));
  span(
    Attr.[
      id("undo"),
      classes(["topbar-icon", "history-button", ...clss]),
      on_mousedown(undo),
    ],
    [Icons.undo],
  );
};

let redo = (~inject, ~disabled: bool) => {
  let clss = disabled ? ["disabled"] : [];
  let redo = _ => unless(disabled, inject(Update.Redo));
  span(
    Attr.[
      id("redo"),
      classes(["topbar-icon", "history-button", ...clss]),
      on_mousedown(redo),
    ],
    [Icons.redo],
  );
};

let copy_log_to_clipboard = _ => {
  Log.append_json_updates_log();
  JsUtil.copy_to_clipboard(Log.get_json_update_log_string());
  Event.Ignore;
};

let editor_mode_view = (~inject: Update.t => 'a, ~model: Model.t) => {
  switch (model.editor_model) {
  | Simple(_) =>
    div(
      [Attr.id("editor-mode"), Attr.on_mousedown(_ => inject(ToggleMode))],
      [text("Simple")],
    )
  | Study(_n, _zs) =>
    let cur_idx = Model.current_editor(model);
    //TODO(andrew): update as general editor mode controls
    let increment_editor = _ => {
      let next_ed = (cur_idx + 1) mod LocalStorage.num_editors;
      Log.append_json_updates_log();
      inject(Update.SwitchEditor(next_ed));
    };
    let decrement_editor = _ => {
      let prev_ed =
        Util.IntUtil.modulo(cur_idx - 1, LocalStorage.num_editors);
      Log.append_json_updates_log();
      inject(Update.SwitchEditor(prev_ed));
    };
    let current_editor =
      Printf.sprintf("%d / %d", cur_idx + 1, LocalStorage.num_editors);
    div(
      [Attr.id("editor-mode")],
      [
        button("topbar-icon", Icons.back, decrement_editor),
        div(
          [Attr.on_mousedown(_ => inject(ToggleMode))],
          [text(current_editor)],
        ),
        button("topbar-icon", Icons.forward, increment_editor),
      ],
    );
  | School(_n, _zs) =>
    div(
      [Attr.id("editor-mode"), Attr.on_mousedown(_ => inject(ToggleMode))],
      [text("School")],
    )
  };
};

let menu_icon =
  div(
    [clss(["menu-icon"]), Attr.on_mousedown(copy_log_to_clipboard)],
    [
      div(
        [clss(["topbar-icon", "menu-icon-inner"])],
        [
          a(
            Attr.[href("http://hazel.org"), create("target", "_blank")],
            [Icons.hazelnut],
          ),
        ],
      ),
    ],
  );

let top_bar_view = (~inject: Update.t => 'a, model: Model.t) => {
  let history = Model.get_history(model);
  div(
    [Attr.id("top-bar")],
    [
      menu_icon,
      undo(~inject, ~disabled=!ActionHistory.can_undo(history)),
      redo(~inject, ~disabled=!ActionHistory.can_redo(history)),
      button("topbar-icon", Icons.export, copy_log_to_clipboard),
      button("topbar-icon", Icons.eye, _ => inject(Set(WhitespaceIcons))),
      button("topbar-icon", Icons.trash, _ => inject(Update.LoadDefault)),
      link("github", "https://github.com/hazelgrove/hazel", Icons.github),
      editor_mode_view(~inject, ~model),
    ],
  );
};

let editor_view =
    (
      {editor_model, font_metrics, show_backpack_targets, settings, _}: Model.t,
    ) =>
  Editor.view(
    ~editor_model,
    ~font_metrics,
    ~show_backpack_targets,
    ~settings,
  );

let view = (~inject, ~handlers, model: Model.t) => {
  div(
    Attr.[
      id("page"),
      // necessary to make cell focusable
      create("tabindex", "0"),
      on_blur(_ => {
        JsUtil.get_elem_by_id("page")##focus;
        Event.Many([]);
      }),
      ...handlers(~inject, ~model),
    ],
    [
      FontSpecimen.view("font-specimen"),
      //FontSpecimen.view("logo-font-specimen"),
      DecUtil.filters,
      top_bar_view(~inject, model),
      //editor_caption_view(model),
      editor_view(~inject, model),
    ],
  );
};
