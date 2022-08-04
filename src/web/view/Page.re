open Virtual_dom.Vdom;
open Node;

// let logo = (~font_metrics) => {
//   let piece = (step, color: Sort.t, shape: PieceDec.piece_shape, s): Measured.t =>
//     Measured.annot(Piece({color, shape, step}), Text(s));
//   let l =
//     Measured.(
//       spaces(
//         Selected,
//         [
//           piece(0, Exp, ((Convex, 0), (Convex, 0)), "t"),
//           piece(1, Pat, ((Concave, 0), (Convex, 0)), "y"),
//           piece(2, Typ, ((Concave, 0), (Concave, 0)), "l"),
//           piece(3, Selected, ((Convex, 0), (Concave, 1)), "r"),
//         ],
//       )
//     );
//   Code.view_of_layout(
//     ~id="logo",
//     ~text_id="logo-text",
//     ~font_metrics,
//     DecPaths.mk(~logo_pieces=[0, 1, 2, 3], ()),
//     l,
//   );
// };

let unless = (p, a) => p ? Event.Many([]) : a;

let button = (icon, action) =>
  div([Attr.class_("topbar-icon"), Attr.on_mousedown(action)], [icon]);

let link = (str, url, icon) =>
  div(
    [Attr.id(str)],
    [a(Attr.[href(url), create("target", "_blank")], [icon])],
  );

let undo = (~inject, ~disabled: bool) => {
  let clss = disabled ? ["disabled"] : [];
  let undo = _ => unless(disabled, inject(Update.Undo));
  span(
    Attr.[
      id("undo"),
      classes(["history-button", ...clss]),
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
      classes(["history-button", ...clss]),
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

let center_panel_view = (~inject, cur_idx) => {
  let increment_editor = _ => {
    let next_ed = (cur_idx + 1) mod LocalStorage.num_editors;
    Log.append_json_updates_log();
    inject(Update.SwitchEditor(next_ed));
  };
  let decrement_editor = _ => {
    let prev_ed = Util.IntUtil.modulo(cur_idx - 1, LocalStorage.num_editors);
    Log.append_json_updates_log();
    inject(Update.SwitchEditor(prev_ed));
  };
  let toggle_captions = _ => inject(Update.Set(Captions));
  let current_editor =
    Printf.sprintf("%d / %d", cur_idx + 1, LocalStorage.num_editors);
  div(
    [Attr.id("editor-id")],
    [
      button(Icons.back, decrement_editor),
      div([Attr.on_mousedown(toggle_captions)], [text(current_editor)]),
      button(Icons.forward, increment_editor),
    ],
  );
};

let right_panel_view = (~inject) =>
  div(
    [Attr.id("about-button-container")],
    [
      button(Icons.eye, _ => inject(Update.Set(WhitespaceIcons))),
      button(Icons.trash, _ => inject(Update.LoadDefault)),
      link("github", "https://github.com/hazelgrove/hazel", Icons.github),
    ],
  );

let left_panel_view = (~inject, model: Model.t) =>
  div(
    [Attr.id("history-button-container")],
    [
      undo(~inject, ~disabled=!ActionHistory.can_undo(model.history)),
      redo(~inject, ~disabled=!ActionHistory.can_redo(model.history)),
      center_panel_view(~inject, Model.current_editor(model)),
      button(Icons.export, copy_log_to_clipboard),
      right_panel_view(~inject),
    ],
  );

let top_bar_view = (~inject, model: Model.t) =>
  div(
    [Attr.id("top-bar")],
    [
      left_panel_view(~inject, model),
      //center_panel_view(~inject, Model.current_editor(model)),
      //right_panel_view(~inject),
    ],
  );

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
