open Hazellib;
module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
open GeneralUtil;
open SemanticsCommon;
open Incr_dom;

// https://github.com/janestreet/incr_dom/blob/6aa4aca2cfc82a17bbcc0424ff6b0ae3d6d8d540/example/text_input/README.md
// https://github.com/janestreet/incr_dom/blob/master/src/app_intf.ml

module Model = Model;
module Action = Update.Action;
module State = State;

[@warning "-27"]
let on_startup = (~schedule_action, _) => {
  let _ =
    JSUtil.listen_to_t(
      Dom.Event.make("selectionchange"), Dom_html.document, _ =>
      schedule_action(Update.Action.SelectionChange)
    );
  Dom_html.window##.onfocus :=
    Dom_html.handler(_ => {
      schedule_action(Update.Action.FocusWindow);
      Js._true;
    });
  schedule_action(Update.Action.FocusCell);
  Async_kernel.Deferred.return(
    State.{setting_caret: ref(false), changing_cards: ref(false)},
  );
};

[@warning "-27"]
let create =
    (
      model: Incr.t(Model.t),
      ~old_model: Incr.t(Model.t),
      ~inject: Update.Action.t => Vdom.Event.t,
    ) => {
  open Incr.Let_syntax;
  let%map model = model;
  Component.create(
    ~apply_action=Update.apply_action(model),
    ~on_display=
      (state: State.t, ~schedule_action: Update.Action.t => unit) => {
        let path = model |> Model.path;
        if (! state.changing_cards^) {
          // Currently drawing non-empty hole borders along with the
          // cursor indicators on each render. This is due to an
          // unfortunate confluence of contenteditable, indented
          // multi-lining, and the desire to not include indentation
          // when decorating a deeply indented Hazel ast node.
          // contenteditable requires us to make all dom elements
          // representing editable code be inline elements (otherwise
          // the caret moves in strange ways). Now suppose we have a
          // dom element representing some deeply indented multi-line
          // node of the Hazel program. Due to its inline layout, the
          // dom element must structurally include the indentation. So
          // if we simply decorated this dom element by styling that
          // element, we would include the indentation, resulting in
          // strange term and non-empty hole indicators that extend
          // beyond the textual code. Thus, we need to draw non-empty
          // hole indicators along with cursor indicators after the
          // code text has been rendered so we can position them in
          // terms of viewport coordinates.
          // TODO figure out better solution
          CursorIndicators.draw_hole_indicators(
            model,
          );
        };
        if (state.changing_cards^) {
          switch (Code.caret_position_of_path(path)) {
          | None => assert(false)
          | Some((node, offset)) =>
            state.setting_caret := true;
            JSUtil.set_caret(node, offset);
          };
          state.changing_cards := false;
        } else if (model.is_cell_focused) {
          let (_, cursor) = path;
          switch (cursor) {
          | OnText(_)
          | OnDelim(_, _) =>
            if (!Code.is_caret_consistent_with_path(path)) {
              state.setting_caret := true;
              let (node, offset) =
                Code.caret_position_of_path(path)
                |> Opt.get(_ => assert(false));
              JSUtil.set_caret(node, offset);
            } else {
              state.setting_caret := false;
              CursorIndicators.draw(~ci=model.cursor_info);
            }
          | Staging(delim_index) =>
            JSUtil.unset_caret();
            CursorIndicators.draw(~ci=model.cursor_info);
          };
        };
      },
    model,
    Page.view(~inject, model),
  );
};
