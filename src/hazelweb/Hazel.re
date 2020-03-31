module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
open Incr_dom;

// https://github.com/janestreet/incr_dom/blob/6aa4aca2cfc82a17bbcc0424ff6b0ae3d6d8d540/example/text_input/README.md
// https://github.com/janestreet/incr_dom/blob/master/src/app_intf.ml

module Model = Model;
module Action = Update.Action;
module State = State;

let on_startup = (~schedule_action, _) => {
  Dom_html.window##.onfocus :=
    Dom_html.handler(_ => {
      schedule_action(Update.Action.FocusWindow);
      Js._true;
    });
  schedule_action(Update.Action.FocusCell);
  let update_font_metrics = () => {
    let rect =
      JSUtil.force_get_elem_by_id("font-specimen")##getBoundingClientRect;
    schedule_action(
      Update.Action.UpdateFontMetrics({
        row_height: rect##.bottom -. rect##.top,
        col_width: rect##.right -. rect##.left,
      }),
    );
  };
  update_font_metrics();
  Dom_html.window##.onresize :=
    Dom_html.handler(_ => {
      update_font_metrics();
      Js._true;
    });

  Async_kernel.Deferred.return(
    State.{setting_caret: ref(false), changing_cards: ref(false)},
  );
};

let restart_caret_animation = () => {
  let caret = JSUtil.force_get_elem_by_id("caret");
  caret##.classList##remove(Js.string("blink"));
  caret##focus;
  caret##.classList##add(Js.string("blink"));
};

let create =
    (
      model: Incr.t(Model.t),
      ~old_model as _: Incr.t(Model.t),
      ~inject: Update.Action.t => Vdom.Event.t,
    ) => {
  open Incr.Let_syntax;
  let%map model = model;
  let (on_display, view) = Page.view(~inject, model);
  Component.create(
    ~apply_action=Update.apply_action(model),
    ~on_display,
    model,
    view,
  );
};
