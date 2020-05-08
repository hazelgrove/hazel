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
  Dom_html.window##.onresize :=
    Dom_html.handler(_ => {
      update_font_metrics();
      Js._true;
    });
  update_font_metrics();

  Dom_html.window##.onfocus :=
    Dom_html.handler(_ => {
      Cell.focus();
      Js._true;
    });
  Cell.focus();

  Async_kernel.Deferred.return(State.{changing_cards: ref(false)});
};

let restart_cursor_animation = caret_elem => {
  caret_elem##.classList##remove(Js.string("blink"));
  // necessary to trigger reflow
  let _ = caret_elem##getBoundingClientRect;
  caret_elem##.classList##add(Js.string("blink"));
};

let scroll_cursor_into_view_if_needed = caret_elem => {
  let page_rect =
    JSUtil.force_get_elem_by_id("page-area")##getBoundingClientRect;
  let caret_rect = caret_elem##getBoundingClientRect;
  if (caret_rect##.top < page_rect##.top) {
    caret_elem##scrollIntoView(Js._true);
  } else if (caret_rect##.bottom > page_rect##.bottom) {
    caret_elem##scrollIntoView(Js._false);
  };
};

let create =
    (
      model: Incr.t(Model.t),
      ~old_model as _: Incr.t(Model.t),
      ~inject: Update.Action.t => Vdom.Event.t,
    ) => {
  open Incr.Let_syntax;
  let%map model = model;
  Printf.printf("\n== Hazel.create times ==\n");
  TimeUtil.measure_time("Hazel.create", () =>
    Component.create(
      ~apply_action=Update.apply_action(model),
      ~on_display=
        (_, ~schedule_action as _) =>
          if (Model.is_cell_focused(model)) {
            let caret_elem = JSUtil.force_get_elem_by_id("caret");
            restart_cursor_animation(caret_elem);
            scroll_cursor_into_view_if_needed(caret_elem);
          },
      model,
      Page.view(~inject, model),
    )
  );
};
