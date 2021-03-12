/**
 * The Hazel Incr_dom component.
 * See <https://github.com/janestreet/incr_dom/blob/master/src/app_intf.ml>
 * for the expected interface.
 * See <https://github.com/janestreet/incr_dom/tree/master/example/text_input>
 * for an example-driven overview.
 */
module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module ResizeObserver = Js_of_ocaml.ResizeObserver;
open Incr_dom;

module Model = Model;
module Action = ModelAction;
module State = State;

// see incr_dom app_intf.ml
let on_startup = (~schedule_action, _) => {
  /* we need line heights + character widths for various layout computations,
      so we created a font specimen and update font metrics whenever that
     element resizes. */
  let _ =
    ResizeObserver.observe(
      ~node=JSUtil.force_get_elem_by_id("font-specimen"),
      ~f=
        (entries, _) => {
          let array = Js_of_ocaml.Js.to_array(entries);
          switch (array) {
          | [|entry|] =>
            let rect = entry##.contentRect;
            schedule_action(
              ModelAction.UpdateFontMetrics({
                row_height: rect##.bottom -. rect##.top,
                col_width: rect##.right -. rect##.left,
              }),
            );
          | _ => failwith("Expected 1 entry")
          };
        },
      (),
    );

  /* need to know whether a Mac is being used to determine certain key
     combinations, such as Ctrl+Z vs Cmd+Z for undo */
  let is_mac =
    Dom_html.window##.navigator##.platform##toUpperCase##indexOf(
      Js.string("MAC"),
    )
    >= 0;
  schedule_action(UpdateIsMac(is_mac));

  /* preserve editor focus across window focus/blur */
  Dom_html.window##.onfocus :=
    Dom_html.handler(_ => {
      UHCode.focus();
      Js._true;
    });
  UHCode.focus();

  Async_kernel.Deferred.return(State.State);
};

let restart_cursor_animation = caret_elem => {
  caret_elem##.classList##remove(Js.string("blink"));
  // necessary to trigger reflow
  // <https://css-tricks.com/restart-css-animation/>
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

let scroll_history_panel_entry = entry_elem => {
  let panel_rect =
    JSUtil.force_get_elem_by_id("history-body")##getBoundingClientRect;

  let entry_rect = entry_elem##getBoundingClientRect;
  if (entry_rect##.top < panel_rect##.top) {
    entry_elem##scrollIntoView(Js._true);
  } else if (entry_rect##.bottom > panel_rect##.bottom) {
    entry_elem##scrollIntoView(Js._false);
  };
};

let create =
    (
      model: Incr.t(Model.t),
      ~old_model as _: Incr.t(Model.t),
      ~inject: ModelAction.t => Vdom.Event.t,
    ) => {
  open Incr.Let_syntax;
  let%map model = model;

  let performance = model.settings.performance;
  if (performance.measure) {
    Printf.printf("\n== Hazel.create times ==\n");
  };
  TimeUtil.measure_time(
    "Hazel.create", performance.measure && performance.hazel_create, () =>
    Component.create(
      ~apply_action=Update.apply_action(model),
      // for things that require actual DOM manipulation post-render
      ~on_display=
        (_, ~schedule_action as _) => {
          if (!Model.get_undo_history(model).disable_auto_scrolling) {
            switch (JSUtil.get_elem_by_id("cur-selected-entry")) {
            | Some(entry_elem) => scroll_history_panel_entry(entry_elem)
            | None => ()
            };
          };
          if (Model.is_cell_focused(model)) {
            // if cell is focused in model, make sure
            // cell element is focused in DOM
            switch (Js.Opt.to_option(Dom_html.document##.activeElement)) {
            | Some(elem) when Js.to_string(elem##.id) == "cell" => ()
            | _ => UHCode.focus()
            };
            let caret_elem = JSUtil.force_get_elem_by_id("caret");
            restart_cursor_animation(caret_elem);
            scroll_cursor_into_view_if_needed(caret_elem);
          };
          switch (
            model.mini_buffer,
            Js.Opt.to_option(Dom_html.document##.activeElement),
          ) {
          | (Some(_), Some(elt)) when Js.to_string(elt##.id) == "mini-buffer" =>
            ()
          | (None, _) => ()
          | (Some(_), _) => JSUtil.force_get_elem_by_id("mini-buffer")##focus
          };
        },
      model,
      Page.view(~inject, model),
    )
  );
};
