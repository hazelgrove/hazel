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

  /* preserve editor focus across window focus/blur */
  Dom_html.window##.onfocus :=
    Dom_html.handler(_ => {
      UHCode.focus();
      Js._true;
    });
  UHCode.focus();

  Async_kernel.Deferred.return(State.State);
};

let restart_cursor_animation = () =>
  try({
    let caret_elem = JSUtil.force_get_elem_by_id("caret");
    caret_elem##.classList##remove(Js.string("blink"));
    // necessary to trigger reflow
    // <https://css-tricks.com/restart-css-animation/>
    let _ = caret_elem##getBoundingClientRect;
    caret_elem##.classList##add(Js.string("blink"));
  }) {
  | _ => ()
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

let move_cursor_inspector_in_view = ci_elem => {
  let ci_rect = ci_elem##getBoundingClientRect;
  let classList = ci_elem##.classList;
  if (ci_rect##.top < 0.0) {
    classList##remove(Js.string("above"));
    classList##add(Js.string("below"));
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
      ~apply_action=
        (action, state) => {
          restart_cursor_animation();
          Update.apply_action(model, action, state);
        },
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
            scroll_cursor_into_view_if_needed(caret_elem);

            if (model.cursor_inspector.visible) {
              let ci_elem = JSUtil.force_get_elem_by_id("cursor-inspector");
              move_cursor_inspector_in_view(ci_elem);
            };
          };
        },
      model,
      Page.view(~inject, model),
    )
  );
};
