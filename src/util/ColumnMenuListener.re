open Js_of_ocaml;

/* Document-level listeners for the table column menu. Mirrors
 * ContextMenuListener: capture-phase pointerdown closes the menu on
 * clicks outside `.column-menu`, capture-phase keydown dispatches
 * Escape/ArrowUp/ArrowDown/Enter to the renderer.
 *
 * Done globally (rather than via tabindex+on_keydown on the menu div)
 * because Hazel's editor (#page) aggressively reclaims focus to the
 * clipboard shim, which would otherwise eat the menu's key events. */

module Effect = Bonsai.Effect;

/* Renderer-supplied effect for the current open menu. */
let close_effect: ref(option(Effect.t(unit))) = ref(None);
let on_key: ref(option(string => option(Effect.t(unit)))) = ref(None);
let is_active: ref(bool) = ref(false);

/* Grace period after open so the same click that opened the menu doesn't
 * immediately close it via the document listener. */
let opened_at: ref(float) = ref(0.0);

let has_ancestor_with_class =
    (elem: Js.t(Dom_html.element), class_name: string): bool => {
  let class_js = Js.string(class_name);
  let rec check = (el: Js.t(Dom_html.element)): bool =>
    if (Js.to_bool(el##.classList##contains(class_js))) {
      true;
    } else {
      switch (Js.Opt.to_option(el##.parentNode)) {
      | Some(parent_node) =>
        switch (Dom_html.CoerceTo.element(parent_node) |> Js.Opt.to_option) {
        | Some(parent) => check(parent)
        | None => false
        }
      | None => false
      };
    };
  check(elem);
};

let execute_close = () =>
  switch (close_effect^) {
  | None => ()
  | Some(effect) =>
    let now = Js.Unsafe.global##.performance##now();
    if (now -. opened_at^ > 50.0) {
      Effect.Expert.handle(effect);
    };
  };

let listener_attached = ref(false);

let setup = (): unit =>
  if (! listener_attached^) {
    listener_attached := true;

    let doc = Js.Unsafe.coerce(Dom_html.document);

    /* Click outside closes the menu. Capture phase so a click that lands
     * on a menu item still reaches the item (the menu div uses
     * Effect.Stop_propagation on its on_click to keep this listener
     * from receiving the inner click). */
    let click_handler =
      Js.wrap_callback((evt: Js.t(Dom_html.mouseEvent)) => {
        let target =
          Js.Opt.to_option(evt##.target)
          |> Option.map(t => Js.Unsafe.coerce(t));
        let in_menu =
          switch (target) {
          | Some(elem) => has_ancestor_with_class(elem, "column-menu")
          | None => false
          };
        if (!in_menu) {
          execute_close();
        };
      });
    let capture =
      Js.Unsafe.obj([|("capture", Js.Unsafe.inject(Js._true))|]);
    let _ =
      doc##addEventListener(
        Js.string("pointerdown"),
        click_handler,
        capture,
      );

    /* Capture-phase keydown so it fires before the editor's window-level
     * listener. Returns false on consumed keys so the editor doesn't
     * also process them. */
    let key_handler =
      Js.wrap_callback((evt: Js.t(Dom_html.keyboardEvent)) =>
        if (is_active^) {
          let key =
            evt##.key |> Js.Optdef.to_option |> Option.map(Js.to_string);
          switch (key, on_key^) {
          | (Some(k), Some(handle)) =>
            switch (handle(k)) {
            | Some(effect) =>
              Dom.preventDefault(evt);
              Dom_html.stopPropagation(evt);
              Effect.Expert.handle(effect);
            | None => ()
            }
          | _ => ()
          };
        }
      );
    let _ =
      doc##addEventListener(Js.string("keydown"), key_handler, capture);

    /* If the window loses focus (alt-tab etc.), close the menu. */
    let blur_handler =
      Js.wrap_callback((_: Js.t(Dom_html.event)) => execute_close());
    let win = Js.Unsafe.coerce(Dom_html.window);
    let _ =
      win##addEventListener(Js.string("blur"), blur_handler, Js._false);
    ();
  };

/* Schedule scrolling the selected menu item into view after the next
 * render tick. Handles two cases: (a) the menu was opened near the
 * bottom and overflows the viewport, (b) arrow-key navigation moves the
 * selection to an item that's off-screen. Falls back to scrolling the
 * menu container if no item is selected. */
let scroll_selected_into_view = () => {
  let _ =
    Dom_html.window##setTimeout(
      Js.wrap_callback(() => {
        let selector = ".column-menu .menu-item.selected";
        let target =
          switch (
            Js.Opt.to_option(
              Dom_html.document##querySelector(Js.string(selector)),
            )
          ) {
          | Some(_) as found => found
          | None =>
            Js.Opt.to_option(
              Dom_html.document##querySelector(Js.string(".column-menu")),
            )
          };
        switch (target) {
        | None => ()
        | Some(elem) =>
          let opts =
            Js.Unsafe.obj([|
              ("block", Js.Unsafe.inject(Js.string("nearest"))),
            |]);
          let _ =
            Js.Unsafe.meth_call(
              elem,
              "scrollIntoView",
              [|Js.Unsafe.inject(opts)|],
            );
          ();
        };
      }),
      0.0,
    );
  ();
};

/* Called on every render. Activates/deactivates the listener and refreshes
 * the captured effect/key handler closures. */
let sync =
    (
      ~menu_open: bool,
      ~on_close: Effect.t(unit),
      ~handle_key: string => option(Effect.t(unit)),
    )
    : unit => {
  setup();
  if (menu_open && ! is_active^) {
    close_effect := Some(on_close);
    on_key := Some(handle_key);
    is_active := true;
    opened_at := Js.Unsafe.global##.performance##now();
    scroll_selected_into_view();
  } else if (!menu_open && is_active^) {
    close_effect := None;
    on_key := None;
    is_active := false;
  } else if (menu_open) {
    /* Still open — refresh closures in case inject changed, and keep
     * the selected item visible as the user navigates with arrow keys. */
    close_effect := Some(on_close);
    on_key := Some(handle_key);
    scroll_selected_into_view();
  };
};
