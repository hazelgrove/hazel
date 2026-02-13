open Js_of_ocaml;

/* ============================================================================
 * Context Menu Click-Outside Handler
 * ============================================================================
 * Manages a document-level capture-phase listener that closes context menus
 * when clicking outside. Uses capture phase so the click still reaches its
 * intended target (single-click UX, not two-click).
 *
 * Usage from view code:
 *   ContextMenuListener.sync(model.context_menu, inject(ToggleContextMenu))
 *
 * This should be called on every render. It activates/deactivates the listener
 * based on the current menu state.
 */

module Effect = Bonsai.Effect;

/* State: effect to close the menu, or None if no menu is open */
let close_effect: ref(option(Effect.t(unit))) = ref(None);
let is_active: ref(bool) = ref(false);

/* Timestamp when the menu was opened - used to prevent immediate close */
let opened_at: ref(float) = ref(0.0);

/* Check if an element or its ancestors have a class */
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

/* Helper to execute the close effect if set */
let execute_close = () =>
  switch (close_effect^) {
  | None => ()
  | Some(effect) =>
    /* Prevent closing if the menu just opened (within 50ms) - this handles
       the case where the right-click that opens the menu also triggers
       the document listener */
    let now = Js.Unsafe.global##.performance##now();
    if (now -. opened_at^ > 50.0) {
      Effect.Expert.handle(effect);
      close_effect := None;
      is_active := false;
    };
  };

/* The listeners that run on pointerdown and window blur */
let listener_attached = ref(false);

let setup = (): unit =>
  if (! listener_attached^) {
    listener_attached := true;

    /* Document-level click listener (capture phase for single-click UX) */
    let doc = Js.Unsafe.coerce(Dom_html.document);
    let click_handler =
      Js.wrap_callback((evt: Js.t(Dom_html.mouseEvent)) => {
        /* Only close if click is NOT inside the context menu */
        let target =
          Js.Opt.to_option(evt##.target)
          |> Option.map(t => Js.Unsafe.coerce(t));
        let in_menu =
          switch (target) {
          | Some(elem) => has_ancestor_with_class(elem, "context-menu")
          | None => false
          };
        if (!in_menu) {
          execute_close();
        };
      });
    let options =
      Js.Unsafe.obj([|("capture", Js.Unsafe.inject(Js._true))|]);
    let _ =
      doc##addEventListener(
        Js.string("pointerdown"),
        click_handler,
        options,
      );

    /* Window blur listener - close menu when user switches away */
    let blur_handler =
      Js.wrap_callback((_: Js.t(Dom_html.event)) => execute_close());
    let win = Js.Unsafe.coerce(Dom_html.window);
    let _ =
      win##addEventListener(Js.string("blur"), blur_handler, Js._false);
    ();
  };

/* Sync the listener state with the menu open state.
   Call this on every render with current menu state and close effect. */
let sync = (menu_open: bool, on_close: Effect.t(unit)): unit => {
  setup(); /* Ensure listener is attached */
  if (menu_open && ! is_active^) {
    /* Menu just opened - activate and record timestamp */
    close_effect := Some(on_close);
    is_active := true;
    opened_at := Js.Unsafe.global##.performance##now();
  } else if (!menu_open && is_active^) {
    /* Menu just closed - deactivate */
    close_effect := None;
    is_active := false;
  } else if (menu_open) {
    /* Menu still open - update effect in case inject changed */
    close_effect := Some(on_close);
  };
};
