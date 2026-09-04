open Js_of_ocaml;

/* Document-level listener shared by transient menus (context menu, column
 * menu). Each instance is functorized over its menu-class string and a
 * couple of feature flags. Provides:
 *   - capture-phase pointerdown that fires `on_close` when the click lands
 *     outside any ancestor with `Config.menu_class`,
 *   - window blur that fires `on_close`,
 *   - optional capture-phase keydown dispatch to a per-render `handle_key`,
 *   - optional scrollIntoView on sync while the menu is open.
 *
 * Capture phase is used so a click on a menu item still reaches the item
 * (menu items stop propagation), and so keydown beats the editor's
 * window-level handler.
 *
 * Each instantiation owns its own private state cell — multiple menus can
 * be wired up independently without colliding. */

module Effect = Bonsai.Effect;

module type Config = {
  /* CSS class identifying ancestors of the menu (used to detect
   * "click outside"). */
  let menu_class: string;
  /* When true, attach a capture-phase keydown listener that dispatches to
   * the most-recent `handle_key` supplied to `sync`. */
  let supports_keys: bool;
  /* When true, call `scrollIntoView` on `.{menu_class} .menu-item.selected`
   * (falling back to the menu container) after every sync where the menu
   * is open. */
  let scroll_into_view: bool;
};

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

module Make = (C: Config) => {
  let close_effect: ref(option(Effect.t(unit))) = ref(None);
  let on_key: ref(option(string => option(Effect.t(unit)))) = ref(None);
  let is_active: ref(bool) = ref(false);
  /* Grace period after open so the same click that opened the menu
   * doesn't immediately close it via the document listener. */
  let opened_at: ref(float) = ref(0.0);
  let listener_attached: ref(bool) = ref(false);

  let execute_close = () =>
    switch (close_effect^) {
    | None => ()
    | Some(effect) =>
      let now = Js.Unsafe.global##.performance##now();
      if (now -. opened_at^ > 50.0) {
        Effect.Expert.handle(effect);
      };
    };

  let scroll_selected_into_view = () => {
    let _ =
      Dom_html.window##setTimeout(
        Js.wrap_callback(() => {
          let selected_sel = "." ++ C.menu_class ++ " .menu-item.selected";
          let container_sel = "." ++ C.menu_class;
          let target =
            switch (
              Js.Opt.to_option(
                Dom_html.document##querySelector(Js.string(selected_sel)),
              )
            ) {
            | Some(_) as found => found
            | None =>
              Js.Opt.to_option(
                Dom_html.document##querySelector(Js.string(container_sel)),
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

  let setup = (): unit =>
    if (! listener_attached^) {
      listener_attached := true;
      let doc = Js.Unsafe.coerce(Dom_html.document);
      let capture =
        Js.Unsafe.obj([|("capture", Js.Unsafe.inject(Js._true))|]);

      let click_handler =
        Js.wrap_callback((evt: Js.t(Dom_html.mouseEvent)) => {
          let target =
            Js.Opt.to_option(evt##.target)
            |> Option.map(t => Js.Unsafe.coerce(t));
          /* Clicks inside the menu, or on an element explicitly tagged
           * as a menu trigger (the ⋮ button), are handled by the menu
           * itself — skip the close-on-outside path so the trigger can
           * implement open/close as a toggle. */
          let owned_by_menu =
            switch (target) {
            | Some(elem) =>
              has_ancestor_with_class(elem, C.menu_class)
              || has_ancestor_with_class(elem, "menu-trigger")
            | None => false
            };
          if (!owned_by_menu) {
            execute_close();
          };
        });
      let _ =
        doc##addEventListener(
          Js.string("pointerdown"),
          click_handler,
          capture,
        );

      if (C.supports_keys) {
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
        ();
      };

      let blur_handler =
        Js.wrap_callback((_: Js.t(Dom_html.event)) => execute_close());
      let win = Js.Unsafe.coerce(Dom_html.window);
      let _ =
        win##addEventListener(Js.string("blur"), blur_handler, Js._false);
      ();
    };

  let sync =
      (
        ~menu_open: bool,
        ~on_close: Effect.t(unit),
        ~handle_key: option(string => option(Effect.t(unit)))=?,
        (),
      )
      : unit => {
    setup();
    if (menu_open && ! is_active^) {
      close_effect := Some(on_close);
      on_key := handle_key;
      is_active := true;
      opened_at := Js.Unsafe.global##.performance##now();
      if (C.scroll_into_view) {
        scroll_selected_into_view();
      };
    } else if (!menu_open && is_active^) {
      close_effect := None;
      on_key := None;
      is_active := false;
    } else if (menu_open) {
      /* Still open — refresh closures in case inject changed. */
      close_effect := Some(on_close);
      on_key := handle_key;
      if (C.scroll_into_view) {
        scroll_selected_into_view();
      };
    };
  };
};
