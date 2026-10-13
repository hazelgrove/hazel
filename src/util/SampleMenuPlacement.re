open Js_of_ocaml;

/* Viewport-aware placement for the probe sample menu
 * (ProbeProj.sample_context_menu). Inline, the menu hangs from its sample's
 * bottom-left (`.live-offside .sample-context-menu`, position:absolute).
 * After each render, measure each shown menu and flip it above its sample,
 * and/or right-align it to its sample, when it doesn't fit below or to the
 * right within #main (Menu.space_from, Menu.fit_direction), naming the side
 * with the editor menu's open-* classes. Drawer-mode menus are
 * position:fixed and flip in FloatingElement (data-float-flip). */

/* The menu's inset from its sample's left edge (`left: 3px`). */
let inset = 3.0;

let place = (menu: Js.t(Dom_html.element)): unit =>
  switch (JsUtil.find_ancestor_with_class(menu, "sample")) {
  | None => ()
  | Some(sample) =>
    let position =
      Js.to_string(Dom_html.window##getComputedStyle(menu)##.position);
    /* In flow (position:relative while a rich-probe modal is open), the
     * menu stays where the flow puts it. */
    if (position == "absolute") {
      let rect = sample##getBoundingClientRect;
      let dir =
        Menu.fit_direction(
          ~menu_height=float_of_int(menu##.offsetHeight),
          ~menu_width=float_of_int(menu##.offsetWidth) +. inset,
          Menu.space_from(
            ~anchor_top=rect##.top,
            ~anchor_bot=rect##.bottom,
            ~anchor_left=rect##.left,
            ~anchor_right=rect##.right,
          ),
        );
      Menu.set_open_class(menu, Some(dir));
    } else {
      Menu.set_open_class(menu, None);
    };
  };

let update_all = (): unit => {
  let menus =
    Dom_html.document##querySelectorAll(
      Js.string(".sample-context-menu:not(.floating-fixed)"),
    );
  for (i in 0 to menus##.length - 1) {
    switch (menus##item(i) |> Js.Opt.to_option) {
    | Some(menu) =>
      let menu: Js.t(Dom_html.element) = Js.Unsafe.coerce(menu);
      /* Shown: open (dropdown-active) or the sticky ('/') focal sample's */
      if (menu##getClientRects##.length > 0) {
        place(menu);
      };
    | None => ()
    };
  };
};
