/* FloatingElement: Escape overflow clipping while preserving logical positioning.

      == THE PROBLEM ==

      CSS `overflow: auto/hidden/scroll` on a container clips ALL descendants to
      the container's bounding box. This is fundamental to how overflow works -
      it's not about z-index or stacking contexts, it's geometric clipping.

      This creates an impossible situation when:
      1. You have a scrollable container (requires overflow: auto)
      2. An element inside needs to visually extend beyond the container bounds
      3. That element must stay logically positioned relative to content inside

      Z-index cannot help here. Even z-index: 999999 won't escape clipping because
      z-index controls paint order within the document flow, not geometric bounds.

      == WHY SIMPLER SOLUTIONS DON'T WORK ==

      - "Just use higher z-index": Doesn't escape overflow clipping
      - "Remove overflow from container": Breaks scrolling
      - "Move element outside container in DOM": Loses logical positioning context;
        also complex in virtual DOM frameworks where the view tree should match
        the logical component tree
      - "Extend container to overlap target area": Works but causes other issues
        (scrollbars in wrong places, click-through problems, layout complexity)

      == THE SOLUTION: position:fixed + viewport coordinate calculation ==

      Elements with `position: fixed` are positioned relative to the viewport,
      completely outside the normal document flow. They are NOT subject to
      ancestor overflow clipping because they're not geometrically contained
      by any ancestor.

      The challenge is maintaining the element's logical position (e.g., "near
      the caret") when its positioning is now viewport-relative. We solve this by:

      1. Rendering the element with position:fixed inside its logical parent
      2. Storing the desired local position as data attributes
      3. After render, querying the anchor element's viewport position
      4. Computing: viewport_pos = anchor_rect + local_offset
      5. Updating the element's fixed position to those viewport coordinates

      == EXAMPLE: HAZEL'S BACKPACK ==

      Hazel's page layout:
        #page (CSS grid)
          #top-bar     (row 1, z-index 31)
          #main        (row 2, overflow: auto - THIS CLIPS CHILDREN)
            code-container
              caret
              backpack  <- needs to float above top-bar when caret is near top

      The backpack shows incomplete syntax (like unmatched parens) and floats
      above the caret. When the caret is on a line near the top of the visible
      area, the backpack needs to extend into/above the top bar region.

      Previous attempts:
      - Making #main span rows 1-2 with padding: caused scrollbar issues
      - Higher z-index on backpack: didn't escape overflow clipping

      With FloatingElement:
      - Backpack renders with position:fixed, visibility:hidden
      - Stores local position relative to code-container
      - After render, update_all() computes viewport position and makes visible
      - Scroll listener keeps position updated during scroll

      This works regardless of which editor mode is active or how many editors
      exist, because each backpack finds its own code-container ancestor.

      == USAGE ==

      1. Add to your element:
         - class "floating-fixed"
         - data-float-anchor-class: class name of ancestor to position relative to
         - data-float-local-top: local Y offset in pixels
         - data-float-local-left: local X offset in pixels
         - data-float-flip (optional): flip up/left when out of room
         - style "position: fixed; visibility: hidden; top: 0; left: 0;"

      2. Call setup_scroll_listener() once at app startup

      3. Call update_all() after each render (e.g., in Bonsai's after_display)

      The element will be positioned at:
        viewport_top = anchor.getBoundingClientRect().top + local_top
        viewport_left = anchor.getBoundingClientRect().left + local_left
   */

open Js_of_ocaml;

/* Get a data attribute as a float */
let get_data_float = (el: Js.t(Dom_html.element), name: string): float => {
  let value =
    el##getAttribute(Js.string("data-float-" ++ name)) |> Js.Opt.to_option;
  switch (value) {
  | Some(v) => float_of_string(Js.to_string(v))
  | None => 0.0
  };
};

/* Get a data attribute as a string */
let get_data_string =
    (el: Js.t(Dom_html.element), name: string): option(string) => {
  el##getAttribute(Js.string("data-float-" ++ name))
  |> Js.Opt.to_option
  |> Option.map(Js.to_string);
};

/* Update position of a single floating element */
let update_one = (el: Js.t(Dom_html.element)): unit => {
  switch (get_data_string(el, "anchor-class")) {
  | None => ()
  | Some(anchor_class) =>
    switch (JsUtil.find_ancestor_with_class(el, anchor_class)) {
    | None => ()
    | Some(anchor) =>
      let rect = anchor##getBoundingClientRect;
      let local_top = get_data_float(el, "local-top");
      let local_left = get_data_float(el, "local-left");
      /* `anchor-edge: bottom`: open below a variable-height anchor (e.g. a
       * multi-line drawer sample) without knowing its height at build time. */
      let base_top =
        switch (get_data_string(el, "anchor-edge")) {
        | Some("bottom") => rect##.bottom
        | _ => rect##.top
        };
      let raw_top = base_top +. local_top;
      /* Raised completion flags stay in the viewport while their pole
         still reaches a visible caret. Other floating elements opt out. */
      let min_top =
        get_data_string(el, "min-top") |> Option.map(float_of_string);
      let viewport_top =
        switch (min_top) {
        | Some(y) => max(y, raw_top)
        | None => raw_top
        };
      let shift = viewport_top -. raw_top;
      let _ =
        el##.style##setProperty(
          Js.string("--float-top-shift"),
          Js.string(Printf.sprintf("%fpx", shift)),
          Js.undefined,
        );
      let visible =
        switch (min_top, get_data_string(el, "local-bottom")) {
        | (Some(y), Some(bottom)) =>
          base_top +. float_of_string(bottom) >= y
        | _ => true
        };
      let viewport_left = rect##.left +. local_left;
      /* `flip`: a menu that opens down-right of its anchor flips above it
       * and/or right-aligns to it when its measured size doesn't fit within
       * #main (Menu.fit_direction); the open-* class names the side taken. */
      let (viewport_top, viewport_left) =
        switch (get_data_string(el, "flip")) {
        | None => (viewport_top, viewport_left)
        | Some(_) =>
          let height = float_of_int(el##.offsetHeight) +. local_top;
          let width = float_of_int(el##.offsetWidth) +. local_left;
          let dir =
            Menu.fit_direction(
              ~menu_height=height,
              ~menu_width=width,
              Menu.space_from(
                ~anchor_top=rect##.top,
                ~anchor_bot=base_top,
                ~anchor_left=rect##.left,
                ~anchor_right=rect##.right,
              ),
            );
          Menu.set_open_class(el, Some(dir));
          (
            switch (dir.vertical) {
            | `Down => viewport_top
            | `Up => rect##.top -. height
            },
            switch (dir.horizontal) {
            | `Right => viewport_left
            | `Left => rect##.right -. width
            },
          );
        };
      el##.style##.top := Js.string(Printf.sprintf("%fpx", viewport_top));
      el##.style##.left := Js.string(Printf.sprintf("%fpx", viewport_left));
      el##.style##.visibility := Js.string(visible ? "visible" : "hidden");
    }
  };
};

/* Update all floating elements */
let update_all = (): unit => {
  let elements =
    Dom_html.document##querySelectorAll(Js.string(".floating-fixed"));
  let len = elements##.length;
  for (i in 0 to len - 1) {
    switch (elements##item(i) |> Js.Opt.to_option) {
    | Some(el) => update_one(Js.Unsafe.coerce(el))
    | None => ()
    };
  };
};

/* Setup scroll listener on #main to update floating elements.
   This ensures floating elements stay positioned correctly during scroll. */
let scroll_listener_attached = ref(false);

let setup_scroll_listener = (): unit =>
  if (! scroll_listener_attached^) {
    switch (JsUtil.get_elem_by_id_opt("main")) {
    | None => ()
    | Some(main) =>
      scroll_listener_attached := true;
      let main_coerced = Js.Unsafe.coerce(main);
      let handler =
        Js.wrap_callback(_ => {
          update_all();
          ();
        });
      let _ = main_coerced##addEventListener("scroll", handler);
      ();
    };
  };
