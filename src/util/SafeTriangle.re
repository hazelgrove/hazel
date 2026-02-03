/* SafeTriangle: Prevents hover menus from closing when moving toward them.
 *
 * When a user hovers over a trigger element to reveal a dropdown menu,
 * moving the mouse toward the menu can cause it to close if the cursor
 * exits the trigger before reaching the menu. This module solves that
 * by tracking mouse movement and keeping the menu open while the cursor
 * is within a "safe triangle" connecting the trigger to the menu.
 *
 * Usage:
 *   1. Call `activate` when menu should appear (e.g., mouseenter on trigger)
 *   2. Call `deactivate` to close the menu
 *   3. Use `is_in_safe_zone` to check if mouse is in the safe area
 *   4. The `make_handlers` function provides ready-to-use event handlers
 *
 * Configuration:
 *   - Set Config.debug := true to visualize the safe triangle
 *   - Adjust Config.padding to expand the safe zone
 *   - Set Config.default_direction for your typical menu placement
 */

open Js_of_ocaml;

/* ============================================================================
 * Configuration
 * ============================================================================ */

module Config = {
  /* Set to true to render a visible SVG overlay showing the safe triangle */
  let debug: ref(bool) = ref(false);

  /* Padding in pixels to expand the safe triangle beyond the menu edges.
   * Higher values make the safe zone more forgiving for diagonal movements. */
  let padding: ref(float) = ref(20.0);

  /* Extra padding at the apex (exit point) to account for cursor size */
  let apex_padding: ref(float) = ref(5.0);

  /* Default menu direction if not specified */
  let default_direction:
    ref(
      [
        | `Below
        | `Right
        | `Left
        | `Above
      ],
    ) =
    ref(`Below);

  /* Debug overlay color (CSS color string) */
  let debug_color: ref(string) = ref("rgba(255, 0, 0, 0.25)");

  /* Debug overlay border color */
  let debug_border_color: ref(string) = ref("rgba(255, 0, 0, 0.6)");
};

/* ============================================================================
 * Types
 * ============================================================================ */

/* Menu direction relative to trigger */
type direction = [
  | `Below
  | `Right
  | `Left
  | `Above
];

/* 2D point type */
type point = {
  x: float,
  y: float,
};

/* Bounding rectangle */
type rect = {
  top: float,
  left: float,
  bottom: float,
  right: float,
};

/* State for an active hover menu */
type active_state = {
  /* Point where mouse exited the trigger */
  exit_point: point,
  /* Bounding rect of the dropdown menu */
  menu_rect: rect,
  /* Direction menu opens relative to trigger */
  menu_direction: direction,
  /* Computed triangle vertices (cached for performance and debug) */
  triangle: (point, point, point),
};

/* ============================================================================
 * Core Geometry
 * ============================================================================ */

/* Check if a point is inside a triangle using barycentric coordinates.
 * Returns true if point P is inside triangle ABC. */
let point_in_triangle = (p: point, a: point, b: point, c: point): bool => {
  let sign = (p1: point, p2: point, p3: point): float =>
    (p1.x -. p3.x) *. (p2.y -. p3.y) -. (p2.x -. p3.x) *. (p1.y -. p3.y);

  let d1 = sign(p, a, b);
  let d2 = sign(p, b, c);
  let d3 = sign(p, c, a);

  let has_neg = d1 < 0.0 || d2 < 0.0 || d3 < 0.0;
  let has_pos = d1 > 0.0 || d2 > 0.0 || d3 > 0.0;

  !(has_neg && has_pos);
};

/* Compute the safe triangle vertices given exit point, menu rect, and direction.
 * The triangle apex is at/near the exit point, and the base spans the
 * edge of the menu closest to the trigger, expanded by padding. */
let compute_triangle =
    (~exit_point: point, ~menu_rect: rect, ~direction: direction)
    : (point, point, point) => {
  let padding = Config.padding^;
  let apex_pad = Config.apex_padding^;
  let r = menu_rect;

  /* Apex: exit point, optionally padded toward the menu */
  let apex =
    switch (direction) {
    | `Below => {
        x: exit_point.x,
        y: exit_point.y -. apex_pad,
      }
    | `Above => {
        x: exit_point.x,
        y: exit_point.y +. apex_pad,
      }
    | `Right => {
        x: exit_point.x -. apex_pad,
        y: exit_point.y,
      }
    | `Left => {
        x: exit_point.x +. apex_pad,
        y: exit_point.y,
      }
    };

  /* Base corners: menu edge closest to trigger, expanded by padding */
  let (corner1, corner2) =
    switch (direction) {
    | `Below => (
        {
          x: r.left -. padding,
          y: r.top,
        },
        {
          x: r.right +. padding,
          y: r.top,
        },
      )
    | `Above => (
        {
          x: r.left -. padding,
          y: r.bottom,
        },
        {
          x: r.right +. padding,
          y: r.bottom,
        },
      )
    | `Right => (
        {
          x: r.left,
          y: r.top -. padding,
        },
        {
          x: r.left,
          y: r.bottom +. padding,
        },
      )
    | `Left => (
        {
          x: r.right,
          y: r.top -. padding,
        },
        {
          x: r.right,
          y: r.bottom +. padding,
        },
      )
    };

  (apex, corner1, corner2);
};

/* ============================================================================
 * Debug Visualization
 * ============================================================================ */

module Debug = {
  let overlay_id = "safe-triangle-debug-overlay";

  /* Remove any existing debug overlay */
  let remove_overlay = (): unit => {
    switch (
      Dom_html.document##getElementById(Js.string(overlay_id))
      |> Js.Opt.to_option
    ) {
    | Some(elem) => Js.Unsafe.meth_call(elem, "remove", [||]) |> ignore
    | None => ()
    };
  };

  /* Helper to create SVG elements (no proper js_of_ocaml bindings for SVG) */
  let create_svg_element = (tag: string): Js.t(Dom_html.element) =>
    Js.Unsafe.meth_call(
      Dom_html.document,
      "createElementNS",
      [|
        Js.Unsafe.inject(Js.string("http://www.w3.org/2000/svg")),
        Js.Unsafe.inject(Js.string(tag)),
      |],
    );

  /* Create/update SVG debug overlay showing the safe triangle */
  let show_overlay = (triangle: (point, point, point), menu_rect: rect): unit => {
    remove_overlay();

    let (apex, corner1, corner2) = triangle;

    /* Create SVG element */
    let svg = create_svg_element("svg");

    svg##setAttribute(Js.string("id"), Js.string(overlay_id));
    svg##.style##.position := Js.string("fixed");
    svg##.style##.top := Js.string("0");
    svg##.style##.left := Js.string("0");
    svg##.style##.width := Js.string("100vw");
    svg##.style##.height := Js.string("100vh");
    svg##.style##.pointerEvents := Js.string("none");
    svg##.style##.zIndex := Js.string("999999");

    /* Create triangle path */
    let path = create_svg_element("path");

    let d =
      Printf.sprintf(
        "M %.1f %.1f L %.1f %.1f L %.1f %.1f Z",
        apex.x,
        apex.y,
        corner1.x,
        corner1.y,
        corner2.x,
        corner2.y,
      );

    path##setAttribute(Js.string("d"), Js.string(d));
    path##setAttribute(Js.string("fill"), Js.string(Config.debug_color^));
    path##setAttribute(
      Js.string("stroke"),
      Js.string(Config.debug_border_color^),
    );
    path##setAttribute(Js.string("stroke-width"), Js.string("2"));

    /* Create menu rect outline */
    let rect_elem = create_svg_element("rect");

    rect_elem##setAttribute(
      Js.string("x"),
      Js.string(Printf.sprintf("%.1f", menu_rect.left)),
    );
    rect_elem##setAttribute(
      Js.string("y"),
      Js.string(Printf.sprintf("%.1f", menu_rect.top)),
    );
    rect_elem##setAttribute(
      Js.string("width"),
      Js.string(Printf.sprintf("%.1f", menu_rect.right -. menu_rect.left)),
    );
    rect_elem##setAttribute(
      Js.string("height"),
      Js.string(Printf.sprintf("%.1f", menu_rect.bottom -. menu_rect.top)),
    );
    rect_elem##setAttribute(Js.string("fill"), Js.string("none"));
    rect_elem##setAttribute(
      Js.string("stroke"),
      Js.string(Config.debug_border_color^),
    );
    rect_elem##setAttribute(Js.string("stroke-width"), Js.string("1"));
    rect_elem##setAttribute(
      Js.string("stroke-dasharray"),
      Js.string("4,4"),
    );

    /* Create apex marker */
    let circle = create_svg_element("circle");

    circle##setAttribute(
      Js.string("cx"),
      Js.string(Printf.sprintf("%.1f", apex.x)),
    );
    circle##setAttribute(
      Js.string("cy"),
      Js.string(Printf.sprintf("%.1f", apex.y)),
    );
    circle##setAttribute(Js.string("r"), Js.string("5"));
    circle##setAttribute(
      Js.string("fill"),
      Js.string(Config.debug_border_color^),
    );

    Dom.appendChild(svg, path);
    Dom.appendChild(svg, rect_elem);
    Dom.appendChild(svg, circle);
    Dom.appendChild(Dom_html.document##.body, svg);
  };
};

/* ============================================================================
 * State Management
 * ============================================================================ */

/* Global state - only one menu can be active at a time */
let current: ref(option(active_state)) = ref(None);

/* Check if a point is within the safe zone (triangle + menu area) */
let is_in_safe_zone = (mouse: point): bool =>
  switch (current^) {
  | None => false
  | Some(state) =>
    /* Check if inside the menu itself (with padding) */
    let padding = Config.padding^;
    let in_menu =
      mouse.x >= state.menu_rect.left
      -. padding
      && mouse.x <= state.menu_rect.right
      +. padding
      && mouse.y >= state.menu_rect.top
      -. padding
      && mouse.y <= state.menu_rect.bottom
      +. padding;

    if (in_menu) {
      true;
    } else {
      /* Check if inside the safe triangle */
      let (apex, corner1, corner2) = state.triangle;
      point_in_triangle(mouse, apex, corner1, corner2);
    };
  };

/* Activate safe triangle tracking */
let activate =
    (
      ~exit_point: point,
      ~menu_rect: rect,
      ~menu_direction: direction=Config.default_direction^,
      (),
    )
    : unit => {
  let triangle =
    compute_triangle(~exit_point, ~menu_rect, ~direction=menu_direction);

  current :=
    Some({
      exit_point,
      menu_rect,
      menu_direction,
      triangle,
    });

  if (Config.debug^) {
    Debug.show_overlay(triangle, menu_rect);
  };
};

/* Deactivate and clear state */
let deactivate = (): unit => {
  current := None;
  if (Config.debug^) {
    Debug.remove_overlay();
  };
};

/* Check if tracking is currently active */
let is_active = (): bool => Option.is_some(current^);

/* Get current state (useful for debugging) */
let get_state = (): option(active_state) => current^;

/* ============================================================================
 * DOM Helpers
 * ============================================================================ */

/* Extract point from mouse event */
let point_of_mouse_event = (evt: Js.t(Dom_html.mouseEvent)): point => {
  x: float_of_int(evt##.clientX),
  y: float_of_int(evt##.clientY),
};

/* Extract rect from DOM element */
let rect_of_element = (elem: Js.t(Dom_html.element)): rect => {
  let r = elem##getBoundingClientRect;
  {
    top: r##.top,
    left: r##.left,
    bottom: r##.bottom,
    right: r##.right,
  };
};

/* ============================================================================
 * Virtual_dom Integration (Handlers module)
 * ============================================================================ */

module Handlers = {
  open Virtual_dom.Vdom;

  /* State for a specific hover menu instance */
  type instance_state = {
    mutable is_over_trigger: bool,
    mutable is_over_menu: bool,
    mutable check_timeout: option(Dom_html.timeout_id),
    on_show: unit => unit,
    on_hide: unit => unit,
  };

  let make_instance = (~on_show: unit => unit, ~on_hide: unit => unit) => {
    is_over_trigger: false,
    is_over_menu: false,
    check_timeout: None,
    on_show,
    on_hide,
  };

  /* Clear any pending timeout */
  let clear_timeout = (state: instance_state) =>
    switch (state.check_timeout) {
    | Some(id) =>
      Dom_html.window##clearTimeout(id);
      state.check_timeout = None;
    | None => ()
    };

  /* Schedule a check to see if we should hide the menu */
  let schedule_hide_check =
      (
        state: instance_state,
        ~menu_elem: Js.t(Dom_html.element),
        ~direction: direction,
        exit_point: point,
      ) => {
    clear_timeout(state);

    /* Activate safe triangle tracking */
    activate(
      ~exit_point,
      ~menu_rect=rect_of_element(menu_elem),
      ~menu_direction=direction,
      (),
    );

    /* Set up mousemove listener on document to track cursor */
    let doc = Dom_html.document;
    let listener_id: ref(option(Dom.event_listener_id)) = ref(None);

    let cleanup = () => {
      switch (listener_id^) {
      | Some(id) => Dom.removeEventListener(id)
      | None => ()
      };
      listener_id := None;
      deactivate();
    };

    let handler =
      Dom.handler(evt => {
        let mouse = point_of_mouse_event(evt);

        if (!is_in_safe_zone(mouse) && !state.is_over_menu) {
          /* Mouse left safe zone and not over menu - hide */
          cleanup();
          state.on_hide();
        };

        Js._true;
      });

    listener_id :=
      Some(
        Dom.addEventListener(
          doc,
          Dom_html.Event.mousemove,
          handler,
          Js._false,
        ),
      );

    /* Also set a timeout as a fallback cleanup */
    state.check_timeout =
      Some(
        Dom_html.window##setTimeout(
          Js.wrap_callback(() =>
            if (!state.is_over_trigger && !state.is_over_menu) {
              cleanup();
              state.on_hide();
            }
          ),
          2000.0 /* 2 second timeout as fallback */
        ),
      );
  };

  /* Create trigger element attributes */
  let trigger_attrs =
      (
        state: instance_state,
        ~menu_elem_id: string,
        ~direction: direction=Config.default_direction^,
        (),
      )
      : list(Attr.t) => [
    Attr.on_mouseenter(_ => {
      state.is_over_trigger = true;
      clear_timeout(state);
      state.on_show();
      Effect.Ignore;
    }),
    Attr.on_mouseleave(evt => {
      state.is_over_trigger = false;
      let exit_point = point_of_mouse_event(evt);

      /* Try to get menu element and activate safe triangle */
      switch (
        Dom_html.document##getElementById(Js.string(menu_elem_id))
        |> Js.Opt.to_option
      ) {
      | Some(menu_elem) =>
        schedule_hide_check(state, ~menu_elem, ~direction, exit_point)
      | None =>
        /* Menu not found, just hide after small delay */
        state.check_timeout =
          Some(
            Dom_html.window##setTimeout(
              Js.wrap_callback(() =>
                if (!state.is_over_trigger) {
                  state.on_hide();
                }
              ),
              100.0,
            ),
          )
      };

      Effect.Ignore;
    }),
  ];

  /* Create menu element attributes */
  let menu_attrs =
      (state: instance_state, ~menu_elem_id: string): list(Attr.t) => [
    Attr.id(menu_elem_id),
    Attr.on_mouseenter(_ => {
      state.is_over_menu = true;
      clear_timeout(state);
      deactivate(); /* No longer need triangle tracking */
      Effect.Ignore;
    }),
    Attr.on_mouseleave(_ => {
      state.is_over_menu = false;
      if (!state.is_over_trigger) {
        state.on_hide();
      };
      Effect.Ignore;
    }),
  ];
};

/* ============================================================================
 * CSS Class-based Dropdown Integration
 * ============================================================================
 *
 * High-level API for dropdowns that show/hide via CSS class toggling.
 * Manages single-active-dropdown semantics and instance caching.
 *
 * Usage:
 *   - Trigger element: add CSSDropdown.trigger_attrs(menu_id)
 *   - Menu element: add CSSDropdown.menu_attrs(menu_id)
 *   - CSS: style .your-menu.dropdown-active { display: block; }
 */
module CSSDropdown = {
  open Virtual_dom.Vdom;

  let active_class = "dropdown-active";

  /* Currently active dropdown ID (only one dropdown open at a time) */
  let active_id: ref(option(string)) = ref(None);

  /* Cached handler instances by menu ID */
  let instances: Hashtbl.t(string, Handlers.instance_state) =
    Hashtbl.create(50);

  /* Helper to add/remove CSS class on element by ID */
  let set_class = (elem_id: string, add: bool): unit =>
    switch (
      Dom_html.document##getElementById(Js.string(elem_id))
      |> Js.Opt.to_option
    ) {
    | Some(elem) =>
      if (add) {
        elem##.classList##add(Js.string(active_class));
      } else {
        elem##.classList##remove(Js.string(active_class));
      }
    | None => ()
    };

  /* Get or create handler instance for a menu ID */
  let get_instance = (menu_id: string): Handlers.instance_state =>
    switch (Hashtbl.find_opt(instances, menu_id)) {
    | Some(inst) => inst
    | None =>
      let inst =
        Handlers.make_instance(
          ~on_show=
            () => {
              /* Hide previous dropdown if different */
              switch (active_id^) {
              | Some(prev) when prev != menu_id => set_class(prev, false)
              | _ => ()
              };
              set_class(menu_id, true);
              active_id := Some(menu_id);
            },
          ~on_hide=
            () => {
              set_class(menu_id, false);
              if (active_id^ == Some(menu_id)) {
                active_id := None;
              };
            },
        );
      Hashtbl.add(instances, menu_id, inst);
      inst;
    };

  /* Attributes for the trigger element (e.g., the hoverable item) */
  let trigger_attrs = (menu_id: string): list(Attr.t) =>
    Handlers.trigger_attrs(get_instance(menu_id), ~menu_elem_id=menu_id, ());

  /* Attributes for the menu element (the dropdown that appears) */
  let menu_attrs = (menu_id: string): list(Attr.t) =>
    Handlers.menu_attrs(get_instance(menu_id), ~menu_elem_id=menu_id);
};
