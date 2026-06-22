open Js_of_ocaml;

/* Shared hover state for the reach-group connecting line.
 *
 * Both the editor offside chips and the sidebar group sections call `set` on
 * mouse enter/leave. `set` toggles the connector SVG's CSS class IMPERATIVELY
 * (no action dispatch, no re-render) — a redraw under the cursor was recreating
 * the offside and firing spurious mouseleave/enter, an open/close jitter loop.
 * The hovered group is also recorded in `hovered` so ReachConnector can keep
 * the right line shown across any unrelated redraw. */

let hovered: ref(option(int)) = ref(None: option(int));

let connector_id = (g: int): string =>
  "reach-connector-" ++ string_of_int(g);

let toggle = (g: int, on: bool): unit =>
  switch (JsUtil.get_elem_by_id_opt(connector_id(g))) {
  | Some(el) =>
    on
      ? el##.classList##add(Js.string("active"))
      : el##.classList##remove(Js.string("active"))
  | None => ()
  };

/* Show the connector for group `g` (hiding any previously-hovered one). */
let set = (g: option(int)): unit => {
  Option.iter(prev => toggle(prev, false), hovered^);
  Option.iter(group => toggle(group, true), g);
  hovered := g;
};
