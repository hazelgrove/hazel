/* Wrappers over browser APIs: element lookup, focus, scrolling, the
   clipboard, file download and upload, local storage, the URL, and timing.
   Everything here is effectful. Most of it needs a DOM; the timestamps also
   work in the worker. */

/** Raises if no element has that id; see `get_elem_by_id_opt`. */
let get_elem_by_id: string => Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.element);
let get_elem_by_id_opt:
  string => option(Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.element));
let get_elem_by_selector:
  string => Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.element);
let get_child_with_class:
  (Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.element), string) =>
  option(Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.element));
/** Wall-clock time, in milliseconds since the Unix epoch. */
let timestamp: unit => float;
/** `performance.now()`: milliseconds since page load, sub-millisecond and
    monotonic. For measuring durations, not for dates. */
let precise_timestamp: unit => float;
/** Formats a `timestamp` in the browser's locale. */
let print_timestamp: float => string;
let download_string_file:
  (~filename: string, ~content_type: string, ~contents: string) => unit;
let download_json: (string, Yojson.Safe.t) => unit;
let read_file:
  (Js_of_ocaml.Js.t(#Js_of_ocaml.File.blob), option(string) => unit) => unit;
let reset_file_input: string => unit;
let confirm: string => bool;
let focus_page: unit => unit;
let install_text_selection_guards: unit => unit;
let active_cell_id: string;
/** Whether an active cell was found to focus. */
let focus_active_cell: unit => bool;
let focus_active_editor: unit => unit;
let copy_text: string => unit;
let show_copy_toast: unit => unit;
let write_clipboard: string => Ui_effect.t(unit);
let read_clipboard: unit => Ui_effect.t(string);
let find_ancestor_with_class:
  (Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.element), string) =>
  option(Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.element));
let adjust_scroll:
  (Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.element), float) => unit;
let caret_in_view: unit => bool;
let scroll_cursor_into_view_if_needed: unit => unit;
let main_scroll_top: unit => float;
let set_main_scroll_top: float => unit;
module Fragment: {
  let get_current: unit => option(string);
};
let setPointerCapture:
  (Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.element), int) => unit;
let releasePointerCapture:
  (Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.element), int) => unit;
let hasPointerCapture:
  (Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.element), int) => bool;
let set_css_custom_property: (string, string) => unit;
let update_main_scroll_width: unit => unit;
let sync_at_bottom_class:
  Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.event) => unit;
/** `delay(ms, f)` runs `f` after `ms` milliseconds. */
let delay: (float, unit => unit) => unit;
let setup_focus_bar_scroll_compensation: unit => unit;
let set_local_storage: (string, string) => unit;
let get_local_storage: string => option(string);
let set_css_variable: (string, string) => unit;
let prompt: (string, string) => option(string);
let font_metrics_from_specimen: unit => (float, float);
let code_viewport_geometry: unit => option((float, float));
let on_dpr_change: (unit => unit) => unit;
/** Whether the primary pointer can hover: false on a phone or tablet. */
let can_hover: unit => bool;
/** Whether the primary pointer is a finger: true on a phone or tablet. */
let coarse_pointer: unit => bool;
/** Track the visual viewport, which an on-screen keyboard shrinks. */
let follow_visual_viewport: unit => unit;
module QueryParams: {
  let get_param: string => option(string);
  let set_param: (string, string) => unit;
};
let navigate_probes:
  (
    ~skip_unaligned: bool=?,
    string,
    [
      | `Down
      | `Up
    ]
  ) =>
  option(Id.t);
