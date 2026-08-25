let get_elem_by_id: string => Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.element);
let get_elem_by_id_opt:
  string => option(Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.element));
let get_elem_by_selector:
  string => Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.element);
let get_child_with_class:
  (Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.element), string) =>
  option(Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.element));
let timestamp: unit => float;
let precise_timestamp: unit => 'res;
let print_timestamp: float => string;
let download_string_file:
  (~filename: string, ~content_type: string, ~contents: string) => unit;
let download_json: (string, Yojson.Safe.t) => unit;
let read_file:
  (Js_of_ocaml.Js.t(#Js_of_ocaml.File.blob), option(string) => unit) => unit;
let reset_file_input: string => unit;
let confirm: string => bool;
let focus_clipboard_shim: unit => unit;
let active_cell_id: string;
let focus_active_cell: unit => bool;
let clipboard_shim: Virtual_dom__Node.t;
let copy_via_shim: string => unit;
let show_copy_toast: unit => unit;
let write_clipboard: string => Ui_effect.t(unit);
let read_clipboard: unit => Ui_effect.t(string);
let find_ancestor_with_class:
  (Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.element), string) =>
  option(Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.element));
let adjust_scroll:
  (Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.element), float) => unit;
let scroll_cursor_into_view_if_needed: unit => unit;
module Fragment: {
  let get_current: unit => option(string);
};
let setPointerCapture:
  (Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.element), int) => unit;
let releasePointerCapture:
  (Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.element), int) => 'a;
let hasPointerCapture:
  (Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.element), int) => 'a;
let set_css_custom_property: (string, string) => unit;
let delay: (float, unit => unit) => unit;
let setup_focus_bar_scroll_compensation: unit => unit;
let prompt: (string, string) => option(string);
let font_metrics_from_specimen: unit => (float, float);
let on_dpr_change: (unit => unit) => unit;
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
