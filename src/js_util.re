open Tyxml_js;

open React;

module Ev = Dom_html.Event;

let log x => Firebug.console##log x;

let forceGetElementById id => {
  let doc = Dom_html.document;
  Js.Opt.get
    (doc##getElementById (Js.string id))
    (
      fun () => {
        log id;
        assert false
      }
    )
};

let listen_to ev elem f => Dom_html.addEventListener elem ev (Dom_html.handler f) Js._false;

let listen_to_t ev elem f =>
  listen_to
    ev
    elem
    (
      fun evt => {
        f evt;
        Js._true
      }
    );

/* create an input and a reactive signal tracking its
 * string value */
let r_input id placeholder_str => {
  let (rs, rf) = S.create "";
  let i_elt =
    Html5.(input a::[a_id id, a_class ["form-control"], a_placeholder placeholder_str] ());
  let i_dom = To_dom.of_input i_elt;
  let _ = listen_to_t Ev.input i_dom (fun _ => rf (Js.to_string i_dom##.value));
  ((rs, rf), i_elt, i_dom)
};

let r_checkbox id label_str default_val => {
  let (rs, rf) = S.create default_val;
  let checkbox_elt_attrs_base = Html5.[a_input_type `Checkbox, a_id id, a_class ["r-checkbox"]];
  let checkbox_elt_attrs =
    if default_val {
      Html5.[a_checked (), ...checkbox_elt_attrs_base]
    } else {
      checkbox_elt_attrs_base
    };
  let checkbox_elt = Html5.(input a::checkbox_elt_attrs ());
  let label_elt = Html5.(label a::[a_label_for id] [pcdata label_str]);
  let control_elt = Html5.(div [checkbox_elt, label_elt]);
  let checkbox_dom = To_dom.of_input checkbox_elt;
  let _ = listen_to_t Ev.change checkbox_dom (fun _ => rf (Js.to_bool checkbox_dom##.checked));
  let control_dom = To_dom.of_div control_elt;
  ((rs, rf), control_elt, control_dom)
};

module KeyCombo: {
  type t;
  let make: string => string => t;
  let name: t => string;
  /* value returned by browser evt.key property, see https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key */
  let key: t => string;
} = {
  type t = (string, string);
  let make name key => (name, key);
  let name (name, key) => name;
  let key (name, key) => key;
};

module KeyCombos = {
  let _kc = KeyCombo.make;
  let enter = _kc "Enter" "Enter";
  let esc = _kc "Esc" "Escape";
  let backspace = _kc "Backspace" "Backspace"; /* on (some?) macs, this is labeled Delete on the kb */
  let del = _kc "Delete" "Delete";
  let space = _kc "Space" " ";
  let p = _kc "p" "p";
  let x = _kc "x" "x";
  let greaterThan = _kc ">" ">";
  let n = _kc "n" "n";
  let s = _kc "s" "s";
  let dot = _kc "." ".";
  let colon = _kc ":" ":";
  let v = _kc "v" "v";
  let backslash = _kc "\\" "\\";
  let openParens = _kc "(" "(";
  let pound = _kc "#" "#";
  let plus = _kc "+" "+";
  let asterisk = _kc "*" "*";
  let l = _kc "l" "l";
  let r = _kc "r" "r";
  let c = _kc "c" "c";
  let qmark = _kc "?" "?";
  let equals = _kc "=" "=";
  let vbar = _kc "|" "|";
};

let get_which (evt: Js.t Dom_html.keyboardEvent) =>
  Js.Optdef.get evt##.which (fun () => assert false);

let get_key (evt: Js.t Dom_html.keyboardEvent) =>
  Js.to_string (Js.Optdef.get evt##.key (fun () => assert false));
