open Tyxml_js;

open React;

module Ev = Dom_html.Event;

let forceGetElementById id => {
  let doc = Dom_html.document;
  Js.Opt.get (doc##getElementById (Js.string id)) (fun () => assert false)
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
  let make: string => int => t;
  let to_string: t => string;
  let keyCode: t => int;
} = {
  type t = (string, int);
  let make str keyCode => (str, keyCode);
  let keyCode (str, keyCode) => keyCode;
  let to_string (str, keyCode) => str;
};

module KeyCombos = {
  let _kc = KeyCombo.make;
  let enter = _kc "Enter" 13;
  let esc = _kc "Esc" 27;
  let backspace = _kc "backspace" 8;
  let del = _kc "del" 46;
  let number_1 = _kc "1" 49;
  let number_2 = _kc "2" 50;
  let number_3 = _kc "3" 51;
  let p = _kc "p" 112;
  let x = _kc "x" 120;
  let greaterThan = _kc ">" 62;
  let n = _kc "n" 110;
  let s = _kc "s" 115;
  let dot = _kc "." 46;
  let colon = _kc ":" 58;
  let v = _kc "v" 118;
  let backslash = _kc "\\" 92;
  let openParens = _kc "(" 40;
  let pound = _kc "#" 35;
  let plus = _kc "+" 43;
  let l = _kc "l" 108;
  let r = _kc "r" 114;
  let c = _kc "c" 99;
  let qmark = _kc "?" 63;
  let equals = _kc "=" 61;
};

let get_keyCode (evt: Js.t Dom_html.keyboardEvent) =>
  Js.Optdef.get evt##.which (fun () => assert false);

let log x => Firebug.console##log x;
