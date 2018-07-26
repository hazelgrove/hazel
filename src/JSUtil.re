open Tyxml_js;
open React;
module Ev = Dom_html.Event;
let log = x => Firebug.console##log(x);
let forceGetElementById = id => {
  let doc = Dom_html.document;
  Js.Opt.get(
    doc##getElementById(Js.string(id)),
    () => {
      log(id);
      assert(false);
    },
  );
};

let listen_to = (ev, elem, f) =>
  Dom_html.addEventListener(elem, ev, Dom_html.handler(f), Js._false);
let listen_to_t = (ev, elem, f) =>
  listen_to(
    ev,
    elem,
    evt => {
      f(evt);
      Js._true;
    },
  );
let r_input = (id, placeholder_str) => {
  let (rs, rf) = S.create("");
  let i_elt =
    Html5.(
      input(
        ~a=[
          a_id(id),
          a_class(["form-control"]),
          a_placeholder(placeholder_str),
        ],
        (),
      )
    );

  let i_dom = To_dom.of_input(i_elt);
  let _ = listen_to_t(Ev.input, i_dom, _ => rf(Js.to_string(i_dom##.value)));

  ((rs, rf), i_elt, i_dom);
};
let r_checkbox = (id, label_str, default_val) => {
  let (rs, rf) = S.create(default_val);
  let checkbox_elt_attrs_base =
    Html5.[a_input_type(`Checkbox), a_id(id), a_class(["r-checkbox"])];

  let checkbox_elt_attrs =
    if (default_val) {
      Html5.[a_checked(), ...checkbox_elt_attrs_base];
    } else {
      checkbox_elt_attrs_base;
    };
  let checkbox_elt = Html5.(input(~a=checkbox_elt_attrs, ()));
  let label_elt =
    Html5.(label(~a=[a_label_for(id)], [pcdata(label_str)]));
  let control_elt = Html5.(div([checkbox_elt, label_elt]));
  let checkbox_dom = To_dom.of_input(checkbox_elt);
  let _ =
    listen_to_t(Ev.change, checkbox_dom, _ =>
      rf(Js.to_bool(checkbox_dom##.checked))
    );

  let control_dom = To_dom.of_div(control_elt);
  ((rs, rf), control_elt, control_dom);
};
module KeyCombo: {
  type t;
  let make: (string, string) => t;
  let name: t => string;
  let key: t => string;
} = {
  type t = (string, string);
  let make = (name, key) => (name, key);
  let name = ((name, key)) => name;
  let key = ((name, key)) => key;
};
module KeyCombos = {
  let _kc = KeyCombo.make;
  let enter = _kc("Enter", "Enter");
  let esc = _kc("Esc", "Escape");
  let backspace = _kc("Backspace", "Backspace");
  let del = _kc("Delete", "Delete");
  let space = _kc("Space", " ");
  let p = _kc("p", "p");
  let x = _kc("x", "x");
  let greaterThan = _kc(">", ">");
  let n = _kc("n", "n");
  let s = _kc("s", "s");
  let dot = _kc(".", ".");
  let colon = _kc(":", ":");
  let v = _kc("v", "v");
  let backslash = _kc("\\", "\\");
  let openParens = _kc("(", "(");
  let pound = _kc("#", "#");
  let plus = _kc("+", "+");
  let asterisk = _kc("*", "*");
  let capitalL = _kc("L", "L");
  let capitalR = _kc("R", "R");
  let c = _kc("c", "c");
  let qmark = _kc("?", "?");
  let equals = _kc("=", "=");
  let vbar = _kc("|", "|");
  let q = _kc("q", "q");
  let w = _kc("w", "w");
};
let get_which = (evt: Js.t(Dom_html.keyboardEvent)) =>
  Js.Optdef.get(evt##.which, () => assert(false));
let get_key = (evt: Js.t(Dom_html.keyboardEvent)) =>
  Js.to_string(Js.Optdef.get(evt##.key, () => assert(false)));
let listen_for_key = (k, f) =>
  listen_to_t(
    Ev.keydown,
    Dom_html.document,
    evt => {
      let key = get_key(evt);
      if (key == KeyCombo.key(k)) {
        f(evt);
        ();
      } else {
        ();
      };
    },
  );

type div_element = Js.t(Dom_html.divElement);
type node = Js.t(Dom.node);
let div_contains_node = (parent: div_element, child: node) : bool => {
  let result: Js.t(bool) = (
    Js.Unsafe.meth_call(parent, "contains", [|Js.Unsafe.inject(child)|]):
      Js.t(bool)
  );
  Js.to_bool(result);
};

let remove_cls_from_all = (cls_to_remove, cls_to_remove_from) => {
  let cls_to_remove_j = Js.string(cls_to_remove);
  let elts =
    Dom_html.document##getElementsByClassName(Js.string(cls_to_remove_from));

  let num_elts = elts##.length;
  for (i in 0 to num_elts - 1) {
    let elt = Js.Opt.get(elts##item(i), () => assert(false));
    elt##.classList##remove(cls_to_remove_j);
  };
};
let add_cls_to_all = (cls_to_add, cls_to_add_to) => {
  let cls_to_add_j = Js.string(cls_to_add);
  let elts =
    Dom_html.document##getElementsByClassName(Js.string(cls_to_add_to));

  let num_elts = elts##.length;
  for (i in 0 to num_elts - 1) {
    let elt = Js.Opt.get(elts##item(i), () => assert(false));
    elt##.classList##add(cls_to_add_j);
  };
};
