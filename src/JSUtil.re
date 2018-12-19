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
module ModKeyReqs: {
  type t;
  type heldReq =
    | MustBeHeld
    | MustNotBeHeld
    | DoesNotMatter;

  let make: (heldReq, heldReq, heldReq) => t;
  let none: t;
  let withShift: heldReq => t;
  let withCtrl: heldReq => t;
  let withAlt: heldReq => t;

  let matches: (t, (bool, bool, bool)) => bool;
} = {
  type heldReq =
    | MustBeHeld
    | MustNotBeHeld
    | DoesNotMatter;
  type t = (heldReq, heldReq, heldReq);

  let make = (shift, ctrl, alt) => (shift, ctrl, alt);
  let none = make(DoesNotMatter, DoesNotMatter, DoesNotMatter);
  let withShift = req => make(req, DoesNotMatter, DoesNotMatter);
  let withCtrl = req => make(DoesNotMatter, req, DoesNotMatter);
  let withAlt = req => make(DoesNotMatter, DoesNotMatter, req);

  let matches = (modKeyReqs, (isShiftHeld, isCtrlHeld, isAltHeld)) => {
    let (shiftReq, ctrlReq, altReq) = modKeyReqs;

    let _matches = (heldReq, isHeld) =>
      switch (heldReq) {
      | DoesNotMatter => true
      | MustBeHeld => isHeld
      | MustNotBeHeld => !isHeld
      };

    _matches(shiftReq, isShiftHeld)
    && _matches(ctrlReq, isCtrlHeld)
    && _matches(altReq, isAltHeld);
  };
};
module KeyCombo: {
  type t;
  let make: (string, string, ModKeyReqs.t) => t;
  let name: t => string;
  let key: t => string;
  let modKeyReqs: t => ModKeyReqs.t;
} = {
  type t = (string, string, ModKeyReqs.t);
  let make = (name, key, modKeyReqs) => (name, key, modKeyReqs);
  let name = ((name, _, _)) => name;
  let key = ((_, key, _)) => key;
  let modKeyReqs = ((_, _, modKeyReqs)) => modKeyReqs;
};
module KeyCombos = {
  let _kc = (name, key) => KeyCombo.make(name, key, ModKeyReqs.none);
  let _kcm = (name, key, modKeyReqs) => KeyCombo.make(name, key, modKeyReqs);
  let enter = _kc("Enter", "Enter");
  let esc = _kc("Esc", "Escape");
  let backspace = _kc("Backspace", "Backspace");
  let del = _kc("Delete", "Delete");
  let tab = _kcm("Tab", "Tab", ModKeyReqs.withShift(MustNotBeHeld));
  let backtab = _kcm("Shift + Tab", "Tab", ModKeyReqs.withShift(MustBeHeld));
  let space = _kc("Space", " ");
  let p = _kc("p", "p");
  let x = _kc("x", "x");
  let greaterThan = _kc(">", ">");
  let n = _kc("n", "n");
  let s = _kc("s", "s");
  let dot = _kc(".", ".");
  let colon = _kc(":", ":");
  let alt_V = _kcm("Alt + V", "v", ModKeyReqs.withAlt(MustBeHeld));
  let backslash = _kc("\\", "\\");
  let openParens = _kc("(", "(");
  let pound = _kc("#", "#");
  let plus = _kc("+", "+");
  let asterisk = _kc("*", "*");
  let alt_L = _kcm("Alt + L", "l", ModKeyReqs.withAlt(MustBeHeld));
  let alt_R = _kcm("Alt + R", "r", ModKeyReqs.withAlt(MustBeHeld));
  let alt_C = _kcm("Alt + C", "c", ModKeyReqs.withAlt(MustBeHeld));
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
      let matchesKey = get_key(evt) == KeyCombo.key(k);
      let matchesModKeys =
        ModKeyReqs.matches(
          KeyCombo.modKeyReqs(k),
          (
            Js.to_bool(evt##.shiftKey),
            Js.to_bool(evt##.ctrlKey),
            Js.to_bool(evt##.altKey),
          ),
        );
      if (matchesKey && matchesModKeys) {
        f(evt);
        ();
      } else {
        ();
      };
    },
  );

type div_element = Js.t(Dom_html.divElement);
type node = Js.t(Dom.node);
let div_contains_node = (parent: div_element, child: node): bool => {
  let result: Js.t(bool) = (
    Js.Unsafe.meth_call(parent, "contains", [|Js.Unsafe.inject(child)|]):
      Js.t(bool)
  );
  Js.to_bool(result);
};

let is_connected = (node: node) => {
  let result: Js.t(bool) = Js.Unsafe.get(node, "isConnected");
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
