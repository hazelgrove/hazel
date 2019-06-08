module U = GeneralUtil;
open Tyxml_js;
open React;
module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Ev = Dom_html.Event;
module PP = Pretty.PP;

let log = x => Js_of_ocaml.Firebug.console##log(x);

let log_sexp = (sexp: Sexplib.Sexp.t) => log(U.string_of_sexp(sexp));

let get_child_nodes = (root: Js.t(Dom.node)): list(Js.t(Dom.node)) =>
  Dom.list_of_nodeList(root##.childNodes);

let rec get_descendant_nodes = (root: Js.t(Dom.node)): list(Js.t(Dom.node)) => {
  let children = root##.childNodes;
  let descendants = ref([]);
  for (i in children##.length - 1 downto 0) {
    let child = Js.Opt.get(children##item(i), () => assert(false));
    descendants := [[child], get_descendant_nodes(child), ...descendants^];
  };
  List.flatten(descendants^);
};

let get_child_node_satisfying =
    (predicate: Js.t(Dom.node) => bool, root: Js.t(Dom.node))
    : option(Js.t(Dom.node)) =>
  List.fold_left(
    (found, child) =>
      switch (found) {
      | Some(_) => found
      | None => predicate(child) ? Some(child) : None
      },
    None,
    get_child_nodes(root),
  );

let get_descendant_node_satisfying =
    (predicate: Js.t(Dom.node) => bool, root: Js.t(Dom.node))
    : option(Js.t(Dom.node)) =>
  List.fold_left(
    (found, descendant) =>
      switch (found) {
      | Some(_) => found
      | None => predicate(descendant) ? Some(descendant) : None
      },
    None,
    get_descendant_nodes(root),
  );

let first_leaf = (node: Js.t(Dom.node)): Js.t(Dom.node) => {
  let cur = ref(node);
  let found = ref(false);
  while (! found^) {
    let cur_node = cur^;
    switch (Js.Opt.to_option(cur_node##.firstChild)) {
    | Some(firstChild) => cur := firstChild
    | None => found := true
    };
  };
  cur^;
};

let last_leaf = (node: Js.t(Dom.node)): Js.t(Dom.node) => {
  let cur = ref(node);
  let found = ref(false);
  while (! found^) {
    let cur_node = cur^;
    switch (Js.Opt.to_option(cur_node##.lastChild)) {
    | Some(lastChild) => cur := lastChild
    | None => found := true
    };
  };
  cur^;
};

let elem_has_cls = (elem: Js.t(Dom_html.element), cls: PP.cls): bool => {
  let found = ref(false);
  let classList = elem##.classList;
  for (j in 0 to classList##.length - 1) {
    let cls_j =
      Js.(to_string(Optdef.get(classList##item(j), () => assert(false))));
    cls_j == cls ? found := true : ();
  };
  found^;
};

let node_has_cls = (node: Js.t(Dom.node), cls: PP.cls): bool =>
  switch (Js.Opt.to_option(Dom_html.CoerceTo.element(node))) {
  | None => false
  | Some(elem) => elem_has_cls(elem, cls)
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
  let label_elt = Html5.(label(~a=[a_label_for(id)], [txt(label_str)]));
  let control_elt = Html5.(div([checkbox_elt, label_elt]));
  let checkbox_dom = To_dom.of_input(checkbox_elt);
  let _ =
    listen_to_t(Ev.change, checkbox_dom, _ =>
      rf(Js.to_bool(checkbox_dom##.checked))
    );

  let control_dom = To_dom.of_div(control_elt);
  ((rs, rf), control_elt, control_dom);
};

let get_key = (evt: Js.t(Dom_html.keyboardEvent)) =>
  Js.to_string(Js.Optdef.get(evt##.key, () => assert(false)));
let get_code = (evt: Js.t(Dom_html.keyboardEvent)) =>
  Js.to_string(Js.Optdef.get(evt##.code, () => assert(false)));

module ModKey = {
  type t =
    | Ctrl
    | Shift
    | Alt
    | Meta;

  let matches = (mk, evt: Js.t(Dom_html.keyboardEvent)) =>
    switch (mk) {
    | Ctrl => Js.to_bool(evt##.ctrlKey)
    | Shift => Js.to_bool(evt##.shiftKey)
    | Alt => Js.to_bool(evt##.altKey)
    | Meta => Js.to_bool(evt##.metaKey)
    };
};

module ModKeys = {
  type req =
    | Held
    | NotHeld
    | Any;

  let is_held =
    fun
    | Held => true
    | NotHeld
    | Any => false;

  type t = {
    c: req,
    s: req,
    a: req,
    m: req,
  };

  let not_held = {c: NotHeld, s: NotHeld, a: NotHeld, m: NotHeld};
  let ctrl = {c: Held, s: Any, a: NotHeld, m: NotHeld};
  let shift = {c: NotHeld, s: Held, a: NotHeld, m: NotHeld};
  let alt = {c: NotHeld, s: Any, a: Held, m: NotHeld};
  let no_ctrl_alt_meta = {c: NotHeld, s: Any, a: NotHeld, m: NotHeld};

  let req_matches = (req, mk, evt) =>
    switch (req) {
    | Any => true
    | Held => ModKey.matches(mk, evt)
    | NotHeld => !ModKey.matches(mk, evt)
    };

  let matches = (mks, evt: Js.t(Dom_html.keyboardEvent)) =>
    req_matches(mks.c, ModKey.Ctrl, evt)
    && req_matches(mks.s, ModKey.Shift, evt)
    && req_matches(mks.a, ModKey.Alt, evt)
    && req_matches(mks.m, ModKey.Meta, evt);

  let mod_prefix = mk => {
    let ctrl_text = is_held(mk.c) ? "Ctrl + " : "";
    let shift_text = is_held(mk.s) ? "Shift + " : "";
    let alt_text = is_held(mk.a) ? "Alt + " : "";
    let meta_text = is_held(mk.m) ? "Meta + " : "";
    meta_text ++ ctrl_text ++ alt_text ++ shift_text;
  };
};

module Key = {
  type recognition_method =
    | Code(string) /* corresponding to KeyboardEvent.code: https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/code */
    | Key(string) /* corresponding to KeyboardEvent.key: https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key */;

  let code_of_letter = letter => "Key" ++ String.uppercase_ascii(letter);

  type t = {
    plain_name: string,
    recognition_methods: list(recognition_method),
  };

  let code1 = (plain_name, code) => {
    plain_name,
    recognition_methods: [Code(code)],
  };
  let code2 = (plain_name, code1, code2) => {
    plain_name,
    recognition_methods: [Code(code1), Code(code2)],
  };

  let key1 = (plain_name, key) => {
    plain_name,
    recognition_methods: [Key(key)],
  };

  let the_letter_code = letter => code1(letter, code_of_letter(letter));
  let the_code = code => code1(code, code);
  let the_key = key => key1(key, key);

  let recognize = (evt: Js.t(Dom_html.keyboardEvent), r) =>
    switch (r) {
    | Code(c) =>
      let code = get_code(evt);
      String.equal(code, c);
    | Key(k) =>
      let key = get_key(evt);
      String.equal(key, k);
    };

  let matches = (k, evt: Js.t(Dom_html.keyboardEvent)) => {
    let recognition_methods = k.recognition_methods;
    U.any(recognition_methods, recognize(evt));
  };
};

module KeyCombo = {
  type t = {
    mod_keys: ModKeys.t,
    key: Key.t,
  };

  let make = (mod_keys, key) => {mod_keys, key};
  let plain = key => {mod_keys: ModKeys.not_held, key};
  let no_ctrl_alt_meta = key => {mod_keys: ModKeys.no_ctrl_alt_meta, key};
  let shift = key => {mod_keys: ModKeys.shift, key};
  let ctrl = key => {mod_keys: ModKeys.ctrl, key};
  let alt = key => {mod_keys: ModKeys.alt, key};

  let matches = (kc, evt: Js.t(Dom_html.keyboardEvent)) =>
    ModKeys.matches(kc.mod_keys, evt) && Key.matches(kc.key, evt);

  let name = kc => {
    let mod_prefix = ModKeys.mod_prefix(kc.mod_keys);
    mod_prefix ++ kc.key.plain_name;
  };
};

module KeyCombos = {
  let enter = KeyCombo.plain(Key.code2("Enter", "Enter", "NumpadEnter"));
  let escape = KeyCombo.plain(Key.the_code("Escape"));
  let backspace = KeyCombo.plain(Key.the_code("Backspace"));
  let delete = KeyCombo.plain(Key.the_code("Delete"));
  let tab = KeyCombo.plain(Key.the_code("Tab"));
  let shift_tab = KeyCombo.shift(Key.the_code("Tab"));
  let space = KeyCombo.plain(Key.the_code("Space"));
  let lt = KeyCombo.no_ctrl_alt_meta(Key.the_key("<"));
  let gt = KeyCombo.no_ctrl_alt_meta(Key.the_key(">"));
  let colon = KeyCombo.no_ctrl_alt_meta(Key.the_key(":"));
  let backslash = KeyCombo.no_ctrl_alt_meta(Key.the_key("\\"));
  let left_parens = KeyCombo.no_ctrl_alt_meta(Key.the_key("("));
  let right_parens = KeyCombo.no_ctrl_alt_meta(Key.the_key(")"));
  let left_bracket = KeyCombo.no_ctrl_alt_meta(Key.the_key("["));
  let right_bracket = KeyCombo.no_ctrl_alt_meta(Key.the_key("]"));
  let qmark = KeyCombo.no_ctrl_alt_meta(Key.the_key("?"));
  let equals = KeyCombo.no_ctrl_alt_meta(Key.the_key("="));
  let pound = KeyCombo.no_ctrl_alt_meta(Key.the_key("#"));
  let plus = KeyCombo.no_ctrl_alt_meta(Key.the_key("+"));
  let asterisk = KeyCombo.no_ctrl_alt_meta(Key.the_key("*"));
  let semicolon = KeyCombo.no_ctrl_alt_meta(Key.the_key(";"));
  let comma = KeyCombo.no_ctrl_alt_meta(Key.the_key(","));
  let vbar = KeyCombo.no_ctrl_alt_meta(Key.the_key("|"));
  let dollar = KeyCombo.no_ctrl_alt_meta(Key.the_key("$"));
  let amp = KeyCombo.no_ctrl_alt_meta(Key.the_key("&"));
  let alt_L = KeyCombo.alt(Key.the_letter_code("l"));
  let alt_R = KeyCombo.alt(Key.the_letter_code("r"));
  let alt_C = KeyCombo.alt(Key.the_letter_code("c"));
  let alt_PageUp = KeyCombo.alt(Key.the_key("PageUp"));
  let alt_PageDown = KeyCombo.alt(Key.the_key("PageDown"));
  let alt_T = KeyCombo.alt(Key.the_letter_code("T"));
  let alt_F = KeyCombo.alt(Key.the_letter_code("F"));
  let key_B = KeyCombo.no_ctrl_alt_meta(Key.the_key("B"));
  let key_N = KeyCombo.no_ctrl_alt_meta(Key.the_key("N"));
  let key_L = KeyCombo.no_ctrl_alt_meta(Key.the_key("L"));
};

let listen_for_key = (kc, f) =>
  listen_to_t(Ev.keydown, Dom_html.document, evt =>
    if (KeyCombo.matches(kc, evt)) {
      f(evt);
      ();
    } else {
      ();
    }
  );

type single_key =
  | Number(int)
  | Letter(string)
  | Underscore;

let letter_regexp = Js_of_ocaml.Regexp.regexp("^[a-zA-Z']$");
let lowercase_letter_regexp = Js_of_ocaml.Regexp.regexp("^[a-z]");

let is_single_key: Js.t(Dom_html.keyboardEvent) => option(single_key) =
  evt => {
    let ctrlKey = Js.to_bool(evt##.ctrlKey);
    let altKey = Js.to_bool(evt##.altKey);
    let metaKey = Js.to_bool(evt##.metaKey);
    if (ctrlKey || altKey || metaKey) {
      None;
    } else {
      let key = get_key(evt);
      switch (int_of_string_opt(key)) {
      | Some(n) => Some(Number(n))
      | None =>
        switch (Js_of_ocaml.Regexp.string_match(letter_regexp, key, 0)) {
        | Some(_) => Some(Letter(key))
        | None => key == "_" ? Some(Underscore) : None
        }
      };
    };
  };

let single_key_string: single_key => string =
  single_key =>
    switch (single_key) {
    | Number(n) => string_of_int(n)
    | Letter(x) => x
    | Underscore => "_"
    };

let is_movement_key: Js.t(Dom_html.keyboardEvent) => bool =
  evt => {
    let key = get_key(evt);
    switch (key) {
    | "ArrowLeft"
    | "ArrowRight"
    | "ArrowUp"
    | "ArrowDown"
    | "PageUp"
    | "PageDown"
    | "Home"
    | "End" => true
    | _ => false
    };
  };

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
let has_class = (classList: Js.t(Dom_html.tokenList), cls: PP.cls): bool =>
  Js.to_bool(classList##contains(Js.string(cls)));
let has_class_satisfying =
    (classList: Js.t(Dom_html.tokenList), pred: PP.cls => option('a))
    : option('a) => {
  let satisfied = ref(None);
  for (i in 0 to classList##.length - 1) {
    Js.Optdef.iter(classList##item(i), cls =>
      switch (pred(Js.to_string(cls))) {
      | None => ()
      | Some(x) => satisfied := Some(x)
      }
    );
  };
  satisfied^;
};
