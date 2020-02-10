module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Ev = Dom_html.Event;

let log = x => Js_of_ocaml.Firebug.console##log(x);

let log_sexp = (sexp: Sexplib.Sexp.t) => log(Sexplib.Sexp.to_string(sexp));

let get_child_nodes = (root: Js.t(Dom.node)): list(Js.t(Dom.node)) =>
  Dom.list_of_nodeList(root##.childNodes);

let get_attr = (attr: string, elem: Js.t(Dom_html.element)): option(string) =>
  Js.Opt.to_option(elem##getAttribute(Js.string(attr)))
  |> OptUtil.map(s => Js.to_string(s));

let force_get_attr = (attr: string, elem: Js.t(Dom_html.element)): string => {
  switch (elem |> get_attr(attr)) {
  | None =>
    log(elem);
    log(attr);
    assert(false);
  | Some(s) => s
  };
};

let has_attr = (attr: string, elem: Js.t(Dom_html.element)): bool =>
  switch (Js.Opt.to_option(elem##getAttribute(Js.string(attr)))) {
  | None => false
  | Some(_) => true
  };

let force_get_closest_elem = node =>
  Js.Opt.get(Dom_html.CoerceTo.element(node), () =>
    switch (node##.nodeType) {
    | TEXT =>
      switch (Js.Opt.to_option(node##.parentNode)) {
      | None => assert(false)
      | Some(parent) =>
        Js.Opt.get(Dom_html.CoerceTo.element(parent), () => assert(false))
      }
    | _ => assert(false)
    }
  );

let rec get_descendant_nodes = (root: Js.t(Dom.node)): list(Js.t(Dom.node)) => {
  let children = root##.childNodes;
  let descendants = ref([]);
  for (i in children##.length - 1 downto 0) {
    let child = Js.Opt.get(children##item(i), () => assert(false));
    descendants := [[child], get_descendant_nodes(child), ...descendants^];
  };
  List.flatten(descendants^);
};

let query_ancestors =
    (query: Js.t(Dom.node) => option('a), node: Js.t(Dom.node))
    : option('a) => {
  let query_result = ref(None);
  let current_node = ref(Js.some(node));
  while (query_result^ == None && Js.Opt.test(current_node^)) {
    let cur = Js.Opt.get(current_node^, () => assert(false));
    query_result := query(cur);
    current_node :=  cur##.parentNode;
  };
  query_result^;
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

let clss_of_elem = elem => {
  let clss = ref([]);
  let classList = elem##.classList;
  for (j in 0 to classList##.length - 1) {
    let cls_j =
      Js.(to_string(Optdef.get(classList##item(j), () => assert(false))));
    clss := [cls_j, ...clss^];
  };
  clss^;
};

let elem_has_cls = (cls: string, elem: Js.t(Dom_html.element)): bool => {
  let found = ref(false);
  let classList = elem##.classList;
  for (j in 0 to classList##.length - 1) {
    let cls_j =
      Js.(to_string(Optdef.get(classList##item(j), () => assert(false))));
    cls_j == cls ? found := true : ();
  };
  found^;
};

let node_has_cls = (cls: string, node: Js.t(Dom.node)): bool =>
  switch (Js.Opt.to_option(Dom_html.CoerceTo.element(node))) {
  | None => false
  | Some(elem) => elem_has_cls(cls, elem)
  };

let get_selection_anchor = () => {
  let selection = Dom_html.window##getSelection;
  (selection##.anchorNode, selection##.anchorOffset);
};

let unset_caret = () => Dom_html.window##getSelection##removeAllRanges;

let set_caret = (anchorNode, offset) => {
  let go = () => {
    let selection = Dom_html.window##getSelection;
    let range = Dom_html.document##createRange;
    range##setStart(anchorNode, offset);
    range##setEnd(anchorNode, offset);
    selection##removeAllRanges;
    selection##addRange(range);
  };
  switch (go()) {
  | () => ()
  | exception e =>
    log(Js.string(__LOC__ ++ ": Failed to set caret"));
    log(Js.string(Printexc.to_string(e)));
    log(anchorNode);
    log(offset);
    assert(false);
  };
};

let reset_caret = () => {
  let selection = Dom_html.window##getSelection;
  if (selection##.rangeCount <= 0) {
    ();
  } else {
    let current_range = selection##getRangeAt(0);
    selection##removeAllRanges;
    selection##addRange(current_range);
  };
};

let get_anchor_node = (): Js.t(Dom.node) =>
  Dom_html.window##getSelection##.anchorNode;

let get_anchor_offset = (): int =>
  Dom_html.window##getSelection##.anchorOffset;

let force_get_anchor_node_value = () => {
  let anchorNode = get_anchor_node();
  Js.to_string(Js.Opt.get(anchorNode##.nodeValue, () => assert(false)));
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

let get_elem_by_id = id => {
  let doc = Dom_html.document;
  Js.Opt.to_option(doc##getElementById(Js.string(id)));
};

let get_elems_with_cls = cls => {
  let doc = Dom_html.document;
  doc##querySelectorAll(Js.string("." ++ cls)) |> Dom.list_of_nodeList;
};

let force_get_elem_by_id = id => {
  let doc = Dom_html.document;
  Js.Opt.get(
    doc##getElementById(Js.string(id)),
    () => {
      log(id);
      assert(false);
    },
  );
};

let force_get_elem_by_cls = cls =>
  switch (
    Dom_html.document##getElementsByClassName(Js.string(cls))
    |> Dom.list_of_nodeList
  ) {
  | [] =>
    log(cls);
    assert(false);
  | [elem, ..._] => elem
  };

let force_get_parent_elem = elem =>
  (elem: Js.t(Dom_html.element) :> Js.t(Dom.node))
  |> (node => node##.parentNode)
  |> Js.Opt.to_option
  |> OptUtil.get(() => assert(false))
  |> Dom_html.CoerceTo.element
  |> Js.Opt.to_option
  |> OptUtil.get(() => assert(false));

let force_get_next_sibling_elem = elem =>
  (elem: Js.t(Dom_html.element) :> Js.t(Dom.node))
  |> (node => node##.nextSibling)
  |> Js.Opt.to_option
  |> OptUtil.get(() => assert(false))
  |> Dom_html.CoerceTo.element
  |> Js.Opt.to_option
  |> OptUtil.get(() => assert(false));

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
  let ctrl = {c: Held, s: NotHeld, a: NotHeld, m: NotHeld};
  let shift = {c: NotHeld, s: Held, a: NotHeld, m: NotHeld};
  let alt = {c: NotHeld, s: Any, a: Held, m: NotHeld};
  let no_ctrl_alt_meta = {c: NotHeld, s: Any, a: NotHeld, m: NotHeld};
  let ctrl_shift = {c: Held, s: Held, a: NotHeld, m: NotHeld};

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
    // https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/code
    | Code(string)
    // https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key
    | Key(string);

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
      String.equal(String.uppercase_ascii(key), String.uppercase_ascii(k));
    };

  let matches = (k, evt: Js.t(Dom_html.keyboardEvent)) => {
    let recognition_methods = k.recognition_methods;
    ListUtil.any(recognition_methods, recognize(evt));
  };
};

module KeyCombo = {
  module Details = {
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
    let ctrl_shift = key => {mod_keys: ModKeys.ctrl_shift, key};

    let matches = (kc, evt: Js.t(Dom_html.keyboardEvent)) =>
      ModKeys.matches(kc.mod_keys, evt) && Key.matches(kc.key, evt);

    let name = kc => {
      let mod_prefix = ModKeys.mod_prefix(kc.mod_keys);
      mod_prefix ++ kc.key.plain_name;
    };

    let enter = plain(Key.code2("Enter", "Enter", "NumpadEnter"));
    let escape = plain(Key.the_code("Escape"));
    let backspace = plain(Key.the_code("Backspace"));
    let delete = plain(Key.the_code("Delete"));
    let tab = plain(Key.the_code("Tab"));
    let shift_tab = shift(Key.the_code("Tab"));
    let space = plain(Key.the_code("Space"));
    let lt = no_ctrl_alt_meta(Key.the_key("<"));
    let gt = no_ctrl_alt_meta(Key.the_key(">"));
    let colon = no_ctrl_alt_meta(Key.the_key(":"));
    let backslash = no_ctrl_alt_meta(Key.the_key("\\"));
    let left_parens = no_ctrl_alt_meta(Key.the_key("("));
    let right_parens = no_ctrl_alt_meta(Key.the_key(")"));
    let left_bracket = no_ctrl_alt_meta(Key.the_key("["));
    let right_bracket = no_ctrl_alt_meta(Key.the_key("]"));
    let qmark = no_ctrl_alt_meta(Key.the_key("?"));
    let equals = no_ctrl_alt_meta(Key.the_key("="));
    let pound = no_ctrl_alt_meta(Key.the_key("#"));
    let plus = no_ctrl_alt_meta(Key.the_key("+"));
    let minus = no_ctrl_alt_meta(Key.the_key("-"));
    let asterisk = no_ctrl_alt_meta(Key.the_key("*"));
    let semicolon = no_ctrl_alt_meta(Key.the_key(";"));
    let comma = no_ctrl_alt_meta(Key.the_key(","));
    let vbar = no_ctrl_alt_meta(Key.the_key("|"));
    let ampersand = no_ctrl_alt_meta(Key.the_key("&"));
    let dollar = no_ctrl_alt_meta(Key.the_key("$"));
    let amp = no_ctrl_alt_meta(Key.the_key("&"));
    let alt_L = alt(Key.the_key("l"));
    let alt_R = alt(Key.the_key("r"));
    let alt_C = alt(Key.the_key("c"));
    let alt_PageUp = alt(Key.the_key("PageUp"));
    let alt_PageDown = alt(Key.the_key("PageDown"));
    let alt_T = alt(Key.the_key("T"));
    let alt_F = alt(Key.the_key("F"));
    let ctrl_z = ctrl(Key.the_key("z"));
    let ctrl_shift_z = ctrl_shift(Key.the_key("Z"));
  };

  [@deriving sexp]
  type t =
    | Escape
    | Backspace
    | Delete
    | ShiftTab
    | Tab
    | GT
    | Ampersand
    | VBar
    | LeftParen
    | Colon
    | Equals
    | Enter
    | Backslash
    | Plus
    | Minus
    | Asterisk
    | LT
    | Space
    | Comma
    | LeftBracket
    | Semicolon
    | Alt_L
    | Alt_R
    | Alt_C
    | Pound
    | Ctrl_Z
    | Ctrl_Shift_Z;

  let get_details =
    fun
    | Pound => Details.pound
    | Escape => Details.escape
    | Backspace => Details.backspace
    | Delete => Details.delete
    | ShiftTab => Details.shift_tab
    | Tab => Details.tab
    | GT => Details.gt
    | Ampersand => Details.ampersand
    | VBar => Details.vbar
    | LeftParen => Details.left_parens
    | Colon => Details.colon
    | Equals => Details.equals
    | Enter => Details.enter
    | Backslash => Details.backslash
    | Plus => Details.plus
    | Minus => Details.minus
    | Asterisk => Details.asterisk
    | LT => Details.lt
    | Space => Details.space
    | Comma => Details.comma
    | LeftBracket => Details.left_bracket
    | Semicolon => Details.semicolon
    | Alt_L => Details.alt_L
    | Alt_R => Details.alt_R
    | Alt_C => Details.alt_C
    | Ctrl_Z => Details.ctrl_z
    | Ctrl_Shift_Z => Details.ctrl_shift_z;

  let of_evt = (evt: Js.t(Dom_html.keyboardEvent)): option(t) => {
    let evt_matches = details => Details.matches(details, evt);
    if (evt_matches(Details.pound)) {
      Some(Pound);
    } else if (evt_matches(Details.ctrl_z)) {
      Some(Ctrl_Z);
    } else if (evt_matches(Details.ctrl_shift_z)) {
      Some(Ctrl_Shift_Z);
    } else if (evt_matches(Details.escape)) {
      Some(Escape);
    } else if (evt_matches(Details.backspace)) {
      Some(Backspace);
    } else if (evt_matches(Details.delete)) {
      Some(Delete);
    } else if (evt_matches(Details.shift_tab)) {
      Some(ShiftTab);
    } else if (evt_matches(Details.tab)) {
      Some(Tab);
    } else if (evt_matches(Details.gt)) {
      Some(GT);
    } else if (evt_matches(Details.ampersand)) {
      Some(Ampersand);
    } else if (evt_matches(Details.vbar)) {
      Some(VBar);
    } else if (evt_matches(Details.left_parens)) {
      Some(LeftParen);
    } else if (evt_matches(Details.colon)) {
      Some(Colon);
    } else if (evt_matches(Details.equals)) {
      Some(Equals);
    } else if (evt_matches(Details.enter)) {
      Some(Enter);
    } else if (evt_matches(Details.backslash)) {
      Some(Backslash);
    } else if (evt_matches(Details.plus)) {
      Some(Plus);
    } else if (evt_matches(Details.minus)) {
      Some(Minus);
    } else if (evt_matches(Details.asterisk)) {
      Some(Asterisk);
    } else if (evt_matches(Details.lt)) {
      Some(LT);
    } else if (evt_matches(Details.space)) {
      Some(Space);
    } else if (evt_matches(Details.comma)) {
      Some(Comma);
    } else if (evt_matches(Details.left_bracket)) {
      Some(LeftBracket);
    } else if (evt_matches(Details.semicolon)) {
      Some(Semicolon);
    } else if (evt_matches(Details.alt_L)) {
      Some(Alt_L);
    } else if (evt_matches(Details.alt_R)) {
      Some(Alt_R);
    } else if (evt_matches(Details.alt_C)) {
      Some(Alt_C);
    } else {
      None;
    };
  };
};

let listen_for_key = (kc, f) =>
  listen_to_t(Ev.keydown, Dom_html.document, evt =>
    if (KeyCombo.Details.matches(kc, evt)) {
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

let window_has_focus = (): bool => {
  let result: Js.t(bool) =
    Js.Unsafe.meth_call(Dom_html.document, "hasFocus", [||]);
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
let has_class = (classList: Js.t(Dom_html.tokenList), cls: string): bool =>
  Js.to_bool(classList##contains(Js.string(cls)));
let has_class_satisfying =
    (classList: Js.t(Dom_html.tokenList), pred: string => option('a))
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

let force_opt = x => Js.Opt.get(x, () => failwith("forced opt"));

// TODO: find better Module to put this in
module Vdom = Virtual_dom.Vdom;
// let contenteditable_of_layout: Layout.t('annot) => Vdom.Node.t =
//   layout => {
//     let record: Layout.text('annot, list(Vdom.Node.t), Vdom.Node.t) = {
//       imp_of_string: string => [Vdom.Node.text(string)],
//       imp_of_annot: (_, string) => [Vdom.Node.span([], string)], // TODO: add span data
//       imp_append: (s1, s2) => s1 @ s2,
//       imp_newline: _ => [Vdom.Node.br([])],
//       t_of_imp: s => Vdom.Node.span([], s) // TODO: use something other than `span`?
//     };
//     Layout.make_of_layout(record, layout);
//   };

let rec vdom_of_box = (box: Box.t('annot)): Vdom.Node.t =>
  switch (box) {
  | Text(string) =>
    Vdom.Node.div([Vdom.Attr.classes(["text"])], [Vdom.Node.text(string)])
  | HBox(bs) =>
    Vdom.Node.div([Vdom.Attr.classes(["hbox"])], List.map(vdom_of_box, bs))
  | VBox(bs) =>
    Vdom.Node.div([Vdom.Attr.classes(["vbox"])], List.map(vdom_of_box, bs))
  | Annot(_, b) =>
    // TODO
    vdom_of_box(b)
  };
