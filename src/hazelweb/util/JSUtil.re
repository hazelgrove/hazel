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
  |> Option.map(s => Js.to_string(s));

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
  Js.Opt.get(doc##getElementById(Js.string(id)), () => {assert(false)});
};

let get_elem_by_cls = cls =>
  switch (
    Dom_html.document##getElementsByClassName(Js.string(cls))
    |> Dom.list_of_nodeList
  ) {
  | [] => None
  | [elem, ..._] => Some(elem)
  };

let force_get_elem_by_cls = cls =>
  switch (
    Dom_html.document##getElementsByClassName(Js.string(cls))
    |> Dom.list_of_nodeList
  ) {
  | [] => assert(false)
  | [elem, ..._] => elem
  };

let get_mouse_position = (evt): MousePosition.t => {
  {
    x: Js.Optdef.get(evt##.pageX, () => assert(false)),
    y: Js.Optdef.get(evt##.pageY, () => assert(false)),
  };
};

let element_from_point = (mouse_pos: MousePosition.t) => {
  Js.Unsafe.meth_call(
    Dom_html.document,
    "elementFromPoint",
    [|Js.Unsafe.inject(mouse_pos.x), Js.Unsafe.inject(mouse_pos.y)|],
  );
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

// let alpha_regexp = Js_of_ocaml.Regexp.regexp("^[a-zA-Z']$");
// let char_regexp = Js_of_ocaml.Regexp.regexp("^.$");

let is_single_key:
  (Js.t(Dom_html.keyboardEvent), Js_of_ocaml.Regexp.regexp) =>
  option(single_key) =
  (evt, char_regexp) => {
    let ctrlKey = Js.to_bool(evt##.ctrlKey);
    // let altKey = Js.to_bool(evt##.altKey);
    let metaKey = Js.to_bool(evt##.metaKey);
    if (ctrlKey || metaKey) {
      None;
    } else {
      let key = Key.get_key(evt);
      switch (int_of_string_opt(key)) {
      | Some(n) => Some(Number(n))
      | None =>
        switch (Js_of_ocaml.Regexp.string_match(char_regexp, key, 0)) {
        | Some(_) => Some(Letter(key))
        | None =>
          /* could be later refactored to a separate regex */
          switch (key) {
          | "_" => Some(Underscore)
          | "." => Some(Letter(key))
          | _ => None
          }
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
    let key = Key.get_key(evt);
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
