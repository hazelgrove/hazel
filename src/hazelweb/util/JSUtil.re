module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Ev = Dom_html.Event;

let log = x => Js_of_ocaml.Firebug.console##log(x);

let get_attr = (attr: string, elem: Js.t(Dom_html.element)): option(string) =>
  Js.Opt.to_option(elem##getAttribute(Js.string(attr)))
  |> Option.map(s => Js.to_string(s));

let get_elem_by_id = id => {
  let doc = Dom_html.document;
  Js.Opt.to_option(doc##getElementById(Js.string(id)));
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

type single_key =
  | Number(int)
  | Letter(string)
  | Underscore;

let letter_regexp = Js_of_ocaml.Regexp.regexp("^[a-zA-Z']$");

let is_single_key: Js.t(Dom_html.keyboardEvent) => option(single_key) =
  evt => {
    let ctrlKey = Js.to_bool(evt##.ctrlKey);
    let altKey = Js.to_bool(evt##.altKey);
    let metaKey = Js.to_bool(evt##.metaKey);
    if (ctrlKey || altKey || metaKey) {
      None;
    } else {
      let key = Key.get_key(evt);
      switch (int_of_string_opt(key)) {
      | Some(n) => Some(Number(n))
      | None =>
        switch (Js_of_ocaml.Regexp.string_match(letter_regexp, key, 0)) {
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

type div_element = Js.t(Dom_html.divElement);
type node = Js.t(Dom.node);

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
//     Layout.mk_of_layout(record, layout);
//   };

open Pretty;
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
