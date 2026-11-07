/* hazel-jit's drawing (^ticket(694), 2026-11-07, a prototype; Matthew: "can we avoid going through a Hazel
   representation at all for the result of the (jit compiled) view function?").

   A compiled view's answer, the JS values its compiled code made, drawn straight into the virtual DOM: never made
   into a Hazel value, never walked by HazelDOM. It makes the nodes HazelDOM.go makes from the same answer carried
   back as Hazel, for the forms below; any other form is Unsupported (HazelJit.Unsupported), and the caller draws
   through Hazel as before. Covered: the command Pure; Text, Br, Input, Node (a custom or SVG element) and the
   elements that take (attrs, children); the attributes Style, Width, Height, Min, Max, Type, Value, Id, Class and
   Create; and the handlers OnClick, OnMouseDownAt, OnMouseMoveAt, OnMouseUpAt, OnInput and OnChange, for a livelit
   (State commit) only.

   A handler crosses into Hazel only when its event comes: a message (OnClick's) is carried back then
   (HazelJitValue.of_js), and a handler that takes a payload is the compiled JS function, called in JS on the payload
   as JS, and its answer, the message, carried back. HazelDOM instead applies the Hazel handler on the interpreter. */
open Virtual_dom.Vdom;
open Js_of_ocaml;
open Language;
module V = HazelJitValue;

type any = Js.Unsafe.any;

let unsupported = V.unsupported;

/* ---- reading the answer: a constructor {c, v}, a string, a list of cons cells, a tuple as an array */

let ctor = (v: any): (string, any) =>
  if (V.typeof(v) == "object" && !V.is_null(v) && V.has(v, "c")) {
    (Js.to_string(Js.Unsafe.coerce(V.get(v, "c"))), V.get(v, "v"));
  } else {
    unsupported("drawing: not a constructor");
  };
let str = (v: any): string =>
  V.typeof(v) == "string"
    ? Js.to_string(Js.Unsafe.coerce(v))
    : unsupported("drawing: not a string");
let list = (v: any): list(any) => {
  let rec walk = (v, acc) =>
    if (V.is_null(v)) {
      List.rev(acc);
    } else if (V.typeof(v) == "object" && V.has(v, "h")) {
      walk(V.get(v, "t"), [V.get(v, "h"), ...acc]);
    } else {
      unsupported("drawing: not a list");
    };
  walk(v, []);
};
let at = (v: any, i: int): any =>
  V.is_array(v) ? Js.Unsafe.get(v, i) : unsupported("drawing: not a tuple");
let fn = (v: any): any =>
  V.typeof(v) == "function"
    ? v : unsupported("drawing: a handler that is not a function");

/* ---- handlers */

let dispatch =
    (mvu: HazelDOM.t, ~gesture=HazelDOM.Commit, types, m: any)
    : Ui_effect.t(unit) =>
  switch (V.of_js(types, None, m)) {
  | msg => HazelDOM.dispatch(mvu, ~gesture, msg)
  | exception exn =>
    prerr_endline("hazel-jit drawing: a message: " ++ V.message(exn));
    Effect.Ignore;
  };
let on_payload =
    (mvu: HazelDOM.t, ~gesture=HazelDOM.Commit, types, f: any, payload: any)
    : Ui_effect.t(unit) =>
  switch (Js.Unsafe.fun_call(f, [|payload|])) {
  | m => dispatch(mvu, ~gesture, types, m)
  | exception exn =>
    prerr_endline("hazel-jit drawing: a handler: " ++ V.message(exn));
    Effect.Ignore;
  };
/* (x, y), the pointer against the element, as HazelDOM.relative_pos makes it: whole pixels */
let relative_pos = (evt): any => {
  let (x, y) = HazelDOM.relative_xy(evt);
  Js.Unsafe.inject(
    Js.array([|V.number(Float.round(x)), V.number(Float.round(y))|]),
  );
};

/* ---- attributes */

let attr = (mvu: HazelDOM.t, types, v: any): Attr.t => {
  let (name, body) = ctor(v);
  let state = () =>
    switch (mvu.commit) {
    | State => ()
    | Syntax => unsupported("drawing: an app's handler (Syntax commit)")
    };
  switch (name) {
  | "Style" =>
    list(body)
    |> List.map(p => str(at(p, 0)) ++ ": " ++ str(at(p, 1)))
    |> String.concat(";")
    |> Attr.create("style")
  | "Width" => Attr.create("width", str(body))
  | "Height" => Attr.create("height", str(body))
  | "Min" => Attr.create("min", str(body))
  | "Max" => Attr.create("max", str(body))
  | "Type" => Attr.type_(str(body))
  | "Value" =>
    let s = str(body);
    Attr.many([Attr.value(s), Attr.value_prop(s)]);
  | "Id" => Attr.id(str(body))
  | "Class" => Attr.class_(str(body))
  | "Create" => Attr.create(str(at(body, 0)), str(at(body, 1)))
  | "OnClick" => Attr.on_click(_ => dispatch(mvu, types, body))
  | "OnMouseDownAt" =>
    state();
    let f = fn(body);
    Attr.many([
      HazelDOM.capture_on_pointerdown,
      Attr.on_mousedown(evt =>
        on_payload(mvu, ~gesture=Transient, types, f, relative_pos(evt))
      ),
    ]);
  | "OnMouseMoveAt" =>
    state();
    let f = fn(body);
    Attr.on_mousemove(evt =>
      on_payload(mvu, ~gesture=Transient, types, f, relative_pos(evt))
    );
  | "OnMouseUpAt" =>
    state();
    let f = fn(body);
    Attr.on_mouseup(evt => on_payload(mvu, types, f, relative_pos(evt)));
  | "OnInput" =>
    state();
    let f = fn(body);
    Attr.many([
      Attr.on_input((_, s) =>
        on_payload(mvu, ~gesture=Transient, types, f, V.jstr(s))
      ),
      Attr.on_change((_, s) => on_payload(mvu, types, f, V.jstr(s))),
    ]);
  | "OnChange" =>
    state();
    let f = fn(body);
    Attr.on_change((_, s) => on_payload(mvu, types, f, V.jstr(s)));
  | n => unsupported("drawing: the attribute " ++ n)
  };
};

/* ---- elements */

/* the elements HazelDOM draws from (attrs, children), each by its own name in lower case */
let standard = [
  "Div",
  "Span",
  "P",
  "Pre",
  "Code",
  "Blockquote",
  "H1",
  "H2",
  "H3",
  "H4",
  "H5",
  "H6",
  "Ul",
  "Ol",
  "Li",
  "Form",
  "Label",
  "Button",
  "Select",
  "Table",
  "Thead",
  "Tbody",
  "Tr",
  "Th",
  "Td",
  "Header",
  "Footer",
  "Nav",
  "Main",
  "Section",
  "Article",
  "Aside",
  "A",
];

let rec elem = (mvu: HazelDOM.t, types, v: any): Node.t => {
  let (name, body) = ctor(v);
  switch (name) {
  | "Text" => Node.text(str(body))
  | "Br" => Node.br()
  | "Input" => Node.input(~attrs=attrs(mvu, types, body), ())
  | "Node" =>
    let tag = str(at(body, 0));
    let a = attrs(mvu, types, at(body, 1));
    let children = elems(mvu, types, at(body, 2));
    HazelDOM.is_svg_tag(tag)
      ? Node.create_svg(tag, ~attrs=a, children)
      : Node.create(tag, ~attrs=a, children);
  | n when List.mem(n, standard) =>
    let a = attrs(mvu, types, at(body, 0));
    Node.create(
      String.lowercase_ascii(n),
      ~attrs=a,
      elems(mvu, types, at(body, 1)),
    );
  | n => unsupported("drawing: the element " ++ n)
  };
}
and attrs = (mvu, types, v: any): list(Attr.t) =>
  List.map(attr(mvu, types), list(v))
and elems = (mvu, types, v: any): list(Node.t) =>
  List.map(elem(mvu, types), list(v));

/* A view's answer, a command, drawn as HazelDOM.go draws the Html it runs to: only Pure(html) for now */
let go = (mvu: HazelDOM.t, types: V.types, answer: any): Node.t => {
  let html =
    switch (ctor(answer)) {
    | ("Pure", h) => h
    | (n, _) => unsupported("drawing: the view command " ++ n)
    };
  Node.div(
    ~attrs=[Attr.tabindex(2), Attr.classes(["MVU-render"])],
    [elem(mvu, types, html)],
  );
};
