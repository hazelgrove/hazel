/* hazel-jit's drawing (^ticket(694), 2026-11-07; Matthew: "can we avoid going through a Hazel representation at all for
   the result of the (jit compiled) view function?"; in the app since ^ticket(714)).

   A compiled view's answer, the JS values its compiled code made, run and drawn straight into the virtual DOM: never
   made into a Hazel value, never walked by HazelDOM. It makes the nodes ViewCmdRunner and HazelDOM.go make from the
   same answer carried back as Hazel: every view command (Pure; Bind, whose continuation is the compiled JS function,
   called in JS; Editor, EvalSplice and ResultView, what they hand on made in JS as ViewCmdRunner makes it in Hazel), and every
   element, attribute and handler HazelDOM draws. What is not well formed is Unsupported (HazelJit.Unsupported), and
   the caller draws through Hazel, where HazelDOM shows it as it always has; so do apps (Syntax commit), whose
   messages transform the program's syntax.

   A handler crosses into Hazel only when its event comes: a message (OnClick's) is carried back then
   (HazelJitValue.of_js), and a handler that takes a payload is the compiled JS function, called in JS on the payload
   (made as HazelDOM makes it, carried into JS) and its answer, the message, carried back. HazelDOM instead applies
   the Hazel handler on the interpreter. */
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
let num = (v: any): float =>
  V.typeof(v) == "number"
    ? Js.float_of_number(Js.Unsafe.coerce(v))
    : unsupported("drawing: not a number");
let int = (v: any): int => {
  let f = num(v);
  Float.is_integer(f)
    ? int_of_float(f) : unsupported("drawing: not an Int");
};
let bool = (v: any): bool =>
  V.typeof(v) == "boolean"
    ? Js.to_bool(Js.Unsafe.coerce(v)) : unsupported("drawing: not a Bool");
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
/* a payload HazelDOM makes as a Hazel value (a mouse event, a key event, a wheel), carried into JS for a compiled handler */
let on_payload_value =
    (
      mvu: HazelDOM.t,
      ~gesture=HazelDOM.Commit,
      types,
      f: any,
      payload: DHExp.t,
    )
    : Ui_effect.t(unit) =>
  switch (V.to_js(payload)) {
  | js => on_payload(mvu, ~gesture, types, f, js)
  | exception exn =>
    prerr_endline("hazel-jit drawing: a payload: " ++ V.message(exn));
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
  let flag = (on: Attr.t) => bool(body) ? on : Attr.empty;
  let simple = (on: Attr.t) => {
    state();
    on;
  };
  let msg = () => dispatch(mvu, types, body);
  let payload = (~gesture=HazelDOM.Commit, f, mk) =>
    on_payload_value(mvu, ~gesture, types, f, mk);
  switch (name) {
  /* identity */
  | "Id" => Attr.id(str(body))
  | "Class" => Attr.class_(str(body))
  | "Classes" => Attr.classes(List.map(str, list(body)))
  /* form state */
  | "Disabled" => flag(Attr.disabled)
  | "Placeholder" => Attr.placeholder(str(body))
  | "Value" =>
    let s = str(body);
    Attr.many([Attr.value(s), Attr.value_prop(s)]);
  | "Checked" => flag(Attr.checked)
  | "Selected" => flag(Attr.create("selected", "selected"))
  | "ReadOnly" => flag(Attr.create("readonly", "readonly"))
  | "Required" => flag(Attr.create("required", "required"))
  | "AutoFocus" => flag(Attr.autofocus(true))
  /* links and media */
  | "Href" => Attr.href(str(body))
  | "Src" => Attr.create("src", str(body))
  | "Alt" => Attr.create("alt", str(body))
  | "Title" => Attr.title(str(body))
  | "Target" => Attr.create("target", str(body))
  /* inputs */
  | "Type" => Attr.type_(str(body))
  | "Name" => Attr.name(str(body))
  | "Min" => Attr.create("min", str(body))
  | "Max" => Attr.create("max", str(body))
  | "Step" => Attr.create("step", str(body))
  | "MaxLength" => Attr.create("maxlength", string_of_int(int(body)))
  | "Pattern" => Attr.create("pattern", str(body))
  /* layout */
  | "Width" => Attr.create("width", str(body))
  | "Height" => Attr.create("height", str(body))
  | "ColSpan" => Attr.create("colspan", string_of_int(int(body)))
  | "RowSpan" => Attr.create("rowspan", string_of_int(int(body)))
  | "Style" =>
    list(body)
    |> List.map(p => str(at(p, 0)) ++ ": " ++ str(at(p, 1)))
    |> String.concat(";")
    |> Attr.create("style")
  | "Data" => Attr.create("data-" ++ str(at(body, 0)), str(at(body, 1)))
  /* events whose handler is the message */
  | "OnClick" => simple(Attr.on_click(_ => msg()))
  | "OnDoubleClick" => simple(Attr.on_double_click(_ => msg()))
  | "OnMouseEnter" => simple(Attr.on_mouseenter(_ => msg()))
  | "OnMouseLeave" => simple(Attr.on_mouseleave(_ => msg()))
  | "OnFocus" => simple(Attr.on_focus(_ => msg()))
  | "OnBlur" => simple(Attr.on_blur(_ => msg()))
  | "OnSubmit" =>
    simple(Attr.on_submit(_ => Effect.Many([Effect.Prevent_default, msg()])))
  /* mouse events, the payload a MouseEvent */
  | "OnMouseDown" =>
    state();
    let f = fn(body);
    Attr.many([
      HazelDOM.capture_on_pointerdown,
      Attr.on_mousedown(evt =>
        payload(~gesture=Transient, f, HazelDOM.mouse_event_value(evt))
      ),
    ]);
  | "OnMouseUp" =>
    state();
    let f = fn(body);
    Attr.on_mouseup(evt => payload(f, HazelDOM.mouse_event_value(evt)));
  | "OnMouseMove" =>
    state();
    let f = fn(body);
    Attr.on_mousemove(evt =>
      payload(~gesture=Transient, f, HazelDOM.mouse_event_value(evt))
    );
  /* the pointer against the element, (x, y) */
  | "OnClickAt" =>
    state();
    let f = fn(body);
    Attr.on_click(evt => on_payload(mvu, types, f, relative_pos(evt)));
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
  | "OnWheelAt" =>
    state();
    let f = fn(body);
    Attr.on_wheel(evt =>
      Effect.Many([
        Effect.Prevent_default,
        payload(f, HazelDOM.wheel_value(evt)),
      ])
    );
  /* keys, the payload a KeyEvent */
  | "OnKeyDown" =>
    state();
    let f = fn(body);
    Attr.on_keydown(evt => payload(f, SubManager.key_event_of_js(evt)));
  | "OnKeyUp" =>
    state();
    let f = fn(body);
    Attr.on_keyup(evt => payload(f, SubManager.key_event_of_js(evt)));
  | "OnKeyPress" =>
    state();
    let f = fn(body);
    Attr.on_keypress(evt => payload(f, SubManager.key_event_of_js(evt)));
  /* text, the payload a String: for a livelit, each input a preview
     (Transient), the change at release the commit (as HazelDOM) */
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
  /* escape hatches */
  | "Create" => Attr.create(str(at(body, 0)), str(at(body, 1)))
  | "BoolAttr" =>
    let k = str(at(body, 0));
    bool(at(body, 1)) ? Attr.create(k, k) : Attr.empty;
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

/* a SpliceRef's id, and its value as JS */
let splice_ref = (v: any): (string, any) =>
  switch (ctor(v)) {
  | ("SpliceRef", p) => (str(at(p, 0)), at(p, 1))
  | _ => unsupported("drawing: not a SpliceRef")
  };
let rec elem = (mvu: HazelDOM.t, types, v: any): Node.t => {
  let (name, body) = ctor(v);
  switch (name) {
  | "Text" => Node.text(str(body))
  | "Bool" => Node.text(string_of_bool(bool(body)))
  | "Int" => Node.text(string_of_int(int(body)))
  | "Float" => Node.text(string_of_float(num(body)))
  | "Br" => Node.br()
  | "Hr" => Node.hr(~attrs=attrs(mvu, types, body), ())
  /* the host's splice, a live sub-editor, by the id its ref carries */
  | "Splice" =>
    let (id, _) = splice_ref(body);
    switch (mvu.splice_view(id)) {
    | Some(node) =>
      Node.div(
        ~key="livelit-splice-" ++ id,
        ~attrs=[Attr.classes(["livelit-splice"])],
        [node],
      )
    | None => unsupported("drawing: no splice " ++ id)
    };
  /* a splice's value in this run, drawn by Hazel (carried back) */
  | "SpliceResult" =>
    let (id, value) = splice_ref(body);
    Node.span(
      ~key="livelit-splice-result-" ++ id,
      ~attrs=[Attr.classes(["livelit-splice-result"])],
      [mvu.view_term(V.of_js(types, None, value))],
    );
  | "Input" => Node.input(~attrs=attrs(mvu, types, body), ())
  | "TextArea" =>
    let a = attrs(mvu, types, at(body, 0));
    let content = str(at(body, 1));
    Node.textarea(
      ~attrs=a @ [Attr.value(content), Attr.value_prop(content)],
      [],
    );
  | "Option" =>
    let a = attrs(mvu, types, at(body, 0));
    Node.option(~attrs=a, [Node.text(str(at(body, 1)))]);
  | "Img" => Node.img(~attrs=attrs(mvu, types, body), ())
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

/* ---- the view's command, run down to its Html (as ViewCmdRunner.run): a continuation is the compiled JS function,
   called in JS; what Editor, EvalSplice and ResultView hand it is made in JS, as ViewCmdRunner makes it in Hazel
   (2026-11-07: carrying the splice's value back to Hazel and in again, only to wrap it, was most of the Witch slide's
   ^picture render: 20 of 21 ms) */
let obj = (c: string, v: option(any)): any =>
  switch (v) {
  | Some(v) => Js.Unsafe.obj([|("c", V.jstr(c)), ("v", v)|])
  | None => Js.Unsafe.obj([|("c", V.jstr(c))|])
  };
let pair = (a: any, b: any): any => Js.Unsafe.inject(Js.array([|a, b|]));
let cons_list = (xs: list(any)): any =>
  List.fold_right(
    (h, t) => Js.Unsafe.obj([|("h", h), ("t", t)|]),
    xs,
    Js.Unsafe.inject(Js.null),
  );
/* Div([Style([... min-width n ch ...])], [inner]): ViewCmdRunner.editor_html and result_html's frame */
let sized = (n: int, inner: any): any =>
  obj(
    "Div",
    Some(
      pair(
        cons_list([
          obj(
            "Style",
            Some(
              cons_list([
                pair(V.jstr("display"), V.jstr("inline-block")),
                pair(
                  V.jstr("min-width"),
                  V.jstr(string_of_int(n) ++ "ch"),
                ),
              ]),
            ),
          ),
        ]),
        cons_list([inner]),
      ),
    ),
  );
let rec run = (types, cmd: any): any => {
  let call = (k, x) => Js.Unsafe.fun_call(fn(k), [|x|]);
  let fixed_width = dim =>
    switch (ctor(dim)) {
    | ("FixedWidth", n) => int(n)
    | _ => unsupported("drawing: expected a Dim, FixedWidth(n)")
    };
  let (name, body) = ctor(cmd);
  switch (name) {
  | "Pure" => body
  | "Bind" => run(types, call(at(body, 1), run(types, at(body, 0))))
  | "Editor" =>
    let args = at(body, 0);
    let r = at(args, 0);
    ignore(splice_ref(r));
    run(
      types,
      call(
        at(body, 1),
        sized(fixed_width(at(args, 1)), obj("Splice", Some(r))),
      ),
    );
  /* (a value the trip in carried is a value: ViewCmdRunner.result_of's Some(Val(v)), never Some(Indet)) */
  | "EvalSplice" =>
    let (_, value) = splice_ref(at(body, 0));
    run(
      types,
      call(at(body, 1), obj("Some", Some(obj("Val", Some(value))))),
    );
  | "ResultView" =>
    let args = at(body, 0);
    let r = at(args, 0);
    ignore(splice_ref(r));
    run(
      types,
      call(
        at(body, 1),
        obj(
          "Some",
          Some(
            sized(fixed_width(at(args, 1)), obj("SpliceResult", Some(r))),
          ),
        ),
      ),
    );
  | n => unsupported("drawing: not a view command: " ++ n)
  };
};

/* A view's answer, a command, run and drawn as HazelDOM.go draws the Html it runs to */
let go = (mvu: HazelDOM.t, types: V.types, answer: any): Node.t => {
  let html = run(types, answer);
  Node.div(
    ~attrs=[Attr.tabindex(2), Attr.classes(["MVU-render"])],
    [elem(mvu, types, html)],
  );
};
