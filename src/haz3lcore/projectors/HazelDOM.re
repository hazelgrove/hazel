open Virtual_dom.Vdom;
open Util;
open Language;
open IdTagged.FreshGrammar;
open MvuShape;

/* Where a dispatched msg is committed:
 * - State: the web-side AppStore evaluates update(model, msg) and stores
 *   the new model.
 * - Syntax: the inline projector (HTMLProj) evaluates msg(model) and
 *   splices the result back into the document. A msg is an Html -> Html
 *   transform: self-modifying = Elm with update = apply.
 * - Value: there is nothing to commit to. The rendered html is an
 *   evaluated value with no syntactic counterpart to splice back into
 *   (HtmlRenderer, which draws a probe sample), so handlers are inert and
 *   events are left to propagate normally. */
[@deriving (show({with_path: false}), sexp, yojson)]
type commit =
  | State
  | Syntax
  | Value;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  inject: DHExp.t => Ui_effect.t(unit),
  view_term: DHExp.t => Node.t,
  commit,
};

let render_style_attr = (d: DHExp.t): option(string) =>
  switch (of_pair(d)) {
  | Some((name, value)) => Some(name ++ ": " ++ value)
  | _ => None
  };

let render_styles = styles =>
  styles
  |> List.filter_map(render_style_attr)
  |> String.concat(";")
  |> Attr.create("style");

// === Event handlers ===
//
// One dispatch path: a handler's job is always to produce a msg, which is
// handed to mvu.inject; the commit target decides what committing means
// (see `commit` above).

let dispatch = (mvu: t, msg: DHExp.t): Ui_effect.t(unit) =>
  Effect.Many([Effect.Stop_propagation, mvu.inject(msg)]);

// Syntax-commit msg for a payload event: fun m -> handler((m, payload)).
// The already-evaluated handler closure and payload value are embedded
// directly in the constructed term (the evaluator handles embedded
// Closures); the commit side applies the msg to the current model.
let payload_transform = (handler: DHExp.t, payload: DHExp.t): DHExp.t =>
  Exp.fn(
    Pat.var("m"),
    Exp.ap(Forward, handler, Exp.tuple([Exp.var("m"), payload])),
    None,
    None,
  );

// Simple event: the handler IS the msg (in Syntax mode: an Html -> Html
// transform). Under Value commit there is nowhere to dispatch to.
let on_ = (mvu: t, handler, _evt) =>
  mvu.commit == Value ? Effect.Ignore : dispatch(mvu, handler);

// Payload event.
// State: handler is payload -> msg; evaluate it now. On error, dispatch
//   nothing (never commit garbage).
// Syntax: handler is (Html, payload) -> Html; the msg is the transform
//   fun m -> handler((m, payload)), applied at commit time.
let on_payload = (mvu: t, what: string, handler, payload: DHExp.t) =>
  switch (mvu.commit) {
  | Value => Effect.Ignore
  | Syntax => dispatch(mvu, payload_transform(handler, payload))
  | State =>
    switch (safe_evaluate(Exp.ap(Forward, handler, payload))) {
    | Ok(msg) => dispatch(mvu, msg)
    | Error(err) =>
      prerr_endline("HazelDOM: " ++ what ++ " handler error: " ++ err);
      Effect.Ignore;
    }
  };

let on_input = (mvu: t, handler, _evt, arg) =>
  on_payload(mvu, "input", handler, Exp.string(arg));

let on_mouse = (mvu: t, handler, evt) => {
  /* MouseEvent value (labeled tuple, see BuiltinsADT.Event.mouse) */
  let mouse_event =
    Exp.tuple([
      field("x", Exp.float(float_of_int(evt##.clientX))),
      field("y", Exp.float(float_of_int(evt##.clientY))),
      field("button", Exp.int(evt##.button)),
      field("ctrl", Exp.bool(Js_of_ocaml.Js.to_bool(evt##.ctrlKey))),
      field("shift", Exp.bool(Js_of_ocaml.Js.to_bool(evt##.shiftKey))),
      field("alt", Exp.bool(Js_of_ocaml.Js.to_bool(evt##.altKey))),
      field("meta", Exp.bool(Js_of_ocaml.Js.to_bool(evt##.metaKey))),
    ]);
  on_payload(mvu, "mouse", handler, mouse_event);
};

/* Pointer position relative to the element the handler is attached to, as
   an (x, y) int pair — what a widget needs to interpret events on its own
   surface (clientX/clientY in MouseEvent can't be, since Hazel code can't
   learn the element's origin). Measured against currentTarget's rect, not
   offsetX/offsetY: offset coords are target-relative, so they jump when the
   pointer passes over a child element mid-gesture. */
let relative_xy = evt =>
  switch (Js_of_ocaml.Js.Opt.to_option(evt##.currentTarget)) {
  | Some(el) =>
    let rect = el##getBoundingClientRect;
    (
      float_of_int(evt##.clientX) -. rect##.left,
      float_of_int(evt##.clientY) -. rect##.top,
    );
  | None => (float_of_int(evt##.offsetX), float_of_int(evt##.offsetY))
  };

let px = (f: float): DHExp.t => Exp.int(int_of_float(Float.round(f)));

let relative_pos = (evt): DHExp.t => {
  let (x, y) = relative_xy(evt);
  Exp.tuple([px(x), px(y)]);
};

let on_mouse_at = (mvu: t, what: string, handler, evt) =>
  on_payload(mvu, what, handler, relative_pos(evt));

/* Wheel payload: (x, y, dx, dy) — element-relative position plus scroll
   deltas. Default is prevented so a zoom/pan surface doesn't also scroll
   the page. */
let on_wheel_at = (mvu: t, handler, evt) => {
  let (x, y) = relative_xy(evt);
  let payload =
    Exp.tuple([
      px(x),
      px(y),
      Exp.float(evt##.deltaX),
      Exp.float(evt##.deltaY),
    ]);
  Effect.Many([
    Effect.Prevent_default,
    on_payload(mvu, "wheel-at", handler, payload),
  ]);
};

let on_key = (mvu: t, handler, evt) =>
  on_payload(mvu, "keyboard", handler, SubManager.key_event_of_js(evt));

// === Attribute rendering ===

let render_attr = (mvu: t, d: DHExp.t): Attr.t => {
  let attr_err = (d: DHExp.t) => {
    let name =
      switch (of_constructor(d)) {
      | Some((name, _)) => name
      | None => "<not a constructor>"
      };
    prerr_endline("HazelDOM: unrecognized attribute: " ++ name);
    Attr.empty;
  };
  switch (of_constructor(d)) {
  | Some(x) =>
    switch (x) {
    // === Identity ===
    | ("Id", body) =>
      switch (of_string(body)) {
      | Some(s) => Attr.id(s)
      | None => attr_err(d)
      }
    | ("Class", body) =>
      switch (of_string(body)) {
      | Some(s) => Attr.class_(s)
      | None => attr_err(d)
      }
    | ("Classes", body) =>
      switch (of_string_list(body)) {
      | Some(ss) => Attr.classes(ss)
      | None => attr_err(d)
      }

    // === Common properties ===
    | ("Disabled", body) =>
      switch (of_bool(body)) {
      | Some(true) => Attr.disabled
      | Some(false) => Attr.empty
      | None => attr_err(d)
      }
    | ("Placeholder", body) =>
      switch (of_string(body)) {
      | Some(s) => Attr.placeholder(s)
      | None => attr_err(d)
      }
    | ("Value", body) =>
      switch (of_string(body)) {
      /* Both the attribute and the property: once a user has typed into an
         input, the browser stops mirroring the attribute into the displayed
         value, so an attribute-only update cannot clear or reset the field.
         An MVU app that empties `Value` after a submit needs the property. */
      | Some(s) => Attr.many([Attr.value(s), Attr.value_prop(s)])
      | None => attr_err(d)
      }
    | ("Checked", body) =>
      switch (of_bool(body)) {
      | Some(true) => Attr.checked
      | Some(false) => Attr.empty
      | None => attr_err(d)
      }
    | ("Selected", body) =>
      switch (of_bool(body)) {
      | Some(true) => Attr.create("selected", "selected")
      | Some(false) => Attr.empty
      | None => attr_err(d)
      }
    | ("ReadOnly", body) =>
      switch (of_bool(body)) {
      | Some(true) => Attr.create("readonly", "readonly")
      | Some(false) => Attr.empty
      | None => attr_err(d)
      }
    | ("Required", body) =>
      switch (of_bool(body)) {
      | Some(true) => Attr.create("required", "required")
      | Some(false) => Attr.empty
      | None => attr_err(d)
      }
    | ("AutoFocus", body) =>
      switch (of_bool(body)) {
      | Some(true) => Attr.autofocus(true)
      | Some(false) => Attr.empty
      | None => attr_err(d)
      }

    // === Links/media ===
    | ("Href", body) =>
      switch (of_string(body)) {
      | Some(s) => Attr.href(s)
      | None => attr_err(d)
      }
    | ("Src", body) =>
      switch (of_string(body)) {
      | Some(s) => Attr.create("src", s)
      | None => attr_err(d)
      }
    | ("Alt", body) =>
      switch (of_string(body)) {
      | Some(s) => Attr.create("alt", s)
      | None => attr_err(d)
      }
    | ("Title", body) =>
      switch (of_string(body)) {
      | Some(s) => Attr.title(s)
      | None => attr_err(d)
      }
    | ("Target", body) =>
      switch (of_string(body)) {
      | Some(s) => Attr.create("target", s)
      | None => attr_err(d)
      }

    // === Input specifics ===
    | ("Type", body) =>
      switch (of_string(body)) {
      | Some(s) => Attr.type_(s)
      | None => attr_err(d)
      }
    | ("Name", body) =>
      switch (of_string(body)) {
      | Some(s) => Attr.name(s)
      | None => attr_err(d)
      }
    | ("Min", body) =>
      switch (of_string(body)) {
      | Some(s) => Attr.create("min", s)
      | None => attr_err(d)
      }
    | ("Max", body) =>
      switch (of_string(body)) {
      | Some(s) => Attr.create("max", s)
      | None => attr_err(d)
      }
    | ("Step", body) =>
      switch (of_string(body)) {
      | Some(s) => Attr.create("step", s)
      | None => attr_err(d)
      }
    | ("MaxLength", body) =>
      switch (of_int(body)) {
      | Some(n) => Attr.create("maxlength", string_of_int(n))
      | None => attr_err(d)
      }
    | ("Pattern", body) =>
      switch (of_string(body)) {
      | Some(s) => Attr.create("pattern", s)
      | None => attr_err(d)
      }

    // === Layout ===
    | ("Width", body) =>
      switch (of_string(body)) {
      | Some(s) => Attr.create("width", s)
      | None => attr_err(d)
      }
    | ("Height", body) =>
      switch (of_string(body)) {
      | Some(s) => Attr.create("height", s)
      | None => attr_err(d)
      }
    | ("ColSpan", body) =>
      switch (of_int(body)) {
      | Some(n) => Attr.create("colspan", string_of_int(n))
      | None => attr_err(d)
      }
    | ("RowSpan", body) =>
      switch (of_int(body)) {
      | Some(n) => Attr.create("rowspan", string_of_int(n))
      | None => attr_err(d)
      }

    // === Styling ===
    | ("Style", {term: ListLit(styles), _}) => render_styles(styles)

    // === Data attributes ===
    | ("Data", p) =>
      switch (of_pair(p)) {
      | Some((name, value)) => Attr.create("data-" ++ name, value)
      | None => attr_err(d)
      }

    // === Simple event handlers: the handler IS the msg ===
    | ("OnClick", handler) => Attr.on_click(on_(mvu, handler))
    | ("OnDoubleClick", handler) => Attr.on_double_click(on_(mvu, handler))
    | ("OnMouseEnter", handler) => Attr.on_mouseenter(on_(mvu, handler))
    | ("OnMouseLeave", handler) => Attr.on_mouseleave(on_(mvu, handler))
    | ("OnFocus", handler) => Attr.on_focus(_ => on_(mvu, handler, ()))
    | ("OnBlur", handler) => Attr.on_blur(_ => on_(mvu, handler, ()))
    // Prevent the browser's default form submission (page reload)
    | ("OnSubmit", handler) =>
      Attr.on_submit(evt =>
        Effect.Many([Effect.Prevent_default, on_(mvu, handler, evt)])
      )

    // === Mouse event handlers (payload: MouseEvent) ===
    | ("OnMouseDown", handler) => Attr.on_mousedown(on_mouse(mvu, handler))
    | ("OnMouseUp", handler) => Attr.on_mouseup(on_mouse(mvu, handler))
    | ("OnMouseMove", handler) => Attr.on_mousemove(on_mouse(mvu, handler))

    // === Element-relative mouse handlers (payload: (x, y) px int pair) ===
    | ("OnClickAt", handler) =>
      Attr.on_click(on_mouse_at(mvu, "click-at", handler))
    | ("OnMouseDownAt", handler) =>
      Attr.on_mousedown(on_mouse_at(mvu, "mousedown-at", handler))
    | ("OnMouseMoveAt", handler) =>
      Attr.on_mousemove(on_mouse_at(mvu, "mousemove-at", handler))
    | ("OnMouseUpAt", handler) =>
      Attr.on_mouseup(on_mouse_at(mvu, "mouseup-at", handler))
    | ("OnWheelAt", handler) => Attr.on_wheel(on_wheel_at(mvu, handler))

    // === Keyboard event handlers (payload: KeyEvent) ===
    | ("OnKeyDown", handler) => Attr.on_keydown(on_key(mvu, handler))
    | ("OnKeyUp", handler) => Attr.on_keyup(on_key(mvu, handler))
    | ("OnKeyPress", handler) => Attr.on_keypress(on_key(mvu, handler))

    // === Input event handlers (payload: String) ===
    | ("OnInput", handler) => Attr.on_input(on_input(mvu, handler))
    | ("OnChange", handler) => Attr.on_change(on_input(mvu, handler))

    // === Generic attributes (escape hatches) ===
    | ("Create", p) =>
      switch (of_pair(p)) {
      | Some((k, v)) => Attr.create(k, v)
      | None => attr_err(d)
      }
    | ("BoolAttr", p) =>
      switch (of_string_bool_pair(p)) {
      | Some((k, true)) => Attr.create(k, k)
      | Some((_, false)) => Attr.empty
      | None => attr_err(d)
      }

    | _ => attr_err(d)
    }
  | None => attr_err(d)
  };
};

let of_error = (elide_errors: bool, mvu: t, d: DHExp.t): Node.t => {
  let d = !elide_errors ? d : Exp.empty_hole();
  mvu.view_term(d);
};

/* Tags a generic Node(tag, attrs, children) creates in the SVG namespace.
   createElement on these yields an inert HTMLUnknownElement that renders
   nothing; createElementNS is required. Ambiguous names shared with HTML
   (a, script, style) are deliberately absent and stay HTML — which means
   SVG links are not expressible; use an HTML wrapper instead.

   `title` and `desc` are the exceptions to that rule: they are shared with
   HTML but resolve to SVG here, because inside an svg subtree <title> is
   the native tooltip and charts rely on it. HTML's <title> belongs in
   <head>, which a projector never renders, so nothing real loses out. */
let svg_tags = [
  "svg",
  "g",
  "defs",
  "symbol",
  "use",
  "circle",
  "ellipse",
  "rect",
  "line",
  "polyline",
  "polygon",
  "path",
  "text",
  "tspan",
  "textPath",
  "image",
  "marker",
  "pattern",
  "mask",
  "clipPath",
  "linearGradient",
  "radialGradient",
  "stop",
  "filter",
  "feBlend",
  "feColorMatrix",
  "feComposite",
  "feDropShadow",
  "feFlood",
  "feGaussianBlur",
  "feMerge",
  "feMergeNode",
  "feMorphology",
  "feOffset",
  "feTurbulence",
  "foreignObject",
  "switch",
  "view",
  "feImage",
  "feTile",
  "desc",
  "title",
  "animate",
  "animateMotion",
  "animateTransform",
];

let is_svg_tag = (tag: string): bool => List.mem(tag, svg_tags);

let rec render_elem = (~elide_errors=false, mvu: t, d: DHExp.t): Node.t =>
  switch (of_constructor(d)) {
  | Some(x) =>
    switch (x) {
    // === Text content ===
    | ("Text", body)
        when {
          switch (body.term) {
          | Atom(String(_)) => false
          | _ => true
          };
        } =>
      of_error(elide_errors, mvu, d)
    | ("Text", {term: Atom(String(str)), _}) => Node.text(str)

    // === Primitive value display ===
    | ("Bool", {term: Atom(Bool(b)), _}) => Node.text(string_of_bool(b))
    | ("Int", {term: Atom(Int(n)), _}) =>
      switch (Bigint.to_int(n)) {
      | Some(n) => Node.text(string_of_int(n))
      | None => of_error(elide_errors, mvu, d)
      }
    | ("Float", {term: Atom(Float(f)), _}) =>
      Node.text(string_of_float(f))

    // === Utility elements ===
    | ("Br", _) => Node.br()
    | ("Hr", body) =>
      let (attrs, _) = attrs_only(mvu, body);
      Node.hr(~attrs, ());

    // === Input element ===
    | ("Input", body) =>
      let (attrs, _) = attrs_only(mvu, body);
      Node.input(~attrs, ());

    // === TextArea: (attrs, content) ===
    | ("TextArea", body) =>
      switch (attrs_and_string(mvu, body)) {
      | Some((attrs, content)) =>
        /* Property as well as attribute, for the same reason as `Value` */
        Node.textarea(
          ~attrs=attrs @ [Attr.value(content), Attr.value_prop(content)],
          [],
        )
      | None => of_error(elide_errors, mvu, d)
      }

    // === Option: (attrs, label) ===
    | ("Option", body) =>
      switch (attrs_and_string(mvu, body)) {
      | Some((attrs, label)) => Node.option(~attrs, [Node.text(label)])
      | None => of_error(elide_errors, mvu, d)
      }

    // === Img (attrs only) ===
    | ("Img", body) =>
      let (attrs, _) = attrs_only(mvu, body);
      Node.img(~attrs, ());

    // === Node: custom element (tagName, attrs, children) ===
    | ("Node", body) =>
      switch (node_body(mvu, body)) {
      | Some((tag, attrs, children)) =>
        is_svg_tag(tag)
          ? Node.create_svg(tag, ~attrs, children)
          : Node.create(tag, ~attrs, children)
      | None => of_error(elide_errors, mvu, d)
      }

    // === Standard elements with (attrs, children) ===
    // Structural
    | ("Div", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.div(~attrs, children);
    | ("Span", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.span(~attrs, children);
    | ("P", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.p(~attrs, children);
    | ("Pre", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.pre(~attrs, children);
    | ("Code", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.code(~attrs, children);
    | ("Blockquote", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.blockquote(~attrs, children);

    // Headings
    | ("H1", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.h1(~attrs, children);
    | ("H2", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.h2(~attrs, children);
    | ("H3", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.h3(~attrs, children);
    | ("H4", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.h4(~attrs, children);
    | ("H5", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.h5(~attrs, children);
    | ("H6", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.h6(~attrs, children);

    // Lists
    | ("Ul", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.ul(~attrs, children);
    | ("Ol", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.ol(~attrs, children);
    | ("Li", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.li(~attrs, children);

    // Forms
    | ("Form", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.create("form", ~attrs, children);
    | ("Label", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.label(~attrs, children);
    | ("Button", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.button(~attrs, children);
    | ("Select", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.select(~attrs, children);

    // Tables
    | ("Table", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.table(~attrs, children);
    | ("Thead", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.thead(~attrs, children);
    | ("Tbody", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.tbody(~attrs, children);
    | ("Tr", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.tr(~attrs, children);
    | ("Th", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.th(~attrs, children);
    | ("Td", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.td(~attrs, children);

    // Semantic (using Node.create since Virtual_dom may not have dedicated functions)
    | ("Header", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.header(~attrs, children);
    | ("Footer", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.footer(~attrs, children);
    | ("Nav", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.create("nav", ~attrs, children);
    | ("Main", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.main(~attrs, children);
    | ("Section", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.section(~attrs, children);
    | ("Article", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.create("article", ~attrs, children);
    | ("Aside", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.create("aside", ~attrs, children);

    // Links
    | ("A", body) =>
      let (attrs, children) = attrs_and_elems(mvu, body);
      Node.a(~attrs, children);

    // Fallback
    | (_name, _body) => of_error(elide_errors, mvu, d)
    }
  | None => of_error(elide_errors, mvu, d)
  }

// Extract (attrs, children) from a tuple body
and attrs_and_elems = (mvu: t, body: DHExp.t): (list(Attr.t), list(Node.t)) => {
  let body = strip_wrappers(body);
  switch (body.term) {
  | Tuple([attrs_exp, elems_exp]) =>
    let attrs_exp = strip_wrappers(attrs_exp);
    let elems_exp = strip_wrappers(elems_exp);
    switch (attrs_exp.term, elems_exp.term) {
    | (ListLit(attrs), ListLit(elems)) => (
        List.map(render_attr(mvu), attrs),
        List.map(render_elem(mvu), elems),
      )
    | _ => ([], [mvu.view_term(body)])
    };
  | _ => ([], [mvu.view_term(body)])
  };
}

// Extract attrs from a list body (for elements that don't take children)
and attrs_only = (mvu: t, body: DHExp.t): (list(Attr.t), unit) => {
  let body = strip_wrappers(body);
  switch (body.term) {
  | ListLit(attrs) => (List.map(render_attr(mvu), attrs), ())
  | _ => ([], ())
  };
}

// Extract (attrs, string) from a tuple body (for TextArea, Option)
and attrs_and_string =
    (mvu: t, body: DHExp.t): option((list(Attr.t), string)) => {
  let body = strip_wrappers(body);
  switch (body.term) {
  | Tuple([attrs_exp, str_exp]) =>
    let attrs_exp = strip_wrappers(attrs_exp);
    let str_exp = strip_wrappers(str_exp);
    switch (attrs_exp.term, str_exp.term) {
    | (ListLit(attrs), Atom(String(s))) =>
      Some((List.map(render_attr(mvu), attrs), s))
    | _ => None
    };
  | _ => None
  };
}

// Extract (tagName, attrs, children) for custom Node element
and node_body =
    (mvu: t, body: DHExp.t): option((string, list(Attr.t), list(Node.t))) => {
  let body = strip_wrappers(body);
  switch (body.term) {
  | Tuple([tag_exp, attrs_exp, elems_exp]) =>
    let tag_exp = strip_wrappers(tag_exp);
    let attrs_exp = strip_wrappers(attrs_exp);
    let elems_exp = strip_wrappers(elems_exp);
    switch (tag_exp.term, attrs_exp.term, elems_exp.term) {
    | (Atom(String(tag)), ListLit(attrs), ListLit(elems)) =>
      Some((
        tag,
        List.map(render_attr(mvu), attrs),
        List.map(render_elem(mvu), elems),
      ))
    | _ => None
    };
  | _ => None
  };
};

// Render-only: subscriptions are owned and reconciled by the web-side
// AppStore update path, never at render time.
let go = (mvu: t, html: DHExp.t): Node.t => {
  let attrs = [Attr.tabindex(2), Attr.classes(["MVU-render"])];
  Node.div(~attrs, [render_elem(mvu, html)]);
};
