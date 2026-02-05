open Virtual_dom.Vdom;
open Util;
open Language;
open IdTagged.FreshGrammar;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  model: DHExp.t,
  inject: DHExp.t => Ui_effect.t(unit),
  view_term: DHExp.t => Node.t,
  // Optional: projector ID for subscription tracking
  projector_id: option(Id.t),
  // Optional: subscriptions to set up (Sub expression)
  subscriptions: option(DHExp.t),
};

// Global registry of active subscriptions per projector
let active_subscriptions: Hashtbl.t(Id.t, SubManager.active_subs) =
  Hashtbl.create(16);

// Legacy input types that render as <input type="...">
let input_type_mappings: list((string, string)) = [
  ("Checkbox", "checkbox"),
  ("Radio", "radio"),
  ("Range", "range"),
];

let of_constructor = (d: DHExp.t): option((string, DHExp.t)) =>
  switch (d.term) {
  | Ap(Forward, {term: Constructor(name, _), _}, body) =>
    Some((name, body))
  | Constructor(name, _) =>
    // Nullary constructor - create an empty tuple as placeholder body
    Some((
      name,
      {
        ...d,
        term: Tuple([]),
      },
    ))
  | _ => None
  };

let of_pair = (d: DHExp.t): option((string, string)) =>
  switch (d.term) {
  | Parens({
      term:
        Tuple([{term: Atom(String(k)), _}, {term: Atom(String(v)), _}]),
      _,
    }) =>
    Some((k, v))
  | Tuple([{term: Atom(String(k)), _}, {term: Atom(String(v)), _}]) =>
    Some((k, v))
  | _ => None
  };

let of_string_bool_pair = (d: DHExp.t): option((string, bool)) =>
  switch (d.term) {
  | Parens({
      term: Tuple([{term: Atom(String(k)), _}, {term: Atom(Bool(v)), _}]),
      _,
    }) =>
    Some((k, v))
  | Tuple([{term: Atom(String(k)), _}, {term: Atom(Bool(v)), _}]) =>
    Some((k, v))
  | _ => None
  };

let of_string = (d: DHExp.t): option(string) =>
  switch (d.term) {
  | Atom(String(s)) => Some(s)
  | Parens({term: Atom(String(s)), _}) => Some(s)
  | _ => None
  };

let of_bool = (d: DHExp.t): option(bool) =>
  switch (d.term) {
  | Atom(Bool(b)) => Some(b)
  | Parens({term: Atom(Bool(b)), _}) => Some(b)
  | _ => None
  };

let of_int = (d: DHExp.t): option(int) =>
  switch (d.term) {
  | Atom(Int(n)) => Bigint.to_int(n)
  | Parens({term: Atom(Int(n)), _}) => Bigint.to_int(n)
  | _ => None
  };

let of_string_list = (d: DHExp.t): option(list(string)) =>
  switch (d.term) {
  | ListLit(items) =>
    let strings = List.filter_map(of_string, items);
    if (List.length(strings) == List.length(items)) {
      Some(strings);
    } else {
      None;
    };
  | _ => None
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

// copy-pasted from CLI/Run.re
let evaluate = exp =>
  fst(
    Evaluator.evaluate(
      ~env=Builtins.env_init,
      fst(
        Elaborator.elaborate(
          Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), exp),
          exp,
        ),
      ),
    ),
  );

// === Event handlers ===

// Process handler result: either Html or (Html, Cmd)
// Returns (new_model, optional_cmd_effect)
let process_handler_result = (mvu: t, result: DHExp.t): Ui_effect.t(unit) => {
  // Check if result is a tuple (Html, Cmd)
  switch (result.term) {
  | Tuple([new_model, cmd]) =>
    // Result is (Html, Cmd) - run the command
    let cmd_ctx: CmdRunner.context = {
      model: new_model,
      inject: mvu.inject,
    };
    let cmd_effect = CmdRunner.run(cmd_ctx, cmd);
    Effect.Many([
      Effect.Stop_propagation,
      mvu.inject(new_model),
      cmd_effect,
    ]);
  | Parens({term: Tuple([new_model, cmd]), _}) =>
    let cmd_ctx: CmdRunner.context = {
      model: new_model,
      inject: mvu.inject,
    };
    let cmd_effect = CmdRunner.run(cmd_ctx, cmd);
    Effect.Many([
      Effect.Stop_propagation,
      mvu.inject(new_model),
      cmd_effect,
    ]);
  | _ =>
    // Result is just Html
    Effect.Many([Effect.Stop_propagation, mvu.inject(result)])
  };
};

// Simple event: Html -> Html or Html -> (Html, Cmd)
let on_ = (mvu: t, handler, _evt) => {
  let result = evaluate(Exp.ap(Forward, handler, mvu.model));
  process_handler_result(mvu, result);
};

// Input/change event: (Html, String) -> Html or (Html, String) -> (Html, Cmd)
let on_input = (mvu: t, handler, _evt, arg) => {
  let result =
    evaluate(
      Exp.ap(Forward, handler, Exp.tuple([mvu.model, Exp.string(arg)])),
    );
  process_handler_result(mvu, result);
};

// Mouse event: (Html, MouseEvent) -> Html or -> (Html, Cmd)
// MouseEvent = (clientX, clientY, button, ctrl, shift, alt, meta)
let on_mouse = (mvu: t, handler, evt) => {
  let mouse_event =
    Exp.tuple([
      Exp.float(float_of_int(evt##.clientX)),
      Exp.float(float_of_int(evt##.clientY)),
      Exp.int(evt##.button),
      Exp.bool(Js_of_ocaml.Js.to_bool(evt##.ctrlKey)),
      Exp.bool(Js_of_ocaml.Js.to_bool(evt##.shiftKey)),
      Exp.bool(Js_of_ocaml.Js.to_bool(evt##.altKey)),
      Exp.bool(Js_of_ocaml.Js.to_bool(evt##.metaKey)),
    ]);
  let result =
    evaluate(Exp.ap(Forward, handler, Exp.tuple([mvu.model, mouse_event])));
  process_handler_result(mvu, result);
};

// Keyboard event: (Html, KeyEvent) -> Html or -> (Html, Cmd)
// KeyEvent = (key, code, ctrl, shift, alt, meta)
let on_key = (mvu: t, handler, evt) => {
  open Js_of_ocaml;
  let get_key = evt =>
    Js.to_string(Js.Optdef.get(evt##.key, () => Js.string("")));
  let get_code = evt =>
    Js.to_string(Js.Optdef.get(evt##.code, () => Js.string("")));
  let key_event =
    Exp.tuple([
      Exp.string(get_key(evt)),
      Exp.string(get_code(evt)),
      Exp.bool(Js.to_bool(evt##.ctrlKey)),
      Exp.bool(Js.to_bool(evt##.shiftKey)),
      Exp.bool(Js.to_bool(evt##.altKey)),
      Exp.bool(Js.to_bool(evt##.metaKey)),
    ]);
  let result =
    evaluate(Exp.ap(Forward, handler, Exp.tuple([mvu.model, key_event])));
  process_handler_result(mvu, result);
};

// === Attribute rendering ===

let render_attr = (mvu: t, d: DHExp.t): Attr.t => {
  let attr_err = (d: DHExp.t) => {
    prerr_endline("render_attr: " ++ DHExp.show(d));
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
      | Some(s) => Attr.value(s)
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

    // === Simple event handlers: Html -> Html ===
    | ("OnClick", handler) => Attr.on_click(on_(mvu, handler))
    | ("OnDoubleClick", handler) => Attr.on_double_click(on_(mvu, handler))
    | ("OnMouseEnter", handler) => Attr.on_mouseenter(on_(mvu, handler))
    | ("OnMouseLeave", handler) => Attr.on_mouseleave(on_(mvu, handler))
    | ("OnFocus", handler) => Attr.on_focus(_ => on_(mvu, handler, ()))
    | ("OnBlur", handler) => Attr.on_blur(_ => on_(mvu, handler, ()))
    // OnSubmit: For now, we don't have a generic Attr.on for submit events
    // This is a limitation - form submission requires special handling
    | ("OnSubmit", _handler) => Attr.empty

    // === Mouse event handlers: (Html, MouseEvent) -> Html ===
    | ("OnMouseDown", handler) => Attr.on_mousedown(on_mouse(mvu, handler))
    | ("OnMouseUp", handler) => Attr.on_mouseup(on_mouse(mvu, handler))
    | ("OnMouseMove", handler) => Attr.on_mousemove(on_mouse(mvu, handler))

    // === Keyboard event handlers: (Html, KeyEvent) -> Html ===
    | ("OnKeyDown", handler) => Attr.on_keydown(on_key(mvu, handler))
    | ("OnKeyUp", handler) => Attr.on_keyup(on_key(mvu, handler))
    | ("OnKeyPress", handler) => Attr.on_keypress(on_key(mvu, handler))

    // === Input event handlers: (Html, String) -> Html ===
    | ("OnInput", handler) => Attr.on_input(on_input(mvu, handler))
    | ("OnChange", handler) => Attr.on_change(on_input(mvu, handler))

    // === Legacy/generic attributes ===
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

    // === Legacy event (backwards compat) ===
    | ("OnMousedown", handler) => Attr.on_mousedown(on_(mvu, handler))

    | _ => attr_err(d)
    }
  | None => attr_err(d)
  };
};

let of_error = (elide_errors: bool, mvu: t, d: DHExp.t): Node.t => {
  let d = !elide_errors ? d : Exp.empty_hole();
  mvu.view_term(d);
};

let rec render_elem = (~elide_errors=false, mvu: t, d: DHExp.t): Node.t =>
  switch (of_constructor(d)) {
  | Some(x) =>
    switch (x) {
    // === Text content ===
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
        Node.textarea(~attrs=attrs @ [Attr.value(content)], [])
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

    // === Legacy input types ===
    | (constructor_name, body)
        when List.mem_assoc(constructor_name, input_type_mappings) =>
      let input_type = List.assoc(constructor_name, input_type_mappings);
      let (attrs, _) = attrs_only(mvu, body);
      Node.input(~attrs=[Attr.type_(input_type), ...attrs], ());

    // === Node: custom element (tagName, attrs, children) ===
    | ("Node", body) =>
      switch (node_body(mvu, body)) {
      | Some((tag, attrs, children)) => Node.create(tag, ~attrs, children)
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
    | _ => of_error(elide_errors, mvu, d)
    }
  | _ => of_error(elide_errors, mvu, d)
  }

// Extract (attrs, children) from a tuple body
and attrs_and_elems = (mvu: t, body: DHExp.t): (list(Attr.t), list(Node.t)) =>
  switch (DHExp.strip_ascriptions(body).term) {
  | Tuple([{term: ListLit(attrs), _}, {term: ListLit(elems), _}]) => (
      List.map(render_attr(mvu), attrs),
      List.map(render_elem(mvu), elems),
    )
  | Parens({
      term: Tuple([{term: ListLit(attrs), _}, {term: ListLit(elems), _}]),
      _,
    }) => (
      List.map(render_attr(mvu), attrs),
      List.map(render_elem(mvu), elems),
    )
  | _ => ([], [mvu.view_term(body)])
  }

// Extract attrs from a list body (for elements that don't take children)
and attrs_only = (mvu: t, body: DHExp.t): (list(Attr.t), unit) =>
  switch (DHExp.strip_ascriptions(body).term) {
  | ListLit(attrs) => (List.map(render_attr(mvu), attrs), ())
  | Parens({term: ListLit(attrs), _}) => (
      List.map(render_attr(mvu), attrs),
      (),
    )
  | _ => ([], ())
  }

// Extract (attrs, string) from a tuple body (for TextArea, Option)
and attrs_and_string =
    (mvu: t, body: DHExp.t): option((list(Attr.t), string)) =>
  switch (DHExp.strip_ascriptions(body).term) {
  | Tuple([{term: ListLit(attrs), _}, {term: Atom(String(s)), _}]) =>
    Some((List.map(render_attr(mvu), attrs), s))
  | Parens({
      term:
        Tuple([{term: ListLit(attrs), _}, {term: Atom(String(s)), _}]),
      _,
    }) =>
    Some((List.map(render_attr(mvu), attrs), s))
  | _ => None
  }

// Extract (tagName, attrs, children) for custom Node element
and node_body =
    (mvu: t, body: DHExp.t): option((string, list(Attr.t), list(Node.t))) =>
  switch (DHExp.strip_ascriptions(body).term) {
  | Tuple([
      {term: Atom(String(tag)), _},
      {term: ListLit(attrs), _},
      {term: ListLit(elems), _},
    ]) =>
    Some((
      tag,
      List.map(render_attr(mvu), attrs),
      List.map(render_elem(mvu), elems),
    ))
  | Parens({
      term:
        Tuple([
          {term: Atom(String(tag)), _},
          {term: ListLit(attrs), _},
          {term: ListLit(elems), _},
        ]),
      _,
    }) =>
    Some((
      tag,
      List.map(render_attr(mvu), attrs),
      List.map(render_elem(mvu), elems),
    ))
  | _ => None
  };

// Manage subscriptions for a projector
let manage_subscriptions = (mvu: t): unit => {
  switch (mvu.projector_id, mvu.subscriptions) {
  | (Some(id), Some(sub_exp)) =>
    // Clean up existing subscriptions for this projector
    switch (Hashtbl.find_opt(active_subscriptions, id)) {
    | Some(handles) =>
      SubManager.cleanup(handles);
      Hashtbl.remove(active_subscriptions, id);
    | None => ()
    };
    // Set up new subscriptions
    let get_model = () => mvu.model;
    let ctx: SubManager.context = {
      model: mvu.model,
      inject: mvu.inject,
    };
    let handles = SubManager.subscribe(ctx, sub_exp, get_model);
    if (List.length(handles) > 0) {
      Hashtbl.replace(active_subscriptions, id, handles);
    };
  | (Some(id), None) =>
    // No subscriptions - clean up any existing ones
    switch (Hashtbl.find_opt(active_subscriptions, id)) {
    | Some(handles) =>
      SubManager.cleanup(handles);
      Hashtbl.remove(active_subscriptions, id);
    | None => ()
    }
  | (None, _) => () // No projector ID, can't track subscriptions
  };
};

let go = (mvu: t): Node.t => {
  // Manage subscriptions before rendering
  manage_subscriptions(mvu);
  let attrs = [Attr.tabindex(2), Attr.classes(["MVU-render"])];
  Node.div(~attrs, [render_elem(mvu, mvu.model)]);
};
