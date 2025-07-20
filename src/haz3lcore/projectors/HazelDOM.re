open Virtual_dom.Vdom;
open Util;
open Language;
open IdTagged.FreshGrammar;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  model: DHExp.t,
  inject: DHExp.t => Ui_effect.t(unit),
  view_term: DHExp.t => Node.t,
};

let input_type_mappings: list((string, string)) = [
  ("Button", "button"),
  ("Checkbox", "checkbox"),
  ("Radio", "radio"),
  ("Range", "range"),
];

let of_constructor = (d: DHExp.t): option((string, DHExp.t)) =>
  switch (d.term) {
  | Ap(Forward, {term: Constructor(name, _), _}, body) =>
    Some((name, body))
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

let on_ = (mvu: t, handler, _evt) => {
  let new_model = evaluate(Exp.ap(Forward, handler, mvu.model));
  Effect.Many([Effect.Stop_propagation, mvu.inject(new_model)]);
};

let on_input = (mvu: t, handler, _evt, arg) => {
  let new_model =
    evaluate(
      Exp.ap(Forward, handler, Exp.tuple([mvu.model, Exp.string(arg)])),
    );
  Effect.Many([Effect.Stop_propagation, mvu.inject(new_model)]);
};

let render_attr = (mvu: t, d: DHExp.t): Attr.t => {
  let attr_err = (d: DHExp.t) => {
    prerr_endline("render_attr: " ++ DHExp.show(d));
    Attr.empty;
  };
  switch (of_constructor(d)) {
  | Some(x) =>
    switch (x) {
    | ("Create", p) =>
      switch (of_pair(p)) {
      | Some((k, v)) => Attr.create(k, v)
      | None => attr_err(d)
      }
    | ("Style", {term: ListLit(styles), _}) => render_styles(styles)
    | ("OnClick", handler) => Attr.on_click(on_(mvu, handler))
    | ("OnMousedown", handler) => Attr.on_mousedown(on_(mvu, handler))
    | ("OnInput", handler) => Attr.on_input(on_input(mvu, handler))
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
    | ("Text", {term: Atom(String(str)), _}) => Node.text(str)
    | ("Bool", {term: Atom(Bool(b)), _}) => Node.text(string_of_bool(b))
    | ("Int", {term: Atom(Int(n)), _}) =>
      switch (Bigint.to_int(n)) {
      | Some(n) => Node.text(string_of_int(n))
      | None => of_error(elide_errors, mvu, d)
      }
    | ("Float", {term: Atom(Float(f)), _}) =>
      Node.text(string_of_float(f))
    | ("Div", body) =>
      let (attrs, divs) = attrs_and_elems(mvu, body);
      Node.div(~attrs, divs);
    | ("Span", body) =>
      let (attrs, divs) = attrs_and_elems(mvu, body);
      Node.span(~attrs, divs);
    | (constructor_name, body) =>
      switch (List.assoc_opt(constructor_name, input_type_mappings)) {
      | Some(input_type) => input_of(input_type, mvu, body)
      | None => of_error(elide_errors, mvu, d)
      }
    }
  | _ => of_error(elide_errors, mvu, d)
  }
and input_of = (input_type: string, mvu: t, body: DHExp.t) => {
  let (attrs, _divs) = attrs_and_elems(mvu, body);
  Node.input(~attrs=[Attr.create("type", input_type)] @ attrs, ());
}
and attrs_and_elems = (mvu: t, body: DHExp.t): (list(Attr.t), list(Node.t)) =>
  switch (DHExp.strip_ascriptions(body).term) {
  | Tuple([{term: ListLit(attrs), _}, {term: ListLit(divs), _}]) => (
      List.map(render_attr(mvu), attrs),
      List.map(render_elem(mvu), divs),
    )
  | _ => ([], [mvu.view_term(body)])
  };

let go = (mvu: t): Node.t => {
  let attrs = [Attr.tabindex(2), Attr.classes(["MVU-render"])];
  Node.div(~attrs, [render_elem(mvu, mvu.model)]);
};
