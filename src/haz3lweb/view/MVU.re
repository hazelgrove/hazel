open Virtual_dom.Vdom;
open Haz3lcore;
open Util;
open OptUtil.Syntax;

type t = {
  name: string, // key to store model state
  inject: UpdateAction.t => Ui_effect.t(unit),
  update: DHExp.t,
  model: DHExp.t,
  view: DHExp.t,
  font_metrics: FontMetrics.t,
};

let mk = (~name, ~inject, ~update, ~model, ~view, ~font_metrics) => {
  name,
  inject,
  update,
  model,
  view,
  font_metrics,
};

let dhexp_view = (~font_metrics, d) =>
  DHCode.view_tylr(
    ~settings={
      evaluate: true,
      show_case_clauses: true,
      show_fn_bodies: true,
      show_casts: true,
      show_unevaluated_elaboration: false,
    },
    ~selected_hole_instance=None,
    ~font_metrics,
    ~width=80,
    d,
  );

let eval = d =>
  switch (Interface.evaluate(d)) {
  | (result, _, _) => EvaluatorResult.unbox(result)
  };

let render_attr =
    ({name, inject, update, model, _}: t, d: DHExp.t): option(Attr.t) =>
  switch (d) {
  | Ap(Tag("Create"), Tuple([StringLit(name), StringLit(value)])) =>
    Some(Attr.create(Form.strip_quotes(name), Form.strip_quotes(value)))
  | Ap(Tag("OnClick"), click_handler) =>
    Attr.on_click(_ => {
      //print_endline("ONCLICK EXECUTING");
      let maybe_action = eval(Ap(click_handler, Tuple([])));
      //print_endline("maybe_action:");
      //print_endline(DHExp.show(maybe_action));
      let maybe_model = eval(Ap(update, Tuple([model, maybe_action])));
      //print_endline("maybe_model:");
      //print_endline(DHExp.show(maybe_model));
      inject(MVUSet(name, DHExp.strip_casts(maybe_model)));
    })
    |> Option.some
  | _ =>
    //print_endline("ERROR: render_attr: " ++ DHExp.show(d));
    Some(Attr.create("error", "error"))
  };

let rec render_div =
        (~elide_errors=false, context: t, d: DHExp.t): option(Node.t) =>
  switch (d) {
  | Ap(
      Tag("Div"),
      Tuple([ListLit(_, _, _, _, attrs), ListLit(_, _, _, _, divs)]),
    ) =>
    let* attrs = attrs |> List.map(render_attr(context)) |> OptUtil.sequence;
    let+ divs = divs |> List.map(render_div(context)) |> OptUtil.sequence;
    Node.div(~attr=Attr.many(attrs), divs);
  | Ap(Tag("Text"), StringLit(str)) =>
    Some(Node.text(Form.strip_quotes(str)))
  | Ap(Tag("Num"), IntLit(n)) => Some(Node.text(string_of_int(n)))
  | _ =>
    //print_endline("ERROR: render_div: " ++ DHExp.show(d));
    let d = elide_errors ? DHExp.EmptyHole(0, 0) : d;
    Some(dhexp_view(~font_metrics=context.font_metrics, d));
  //Some(Node.text("error"))
  };

let go = (context: t) => {
  let result = eval(Ap(context.view, context.model));
  //print_endline("RESULT:");
  //print_endline(DHExp.show(result));
  let node =
    switch (render_div(context, result)) {
    | Some(node) =>
      //print_endline("context.view: " ++ DHExp.show(context.view));
      node
    | None => Node.text("ERROR: render_div returned None")
    };
  [
    Node.div(
      ~attr=Attr.classes(["mvu-render"]),
      [Node.text("Rendered Node: "), node],
    ),
  ];
};
