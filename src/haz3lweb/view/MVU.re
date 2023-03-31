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
};

let mk = (~name, ~inject, ~update, ~model, ~view) => {
  name,
  inject,
  update,
  model,
  view,
};

let strip_quotes = s => String.sub(s, 1, String.length(s) - 2);
let eval = d =>
  switch (Interface.evaluate(d)) {
  | (result, _, _) => EvaluatorResult.unbox(result)
  };

let render_attr =
    ({name, inject, update, model, _}: t, d: DHExp.t): option(Attr.t) =>
  switch (d) {
  | Ap(Tag("Create"), Tuple([StringLit(name), StringLit(value)])) =>
    Some(Attr.create(strip_quotes(name), strip_quotes(value)))
  | Ap(Tag("OnClick"), click_handler) =>
    Attr.on_click(_ => {
      //print_endline("ONCLICK EXECUTING");
      let maybe_action = eval(Ap(click_handler, Tuple([])));
      //print_endline("maybe_action:");
      //print_endline(DHExp.show(maybe_action));
      let maybe_model = eval(Ap(update, Tuple([model, maybe_action])));
      //print_endline("maybe_model:");
      //print_endline(DHExp.show(maybe_model));
      inject(SetModel(name, DHExp.strip_casts(maybe_model)));
    })
    |> Option.some
  | _ =>
    print_endline("ERROR: render_attr: " ++ DHExp.show(d));
    None;
  };

let rec render_div = (context: t, d: DHExp.t): option(Node.t) =>
  switch (d) {
  | Ap(
      Tag("Div"),
      Tuple([ListLit(_, _, _, _, attrs), ListLit(_, _, _, _, divs)]),
    ) =>
    let* attrs = attrs |> List.map(render_attr(context)) |> OptUtil.sequence;
    let+ divs = divs |> List.map(render_div(context)) |> OptUtil.sequence;
    Node.div(~attr=Attr.many(attrs), divs);
  | Ap(Tag("Text"), StringLit(str)) => Some(Node.text(strip_quotes(str)))
  | Ap(Tag("Num"), IntLit(n)) => Some(Node.text(string_of_int(n)))
  | _ =>
    print_endline("ERROR: render_div: " ++ DHExp.show(d));
    None;
  };

let go = (context: t) => {
  print_endline("eval_result blee");
  let result = eval(Ap(context.view, context.model));
  let node =
    switch (render_div(context, result)) {
    | Some(node) => node
    | None => Node.text("ERROR: render_div returned None")
    };
  //print_endline("RESULT:");
  //print_endline(DHExp.show(result));
  [
    Node.div(
      ~attr=Attr.classes(["rendered"]),
      [Node.text("Rendered Node: "), node],
    ),
  ];
};
