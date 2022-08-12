open Virtual_dom.Vdom;
open Node;
open Util.Web;

let ty_view = (cls: string, s: string): Node.t =>
  div([clss(["typ-view", cls])], [text(s)]);

let rec view = (ty: Core.Typ.t): Node.t =>
  //TODO(andrew): parens on ops when ambiguous
  switch (ty) {
  | Unknown(Internal) => ty_view("unknown", "?")
  | Unknown(TypeHole) => ty_view("unknown", "?[𝜏]")
  | Unknown(SynSwitch) => ty_view("unknown", "?[⇒]")
  | Unit => ty_view("()", "()")
  | Int => ty_view("Int", "Int")
  | Float => ty_view("Float", "Float")
  | Bool => ty_view("Bool", "Bool")
  | List(t) =>
    div([clss(["typ-view", "List"])], [text("["), view(t), text("]")])
  | Arrow(t1, t2) =>
    div([clss(["typ-view", "Arrow"])], [view(t1), text("->"), view(t2)])
  | Prod(t1, t2) =>
    div([clss(["typ-view", "Prod"])], [view(t1), text(","), view(t2)])
  };
