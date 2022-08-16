open Virtual_dom.Vdom;
open Node;
open Util.Web;

let ty_view = (cls: string, s: string): Node.t =>
  div([clss(["typ-view", cls])], [text(s)]);

let prov_view: Core.Typ.Typ_syntax.type_provenance => Node.t =
  fun
  | Internal => div([], [])
  | TypeHole => div([clss(["typ-mod", "type-hole"])], [text("𝜏")])
  | SynSwitch => div([clss(["typ-mod", "syn-switch"])], [text("⇒")]);

let rec view = (ty: Core.Typ.t): Node.t =>
  //TODO(andrew): parens on ops when ambiguous
  switch (Core.Typ.to_syntax(ty)) {
  | Unknown(prov) =>
    div(
      [clss(["typ-view", "atom", "unknown"])],
      [text("?"), prov_view(prov)],
    )
  | Int => ty_view("Int", "Int")
  | Float => ty_view("Float", "Float")
  | Bool => ty_view("Bool", "Bool")
  | List(t) =>
    div(
      [clss(["typ-view", "atom", "List"])],
      [text("["), view(Core.Typ.of_syntax(t)), text("]")],
    )
  | Arrow(t1, t2) =>
    div(
      [clss(["typ-view", "Arrow"])],
      [
        view(Core.Typ.of_syntax(t1)),
        text("->"),
        view(Core.Typ.of_syntax(t2)),
      ],
    )
  | Prod([]) => div([clss(["typ-view", "Prod"])], [text("Unit")])
  | Prod([_]) => div([clss(["typ-view", "Prod"])], [text("BadProduct")])
  | Prod([t0, ...ts]) =>
    div(
      [clss(["typ-view", "atom", "Prod"])],
      [
        text("("),
        div(
          [clss(["typ-view", "Prod"])],
          [view(Core.Typ.of_syntax(t0))]
          @ (
            List.map(t => [text(","), view(Core.Typ.of_syntax(t))], ts)
            |> List.flatten
          ),
        ),
        text(")"),
      ],
    )
  | TyVar(_) => assert(false)
  };
