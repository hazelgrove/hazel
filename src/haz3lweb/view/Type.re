open Virtual_dom.Vdom;
open Node;
open Util.Web;

let ty_view = (cls: string, s: string): Node.t =>
  div(~attr=clss(["typ-view", cls]), [text(s)]);

let prov_view: Haz3lcore.Typ.type_provenance => Node.t =
  fun
  | Internal => div([])
  | TypeHole => div(~attr=clss(["typ-mod", "type-hole"]), [text("𝜏")])
  | SynSwitch => div(~attr=clss(["typ-mod", "syn-switch"]), [text("⇒")]);

let rec view = (ty: Haz3lcore.Typ.t): Node.t =>
  //TODO: parens on ops when ambiguous
  switch (ty) {
  | Unknown(prov) =>
    div(
      ~attr=clss(["typ-view", "atom", "unknown"]),
      [text("?"), prov_view(prov)],
    )
  | Int => ty_view("Int", "Int")
  | Float => ty_view("Float", "Float")
  | String => ty_view("String", "String")
  | Bool => ty_view("Bool", "Bool")
  | Var({ann: name, _}) => ty_view("Var", name)
  | Rec({item: t, ann: x}) =>
    div(
      ~attr=clss(["typ-view", "Rec"]),
      [text("Rec " ++ x ++ ". "), view(t)],
    )
  | Forall({item: t, ann: x}) =>
    div(
      ~attr=clss(["typ-view", "Forall"]),
      [text("Forall " ++ x ++ " -> "), view(t)],
    )
  | List(t) =>
    div(
      ~attr=clss(["typ-view", "atom", "List"]),
      [text("["), view(t), text("]")],
    )
  | Arrow(t1, t2) =>
    div(
      ~attr=clss(["typ-view", "Arrow"]),
      [view(t1), text(" -> "), view(t2)],
    )
  | Prod([]) => div(~attr=clss(["typ-view", "Prod"]), [text("Unit")])
  | Prod([_]) =>
    div(~attr=clss(["typ-view", "Prod"]), [text("Singleton Product")])
  | Prod([t0, ...ts]) =>
    div(
      ~attr=clss(["typ-view", "atom", "Prod"]),
      [
        text("("),
        div(
          ~attr=clss(["typ-view", "Prod"]),
          [view(t0)]
          @ (List.map(t => [text(", "), view(t)], ts) |> List.flatten),
        ),
        text(")"),
      ],
    )
  | LabelSum(ts) =>
    div(
      ~attr=clss(["typ-view", "LabelSum"]),
      switch (ts) {
      | [] => [text("Nullary Sum")]
      | [t0] => [text("sum ")] @ tagged_view(t0) @ [text(" end")]
      | [t0, ...ts] =>
        let ts_views =
          List.map(t => [text(" + ")] @ tagged_view(t), ts) |> List.flatten;
        [text("sum ")] @ tagged_view(t0) @ ts_views @ [text(" end")];
      },
    )
  | Sum(t1, t2) =>
    div(
      ~attr=clss(["typ-view", "Sum"]),
      [view(t1), text(" + "), view(t2)],
    )
  }
and tagged_view = (t: Haz3lcore.Typ.tagged) =>
  switch (t.typ) {
  | None => [text(t.tag)]
  | Some(typ) => [text(t.tag ++ "("), view(typ), text(")")]
  };

let view_entry = (name, typ) => [
  text(name),
  text(" "),
  text(":"),
  view(typ),
];
