open Virtual_dom.Vdom;
open Node;
open Util.Web;

let ty_view = (cls: string, s: string): Node.t =>
  div(~attr=clss(["typ-view", cls]), [text(s)]);

let alias_view = (s: string): Node.t =>
  div(~attr=clss(["typ-alias-view"]), [text(s)]);

let prov_view: Haz3lcore.Typ.type_provenance => Node.t =
  fun
  | Internal => div([])
  | TypeHole => div(~attr=clss(["typ-mod", "type-hole"]), [text("𝜏")])
  | SynSwitch => div(~attr=clss(["typ-mod", "syn-switch"]), [text("⇒")])
  | EmptyExp => div(~attr=clss(["typ-mod", "empty-exp"]), [text("e🕳")])
  | Err => div(~attr=clss(["typ-mod", "type-hole"]), [text("⚠")])
  | EmptyPat => div(~attr=clss(["typ-mod", "empty-pat"]), [text("p🕳")])
  | PatVar => div(~attr=clss(["typ-mod", "pat-var"]), [text("𝑥")])
  | EmptyList => div(~attr=clss(["typ-mod", "empty-list"]), [text("[]")])
  | EmptyJoin => div(~attr=clss(["typ-mod", "empty-join"]), [text("∅")])
  | TagShit => div(~attr=clss(["typ-mod", "tagshit"]), [text("💩")]);

let rec view_ty = (ty: Haz3lcore.Typ.t): Node.t =>
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
  | Var(name) => ty_view("Var", name)
  | Rec(x, t) =>
    div(
      ~attr=clss(["typ-view", "Rec"]),
      [text("Rec " ++ x ++ ". "), view_ty(t)],
    )
  | List(t) =>
    div(
      ~attr=clss(["typ-view", "atom", "List"]),
      [text("["), view_ty(t), text("]")],
    )
  | Arrow(t1, t2) =>
    div(
      ~attr=clss(["typ-view", "Arrow"]),
      [view_ty(t1), text(" -> "), view_ty(t2)],
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
          [view_ty(t0)]
          @ (List.map(t => [text(", "), view_ty(t)], ts) |> List.flatten),
        ),
        text(")"),
      ],
    )
  | Sum(ts) =>
    div(
      ~attr=clss(["typ-view", "Sum"]),
      switch (ts) {
      | [] => [text("Nullary Sum")]
      | [t0] => [text("Singleton Sum {")] @ tagged_view(t0) @ [text("}")]
      | [t0, ...ts] =>
        let ts_views =
          List.map(t => [text(" + ")] @ tagged_view(t), ts) |> List.flatten;
        tagged_view(t0) @ ts_views;
      },
    )
  }
and tagged_view = ((tag, typ)) =>
  switch (typ) {
  | None => [text(tag)]
  | Some(typ) => [text(tag ++ "("), view_ty(typ), text(")")]
  };

let view = (ty: Haz3lcore.Typ.t): Node.t =>
  div_c("typ-wrapper", [view_ty(ty)]);
