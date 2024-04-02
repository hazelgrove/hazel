open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Haz3lcore;

let ty_view = (cls: string, s: string): Node.t =>
  div(~attr=clss(["typ-view", cls]), [text(s)]);

let alias_view = (s: string): Node.t =>
  div(~attr=clss(["typ-alias-view"]), [text(s)]);

let rec view_ty = (ty: Haz3lcore.Typ.t): Node.t =>
  //TODO: parens on ops when ambiguous
  switch (Typ.term_of(ty)) {
  | Unknown(prov) =>
    div(
      ~attr=
        Attr.many([
          clss(["typ-view", "atom", "unknown"]),
          Attr.title(Typ.show_type_provenance(prov)),
        ]),
      [text("?") /*, prov_view(prov)*/],
    )
  | Parens(ty) => view_ty(ty)
  | Int => ty_view("Int", "Int")
  | Float => ty_view("Float", "Float")
  | String => ty_view("String", "String")
  | Bool => ty_view("Bool", "Bool")
  | Var(name) => ty_view("Var", name)
  | Rec({term: Var(x), _}, t) =>
    div(
      ~attr=clss(["typ-view", "Rec"]),
      [text("Rec " ++ x ++ ". "), view_ty(t)],
    )
  | Rec(_, t) =>
    div(
      ~attr=clss(["typ-view", "Rec"]),
      [text("Rec " ++ "?" ++ ". "), view_ty(t)],
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
  | Prod([]) => div(~attr=clss(["typ-view", "Prod"]), [text("()")])
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
      | [t0] => [text("+")] @ ctr_view(t0)
      | [t0, ...ts] =>
        let ts_views =
          List.map(t => [text(" + ")] @ ctr_view(t), ts) |> List.flatten;
        ctr_view(t0) @ ts_views;
      },
    )
  | Ap(_) => failwith("type application in view")
  }
and ctr_view =
  fun
  | Variant(ctr, _, None) => [text(ctr)]
  | Variant(ctr, _, Some(typ)) => [
      text(ctr ++ "("),
      view_ty(typ),
      text(")"),
    ]
  | BadEntry(typ) => [view_ty(typ)];

let view = (ty: Haz3lcore.Typ.t): Node.t =>
  div_c("typ-wrapper", [view_ty(ty)]);
