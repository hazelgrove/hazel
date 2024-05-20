open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Haz3lcore;

let ty_view = (cls: string, s: string): Node.t =>
  div(~attr=clss(["typ-view", cls]), [text(s)]);

let alias_view = (s: string): Node.t =>
  div(~attr=clss(["typ-alias-view"]), [text(s)]);

let prov_view: Typ.type_provenance => Node.t =
  fun
  | Internal => div([])
  | Free(name) =>
    div(~attr=clss(["typ-mod", "free-type-var"]), [text(name)])
  | TypeHole => div(~attr=clss(["typ-mod", "type-hole"]), [text("𝜏")])
  | SynSwitch => div(~attr=clss(["typ-mod", "syn-switch"]), [text("⇒")]);

let rec view_ty = (~strip_outer_parens=false, ty: Haz3lcore.Typ.t): Node.t =>
  switch (ty) {
  | Unknown(prov) =>
    div(
      ~attr=
        Attr.many([
          clss(["typ-view", "atom", "unknown"]),
          Attr.title(Typ.show_type_provenance(prov)),
        ]),
      [text("?") /*, prov_view(prov)*/],
    )
  | Int => ty_view("Int", "Int")
  | Float => ty_view("Float", "Float")
  | String => ty_view("String", "String")
  | Bool => ty_view("Bool", "Bool")
  | Var(name) => ty_view("Var", name)
  | Rec(name, t) =>
    div(
      ~attr=clss(["typ-view", "Rec"]),
      [text("rec " ++ name ++ " -> "), view_ty(t)],
    )
  | Forall(name, t) =>
    div(
      ~attr=clss(["typ-view", "Forall"]),
      [text("forall " ++ name ++ " -> "), view_ty(t)],
    )
  | List(t) =>
    div(
      ~attr=clss(["typ-view", "atom", "List"]),
      [text("["), view_ty(t), text("]")],
    )
  | Arrow(t1, t2) =>
    div(
      ~attr=clss(["typ-view", "Arrow"]),
      paren_view(t1) @ [text(" -> "), view_ty(t2)],
    )
  | Prod([]) => div(~attr=clss(["typ-view", "Prod"]), [text("()")])
  | Prod([_]) =>
    div(~attr=clss(["typ-view", "Prod"]), [text("Singleton Product")])
  | Prod([t0, ...ts]) =>
    div(
      ~attr=clss(["typ-view", "atom", "Prod"]),
      (
        if (!strip_outer_parens) {
          [text("(")];
        } else {
          [];
        }
      )
      @ [
        div(
          ~attr=clss(["typ-view", "Prod"]),
          paren_view(t0)
          @ (
            List.map(t => [text(", "), ...paren_view(t)], ts)
            |> List.flatten
          ),
        ),
      ]
      @ (
        if (!strip_outer_parens) {
          [text(")")];
        } else {
          [];
        }
      ),
    )
  | Module({inner_ctx: [], incomplete: false}) =>
    div(~attr=clss(["typ-view", "Module"]), [text("Module")])
  | Module({inner_ctx: [], incomplete: true}) =>
    div(~attr=clss(["typ-view", "Module"]), [text("Module{...}")])
  | Module({inner_ctx: [e, ...es], incomplete}) =>
    let view_entry = (m: Haz3lcore.TypBase.Ctx.entry): list(t) => {
      switch (m) {
      | VarEntry({name: n0, typ: t0, _})
      | ConstructorEntry({name: n0, typ: t0, _}) => [
          text(n0),
          text(":"),
          view_ty(t0),
        ]
      | TVarEntry({name: n0, kind: Singleton(t0), _}) => [
          text("Type "),
          text(n0),
          text("="),
          view_ty(t0),
        ]
      | TVarEntry({name: n0, _}) => [
          text("type "),
          text(n0),
          text("="),
          view_ty(Unknown(Internal)),
        ]
      };
    };
    div(
      ~attr=clss(["typ-view", "atom", "Module"]),
      [
        text("Module{"),
        div(
          ~attr=clss(["typ-view", "Module"]),
          // let the earlier definitions to be displayed first
          (
            List.map(t => view_entry(t) @ [text(", ")], es)
            |> List.rev
            |> List.flatten
          )
          @ view_entry(e),
        ),
        text((incomplete ? "..." : "") ++ "}"),
      ],
    );
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
  | Member(name, _) => ty_view("Member", name)
  }
and ctr_view = ((ctr, typ)) =>
  switch (typ) {
  | None => [text(ctr)]
  | Some(typ) => [
      text(ctr ++ "("),
      view_ty(~strip_outer_parens=true, typ),
      text(")"),
    ]
  }
and paren_view = typ =>
  if (Typ.needs_parens(typ)) {
    [text("("), view_ty(~strip_outer_parens=true, typ), text(")")];
  } else {
    [view_ty(typ)];
  };

let view = (ty: Haz3lcore.Typ.t): Node.t =>
  div_c("typ-wrapper", [view_ty(ty)]);
