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

let rec view_ty =
        (
          ~font_metrics: option(FontMetrics.t)=None,
          ~with_cls: bool=true,
          ~is_left_child: bool=false,
          ty: Haz3lcore.Typ.t,
        )
        : Node.t => {
  //TODO: parens on ops when ambiguous
  let parenthesize_if_left_child = (n): Node.t =>
    (is_left_child ? [Node.text("("), ...n] @ [Node.text(")")] : n) |> span;
  let div = (~attr, nodes) => with_cls ? div(~attr, nodes) : span(nodes);
  let ty_view = (cls: string, s: string): Node.t =>
    div(~attr=clss(["typ-view", cls]), [text(s)]);
  switch (ty) {
  | Unknown(_) =>
    switch (font_metrics) {
    | Some(font_metrics) =>
      div(
        ~attr=
          Attr.many([
            clss(["typ-view", "atom", "unknown"]),
            Attr.title(Typ.show_type_provenance(prov)),
          ]),
        [
          EmptyHoleDec.relative_view(
            ~font_metrics,
            false,
            Haz3lcore.InferenceResult.hole_mold,
          ),
        ],
      )
    | _ => div(~attr=clss(["typ-view", "atom", "unknown"]), [text("")])
    }
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
      [text("["), view_ty(~font_metrics, ~with_cls, t), text("]")],
    )
  | Arrow(t1, t2) =>
    [
      div(
        ~attr=clss(["typ-view", "Arrow"]),
        [
          view_ty(~font_metrics, ~with_cls, ~is_left_child=true, t1),
          text(" -> "),
          view_ty(~font_metrics, ~with_cls, t2),
        ],
      ),
    ]
    |> parenthesize_if_left_child
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
          [view_ty(~font_metrics, ~with_cls, t0)]
          @ (
            List.map(
              t => [text(", "), view_ty(~font_metrics, ~with_cls, t)],
              ts,
            )
            |> List.flatten
          ),
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
          List.map(t => [text(" + ")] @ ctr_view(~font_metrics, t), ts)
          |> List.flatten;
        ctr_view(t0) @ ts_views;
      },
    )
  };
}
and ctr_view = (~font_metrics, (ctr, typ)) =>
  switch (typ) {
  | None => [text(ctr)]
  | Some(typ) => [text(ctr ++ "("), view_ty(~font_metrics, typ), text(")")]
  };

let view = (ty: Haz3lcore.Typ.t): Node.t =>
  div_c("typ-wrapper", [view_ty(ty)]);
