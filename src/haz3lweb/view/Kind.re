open Virtual_dom.Vdom;
open Node;
open Util.Web;

// TODO: (poly) Finish the skeleton
let kind_view = (cls: string, s: string): Node.t =>
  div(~attr=clss(["kind-view", cls]), [text(s)]);

// TODO: (poly) Finish the skeleton
let view = (ty: Haz3lcore.Kind.t): Node.t =>
  switch (ty) {
  | Singleton(_ty) =>
    div(
      ~attr=clss(["kind-view"]),
      [text("S"), kind_view("Singleton", "Singleton")],
    )
  };
