open Virtual_dom.Vdom;
open Node;
open Util.Web;

let kind_view = (cls: string, s: string): Node.t =>
  div(~attr=clss(["typ-view", cls]), [text(s)]);

// TODO: (poly) add kind-view to frontend
let rec view = (kind: Haz3lcore.Kind.t): Node.t =>
  //TODO(andrew): parens on ops when ambiguous
  switch (kind) {
  | Unknown =>
    div(~attr=clss(["kind-view", "atom", "unknown"]), [text("?")])
  | Abstract =>
    div(~attr=clss(["kind-view", "atom", "abstract"]), [text("A")])
  | Singleton(ty) => div(~attr=clss(["kind-view"]), [Type.view(ty)])
  };
