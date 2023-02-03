open Virtual_dom.Vdom;
open Node;
open Util.Web;

let view = (kind: Haz3lcore.Kind.t): Node.t =>
  switch (kind) {
  | Singleton(ty) => div_c("kind-view", [Type.view(ty)])
  | Abstract => div_c("kind-view", [text("Type")])
  };
