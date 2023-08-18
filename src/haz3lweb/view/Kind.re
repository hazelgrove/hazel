open Virtual_dom.Vdom;
open Node;
open Util.Web;

let view = (kind: Haz3lcore.Kind.t): Node.t =>
  switch (kind) {
  | Arrow(ty1, ty2) =>
    div_c("kind-view", [Type.view(ty1), text(" -> "), Type.view(ty2)])
  | Singleton(ty) => div_c("kind-view", [Type.view(ty)])
  | Abstract => div_c("kind-view", [text("Type")])
  };
