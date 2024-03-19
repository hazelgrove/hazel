open Virtual_dom.Vdom;
open Node;
open Util.Web;

let rec view = (kind: Haz3lcore.Kind.t): Node.t =>
  switch (kind) {
  | Arrow(ty1, ty2) =>
    div_c("kind-view", [view(ty1), text(" -> "), view(ty2)])
  | Singleton(ty) => div_c("kind-view", [Type.view(ty)])
  | Abstract => div_c("kind-view", [text("Type")])
  };
