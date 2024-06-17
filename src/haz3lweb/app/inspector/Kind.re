open Virtual_dom.Vdom;
open Node;
open Util.Web;

let view = (~globals, kind: Haz3lcore.Ctx.kind): Node.t =>
  switch (kind) {
  | Singleton(ty) =>
    div_c("kind-view", [CodeViewable.view_typ(~globals, ~inline=true, ty)])
  | Abstract => div_c("kind-view", [text("Type")])
  };
