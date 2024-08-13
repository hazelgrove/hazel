open Virtual_dom.Vdom;
open Node;
open Util.Web;

let view = (~globals, kind: Haz3lcore.Ctx.kind): Node.t =>
  switch (kind) {
  | Singleton(ty) =>
    div_c(
      "kind-view",
      [
        CodeViewable.view_typ(
          ~globals,
          ~inline=true,
          ~info_map=Haz3lcore.Id.Map.empty,
          ty,
        ),
      ],
    )
  | Abstract => div_c("kind-view", [text("Type")])
  };
