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
          ~settings={
            inline: true,
            fold_case_clauses: false,
            fold_fn_bodies: false,
            hide_fixpoints: false,
            fold_cast_types: false,
          },
          ~info_map=Haz3lcore.Id.Map.empty,
          ty,
        ),
      ],
    )
  | Abstract => div_c("kind-view", [text("Type")])
  };
