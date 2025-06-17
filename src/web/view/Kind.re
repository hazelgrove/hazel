open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

let view = (~globals: Globals.t, kind: Language.Ctx.kind): Node.t =>
  switch (kind) {
  | Singleton(ty) =>
    div_c(
      "kind-view",
      [
        CodeViewable.view_typ(
          ~secondary_icons=globals.settings.secondary_icons,
          ~font_metrics=globals.font_metrics,
          ~settings={
            inline: true,
            fold_case_clauses: false,
            fold_fn_bodies: false,
            hide_fixpoints: false,
            show_filters: false,
            show_unknown_as_hole: true,
          },
          ty,
        ),
      ],
    )
  | Abstract => div_c("kind-view", [text("Type")])
  };
