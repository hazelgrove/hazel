open Virtual_dom.Vdom;
open Node;
open Util_web.WebUtil;

let view = (~globals, kind: Language.Ctx.kind): Node.t =>
  switch (kind) {
  | Singleton(ty) =>
    div_c(
      "kind-view",
      [
        CodeViewable.view_typ(
          ~globals,
          ~settings={
            secondary: AutoFormat,
            parenthesization: Defensive,
            label_format: QuoteWhenNecessary,
            inline: true,
            fold_case_clauses: false,
            fold_fn_bodies: `NoFold,
            hide_fixpoints: false,
            show_ascriptions: true,
            show_filters: false,
            show_unknown_as_hole: true,
            hole_tiles: false,
            project_tables: false,
          },
          ty,
        ),
      ],
    )
  | Abstract => div_c("kind-view", [text("Type")])
  };
