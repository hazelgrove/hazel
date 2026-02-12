open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

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
            project_tables: false,
            fold_fn_bodies: `NoFold,
            hide_fixpoints: false,
            show_filters: false,
            show_unknown_as_hole: true,
            raise_if_padding: false,
          },
          ty,
        ),
      ],
    )
  | Abstract => div_c("kind-view", [text("Type")])
  };
