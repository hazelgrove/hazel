open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

let rec view = (~globals, kind: Language.Ctx.kind): Node.t => {
  let rec intersperse = (sep, list) =>
    switch (list) {
    | [] => []
    | [x] => [x]
    | [x, ...xs] => [x, sep, ...intersperse(sep, xs)]
    };

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
            show_filters: false,
            show_unknown_as_hole: true,
          },
          ty,
        ),
      ],
    )
  | Abstract => div_c("kind-view", [text("Type")])
  | Arr(dom, cod) =>
    let dom_view =
      switch (dom) {
      | Arr(_, _) => div([text("("), view(~globals, dom), text(")")])
      | _ => view(~globals, dom)
      };
    div_c("kind-view", [dom_view, text(" → "), view(~globals, cod)]);
  | Prod(k1, k2) =>
    let views = List.map(k => view(~globals, k), [k1, k2]);
    div_c(
      "kind-view",
      [text("(")] @ intersperse(text(", "), views) @ [text(")")],
    );
  };
};
