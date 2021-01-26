module Vdom = Virtual_dom.Vdom;
open Pretty;

let view_of_layout =
  Layout.mk_of_layout(
    Vdom.{
      imp_of_string: s => [Node.text(s)],
      imp_append: (@),
      imp_newline: indent => [
        Node.br([]),
        Node.span(
          [],
          [Node.text(StringUtil.replicat(indent, Unicode.nbsp))],
        ),
      ],
      imp_of_annot: (annot: HTypAnnot.t, vs) =>
        switch (annot) {
        | Delim => [Node.span([Attr.classes(["Delim"])], vs)]
        | HoleLabel => [Node.span([Attr.classes(["HoleLabel"])], vs)]
        },
      t_of_imp: vs => Node.div([Attr.classes(["code", "HTypCode"])], vs),
    },
  );

let view = (~width=30, ~pos=0, ty: HTyp.t): Vdom.Node.t => {
  let l =
    ty
    |> HTypDoc.mk(~enforce_inline=false)
    |> LayoutOfDoc.layout_of_doc(~width, ~pos);
  switch (l) {
  | None => failwith("unimplemented: view_of_htyp on layout failure")
  | Some(l) => view_of_layout(l)
  };
};
