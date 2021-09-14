module Vdom = Virtual_dom.Vdom;
open Pretty;

let view_of_layout = (~inject, ~selected_tag_hole) =>
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
        | EmptyTagHole(u) =>
          let selected =
            selected_tag_hole
            |> Option.map(MetaVar.eq(u))
            |> Option.value(~default=false);
          [
            Node.span(
              [
                Attr.classes([
                  "HoleLabel",
                  "EmptyTagHole",
                  ...selected ? ["selected"] : [],
                ]),
                Attr.on_click(_ => inject(ModelAction.SelectTagHole(u))),
              ],
              vs,
            ),
          ];
        | Delim => [Node.span([Attr.classes(["Delim"])], vs)]
        | HoleLabel => [Node.span([Attr.classes(["HoleLabel"])], vs)]
        },
      t_of_imp: vs => Node.div([Attr.classes(["code", "HTypCode"])], vs),
    },
  );

let view =
    (
      ~inject: ModelAction.t => Ui_event.t,
      ~selected_tag_hole: option(MetaVar.t),
      ~width: int=30,
      ~pos: int=0,
      ty: HTyp.t,
    )
    : Vdom.Node.t => {
  Sexplib.Sexp.(
    {
      print_endline("HTypCode.view");
      print_endline(to_string_hum(HTyp.sexp_of_t(ty)));
    }
  );
  let l =
    ty
    |> HTypDoc.mk(~enforce_inline=false, ~selected_tag_hole)
    |> LayoutOfDoc.layout_of_doc(~width, ~pos);
  switch (l) {
  | None => failwith("unimplemented: view_of_htyp on layout failure")
  | Some(l) => view_of_layout(~inject, ~selected_tag_hole, l)
  };
};
