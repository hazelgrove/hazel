module Vdom = Virtual_dom.Vdom;

let view_of_layout = (l: DHLayout.t): Vdom.Node.t => {
  open Vdom;
  let rec go = (l: DHLayout.t) =>
    switch (l) {
    | Text(s) => [Node.text(s)]
    | Cat(l1, l2) => go(l1) @ go(l2)
    | Linebreak => [Node.br([])]
    | Align(l) => [Node.div([Attr.classes(["Align"])], go(l))]
    | Annot(HoleLabel, l) => [
        Node.span([Attr.classes(["HoleLabel"])], go(l)),
      ]
    | Annot(Delim, l) => [
        Node.span([Attr.classes(["code-delim"])], go(l)),
      ]
    | Annot(NonEmptyHole(_), l) => [
        Node.span([Attr.classes(["InHole"])], go(l)),
      ]
    | Annot(VarHole(_), l) => [
        Node.span([Attr.classes(["InVarHole"])], go(l)),
      ]
    | Annot(EmptyHole(_), l) => go(l)
    | Annot(FailedCast, l) => [
        Node.span([Attr.classes(["FailedCast"])], go(l)),
      ]
    };
  Node.div([Attr.classes(["code", "DHCode"])], go(l));
};

let view = (~width=80, ~pos=0, d: DHExp.t): Vdom.Node.t => {
  d
  |> DHDoc.Exp.mk(~enforce_inline=false)
  |> LayoutOfDoc.layout_of_doc(~width, ~pos)
  |> OptUtil.get(() =>
       failwith("unimplemented: view_of_dhexp on layout failure")
     )
  |> view_of_layout;
};
