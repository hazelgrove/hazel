module Vdom = Virtual_dom.Vdom;

let view_of_layout = (l: DHLayout.t): Vdom.Node.t => {
  open Vdom;
  let rec go = (l: DHLayout.t) =>
    switch (l) {
    | Text(s) => [Node.text(s)]
    | Cat(l1, l2) => go(l1) @ go(l2)
    | Linebreak => [Node.br([])]
    | Align(l) => [Node.div([Attr.classes(["Align"])], go(l))]
    | Annot(_, l) => go(l)
    };
  Node.div([Attr.classes(["dhcode"])], go(l));
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
