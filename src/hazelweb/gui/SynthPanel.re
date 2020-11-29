open Virtual_dom.Vdom;

let view = (~inject as _, ~view_of_text, j, es, _constraints) => {
  let fillings =
    es
    |> List.map(
         Lazy.force(UHDoc_Exp.mk, ~memoize=false, ~enforce_inline=false),
       )
    |> List.map(
         // TODO layout constraints
         Pretty.LayoutOfDoc.layout_of_doc(~width=40, ~pos=0),
       )
    |> List.map(OptUtil.get(() => failwith("failed layout")))
    |> List.mapi((i, l) =>
         Node.div(
           [Attr.classes(i == j ? ["selected-filling"] : [])],
           view_of_text(l),
         )
       );

  Node.div([Attr.classes(["synth-panel"])], fillings);
};
