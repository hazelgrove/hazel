open Virtual_dom.Vdom;

let view = (~inject as _, ~view_of_text, _i, es) => {
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
    |> List.map(view_of_text);
  Node.div([Attr.classes(["synth-panel"])], fillings);
};
