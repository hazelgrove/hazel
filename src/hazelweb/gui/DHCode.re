module Vdom = Virtual_dom.Vdom;
open Pretty;

let view_of_layout = (~inject, l: DHLayout.t): Vdom.Node.t => {
  open Vdom;
  let rec go = (l: DHLayout.t) =>
    switch (l) {
    | Text(s) => [Node.text(s)]
    | Cat(l1, l2) => go(l1) @ go(l2)
    | Linebreak => [Node.br([])]
    | Align(l) => [Node.div([Attr.classes(["Align"])], go(l))]
    | Annot(Collapsed, l) => [
        Node.span([Attr.classes(["Collapsed"])], go(l)),
      ]
    | Annot(HoleLabel, l) => [
        Node.span([Attr.classes(["HoleLabel"])], go(l)),
      ]
    | Annot(FreeLivelitLabel, l) => [
        Node.span([Attr.classes(["FreeLivelitLabel"])], go(l)),
      ]
    | Annot(Delim, l) => [
        Node.span([Attr.classes(["code-delim"])], go(l)),
      ]
    | Annot(NonEmptyHole(_), l) => [
        Node.span([Attr.classes(["InHole"])], go(l)),
      ]
    | Annot(InconsistentBranches(_), l) => [
        Node.span([Attr.classes(["InconsistentBranches"])], go(l)),
      ]
    | Annot(Invalid(_), l) => [
        Node.span([Attr.classes(["InHole"])], go(l)),
      ]
    | Annot(VarHole(_), l) => [
        Node.span([Attr.classes(["InVarHole"])], go(l)),
      ]
    | Annot(EmptyHole(selected, inst), l) => [
        Node.span(
          [
            Attr.classes(["EmptyHole", ...selected ? ["selected"] : []]),
            Attr.on_click(_ =>
              inject(
                ModelAction.SelectInstance(TaggedNodeInstance.Hole, inst),
              )
            ),
          ],
          go(l),
        ),
      ]
    | Annot(FreeLivelit(_), l) => [Node.span([], go(l))]
    | Annot(FailedCastDelim, l) => [
        Node.span([Attr.classes(["FailedCastDelim"])], go(l)),
      ]
    | Annot(FailedCastDecoration, l) => [
        Node.span([Attr.classes(["FailedCastDecoration"])], go(l)),
      ]
    | Annot(CastDecoration, l) => [
        Node.div([Attr.classes(["CastDecoration"])], go(l)),
      ]
    | Annot(DivideByZero, l) => [
        Node.span([Attr.classes(["DivideByZero"])], go(l)),
      ]
    | Annot(InvalidOpDecoration, l) => [
        Node.span([Attr.classes(["InvalidOpDecoration"])], go(l)),
      ]
    | Annot(String, l) => [Node.span([Attr.classes(["String"])], go(l))]
    };
  Node.div([Attr.classes(["code", "DHCode"])], go(l));
};

let view =
    (
      ~inject,
      ~show_casts=false,
      ~show_fn_bodies=false,
      ~show_case_clauses=false,
      ~selected_hole_instance: option(NodeInstance.t)=None,
      ~width: int,
      ~pos=0,
      d: DHExp.t,
    )
    : Vdom.Node.t => {
  d
  |> DHDoc_Exp.mk(
       ~show_casts,
       ~show_fn_bodies,
       ~show_case_clauses,
       ~enforce_inline=false,
       ~selected_hole_instance,
     )
  |> LayoutOfDoc.layout_of_doc(~width, ~pos)
  |> OptUtil.get(() =>
       failwith("unimplemented: view_of_dhexp on layout failure")
     )
  |> view_of_layout(~inject);
};

let view_of_hole_instance =
    (
      ~inject,
      ~width: int,
      ~pos=0,
      ~selected_hole_instance,
      (u, i): NodeInstance.t,
    )
    : Vdom.Node.t =>
  view(
    ~inject,
    ~show_casts=false,
    ~show_fn_bodies=false,
    ~show_case_clauses=false,
    ~selected_hole_instance,
    ~width,
    ~pos,
    DHExp.EmptyHole(u, i, []),
  );

let view_of_var = x => Vdom.Node.text(x);
