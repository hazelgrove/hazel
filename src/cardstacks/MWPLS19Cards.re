module Vdom = Virtual_dom.Vdom;
open OperatorSeq;
open OpSeqUtil;

module Attr = Vdom.Attr;
let div = Vdom.Node.div;
let span = Vdom.Node.span;
let txt = Vdom.Node.text;
let p = Vdom.Node.p;

let selected_feature_clss = (selected, feature) =>
  switch (selected) {
  | None => [feature, "feature"]
  | Some(s) => [feature, "feature", s == feature ? "selected" : "unselected"]
  };

[@warning "-27"]
let feature_header = (~selected: option(string)=?, (): unit): Vdom.Node.t =>
  div(
    // for some reason incr_dom freaks out when
    // I use Attr.classes(["feature-header"])...
    [Attr.create("class", "feature-header")],
    [
      div(
        [
          Attr.classes(
            selected_feature_clss(selected, "automatic-hole-insertion"),
          ),
        ],
        [span([], [txt("Automatic Hole Insertion")])],
      ),
      div(
        [
          Attr.classes(
            selected_feature_clss(selected, "linear-editing-affordances"),
          ),
        ],
        [span([], [txt("Linear Editing Affordances")])],
      ),
      div(
        [
          Attr.classes(
            selected_feature_clss(selected, "visual-tree-signifiers"),
          ),
        ],
        [span([], [txt("Visual Tree Signifiers")])],
      ),
      div(
        [
          Attr.classes(selected_feature_clss(selected, "node-staging-mode")),
        ],
        [span([], [txt("Node Staging Mode")])],
      ),
    ],
  );

let cards: list(Card.t) = [
  {
    caption: feature_header(),
    init_zblock:
      UHExp.(wrap_in_block(EmptyHole(-1))) |> ZExp.place_before_block,
  },
  {
    caption: feature_header(~selected="automatic-hole-insertion", ()),
    init_zblock:
      UHExp.(wrap_in_block(EmptyHole(-1))) |> ZExp.place_before_block,
  },
  {
    caption: feature_header(~selected="linear-editing-affordances", ()),
    init_zblock:
      UHExp.(
        wrap_in_block(
          ExpOpExp(
            numlit(2),
            Times,
            var(~var_err_status=InVarHole(Free, 0), "x"),
          )
          |> Exp.mk_OpSeq,
        )
      )
      |> ZExp.place_after_block,
  },
  {
    caption: feature_header(~selected="visual-tree-signifiers", ()),
    init_zblock:
      UHExp.(wrap_in_block(EmptyHole(-1))) |> ZExp.place_before_block,
  },
  {
    caption: feature_header(~selected="node-staging-mode", ()),
    init_zblock:
      UHExp.(wrap_in_block(EmptyHole(-1))) |> ZExp.place_before_block,
  },
];

let cardstack: CardStack.t = {title: "MWPLS '19", cards};
