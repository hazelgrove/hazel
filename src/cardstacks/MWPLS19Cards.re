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

let feature_header = (~selected=?, ~body=?, ()): Vdom.Node.t => {
  let header =
    div(
      [Attr.id("feature-header")],
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
            Attr.classes(
              selected_feature_clss(selected, "node-staging-mode"),
            ),
          ],
          [span([], [txt("Node Staging Mode")])],
        ),
      ],
    );
  div(
    [Attr.id("feature-container")],
    [
      header,
      switch (body) {
      | None => div([], [])
      | Some(body) => body
      },
    ],
  );
};

let syntax = (~selected) =>
  div(
    [Attr.id("syntax-container")],
    [
      div(
        [
          Attr.id("uhexp-syntax"),
          Attr.classes(
            selected == "uhexp-syntax" ? ["selected-syntax"] : [],
          ),
        ],
        [Vdom.Node.create("img", [Attr.create("src", "uhexp.png")], [])],
      ),
      div(
        [
          Attr.id("hexp-syntax"),
          Attr.classes(selected == "hexp-syntax" ? ["selected-syntax"] : []),
        ],
        [Vdom.Node.create("img", [Attr.create("src", "hexp.png")], [])],
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
    caption:
      feature_header(
        ~selected="linear-editing-affordances",
        ~body=syntax(~selected="uhexp-syntax"),
        (),
      ),
    init_zblock:
      UHExp.(
        Block(
          [LetLine(UHPat.var("x"), None, wrap_in_block(numlit(1)))],
          SeqOpExp(ExpOpExp(numlit(2), Times, var("x")), Plus, numlit(3))
          |> Exp.mk_OpSeq,
        )
      )
      |> ZExp.place_after_block,
  },
  {
    caption:
      feature_header(
        ~selected="visual-tree-signifiers",
        ~body=syntax(~selected="hexp-syntax"),
        (),
      ),
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
