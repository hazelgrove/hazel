module Vdom = Virtual_dom.Vdom;
open GeneralUtil;
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

let simple = i =>
  div(
    [Attr.id("simple-container")],
    [
      div(
        [Attr.id("simple")],
        [
          Vdom.Node.create(
            "img",
            [Attr.create("src", "simple-" ++ string_of_int(i) ++ ".png")],
            [],
          ),
        ],
      ),
    ],
  );

let cards: list(Card.t) =
  [
    (
      {
        caption: feature_header(),
        init_zblock:
          UHExp.(wrap_in_block(EmptyHole(-1))) |> ZExp.place_before_block,
      }: Card.t
    ),
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
  ]
  @ (
    range(~lo=1, 8)
    |> List.map(
         i => {
           caption:
             feature_header(
               ~selected="linear-editing-affordances",
               ~body=simple(i),
               (),
             ),
           init_zblock:
             UHExp.(
               Block(
                 [
                   LetLine(UHPat.var("x"), None, wrap_in_block(numlit(1))),
                 ],
                 SeqOpExp(
                   ExpOpExp(numlit(2), Times, var("x")),
                   Plus,
                   numlit(3),
                 )
                 |> Exp.mk_OpSeq,
               )
             )
             |> ZExp.place_after_block,
         }: int => Card.t,
       )
  )
  @ [
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
            SeqOpExp(
              ExpOpExp(numlit(2), Times, var("x")),
              Plus,
              numlit(3),
            )
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
        UHExp.(
          Block(
            [LetLine(UHPat.var("x"), None, wrap_in_block(numlit(1)))],
            SeqOpExp(
              ExpOpExp(numlit(2), Times, var("x")),
              Plus,
              numlit(3),
            )
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
        UHExp.Block(
          [
            UHExp.letline(
              UHPat.var("damage"),
              ~ann=
                UHTyp.(
                  ExpOpExp(
                    Parenthesized(ExpOpExp(Bool, Prod, Num) |> Typ.mk_OpSeq),
                    Arrow,
                    Num,
                  )
                  |> Typ.mk_OpSeq
                ),
              UHExp.(
                wrap_in_block(
                  lam(
                    UHPat.(
                      Parenthesized(
                        ExpOpExp(var("is_melee"), Comma, var("crit_hit"))
                        |> Pat.mk_OpSeq,
                      )
                    ),
                    wrap_in_block(
                      case(
                        var("is_melee") |> wrap_in_block,
                        [
                          Rule(
                            UHPat.boollit(false),
                            numlit(5) |> wrap_in_block,
                          ),
                          Rule(
                            UHPat.boollit(true),
                            SeqOpExp(
                              ExpOpExp(numlit(2), Times, var("crit_hit")),
                              Plus,
                              numlit(1),
                            )
                            |> Exp.mk_OpSeq
                            |> wrap_in_block,
                          ),
                        ],
                      ),
                    ),
                  ),
                )
              ),
            ),
          ],
          EmptyHole(-1),
        )
        |> ZExp.place_before_block,
    },
    {
      caption:
        feature_header(
          ~selected="node-staging-mode",
          ~body=syntax(~selected="uhexp-syntax"),
          (),
        ),
      init_zblock:
        UHExp.Block(
          [
            UHExp.letline(
              UHPat.var("damage"),
              ~ann=
                UHTyp.(
                  ExpOpExp(
                    Parenthesized(ExpOpExp(Bool, Prod, Num) |> Typ.mk_OpSeq),
                    Arrow,
                    Num,
                  )
                  |> Typ.mk_OpSeq
                ),
              UHExp.(
                wrap_in_block(
                  lam(
                    UHPat.(
                      Parenthesized(
                        ExpOpExp(var("is_melee"), Comma, var("crit_hit"))
                        |> Pat.mk_OpSeq,
                      )
                    ),
                    wrap_in_block(
                      case(
                        var("is_melee") |> wrap_in_block,
                        [
                          Rule(
                            UHPat.boollit(false),
                            numlit(5) |> wrap_in_block,
                          ),
                          Rule(
                            UHPat.boollit(true),
                            SeqOpExp(
                              ExpOpExp(numlit(2), Times, var("crit_hit")),
                              Plus,
                              numlit(1),
                            )
                            |> Exp.mk_OpSeq
                            |> wrap_in_block,
                          ),
                        ],
                      ),
                    ),
                  ),
                )
              ),
            ),
          ],
          EmptyHole(-1),
        )
        |> ZExp.place_before_block,
    },
    {
      caption:
        feature_header(
          ~selected="node-staging-mode",
          ~body=syntax(~selected="uhexp-syntax"),
          (),
        ),
      init_zblock:
        UHExp.Block(
          [
            UHExp.letline(
              UHPat.var("defense_score"),
              ~ann=UHTyp.Num,
              UHExp.EmptyHole(-1) |> UHExp.wrap_in_block,
            ),
            UHExp.EmptyLine,
            UHExp.letline(
              UHPat.var("damage"),
              ~ann=
                UHTyp.(
                  ExpOpExp(
                    Parenthesized(ExpOpExp(Bool, Prod, Num) |> Typ.mk_OpSeq),
                    Arrow,
                    Num,
                  )
                  |> Typ.mk_OpSeq
                ),
              UHExp.(
                wrap_in_block(
                  lam(
                    UHPat.(
                      Parenthesized(
                        ExpOpExp(var("is_melee"), Comma, var("crit_hit"))
                        |> Pat.mk_OpSeq,
                      )
                    ),
                    wrap_in_block(
                      case(
                        var("is_melee") |> wrap_in_block,
                        [
                          Rule(
                            UHPat.boollit(false),
                            numlit(5) |> wrap_in_block,
                          ),
                          Rule(
                            UHPat.boollit(true),
                            SeqOpExp(
                              ExpOpExp(numlit(2), Times, var("crit_hit")),
                              Plus,
                              numlit(1),
                            )
                            |> Exp.mk_OpSeq
                            |> wrap_in_block,
                          ),
                        ],
                      ),
                    ),
                  ),
                )
              ),
            ),
          ],
          EmptyHole(-1),
        )
        |> ZExp.place_before_block,
    },
  ];

let cardstack: CardStack.t = {title: "MWPLS '19", cards};
