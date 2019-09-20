module Vdom = Virtual_dom.Vdom;
open GeneralUtil;
open OperatorSeq;
open OpSeqUtil;

module Node = Vdom.Node;
module Attr = Vdom.Attr;
let div = Node.div;
let span = Node.span;
let txt = Node.text;
let p = Node.p;
let a = Node.a;
let strong = Node.strong;

let selected_feature_clss = (selected, feature) =>
  switch (selected) {
  | None => [feature, "feature"]
  | Some(s) => [feature, "feature", s == feature ? "selected" : "unselected"]
  };

let page_header_0 =
  span(
    [Attr.id("page-header-0")],
    [
      a(
        [Attr.classes(["logo-text"]), Attr.href("https://hazel.org")],
        [txt("Hazel")],
      ),
      span(
        [Attr.classes(["tagline"])],
        [
          txt(" is a "),
          strong([Attr.id("tagline-1")], [txt("live")]),
          txt(" functional programming environment featuring a "),
          strong([Attr.id("tagline-2")], [txt("type-aware")]),
          txt(" "),
          strong([Attr.id("tagline-3")], [txt("structure editor")]),
        ],
      ),
    ],
  );

let page_header_1 =
  span(
    [Attr.id("page-header-1")],
    [
      a(
        [Attr.classes(["logo-text"]), Attr.href("https://hazel.org")],
        [txt("Hazel")],
      ),
      span(
        [Attr.classes(["tagline"])],
        [
          txt(" is a "),
          span(
            [Attr.classes(["removed"])],
            [
              strong([Attr.id("tagline-1")], [txt("live")]),
              txt(" functional programming environment featuring a "),
            ],
          ),
          strong([Attr.id("tagline-2")], [txt("type-aware")]),
          txt(" "),
          strong([Attr.id("tagline-3")], [txt("structure editor")]),
        ],
      ),
    ],
  );

let page_header_2 =
  span(
    [Attr.id("page-header-2")],
    [
      a(
        [Attr.classes(["logo-text"]), Attr.href("https://hazel.org")],
        [txt("Hazel")],
      ),
      span(
        [Attr.classes(["tagline"])],
        [
          txt(" is a "),
          span(
            [Attr.classes(["removed"])],
            [
              strong([Attr.id("tagline-1")], [txt("live")]),
              txt(" functional programming environment featuring a "),
              strong([Attr.id("tagline-2")], [txt("type-aware")]),
              txt(" "),
            ],
          ),
          strong([Attr.id("tagline-3")], [txt("structure editor")]),
        ],
      ),
    ],
  );

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

let damage_refactor_start =
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
                    Rule(UHPat.boollit(false), numlit(5) |> wrap_in_block),
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
  |> ZExp.place_before_block;

let damage_refactor_end =
  UHExp.(
    ZExp.BlockZL(
      (
        [
          letline(
            UHPat.var("defense_score"),
            ~ann=UHTyp.Num,
            EmptyHole(-1) |> wrap_in_block,
          ),
          EmptyLine,
        ],
        ZExp.LetLineZE(
          UHPat.var("damage"),
          Some(
            UHTyp.(
              ExpOpExp(
                Parenthesized(ExpOpExp(Bool, Prod, Num) |> Typ.mk_OpSeq),
                Arrow,
                Num,
              )
              |> Typ.mk_OpSeq
            ),
          ),
          ZExp.wrap_in_block(
            LamZE(
              NotInHole,
              UHPat.(
                Parenthesized(
                  ExpOpExp(var("is_melee"), Comma, var("crit_hit"))
                  |> Pat.mk_OpSeq,
                )
              ),
              None,
              BlockZE(
                [
                  letline(
                    UHPat.var("attack_score"),
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
                    )
                    |> wrap_in_block,
                  ),
                ],
                Exp.mk_OpSeqZ(
                  Parenthesized(
                    wrap_in_block(
                      ExpOpExp(
                        var("attack_score"),
                        Minus,
                        var("defense_score"),
                      )
                      |> Exp.mk_OpSeq,
                    ),
                  )
                  |> ZExp.place_after_exp,
                  EmptySuffix(ExpPrefix(numlit(2), Times)),
                ),
              ),
            ),
          ),
        ),
        [],
      ),
      EmptyHole(-1),
    )
  );

let init_zblock =
  UHExp.(wrap_in_block(EmptyHole(-1))) |> ZExp.place_before_block;

let cards: list(Card.t) =
  [
    ({header: page_header_0, caption: div([], []), init_zblock}: Card.t), // wtf required type annotation
    {
      header: page_header_0,
      caption: txt("something about invariant"),
      init_zblock,
    },
    {
      header: page_header_0,
      caption: txt("something about cool affordances"),
      init_zblock,
    },
    {
      header: page_header_1,
      caption: txt("something about cool affordances"),
      init_zblock,
    },
    {
      header: page_header_1,
      caption: feature_header(),
      init_zblock:
        UHExp.(wrap_in_block(EmptyHole(-1))) |> ZExp.place_before_block,
    },
    {
      header: page_header_1,
      caption: feature_header(~selected="automatic-hole-insertion", ()),
      init_zblock:
        UHExp.(wrap_in_block(EmptyHole(-1))) |> ZExp.place_before_block,
    },
    {
      header: page_header_2,
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
           header: page_header_2,
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
      header: page_header_2,
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
      header: page_header_2,
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
      header: page_header_2,
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
      header: page_header_2,
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
      header: page_header_2,
      caption:
        feature_header(
          ~selected="node-staging-mode",
          ~body=syntax(~selected="uhexp-syntax"),
          (),
        ),
      init_zblock: damage_refactor_start,
    },
    {
      header: page_header_2,
      caption:
        div(
          [Attr.id("feature-container")],
          [
            div(
              [Attr.id("feature-header")],
              [
                div(
                  [Attr.classes(["feature", "unselected"])],
                  [span([], [txt("Automatic Hole Insertion")])],
                ),
                div(
                  [Attr.classes(["feature", "absent"])],
                  [span([], [txt("Linear Editing Affordances")])],
                ),
                div(
                  [Attr.classes(["feature"])],
                  [span([], [txt("Visual Tree Signifiers")])],
                ),
                div(
                  [Attr.classes(["feature", "unselected"])],
                  [span([], [txt("Node Staging Mode")])],
                ),
              ],
            ),
            div(
              [Attr.id("scratch-container")],
              [
                div(
                  [Attr.id("scratch-logo")],
                  [
                    Vdom.Node.create(
                      "img",
                      [Attr.create("src", "scratch-logo.png")],
                      [],
                    ),
                  ],
                ),
                div(
                  [Attr.id("scratch")],
                  [
                    Vdom.Node.create(
                      "img",
                      [Attr.create("src", "scratch.png")],
                      [],
                    ),
                  ],
                ),
              ],
            ),
          ],
        ),
      init_zblock: damage_refactor_end,
    },
    {
      header: page_header_2,
      caption:
        div(
          [Attr.id("feature-container")],
          [
            div(
              [Attr.id("feature-header")],
              [
                div(
                  [Attr.classes(["feature", "unselected"])],
                  [span([], [txt("Automatic Hole Insertion")])],
                ),
                div(
                  [Attr.classes(["feature"])],
                  [span([], [txt("Linear Editing Affordances")])],
                ),
                div(
                  [Attr.classes(["feature", "absent"])],
                  [span([], [txt("Visual Tree Signifiers")])],
                ),
                div(
                  [Attr.classes(["feature", "unselected"])],
                  [span([], [txt("Node Staging Mode")])],
                ),
              ],
            ),
            div(
              [Attr.id("mps-container")],
              [
                div(
                  [Attr.id("mps")],
                  [
                    Vdom.Node.create(
                      "img",
                      [Attr.create("src", "mps.png")],
                      [],
                    ),
                  ],
                ),
                div(
                  [Attr.id("mps-logo")],
                  [
                    Vdom.Node.create(
                      "img",
                      [Attr.create("src", "mps-logo.png")],
                      [],
                    ),
                  ],
                ),
              ],
            ),
          ],
        ),
      init_zblock: damage_refactor_end,
    },
    {
      header: page_header_2,
      caption:
        div(
          [Attr.id("feature-container")],
          [
            div(
              [Attr.id("feature-header")],
              [
                div(
                  [Attr.classes(["feature", "unselected"])],
                  [span([], [txt("Automatic Hole Insertion")])],
                ),
                div(
                  [Attr.classes(["feature"])],
                  [span([], [txt("Linear Editing Affordances")])],
                ),
                div(
                  [Attr.classes(["feature", "absent"])],
                  [span([], [txt("Visual Tree Signifiers")])],
                ),
                div(
                  [Attr.classes(["feature", "unselected"])],
                  [span([], [txt("Node Staging Mode")])],
                ),
              ],
            ),
            div(
              [],
              [
                div(
                  [],
                  [
                    Vdom.Node.create(
                      "img",
                      [Attr.id("berger"), Attr.create("src", "berger.png")],
                      [],
                    ),
                  ],
                ),
              ],
            ),
          ],
        ),
      init_zblock: damage_refactor_end,
    },
  ];

let cardstack: CardStack.t = {title: "MWPLS '19", cards};
