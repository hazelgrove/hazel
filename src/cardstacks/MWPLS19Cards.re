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
let br = Node.br;

let selected_feature_clss = (selected, feature) =>
  switch (selected) {
  | None => [feature, "feature"]
  | Some(s) => [feature, "feature", s == feature ? "selected" : "unselected"]
  };

let hazel_logo =
  a(
    [Attr.classes(["logo-text"]), Attr.href("https://hazel.org")],
    [txt("Hazel")],
  );

let page_header_0a = span([], [hazel_logo]);

let page_header_0b =
  span(
    [],
    [
      hazel_logo,
      span(
        [Attr.classes(["tagline"])],
        [
          txt(" is a "),
          strong([Attr.id("tagline-1")], [txt("live")]),
          txt(" functional programming environment with a "),
          strong([Attr.id("tagline-2")], [txt("type-aware")]),
          txt(" "),
          strong([Attr.id("tagline-3")], [txt("structure editor")]),
        ],
      ),
    ],
  );

let page_header_0c =
  span(
    [],
    [
      hazel_logo,
      span(
        [Attr.classes(["tagline"])],
        [
          txt(" is a "),
          strong([Attr.id("tagline-1")], [txt("live")]),
          txt(" functional programming environment "),
          span(
            [Attr.classes(["removed"])],
            [
              txt("with a "),
              strong([Attr.id("tagline-2")], [txt("type-aware")]),
              txt(" "),
              strong([Attr.id("tagline-3")], [txt("structure editor")]),
            ],
          ),
        ],
      ),
    ],
  );

let page_header_1 =
  span(
    [Attr.id("page-header-1")],
    [
      hazel_logo,
      span(
        [Attr.classes(["tagline"])],
        [
          txt(" is "),
          span(
            [Attr.classes(["removed"])],
            [
              txt("a "),
              strong([Attr.id("tagline-1")], [txt("live")]),
              txt(" functional programming environment with"),
            ],
          ),
          txt(" a "),
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
          txt(" is "),
          span(
            [Attr.classes(["removed"])],
            [
              txt("a "),
              strong([Attr.id("tagline-1")], [txt("live")]),
              txt(" functional programming environment with"),
            ],
          ),
          txt(" a "),
          strong([Attr.id("tagline-2")], [txt("type-aware")]),
          txt(" "),
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
        div([Attr.classes(["feature-spacer"])], [span([], [txt(" ")])]),
        /*
         div(
           [
             Attr.classes(
               switch (selected) {
               | None => ["related-work"]
               | Some("related-work") => ["related-work", "selected"]
               | Some(_) => ["related-work", "unselected"]
               },
             ),
           ],
           [span([], [txt("Related Work")])],
         ),
         div(
           [
             Attr.classes(
               switch (selected) {
               | None => ["future-work"]
               | Some("future-work") => ["future-work", "selected"]
               | Some(_) => ["future-work", "unselected"]
               },
             ),
           ],
           [span([], [txt("Future Work")])],
         ),
         */
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

let hexp =
  div(
    [Attr.id("hexp")],
    [
      Node.create("img", [Attr.create("src", "hexp.png")], []),
      div([Attr.classes(["citation"])], [txt("[Omar et al. POPL'17]")]),
    ],
  );

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

let syntax1 =
  div(
    [Attr.id("syntax")],
    [Node.create("img", [Attr.create("src", "syntax-1.png")], [])],
  );
let syntax2 =
  div(
    [Attr.id("syntax")],
    [Node.create("img", [Attr.create("src", "syntax-2.png")], [])],
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

let linear_prior =
  div(
    [Attr.id("linear-prior")],
    [
      span([], [txt("Prior work")]),
      Node.ul(
        [],
        [
          Node.li(
            [],
            [
              txt("text escapes"),
              br([]),
              span(
                [Attr.classes(["citation"])],
                [txt("[Teitelbaum & Reps CACM'81]")],
              ),
            ],
          ),
          Node.li(
            [],
            [
              txt("online precedence parsing"),
              br([]),
              span(
                [Attr.classes(["citation"])],
                [txt("[Sufrin & de Moor '99, Voelter et al. SLE'16]")],
              ),
            ],
          ),
        ],
      ),
    ],
  );
/*
 div(
   [Attr.id("linear-prior-container")],
   [
     div(
       [Attr.id("linear-prior-fig")],
       [
         Vdom.Node.create("img", [Attr.create("src", "simple-7.png")], []),
       ],
     ),
     div(
       [Attr.id("linear-prior-cite")],
       [
         span(
           [],
           [
             txt("Prior work:"),
             br([]),
             txt("- text escapes"),
             br([]),
             txt("- online precedence parsing"),
           ],
         ),
       ],
     ),
   ],
 );
 */

let partial = i =>
  div(
    [Attr.id("partial")],
    [
      Node.create(
        "img",
        [Attr.create("src", "partial-" ++ string_of_int(i) ++ ".png")],
        [],
      ),
    ],
  );

let complete = i =>
  div(
    [Attr.id("complete")],
    [
      Node.create(
        "img",
        [Attr.create("src", "complete-" ++ string_of_int(i) ++ ".png")],
        [],
      ),
    ],
  );

let linear_start =
  UHExp.(
    ExpOpExp(numlit(2), Times, var("x")) |> Exp.mk_OpSeq |> wrap_in_block
  )
  |> ZExp.place_after_block;

let linear_end =
  UHExp.(
    Block(
      [LetLine(UHPat.var("x"), None, wrap_in_block(numlit(1)))],
      SeqOpExp(ExpOpExp(numlit(2), Times, var("x")), Plus, numlit(3))
      |> Exp.mk_OpSeq,
    )
  )
  |> ZExp.place_after_block;

let linear_final =
  UHExp.(
    ZExp.BlockZE(
      [LetLine(UHPat.var("x"), None, wrap_in_block(numlit(1)))],
      CursorE(
        OnDelim(2, After),
        SeqOpExp(ExpOpExp(numlit(2), Times, var("x")), Plus, numlit(3))
        |> Exp.mk_OpSeq,
      ),
    )
  );

let damage =
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

let map_example = {
  open OperatorSeq;
  let case_node =
    UHExp.(
      case(
        wrap_in_block(var("xs")),
        [
          Rule(UHPat.listnil(), wrap_in_block(listnil())),
          Rule(
            Pat.mk_OpSeq(ExpOpExp(UHPat.var("y"), Cons, UHPat.var("ys"))),
            wrap_in_block(
              Exp.mk_OpSeq(
                ExpOpExp(
                  Parenthesized(
                    wrap_in_block(
                      Exp.mk_OpSeq(ExpOpExp(var("f"), Space, var("y"))),
                    ),
                  ),
                  Cons,
                  Parenthesized(
                    wrap_in_block(
                      Exp.mk_OpSeq(
                        SeqOpExp(
                          ExpOpExp(var("map"), Space, var("f")),
                          Space,
                          var("ys"),
                        ),
                      ),
                    ),
                  ),
                ),
              ),
            ),
          ),
        ],
      )
    );
  let lam_node =
    UHExp.(
      lam(
        UHPat.var("f"),
        wrap_in_block(lam(UHPat.var("xs"), wrap_in_block(case_node))),
      )
    );
  let letline_node =
    UHExp.(
      letline(
        UHPat.var("map"),
        ~ann=
          Typ.mk_OpSeq(
            SeqOpExp(
              ExpOpExp(
                Parenthesized(Typ.mk_OpSeq(ExpOpExp(Num, Arrow, Num))),
                Arrow,
                List(Num),
              ),
              Arrow,
              List(Num),
            ),
          ),
        wrap_in_block(lam_node),
      )
    );
  UHExp.(
    ZExp.BlockZE(
      [letline_node],
      Exp.mk_OpSeqZ(
        var("f") |> ZExp.place_before_exp,
        BothNonEmpty(
          ExpPrefix(var("map"), Space),
          ExpSuffix(
            Space,
            Parenthesized(
              SeqOpExp(
                SeqOpExp(
                  ExpOpExp(numlit(1), Cons, numlit(2)),
                  Cons,
                  numlit(3),
                ),
                Cons,
                listnil(),
              )
              |> Exp.mk_OpSeq
              |> wrap_in_block,
            ),
          ),
        ),
      ),
    )
  );
};

let invariant =
  div(
    [],
    [
      div(
        [],
        [
          span(
            [Attr.create("style", "text-decoration: underline;")],
            [
              txt(
                "Every edit state is statically and dynamically well-defined",
              ),
            ],
          ),
        ],
      ),
      div(
        [Attr.id("citation-container")],
        [
          span([Attr.id("citation-fill")], [txt("")]),
          span(
            [Attr.classes(["citation"])],
            [txt("[Omar et al. POPL'17, Omar et al. POPL'19]")],
          ),
        ],
      ),
    ],
  );

let cards: list(Card.t) =
  [
    ({header: page_header_0a, caption: div([], []), init_zblock}: Card.t), // wtf required type annotation
    {header: page_header_0b, caption: div([], []), init_zblock},
    {header: page_header_0c, caption: invariant, init_zblock},
    {header: page_header_0c, caption: invariant, init_zblock: map_example},
    {header: page_header_1, caption: invariant, init_zblock: map_example},
    {header: page_header_1, caption: feature_header(), init_zblock},
    {
      header: page_header_1,
      caption:
        feature_header(~selected="automatic-hole-insertion", ~body=hexp, ()),
      init_zblock,
    },
    {
      header: page_header_2,
      caption: feature_header(~selected="linear-editing-affordances", ()),
      init_zblock: linear_start,
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
           init_zblock: linear_end,
         }: int => Card.t,
       )
  )
  @ [
    (
      {
        header: page_header_2,
        caption:
          feature_header(
            ~selected="linear-editing-affordances",
            ~body=linear_prior,
            (),
          ),
        init_zblock: linear_end,
      }: Card.t
    ),
    {
      header: page_header_2,
      caption:
        feature_header(
          ~selected="linear-editing-affordances",
          ~body=syntax1,
          (),
        ),
      init_zblock: linear_end,
    },
  ]
  /*
    @ (
      [1, 2]
      |> List.map(
           i => {
             header: page_header_2,
             caption:
               feature_header(
                 ~selected="linear-editing-affordances",
                 ~body=partial(i),
                 (),
               ),
             init_zblock: linear_end,
           }: int => Card.t,
         )
    )
    @ (
      [1, 2]
      |> List.map(
            i => {
              header: page_header_2,
              caption:
                feature_header(
                  ~selected="linear-editing-affordances",
                  ~body=complete(i),
                  (),
                ),
              init_zblock: linear_end,
            }: int => Card.t,
          )
    )
   */
  @ [
    {
      header: page_header_2,
      caption:
        feature_header(~selected="visual-tree-signifiers", ~body=syntax1, ()),
      init_zblock: linear_end,
    },
    {
      header: page_header_2,
      caption:
        feature_header(~selected="visual-tree-signifiers", ~body=syntax2, ()),
      init_zblock: linear_final,
    },
    /*
     {
       header: page_header_2,
       caption:
         feature_header(~selected="visual-tree-signifiers", ~body=syntax2, ()),
       init_zblock: damage,
     },
      {
        header: page_header_2,
        caption:
          feature_header(~selected="node-staging-mode", ~body=syntax2, ()),
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
      */
    {
      header: page_header_2,
      caption:
        feature_header(~selected="node-staging-mode", ~body=syntax2, ()),
      init_zblock: damage_refactor_start,
    },
    /*
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
                   [span([], [txt("❌ Linear Editing Affordances")])],
                 ),
                 div(
                   [Attr.classes(["feature"])],
                   [span([], [txt("✅ Visual Tree Signifiers")])],
                 ),
                 div(
                   [Attr.classes(["feature", "unselected"])],
                   [span([], [txt("Node Staging Mode")])],
                 ),
                 div(
                   [Attr.classes(["feature-spacer"])],
                   [span([], [txt(" ")])],
                 ),
                 div(
                   [Attr.classes(["related-work", "selected"])],
                   [span([], [txt("Related Work")])],
                 ),
                 div(
                   [Attr.classes(["future-work", "unselected"])],
                   [span([], [txt("Future Work")])],
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
                   [span([], [txt("✅ Linear Editing Affordances")])],
                 ),
                 div(
                   [Attr.classes(["feature"])],
                   [span([], [txt("❌ Visual Tree Signifiers")])],
                 ),
                 div(
                   [Attr.classes(["feature", "unselected"])],
                   [span([], [txt("Node Staging Mode")])],
                 ),
                 div(
                   [Attr.classes(["feature-spacer"])],
                   [span([], [txt(" ")])],
                 ),
                 div(
                   [Attr.classes(["related-work", "selected"])],
                   [span([], [txt("Related Work")])],
                 ),
                 div(
                   [Attr.classes(["future-work", "unselected"])],
                   [span([], [txt("Future Work")])],
                 ),
               ],
             ),
             div(
               [Attr.id("mps-container")],
               [
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
                   [span([], [txt("✅ Linear Editing Affordances")])],
                 ),
                 div(
                   [Attr.classes(["feature"])],
                   [span([], [txt("❌ Visual Tree Signifiers")])],
                 ),
                 div(
                   [Attr.classes(["feature", "unselected"])],
                   [span([], [txt("Node Staging Mode")])],
                 ),
                 div(
                   [Attr.classes(["feature-spacer"])],
                   [span([], [txt(" ")])],
                 ),
                 div(
                   [Attr.classes(["related-work", "selected"])],
                   [span([], [txt("Related Work")])],
                 ),
                 div(
                   [Attr.classes(["future-work", "unselected"])],
                   [span([], [txt("Future Work")])],
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
     */
  ];

let cardstack: CardStack.t = {title: "MWPLS '19", cards};
