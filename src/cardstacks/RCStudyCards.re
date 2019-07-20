module Vdom = Virtual_dom.Vdom;
open OperatorSeq;
open OpSeqUtil;

let span = Vdom.Node.span;
let txt = Vdom.Node.text;
let p = Vdom.Node.p;

let code = s => span([Vdom.Attr.classes(["code"])], [txt(s)]);

let append_case =
  UHExp.(
    case(
      wrap_in_block(var("xs")),
      [
        Rule(UHPat.listnil(), wrap_in_block(var("ys"))),
        Rule(
          Pat.mk_OpSeq(ExpOpExp(UHPat.var("z"), Cons, UHPat.var("zs"))),
          wrap_in_block(
            Exp.mk_OpSeq(
              ExpOpExp(
                var("z"),
                Cons,
                Parenthesized(
                  wrap_in_block(
                    Exp.mk_OpSeq(
                      SeqOpExp(
                        ExpOpExp(var("append"), Space, var("zs")),
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
let append_lam =
  UHExp.(
    lam(
      UHPat.var("xs"),
      wrap_in_block(lam(UHPat.var("ys"), wrap_in_block(append_case))),
    )
  );
let append_letline =
  UHExp.(
    letline(
      UHPat.var("append"),
      ~ann=
        Typ.mk_OpSeq(
          SeqOpExp(
            ExpOpExp(List(Num), Arrow, List(Num)),
            Arrow,
            List(Num),
          ),
        ),
      wrap_in_block(append_lam),
    )
  );

let cardstack: CardStack.t =
  Vdom.[
    {
      caption:
        span(
          [],
          [
            txt(
              "Suppose we are implementing a combat game "
              ++ "and, specifically, defining the function ",
            ),
            p(
              [Attr.create("style", "text-align: center;")],
              [code("damage : (Bool, Num) -> Num"), txt(".")],
            ),
            txt("The input tuple of type "),
            code("(Bool, Num)"),
            txt(
              " represents an enemy attack dealt to the "
              ++ "current player, consisting of a ",
            ),
            code("Bool"),
            txt(" indicating whether the attack is a melee attack, and a "),
            code("Num"),
            txt(
              " representing the critical hit multiplier. The output type of ",
            ),
            code("Num"),
            txt(" is the damage points inflicted upon the current player."),
            p(
              [],
              [
                txt(
                  "Take a moment to understand the current implementation, "
                  ++ "then click \'Next\' to begin the exercise.",
                ),
              ],
            ),
          ],
        ),
      init_block:
        UHExp.Block(
          [
            UHExp.letline(
              UHPat.var("y"),
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
                        ExpOpExp(var("isMelee"), Comma, var("critHit"))
                        |> Pat.mk_OpSeq,
                      )
                    ),
                    wrap_in_block(
                      case(
                        var("isMelee") |> wrap_in_block,
                        [
                          Rule(
                            UHPat.boollit(false),
                            numlit(5) |> wrap_in_block,
                          ),
                          Rule(
                            UHPat.boollit(true),
                            SeqOpExp(
                              ExpOpExp(numlit(2), Times, var("critHit")),
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
        ),
    },
    {
      caption:
        span(
          [],
          [
            txt("Suppose we have in scope the current player's"),
            code(" defenseScore : Num "),
            txt(
              "and wish to integrate it into the damage calculation. "
              ++ "Modify the body of ",
            ),
            code("damage"),
            txt(" so that the "),
            code("case"),
            txt(" expression is bound to a new variable called "),
            code("attackScore"),
            txt(", then return twice the difference between "),
            code("attackScore"),
            txt(" and "),
            code("defenseScore"),
            txt("."),
          ],
        ),
      init_block:
        UHExp.Block(
          [
            UHExp.letline(
              UHPat.var("defenseScore"),
              ~ann=UHTyp.Num,
              UHExp.EmptyHole(-1) |> UHExp.wrap_in_block,
            ),
            UHExp.EmptyLine,
            UHExp.letline(
              UHPat.var("y"),
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
                        ExpOpExp(var("isMelee"), Comma, var("critHit"))
                        |> Pat.mk_OpSeq,
                      )
                    ),
                    wrap_in_block(
                      case(
                        var("isMelee") |> wrap_in_block,
                        [
                          Rule(
                            UHPat.boollit(false),
                            numlit(5) |> wrap_in_block,
                          ),
                          Rule(
                            UHPat.boollit(true),
                            SeqOpExp(
                              ExpOpExp(numlit(2), Times, var("critHit")),
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
        ),
    },
    {
      caption:
        span(
          [],
          [
            txt(
              "Suppose we are implementing a slot machine game. "
              ++ "We represent the state of the slot machine as "
              ++ "3 lists of numbers, each representing a spinning "
              ++ "reel of the slot machine. Below we have implemented "
              ++ "the function",
            ),
            p(
              [Attr.create("style", "text-align: center;")],
              [
                code(
                  "step : (List(Num), List(Num), List(Num)) -> (List(Num), List(Num), List(Num))",
                ),
              ],
            ),
            txt(
              "that takes an inital slot machine state and returns "
              ++ "the new state after a single time step. We have "
              ++ "designed this slot machine so that the middle reel "
              ++ "spins twice as fast as the other two reels.",
            ),
            p(
              [],
              [
                txt(
                  "Take a moment to read and understand the current "
                  ++ "implementation, then click 'Next' to begin the exercise.",
                ),
              ],
            ),
          ],
        ),
      init_block:
        UHExp.Block(
          [
            append_letline,
            UHExp.EmptyLine,
            UHExp.letline(
              UHPat.var("rotate"),
              ~ann=
                UHTyp.(
                  ExpOpExp(List(Num), Arrow, List(Num)) |> Typ.mk_OpSeq
                ),
              UHExp.(
                wrap_in_block(
                  lam(
                    UHPat.var("xs"),
                    wrap_in_block(
                      case(
                        var("xs") |> wrap_in_block,
                        [
                          Rule(UHPat.listnil(), listnil() |> wrap_in_block),
                          Rule(
                            UHPat.(
                              ExpOpExp(var("y"), Cons, var("ys"))
                              |> Pat.mk_OpSeq
                            ),
                            UHExp.(
                              SeqOpExp(
                                ExpOpExp(var("append"), Space, var("ys")),
                                Space,
                                Parenthesized(
                                  ExpOpExp(var("y"), Cons, listnil())
                                  |> Exp.mk_OpSeq
                                  |> wrap_in_block,
                                ),
                              )
                              |> Exp.mk_OpSeq
                              |> wrap_in_block
                            ),
                          ),
                        ],
                      ),
                    ),
                  ),
                )
              ),
            ),
            UHExp.EmptyLine,
            UHExp.letline(
              UHPat.var("step"),
              ~ann=
                UHTyp.(
                  ExpOpExp(
                    Parenthesized(
                      SeqOpExp(
                        ExpOpExp(List(Num), Prod, List(Num)),
                        Prod,
                        List(Num),
                      )
                      |> Typ.mk_OpSeq,
                    ),
                    Arrow,
                    Parenthesized(
                      SeqOpExp(
                        ExpOpExp(List(Num), Prod, List(Num)),
                        Prod,
                        List(Num),
                      )
                      |> Typ.mk_OpSeq,
                    ),
                  )
                  |> Typ.mk_OpSeq
                ),
              UHExp.(
                Block(
                  [EmptyLine],
                  lam(
                    UHPat.(
                      Parenthesized(
                        SeqOpExp(
                          ExpOpExp(var("reel1"), Comma, var("reel2")),
                          Comma,
                          var("reel3"),
                        )
                        |> Pat.mk_OpSeq,
                      )
                    ),
                    Block(
                      [EmptyLine],
                      Parenthesized(
                        wrap_in_block(
                          SeqOpExp(
                            SeqOpExp(
                              SeqOpExp(
                                SeqOpExp(
                                  ExpOpExp(
                                    var("rotate"),
                                    Space,
                                    var("reel1"),
                                  ),
                                  Comma,
                                  var("rotate"),
                                ),
                                Space,
                                Parenthesized(
                                  ExpOpExp(
                                    var("rotate"),
                                    Space,
                                    var("reel2"),
                                  )
                                  |> Exp.mk_OpSeq
                                  |> wrap_in_block,
                                ),
                              ),
                              Comma,
                              var("rotate"),
                            ),
                            Space,
                            var("reel3"),
                          )
                          |> Exp.mk_OpSeq,
                        ),
                      ),
                    ),
                  ),
                )
              ),
            ),
          ],
          UHExp.EmptyHole(-1),
        ),
    },
  ];
