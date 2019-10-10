module Vdom = Virtual_dom.Vdom;
open GeneralUtil;
open OperatorSeq;
open OpSeqUtil;

let card_start =
  Card.mk(UHExp.(wrap_in_block(EmptyHole(-1))) |> ZExp.place_before_block);

let simple = i =>
  Vdom.(
    Node.div(
      [Attr.id("simple-container")],
      [
        Node.div(
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
    )
  );

let linear_end =
  UHExp.(
    Block(
      [LetLine(UHPat.var("x"), None, wrap_in_block(numlit(1)))],
      SeqOpExp(ExpOpExp(numlit(2), Times, var("x")), Plus, numlit(3))
      |> Exp.mk_OpSeq,
    )
  )
  |> ZExp.place_after_block;

let simple_cards =
  range(~lo=1, 8) |> List.map(i => Card.mk(~caption=simple(i), linear_end));

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

let card_damage = Card.mk(damage);
let card_damage_refactor = Card.mk(damage_refactor_start);

let cards =
  [card_start, ...simple_cards] @ [card_damage, card_damage_refactor];

let cardstack: CardStack.t = {
  title: "User Interfaces for Programming Languages",
  cards,
};
