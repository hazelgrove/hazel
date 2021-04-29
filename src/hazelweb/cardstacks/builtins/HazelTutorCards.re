/*module Vdom = Virtual_dom.Vdom;
  module Attr = Vdom.Attr;
  open Sexplib;

  let div = Vdom.Node.div;
    let span = Vdom.Node.span;
    let txt = Vdom.Node.text;
    let p = Vdom.Node.p;
    let ptxt = s => p([], [txt(s)]);

    let deserialize = s => ZExp.t_of_sexp(Sexp.of_string(s));

    let code = s => span([Vdom.Attr.classes(["code"])], [txt(s)]);
    let keyblock = s => span([Vdom.Attr.classes(["keyblock"])], [txt(s)]);

    let map_caption =
      div(
        [],
        [
          p(
            [],
            [
              txt("Finish implementing the "),
              code("convert_scores"),
              txt(
                " function by filling in the holes. This function should take a list of submission data and produce a list of final scores. Each submission is represented as a tuple of ",
              ),
              code("(Float, Bool)"),
              txt(" indicating the "),
              code("raw_score"),
              txt(" and whether or not a "),
              code("bonus_question"),
              txt(" was answered correctly, respectively. "),
              code("2.5"),
              txt(
                " pts should be added to each submission that got the bonus question correct to get the final score.",
              ),
            ],
          ),
          p(
            [],
            [
              txt("Your solution must make use of the provided "),
              code("map"),
              txt(" function."),
            ],
          ),
        ],
      );
    let map_init_zexp =
      deserialize(
        "(((LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole map)E))((OpSeq(BinOp NotInHole Arrow(Placeholder 0)(BinOp NotInHole Arrow(Placeholder 1)(Placeholder 2)))(S(Parenthesized(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(Placeholder 1))(S(Parenthesized(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S Float(A Prod(S Bool E)))))(A Arrow(S Float E)))))(A Arrow(S(List(OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S Float(A Prod(S Bool E)))))E)))(A Arrow(S(List(OpSeq(Placeholder 0)(S Float E)))E)))))))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 10)E)))))EmptyLine(LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole bonus)E))()((ExpLine(OpSeq(Placeholder 0)(S(FloatLit NotInHole 2.5)E))))))(LetLineZE(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole convert_scores)E))((OpSeq(BinOp NotInHole Arrow(Placeholder 0)(Placeholder 1))(S(List(OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S Float(A Prod(S Bool E)))))E)))(A Arrow(S(List(OpSeq(Placeholder 0)(S Float E)))E)))))(()(ExpLineZ(ZOpSeq(Placeholder 0)(ZOperand(LamZE NotInHole(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole scores_and_bonuses)E))()(((LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole apply_bonus)E))((OpSeq(BinOp NotInHole Arrow(Placeholder 0)(Placeholder 1))(S(Parenthesized(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S Float(A Prod(S Bool E)))))(A Arrow(S Float E)))))((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole raw_score)(A Comma(S(Var NotInHole NotInVarHole bonus_question)E)))))E))()(EmptyLine(ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 28)E)))))E))))))(ExpLineZ(ZOpSeq(BinOp NotInHole Space(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(Placeholder 2))(ZOperand(CursorE(OnDelim 0 Before)(EmptyHole 39))((A Space(S(Var NotInHole NotInVarHole map)E))(A Space(S(EmptyHole 37)E))))))()))(E E))))()))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 23)E)))))",
      );

    let map_card: CardInfo.t = {caption: map_caption, init_zexp: map_init_zexp};

    let cardstack: CardstackInfo.t = {title: "Hazel Tutor", cards: [map_card]};*/
